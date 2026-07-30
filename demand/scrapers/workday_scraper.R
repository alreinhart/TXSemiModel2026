# Workday Platform Scraper
# =========================
# Scrapes job listings from Workday-based career sites via JSON API
# Used by: Applied Materials, NXP Semiconductors, GlobalFoundries
# Filters to US-only jobs and parses description HTML for details

library(httr)
library(jsonlite)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)

source("config/scraper_config.R")
source("scrapers/utils.R")

#' Extract the tenant name from a Workday careers URL
#'
#' Workday URLs follow pattern: https://{tenant}.wd{N}.myworkdayjobs.com/{site}
#'
#' @param careers_url Full careers URL (e.g., https://amat.wd1.myworkdayjobs.com/External)
#' @return Tenant name string (e.g., "amat")
extract_workday_tenant <- function(careers_url) {
  match <- str_match(careers_url, "https?://([^.]+)\\.wd\\d+\\.myworkdayjobs\\.com")
  if (is.na(match[1, 2])) {
    log_message("Could not extract tenant from Workday URL", level = "ERROR")
    return(NULL)
  }
  match[1, 2]
}

#' Extract the site name from a Workday careers URL
#'
#' @param careers_url Full careers URL
#' @return Site name string (e.g., "External")
extract_workday_site <- function(careers_url) {
  match <- str_match(careers_url, "myworkdayjobs\\.com/([^/?]+)")
  if (is.na(match[1, 2])) {
    log_message("Could not extract site from Workday URL, defaulting to 'External'", level = "WARN")
    return("External")
  }
  match[1, 2]
}

#' Extract the base host URL from a Workday careers URL
#'
#' @param careers_url Full careers URL
#' @return Base host URL (e.g., "https://amat.wd1.myworkdayjobs.com")
extract_workday_base <- function(careers_url) {
  str_extract(careers_url, "^https?://[^/]+")
}

#' Construct the Workday CXS API URL for job listings
#'
#' @param careers_url Full careers URL
#' @return API endpoint URL for job search
construct_workday_api_url <- function(careers_url) {
  base <- extract_workday_base(careers_url)
  tenant <- extract_workday_tenant(careers_url)
  site <- extract_workday_site(careers_url)

  if (is.null(tenant)) return(NULL)

  paste0(base, "/wday/cxs/", tenant, "/", site, "/jobs")
}

#' Construct the Workday CXS API URL for a single job's details
#'
#' @param careers_url Full careers URL
#' @param external_path The externalPath from the job listing (e.g., "/job/Austin/Title_R123")
#' @return API endpoint URL for job details
construct_workday_detail_url <- function(careers_url, external_path) {
  base <- extract_workday_base(careers_url)
  tenant <- extract_workday_tenant(careers_url)
  site <- extract_workday_site(careers_url)

  if (is.null(tenant)) return(NULL)

  paste0(base, "/wday/cxs/", tenant, "/", site, external_path)
}

#' Fetch a page of jobs from the Workday CXS JSON API
#'
#' @param api_url Full API endpoint URL
#' @param offset Pagination offset
#' @param limit Number of jobs per page
#' @param country_facet List with $id and $param from find_us_country_facet (NULL for no filter)
#' @return Parsed JSON response or NULL on failure
fetch_workday_job_page <- function(api_url, offset = 0, limit = 20, country_facet = NULL, search_text = "", extra_facets = list()) {

  # Build request body
  applied_facets <- list()
  if (!is.null(country_facet)) {
    applied_facets[[country_facet$param]] <- list(country_facet$id)
  }
  # Merge extra facets (e.g., jobFamilyGroup filters)
  for (fname in names(extra_facets)) {
    applied_facets[[fname]] <- extra_facets[[fname]]
  }

  body <- list(
    appliedFacets = applied_facets,
    limit = limit,
    offset = offset,
    searchText = search_text
  )

  log_message(paste("Fetching Workday API: offset =", offset), level = "DEBUG")

  response <- tryCatch({
    POST(
      api_url,
      body = toJSON(body, auto_unbox = TRUE),
      encode = "raw",
      content_type_json(),
      timeout(SCRAPER_CONFIG$request_timeout),
      user_agent(SCRAPER_CONFIG$user_agent),
      add_headers(
        "Accept" = "application/json",
        "Accept-Language" = "en-US,en;q=0.5",
        "Accept-Encoding" = "gzip, deflate, br",
        "Connection" = "keep-alive"
      )
    )
  }, error = function(e) {
    log_message(paste("Workday API request failed:", e$message), level = "ERROR")
    return(NULL)
  })

  if (is.null(response)) return(NULL)

  if (status_code(response) != 200) {
    log_message(paste("Workday API returned HTTP", status_code(response)), level = "WARN")
    return(NULL)
  }

  json_text <- content(response, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(json_text, simplifyVector = FALSE)

  return(parsed)
}

#' Discover the US country facet ID and parameter name from the first API response
#'
#' Different Workday tenants may use different facet parameter names
#' (e.g., "Country" for Applied Materials, "Location_Country" for NXP).
#'
#' @param parsed Parsed JSON response from the jobs endpoint
#' @return List with $id (facet ID string) and $param (facet parameter name), or NULL
find_us_country_facet <- function(parsed) {
  facets <- parsed$facets
  if (is.null(facets)) return(NULL)

  # Try known country facet parameter names
  country_params <- c("Country", "Location_Country", "country",
                      "countryRegion", "locationCountry", "Country_Region",
                      "location_country", "countryISOCode")

  for (facet in facets) {
    param <- facet$facetParameter
    if (!is.null(param) && param %in% country_params) {
      for (val in facet$values) {
        if (!is.null(val$descriptor) && grepl("United States", val$descriptor, ignore.case = TRUE)) {
          return(list(id = val$id, param = param))
        }
      }
    }
  }

  return(NULL)
}

#' Parse a single job posting from the Workday API response
#'
#' @param job A single job list element from jobPostings
#' @param careers_url Base careers URL for constructing links
#' @return A one-row tibble
parse_workday_job <- function(job, careers_url) {

  title <- job$title
  if (is.null(title) || nchar(title) == 0) return(NULL)

  location <- if (!is.null(job$locationsText)) job$locationsText else NA_character_
  external_path <- job$externalPath

  # Construct human-readable job URL
  base <- extract_workday_base(careers_url)
  site <- extract_workday_site(careers_url)
  job_url <- paste0(base, "/", site, external_path)

  # Parse posted date from relative text like "Posted 3 Days Ago"
  posted_date <- tryCatch({
    parse_date_string(job$postedOn)
  }, error = function(e) NA_Date_)

  # Extract job req ID from bulletFields if available
  job_req_id <- NA_character_
  if (!is.null(job$bulletFields) && length(job$bulletFields) > 0) {
    job_req_id <- job$bulletFields[[1]]
  }

  tibble(
    job_title = title,
    location = location,
    job_url = job_url,
    external_path = external_path,
    posting_date = posted_date,
    job_req_id = job_req_id
  )
}

#' Scrape US jobs from a Workday career site via JSON API
#'
#' @param company_name Name of the company
#' @param careers_url Full Workday careers URL
#' @param max_pages Maximum number of pages to scrape
#' @return Dataframe of US-only job listings
scrape_workday <- function(company_name, careers_url, max_pages = SCRAPER_CONFIG$max_pages_per_company) {

  log_message(paste("Starting Workday API scrape for", company_name, "(US only)"))

  # Extract search query from URL if present (e.g., ?q=Texas+Institute+for+Electronics)
  search_text <- ""
  parsed_url <- httr::parse_url(careers_url)
  if (!is.null(parsed_url$query$q)) {
    search_text <- parsed_url$query$q
    log_message(paste("Using search filter:", search_text))
  }

  # Extract facet filters from URL (e.g., ?facet_jobFamilyGroup=id1,id2,id3)
  extra_facets <- list()
  for (param_name in names(parsed_url$query)) {
    if (startsWith(param_name, "facet_")) {
      facet_param <- sub("^facet_", "", param_name)
      facet_ids <- strsplit(parsed_url$query[[param_name]], ",")[[1]]
      extra_facets[[facet_param]] <- as.list(facet_ids)
      log_message(paste("Using facet filter:", facet_param, "with", length(facet_ids), "values"))
    }
  }

  # Extract state filter for location post-filtering (e.g., ?states=Maryland,California,Florida)
  state_filter <- c()
  if (!is.null(parsed_url$query$states)) {
    state_filter <- strsplit(parsed_url$query$states, ",")[[1]]
    log_message(paste("Will post-filter to states:", paste(state_filter, collapse = ", ")))
  }

  # Strip all query params from careers_url for API construction
  if (length(parsed_url$query) > 0) {
    url_path <- parsed_url$path
    if (!startsWith(url_path, "/")) url_path <- paste0("/", url_path)
    careers_url <- paste0(parsed_url$scheme, "://", parsed_url$hostname, url_path)
  }

  api_url <- construct_workday_api_url(careers_url)
  if (is.null(api_url)) {
    log_message("Could not construct API URL", level = "ERROR")
    return(tibble(
      job_title = character(), location = character(), job_url = character(),
      external_path = character(), posting_date = as.Date(character()),
      job_req_id = character()
    ))
  }

  # First request without country filter to discover facet IDs
  first_page <- fetch_workday_job_page(api_url, offset = 0, limit = 1, search_text = search_text, extra_facets = extra_facets)
  if (is.null(first_page)) {
    log_message("Failed to fetch initial page for facet discovery", level = "ERROR")
    return(tibble(
      job_title = character(), location = character(), job_url = character(),
      external_path = character(), posting_date = as.Date(character()),
      job_req_id = character()
    ))
  }

  # Skip US country facet filtering when search text or extra facets are used
  us_facet <- NULL
  if (nchar(search_text) == 0 && length(extra_facets) == 0) {
    us_facet <- find_us_country_facet(first_page)
    if (!is.null(us_facet)) {
      log_message(paste("Found US country facet:", us_facet$param, "=", us_facet$id))
    } else {
      log_message("Could not find US country facet - fetching all jobs", level = "WARN")
    }
  } else if (length(extra_facets) > 0) {
    log_message(paste("Facet-filtered mode: skipping country facet, total matches:", first_page$total))
  } else {
    log_message(paste("Search-filtered mode: skipping country facet, total matches:", first_page$total))
  }

  Sys.sleep(SCRAPER_CONFIG$delay_between_requests)

  all_jobs <- list()
  offset <- 0
  limit <- 20
  total_jobs <- NULL
  page <- 1

  while (page <= max_pages) {

    log_message(paste("Scraping page", page, "for", company_name, "(offset:", offset, ")"))

    parsed <- fetch_workday_job_page(api_url, offset, limit, us_facet, search_text, extra_facets)

    if (is.null(parsed)) {
      log_message("Failed to fetch page - stopping", level = "WARN")
      break
    }

    if (is.null(total_jobs)) {
      total_jobs <- parsed$total
      if (!is.null(total_jobs)) {
        log_message(paste("Total US jobs reported by API:", total_jobs))
      }
    }

    postings <- parsed$jobPostings
    if (is.null(postings) || length(postings) == 0) {
      log_message("No more job postings - stopping")
      break
    }

    # Parse each job
    page_jobs <- list()
    for (job in postings) {
      parsed_job <- parse_workday_job(job, careers_url)
      if (!is.null(parsed_job)) {
        page_jobs <- c(page_jobs, list(parsed_job))
      }
    }

    if (length(page_jobs) > 0) {
      page_df <- bind_rows(page_jobs)
      all_jobs <- c(all_jobs, list(page_df))
      log_message(paste("  Found", nrow(page_df), "jobs on this page"))
    }

    # Check if we've fetched all jobs
    fetched_so_far <- offset + length(postings)
    if (!is.null(total_jobs) && fetched_so_far >= total_jobs) {
      log_message(paste("Reached all", total_jobs, "jobs"))
      break
    }

    if (length(postings) < limit) {
      log_message("Last page (fewer results than limit)")
      break
    }

    offset <- offset + limit
    page <- page + 1
    Sys.sleep(SCRAPER_CONFIG$delay_between_requests)
  }

  if (length(all_jobs) > 0) {
    jobs_df <- bind_rows(all_jobs)
    log_message(paste("Found", nrow(jobs_df), "jobs for", company_name, "(pre-filter)"))

    # Location post-filter: when using search text for state filtering (e.g., "TX"),
    # remove false positives where the location doesn't match the state
    if (nchar(search_text) > 0 && nchar(search_text) <= 3 && grepl("^[A-Z]{2,3}$", search_text)) {
      state_pattern <- paste0("US-", search_text)
      before_count <- nrow(jobs_df)
      jobs_df <- jobs_df %>% filter(grepl(state_pattern, location, fixed = TRUE) | is.na(location))
      filtered_count <- before_count - nrow(jobs_df)
      if (filtered_count > 0) {
        log_message(paste("Location filter removed", filtered_count, "non-", search_text, "jobs"))
      }
      log_message(paste("Found", nrow(jobs_df), "TX-located jobs for", company_name))
    }

    # State-name post-filter (e.g., states=Maryland,California,Florida)
    # Matches location strings like "United States-Maryland-Baltimore"
    if (length(state_filter) > 0) {
      state_pattern <- paste(state_filter, collapse = "|")
      before_count <- nrow(jobs_df)
      jobs_df <- jobs_df %>% filter(grepl(state_pattern, location, ignore.case = TRUE) | is.na(location))
      filtered_count <- before_count - nrow(jobs_df)
      if (filtered_count > 0) {
        log_message(paste("State filter removed", filtered_count, "jobs not in:",
                          paste(state_filter, collapse = ", ")))
      }
      log_message(paste("Found", nrow(jobs_df), "jobs in target states for", company_name))
    }

    # Fallback US-only filter: when no country facet was discovered and no other location
    # filters are active, use the external_path to identify US jobs. Workday encodes the
    # city/state in the path (e.g. "/job/Richardson-TX/..." or "/job/Boise-ID---Main-Site/...").
    # A trailing two-letter US state code in the path segment reliably distinguishes US jobs
    # from international ones (e.g. "/job/Taichung---MTB-Taiwan/..." or "/job/Fab-10NX-Singapore/...").
    if (is.null(us_facet) && length(state_filter) == 0 && nchar(search_text) == 0) {
      us_state_codes <- paste0(
        "AL|AK|AZ|AR|CA|CO|CT|DE|FL|GA|HI|ID|IL|IN|IA|KS|KY|LA|ME|MD|",
        "MA|MI|MN|MS|MO|MT|NE|NV|NH|NJ|NM|NY|NC|ND|OH|OK|OR|PA|RI|SC|",
        "SD|TN|TX|UT|VT|VA|WA|WV|WI|WY|DC"
      )
      # Match state code only in the CITY segment of the path (between /job/ and the next /).
      # Workday paths are: /job/{City-ST}/job-title_JRXXXXX  or  /job/{City-ST---Suffix}/...
      # We extract the city segment to avoid false positives from state codes in job title words
      # (e.g. "-ME-" in "ENGINEER-ME-FST-PAC" matching Maine).
      us_path_pattern <- paste0("/job/[^/]*-(", us_state_codes, ")(?:/|---|$|-[^A-Z]|-[A-Z][a-z])")
      # Also match ", ST" at end of location text (e.g. "Richardson, TX")
      us_loc_pattern  <- paste0(",\\s*(", us_state_codes, ")\\s*$")
      before_count <- nrow(jobs_df)
      jobs_df <- jobs_df %>%
        filter(
          grepl(us_path_pattern, external_path, perl = TRUE) |
          grepl(us_loc_pattern,  location,      perl = TRUE) |
          is.na(location)
        )
      filtered_count <- before_count - nrow(jobs_df)
      if (filtered_count > 0) {
        log_message(paste("US fallback filter removed", filtered_count,
                          "non-US jobs (no country facet available)"))
      }
      log_message(paste("Found", nrow(jobs_df), "US jobs for", company_name, "(fallback filter)"))
    }
  } else {
    jobs_df <- tibble(
      job_title = character(), location = character(), job_url = character(),
      external_path = character(), posting_date = as.Date(character()),
      job_req_id = character()
    )
    log_message(paste("No US jobs found for", company_name), level = "WARN")
  }

  return(jobs_df)
}

#' Strip the standard Workday boilerplate from description HTML
#'
#' Handles boilerplate from multiple companies:
#' - Applied Materials: "Who We Are" / "What We Offer" header, "Additional Information" footer
#' - NXP: "More information about NXP..." footer, EEO text, #LI-xxxx tags
#'
#' @param html_string Raw HTML job description
#' @return HTML string with boilerplate removed
strip_workday_boilerplate <- function(html_string) {

  if (is.null(html_string) || nchar(html_string) == 0) return(html_string)

  # Normalize common HTML entities
  html_string <- str_replace_all(html_string, "&#39;", "'")
  html_string <- str_replace_all(html_string, "&#43;", "+")
  html_string <- str_replace_all(html_string, "&#64;", "@")
  html_string <- str_replace_all(html_string, "&amp;", "&")

  # --- Applied Materials header boilerplate ---
  header_end_patterns <- c(
    "(?s)^.*?Learn more about our\\s*<a[^>]*>\\s*<u>?benefits</u>?\\s*</a>\\s*\\.?\\s*</p>",
    "(?s)^.*?care for you at work, at home, or wherever you may go\\..*?</p>"
  )

  # --- Samsung header boilerplate ---
  samsung_header_patterns <- c(
    "(?si)<p[^>]*>\\s*<b>\\s*About Samsung.*?</p>(\\s*<p[^>]*>\\s*</p>)*\\s*(<p[^>]*>\\s*<b>\\s*Come innovate with us!?\\s*</b>\\s*</p>)?(\\s*<p[^>]*>\\s*</p>)*"
  )

  result <- html_string
  for (pat in header_end_patterns) {
    stripped <- tryCatch(
      sub(pat, "", result, perl = TRUE),
      error = function(e) NULL
    )
    if (!is.null(stripped) && nchar(stripped) < nchar(result)) {
      result <- stripped
      result <- sub("^(\\s*<p[^>]*>\\s*(&nbsp;|\\s)*</p>\\s*)*", "", result, perl = TRUE)
      break
    }
  }

  # Strip Samsung header boilerplate
  for (pat in samsung_header_patterns) {
    stripped <- tryCatch(sub(pat, "", result, perl = TRUE), error = function(e) NULL)
    if (!is.null(stripped) && nchar(stripped) < nchar(result)) {
      result <- stripped
      result <- sub("^(\\s*<p[^>]*>\\s*(&nbsp;|\\s)*</p>\\s*)*", "", result, perl = TRUE)
      break
    }
  }

  # --- GlobalFoundries header boilerplate ---
  gf_header_patterns <- c(
    "(?si)<p[^>]*>\\s*(?:<b>\\s*)?About GlobalFoundries.*?</p>(\\s*<p[^>]*>\\s*</p>)*",
    "(?si)<p[^>]*>\\s*GlobalFoundries\\s*\\(GF\\)\\s+is.*?</p>(\\s*<p[^>]*>\\s*</p>)*"
  )
  for (pat in gf_header_patterns) {
    stripped <- tryCatch(sub(pat, "", result, perl = TRUE), error = function(e) NULL)
    if (!is.null(stripped) && nchar(stripped) < nchar(result)) {
      result <- stripped
      result <- sub("^(\\s*<p[^>]*>\\s*(&nbsp;|\\s)*</p>\\s*)*", "", result, perl = TRUE)
      break
    }
  }

  # --- Micron header boilerplate: "Our vision is to transform..." + company description ---
  # Micron often wraps text in <span> inside <p>, so we match the text content not the outer tag.
  # Strip from wherever the vision sentence appears through the end of the Micron company paragraph.
  micron_header_patterns <- c(
    # Full two-sentence intro (vision + Micron Technology sentence)
    "(?si)(<[^>]+>\\s*)*Our vision is to transform how the world uses information.*?advance faster than ever\\.\\s*(</[^>]+>\\s*)*(<p[^>]*>\\s*(&nbsp;|</p>|\\s)*)*",
    # Vision sentence alone, flexible inner tags
    "(?si)(<[^>]+>\\s*)*Our vision is to transform how the world uses information[^.]*\\.\\s*(</[^>]+>\\s*)*(<p[^>]*>\\s*(&nbsp;|</p>|\\s)*)*",
    # Micron Technology world-leader sentence if vision already stripped or absent
    "(?si)(<[^>]+>\\s*)*Micron Technology is a world leader in innovating memory and storage solutions.*?advance faster than ever\\.\\s*(</[^>]+>\\s*)*(<p[^>]*>\\s*(&nbsp;|</p>|\\s)*)*"
  )
  for (pat in micron_header_patterns) {
    stripped <- tryCatch(gsub(pat, "", result, perl = TRUE), error = function(e) NULL)
    if (!is.null(stripped) && nchar(stripped) < nchar(result)) {
      result <- stripped
      result <- sub("^(\\s*<p[^>]*>\\s*(&nbsp;|\\s)*</p>\\s*)*", "", result, perl = TRUE)
      break
    }
  }

  # --- RTX header boilerplate: strip metadata fields before "What You Will Do" ---
  # RTX format has Date Posted, Country, Location, Position Role Type, Security Clearance fields
  rtx_header <- regexpr("(?si)<p[^>]*>\\s*<b>\\s*What You Will Do\\s*</b>\\s*</p>", result, perl = TRUE)
  if (rtx_header > 0) {
    result <- substring(result, rtx_header)
  }

  # --- NGC header boilerplate: strip metadata + "At Northrop Grumman" intro ---
  # NGC starts with RELOCATION ASSISTANCE/CLEARANCE/TRAVEL fields, then <h2><b>Description</b></h2>,
  # then standard "At Northrop Grumman, our employees have incredible opportunities..." paragraph
  ngc_desc_header <- regexpr("(?si)<h2>\\s*<b>\\s*Description\\s*</b>\\s*</h2>", result, perl = TRUE)
  if (ngc_desc_header > 0) {
    # Strip everything before <h2>Description</h2> including the heading itself
    result <- substring(result, ngc_desc_header + attr(ngc_desc_header, "match.length"))
    result <- sub("^(\\s*<p[^>]*>\\s*(&nbsp;|\\s)*</p>\\s*)*", "", result, perl = TRUE)
    # Strip the standard "At Northrop Grumman, our employees have incredible opportunities..." paragraph
    result <- sub("(?si)^\\s*At Northrop Grumman, our employees have incredible opportunities.*?they're making history\\.\\s*(<p[^>]*>\\s*(&nbsp;|\\s)*</p>\\s*)*", "", result, perl = TRUE)
  }

  # --- Footer boilerplate (both companies) ---
  footer_patterns <- c(
    # Applied Materials "Additional Information"
    '(?si)<p[^>]*>\\s*<b>\\s*Additional Information\\s*</b>\\s*</p>.*$',
    '(?si)<b>\\s*Additional Information\\s*</b>.*$',
    # NXP "More information about NXP..."
    '(?si)<p[^>]*>\\s*More information about NXP in the United States.*$',
    '(?si)More information about NXP in the United States.*$',
    # Samsung "Total Rewards" benefits section and everything after
    '(?si)<p[^>]*>\\s*<b>\\s*Total Rewards\\s*</b>.*$',
    # Samsung SEA-format: benefits paragraph starting with "Regular full-time employees"
    '(?si)<p[^>]*>\\s*Regular full-time employees \\(salaried or hourly\\) have access to benefits.*$',
    # Samsung "Life @ Samsung" links and benefits paragraphs
    '(?si)<p[^>]*>\\s*Life\\s+@\\s+Samsung.*$',
    # Samsung "U.S. Export Control Compliance"
    '(?si)<p[^>]*>\\s*<b>\\s*U\\.S\\.\\s+Export Control.*$',
    # Samsung "Trade Secrets Notice"
    '(?si)<p[^>]*>\\s*<b>\\s*Trade Secrets Notice.*$',
    # RTX "What We Offer" section and everything after
    '(?si)<p[^>]*>\\s*<b>\\s*What We Offer\\s*</b>\\s*</p>.*$',
    # NGC "As a full-time employee of Northrop Grumman" benefits section
    '(?si)<p[^>]*>\\s*<b>\\s*(?:<span[^>]*>\\s*)?As a full-time employee of Northrop Grumman.*?</ul>'
  )

  for (pat in footer_patterns) {
    stripped <- tryCatch(
      sub(pat, "", result, perl = TRUE),
      error = function(e) NULL
    )
    if (!is.null(stripped) && nchar(stripped) < nchar(result)) {
      result <- stripped
      break
    }
  }

  # --- EEO / accessibility boilerplate (both companies) ---
  eeo_patterns <- c(
    "(?si)<p[^>]*>(?:(?!</p>).){0,300}Applied Materials is an Equal Opportunity.*$",
    "(?si)<p[^>]*>(?:(?!</p>).){0,300}If you would like to contact us regarding accessibility.*$",
    "(?si)<p[^>]*>(?:(?!</p>).){0,300}Qualified applicants will receive consideration.*$",
    "(?si)<p[^>]*>(?:(?!</p>).){0,300}salary offered to a selected candidate.*$",
    "(?si)<p[^>]*>(?:(?!</p>).){0,300}NXP Semiconductors N\\.V\\. is an equal opportunity.*$",
    "(?si)<p[^>]*>(?:(?!</p>).){0,300}equal opportunity employer.*$",
    # Samsung privacy policy
    "(?si)<p[^>]*>\\s*\\*?\\s*Please visit\\s*<a[^>]*>\\s*Samsung membership.*$",
    # Samsung "At Samsung, we believe that innovation"
    "(?si)<p[^>]*>\\s*<span>\\s*At Samsung, we believe that innovation.*$",
    # Samsung "Reasonable Accommodations"
    "(?si)<p[^>]*>\\s*<u>?\\s*<b>\\s*Reasonable Accommodations.*$",
    # RTX EEO and boilerplate
    "(?si)<p[^>]*>\\s*<span[^>]*>\\s*<i>\\s*RTX is an Equal Opportunity.*$",
    "(?si)<p[^>]*>\\s*<b>\\s*Privacy Policy and Terms:.*$",
    "(?si)<p[^>]*>\\s*<i>\\s*<b>\\s*As part of our commitment to maintaining a secure hiring process.*?</p>",
    "(?si)The salary range for this role is.*?application window\\.",
    "(?si)Hired applicants may be eligible for benefits.*?company.s performance\\.",
    "(?si)This role is a U\\.S\\.-based role.*?benefits will apply\\.",
    "(?si)RTX anticipates the application window closing.*?application window\\.",
    # Samsung "All positions at Samsung Austin Semiconductor" / "All positions at SAS"
    "(?si)<p[^>]*>\\s*All positions at (?:Samsung Austin Semiconductor|SAS) require you to be onsite.*?</p>",
    # GlobalFoundries EEO and benefits
    "(?si)<p[^>]*>(?:(?!</p>).){0,300}GlobalFoundries is an equal opportunity.*$",
    "(?si)<p[^>]*>\\s*(?:<b>\\s*)?An offer of employment.*?</p>.*$",
    # Intel "Benefits at Intel" section and everything after
    '(?si)<(?:b|strong)>\\s*Benefits at Intel\\s*</(?:b|strong)>.*$',
    # Intel Job Type/Shift/Location/Business metadata after job content
    "(?si)<h2>\\s*Job Type:\\s*</h2>.*$",
    # Intel salary range text in description
    "(?si)Annual Salary Range for jobs which could be performed in the US.*$",
    # Intel "Intel is committed to" EEO
    "(?si)Intel is committed to.*$",
    # NGC salary range line and EEO statement
    "(?si)(?:Primary|Senior)\\s+Level\\s+Salary\\s+Range:.*$",
    "(?si)Northrop Grumman is (?:committed to|an Equal Opportunity).*$",
    # NGC salary explanation paragraphs
    "(?si)The above salary range represents a general guideline.*$",
    # NGC application period
    "(?si)The application period for the job is estimated.*$",
    # NGC benefits explanation
    "(?si)Depending on the position, employees may be eligible for overtime.*$",
    # Micron fraud alert disclaimer
    "(?si)<p[^>]*>\\s*(?:<[^>]+>\\s*)*Fraud\\s+alert:?\\s+Micron\\s+advises.*$",
    "(?si)Fraud\\s+alert:?\\s+Micron\\s+advises.*$",
    # Micron "As a world leader in the semiconductor industry" benefits footer
    "(?si)<p[^>]*>(?:[^<]|<(?!p))*As a world(?:wide provider of semiconductor solutions| leader in the semiconductor industry).*$",
    "(?si)As a world(?:wide provider of semiconductor solutions| leader in the semiconductor industry).*$",
    # Micron salary range boilerplate (appears in or after Preferred Qualifications)
    "(?si)<p[^>]*>(?:(?!</p>).){0,200}The US base salary range that Micron Technology estimates.*$",
    "(?si)The US base salary range that Micron Technology estimates.*$"
  )
  for (pat in eeo_patterns) {
    result <- tryCatch(sub(pat, "", result, perl = TRUE), error = function(e) result)
  }

  # Remove #LI-xxxx tags (NXP uses these) and #XX-XXX tags (NGC uses these)
  result <- gsub("#LI-[A-Z0-9]+", "", result, perl = TRUE)
  result <- gsub("#[A-Z]{2}-[A-Z0-9]{2,4}", "", result, perl = TRUE)

  # --- UT Austin structured header boilerplate ---
  # Strip everything before "Job Details:" heading (metadata fields separated by ----)
  ut_header <- regexpr("(?si)<p[^>]*>\\s*<b>\\s*Job Details:\\s*</b>\\s*</p>", result, perl = TRUE)
  if (ut_header > 0) {
    result <- substring(result, ut_header + attr(ut_header, "match.length"))
    result <- sub("^(\\s*<p[^>]*>\\s*(&nbsp;|\\s)*</p>\\s*)*", "", result, perl = TRUE)
  }

  # Strip UT Austin "General Notes" section (org description + benefits list)
  ut_general_notes <- "(?si)<h2>\\s*General Notes\\s*</h2>.*?(?=<h2>\\s*Purpose\\s*</h2>|<h2>\\s*Responsibilities\\s*</h2>|<h2>\\s*Required Qualifications\\s*</h2>)"
  result <- tryCatch(sub(ut_general_notes, "", result, perl = TRUE), error = function(e) result)

  # Strip UT Austin footer: everything from "Required Materials" or "Working Conditions" section onward
  ut_footer_patterns <- c(
    '(?si)<h2>\\s*Required Materials\\s*</h2>.*$',
    '(?si)<p[^>]*>\\s*<b>\\s*Employment Eligibility:\\s*</b>\\s*</p>.*$',
    '(?si)<p[^>]*>\\s*<b>\\s*Retirement Plan Eligibility:\\s*</b>\\s*</p>.*$',
    '(?si)<p[^>]*>\\s*<b>\\s*Background Checks:\\s*</b>\\s*</p>.*$',
    '(?si)<p[^>]*>\\s*<b>\\s*Equal Opportunity Employer:\\s*</b>\\s*</p>.*$'
  )
  for (pat in ut_footer_patterns) {
    stripped <- tryCatch(sub(pat, "", result, perl = TRUE), error = function(e) NULL)
    if (!is.null(stripped) && nchar(stripped) < nchar(result)) {
      result <- stripped
      break
    }
  }

  # Strip UT Austin "Working Conditions" and "Work Shift" sections
  result <- tryCatch(sub("(?si)<h2>\\s*Working Conditions\\s*</h2>.*?(?=<h2>|$)", "", result, perl = TRUE), error = function(e) result)
  result <- tryCatch(sub("(?si)<h2>\\s*Work Shift\\s*</h2>.*?(?=<h2>|$)", "", result, perl = TRUE), error = function(e) result)

  # Strip UT Austin "Salary Range" section (usually just says "TIE Pays Industry Competitive Salaries")
  result <- tryCatch(sub("(?si)<h2>\\s*Salary Range\\s*</h2>.*?(?=<h2>|$)", "", result, perl = TRUE), error = function(e) result)

  # Strip ---- dividers
  result <- gsub('<p[^>]*>\\s*<b>\\s*-+\\s*</b>\\s*</p>', '', result, perl = TRUE)
  result <- gsub('<p[^>]*>\\s*-{3,}\\s*</p>', '', result, perl = TRUE)
  result <- gsub('<p>\\s*<b>-+</b>\\s*</p>', '', result, perl = TRUE)

  str_trim(result)
}

#' Extract text content of a section that starts with a bold heading
#'
#' Finds the bold heading in the HTML, extracts everything between it and
#' the next bold heading (or end of content). Returns bullet items if <li>
#' elements exist, otherwise returns paragraph text.
#'
#' @param html_string Raw HTML string (should already have boilerplate stripped)
#' @param heading_patterns Vector of regex patterns to match bold headings
#' @param bullets_only If TRUE, only return content if <li> items found
#' @return Character string or NA
extract_workday_section <- function(html_string, heading_patterns, bullets_only = FALSE, allow_bold_subheadings = FALSE) {

  if (is.null(html_string) || is.na(html_string) || nchar(html_string) == 0) {
    return(NA_character_)
  }

  # Normalize common HTML entities so regex patterns can match consistently
  html_string <- str_replace_all(html_string, "&#39;", "'")
  html_string <- str_replace_all(html_string, "&#43;", "+")
  html_string <- str_replace_all(html_string, "&#64;", "@")
  html_string <- str_replace_all(html_string, "&amp;", "&")

  # Check plain text for heading existence first
  plain_text <- str_replace_all(html_string, "<[^>]+>", " ")
  plain_text <- str_replace_all(plain_text, "&[a-zA-Z0-9#]+;", " ")
  plain_text <- str_replace_all(plain_text, "\\s+", " ")

  for (pattern in heading_patterns) {
    if (!grepl(pattern, plain_text, ignore.case = TRUE, perl = TRUE)) next

    # Split HTML at the heading.
    # Also build an HTML-aware version of the pattern that allows HTML tags interspersed
    # between words — needed when Workday splits heading words across separate <span>
    # elements (e.g., Micron: <b><span>Minimum</span><span> Requirements:</span></b>).
    # Replace \s+ with a pattern that matches inter-word content across HTML span
    # boundaries, including non-breaking spaces (U+00A0, U+202F) that PCRE \s misses.
    # (?:[^<>]*<[^>]*>)*[^<>]* matches any sequence of text+tags between heading words.
    html_p <- gsub("\\s+", "(?:[^<>]{0,30}<[^>]*>){0,8}[^<>]{0,30}", pattern, fixed = TRUE)
    split_patterns <- c(
      paste0("(?i)(?:<[^>]*>\\s*)*", pattern, "\\s*(?:</[^>]*>\\s*)*(?:&nbsp;)?\\s*(?:</[^>]*>)*"),
      paste0("(?i)(?:<[^>]*>\\s*)*", html_p,  "\\s*(?:</[^>]*>\\s*)*(?:&nbsp;)?\\s*(?:</[^>]*>)*"),
      paste0("(?i)", pattern, "\\s*(?:&nbsp;)?\\s*(?:</[^>]*>\\s*)*"),
      paste0("(?i)", html_p,  "\\s*(?:&nbsp;)?\\s*(?:</[^>]*>\\s*)*")
    )

    for (sp in split_patterns) {
      parts <- tryCatch(strsplit(html_string, sp, perl = TRUE)[[1]], error = function(e) NULL)
      if (is.null(parts) || length(parts) < 2) next

      # When strsplit produces multiple splits (heading text appears more than once),
      # try parts in REVERSE order (last first), since the actual bold heading is
      # typically the last occurrence. Earlier matches are often in explanatory text
      # like "Minimum qualifications are required to be considered..."
      for (part_idx in length(parts):2) {
      after_heading <- parts[part_idx]

      # Truncate at the next section heading (bold tag or plain-text heading pattern)
      # When allow_bold_subheadings = TRUE (for qualification sections), only truncate
      # at known major section headings in bold, not arbitrary bold text like sub-labels
      # (e.g., "Technical expertise:", "Soft skills:" within a qualifications section)
      if (allow_bold_subheadings) {
        bold_pattern <- paste0(
          "(?i)<(?:strong|b|h[1-6])>\\s*(?:<[^>]*>\\s*)*(",
          "Key\\s+Responsibilities|Responsibilities|Primary\\s+Responsibilities|",
          "Job\\s+Description|Qualifications|Job\\s+Qualifications|",
          "Basic\\s+Qualifications|Minimum\\s+Qualifications|Minimum\\s+Requirements|Preferred\\s+Qualifications|Preferred\\s+Skills|Preferred\\s+Requirements|",
          "Required\\s+Qualifications|Attributes|Education\\s+and\\s+Skills|",
          "Education\\s+Requirements|Your\\s+Background|What\\s+Sets\\s+You\\s+Apart|",
          "How\\s+to\\s+Qualify|What.s\\s+Encouraged\\s+Daily|Required\\s+Skills|Preferred\\s+Experience|",
          "Additional\\s+Information|Job\\s+Summary|Key\\s+Challenges|",
          "About\\s+the\\s+role|Role\\s+Overview|",
          "What\\s+you.ll\\s+do|What\\s+you\\s+bring|Minimum\\s+requirements|",
          "Functional\\s+Knowledge|Business\\s+Expertise|",
          "Functional\\s+(?:&(?:amp;)?|and)\\s+Business\\s+Expertise|",
          "Experience\\s+Required|Impact\\s+(?:&(?:amp;)?|and)\\s+Interpersonal\\s+Skills|Objectives|",
          "Position\\s+Summary|Role\\s+and\\s+Responsibilities|Skills\\s+and\\s+Qualifications|",
          "Here.s\\s+What\\s+You.ll\\s+Be\\s+Responsible\\s+For|Here.s\\s+what\\s+you.ll\\s+need|",
          "We\\s+are\\s+looking\\s+for|Total\\s+Rewards|Duties\\s+of\\s+the\\s+job|Core\\s+[Cc]ompetencies|",
          "Competencies\\s+and\\s+Skills|Technical\\s+Proficiencies|",
          "Purpose|Salary\\s+Range|Working\\s+Conditions|Work\\s+Shift|Required\\s+Materials|",
          "What\\s+You\\s+Will\\s+Do|Basic\\s+Qualifications|Qualifications\\s+You\\s+Must\\s+Have|Qualifications\\s+We\\s+Prefer|What\\s+We\\s+Offer|",
          "What\\s+you\\s+should\\s+have|The\\s+following\\s+are\\s+a\\s+PLUS|",
          "The\\s+ideal\\s+candidate\\s+should\\s+demonstrate|Benefits\\s+at\\s+Intel|",
          "Additional\\s+Qualifications)"
        )
      } else {
        # Default: truncate at ANY bold text starting with a capital letter (aggressive)
        bold_pattern <- "(?i)<(?:strong|b|h[1-6])>\\s*(?:<[^>]*>\\s*)*[A-Z][^<]{2,}"
      }
      next_section_patterns <- c(
        bold_pattern,
        # Plain text heading on its own paragraph: <p>Heading:</p> or <p>Heading:<br>
        "(?i)<p[^>]*>\\s*(?:Job\\s+Qualifications|Qualifications|Attributes|Basic\\s+Qualifications|Minimum\\s+Qualifications|Minimum\\s+Requirements|Preferred\\s+Qualifications|Preferred\\s+Skills|Preferred\\s+Requirements|Preferred\\s+Experience|Required\\s+Qualifications|Required\\s+Skills|Key\\s+Responsibilities|Primary\\s+Responsibilities|Job\\s+Summary|Key\\s+Challenges|Required\\s+skills|Experience\\s+and\\s+[Ee]ducation|Education\\s+and\\s+Skills|Education\\s+Requirements|Experience(?=\\s*:)|Business\\s+Line|Your\\s+Background|What\\s+Sets\\s+You\\s+Apart|What\\s+You\\s+will\\s+Drive|How\\s+to\\s+Qualify|What.s\\s+Encouraged\\s+Daily|Role\\s+Overview|About\\s+the\\s+role|What\\s+you.ll\\s+do|What\\s+you\\s+bring|Minimum\\s+requirements|Position\\s+Summary|Role\\s+and\\s+Responsibilities|Skills\\s+and\\s+Qualifications|Here.s\\s+What\\s+You.ll\\s+Be\\s+Responsible\\s+For|Here.s\\s+what\\s+you.ll\\s+need|We\\s+are\\s+looking\\s+for|Duties\\s+of\\s+the\\s+job|What\\s+You\\s+Will\\s+Do|Qualifications\\s+You\\s+Must\\s+Have|Qualifications\\s+We\\s+Prefer|What\\s+We\\s+Offer|Core\\s+[Cc]ompetencies|What\\s+you\\s+should\\s+have|The\\s+following\\s+are\\s+a\\s+PLUS|The\\s+ideal\\s+candidate\\s+should\\s+demonstrate|Benefits\\s+at\\s+Intel)\\s*:?"
      )
      next_section <- -1
      for (nsp in next_section_patterns) {
        pos <- regexpr(nsp, after_heading, perl = TRUE)
        if (pos > 0 && (next_section < 0 || pos < next_section)) {
          next_section <- pos
        }
      }
      if (next_section > 0) {
        after_heading <- substr(after_heading, 1, next_section - 1)
      }

      section_doc <- tryCatch(
        read_html(paste0("<div>", after_heading, "</div>")),
        error = function(e) NULL
      )
      if (is.null(section_doc)) next

      # Try bullet items first — only top-level <li> to avoid duplicating nested sub-items
      li_nodes <- section_doc %>% html_elements(xpath = "//li[not(ancestor::li)]")
      li_items <- li_nodes %>% html_text2()
      li_items <- str_squish(li_items)
      li_items <- li_items[nchar(li_items) > 0]

      # Also collect paragraph text before bullet lists (e.g., RTX "Typically requires a Bachelor's...")
      # Exclude <p> tags nested inside <li> to avoid duplicating bullet content
      paras <- section_doc %>% html_elements(xpath = "//p[not(ancestor::li)]") %>% html_text2()
      paras <- str_squish(paras)
      paras <- paras[nchar(paras) > 5]

      if (length(li_items) > 0) {
        # Include any leading paragraph text that appears before the bullet list
        if (length(paras) > 0 && !bullets_only) {
          all_parts <- c(paras, li_items)
          return(paste(all_parts, collapse = "\n"))
        }
        return(paste(li_items, collapse = "\n"))
      }

      if (!bullets_only && length(paras) > 0) {
        return(paste(paras, collapse = "\n"))
      }

      }  # end for (part_idx)
    }
  }

  return(NA_character_)
}

#' Extract content under a plain-text paragraph heading
#'
#' For headings that appear as standalone paragraphs like <p>Heading:</p>,
#' extracts the content paragraphs/lists that follow until the next heading
#' or end of content. More precise than extract_workday_section for
#' plain-text headings that could match content text.
#'
#' @param html_string Raw HTML string
#' @param heading_regex Regex to match the heading text (must match the FULL paragraph text)
#' @return Character string or NA
extract_paragraph_section <- function(html_string, heading_regex) {

  if (is.null(html_string) || is.na(html_string) || nchar(html_string) == 0) {
    return(NA_character_)
  }

  # Split HTML into paragraph-level elements
  # Match <p...>content</p> or <ul>...</ul> or <ol>...</ol>
  elements <- str_match_all(html_string, "(?si)(<(?:p|ul|ol)[^>]*>)(.*?)(</(?:p|ul|ol)>)")[[1]]
  if (nrow(elements) == 0) return(NA_character_)

  # Find the heading paragraph - must match the ENTIRE trimmed paragraph text
  heading_idx <- NA
  for (i in seq_len(nrow(elements))) {
    # Get plain text of this paragraph
    elem_doc <- tryCatch(read_html(paste0("<div>", elements[i, 1], "</div>")), error = function(e) NULL)
    if (is.null(elem_doc)) next
    elem_text <- str_squish(html_text(elem_doc))
    # Check if this paragraph IS the heading (short text matching the pattern)
    if (nchar(elem_text) > 0 && nchar(elem_text) < 80 && grepl(paste0("^", heading_regex, "$"), elem_text, ignore.case = TRUE, perl = TRUE)) {
      heading_idx <- i
      break
    }
  }

  if (is.na(heading_idx)) return(NA_character_)

  # Known heading patterns that signal the start of a new section
  section_headings <- "(?i)^\\s*(Education\\s+and\\s+Skills|Education\\s+Requirements|Experience|Experience\\s+and\\s+[Ee]ducation|Qualifications|Job\\s+Qualifications|Basic\\s+Qualifications|Minimum\\s+Qualifications|Minimum\\s+Requirements|Preferred\\s+Qualifications|Preferred\\s+Requirements|Preferred\\s+Experience|Required\\s+Qualifications|Required\\s+Skills|Attributes|Required\\s+skills|Your\\s+Background|What\\s+Sets\\s+You\\s+Apart|How\\s+to\\s+Qualify|What.s\\s+Encouraged\\s+Daily|Key\\s+Responsibilities|Primary\\s+Responsibilities|Role\\s+Overview|About\\s+the\\s+role|What\\s+you.ll\\s+do|What\\s+you\\s+bring|Minimum\\s+requirements|Position\\s+Summary|Role\\s+and\\s+Responsibilities|Skills\\s+and\\s+Qualifications|Here.s\\s+What\\s+You.ll\\s+Be\\s+Responsible\\s+For|Here.s\\s+what\\s+you.ll\\s+need|We\\s+are\\s+looking\\s+for|Duties\\s+of\\s+the\\s+job|What\\s+You\\s+Will\\s+Do|Qualifications\\s+You\\s+Must\\s+Have|Qualifications\\s+We\\s+Prefer|What\\s+We\\s+Offer|Core\\s+[Cc]ompetencies|What\\s+you\\s+should\\s+have|The\\s+following\\s+are\\s+a\\s+PLUS,?\\s+but\\s+not\\s+required|The\\s+ideal\\s+candidate\\s+should\\s+demonstrate|Benefits\\s+at\\s+Intel)\\s*:?\\s*$"

  # Collect content paragraphs after the heading until next heading or end
  content_parts <- c()
  for (j in (heading_idx + 1):nrow(elements)) {
    elem_doc <- tryCatch(read_html(paste0("<div>", elements[j, 1], "</div>")), error = function(e) NULL)
    if (is.null(elem_doc)) next
    elem_text <- str_squish(html_text(elem_doc))

    # Skip empty paragraphs
    if (nchar(elem_text) == 0) next

    # Stop if we hit another section heading
    if (nchar(elem_text) < 80 && grepl(section_headings, elem_text, perl = TRUE)) break

    # Stop if we hit bold-wrapped heading
    if (grepl("^<(?:b|strong)>", str_trim(elements[j, 3]), perl = TRUE)) break

    # Check for list items
    if (grepl("<li", elements[j, 1], fixed = TRUE)) {
      li_doc <- tryCatch(read_html(paste0("<div>", elements[j, 1], "</div>")), error = function(e) NULL)
      if (!is.null(li_doc)) {
        li_items <- li_doc %>% html_elements("li") %>% html_text2()
        li_items <- str_squish(li_items)
        li_items <- li_items[nchar(li_items) > 0]
        content_parts <- c(content_parts, li_items)
        next
      }
    }

    content_parts <- c(content_parts, elem_text)
  }

  if (length(content_parts) == 0) return(NA_character_)
  paste(content_parts, collapse = "\n")
}

#' Extract the introductory role summary paragraph
#'
#' Finds paragraphs that introduce the role before the structured sections.
#' Handles multiple patterns:
#' - Applied Materials: "As a [title], you'll..."
#' - NXP: "We are looking for...", "NXP is seeking...", "We are seeking...",
#'        "In this role, you will...", or other intro paragraphs before headings
#'
#' @param html_string Raw HTML (boilerplate-stripped)
#' @return The role summary text or NA
extract_workday_role_summary <- function(html_string) {

  if (is.null(html_string) || nchar(html_string) == 0) return(NA_character_)

  # Convert HTML to plain text
  doc <- tryCatch(read_html(paste0("<div>", html_string, "</div>")), error = function(e) NULL)
  if (is.null(doc)) return(NA_character_)

  full_text <- doc %>% html_text2()

  lines <- str_split(full_text, "\n")[[1]]
  lines <- str_squish(lines)

  # Patterns that indicate an intro paragraph
  intro_patterns <- c(
    "^As an? ",
    "^We are (?:looking|seeking) ",
    "^NXP is (?:looking|seeking) ",
    "^We\\s+seek ",
    "^In this role",
    "^This position ",
    "^The .+ (?:is responsible|will be responsible|team is)",
    "^Northrop Grumman (?:Corporation )?is (?:looking|seeking) "
  )

  # Known heading keywords to stop at
  heading_stop <- "^(Key |Role |General |Minimum |Preferred |Functional |Business |Physical |Primary |Job |Qualifications|Attributes|Required |Responsibilities|Description|What you|What's |Position Summary|Skills and Qualifications|Here.s What|Here.s what|We are looking|Duties of|How to Qualify|The following are a PLUS|The ideal candidate|Benefits at Intel)"

  for (pat in intro_patterns) {
    match_idx <- which(grepl(pat, lines, perl = TRUE, ignore.case = FALSE))
    if (length(match_idx) == 0) next

    start <- match_idx[1]
    result_lines <- lines[start]

    if (start < length(lines)) {
      for (i in (start + 1):length(lines)) {
        line <- lines[i]
        if (nchar(line) == 0) break
        if (grepl(heading_stop, line, perl = TRUE)) break
        result_lines <- c(result_lines, line)
      }
    }

    result <- paste(result_lines, collapse = " ")
    result <- str_squish(result)

    if (nchar(result) > 30) return(result)
  }

  return(NA_character_)
}

#' Extract essential knowledge & skills from bold category sections
#'
#' Captures content under: Functional Knowledge, Business Expertise,
#' Leadership, Problem Solving, Impact, Interpersonal Skills, Physical Requirements
#'
#' @param html_string Raw HTML (boilerplate-stripped)
#' @return Named character string with all skills sections, or NA
extract_workday_essential_skills <- function(html_string) {

  if (is.null(html_string) || nchar(html_string) == 0) return(NA_character_)

  skill_headings <- c(
    "Leadership",
    "Problem Solving",
    "Impact",
    "Interpersonal Skills",
    "Physical Requirements"
  )

  # Only extract these if they appear as bold headings in the HTML
  sections <- list()
  for (heading in skill_headings) {
    # Check that this heading actually appears as a bold element
    bold_pattern <- paste0("(?i)<(?:b|strong)>\\s*", gsub(" ", "\\\\s+", heading), "\\s*(?:</(?:b|strong)>)")
    if (!grepl(bold_pattern, html_string, perl = TRUE)) next

    pattern <- gsub(" ", "\\\\s+", heading)
    content <- extract_workday_section(html_string, paste0(pattern, "\\s*:?"))

    if (!is.na(content) && nchar(str_squish(content)) > 0) {
      sections <- c(sections, paste0(heading, ": ", str_squish(content)))
    }
  }

  if (length(sections) == 0) return(NA_character_)

  paste(sections, collapse = "\n")
}

#' Fetch detailed job information from Workday CXS job detail API
#'
#' Strips boilerplate, then parses the cleaned HTML for:
#' - Role summary ("As a [title], you'll...")
#' - Responsibilities (Key Responsibilities / Role Responsibilities)
#' - Minimum education (1st bullet under Minimum Qualifications)
#' - Minimum experience (2nd bullet under Minimum Qualifications)
#' - Preferred qualifications
#' - Essential knowledge & skills (Functional Knowledge, Business Expertise, etc.)
#' - Salary range
#'
#' @param careers_url Base careers URL
#' @param external_path The externalPath for the job
#' @return List with detailed job information
scrape_workday_job_details <- function(careers_url, external_path) {

  log_message(paste("Fetching Workday job details:", external_path), level = "DEBUG")

  detail_url <- construct_workday_detail_url(careers_url, external_path)
  if (is.null(detail_url)) {
    return(list(
      job_identification = NA_character_,
      job_responsibilities = NA_character_,
      min_education = NA_character_,
      min_experience = NA_character_,
      preferred_qualifications = NA_character_,
      salary_range = NA_character_,
      essential_skills = NA_character_
    ))
  }

  response <- tryCatch({
    GET(
      detail_url,
      timeout(SCRAPER_CONFIG$request_timeout),
      user_agent(SCRAPER_CONFIG$user_agent),
      add_headers(
        "Accept" = "application/json",
        "Accept-Language" = "en-US,en;q=0.5",
        "Accept-Encoding" = "gzip, deflate, br",
        "Connection" = "keep-alive"
      )
    )
  }, error = function(e) {
    log_message(paste("Detail request failed:", e$message), level = "WARN")
    return(NULL)
  })

  na_result <- list(
    job_identification = NA_character_,
    job_responsibilities = NA_character_,
    min_education = NA_character_,
    min_experience = NA_character_,
    preferred_qualifications = NA_character_,
    salary_range = NA_character_,
    essential_skills = NA_character_
  )

  if (is.null(response) || status_code(response) != 200) {
    return(na_result)
  }

  json_text <- content(response, as = "text", encoding = "UTF-8")
  detail <- fromJSON(json_text, simplifyVector = FALSE)

  job_info <- detail$jobPostingInfo
  if (is.null(job_info)) return(na_result)

  raw_html <- job_info$jobDescription
  if (is.null(raw_html) || nchar(raw_html) == 0) return(na_result)

  # --- Extract salary BEFORE stripping (it's in the "What We Offer" header) ---
  salary <- NA_character_
  # Normalize &#43; to + for salary matching
  salary_html <- str_replace_all(raw_html, "&#43;", "+")
  # Pattern 1: Range like $100,000-$150,000 or $100,000 and $150,000
  salary_match <- str_match(salary_html, "\\$([0-9,]+(?:\\.\\d{2})?)\\s*(?:[-\u2013]|and)\\s*\\$?([0-9,]+(?:\\.\\d{2})?)")
  if (!is.na(salary_match[1, 1])) {
    salary <- salary_match[1, 1]
  }
  # Pattern 2: Single minimum like $100,000+ or $100,000 + depending on qualifications
  if (is.na(salary)) {
    salary_min_match <- str_match(salary_html, "\\$([0-9,]+(?:\\.\\d{2})?)\\s*\\+")
    if (!is.na(salary_min_match[1, 1])) {
      salary <- salary_min_match[1, 1]
    }
  }
  # Pattern 3: RTX format like "86,800 USD - 165,200 USD" (no $ sign)
  if (is.na(salary)) {
    rtx_sal_match <- str_match(salary_html, "([0-9,]+)\\s*USD\\s*[-\u2013]\\s*([0-9,]+)\\s*USD")
    if (!is.na(rtx_sal_match[1, 1])) {
      salary <- paste0("$", rtx_sal_match[1, 2], "-$", rtx_sal_match[1, 3])
    }
  }

  # Pattern 4: NGC format like "Primary Level Salary Range: $44,600.00 - $74,400.00"
  if (is.na(salary)) {
    ngc_sal_match <- str_match(salary_html, "(?:Primary|Senior)\\s+Level\\s+Salary\\s+Range:\\s*\\$([0-9,.]+)\\s*[-\u2013]\\s*\\$([0-9,.]+)")
    if (!is.na(ngc_sal_match[1, 1])) {
      salary <- paste0("$", ngc_sal_match[1, 2], "-$", ngc_sal_match[1, 3])
    }
  }

  # Pattern 5: Intel format like "Annual Salary Range for jobs which could be performed in the US: $128,880.00-245,160.00 USD"
  if (is.na(salary)) {
    intel_sal_text <- str_replace_all(salary_html, "<[^>]+>", " ")
    intel_sal_match <- str_match(intel_sal_text, "Annual Salary Range[^:]*:\\s*\\$([0-9,.]+)\\s*[-\u2013]\\s*\\$?([0-9,.]+)\\s*USD")
    if (!is.na(intel_sal_match[1, 1])) {
      salary <- paste0("$", intel_sal_match[1, 2], "-$", intel_sal_match[1, 3])
    }
  }

  # --- Extract job identification (jobReqId) ---
  job_identification <- NA_character_
  if (!is.null(job_info$jobReqId) && nchar(job_info$jobReqId) > 0) {
    job_identification <- job_info$jobReqId
  }

  # --- Strip boilerplate ---
  desc_html <- strip_workday_boilerplate(raw_html)

  # --- Extract "As a [title], you'll..." role summary ---
  role_summary <- extract_workday_role_summary(desc_html)

  # --- Extract responsibilities ---
  # Covers Applied Materials (Key/Role Responsibilities) and NXP patterns
  # (Job Description, Primary Responsibilities, Job Summary, Key Challenges, etc.)
  responsibility_patterns <- c(
    "Key\\s+Responsibilities\\s*:?",
    "Role\\s+Responsibilities\\s*:?",
    "Primary\\s+Responsibilities\\s*:?",
    "Primary\\s+Job\\s+Duties\\s*:?",
    "Roles?\\s+and\\s+[Rr]esponsibilities\\s*:?",
    "Job\\s+Description\\s*:?",
    "Responsibilities\\s*:?",
    "What\\s+you.ll\\s+do\\s*:?",
    "Here.s\\s+What\\s+You.ll\\s+Be\\s+Responsible\\s+For\\s*:?",
    "What\\s+You\\s+Will\\s+Do\\s*:?",
    "Core\\s+[Cc]ompetencies\\s*:?",
    "What.s\\s+Encouraged\\s+Daily\\s*:?"
  )
  responsibilities_bullets <- extract_workday_section(desc_html, responsibility_patterns, allow_bold_subheadings = TRUE)

  # Also extract supplementary sections that contribute to responsibilities
  # NXP uses various additional headings for responsibilities
  supplementary_resp_patterns <- list(
    c("Job\\s+Summary\\s*:?"),
    c("Key\\s+Challenges\\s*:?"),
    c("Business\\s+Line\\s+Description\\s*:?"),
    c("Role\\s+Overview\\s*:?"),
    c("What\\s+You\\s+will\\s+Drive\\s*:?"),
    c("About\\s+the\\s+role\\s*:?"),
    c("Position\\s+Summary\\s*:?"),
    c("Duties\\s+of\\s+the\\s+job\\s+include\\s*:?")
  )
  supplementary_parts <- c()
  for (sp_patterns in supplementary_resp_patterns) {
    sp_text <- extract_workday_section(desc_html, sp_patterns, allow_bold_subheadings = TRUE)
    if (!is.na(sp_text)) supplementary_parts <- c(supplementary_parts, sp_text)
  }

  # Combine role summary + supplementary sections + responsibilities bullets
  # With deduplication: if role_summary text is contained within a supplementary part
  # or responsibilities bullets, skip it to avoid repeating the same text
  resp_parts <- c()
  if (length(supplementary_parts) > 0) resp_parts <- c(resp_parts, supplementary_parts)
  if (!is.na(responsibilities_bullets)) resp_parts <- c(resp_parts, responsibilities_bullets)

  if (!is.na(role_summary)) {
    # Check if role_summary is already contained in any other resp_part (whole or line-level)
    snippet <- substr(str_squish(role_summary), 1, min(80, nchar(str_squish(role_summary))))
    all_other_text <- paste(resp_parts, collapse = "\n")
    is_dup <- length(resp_parts) > 0 && grepl(snippet, all_other_text, fixed = TRUE)
    if (!is_dup) {
      resp_parts <- c(role_summary, resp_parts)  # prepend summary before other sections
    }
  }

  # If no structured responsibility sections were found (only role_summary or nothing),
  # extract all paragraphs before the first qualification heading as the description
  has_structured_resp <- !is.na(responsibilities_bullets) || length(supplementary_parts) > 0
  if (!has_structured_resp && !is.null(desc_html) && nchar(desc_html) > 0) {
    # Truncate HTML at the first qualification-like heading to avoid mixing in quals
    truncated_html <- desc_html
    qual_heading_patterns <- c(
      "(?i)<p[^>]*>\\s*(?:Education\\s+and\\s+Skills|Education\\s+Requirements|Qualifications|Job\\s+Qualifications|Attributes|Minimum\\s+Qualifications|Required\\s+Qualifications|Your\\s+Background|Required\\s+skills|What\\s+you\\s+bring|Minimum\\s+requirements|How\\s+to\\s+Qualify|Skills\\s+and\\s+Qualifications|Here.s\\s+what\\s+you.ll\\s+need|We\\s+are\\s+looking\\s+for|What\\s+you\\s+should\\s+have)\\s*:?\\s*</p>",
      "(?i)<(?:b|strong|h[1-6])>\\s*(?:<[^>]*>\\s*)*(?:Education|Qualifications|Attributes|Required|Your\\s+Background|Minimum|How\\s+to\\s+Qualify|What\\s+you\\s+bring|Skills\\s+and\\s+Qualifications|Here.s\\s+what|We\\s+are\\s+looking|What\\s+you\\s+should\\s+have)\\s*"
    )
    for (qhp in qual_heading_patterns) {
      pos <- regexpr(qhp, truncated_html, perl = TRUE)
      if (pos > 0) {
        truncated_html <- substr(truncated_html, 1, pos - 1)
        break
      }
    }

    doc <- tryCatch(read_html(paste0("<div>", truncated_html, "</div>")), error = function(e) NULL)
    if (!is.null(doc)) {
      all_text <- doc %>% html_text2()
      lines <- str_split(all_text, "\n")[[1]]
      lines <- str_squish(lines)
      lines <- lines[nchar(lines) > 0]
      all_text <- paste(lines, collapse = "\n")
      if (nchar(all_text) > 30) {
        # Replace role_summary with the fuller pre-heading text (which includes it)
        resp_parts <- c(all_text)
      }
    }
  }

  # Deduplicate lines within responsibilities (e.g., Job Summary appearing in both
  # role_summary and supplementary section)
  if (length(resp_parts) > 0) {
    combined_text <- paste(resp_parts, collapse = "\n\n")
    resp_lines <- str_split(combined_text, "\n")[[1]]
    # Remove duplicate lines (keep first occurrence), only for lines > 20 chars
    seen <- character(0)
    deduped_lines <- c()
    for (line in resp_lines) {
      trimmed <- str_squish(line)
      if (nchar(trimmed) > 20 && trimmed %in% seen) next
      if (nchar(trimmed) > 20) seen <- c(seen, trimmed)
      deduped_lines <- c(deduped_lines, line)
    }
    responsibilities <- paste(deduped_lines, collapse = "\n")
  } else {
    responsibilities <- NA_character_
  }

  # --- Extract "The ideal candidate should demonstrate:" section (Intel pattern) ---
  # This appears between responsibilities and qualifications, content goes to experience
  ideal_candidate_text <- NA_character_
  ideal_candidate_patterns <- c(
    "The\\s+ideal\\s+candidate\\s+should\\s+demonstrate\\s*:?"
  )
  ideal_candidate_text <- extract_workday_section(desc_html, ideal_candidate_patterns, allow_bold_subheadings = TRUE)

  # --- Extract Minimum Qualifications (1st bullet = education, 2nd+ = experience) ---
  min_education <- NA_character_
  min_experience <- NA_character_
  extra_preferred_bullets <- character(0)  # "preferred" clauses found inside min_qual bullets

  min_qual_patterns <- c(
    "Minimum\\s+Qualifications\\s*:?",
    "Min\\s+Qualifications\\s*:?",
    "Minimum\\s+Requirements\\s*:?",
    "Required\\s+Qualifications\\s*:?",
    "Required\\s+Experience\\s*:?",
    "Basic\\s+Qualifications\\s*:?",
    "Here.s\\s+what\\s+you.ll\\s+need\\s*:?",
    "Qualifications\\s+You\\s+Must\\s+Have\\s*:?",
    "Job\\s+requirements\\s*:?",
    "What\\s+you\\s+should\\s+have\\s*:?",
    "As\\s+a\\s+Minimum,?\\s+You\\s+Will\\s+Need\\s*:?",
    "As\\s+a\\s+Minimum\\s*:?",
    "Requirements\\s+and\\s+Technical\\s+Competency\\s*:?",
    "How\\s+to\\s+Qualify\\s*:?",
    "Required\\s+Skills\\s*:?"
  )
  min_qual_text <- extract_workday_section(desc_html, min_qual_patterns, bullets_only = FALSE, allow_bold_subheadings = TRUE)

  if (!is.na(min_qual_text)) {
    bullets <- str_split(min_qual_text, "\n")[[1]]
    bullets <- str_squish(bullets)
    bullets <- bullets[nchar(bullets) > 0]

    # Strip Micron fraud alert and similar boilerplate bullets
    micron_boilerplate <- "(?i)(^Fraud\\s+alert:?\\s+Micron|^As a worldwide provider of semiconductor)"
    bullets <- bullets[!grepl(micron_boilerplate, bullets, perl = TRUE)]

    # Split "DEGREE + N years experience" combined bullets (e.g. "MSEE + 5 years' ...")
    # into two separate bullets so the degree lands in min_education and experience in min_experience.
    combo_deg_pat <- "^([A-Z]{2,8}(?:/[A-Z]{2,8})?)\\s*\\+\\s*(.{10,})$"
    valid_deg_abbrevs <- paste0(
      "^(BS|MS|BA|MA|BE|ME|PhD|PhDs|",
      "[A-Z]{2,4}EE|[A-Z]{2,4}CS|[A-Z]{2,4}ME|[A-Z]{2,4}CE|[A-Z]{2,4}SE|",
      "[A-Z]{2,4}PE|[A-Z]{2,4}EN|[A-Z]{2,4}ECE|BS/MS|MS/PhD)$"
    )
    expanded <- character(0)
    for (b in bullets) {
      m <- regmatches(b, regexec(combo_deg_pat, b, perl = TRUE))[[1]]
      if (length(m) == 3 && grepl(valid_deg_abbrevs, m[2], perl = TRUE)) {
        expanded <- c(expanded, str_squish(m[2]), str_squish(m[3]))
      } else {
        expanded <- c(expanded, b)
      }
    }
    bullets <- expanded

    # Strip "Experience & Education:" or "Education & Experience:" bold sub-heading prefix
    # (Micron "Requirements and Technical Competency" posting format)
    bullets <- sub(
      "^(?:Experience\\s*(?:&|and)\\s*Education|Education\\s*(?:&|and)\\s*Experience)\\s*:?\\s*",
      "", bullets, perl = TRUE, ignore.case = TRUE
    )

    # Use degree pattern to identify education vs experience bullets
    edu_pattern <- paste0(
      "(?i)(\\bbachelor|\\bmaster(?!ing)|\\bassociate(?!d)|ph\\.?d|\\bdegree\\b|\\bdiploma\\b|",
      "high\\s+school|\\bGED\\b|college|technical\\s+school|education\\s+requirement)|",
      "(?-i:\\bB\\.?S\\.?\\b|\\bM\\.?S\\.?\\b|\\bB\\.?A\\.?\\b|\\bM\\.?A\\.?\\b|",
      "\\bA\\.?A\\.?S?\\.?\\b|\\bA\\.?S\\.\\b|",
      "\\bMSEE\\b|\\bBSEE\\b|\\bMSCS\\b|\\bBSCS\\b|\\bMSME\\b|\\bBSME\\b|",
      "\\bMSCE\\b|\\bBSCE\\b|\\bMSECE\\b|\\bBSECE\\b|\\bMSPE\\b|\\bBSPE\\b)"
    )
    # Split education bullets that also embed a "preferred" clause after a sentence boundary.
    # Micron "Requirements and Technical Competency" pattern:
    #   "Bachelor's, Master's, or PhD in a relevant field. Minimum 3+ years in ... preferred (asset)..."
    bullets_clean <- character(0)
    for (b in bullets) {
      if (grepl(edu_pattern, b, perl = TRUE) && grepl("\\bpreferred\\b", b, perl = TRUE, ignore.case = TRUE)) {
        m <- regmatches(b, regexec("^(.+?[.])\\s+(.+\\bpreferred\\b.*)$", b, perl = TRUE))[[1]]
        if (length(m) == 3 && grepl(edu_pattern, m[2], perl = TRUE)) {
          bullets_clean <- c(bullets_clean, str_squish(m[2]))
          extra_preferred_bullets <- c(extra_preferred_bullets, str_squish(m[3]))
        } else {
          bullets_clean <- c(bullets_clean, b)
        }
      } else {
        bullets_clean <- c(bullets_clean, b)
      }
    }
    bullets <- bullets_clean

    edu_idx <- which(grepl(edu_pattern, bullets, perl = TRUE))

    if (length(edu_idx) > 0) {
      min_education <- paste(bullets[edu_idx], collapse = "\n")
      remaining <- bullets[-edu_idx]
      if (length(remaining) > 0) min_experience <- paste(remaining, collapse = "\n")
    } else {
      # No degree keywords found - put all bullets in experience
      if (length(bullets) >= 1) {
        min_experience <- paste(bullets, collapse = "\n")
      }
    }
  }

  # Check for "Education and Skills:" section (NXP pattern with mixed content)
  # This section can contain education lines AND skills/tools that belong in responsibilities
  if (is.na(min_education) && is.na(min_experience)) {
    edu_skills_text <- extract_paragraph_section(desc_html, "Education\\s+and\\s+Skills\\s*:?")

    if (!is.na(edu_skills_text)) {
      es_lines <- str_split(edu_skills_text, "\n")[[1]]
      es_lines <- str_squish(es_lines)
      es_lines <- es_lines[nchar(es_lines) > 0]

      degree_pattern <- "(?i)(\\bbachelor|\\bmaster(?!ing)|\\bassociate(?!d)|ph\\.?d|\\bdegree\\b|\\bdiploma\\b|high\\s+school|\\bGED\\b|education\\s+along\\s+with|formal\\s+education)|(?-i:\\bB\\.?S\\.?\\b|\\bM\\.?S\\.?\\b|\\bB\\.?A\\.?\\b|\\bM\\.?A\\.?\\b|\\bA\\.?A\\.?S?\\.?\\b|\\bA\\.?S\\.\\b)"
      edu_idx <- which(grepl(degree_pattern, es_lines, perl = TRUE))

      if (length(edu_idx) > 0) {
        min_education <- paste(es_lines[edu_idx], collapse = "\n")
        remaining <- es_lines[-edu_idx]
        # Non-education lines from "Education and Skills" are skills/tools - add to responsibilities
        if (length(remaining) > 0) {
          skills_text <- paste(remaining, collapse = "\n")
          if (nchar(skills_text) > 20) {
            responsibilities <- if (is.na(responsibilities)) {
              skills_text
            } else {
              paste(responsibilities, skills_text, sep = "\n\n")
            }
          }
        }
      } else {
        # No degree lines found - entire section is skills
        min_experience <- paste(es_lines, collapse = "\n")
      }
    }
  }

  # Check for standalone "Experience:" section (plain-text paragraph heading, NXP pattern)
  if (is.na(min_experience)) {
    standalone_exp_text <- extract_paragraph_section(desc_html, "Experience\\s*:?")
    if (!is.na(standalone_exp_text) && nchar(str_squish(standalone_exp_text)) > 0) {
      min_experience <- str_squish(standalone_exp_text)
    }
  }

  # Fallback: combined "Qualifications" or "Job Qualifications" section (NXP patterns)
  if (is.na(min_education) && is.na(min_experience)) {
    combined_qual_patterns <- c(
      "Job\\s+Qualifications\\s*:?",
      "Required\\s+skills?\\s+and\\s+qualifications\\s*:?",
      "Attributes\\s*:?",
      "Your\\s+Background\\s*:?",
      "We\\s+are\\s+looking\\s+for\\s*:?",
      "Qualifications\\s*.*?and\\s+Experience\\s*.*?\\)",
      "(?<!Minimum\\s)(?<!Preferred\\s)Qualifications\\s*:?"
    )
    combined_qual_text <- extract_workday_section(desc_html, combined_qual_patterns, allow_bold_subheadings = TRUE)

    if (!is.na(combined_qual_text)) {
      qual_lines <- str_split(combined_qual_text, "\n")[[1]]
      qual_lines <- str_squish(qual_lines)
      qual_lines <- qual_lines[nchar(qual_lines) > 0]

      # Strip inline compensation/benefits/boilerplate text from qual bullets
      boilerplate_filter <- "(?i)(^Compensation for this role|^Regular full-time employees|benefits including Medical|^#LI-|^Life @ Samsung|^U\\.S\\.\\s+Export Control|^Trade Secrets|^Fraud\\s+alert:?\\s+Micron|^As a worldwide provider of semiconductor)"
      qual_lines <- qual_lines[!grepl(boilerplate_filter, qual_lines, perl = TRUE)]
      # Also strip trailing compensation text appended after <br/> within a bullet
      qual_lines <- str_replace(qual_lines, "(?i)\\s*Compensation for this role.*$", "")
      qual_lines <- str_replace(qual_lines, "(?i)\\s*Regular full-time employees.*$", "")
      qual_lines <- str_replace(qual_lines, "(?i)\\s*#LI-.*$", "")
      qual_lines <- str_squish(qual_lines)
      qual_lines <- qual_lines[nchar(qual_lines) > 0]

      # Try to separate education from experience:
      degree_pattern <- paste0(
        "(?i)(^Education\\s*:|\\bbachelor|\\bmaster(?!ing)|\\bassociate(?!d)|ph\\.?d|\\bdegree\\b|",
        "\\bdiploma\\b|high\\s+school|\\bGED\\b|college|technical\\s+school|education\\s+requirement)|",
        "(?-i:\\bB\\.?S\\.?\\b|\\bM\\.?S\\.?\\b|\\bB\\.?A\\.?\\b|\\bM\\.?A\\.?\\b|",
        "\\bA\\.?A\\.?S?\\.?\\b|\\bA\\.?S\\.\\b|",
        "\\bBSEE\\b|\\bMSEE\\b|\\bBSCS\\b|\\bMSCS\\b|\\bBSME\\b|\\bMSME\\b|",
        "\\bBSCE\\b|\\bMSCE\\b|\\bBSECE\\b|\\bMSECE\\b|\\bBSPE\\b|\\bMSPE\\b)"
      )
      edu_idx <- which(grepl(degree_pattern, qual_lines, perl = TRUE))

      if (length(edu_idx) > 0) {
        min_education <- paste(qual_lines[edu_idx], collapse = "\n")
        remaining <- qual_lines[-edu_idx]
        if (length(remaining) > 0) {
          min_experience <- paste(remaining, collapse = "\n")
        }
      } else {
        # No clear education line - treat entire section as experience/qualifications
        min_experience <- paste(qual_lines, collapse = "\n")
      }
    }
  }

  # Standalone "Education:" bold heading — bullets_only = FALSE to also capture <p> text (AMAT pattern)
  if (is.na(min_education)) {
    edu_section_text <- extract_workday_section(desc_html, c("Education\\s*:?"), bullets_only = FALSE)
    if (!is.na(edu_section_text)) {
      min_education <- str_squish(edu_section_text)
    }
  }

  # Micron "Required Education:" standalone bold heading
  if (is.na(min_education)) {
    micron_edu_text <- extract_workday_section(desc_html, c("Required\\s+Education\\s*:?"), bullets_only = FALSE)
    if (!is.na(micron_edu_text)) {
      min_education <- str_squish(micron_edu_text)
    }
  }

  # Final fallback: inline bold "Education:" / "Experience:" patterns (Applied Materials)
  if (is.na(min_education)) {
    edu_inline <- str_match(desc_html, "(?i)<b>\\s*Education\\s*:?\\s*</b>\\s*([^<]+)")
    if (!is.na(edu_inline[1, 2])) {
      edu_doc <- tryCatch(read_html(paste0("<p>", edu_inline[1, 2], "</p>")), error = function(e) NULL)
      min_education <- if (!is.null(edu_doc)) str_squish(html_text(edu_doc)) else str_squish(edu_inline[1, 2])
    }
  }
  if (is.na(min_experience)) {
    exp_inline <- str_match(desc_html, "(?i)<b>\\s*Experience\\s*:?\\s*</b>\\s*([^<]+)")
    if (!is.na(exp_inline[1, 2])) {
      exp_doc <- tryCatch(read_html(paste0("<p>", exp_inline[1, 2], "</p>")), error = function(e) NULL)
      min_experience <- if (!is.null(exp_doc)) str_squish(html_text(exp_doc)) else str_squish(exp_inline[1, 2])
    }
  }

  # AMAT "Functional & Business Expertise:" bold heading → min_experience
  if (is.na(min_experience)) {
    amat_fb_text <- extract_workday_section(
      desc_html,
      c("Functional\\s+(?:&(?:amp;)?|and)\\s+Business\\s+Expertise\\s*:?")
    )
    if (!is.na(amat_fb_text)) min_experience <- str_squish(amat_fb_text)
  }

  # AMAT "Experience Required:" bold heading → append to min_experience
  amat_exp_req <- extract_workday_section(desc_html, c("Experience\\s+Required\\s*:?"))
  if (!is.na(amat_exp_req)) {
    amat_exp_req <- str_squish(amat_exp_req)
    min_experience <- if (is.na(min_experience)) amat_exp_req else paste(min_experience, amat_exp_req, sep = "\n")
  }

  # AMAT "Functional Knowledge" and "Business Expertise" → append to min_experience
  for (.amat_sec in c("Functional\\s+Knowledge\\s*:?", "Business\\s+Expertise\\s*:?")) {
    .amat_txt <- extract_workday_section(desc_html, c(.amat_sec))
    if (!is.na(.amat_txt)) {
      .amat_txt <- str_squish(.amat_txt)
      min_experience <- if (is.na(min_experience)) .amat_txt else paste(min_experience, .amat_txt, sep = "\n")
    }
  }

  # AMAT "Other Requirements:" → append to min_experience
  amat_other_req <- extract_workday_section(desc_html, c("Other\\s+Requirements\\s*:?"))
  if (!is.na(amat_other_req)) {
    amat_other_req <- str_squish(amat_other_req)
    min_experience <- if (is.na(min_experience)) amat_other_req else paste(min_experience, amat_other_req, sep = "\n")
  }

  # AMAT residual: role-specific paragraphs after the last standard skill section
  # (Interpersonal Skills / Physical Requirements) and before Experience Required / Education.
  # These are plain <p> blocks — not bold-headed — and contain role-specific technical requirements.
  {
    skill_end_pat <- paste0(
      "(?si)(?:Interpersonal\\s+Skills|Physical\\s+Requirements)",
      "[^<]*(?:</b>|</strong>)[^<]*(?:</p>)?\\s*<ul>.*?</ul>"
    )
    skill_end_m <- regexpr(skill_end_pat, desc_html, perl = TRUE)
    if (skill_end_m > 0) {
      after_pos <- skill_end_m + attr(skill_end_m, "match.length")
      next_head_pat <- "(?i)<(?:b|strong)>\\s*(?:Experience\\s+Required|Education|Other\\s+Requirements)"
      next_head_m <- regexpr(next_head_pat,
        substr(desc_html, after_pos, nchar(desc_html)), perl = TRUE)
      chunk <- if (next_head_m > 0) {
        substr(desc_html, after_pos, after_pos + next_head_m - 2)
      } else {
        substr(desc_html, after_pos, nchar(desc_html))
      }
      chunk_doc <- tryCatch(
        read_html(paste0("<div>", chunk, "</div>")), error = function(e) NULL)
      if (!is.null(chunk_doc)) {
        para_nodes <- chunk_doc %>% html_elements("p")
        residual_lines <- character(0)
        for (.node in para_nodes) {
          if (length(html_elements(.node, "b, strong")) > 0) next  # skip bold-headed paragraphs
          .txt <- str_squish(html_text2(.node))
          if (nchar(.txt) > 15) residual_lines <- c(residual_lines, .txt)
        }
        if (length(residual_lines) > 0) {
          residual_txt <- paste(residual_lines, collapse = "\n")
          min_experience <- if (is.na(min_experience)) residual_txt else paste(min_experience, residual_txt, sep = "\n")
        }
      }
    }
  }

  # Also check for "Experience and education:" sub-section within Required skills (NXP Pattern 5)
  if (is.na(min_education)) {
    edu_exp_patterns <- c("Experience\\s+and\\s+[Ee]ducation\\s*:?")
    edu_exp_text <- extract_workday_section(desc_html, edu_exp_patterns, allow_bold_subheadings = TRUE)
    if (!is.na(edu_exp_text)) {
      ee_lines <- str_split(edu_exp_text, "\n")[[1]]
      ee_lines <- str_squish(ee_lines)
      ee_lines <- ee_lines[nchar(ee_lines) > 0]
      degree_pattern <- "(?i)(\\bbachelor|\\bmaster(?!ing)|\\bassociate(?!d)|ph\\.?d|\\bdegree\\b|\\bdiploma\\b|high\\s+school|\\bGED\\b)|(?-i:\\bB\\.?S\\.?\\b|\\bM\\.?S\\.?\\b|\\bB\\.?A\\.?\\b|\\bM\\.?A\\.?\\b|\\bA\\.?A\\.?S?\\.?\\b|\\bA\\.?S\\.\\b)"
      edu_idx <- which(grepl(degree_pattern, ee_lines, perl = TRUE))
      if (length(edu_idx) > 0) {
        min_education <- ee_lines[edu_idx[1]]
        remaining <- ee_lines[-edu_idx[1]]
        if (length(remaining) > 0 && is.na(min_experience)) min_experience <- paste(remaining, collapse = "\n")
      } else if (length(ee_lines) >= 1) {
        min_education <- ee_lines[1]
        if (length(ee_lines) >= 2 && is.na(min_experience)) min_experience <- paste(ee_lines[2:length(ee_lines)], collapse = "\n")
      }
    }
  }

  # PERM/immigration format: "Employer will accept a [degree]..." inline paragraph
  # followed by "Position requires:" numbered items (no bold section headings).
  if (is.na(min_education) || is.na(min_experience)) {
    perm_doc <- tryCatch(read_html(paste0("<div>", desc_html, "</div>")), error = function(e) NULL)
    if (!is.null(perm_doc)) {
      all_paras <- html_elements(perm_doc, "p")
      para_texts <- str_squish(html_text(all_paras))
      # Education (form 1): paragraph starting with "Employer will accept a"
      if (is.na(min_education)) {
        edu_idx_perm <- which(grepl("^Employer\\s+will\\s+accept\\s+a\\s+", para_texts,
                                    perl = TRUE, ignore.case = TRUE))
        if (length(edu_idx_perm) > 0) min_education <- para_texts[edu_idx_perm[1]]
      }
      # Education+Experience (form 2): "Minimum [degree]..., and [N] years of experience..."
      # Strip "Minimum " and split at ", and [N] years" to get education and experience.
      if (is.na(min_education)) {
        deg_kw_pat <- "(?i)(bachelor|master|associate|ph\\.?d|degree|diploma|BSEE|MSEE|BSCS|MSCS|\\bB\\.S\\.\\b|\\bM\\.S\\.\\b)"
        min_para_idx <- which(
          grepl("^Minimum\\s+", para_texts, perl = TRUE, ignore.case = TRUE) &
          grepl(deg_kw_pat, para_texts, perl = TRUE)
        )
        if (length(min_para_idx) > 0) {
          p <- para_texts[min_para_idx[1]]
          edu_part <- str_replace(p, "(?i)^Minimum\\s+", "")
          # Split at ", and [N+] years" suffix
          split_m <- regmatches(edu_part,
            regexec(",?\\s+and\\s+(\\d[^$]+)$", edu_part, perl = TRUE))[[1]]
          if (length(split_m) >= 2) {
            min_education <- str_squish(str_replace(edu_part, ",?\\s+and\\s+\\d[^$]+$", ""))
            if (is.na(min_experience)) min_experience <- str_squish(split_m[2])
          } else {
            min_education <- str_squish(edu_part)
          }
        }
      }
      # Experience: paragraphs after a "Position requires:" paragraph.
      # Only set if min_experience is still NA (the "Minimum..." paragraph above takes precedence).
      if (is.na(min_experience)) {
        req_idx <- which(grepl("^Position\\s+requires\\s*:?$", para_texts,
                               perl = TRUE, ignore.case = TRUE))
        if (length(req_idx) > 0) {
          start <- req_idx[1] + 1
          end <- length(para_texts)
          section_stop <- "(?i)^(Preferred\\s+Qualifications|Preferred\\s+Skills|What\\s+Sets\\s+You\\s+Apart)"
          exp_lines <- character(0)
          for (k in start:end) {
            t <- para_texts[k]
            if (nchar(t) == 0) break
            if (grepl(section_stop, t, perl = TRUE)) break
            exp_lines <- c(exp_lines, str_replace(t, "^\\d+\\.\\s*", ""))
          }
          if (length(exp_lines) > 0) min_experience <- paste(exp_lines, collapse = "\n")
        }
      }
    }
  }

  # --- Extract preferred qualifications ---
  preferred_patterns <- c(
    "Preferred\\s+Qualifications\\s*:?",
    "Preferred\\s+Skills\\s*:?",
    "Nice\\s+to\\s+Have\\s*:?",
    "Desired\\s+Qualifications\\s*:?",
    "What\\s+Sets\\s+You\\s+Apart\\s*:?",
    "What\\s+you\\s+bring\\s*:?",
    "Qualifications\\s+We\\s+Prefer\\s*:?",
    "The\\s+following\\s+are\\s+a\\s+PLUS,?\\s+but\\s+not\\s+required\\s*:?",
    "Additional\\s+Qualifications\\s*\\(?\\s*Preferred\\s*\\)?\\s*:?",
    "Preferred\\s+Requirements\\s*:?",
    "Preferred\\s+Experience\\s*:?"
  )
  preferred_quals <- extract_workday_section(desc_html, preferred_patterns, allow_bold_subheadings = TRUE)

  # Fallback: check for standalone "Preferred:" paragraph heading (NXP pattern)
  if (is.na(preferred_quals)) {
    preferred_quals <- extract_paragraph_section(desc_html, "Preferred\\s*:?")
  }

  # Fallback: collect <li> items that start with "Preferred:" (NXP inline label pattern)
  if (is.na(preferred_quals)) {
    doc <- tryCatch(read_html(paste0("<div>", desc_html, "</div>")), error = function(e) NULL)
    if (!is.null(doc)) {
      li_items <- doc %>% html_elements("li") %>% html_text2()
      pref_items <- li_items[grepl("^\\s*Preferred\\s*:", li_items)]
      if (length(pref_items) > 0) {
        # Strip the "Preferred:" prefix
        pref_items <- sub("^\\s*Preferred\\s*:\\s*", "", pref_items)
        pref_items <- str_squish(pref_items)
        pref_items <- pref_items[nchar(pref_items) > 0]
        if (length(pref_items) > 0) {
          preferred_quals <- paste(pref_items, collapse = "\n")
        }
      }
    }
  }

  # Prepend any "preferred" clauses extracted from education bullets
  # (e.g. Micron "Requirements and Technical Competency" pattern: edu + preferred text in same bullet)
  if (length(extra_preferred_bullets) > 0) {
    extra_pref_text <- paste(extra_preferred_bullets, collapse = "\n")
    preferred_quals <- if (is.na(preferred_quals)) {
      extra_pref_text
    } else {
      paste(extra_pref_text, preferred_quals, sep = "\n")
    }
  }

  # --- Promote education line from preferred_quals when min_education is still NA ---
  # Some postings (e.g. Micron) omit a degree requirement from Minimum Qualifications
  # and instead list it as the last bullet in Preferred Qualifications.
  if (is.na(min_education) && !is.na(preferred_quals)) {
    edu_pattern_pref <- paste0(
      "(?i)(\\bbachelor|\\bmaster(?!ing)|\\bassociate(?!d)|ph\\.?d|\\bdegree\\b|\\bdiploma\\b|",
      "high\\s+school|\\bGED\\b|college|technical\\s+school|education\\s+requirement)|",
      "(?-i:\\bB\\.?S\\.?\\b|\\bM\\.?S\\.?\\b|\\bB\\.?A\\.?\\b|\\bM\\.?A\\.?\\b|",
      "\\bA\\.?A\\.?S?\\.?\\b|\\bA\\.?S\\.\\b|",
      "\\bMSEE\\b|\\bBSEE\\b|\\bMSCS\\b|\\bBSCS\\b|\\bMSME\\b|\\bBSME\\b|",
      "\\bMSCE\\b|\\bBSCE\\b|\\bMSECE\\b|\\bBSECE\\b|\\bMSPE\\b|\\bBSPE\\b)"
    )
    pq_lines <- str_split(preferred_quals, "\n")[[1]]
    pq_lines <- str_squish(pq_lines)
    pq_lines <- pq_lines[nchar(pq_lines) > 0]
    edu_in_pref <- which(grepl(edu_pattern_pref, pq_lines, perl = TRUE))
    if (length(edu_in_pref) > 0) {
      min_education <- paste(pq_lines[edu_in_pref], collapse = "\n")
      # Remove those lines from preferred_quals so they're not duplicated
      remaining_pq <- pq_lines[-edu_in_pref]
      preferred_quals <- if (length(remaining_pq) > 0) paste(remaining_pq, collapse = "\n") else NA_character_
    }
  }

  # --- Merge "ideal candidate" text into min_experience (Intel pattern) ---
  if (!is.na(ideal_candidate_text)) {
    if (is.na(min_experience)) {
      min_experience <- ideal_candidate_text
    } else {
      min_experience <- paste(ideal_candidate_text, min_experience, sep = "\n")
    }
  }

  # --- Extract essential knowledge & skills ---
  essential_skills <- extract_workday_essential_skills(desc_html)

  Sys.sleep(SCRAPER_CONFIG$delay_between_requests)

  return(list(
    job_identification = job_identification,
    job_responsibilities = responsibilities,
    min_education = min_education,
    min_experience = min_experience,
    preferred_qualifications = preferred_quals,
    salary_range = salary,
    essential_skills = essential_skills
  ))
}

#' Main function to scrape a Workday company with full details
#'
#' @param company_name Name of the company
#' @param base_url Base URL of career site
#' @param fetch_details Whether to fetch full job details (slower)
#' @return Dataframe of jobs with all available details
scrape_workday_company <- function(company_name, base_url, fetch_details = TRUE) {

  log_message(paste("=== Starting Workday API scrape for", company_name, "==="))
  start_time <- Sys.time()

  jobs <- scrape_workday(company_name, base_url)

  if (nrow(jobs) == 0) {
    log_message(paste("No jobs found for", company_name), level = "WARN")
    return(data.frame())
  }

  if (fetch_details) {
    log_message(paste("Fetching details for", nrow(jobs), "US jobs"))

    pb <- txtProgressBar(min = 0, max = nrow(jobs), style = 3)

    detail_results <- vector("list", nrow(jobs))
    for (i in seq_len(nrow(jobs))) {
      detail_results[[i]] <- tryCatch(
        scrape_workday_job_details(base_url, jobs$external_path[i]),
        error = function(e) {
          log_message(paste("Error fetching details for job", jobs$job_req_id[i], ":", e$message), level = "WARN")
          list(
            job_identification = NA_character_,
            job_responsibilities = NA_character_,
            min_education = NA_character_,
            min_experience = NA_character_,
            preferred_qualifications = NA_character_,
            salary_range = NA_character_,
            essential_skills = NA_character_
          )
        }
      )
      setTxtProgressBar(pb, i)
    }
    close(pb)

    # Bind detail columns to jobs
    details_df <- bind_rows(lapply(detail_results, as_tibble))
    jobs <- bind_cols(jobs, details_df)
  } else {
    jobs <- jobs %>%
      mutate(
        job_identification = NA_character_,
        job_responsibilities = NA_character_,
        min_education = NA_character_,
        min_experience = NA_character_,
        preferred_qualifications = NA_character_,
        salary_range = NA_character_,
        essential_skills = NA_character_
      )
  }

  # Use job_identification from detail API; fall back to job_req_id from listing
  if ("job_identification" %in% names(jobs)) {
    jobs <- jobs %>%
      mutate(job_identification = ifelse(is.na(job_identification), job_req_id, job_identification))
  } else {
    jobs <- jobs %>% mutate(job_identification = job_req_id)
  }

  # Drop internal columns and add metadata
  jobs <- jobs %>%
    select(-external_path, -job_req_id) %>%
    mutate(
      company_name = company_name,
      scraped_at = Sys.time()
    )

  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

  log_message(paste("Completed", company_name, "in", round(duration, 2), "seconds"))
  log_message(paste("Total US jobs:", nrow(jobs)))

  return(jobs)
}
