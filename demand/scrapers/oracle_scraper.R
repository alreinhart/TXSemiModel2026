# Oracle CX Platform Scraper
# ===========================
# Scrapes job listings from Oracle Cloud CX career sites via REST API
# Used by: Texas Instruments
# Filters to US-only jobs and parses TI-specific description format

library(httr)
library(jsonlite)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)

source("config/scraper_config.R")
source("scrapers/utils.R")

#' Extract the API base URL from an Oracle CX careers URL
#'
#' @param careers_url Full careers URL
#' @return API base URL root (e.g., https://edbz.fa.us2.oraclecloud.com)
extract_oracle_api_base <- function(careers_url) {
  str_extract(careers_url, "^https?://[^/]+")
}

#' Extract the site number from an Oracle CX careers URL
#'
#' @param careers_url Full careers URL
#' @return Site number string (e.g., "CX")
extract_oracle_site_number <- function(careers_url) {
  match <- str_match(careers_url, "/sites/([^/]+)")
  if (is.na(match[1, 2])) {
    log_message("Could not extract site number from URL, defaulting to 'CX'", level = "WARN")
    return("CX")
  }
  match[1, 2]
}

#' Fetch a page of jobs from the Oracle CX REST API
#'
#' @param api_base API base URL root
#' @param site_number Site number (e.g., "CX")
#' @param offset Pagination offset
#' @param limit Number of jobs per page
#' @return Parsed JSON response or NULL on failure
fetch_oracle_job_page <- function(api_base, site_number, offset = 0, limit = 25) {

  api_url <- paste0(
    api_base,
    "/hcmRestApi/resources/latest/recruitingCEJobRequisitions",
    "?onlyData=true",
    "&expand=requisitionList.secondaryLocations,requisitionList.workLocation",
    "&finder=findReqs;siteNumber=", site_number,
    ",limit=", limit,
    ",offset=", offset
  )

  log_message(paste("Fetching Oracle API: offset =", offset), level = "DEBUG")

  response <- tryCatch({
    GET(
      api_url,
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
    log_message(paste("API request failed:", e$message), level = "ERROR")
    return(NULL)
  })

  if (is.null(response)) return(NULL)

  if (status_code(response) != 200) {
    log_message(paste("Oracle API returned HTTP", status_code(response)), level = "WARN")
    return(NULL)
  }

  json_text <- content(response, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(json_text, simplifyVector = FALSE)

  return(parsed)
}

#' Parse a single job from the requisition list, filtering to US only
#'
#' @param job A single job list element from the API
#' @param jobs_base_url Base URL for constructing human-readable job links
#' @return A one-row tibble or NULL if not a US job
parse_oracle_job <- function(job, jobs_base_url) {

  # Check if any workLocation is in the US
  us_location <- NULL
  if (!is.null(job$workLocation)) {
    for (wl in job$workLocation) {
      if (!is.null(wl$Country) && wl$Country == "US") {
        city <- if (!is.null(wl$TownOrCity) && nchar(wl$TownOrCity) > 0) wl$TownOrCity else ""
        state <- if (!is.null(wl$Region2) && nchar(wl$Region2) > 0) wl$Region2 else ""
        if (nchar(city) > 0 && nchar(state) > 0) {
          us_location <- paste0(city, ", ", state)
        } else if (nchar(city) > 0) {
          us_location <- city
        } else if (nchar(state) > 0) {
          us_location <- state
        } else {
          us_location <- "United States"
        }
        break
      }
    }
  }

  # Skip non-US jobs
  if (is.null(us_location)) return(NULL)

  job_id <- job$Id
  title <- job$Title
  if (is.null(title)) return(NULL)

  posted_date <- tryCatch({
    parse_date_string(job$PostedDate)
  }, error = function(e) NA_Date_)

  job_url <- paste0(jobs_base_url, "/", job_id)

  tibble(
    job_id_oracle = as.character(job_id),
    job_title = title,
    location = us_location,
    job_url = job_url,
    posting_date = posted_date
  )
}

#' Scrape US jobs from Oracle CX career site via REST API
#'
#' @param company_name Name of the company
#' @param base_url Base URL of the Oracle CX career site
#' @param max_pages Maximum number of pages to scrape
#' @return Dataframe of US-only job listings
scrape_oracle <- function(company_name, base_url, max_pages = SCRAPER_CONFIG$max_pages_per_company) {

  log_message(paste("Starting Oracle CX API scrape for", company_name, "(US only)"))

  api_base <- extract_oracle_api_base(base_url)
  site_number <- extract_oracle_site_number(base_url)
  jobs_base_url <- str_replace(base_url, "/jobs$", "/jobs")

  all_jobs <- list()
  offset <- 0
  limit <- 25
  total_jobs <- NULL
  page <- 1
  total_us <- 0

  while (page <= max_pages) {

    log_message(paste("Scraping page", page, "for", company_name, "(offset:", offset, ")"))

    parsed <- fetch_oracle_job_page(api_base, site_number, offset, limit)

    if (is.null(parsed)) {
      log_message("Failed to fetch page - stopping", level = "WARN")
      break
    }

    items <- parsed$items
    if (length(items) == 0) {
      log_message("No items in API response - stopping")
      break
    }

    top_item <- items[[1]]

    if (is.null(total_jobs)) {
      total_jobs <- top_item$TotalJobsCount
      if (!is.null(total_jobs)) {
        log_message(paste("Total jobs reported by API:", total_jobs))
      }
    }

    req_list <- top_item$requisitionList
    if (is.null(req_list) || length(req_list) == 0) {
      log_message("No more jobs in requisitionList - stopping")
      break
    }

    # Parse each job, filtering to US only
    page_jobs <- list()
    for (job in req_list) {
      parsed_job <- parse_oracle_job(job, jobs_base_url)
      if (!is.null(parsed_job)) {
        page_jobs <- c(page_jobs, list(parsed_job))
      }
    }

    if (length(page_jobs) > 0) {
      page_df <- bind_rows(page_jobs)
      all_jobs <- c(all_jobs, list(page_df))
      total_us <- total_us + nrow(page_df)
      log_message(paste("  Found", nrow(page_df), "US jobs on this page (", total_us, "total US)"))
    }

    # Check if we've fetched all jobs
    fetched_so_far <- offset + length(req_list)
    if (!is.null(total_jobs) && fetched_so_far >= total_jobs) {
      log_message(paste("Reached all", total_jobs, "jobs"))
      break
    }

    if (length(req_list) < limit) {
      log_message("Last page (fewer results than limit)")
      break
    }

    offset <- offset + limit
    page <- page + 1
    Sys.sleep(SCRAPER_CONFIG$delay_between_requests)
  }

  if (length(all_jobs) > 0) {
    jobs_df <- bind_rows(all_jobs)
    log_message(paste("Found", nrow(jobs_df), "US jobs for", company_name))
  } else {
    jobs_df <- tibble(
      job_id_oracle = character(),
      job_title = character(),
      location = character(),
      job_url = character(),
      posting_date = as.Date(character())
    )
    log_message(paste("No US jobs found for", company_name), level = "WARN")
  }

  return(jobs_df)
}

#' Extract bulleted list items from HTML that follow a heading matching a pattern
#'
#' Uses a text-based approach: converts HTML to plain text, finds the heading,
#' then goes back to the HTML to extract <li> items from the <ul>/<ol> that
#' follows the heading in the source.
#'
#' @param html_string Raw HTML string
#' @param heading_patterns Vector of regex patterns to try (case-insensitive)
#' @return Character string with bullet items separated by newlines, or NA
extract_html_section_bullets <- function(html_string, heading_patterns) {

  if (is.null(html_string) || is.na(html_string) || nchar(html_string) == 0) {
    return(NA_character_)
  }

  # Ensure heading_patterns is a vector
  if (length(heading_patterns) == 1) heading_patterns <- c(heading_patterns)

  for (pattern in heading_patterns) {
    result <- extract_bullets_for_pattern(html_string, pattern)
    if (!is.na(result)) return(result)
  }

  return(NA_character_)
}

#' Extract bullets for a single heading pattern
#'
#' @param html_string Raw HTML
#' @param heading_pattern Single regex pattern
#' @return Bullet string or NA
extract_bullets_for_pattern <- function(html_string, heading_pattern) {

  # Strip HTML tags to get plain text for heading detection
  plain_text <- str_replace_all(html_string, "<[^>]+>", " ")
  plain_text <- str_replace_all(plain_text, "&[a-zA-Z]+;", " ")
  plain_text <- str_replace_all(plain_text, "\\s+", " ")

  # Check if the heading exists in the text
  if (!grepl(heading_pattern, plain_text, ignore.case = TRUE, perl = TRUE)) {
    return(NA_character_)
  }

  # Find the heading location in the raw HTML by searching for the pattern
  # across tag boundaries. Build a flexible split pattern that allows HTML tags
  # between words of the heading.
  # Strategy: split the raw HTML at the point where the heading text ends,
  # then extract <li> from the first <ul>/<ol> after that point.

  # Try multiple split strategies to handle headings in various tag contexts

  # Strategy 1: heading inside any tag (strong, b, span, p, etc.)
  split_patterns <- c(
    # Heading inside tags like <strong>Key Responsibilities</strong>
    paste0("(?i)(?:<[^>]*>\\s*)*", heading_pattern, "\\s*(?:</[^>]*>\\s*)*(?:&nbsp;)?\\s*(?:</[^>]*>)*"),
    # Heading in plain text followed by </span></p> etc then <ul>
    paste0("(?i)", heading_pattern, "\\s*(?:&nbsp;)?\\s*(?:</[^>]*>\\s*)*")
  )

  for (sp in split_patterns) {
    parts <- tryCatch(
      strsplit(html_string, sp, perl = TRUE)[[1]],
      error = function(e) NULL
    )

    if (is.null(parts) || length(parts) < 2) next

    # Take everything after the heading
    after_heading <- parts[2]

    # Truncate at the next major section heading (bold text or strong tag with text)
    next_section <- regexpr(
      "(?i)<(?:strong|b)>\\s*[A-Z][^<]{2,}",
      after_heading, perl = TRUE
    )
    if (next_section > 0) {
      after_heading <- substr(after_heading, 1, next_section - 1)
    }

    # Parse and extract <li> items
    section_doc <- tryCatch(
      read_html(paste0("<div>", after_heading, "</div>")),
      error = function(e) NULL
    )
    if (is.null(section_doc)) next

    li_items <- section_doc %>% html_elements("li") %>% html_text2()
    li_items <- str_squish(li_items)
    li_items <- li_items[nchar(li_items) > 0]

    if (length(li_items) > 0) {
      return(paste(li_items, collapse = "\n"))
    }
  }

  return(NA_character_)
}

#' Extract prose description paragraphs from job HTML as a fallback
#' when no bulleted responsibilities section exists.
#'
#' Strips boilerplate (taglines, visa notices, EEO statements) and returns
#' the substantive role description paragraphs.
#'
#' @param html_string Raw HTML description string
#' @return Cleaned prose text or NA
extract_prose_description <- function(html_string) {

  if (is.null(html_string) || is.na(html_string) || nchar(html_string) == 0) {
    return(NA_character_)
  }

  doc <- tryCatch(read_html(paste0("<div>", html_string, "</div>")), error = function(e) NULL)
  if (is.null(doc)) return(NA_character_)

  # Get all paragraph/block text nodes
  paragraphs <- doc %>% html_elements("p, div > span, li") %>% html_text2()
  paragraphs <- str_squish(paragraphs)
  paragraphs <- paragraphs[nchar(paragraphs) > 0]

  if (length(paragraphs) == 0) return(NA_character_)

  # Strip boilerplate phrases from within paragraphs
  strip_patterns <- c(
    "Change the world\\.\\s*Love your job\\.\\s*",
    "Put your talent to work with us[^.!]*[.!]\\s*",
    "Texas Instruments will not sponsor[^.]*\\.\\s*",
    "TI will not sponsor[^.]*\\.\\s*"
  )
  for (pat in strip_patterns) {
    paragraphs <- str_replace_all(paragraphs, regex(pat, ignore_case = TRUE), "")
  }
  paragraphs <- str_squish(paragraphs)

  # Remove entire paragraphs that are pure boilerplate
  discard_patterns <- c(
    "^Texas Instruments Incorporated \\(TI\\) is a global semiconductor",
    "^Texas Instruments is an equal opportunity",
    "^If you are interested in this position",
    "^All qualified applicants will receive",
    "^Why TI\\s*$",
    "^About Texas Instruments\\s*$",
    "^\\s*$"
  )

  keep <- rep(TRUE, length(paragraphs))
  for (pat in discard_patterns) {
    keep <- keep & !grepl(pat, paragraphs, ignore.case = TRUE, perl = TRUE)
  }
  paragraphs <- paragraphs[keep]

  # Remove very short fragments (< 20 chars) that are likely headings or artifacts
  paragraphs <- paragraphs[nchar(paragraphs) >= 20]

  if (length(paragraphs) == 0) return(NA_character_)

  result <- paste(paragraphs, collapse = "\n\n")

  # Trim to reasonable length
  if (nchar(result) > 5000) {
    result <- substr(result, 1, 5000)
  }

  # Don't return if too short to be meaningful
  if (nchar(result) < 30) return(NA_character_)

  result
}

#' Fetch detailed job information from Oracle CX job details API
#'
#' Parses TI-specific HTML structure for responsibilities, requirements,
#' qualifications, and extracts JOB INFO metadata fields.
#'
#' @param api_base API base URL root
#' @param job_id_oracle Oracle job requisition ID
#' @return List with detailed job information
scrape_oracle_job_details <- function(api_base, job_id_oracle) {

  log_message(paste("Fetching Oracle CX job details: ID", job_id_oracle), level = "DEBUG")

  detail_url <- paste0(
    api_base,
    "/hcmRestApi/resources/latest/recruitingCEJobRequisitionDetails/",
    job_id_oracle,
    "?onlyData=true&expand=all"
  )

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
    job_responsibilities = NA_character_,
    min_education = NA_character_,
    min_experience = NA_character_,
    preferred_qualifications = NA_character_,
    salary_range = NA_character_,
    job_identification = NA_character_,
    job_category = NA_character_,
    degree_level = NA_character_,
    ecl_gtc_required = NA_character_
  )

  if (is.null(response) || status_code(response) != 200) {
    return(na_result)
  }

  json_text <- content(response, as = "text", encoding = "UTF-8")
  detail <- fromJSON(json_text, simplifyVector = FALSE)

  # --- Extract JOB INFO fields ---
  job_identification <- if (!is.null(detail$Id)) as.character(detail$Id) else NA_character_
  job_category <- if (!is.null(detail$Category)) detail$Category else NA_character_
  degree_level <- if (!is.null(detail$StudyLevel)) detail$StudyLevel else NA_character_

  # ECL/GTC: check OrganizationDescriptionStr for export control language
  ecl_gtc <- NA_character_
  org_desc <- detail$OrganizationDescriptionStr
  if (!is.null(org_desc) && nchar(org_desc) > 0) {
    if (grepl("export control|export license|ECL|GTC", org_desc, ignore.case = TRUE)) {
      ecl_gtc <- "Yes"
    } else {
      ecl_gtc <- "No"
    }
  }

  # --- Parse HTML sections ---
  desc_html <- detail$ExternalDescriptionStr

  # Responsibilities: try multiple heading patterns in order of specificity
  responsibility_patterns <- c(
    "Responsibilities\\s+include\\s*:?",
    "Specific\\s+responsibilities\\s+(?:could|may|will)\\s+include\\s*:?",
    "Key\\s+Responsibilities\\s*:?",
    "responsibilities\\s+of\\s+a\\s+[^:]+\\s+in\\s+this\\s+role\\s+include\\s*:?",
    "you\\s+will\\s+be\\s+responsible\\s+for\\s*:?",
    "About\\s+the\\s+job"
  )
  responsibilities <- extract_html_section_bullets(desc_html, responsibility_patterns)

  # Fallback: try ExternalResponsibilitiesStr if present
  if (is.na(responsibilities)) {
    resp_html <- detail$ExternalResponsibilitiesStr
    if (!is.null(resp_html) && nchar(resp_html) > 0) {
      responsibilities <- extract_html_section_bullets(resp_html, c("."))
    }
  }

  # Final fallback: extract prose description paragraphs
  if (is.na(responsibilities)) {
    responsibilities <- extract_prose_description(desc_html)
  }

  # Minimum requirements and Preferred qualifications: from ExternalQualificationsStr
  qual_html <- detail$ExternalQualificationsStr
  min_requirements <- extract_html_section_bullets(qual_html, c("Minimum\\s+[Rr]equirements\\s*:?"))
  preferred_quals <- extract_html_section_bullets(qual_html, c("Preferred\\s+[Qq]ualifications\\s*:?"))

  # Salary: try to find in description or qualifications
  salary <- NA_character_
  for (html_field in c(desc_html, qual_html)) {
    if (!is.null(html_field) && nchar(html_field) > 0) {
      salary_match <- str_match(html_field, "\\$[0-9,]+\\s*[-\u2013]\\s*\\$[0-9,]+")
      if (!is.na(salary_match[1, 1])) {
        salary <- salary_match[1, 1]
        break
      }
    }
  }

  Sys.sleep(SCRAPER_CONFIG$delay_between_requests)

  return(list(
    job_responsibilities = responsibilities,
    min_education = min_requirements,
    min_experience = NA_character_,
    preferred_qualifications = preferred_quals,
    salary_range = salary,
    job_identification = job_identification,
    job_category = job_category,
    degree_level = degree_level,
    ecl_gtc_required = ecl_gtc
  ))
}

#' Main function to scrape an Oracle CX company
#'
#' @param company_name Name of the company
#' @param base_url Base URL of career site
#' @param fetch_details Whether to fetch full job details
#' @return Dataframe of jobs
scrape_oracle_company <- function(company_name, base_url, fetch_details = TRUE) {

  log_message(paste("=== Starting Oracle CX scrape for", company_name, "==="))
  start_time <- Sys.time()

  api_base <- extract_oracle_api_base(base_url)

  jobs <- scrape_oracle(company_name, base_url)

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
        scrape_oracle_job_details(api_base, jobs$job_id_oracle[i]),
        error = function(e) {
          log_message(paste("Error fetching details for job", jobs$job_id_oracle[i], ":", e$message), level = "WARN")
          list(
            job_responsibilities = NA_character_,
            min_education = NA_character_,
            min_experience = NA_character_,
            preferred_qualifications = NA_character_,
            salary_range = NA_character_,
            job_identification = NA_character_,
            job_category = NA_character_,
            degree_level = NA_character_,
            ecl_gtc_required = NA_character_
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
    # Add NA detail columns when skipping details
    jobs <- jobs %>%
      mutate(
        job_responsibilities = NA_character_,
        min_education = NA_character_,
        min_experience = NA_character_,
        preferred_qualifications = NA_character_,
        salary_range = NA_character_,
        job_identification = NA_character_,
        job_category = NA_character_,
        degree_level = NA_character_,
        ecl_gtc_required = NA_character_
      )
  }

  # Drop the internal Oracle ID column and add metadata
  jobs <- jobs %>%
    select(-job_id_oracle) %>%
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
