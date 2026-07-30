# TalentBrew Platform Scraper
# ============================
# Scrapes job listings from TalentBrew career sites via JSON/HTML API
# Used by: Synopsys
# Detail pages contain JSON-LD (application/ld+json) with full job description HTML.

library(httr)
library(jsonlite)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)

source("config/scraper_config.R")
source("scrapers/utils.R")

#' Fetch a page of job listings from a TalentBrew career site
#'
#' @param base_url Base careers URL (e.g., https://careers.synopsys.com)
#' @param page_num Page number (1-indexed)
#' @param per_page Number of results per page
#' @return Parsed JSON response or NULL on failure
fetch_talentbrew_page <- function(base_url, page_num = 1, per_page = 100) {

  search_url <- paste0(
    base_url,
    "/search-jobs/results?ActiveFacetID=0&CurrentPage=", page_num,
    "&RecordsPerPage=", per_page,
    "&Distance=50&RadiusUnitType=0&Keywords=&Location=",
    "&ShowRadius=False&IsPagination=True",
    "&CustomFacetName=&FacetTerm=&FacetType=0",
    "&SearchResultsModuleName=Search+Results",
    "&SearchFiltersModuleName=Search+Filters",
    "&SortCriteria=0&SortDirection=0&SearchType=5",
    "&PostalCode=&fc=&fl=&fcf=&afc=&afl=&afcf="
  )

  log_message(paste("Fetching TalentBrew page:", page_num), level = "DEBUG")

  response <- tryCatch({
    GET(
      search_url,
      timeout(SCRAPER_CONFIG$request_timeout),
      user_agent(SCRAPER_CONFIG$user_agent),
      add_headers(
        "Accept" = "application/json",
        "X-Requested-With" = "XMLHttpRequest"
      )
    )
  }, error = function(e) {
    log_message(paste("TalentBrew request failed:", e$message), level = "ERROR")
    return(NULL)
  })

  if (is.null(response)) return(NULL)

  if (status_code(response) != 200) {
    log_message(paste("TalentBrew API returned HTTP", status_code(response)), level = "WARN")
    return(NULL)
  }

  json_text <- content(response, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(
    fromJSON(json_text, simplifyVector = FALSE),
    error = function(e) {
      log_message(paste("Failed to parse TalentBrew JSON:", e$message), level = "ERROR")
      NULL
    }
  )

  return(parsed)
}

#' Parse job listings from TalentBrew search results HTML
#'
#' @param results_html HTML string from the results field of the API response
#' @param base_url Base careers URL for constructing absolute job URLs
#' @return Tibble of jobs (job_title, location, job_url, posting_date)
parse_talentbrew_results <- function(results_html, base_url) {

  if (is.null(results_html) || nchar(results_html) == 0) {
    return(tibble(job_title = character(), location = character(),
                  job_url = character(), posting_date = as.Date(character())))
  }

  page <- tryCatch(
    read_html(paste0("<div>", results_html, "</div>")),
    error = function(e) {
      log_message(paste("Failed to parse results HTML:", e$message), level = "ERROR")
      NULL
    }
  )
  if (is.null(page)) {
    return(tibble(job_title = character(), location = character(),
                  job_url = character(), posting_date = as.Date(character())))
  }

  items <- page %>% html_elements("li")
  if (length(items) == 0) {
    return(tibble(job_title = character(), location = character(),
                  job_url = character(), posting_date = as.Date(character())))
  }

  jobs <- list()
  for (item in items) {
    # TalentBrew structure: <a class="sr-job-link"><h2>Title</h2>...spans...</a>
    job_link <- item %>% html_element("a.sr-job-link")
    if (is.null(job_link) || is.na(html_name(job_link))) next

    title_el <- job_link %>% html_element("h2")
    if (is.null(title_el) || is.na(html_name(title_el))) next

    title <- str_squish(html_text2(title_el))
    href <- html_attr(job_link, "href")
    if (is.null(title) || nchar(title) == 0 || is.null(href)) next

    # Build absolute URL
    job_url <- if (startsWith(href, "http")) href else paste0(sub("/$", "", base_url), href)

    # Extract spans from the job link: [Location, Category, Posted, Job ID]
    spans <- job_link %>% html_elements("span") %>% html_text2()

    location <- if (length(spans) >= 1) str_squish(spans[1]) else NA_character_

    # Parse posted date from "Posted: MM/DD/YYYY" span
    posting_date <- NA_Date_
    posted_spans <- spans[grepl("Posted", spans)]
    if (length(posted_spans) > 0) {
      date_str <- gsub(".*Posted:\\s*", "", posted_spans[1])
      posting_date <- tryCatch(
        as.Date(str_squish(date_str), format = "%m/%d/%Y"),
        error = function(e) NA_Date_
      )
    }

    jobs <- c(jobs, list(tibble(
      job_title = title,
      location = location,
      job_url = job_url,
      posting_date = posting_date
    )))
  }

  if (length(jobs) > 0) bind_rows(jobs) else {
    tibble(job_title = character(), location = character(),
           job_url = character(), posting_date = as.Date(character()))
  }
}

#' Scrape all job listings from a TalentBrew career site with pagination
#'
#' @param company_name Name of the company
#' @param base_url Base careers URL
#' @param max_pages Maximum pages to scrape (safety limit)
#' @return Tibble of all job listings
scrape_talentbrew <- function(company_name, base_url, max_pages = SCRAPER_CONFIG$max_pages_per_company) {

  log_message(paste("Starting TalentBrew scrape for", company_name))

  all_jobs <- list()
  per_page <- 100
  page_num <- 1

  while (page_num <= max_pages) {
    log_message(paste("Scraping page", page_num, "for", company_name))

    parsed <- fetch_talentbrew_page(base_url, page_num, per_page)
    if (is.null(parsed)) {
      log_message("Failed to fetch page - stopping", level = "WARN")
      break
    }

    page_jobs <- parse_talentbrew_results(parsed$results, base_url)

    if (nrow(page_jobs) == 0) {
      log_message("No job rows parsed - stopping")
      break
    }

    all_jobs <- c(all_jobs, list(page_jobs))
    log_message(paste("  Found", nrow(page_jobs), "jobs on this page"))

    if (nrow(page_jobs) < per_page) {
      log_message("Last page (fewer results than page size)")
      break
    }

    page_num <- page_num + 1
    Sys.sleep(SCRAPER_CONFIG$delay_between_requests)
  }

  if (length(all_jobs) > 0) {
    jobs_df <- bind_rows(all_jobs)
    log_message(paste("Found", nrow(jobs_df), "total jobs for", company_name))
  } else {
    jobs_df <- tibble(job_title = character(), location = character(),
                      job_url = character(), posting_date = as.Date(character()))
    log_message(paste("No jobs found for", company_name), level = "WARN")
  }

  return(jobs_df)
}

#' Extract a section from TalentBrew description HTML by header text
#'
#' Sections in Synopsys TalentBrew use <h3><b>Header:</b></h3> pattern,
#' often with heavy inline styles and &rsquo; entities for apostrophes.
#' Finds the header in raw HTML by matching text content, then extracts
#' content between it and the next </h3> closing tag.
#'
#' @param desc_html Raw description HTML string
#' @param header_pattern Regex to match the header text (plain text, handles entities)
#' @return Character string of section content or NA
extract_talentbrew_section <- function(desc_html, header_pattern) {

  if (is.null(desc_html) || is.na(desc_html) || nchar(desc_html) == 0) {
    return(NA_character_)
  }

  # Build pattern that matches the header text with possible &rsquo; for apostrophes
  # Replace . (regex any) positions that might be apostrophes with (?:'|&rsquo;|.)
  header_html_pattern <- gsub("\\.", "(?:'|&rsquo;|&\\\\#\\\\d+;|.)", header_pattern)

  # Find </h3> after the header text, then extract content until next <h3
  # Pattern: header text ... </h3> ... (content) ... <h3
  search_pattern <- paste0("(?si)", header_html_pattern, "[^<]*(?:</[^>]*>)*\\s*</h3>")
  match <- regexpr(search_pattern, desc_html, perl = TRUE)
  if (match < 0) return(NA_character_)

  # Content starts after the </h3> that closes this header
  content_start <- match + attr(match, "match.length")
  remaining <- substr(desc_html, content_start, nchar(desc_html))

  # Truncate at the next <h3 tag
  next_h3 <- regexpr("<h3[\\s>]", remaining, perl = TRUE)
  if (next_h3 > 0) {
    remaining <- substr(remaining, 1, next_h3 - 1)
  }

  if (nchar(str_trim(remaining)) == 0) return(NA_character_)

  # Parse the section content
  section_doc <- tryCatch(
    read_html(paste0("<div>", remaining, "</div>")),
    error = function(e) NULL
  )
  if (is.null(section_doc)) return(NA_character_)

  # Try bullet items first
  li_items <- section_doc %>% html_elements("li") %>% html_text2()
  li_items <- str_squish(li_items)
  li_items <- li_items[nchar(li_items) > 0]

  # Also get paragraph text
  paras <- section_doc %>% html_elements("p") %>% html_text2()
  paras <- str_squish(paras)
  paras <- paras[nchar(paras) > 0]

  if (length(li_items) > 0 && length(paras) > 0) {
    return(paste(c(paras, li_items), collapse = "\n"))
  } else if (length(li_items) > 0) {
    return(paste(li_items, collapse = "\n"))
  } else if (length(paras) > 0) {
    return(paste(paras, collapse = "\n"))
  }

  return(NA_character_)
}

#' Fetch and parse job details from a TalentBrew detail page
#'
#' Extracts job description from the JSON-LD block and metadata from visible HTML.
#'
#' @param job_url URL of the job detail page
#' @return List with extracted fields or list of NAs on failure
scrape_talentbrew_job_details <- function(job_url) {

  result <- list(
    job_responsibilities = NA_character_,
    min_education = NA_character_,
    min_experience = NA_character_,
    preferred_qualifications = NA_character_,
    salary_range = NA_character_,
    essential_skills = NA_character_
  )

  log_message(paste("Fetching TalentBrew job details:", basename(job_url)), level = "DEBUG")

  response <- tryCatch({
    GET(
      job_url,
      timeout(SCRAPER_CONFIG$request_timeout),
      user_agent(SCRAPER_CONFIG$user_agent)
    )
  }, error = function(e) {
    log_message(paste("TalentBrew detail request failed:", e$message), level = "ERROR")
    return(NULL)
  })

  if (is.null(response) || status_code(response) != 200) return(result)

  page_html <- content(response, as = "text", encoding = "UTF-8")
  page_doc <- tryCatch(read_html(page_html), error = function(e) NULL)
  if (is.null(page_doc)) return(result)

  # --- Extract JSON-LD block ---
  jsonld_scripts <- page_doc %>% html_elements("script[type='application/ld+json']")
  desc_html <- NULL

  for (script in jsonld_scripts) {
    json_text <- html_text(script)
    parsed_json <- tryCatch(fromJSON(json_text, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(parsed_json) && !is.null(parsed_json$`@type`) && parsed_json$`@type` == "JobPosting") {
      desc_html <- parsed_json$description
      break
    }
  }

  if (is.null(desc_html) || nchar(desc_html) == 0) return(result)

  # --- Extract responsibilities from "What You'll Be Doing" ---
  responsibilities <- extract_talentbrew_section(desc_html, "What You.ll Be Doing")

  # Also try "The Impact You Will Have" as supplementary responsibilities
  impact <- extract_talentbrew_section(desc_html, "The Impact You Will Have")
  if (!is.na(impact) && !is.na(responsibilities)) {
    responsibilities <- paste(responsibilities, impact, sep = "\n")
  } else if (!is.na(impact)) {
    responsibilities <- impact
  }

  result$job_responsibilities <- responsibilities

  # --- Extract education & experience from "What You'll Need" ---
  quals_text <- extract_talentbrew_section(desc_html, "What You.ll Need")

  if (!is.na(quals_text)) {
    bullets <- str_split(quals_text, "\n")[[1]]
    bullets <- str_squish(bullets)
    bullets <- bullets[nchar(bullets) > 0]

    edu_pattern <- "(?i)(\\bbachelor|\\bmaster(?!ing)|\\bassociate(?!d)|ph\\.?d|\\bdegree\\b|\\bdiploma\\b|high\\s+school|\\bGED\\b|education|technical\\s+school)|(?-i:\\bB\\.?S\\.?\\b|\\bM\\.?S\\.?\\b|\\bB\\.?A\\.?\\b|\\bM\\.?A\\.?\\b)"

    edu_idx <- which(grepl(edu_pattern, bullets, perl = TRUE))
    exp_pattern <- "(?i)(\\byears?\\b.*\\b(?:experience|professional)\\b|\\bexperience\\b.*\\byears?\\b|\\bminimum\\b.*\\byears?\\b)"
    exp_idx <- which(grepl(exp_pattern, bullets, perl = TRUE))

    # Some bullets match both edu and exp (e.g., "BS+5, MS+3, PhD+0")
    edu_only <- setdiff(edu_idx, exp_idx)
    exp_only <- setdiff(exp_idx, edu_idx)
    both_idx <- intersect(edu_idx, exp_idx)

    edu_bullets <- bullets[edu_only]
    exp_bullets <- bullets[exp_only]

    for (bi in both_idx) {
      bullet <- bullets[bi]
      if (grepl("(?i)required.*education|degree\\s+type|education.*degree", bullet, perl = TRUE)) {
        edu_bullets <- c(edu_bullets, bullet)
      } else {
        exp_bullets <- c(exp_bullets, bullet)
      }
    }

    # Remaining bullets (neither edu nor exp) go to experience
    other_idx <- setdiff(seq_along(bullets), union(edu_idx, exp_idx))
    exp_bullets <- c(exp_bullets, bullets[other_idx])

    if (length(edu_bullets) > 0) result$min_education <- paste(edu_bullets, collapse = "\n")
    if (length(exp_bullets) > 0) result$min_experience <- paste(exp_bullets, collapse = "\n")
  }

  # --- Extract essential skills from "Who You Are" ---
  who_you_are <- extract_talentbrew_section(desc_html, "Who You Are")
  result$essential_skills <- who_you_are

  # --- Extract salary from visible HTML ---
  # Look for "Base Salary Range" in page h3 or span elements
  salary_nodes <- page_doc %>% html_elements("h3, span")
  for (node in salary_nodes) {
    node_text <- html_text2(node)
    if (grepl("Base Salary Range", node_text, ignore.case = TRUE)) {
      salary_match <- regmatches(node_text, regexpr("\\$[0-9,]+-\\$[0-9,]+", node_text, perl = TRUE))
      if (length(salary_match) > 0) {
        result$salary_range <- salary_match[1]
      }
      break
    }
  }

  # Fallback: check "Rewards and Benefits" section in description for salary
  if (is.na(result$salary_range)) {
    rewards <- extract_talentbrew_section(desc_html, "Rewards and Benefits")
    if (!is.na(rewards)) {
      salary_match <- regmatches(rewards, regexpr("\\$[0-9,]+-\\$[0-9,]+", rewards, perl = TRUE))
      if (length(salary_match) > 0) {
        result$salary_range <- salary_match[1]
      }
    }
  }

  Sys.sleep(SCRAPER_CONFIG$delay_between_requests)

  return(result)
}

#' Main function to scrape a TalentBrew company
#'
#' @param company_name Name of the company
#' @param base_url Base URL of career site
#' @param fetch_details Whether to fetch detail pages for each job
#' @return Dataframe of jobs with listing and detail data
scrape_talentbrew_company <- function(company_name, base_url, fetch_details = TRUE) {

  log_message(paste("=== Starting TalentBrew scrape for", company_name, "==="))
  start_time <- Sys.time()

  jobs <- scrape_talentbrew(company_name, base_url)

  if (nrow(jobs) == 0) {
    log_message(paste("No jobs found for", company_name), level = "WARN")
    return(data.frame())
  }

  # --- US location filter ---
  # TalentBrew sites like Synopsys have worldwide postings; filter to US only
  us_state_pattern <- paste0(
    ",\\s*(Alabama|Alaska|Arizona|Arkansas|California|Colorado|Connecticut|",
    "Delaware|Florida|Georgia|Hawaii|Idaho|Illinois|Indiana|Iowa|Kansas|",
    "Kentucky|Louisiana|Maine|Maryland|Massachusetts|Michigan|Minnesota|",
    "Mississippi|Missouri|Montana|Nebraska|Nevada|New Hampshire|New Jersey|",
    "New Mexico|New York|North Carolina|North Dakota|Ohio|Oklahoma|Oregon|",
    "Pennsylvania|Rhode Island|South Carolina|South Dakota|Tennessee|Texas|",
    "Utah|Vermont|Virginia|Washington|West Virginia|Wisconsin|Wyoming)\\b|",
    "United States"
  )
  before_count <- nrow(jobs)
  jobs <- jobs %>% filter(grepl(us_state_pattern, location, perl = TRUE) | is.na(location))
  filtered_count <- before_count - nrow(jobs)
  log_message(paste("US location filter: kept", nrow(jobs), "of", before_count,
                    "jobs (removed", filtered_count, "non-US)"))

  if (nrow(jobs) == 0) {
    log_message(paste("No US jobs found for", company_name), level = "WARN")
    return(data.frame())
  }

  # --- Fetch detail pages ---
  if (fetch_details) {
    log_message(paste("Fetching details for", nrow(jobs), "US jobs"))

    detail_results <- list()
    pb <- txtProgressBar(min = 0, max = nrow(jobs), style = 3, file = stderr())

    for (i in seq_len(nrow(jobs))) {
      details <- scrape_talentbrew_job_details(jobs$job_url[i])
      detail_results[[i]] <- details
      setTxtProgressBar(pb, i)
    }
    close(pb)

    # Bind detail results into the jobs dataframe
    jobs <- jobs %>%
      mutate(
        job_responsibilities = map_chr(detail_results, ~ .x$job_responsibilities %||% NA_character_),
        min_education = map_chr(detail_results, ~ .x$min_education %||% NA_character_),
        min_experience = map_chr(detail_results, ~ .x$min_experience %||% NA_character_),
        preferred_qualifications = map_chr(detail_results, ~ .x$preferred_qualifications %||% NA_character_),
        salary_range = map_chr(detail_results, ~ .x$salary_range %||% NA_character_),
        essential_skills = map_chr(detail_results, ~ .x$essential_skills %||% NA_character_)
      )
  } else {
    jobs <- jobs %>%
      mutate(
        job_responsibilities = NA_character_,
        min_education = NA_character_,
        min_experience = NA_character_,
        preferred_qualifications = NA_character_,
        salary_range = NA_character_,
        essential_skills = NA_character_
      )
  }

  # Add metadata
  jobs <- jobs %>%
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
