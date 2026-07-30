# Boeing Scraper
# ==============
# Scrapes job listings from jobs.boeing.com (TalentBrew / Radancy platform)
# Strategy: paginate the /search-jobs/results JSON API (50 per page),
#            filter to US locations, then fetch detail pages via JSON-LD.

library(httr)
library(jsonlite)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)

source("config/scraper_config.R")
source("scrapers/utils.R")

BOEING_BASE <- "https://jobs.boeing.com"
BOEING_PER_PAGE <- 50

# Known Boeing boilerplate section headings — used as stop boundaries
.boeing_stop_re <- paste0(
  "(?i)^(",
  "Shift|Union|Conflict of Interest|Relocation|Travel|Drug Free Workplace|",
  "Export Control Requirements|Pay & Benefits|Summary Pay Range|",
  "Equal Opportunity Employer|Notice to candidates|",
  "NASA Access|Contingent upon|",
  "This position must meet export|This position is expected to be",
  ")\\s*:?\\s*$"
)

# --- Listing fetch & parse -------------------------------------------

.boeing_fetch_page <- function(page_num) {
  url <- paste0(
    BOEING_BASE, "/search-jobs/results",
    "?ActiveFacetID=0&CurrentPage=", page_num,
    "&RecordsPerPage=", BOEING_PER_PAGE,
    "&Distance=50&RadiusUnitType=0&Keywords=&Location=",
    "&ShowRadius=False&IsPagination=True",
    "&CustomFacetName=&FacetTerm=&FacetType=0",
    "&SearchResultsModuleName=Search+Results",
    "&SearchFiltersModuleName=Search+Filters",
    "&SortCriteria=0&SortDirection=0&SearchType=5",
    "&PostalCode=&fc=&fl=&fcf=&afc=&afl=&afcf="
  )

  r <- tryCatch(
    GET(url, timeout(SCRAPER_CONFIG$request_timeout),
        user_agent(SCRAPER_CONFIG$user_agent),
        add_headers("Accept" = "application/json",
                    "X-Requested-With" = "XMLHttpRequest")),
    error = function(e) NULL
  )
  if (is.null(r) || status_code(r) != 200) return(NULL)

  txt <- content(r, as = "text", encoding = "UTF-8")
  tryCatch(fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
}

.boeing_parse_listings <- function(results_html) {
  if (is.null(results_html) || nchar(results_html) == 0)
    return(tibble(job_title = character(), location = character(),
                  job_url = character(), posting_date = as.Date(character())))

  doc <- tryCatch(read_html(paste0("<div>", results_html, "</div>")),
                  error = function(e) NULL)
  if (is.null(doc)) return(tibble(job_title = character(), location = character(),
                                  job_url = character(), posting_date = as.Date(character())))

  links <- doc %>% html_elements("a")
  if (length(links) == 0) return(tibble(job_title = character(), location = character(),
                                        job_url = character(), posting_date = as.Date(character())))

  titles <- links %>% html_elements("span") %>% html_text2()
  hrefs  <- html_attr(links, "href")
  locs   <- doc %>% html_elements("span.location") %>% html_text2()
  dates  <- doc %>% html_elements("span.date")    %>% html_text2()

  # Align locs/dates with links (some <a> are non-job save buttons — filter by href pattern)
  job_idx <- which(grepl("^/job/", hrefs))
  if (length(job_idx) == 0) return(tibble(job_title = character(), location = character(),
                                          job_url = character(), posting_date = as.Date(character())))

  tibble(
    job_title    = titles[job_idx],
    location     = if (length(locs) >= length(job_idx)) locs[seq_along(job_idx)] else rep(NA_character_, length(job_idx)),
    job_url      = paste0(BOEING_BASE, hrefs[job_idx]),
    posting_date = tryCatch(as.Date(dates[seq_along(job_idx)], format = "%m/%d/%Y"),
                            error = function(e) rep(NA_Date_, length(job_idx)))
  )
}

# --- Detail page parsing ---------------------------------------------

.boeing_extract_section <- function(plain_text, heading_pattern,
                                    stop_headings = NULL) {
  sp <- paste0("(?im)^\\s*", heading_pattern, "\\s*$\\n?")
  parts <- tryCatch(strsplit(plain_text, sp, perl = TRUE)[[1]], error = function(e) NULL)
  if (is.null(parts) || length(parts) < 2) return(NA_character_)

  after <- parts[2]

  # Stop at next major section heading (line ending with colon on its own)
  # Generic: any line that is just "Words Words:" (title-case or upper, ends with colon)
  generic_stop <- "(?im)^[A-Z][^\\n]{2,60}:\\s*$"
  stop_pos <- regexpr(generic_stop, after, perl = TRUE)
  if (stop_pos > 0) after <- substr(after, 1, stop_pos - 1)

  lines <- str_split(after, "\n")[[1]]
  lines <- str_squish(lines)
  lines <- lines[nchar(lines) > 2]
  # Remove boilerplate lines
  boiler <- paste0(
    "(?i)(At Boeing, we innovate|committed to fostering|Find your future with us|",
    "This position is expected to be 100%|100% onsite|",
    "post offer applicants|Drug Free|export control compliance|",
    "U\\.S\\. Person|22 C\\.F\\.R|Equal Opportunity|",
    "Boeing does not|unsolicited resumes|",
    "^Job Description$)"
  )
  lines <- lines[!grepl(boiler, lines, perl = TRUE)]
  if (length(lines) == 0) return(NA_character_)
  paste(lines, collapse = "\n")
}

.boeing_parse_details <- function(job_url) {
  result <- list(
    job_responsibilities    = NA_character_,
    min_education           = NA_character_,
    min_experience          = NA_character_,
    preferred_qualifications = NA_character_,
    salary_range            = NA_character_,
    essential_skills        = NA_character_,
    job_category            = NA_character_
  )

  r <- tryCatch(
    GET(job_url, timeout(SCRAPER_CONFIG$request_timeout),
        user_agent(SCRAPER_CONFIG$user_agent)),
    error = function(e) NULL
  )
  if (is.null(r) || status_code(r) != 200) return(result)

  page_html <- content(r, as = "text", encoding = "UTF-8")
  page_doc  <- tryCatch(read_html(page_html), error = function(e) NULL)
  if (is.null(page_doc)) return(result)

  # Job category from page metadata
  cat_el <- page_doc %>% html_element("meta[name='tb-category']")
  if (!is.na(html_attr(cat_el, "content"))) {
    result$job_category <- str_squish(html_attr(cat_el, "content"))
  }

  # Extract description from JSON-LD
  desc_html <- NULL
  for (s in (page_doc %>% html_elements("script[type='application/ld+json']") %>% html_text())) {
    p <- tryCatch(fromJSON(s, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(p) && identical(p[["@type"]], "JobPosting")) {
      desc_html <- p$description
      break
    }
  }
  if (is.null(desc_html) || nchar(desc_html) == 0) return(result)

  # Convert to plain text (html_text2 handles fragmented Word headings)
  desc_doc  <- tryCatch(read_html(paste0("<body>", desc_html, "</body>")), error = function(e) NULL)
  if (is.null(desc_doc)) return(result)
  plain <- html_text2(desc_doc)

  # --- Job Responsibilities: combine Summary + Position Responsibilities ---
  summary_text <- .boeing_extract_section(plain, "Summary\\s*:?")
  pos_resp_text <- .boeing_extract_section(plain, "Position\\s+Responsibilities\\s*:?")

  resp_parts <- c()
  if (!is.na(summary_text))   resp_parts <- c(resp_parts, summary_text)
  if (!is.na(pos_resp_text))  resp_parts <- c(resp_parts, pos_resp_text)
  result$job_responsibilities <- if (length(resp_parts) > 0) paste(resp_parts, collapse = "\n") else NA_character_

  # --- Basic Qualifications → min_experience (with edu split) ---
  basic_raw <- .boeing_extract_section(plain,
    "Basic\\s+Qualifications\\s*(?:\\([^)]*\\))?\\s*:?")

  edu_pattern <- paste0(
    "(?i)(\\bbachelor|\\bmaster(?!ing)|\\bassociate'?s?\\s+degree|ph\\.?d|",
    "\\bdegree(?!\\s+of)\\b|college|technical\\s+school)|",
    "(?-i:\\bB\\.S[c]?\\.?\\b|\\bM\\.S[c]?\\.?\\b|",
    "\\bB\\.A\\.?\\b|\\bM\\.A\\.?\\b|",
    "\\bBS[A-Z]{2,4}\\b|\\bMS[A-Z]{2,4}\\b)"
  )

  if (!is.na(basic_raw)) {
    bullets <- str_split(basic_raw, "\n")[[1]]
    bullets <- str_squish(bullets[nchar(str_squish(bullets)) > 0])

    edu_idx <- which(grepl(edu_pattern, bullets, perl = TRUE))
    if (length(edu_idx) > 0) {
      result$min_education <- paste(bullets[edu_idx], collapse = "\n")
      remaining <- bullets[-edu_idx]
      if (length(remaining) > 0)
        result$min_experience <- paste(remaining, collapse = "\n")
    } else {
      result$min_experience <- basic_raw
    }
  }

  # --- Preferred Qualifications ---
  result$preferred_qualifications <- .boeing_extract_section(plain,
    "Preferred\\s+Qualifications\\s*(?:\\([^)]*\\))?\\s*:?")

  # --- Salary from "Summary Pay Range:" ---
  pay_section <- .boeing_extract_section(plain, "Summary\\s+Pay\\s+Range\\s*:?")
  if (!is.na(pay_section)) {
    # e.g. "$99,900 - $135,150" or "$53.26 - $72.09"
    m <- regmatches(pay_section, regexpr(
      "\\$[0-9,]+(?:\\.[0-9]{2})?\\s*[-\u2013]\\s*\\$[0-9,]+(?:\\.[0-9]{2})?",
      pay_section, perl = TRUE))
    if (length(m) > 0) result$salary_range <- m[1]
  }

  return(result)
}

# --- Main orchestration ----------------------------------------------

#' Scrape all US Boeing jobs
#'
#' @param company_name Display name (default "Boeing")
#' @return Tibble of US jobs with detail fields
scrape_boeing_company <- function(company_name = "Boeing") {

  log_message(paste("=== Starting Boeing scrape ==="))
  start_time <- Sys.time()

  us_state_re <- paste0(
    "Alabama|Alaska|Arizona|Arkansas|California|Colorado|Connecticut|",
    "Delaware|Florida|Georgia|Hawaii|Idaho|Illinois|Indiana|Iowa|Kansas|",
    "Kentucky|Louisiana|Maine|Maryland|Massachusetts|Michigan|Minnesota|",
    "Mississippi|Missouri|Montana|Nebraska|Nevada|New Hampshire|New Jersey|",
    "New Mexico|New York|North Carolina|North Dakota|Ohio|Oklahoma|Oregon|",
    "Pennsylvania|Rhode Island|South Carolina|South Dakota|Tennessee|Texas|",
    "Utah|Vermont|Virginia|Washington|West Virginia|Wisconsin|Wyoming"
  )

  # Step 1: Get total page count
  first <- .boeing_fetch_page(1)
  if (is.null(first)) {
    log_message("Failed to fetch first page", level = "ERROR")
    return(data.frame())
  }

  # Parse page 1 to get total pages
  doc1 <- tryCatch(read_html(paste0("<div>", first$results, "</div>")), error = function(e) NULL)
  total_pages <- if (!is.null(doc1)) {
    tp <- doc1 %>% html_element("[data-total-pages]") %>% html_attr("data-total-pages")
    as.integer(tp %||% "1")
  } else 1L

  total_jobs <- if (!is.null(doc1)) {
    tj <- doc1 %>% html_element("[data-total-results]") %>% html_attr("data-total-results")
    as.integer(tj %||% "0")
  } else 0L

  log_message(paste("Boeing: total jobs =", total_jobs, "| pages =", total_pages,
                    "| per page =", BOEING_PER_PAGE))

  # Step 2: Paginate listings
  all_listings <- list()
  page_data <- list(first)  # already have page 1

  for (pg in seq_len(total_pages)) {
    if (pg == 1) {
      parsed <- first
    } else {
      Sys.sleep(SCRAPER_CONFIG$delay_between_requests)
      parsed <- .boeing_fetch_page(pg)
    }
    if (is.null(parsed)) {
      log_message(paste("Failed to fetch page", pg, "- skipping"), level = "WARN")
      next
    }

    page_jobs <- .boeing_parse_listings(parsed$results)
    if (nrow(page_jobs) == 0) {
      log_message(paste("No jobs on page", pg, "- stopping"))
      break
    }

    # Filter to US
    us_jobs <- page_jobs %>% filter(grepl(us_state_re, location, perl = TRUE))
    all_listings <- c(all_listings, list(us_jobs))

    log_message(paste0("Page ", pg, "/", total_pages,
                       ": ", nrow(page_jobs), " jobs, ",
                       nrow(us_jobs), " US"))
  }

  if (length(all_listings) == 0) {
    log_message("No US Boeing jobs found", level = "WARN")
    return(data.frame())
  }

  listings <- bind_rows(all_listings)
  listings <- listings[!duplicated(listings$job_url), ]
  log_message(paste("Total unique US jobs:", nrow(listings)))

  # Step 3: Fetch detail pages
  log_message(paste("Fetching details for", nrow(listings), "jobs"))
  details <- vector("list", nrow(listings))
  pb <- txtProgressBar(min = 0, max = nrow(listings), style = 3, file = stderr())

  for (i in seq_len(nrow(listings))) {
    details[[i]] <- tryCatch(
      .boeing_parse_details(listings$job_url[i]),
      error = function(e) list(
        job_responsibilities = NA_character_, min_education = NA_character_,
        min_experience = NA_character_, preferred_qualifications = NA_character_,
        salary_range = NA_character_, essential_skills = NA_character_,
        job_category = NA_character_
      )
    )
    setTxtProgressBar(pb, i)
    Sys.sleep(SCRAPER_CONFIG$delay_between_requests)
  }
  close(pb)

  # Step 4: Combine
  jobs <- listings %>%
    mutate(
      job_responsibilities     = map_chr(details, ~ .x$job_responsibilities %||% NA_character_),
      min_education            = map_chr(details, ~ .x$min_education %||% NA_character_),
      min_experience           = map_chr(details, ~ .x$min_experience %||% NA_character_),
      preferred_qualifications = map_chr(details, ~ .x$preferred_qualifications %||% NA_character_),
      salary_range             = map_chr(details, ~ .x$salary_range %||% NA_character_),
      essential_skills         = map_chr(details, ~ .x$essential_skills %||% NA_character_),
      job_category             = map_chr(details, ~ .x$job_category %||% NA_character_),
      company_name             = company_name,
      scraped_at               = Sys.time()
    )

  duration <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  log_message(paste("Completed Boeing in", round(duration, 1), "minutes"))
  log_message(paste("Total US jobs:", nrow(jobs)))

  for (col in c("job_responsibilities", "min_education", "min_experience",
                "preferred_qualifications", "salary_range")) {
    filled <- sum(!is.na(jobs[[col]]))
    log_message(paste0("  ", col, ": ", filled, "/", nrow(jobs)))
  }

  return(jobs)
}
