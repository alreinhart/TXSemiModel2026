# Dayforce HCM Platform Scraper
# ==============================
# Scrapes job listings from Dayforce (formerly Ceridian) career portals
# Used by: SkyWater Technology
# Strategy: probe job ID range via SSR __NEXT_DATA__ on individual job pages,
#           since the job listing API is client-side only (React Query, no public endpoint)

library(httr)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(jsonlite)

source("config/scraper_config.R")
source("scrapers/utils.R")

DAYFORCE_BASE_URL    <- "https://jobs.dayforcehcm.com"
DAYFORCE_PROBE_MIN   <- 5000L    # start of ID probe range
DAYFORCE_PROBE_STEP  <- 5L       # probe every Nth ID
DAYFORCE_MAX_MISS    <- 500L     # stop after this many consecutive misses
DAYFORCE_PROBE_CAP   <- 15000L   # hard upper limit on IDs to probe

# Helpers to access SCRAPER_CONFIG with fallbacks
.dayforce_ua <- function() {
  if (exists("SCRAPER_CONFIG") && !is.null(SCRAPER_CONFIG$user_agent))
    SCRAPER_CONFIG$user_agent
  else
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
}
.dayforce_delay <- function() {
  if (exists("SCRAPER_CONFIG") && !is.null(SCRAPER_CONFIG$delay_between_requests))
    SCRAPER_CONFIG$delay_between_requests
  else
    3
}

#' Extract clientNamespace from a Dayforce careers URL
#'
#' Dayforce URLs: https://jobs.dayforcehcm.com/en-US/{namespace}/CANDIDATEPORTAL/
#'
#' @param careers_url Full careers URL
#' @return Client namespace string (e.g., "skywater")
extract_dayforce_namespace <- function(careers_url) {
  m <- str_match(careers_url, "dayforcehcm\\.com/[^/]+/([^/]+)/")
  if (is.na(m[1, 2])) {
    m2 <- str_match(careers_url, "dayforcehcm\\.com/([^/]+)/")
    if (is.na(m2[1, 2])) {
      log_message("Could not extract namespace from Dayforce URL", level = "ERROR")
      return(NULL)
    }
    return(tolower(m2[1, 2]))
  }
  tolower(m[1, 2])
}

#' Fetch and parse __NEXT_DATA__ jobData for a single Dayforce job page
#'
#' @param namespace Client namespace (e.g., "skywater")
#' @param job_id    Integer job posting ID
#' @return List with jobData fields, or NULL if not found / not a valid job
fetch_dayforce_job <- function(namespace, job_id) {
  url <- sprintf("%s/en-US/%s/CANDIDATEPORTAL/jobs/%d",
                 DAYFORCE_BASE_URL, namespace, job_id)
  r <- tryCatch(
    GET(url, add_headers("User-Agent" = .dayforce_ua()), timeout(20)),
    error = function(e) NULL
  )
  if (is.null(r) || status_code(r) != 200) return(NULL)

  html <- tryCatch(content(r, as = "text", encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(html)) return(NULL)

  nd_json <- tryCatch(
    regmatches(html, regexpr(
      '(?<=<script id="__NEXT_DATA__" type="application/json">)[^<]+',
      html, perl = TRUE)),
    error = function(e) NULL
  )
  if (is.null(nd_json) || length(nd_json) == 0) return(NULL)

  nd <- tryCatch(fromJSON(nd_json, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(nd)) return(NULL)

  jd <- nd$props$pageProps$jobData
  if (is.null(jd) || is.null(jd$jobTitle)) return(NULL)

  # Skip placeholder pages for removed/expired jobs
  if (trimws(jd$jobTitle) %in% c("Job Details", "Job Posting", "")) return(NULL)

  jd$job_url <- url
  jd
}

#' Clean HTML to plain text, collapsing whitespace
html_to_text <- function(html_str) {
  if (is.null(html_str) || !nzchar(html_str)) return(NA_character_)
  tryCatch({
    node <- read_html(html_str)
    # Convert bullet entities to ASCII bullet
    txt <- html_text(node, trim = TRUE)
    txt <- str_replace_all(txt, "\u00b7|\u2022|\u2023|\u25e6", "•")
    txt <- str_squish(txt)
    if (!nzchar(txt)) NA_character_ else txt
  }, error = function(e) NA_character_)
}

#' Split HTML description into named sections using heading patterns
#'
#' @param html_str Raw HTML string from jobDescription
#' @return Named list of section text (character), or NA if absent
parse_dayforce_sections <- function(html_str) {
  if (is.null(html_str) || !nzchar(html_str)) {
    return(list(responsibilities = NA_character_,
                min_education    = NA_character_,
                min_experience   = NA_character_,
                preferred_qualifications = NA_character_))
  }

  # Replace <br> / <p> / <li> tags with newlines for easier splitting
  cleaned <- str_replace_all(html_str, "<br\\s*/?>|</p>|</li>|</div>", "\n")
  cleaned <- str_replace_all(cleaned, "<[^>]+>", "")
  cleaned <- str_replace_all(cleaned, "&amp;", "&")
  cleaned <- str_replace_all(cleaned, "&nbsp;", " ")
  cleaned <- str_replace_all(cleaned, "&bull;|&#8226;|&#x2022;", "• ")
  cleaned <- str_replace_all(cleaned, "&ldquo;|&rdquo;|&#8220;|&#8221;", '"')
  cleaned <- str_replace_all(cleaned, "&lsquo;|&rsquo;|&#8216;|&#8217;", "'")
  cleaned <- str_replace_all(cleaned, "&ndash;|&#8211;", "-")
  cleaned <- str_replace_all(cleaned, "&mdash;|&#8212;", "—")
  cleaned <- str_replace_all(cleaned, "&#[0-9]+;|&[a-z]+;", " ")

  lines <- str_split(cleaned, "\n")[[1]]
  lines <- str_squish(lines)
  lines <- lines[nchar(lines) > 0]

  heading_re <- paste0(
    "(?i)^(responsibilities|position\\s+summary|",
    "required\\s+qualifications?|",
    "education|experience\\s+and\\s+skills?|",
    "preferred\\s+qualifications?|",
    "us\\s+citizenship\\s+required)\\s*:?\\s*$"
  )

  # Assign each line to a section
  sections <- list()
  current_section <- "summary"
  current_lines  <- character()

  for (line in lines) {
    if (grepl(heading_re, line, perl = TRUE)) {
      if (length(current_lines) > 0)
        sections[[current_section]] <- paste(current_lines, collapse = "\n")
      current_section <- tolower(str_replace_all(str_extract(line, "(?i)^[A-Za-z ]+"), "\\s+", "_"))
      current_lines <- character()
    } else {
      current_lines <- c(current_lines, line)
    }
  }
  if (length(current_lines) > 0)
    sections[[current_section]] <- paste(current_lines, collapse = "\n")

  extract_sec <- function(...) {
    keys <- c(...)
    for (k in keys) {
      for (sk in names(sections)) {
        if (grepl(k, sk, ignore.case = TRUE)) return(sections[[sk]])
      }
    }
    NA_character_
  }

  responsibilities <- extract_sec("responsibilit")
  education        <- extract_sec("education")
  experience       <- extract_sec("experience")
  preferred        <- extract_sec("preferred")

  # "Required Qualifications" block: first education-matching bullet → min_education,
  # remaining bullets → min_experience (if not already populated from a dedicated section).
  req_block <- extract_sec("required_qual", "required qual")
  if (!is.na(req_block)) {
    req_lines <- str_squish(str_split(req_block, "\n")[[1]])
    req_lines <- req_lines[nzchar(req_lines)]
    # Strip leading bullet characters (•, -, *) so pattern matching works regardless
    # of whether the job used <li> tags or <p>&bull; inline bullets
    req_lines <- str_squish(str_replace(req_lines, "^[•\\-*]\\s*", ""))
    req_lines <- req_lines[nzchar(req_lines)]

    # Strategy 1: bullet explicitly labelled "Education: ..." — strip the prefix
    edu_prefix_pat <- "(?i)^Education:\\s+"
    edu_prefix_idx <- which(grepl(edu_prefix_pat, req_lines, perl = TRUE))

    if (length(edu_prefix_idx) > 0) {
      first_edu <- edu_prefix_idx[1]
      if (is.na(education))
        education <- str_squish(sub(edu_prefix_pat, "", req_lines[first_edu], perl = TRUE))
    } else {
      # Strategy 2: line opens with a degree keyword (High School, BS, B.S., etc.)
      edu_kw_pat <- paste0(
        "(?i)^(High\\s+School|GED|B\\.S\\.|B\\.A\\.|M\\.S\\.|",
        "BS(?!\\w)|MS(?!\\w)|Ph\\.?D|Bachelor|Master|Associate)"
      )
      edu_kw_idx <- which(grepl(edu_kw_pat, req_lines, perl = TRUE))
      first_edu  <- if (length(edu_kw_idx) > 0) edu_kw_idx[1] else NA_integer_
      if (is.na(education) && !is.na(first_edu))
        education <- req_lines[first_edu]
    }

    if (is.na(experience)) {
      keep <- if (!is.na(first_edu)) setdiff(seq_along(req_lines), first_edu) else seq_along(req_lines)
      exp_lines <- req_lines[keep]
      if (length(exp_lines) > 0)
        experience <- paste(exp_lines, collapse = "\n")
    }
  }

  list(
    responsibilities         = responsibilities,
    min_education            = education,
    min_experience           = experience,
    preferred_qualifications = preferred
  )
}

#' Extract salary range from footer HTML
#'
#' Matches "$X,XXX - $X,XXX" or "$X,XXX to $X,XXX" patterns
parse_dayforce_salary <- function(footer_html) {
  if (is.null(footer_html) || !nzchar(footer_html)) return(NA_character_)

  txt <- html_to_text(footer_html)
  if (is.na(txt)) return(NA_character_)

  # "$91,600 - $137,400" or "91600 to 137400"
  m <- str_match(txt,
    "\\$([0-9,]+(?:\\.[0-9]{2})?)\\s*[-–to]+\\s*\\$([0-9,]+(?:\\.[0-9]{2})?)")
  if (!is.na(m[1, 1])) {
    return(paste0("$", m[1, 2], "-$", m[1, 3]))
  }

  # Hourly pattern "$22.50 - $35.00 per hour"
  m2 <- str_match(txt,
    "(?i)\\$([0-9,]+(?:\\.[0-9]{2})?)\\s*[-–]\\s*\\$([0-9,]+(?:\\.[0-9]{2})?)\\s*(?:per\\s+hour|/hr|hourly)")
  if (!is.na(m2[1, 1])) {
    return(paste0("$", m2[1, 2], "-$", m2[1, 3], "/hr"))
  }

  NA_character_
}

#' Convert a single raw jobData list to a one-row data frame
parse_dayforce_job <- function(jd) {
  ns_check <- tryCatch({
    locs <- jd$postingLocations
    if (length(locs) == 0) return(NULL)
    # Filter to US locations only
    us_locs <- Filter(function(l) {
      !is.null(l$isoCountryCode) && l$isoCountryCode == "US"
    }, locs)
    if (length(us_locs) == 0) return(NULL)
    loc <- us_locs[[1]]
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(ns_check)) return(NULL)

  loc <- Filter(function(l) !is.null(l$isoCountryCode) && l$isoCountryCode == "US",
                jd$postingLocations)[[1]]

  location_str <- paste0(
    loc$cityName %||% "",
    if (!is.null(loc$cityName) && !is.null(loc$stateCode)) ", " else "",
    loc$stateCode %||% ""
  )

  jpc   <- jd$jobPostingContent
  secs  <- parse_dayforce_sections(jpc$jobDescription %||% "")
  salary <- parse_dayforce_salary(jpc$jobDescriptionFooter %||% "")

  # jobPostingAttributes is a list of {name, value} pairs
  attrs <- jd$jobPostingAttributes %||% list()
  get_attr <- function(name) {
    val <- Filter(function(a) !is.null(a$name) && a$name == name, attrs)
    if (length(val) > 0) val[[1]]$value %||% NA_character_ else NA_character_
  }

  posting_date <- tryCatch(
    as.Date(substr(jd$postingStartTimestampUTC %||% "", 1, 10)),
    error = function(e) NA
  )

  data.frame(
    job_title                = jd$jobTitle %||% NA_character_,
    job_url                  = jd$job_url  %||% NA_character_,
    location                 = location_str,
    job_responsibilities     = secs$responsibilities,
    min_education            = secs$min_education,
    min_experience           = secs$min_experience,
    preferred_qualifications = secs$preferred_qualifications,
    salary_range             = salary,
    job_identification       = as.character(jd$jobReqId %||% jd$jobPostingId %||% NA_character_),
    job_category             = get_attr("JobFamily"),
    degree_level             = NA_character_,
    ecl_gtc_required         = NA_character_,
    essential_skills         = NA_character_,
    posting_date             = posting_date,
    stringsAsFactors         = FALSE
  )
}

#' Scrape all US jobs from a Dayforce career portal
#'
#' @param company_name  Company name (for logging)
#' @param careers_url   Full portal URL (e.g., https://jobs.dayforcehcm.com/en-US/skywater/CANDIDATEPORTAL/)
#' @param fetch_details Ignored (details always fetched); kept for API compatibility
#' @return data.frame with one row per US job
scrape_dayforce_company <- function(company_name, careers_url, fetch_details = TRUE) {

  log_message(paste("Starting Dayforce scrape for:", company_name))

  namespace <- extract_dayforce_namespace(careers_url)
  if (is.null(namespace)) {
    log_message("Could not extract Dayforce namespace", level = "ERROR")
    return(data.frame())
  }
  log_message(paste("Namespace:", namespace))

  consecutive_miss <- 0L
  current_id       <- DAYFORCE_PROBE_MIN
  jobs_list        <- list()
  found_any        <- FALSE
  max_found_id     <- DAYFORCE_PROBE_MIN

  log_message(paste0("Probing job IDs ", DAYFORCE_PROBE_MIN, "-", DAYFORCE_PROBE_CAP,
                     " (step=", DAYFORCE_PROBE_STEP, ", max_miss=", DAYFORCE_MAX_MISS, ")"))

  while (current_id <= DAYFORCE_PROBE_CAP) {
    jd <- fetch_dayforce_job(namespace, current_id)

    if (!is.null(jd)) {
      found_any        <- TRUE
      consecutive_miss <- 0L
      max_found_id     <- max(max_found_id, current_id)

      row <- tryCatch(
        parse_dayforce_job(jd),
        error = function(e) {
          log_message(paste0("  [", current_id, "] parse error: ", conditionMessage(e)),
                      level = "WARN")
          NULL
        }
      )
      if (!is.null(row)) {
        jobs_list[[length(jobs_list) + 1]] <- row
        log_message(paste0("  [", current_id, "] ", row$job_title,
                           " | ", row$location))
      } else {
        log_message(paste0("  [", current_id, "] ", jd$jobTitle,
                           " (skipped - not US or not parseable)"), level = "DEBUG")
      }
    } else {
      consecutive_miss <- consecutive_miss + 1L
      # Once we've found at least one job, stop after too many consecutive misses
      if (found_any && consecutive_miss >= DAYFORCE_MAX_MISS) {
        log_message(paste0("Stopping probe: ", DAYFORCE_MAX_MISS,
                           " consecutive misses after last active ID ", max_found_id))
        break
      }
    }

    current_id <- current_id + DAYFORCE_PROBE_STEP
    Sys.sleep(.dayforce_delay())
  }

  if (length(jobs_list) == 0) {
    log_message(paste("No jobs found for", company_name), level = "WARN")
    return(data.frame())
  }

  jobs_df <- bind_rows(jobs_list)
  log_message(paste("Found", nrow(jobs_df), "US jobs for", company_name))
  jobs_df
}
