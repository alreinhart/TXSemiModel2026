# SAP SuccessFactors Platform Scraper
# ====================================
# Scrapes job listings from SAP SuccessFactors career sites via HTML parsing
# Used by: Qorvo, TSMC, Lam Research
# Parses HTML job listing tables and detail pages for structured data

library(httr)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)

source("config/scraper_config.R")
source("scrapers/utils.R")

#' Fetch a page of job listings from a SuccessFactors career site
#'
#' @param base_url Base careers URL (e.g., https://careers.qorvo.com)
#' @param offset Starting row for pagination (0-indexed, increments by 25)
#' @return Parsed HTML document or NULL on failure
fetch_successfactors_page <- function(base_url, offset = 0) {

  # Detect if base_url already contains a custom search path (e.g., Lam Research)
  # Lam uses path-based pagination: /go/Search/8797500/25/?q=&sortColumn=...
  # Qorvo/TSMC use: /search/?q=&sortColumn=...&startrow=N
  parsed <- httr::parse_url(base_url)
  if (!is.null(parsed$path) && parsed$path != "" && !grepl("^/?$", parsed$path)) {
    # Custom path URL — use path-based pagination (offset as path segment)
    clean_path <- sub("/$", "", base_url)
    if (offset > 0) {
      search_url <- paste0(clean_path, "/", offset, "/?q=&sortColumn=referencedate&sortDirection=desc")
    } else {
      search_url <- paste0(clean_path, "/?q=&sortColumn=referencedate&sortDirection=desc")
    }
  } else {
    # Default SuccessFactors search pattern (Qorvo, TSMC)
    search_url <- paste0(base_url, "/search/?q=&sortColumn=referencedate&sortDirection=desc&startrow=", offset)
  }

  log_message(paste("Fetching SuccessFactors page: startrow =", offset), level = "DEBUG")

  response <- fetch_with_retry(search_url)

  if (is.null(response)) return(NULL)

  page <- tryCatch(
    read_html(content(response, as = "text", encoding = "UTF-8")),
    error = function(e) {
      log_message(paste("Failed to parse HTML:", e$message), level = "ERROR")
      NULL
    }
  )

  return(page)
}

#' Parse job rows from a SuccessFactors listing page
#'
#' @param page Parsed HTML document from fetch_successfactors_page
#' @param base_url Base careers URL for constructing absolute job URLs
#' @return Tibble of jobs (job_title, location, job_url) or empty tibble
parse_successfactors_listing <- function(page, base_url) {

  if (is.null(page)) {
    return(tibble(job_title = character(), location = character(), job_url = character()))
  }

  rows <- page %>% html_elements("tr.data-row")

  # Fallback for sites without CSS classes (e.g., Lam Research):
  # find all <tr> rows inside the job listing table
  use_fallback <- length(rows) == 0
  if (use_fallback) {
    rows <- page %>% html_elements("table.jobs tr")
    # Also try generic table rows if table.jobs not found
    if (length(rows) == 0) {
      rows <- page %>% html_elements("table tr")
    }
  }

  if (length(rows) == 0) {
    return(tibble(job_title = character(), location = character(), job_url = character()))
  }

  # Extract host from base_url for building absolute URLs
  parsed_url <- httr::parse_url(base_url)
  url_host <- paste0(parsed_url$scheme, "://", parsed_url$hostname)

  jobs <- list()
  for (row in rows) {
    # Try CSS-classed elements first, then fallback
    title_link <- row %>% html_element("a.jobTitle-link")

    if (is.null(title_link) || length(title_link) == 0) {
      # Fallback: find first <a> with href containing "/job/"
      all_links <- row %>% html_elements("a")
      for (link in all_links) {
        link_href <- html_attr(link, "href")
        if (!is.na(link_href) && grepl("/job/", link_href)) {
          title_link <- link
          break
        }
      }
    }
    if (is.null(title_link) || length(title_link) == 0) next

    title <- title_link %>% html_text2() %>% str_squish()
    href <- title_link %>% html_attr("href")

    if (is.null(title) || is.na(title) || nchar(title) == 0 || is.null(href) || is.na(href)) next

    # Build absolute URL from relative href
    if (startsWith(href, "http")) {
      job_url <- href
    } else {
      # Use host root for absolute-path hrefs (e.g., /job/12345)
      job_url <- paste0(url_host, href)
    }

    # Try CSS-classed location first, then fallback to second <td>
    location_node <- row %>% html_element("span.jobLocation")
    if (!is.null(location_node) && length(location_node) > 0) {
      location <- str_squish(html_text2(location_node))
    } else {
      tds <- row %>% html_elements("td")
      location <- if (length(tds) >= 2) {
        str_squish(html_text2(tds[[2]]))
      } else {
        NA_character_
      }
    }

    jobs <- c(jobs, list(tibble(
      job_title = title,
      location = location,
      job_url = job_url
    )))
  }

  if (length(jobs) > 0) bind_rows(jobs) else tibble(job_title = character(), location = character(), job_url = character())
}

#' Scrape all job listings from a SuccessFactors career site with pagination
#'
#' @param company_name Name of the company
#' @param base_url Base careers URL
#' @param max_pages Maximum pages to scrape (safety limit)
#' @return Tibble of all job listings
scrape_successfactors <- function(company_name, base_url, max_pages = SCRAPER_CONFIG$max_pages_per_company) {

  log_message(paste("Starting SuccessFactors listing scrape for", company_name))

  all_jobs <- list()
  offset <- 0
  page_size <- 25
  page_num <- 1

  while (page_num <= max_pages) {
    log_message(paste("Scraping page", page_num, "for", company_name, "(startrow:", offset, ")"))

    page <- fetch_successfactors_page(base_url, offset)
    if (is.null(page)) {
      log_message("Failed to fetch page - stopping", level = "WARN")
      break
    }

    page_jobs <- parse_successfactors_listing(page, base_url)

    if (nrow(page_jobs) == 0) {
      log_message("No more job rows found - stopping")
      break
    }

    all_jobs <- c(all_jobs, list(page_jobs))
    log_message(paste("  Found", nrow(page_jobs), "jobs on this page"))

    # If fewer than page_size results, we're on the last page
    if (nrow(page_jobs) < page_size) {
      log_message("Last page (fewer results than page size)")
      break
    }

    offset <- offset + page_size
    page_num <- page_num + 1
    Sys.sleep(SCRAPER_CONFIG$delay_between_requests)
  }

  if (length(all_jobs) > 0) {
    jobs_df <- bind_rows(all_jobs)
    log_message(paste("Found", nrow(jobs_df), "total jobs for", company_name))
  } else {
    jobs_df <- tibble(job_title = character(), location = character(), job_url = character())
    log_message(paste("No jobs found for", company_name), level = "WARN")
  }

  return(jobs_df)
}

#' Strip SuccessFactors / Qorvo boilerplate from description HTML
#'
#' Removes company intro paragraphs, footer sections, EEO text,
#' and recruiter tags from job description HTML.
#'
#' @param html_string Raw HTML job description
#' @return HTML string with boilerplate removed
strip_successfactors_boilerplate <- function(html_string) {

  if (is.null(html_string) || nchar(html_string) == 0) return(html_string)

  result <- html_string

  # --- Header boilerplate ---
  # Qorvo intro: "Qorvo (Nasdaq: QRVO) supplies innovative semiconductor solutions..."
  # TSMC intro: "TSMC is the world's leading..." or "About TSMC"
  header_patterns <- c(
    "(?si)^\\s*<p[^>]*>\\s*Qorvo\\s*\\(Nasdaq:\\s*QRVO\\).*?</p>\\s*",
    '(?si)^\\s*Qorvo\\s*\\(Nasdaq:\\s*QRVO\\).*?(?=<h[1-6]|<p[^>]*>\\s*<(?:strong|b)>)',
    "(?si)^\\s*<p[^>]*>\\s*TSMC\\s+is\\s+the\\s+world.s\\s+leading.*?</p>\\s*",
    "(?si)^\\s*<p[^>]*>\\s*About\\s+TSMC.*?</p>\\s*",
    # Lam Research intro: "The Group You'll Be A Part Of" section
    "(?si)^\\s*<[^>]*>\\s*(?:<[^>]*>\\s*)*The\\s+Group\\s+You.ll\\s+Be\\s+A\\s+Part\\s+Of.*?(?=<[^>]*>\\s*(?:<[^>]*>\\s*)*(?:The\\s+Impact\\s+You|What\\s+You.ll\\s+Do))",
    # Lam Research standalone company intro paragraph
    "(?si)^\\s*<p[^>]*>\\s*(?:At\\s+)?Lam\\s+Research.*?</p>\\s*"
  )
  for (pat in header_patterns) {
    stripped <- tryCatch(sub(pat, "", result, perl = TRUE), error = function(e) NULL)
    if (!is.null(stripped) && nchar(stripped) < nchar(result)) {
      result <- stripped
      break
    }
  }

  # --- Footer boilerplate ---
  footer_patterns <- c(
    # "MAKE A DIFFERENCE AT QORVO" and everything after
    "(?si)<[^>]*>\\s*MAKE A DIFFERENCE AT QORVO.*$",
    "(?si)MAKE A DIFFERENCE AT QORVO.*$",
    # TSMC footer boilerplate
    "(?si)<[^>]*>\\s*TSMC\\s+is\\s+an\\s+equal\\s+opportunity\\s+employer.*$",
    "(?si)<[^>]*>\\s*Please\\s+be\\s+informed\\s+that\\s+TSMC.*$",
    # TSMC benefits paragraph: "As a valued member of the TSMC family..."
    "(?si)<p[^>]*>\\s*(?:<[^>]*>\\s*)*As a valued member of the TSMC family.*$",
    # TSMC "Candidates must be willing and able to work on-site" + everything after
    "(?si)<p[^>]*>\\s*(?:<[^>]*>\\s*)*Candidates must be willing and able to work on-site.*$",
    # TSMC Work Location / Shift / Travel metadata
    "(?si)<p[^>]*>\\s*(?:<[^>]*>\\s*)*(?:<b[^>]*>\\s*(?:<[^>]*>\\s*)*)?Work Location\\s*(?:</[^>]*>\\s*)*:.*$",
    # Lam Research EEO / footer
    "(?si)<[^>]*>\\s*(?:<[^>]*>\\s*)*Lam\\s+Research\\s+is\\s+an\\s+equal\\s+opportunity\\s+employer.*$",
    "(?si)<[^>]*>\\s*(?:<[^>]*>\\s*)*Our\\s+Perks\\s+and\\s+Benefits.*$"
  )
  for (pat in footer_patterns) {
    stripped <- tryCatch(sub(pat, "", result, perl = TRUE), error = function(e) NULL)
    if (!is.null(stripped) && nchar(stripped) < nchar(result)) {
      result <- stripped
      break
    }
  }

  # --- EEO statement ---
  eeo_patterns <- c(
    "(?si)We are an Equal Employment Opportunity.*$",
    "(?si)<p[^>]*>\\s*We are an Equal Employment Opportunity.*$"
  )
  for (pat in eeo_patterns) {
    result <- tryCatch(sub(pat, "", result, perl = TRUE), error = function(e) result)
  }

  # --- Visa sponsorship notice ---
  result <- tryCatch(
    sub("(?si)This position is not eligible for visa sponsorship[^<]*", "", result, perl = TRUE),
    error = function(e) result
  )

  # --- #LI-XXX recruiter tags and #cj tags ---
  result <- gsub("#LI-[A-Z0-9]+", "", result, perl = TRUE)
  result <- gsub("#cj\\b", "", result, perl = TRUE)

  str_trim(result)
}

#' Extract a section from SuccessFactors job description HTML
#'
#' Finds content between heading patterns (h2/strong) in the description.
#'
#' @param html_string Cleaned HTML description
#' @param heading_patterns Vector of regex patterns to match section headings
#' @return Character string of section content, or NA
extract_sf_section <- function(html_string, heading_patterns) {

  if (is.null(html_string) || is.na(html_string) || nchar(html_string) == 0) {
    return(NA_character_)
  }

  # Check plain text for heading existence
  plain_text <- str_replace_all(html_string, "<[^>]+>", " ")
  plain_text <- str_replace_all(plain_text, "&[a-zA-Z0-9#]+;", " ")
  plain_text <- str_replace_all(plain_text, "\\s+", " ")

  for (pattern in heading_patterns) {
    if (!grepl(pattern, plain_text, ignore.case = TRUE, perl = TRUE)) next

    # Split HTML at the heading — try multiple wrapping styles:
    # 1. h1-h6 (with optional bold inside)
    # 2. standalone strong/b tags (with optional nested span tags inside)
    # 3. flexible: any sequence of tags before/after the heading text
    # 4. p tags with bold inside
    # 5. p tags with span-only (plain text headings, no bold)
    split_patterns <- c(
      paste0("(?si)<h[1-6][^>]*>\\s*(?:<(?:strong|b)>\\s*)?", pattern, "\\s*(?:</(?:strong|b)>\\s*)?</h[1-6]>"),
      paste0("(?si)<(?:strong|b)[^>]*>\\s*(?:<[^>]*>\\s*)*", pattern, "\\s*(?:</[^>]*>\\s*)*</(?:strong|b)>"),
      paste0("(?si)(?:<[^>]*>\\s*)*", pattern, "\\s*(?:</[^>]*>\\s*)*(?:&nbsp;)?\\s*(?:</[^>]*>)*"),
      paste0("(?si)<p[^>]*>\\s*(?:<(?:strong|b)>\\s*)?", pattern, "\\s*(?:</(?:strong|b)>\\s*)?</p>"),
      paste0("(?si)<p[^>]*>\\s*(?:<span[^>]*>\\s*)*", pattern, "\\s*(?:</span>\\s*)*</p>")
    )

    for (sp in split_patterns) {
      parts <- tryCatch(strsplit(html_string, sp, perl = TRUE)[[1]], error = function(e) NULL)
      if (is.null(parts) || length(parts) < 2) next

      after_heading <- parts[2]

      # Truncate at next major heading: bold, h-tag, or plain-text paragraph heading
      next_heading_patterns <- c(
        "(?si)<h[1-6][^>]*>",
        "(?si)<(?:strong|b)>\\s*[A-Z]",
        "(?si)<p[^>]*>\\s*(?:<span[^>]*>\\s*)*[A-Z][A-Za-z &/-]{2,50}(?:\\s*</span>\\s*)*</p>\\s*<ul"
      )
      next_heading <- -1
      for (nhp in next_heading_patterns) {
        pos <- regexpr(nhp, after_heading, perl = TRUE)
        if (pos > 0 && (next_heading < 0 || pos < next_heading)) {
          next_heading <- pos
        }
      }
      if (next_heading > 0) {
        after_heading <- substr(after_heading, 1, next_heading - 1)
      }

      section_doc <- tryCatch(
        read_html(paste0("<div>", after_heading, "</div>")),
        error = function(e) NULL
      )
      if (is.null(section_doc)) next

      # Try bullet items first
      li_items <- section_doc %>% html_elements("li") %>% html_text2()
      li_items <- str_squish(li_items)
      li_items <- li_items[nchar(li_items) > 0]

      if (length(li_items) > 0) {
        return(paste(li_items, collapse = "\n"))
      }

      # Fallback to paragraph text
      paras <- section_doc %>% html_elements("p") %>% html_text2()
      paras <- str_squish(paras)
      paras <- paras[nchar(paras) > 3]

      if (length(paras) > 0) {
        return(paste(paras, collapse = "\n"))
      }
    }
  }

  return(NA_character_)
}

#' Scrape detailed job information from a SuccessFactors job detail page
#'
#' Fetches the individual job page and extracts posting date, responsibilities,
#' education, experience, preferred qualifications, etc.
#'
#' @param job_url Full URL to the job detail page
#' @return List with structured job detail fields
scrape_successfactors_job_details <- function(job_url) {

  log_message(paste("Fetching SuccessFactors job details:", job_url), level = "DEBUG")

  na_result <- list(
    posting_date = NA_Date_,
    job_responsibilities = NA_character_,
    min_education = NA_character_,
    min_experience = NA_character_,
    preferred_qualifications = NA_character_,
    salary_range = NA_character_,
    essential_skills = NA_character_,
    job_identification = NA_character_
  )

  response <- fetch_with_retry(job_url)
  if (is.null(response)) return(na_result)

  page <- tryCatch(
    read_html(content(response, as = "text", encoding = "UTF-8")),
    error = function(e) {
      log_message(paste("Failed to parse job detail HTML:", e$message), level = "WARN")
      NULL
    }
  )
  if (is.null(page)) return(na_result)

  # --- Extract posting date from schema.org meta ---
  posting_date <- NA_Date_
  date_meta <- page %>% html_element("meta[itemprop='datePosted']")
  if (!is.null(date_meta)) {
    date_str <- html_attr(date_meta, "content")
    if (!is.na(date_str) && nchar(date_str) > 0) {
      posting_date <- tryCatch(parse_date_string(date_str), error = function(e) NA_Date_)
    }
  }

  # --- Extract description HTML ---
  desc_node <- page %>% html_element("span.jobdescription")
  if (is.null(desc_node)) {
    return(c(na_result, list(posting_date = posting_date)))
  }

  raw_html <- as.character(desc_node %>% html_children()) %>% paste(collapse = "")
  if (nchar(raw_html) == 0) {
    raw_html <- as.character(desc_node)
  }

  # --- Extract salary before stripping boilerplate ---
  salary <- NA_character_
  salary_match <- str_match(raw_html, "\\$([0-9,]+(?:\\.\\d{2})?)\\s*(?:[-\u2013]|to|and)\\s*\\$?([0-9,]+(?:\\.\\d{2})?)")
  if (!is.na(salary_match[1, 1])) {
    salary <- salary_match[1, 1]
  }

  # --- Strip boilerplate ---
  desc_html <- strip_successfactors_boilerplate(raw_html)

  # --- Extract responsibilities ---
  responsibility_patterns <- c(
    "Responsibilities\\s+may\\s+include\\s*:?",
    "Key\\s+Roles?\\s+and\\s+[Rr]esponsibilities\\s*:?",
    "Key\\s+Responsibilities\\s*:?",
    "Role\\s+Responsibilities\\s*:?",
    "Primary\\s+Responsibilities\\s*:?",
    "Typical\\s+duties\\s+include[^:]*:?",
    "RESPONSIBILITIES\\s*:?",
    "Responsibilities\\s*:?",
    "What\\s+You.ll\\s+Do\\s*:?",
    "The\\s+Impact\\s+You.ll\\s+Make\\s*:?",
    "POSITION\\s+DESCRIPTION\\s*:?",
    "Job\\s+Description\\s*:?"
  )
  responsibilities <- extract_sf_section(desc_html, responsibility_patterns)

  # Fallback: if no structured responsibilities found, extract all text after
  # boilerplate stripping as the description (for jobs with no section headings)
  if (is.na(responsibilities) && !is.null(desc_html) && nchar(desc_html) > 100) {
    # Truncate at first qualification-like heading
    truncated <- desc_html
    qual_stop_patterns <- c(
      "(?si)<(?:strong|b)>\\s*(?:Qualifications|QUALIFICATIONS|Requirements|REQUIREMENTS|Required\\s+Education|Essential\\s+Qualifications|Technical\\s+Knowledge|Technical\\s+Skills|Who\\s+We.re\\s+Looking\\s+For)",
      "(?si)<p[^>]*>\\s*(?:<span[^>]*>\\s*)*(?:Qualifications|Technical\\s+Skills|Soft\\s+Skills)\\s*(?:</span>)*\\s*</p>",
      "(?si)<h[1-6][^>]*>\\s*(?:<[^>]*>\\s*)*Who\\s+We.re\\s+Looking\\s+For"
    )
    for (qp in qual_stop_patterns) {
      pos <- regexpr(qp, truncated, perl = TRUE)
      if (pos > 0) {
        truncated <- substr(truncated, 1, pos - 1)
        break
      }
    }
    doc <- tryCatch(read_html(paste0("<div>", truncated, "</div>")), error = function(e) NULL)
    if (!is.null(doc)) {
      all_text <- doc %>% html_text2()
      lines <- str_split(all_text, "\n")[[1]]
      lines <- str_squish(lines)
      lines <- lines[nchar(lines) > 0]
      all_text <- paste(lines, collapse = "\n")
      if (nchar(all_text) > 30) {
        responsibilities <- all_text
      }
    }
  }

  # --- Extract required qualifications ---
  min_education <- NA_character_
  min_experience <- NA_character_

  req_qual_patterns <- c(
    "Required\\s+Qualifications\\s*:?",
    "Minimum\\s+Qualifications\\s*:?",
    "Basic\\s+Qualifications\\s*:?",
    "Essential\\s+Qualifications\\s*:?",
    "Required\\s+Education\\s*(?:&amp;|&|and)\\s*Experience\\s*:?",
    "Technical\\s+Knowledge/Skills/Abilities\\s+Required\\s*:?",
    "Who\\s+We.re\\s+Looking\\s+For\\s*:?",
    "Requirements\\s*:?",
    "REQUIREMENTS\\s*:?",
    "QUALIFICATIONS\\s*:?",
    "Qualifications\\s*:?"
  )
  req_qual_text <- extract_sf_section(desc_html, req_qual_patterns)

  if (!is.na(req_qual_text)) {
    bullets <- str_split(req_qual_text, "\n")[[1]]
    bullets <- str_squish(bullets)
    bullets <- bullets[nchar(bullets) > 0]

    # Handle " - OR - " separator in first bullet (TSMC pattern):
    # "Bachelor's degree... - OR - a minimum of 5 years of experience..."
    if (length(bullets) > 0 && grepl("\\s+-\\s+OR\\s+-\\s+", bullets[1])) {
      or_parts <- strsplit(bullets[1], "\\s+-\\s+OR\\s+-\\s+")[[1]]
      if (length(or_parts) == 2) {
        min_education <- str_squish(or_parts[1])
        min_experience <- str_squish(or_parts[2])
        bullets <- bullets[-1]
        # Remaining bullets go to experience
        if (length(bullets) > 0) {
          min_experience <- paste(c(min_experience, bullets), collapse = "\n")
        }
      }
    }

    # Standard separation: use degree pattern to separate education from experience
    if (is.na(min_education) && is.na(min_experience)) {
      edu_pattern <- "(?i)(\\bbachelor|\\bmaster(?!ing)|\\bassociate(?!d)|ph\\.?d|\\bdegree\\b|\\bdiploma\\b|high\\s+school|\\bGED\\b|college|technical\\s+school)|(?-i:\\bB\\.?S\\.?\\b|\\bM\\.?S\\.?\\b|\\bB\\.?A\\.?\\b|\\bM\\.?A\\.?\\b)"
      edu_idx <- which(grepl(edu_pattern, bullets, perl = TRUE))

      if (length(edu_idx) > 0) {
        min_education <- paste(bullets[edu_idx], collapse = "\n")
        remaining <- bullets[-edu_idx]
        if (length(remaining) > 0) min_experience <- paste(remaining, collapse = "\n")
      } else if (length(bullets) > 0) {
        min_experience <- paste(bullets, collapse = "\n")
      }
    }
  }

  # --- Extract job identification (Req ID) ---
  job_identification <- NA_character_
  req_id_match <- str_match(raw_html, "Req\\s+ID\\s*:\\s*(\\d+)")
  if (!is.na(req_id_match[1, 1])) {
    job_identification <- req_id_match[1, 2]
  }

  # --- Extract preferred qualifications ---
  preferred_patterns <- c(
    "Preferred\\s+Qualifications\\s*:?",
    "Preferred\\s+Skills\\s*:?",
    "Nice\\s+to\\s+Have\\s*:?",
    "Desired\\s+(?:Qualifications|Experiences?)\\s*:?",
    "Technical\\s+Skills\\s*[-\u2013]\\s*Desired\\s*:?"
  )
  preferred_quals <- extract_sf_section(desc_html, preferred_patterns)

  Sys.sleep(SCRAPER_CONFIG$delay_between_requests)

  return(list(
    posting_date = posting_date,
    job_responsibilities = responsibilities,
    min_education = min_education,
    min_experience = min_experience,
    preferred_qualifications = preferred_quals,
    salary_range = salary,
    essential_skills = NA_character_,
    job_identification = job_identification
  ))
}

#' Main function to scrape a SuccessFactors company with full details
#'
#' @param company_name Name of the company
#' @param base_url Base URL of career site
#' @param fetch_details Whether to fetch full job details (slower)
#' @return Dataframe of jobs with all available details
scrape_successfactors_company <- function(company_name, base_url, fetch_details = TRUE) {

  log_message(paste("=== Starting SuccessFactors scrape for", company_name, "==="))
  start_time <- Sys.time()

  jobs <- scrape_successfactors(company_name, base_url)

  if (nrow(jobs) == 0) {
    log_message(paste("No jobs found for", company_name), level = "WARN")
    return(data.frame())
  }

  # --- US location filter for companies with worldwide postings (e.g., TSMC) ---
  if (company_name == "TSMC") {
    us_state_pattern <- "\\b(AL|AK|AZ|AR|CA|CO|CT|DE|FL|GA|HI|ID|IL|IN|IA|KS|KY|LA|ME|MD|MA|MI|MN|MS|MO|MT|NE|NV|NH|NJ|NM|NY|NC|ND|OH|OK|OR|PA|RI|SC|SD|TN|TX|UT|VT|VA|WA|WV|WI|WY)\\b|United States"
    before_count <- nrow(jobs)
    jobs <- jobs %>% filter(grepl(us_state_pattern, location, perl = TRUE) | is.na(location))
    filtered_count <- before_count - nrow(jobs)
    log_message(paste("US location filter: kept", nrow(jobs), "of", before_count,
                      "jobs (removed", filtered_count, "non-US)"))
    if (nrow(jobs) == 0) {
      log_message(paste("No US jobs found for", company_name), level = "WARN")
      return(data.frame())
    }
  }

  if (fetch_details) {
    log_message(paste("Fetching details for", nrow(jobs), "jobs"))

    pb <- txtProgressBar(min = 0, max = nrow(jobs), style = 3)

    detail_results <- vector("list", nrow(jobs))
    for (i in seq_len(nrow(jobs))) {
      detail_results[[i]] <- tryCatch(
        scrape_successfactors_job_details(jobs$job_url[i]),
        error = function(e) {
          log_message(paste("Error fetching details for", jobs$job_url[i], ":", e$message), level = "WARN")
          list(
            posting_date = NA_Date_,
            job_responsibilities = NA_character_,
            min_education = NA_character_,
            min_experience = NA_character_,
            preferred_qualifications = NA_character_,
            salary_range = NA_character_,
            essential_skills = NA_character_,
            job_identification = NA_character_
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
        posting_date = NA_Date_,
        job_responsibilities = NA_character_,
        min_education = NA_character_,
        min_experience = NA_character_,
        preferred_qualifications = NA_character_,
        salary_range = NA_character_,
        essential_skills = NA_character_,
        job_identification = NA_character_
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
  log_message(paste("Total jobs:", nrow(jobs)))

  return(jobs)
}
