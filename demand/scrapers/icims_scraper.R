# iCIMS / Jibe Platform Scraper
# ==============================
# Scrapes job listings from iCIMS career sites via sitemap + JSON-LD
# Used by: AMD
# Strategy: fetch sitemap.xml for all job URLs, then extract JSON-LD from each page
# Optimized: parallel batch fetching with curl multi-handles (~10x faster)

library(httr)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(jsonlite)
library(xml2)
library(curl)

source("config/scraper_config.R")
source("scrapers/utils.R")

#' Fetch all job URLs from the site's sitemap
#'
#' @param base_url Base careers URL (e.g., https://careers.amd.com)
#' @return Character vector of job page URLs
fetch_icims_sitemap <- function(base_url) {

  clean_base <- sub("/$", "", base_url)

  # Try sitemap index first
  sitemap_index_url <- paste0(clean_base, "/sitemap.xml")
  log_message(paste("Fetching sitemap index:", sitemap_index_url), level = "DEBUG")

  response <- fetch_with_retry(sitemap_index_url)
  if (is.null(response)) {
    log_message("Failed to fetch sitemap index", level = "ERROR")
    return(character())
  }

  xml_text <- content(response, as = "text", encoding = "UTF-8")
  doc <- tryCatch(read_xml(xml_text), error = function(e) {
    log_message(paste("Failed to parse sitemap XML:", e$message), level = "ERROR")
    NULL
  })
  if (is.null(doc)) return(character())

  # Remove namespaces for easier XPath
  xml_ns_strip(doc)

  # Check if this is a sitemap index (contains <sitemap> elements)
  sitemap_refs <- xml_find_all(doc, "//sitemap/loc")

  all_urls <- character()

  if (length(sitemap_refs) > 0) {
    sitemap_urls <- xml_text(sitemap_refs)
    log_message(paste("Found", length(sitemap_urls), "sub-sitemaps"))

    for (sm_url in sitemap_urls) {
      log_message(paste("Fetching sub-sitemap:", sm_url), level = "DEBUG")
      sm_response <- fetch_with_retry(sm_url)
      if (is.null(sm_response)) next

      sm_xml <- content(sm_response, as = "text", encoding = "UTF-8")
      sm_doc <- tryCatch(read_xml(sm_xml), error = function(e) NULL)
      if (is.null(sm_doc)) next

      xml_ns_strip(sm_doc)
      locs <- xml_find_all(sm_doc, "//url/loc")
      urls <- xml_text(locs)
      all_urls <- c(all_urls, urls)

      Sys.sleep(1)
    }
  } else {
    locs <- xml_find_all(doc, "//url/loc")
    all_urls <- xml_text(locs)
  }

  # Filter to only job detail pages (pattern: /jobs/NNNNN)
  job_urls <- all_urls[grepl("/jobs/[0-9]+", all_urls)]
  log_message(paste("Found", length(job_urls), "job URLs in sitemap"))

  return(job_urls)
}

#' Fetch a single URL quickly using curl (faster than httr::GET)
#'
#' @param url URL to fetch
#' @return HTML string or NULL on failure
fetch_url_fast <- function(url) {
  h <- new_handle()
  handle_setheaders(h,
    "User-Agent" = SCRAPER_CONFIG$user_agent,
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language" = "en-US,en;q=0.5",
    "Connection" = "keep-alive"
  )
  handle_setopt(h, timeout = 30, followlocation = TRUE)

  resp <- tryCatch(curl_fetch_memory(url, handle = h), error = function(e) NULL)
  if (is.null(resp) || resp$status_code != 200) return(NULL)
  rawToChar(resp$content)
}

#' Strip AMD boilerplate from description HTML
#'
#' @param html_string Raw HTML description
#' @return Cleaned HTML string
strip_icims_boilerplate <- function(html_string) {

  if (is.null(html_string) || nchar(html_string) == 0) return(html_string)

  result <- html_string

  # --- Header boilerplate ---
  header_patterns <- c(
    "(?si)^\\s*<[^>]*>\\s*<[^>]*>\\s*WHAT YOU DO AT AMD CHANGES EVERYTHING.*?</[^>]*>\\s*</[^>]*>\\s*",
    "(?si)<[^>]*>\\s*<[^>]*>\\s*WHAT YOU DO AT AMD CHANGES EVERYTHING.*?</[^>]*>\\s*</[^>]*>\\s*",
    "(?si)<[^>]*>\\s*<[^>]*>\\s*Together,? we advance your career.*?</[^>]*>\\s*</[^>]*>\\s*"
  )
  for (pat in header_patterns) {
    result <- tryCatch(gsub(pat, "", result, perl = TRUE), error = function(e) result)
  }

  # --- Footer boilerplate ---
  footer_patterns <- c(
    "(?si)<[^>]*>\\s*(?:<[^>]*>\\s*)*(?:At AMD,? )?(?:we )?(?:it is our policy to provide|AMD is committed to).*$",
    "(?si)<[^>]*>\\s*(?:<[^>]*>\\s*)*Benefits offered.*$",
    "(?si)<[^>]*>\\s*(?:<[^>]*>\\s*)*AMD does not accept unsolicited resumes.*$",
    "(?si)<[^>]*>\\s*(?:<[^>]*>\\s*)*This role is not eligible for visa sponsorship\\..*$",
    # Keysight footer: benefits list, EEO, privacy statement
    "(?si)<[^>]*>\\s*(?:<[^>]*>\\s*)*(?:This role is eligible for Keysight Results Bonus|US Employees may be eligible for the following benefits).*$",
    "(?si)<[^>]*>\\s*(?:<[^>]*>\\s*)*\\*?\\*?\\*?Keysight is an Equal Opportunity Employer.*$",
    "(?si)<[^>]*>\\s*(?:<[^>]*>\\s*)*Careers Privacy Statement.*$"
  )
  for (pat in footer_patterns) {
    stripped <- tryCatch(sub(pat, "", result, perl = TRUE), error = function(e) NULL)
    if (!is.null(stripped) && nchar(stripped) < nchar(result)) {
      result <- stripped
      # Do NOT break — multiple footer phrases can appear in different orders
      # (e.g. "This role is not eligible" before "AMD is committed" in 80717),
      # so we apply all patterns to ensure full cleanup.
    }
  }

  # --- #LI-XXX recruiter tags ---
  result <- gsub("#LI-[A-Z0-9]+", "", result, perl = TRUE)

  str_trim(result)
}

#' Extract a section from iCIMS job description HTML by heading text
#'
#' Handles five AMD heading formats:
#'   - <h2> tags (newer format, e.g. 84692)
#'   - <h3> tags (e.g. 81108 KEY RESPONSIBILITIES / PREFERRED EXPERIENCE)
#'   - <strong>/<b> tags (original format, e.g. 79957; also inline <p><strong> in 81108)
#'   - Plain <p> paragraph headings (e.g. 80965)
#'   - DOM fallback for fragmented spans (75063) or div/span headings (80717)
#'
#' @param html_string Cleaned HTML description
#' @param heading_patterns Vector of regex patterns to match section headings
#' @return Character string of section content, or NA
extract_icims_section <- function(html_string, heading_patterns) {

  if (is.null(html_string) || is.na(html_string) || nchar(html_string) == 0) {
    return(NA_character_)
  }

  # Check plain text for heading existence.
  # Use BOTH tag-spaced and tag-collapsed variants: spaced handles normal headings,
  # collapsed handles Word-copy-paste fragmented headings like <span>A</span><span>CADEMIC...</span>
  plain_text_spaced <- str_squish(str_replace_all(
    str_replace_all(str_replace_all(html_string, "<[^>]+>", " "), "&[a-zA-Z0-9#]+;", " "),
    "\\s+", " "
  ))
  plain_text_nospace <- str_squish(str_replace_all(
    str_replace_all(str_replace_all(html_string, "<[^>]+>", ""), "&[a-zA-Z0-9#]+;", " "),
    "\\s+", " "
  ))
  plain_text <- plain_text_spaced  # used by regex split entries below

  # tag_ws: any mix of whitespace, &nbsp;, and HTML tags (handles <br/>, nested spans, etc.)
  tag_ws <- "(?:(?:&nbsp;)|\\s|<[^>]+>)*"

  # Known AMD top-level headings used as section stop boundaries
  known_headings_re <- paste0(
    "(?:THE ROLE|THE PERSON|KEY RESPONSIBILITIES|PREFERRED EXPERIENCE|",
    "ACADEMIC CREDENTIALS|PREFERRED QUALIFICATIONS|REQUIRED QUALIFICATIONS|",
    "JOB DESCRIPTION|JOB QUALIFICATIONS|EDUCATION REQUIREMENTS|LOCATION|",
    "JOB SUMMARY|WHAT WE NEED TO SEE|WAYS TO STAND OUT FROM THE CROWD|",
    "WHAT YOU.LL BE DOING|WHAT YOU.LL DO)"
  )
  plain_p_stop <- paste0(
    "(?si)<p[^>]*>", tag_ws, "(?:<[^>]+>", tag_ws, ")*",
    known_headings_re, "\\s*:?", tag_ws, "</p>"
  )

  for (pattern in heading_patterns) {
    # Accept the heading if it appears in either the spaced or the no-space plain text.
    # The no-space version catches Word-fragmented headings like "A" + "CADEMIC CREDENTIALS".
    found_spaced   <- grepl(pattern, plain_text_spaced,  ignore.case = TRUE, perl = TRUE)
    found_nospace  <- grepl(pattern, plain_text_nospace, ignore.case = TRUE, perl = TRUE)
    if (!found_spaced && !found_nospace) next

    # Split patterns ordered: h2 first, then h3, then strong/b, then plain <p> fallback.
    # Each entry is list(sp, fmt) where fmt controls which next-heading stop patterns are used.
    split_entries <- list(
      list(
        sp = paste0("(?si)<h2[^>]*>", tag_ws, "(?:<[^>]+>", tag_ws, ")*",
                    pattern, "\\s*:?", tag_ws, "</h2>"),
        fmt = "h2"
      ),
      list(
        sp = paste0("(?si)<h3[^>]*>", tag_ws, "(?:<[^>]+>", tag_ws, ")*",
                    pattern, "\\s*:?", tag_ws, "</h3>"),
        fmt = "h3"
      ),
      list(
        sp = paste0("(?si)<strong[^>]*>", tag_ws, "(?:<[^>]+>", tag_ws, ")*",
                    pattern, "\\s*:?", tag_ws, "</strong>"),
        fmt = "strong"
      ),
      list(
        sp = paste0("(?si)<b(?:\\s[^>]*)?>", tag_ws, "(?:<[^>]+>", tag_ws, ")*",
                    pattern, "\\s*:?", tag_ws, "</b>"),
        fmt = "strong"
      ),
      list(
        sp = paste0("(?si)<p[^>]*>", tag_ws, "(?:<[^>]+>", tag_ws, ")*",
                    pattern, "\\s*:?", tag_ws, "</p>"),
        fmt = "plain_p"
      )
    )

    for (entry in split_entries) {
      parts <- tryCatch(strsplit(html_string, entry$sp, perl = TRUE)[[1]],
                        error = function(e) NULL)
      if (is.null(parts) || length(parts) < 2) next

      after_heading <- parts[2]

      # Choose stop patterns based on the heading format that matched.
      # h2 format: only stop at next <h2> (inline <strong> inside section should not stop).
      # h3 format: stop at next <h3> or <h2>.
      # strong/b format: stop at next <strong>/<b> heading or known plain-<p> heading.
      # plain_p format: stop at next known plain-<p> heading or <h2>.
      if (entry$fmt == "h2") {
        nhps <- c("(?si)<h2[^>]*>")
      } else if (entry$fmt == "h3") {
        nhps <- c("(?si)<h3[^>]*>", "(?si)<h2[^>]*>")
      } else if (entry$fmt == "strong") {
        # Use tag_ws so <strong><u><span>HEADING handles <u> and other inline wrappers
        nhps <- c(
          paste0("(?si)<strong[^>]*>", tag_ws, "[A-Z]"),
          paste0("(?si)<b(?:\\s[^>]*)?>", tag_ws, "[A-Z]"),
          plain_p_stop
        )
      } else {  # plain_p
        nhps <- c(plain_p_stop, "(?si)<h2[^>]*>")
      }

      next_heading <- -1
      for (nhp in nhps) {
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

      li_items <- section_doc %>% html_elements("li") %>% html_text2()
      li_items <- str_squish(li_items)
      li_items <- li_items[nchar(li_items) > 0]

      # Expand each <p> on its own newlines before squishing so that <br>-separated
      # bullet lists within a single <p> tag are preserved as separate lines.
      para_nodes <- section_doc %>% html_elements("p")
      paras <- character(0)
      for (pn in para_nodes) {
        p_lines <- str_split(html_text2(pn), "\n")[[1]]
        p_lines <- str_squish(p_lines)
        p_lines <- p_lines[nchar(p_lines) > 3]
        paras <- c(paras, p_lines)
      }

      # Filter AMD/Keysight boilerplate lines that can bleed into the last section
      amd_boilerplate <- paste0(
        "(?i)(This role is not|not eligible for visa sponsorship|AMD is committed|AMD does not accept|",
        "it is our policy to provide|Benefits offered|",
        "Keysight is an Equal Opportunity|Careers Privacy Statement|",
        "US Employees may be eligible|This role is eligible for Keysight Results Bonus|",
        "Pay Range.*(?:MIN|MAX|USD|\\$)|USD\\s+\\$[0-9]|",
        "requires access to technology.*export control|U\\.S\\. citizen.*export control|",
        "export control.*U\\.S\\. Government|",
        "level of role will be based on applicable experience|Salary Range listed below|",
        "Visa Sponsorship is not available|sponsorship for employment visa)"
      )
      li_items <- li_items[!grepl(amd_boilerplate, li_items, perl = TRUE)]
      paras    <- paras[!grepl(amd_boilerplate, paras, perl = TRUE)]

      if (length(li_items) > 0 && length(paras) > 0) {
        # Both present — use html_text2 to get everything in document order
        lines <- str_split(section_doc %>% html_text2(), "\n")[[1]]
        lines <- str_squish(lines)
        lines <- lines[nchar(lines) > 3]
        lines <- lines[!grepl(amd_boilerplate, lines, perl = TRUE)]
        if (length(lines) > 0) return(paste(lines, collapse = "\n"))
      }

      if (length(li_items) > 0) return(paste(li_items, collapse = "\n"))
      if (length(paras) > 0) return(paste(paras, collapse = "\n"))

      # Boilerplate-filtered all_text fallback
      all_text <- section_doc %>% html_text2()
      bp_pos_at <- regexpr(amd_boilerplate, all_text, perl = TRUE)
      if (bp_pos_at > 0) all_text <- substr(all_text, 1, bp_pos_at - 1)
      all_text <- str_squish(all_text)
      if (nchar(all_text) > 10) return(all_text)
    }
  }

  # --- DOM-based fallback ---
  # Handles two cases that regex splits cannot:
  #   1. Fragmented headings: Word copy-paste splits a word across <span> tags,
  #      e.g. <span>A</span><span>CADEMIC CREDENTIALS:</span> (75063) or
  #           <span>P</span><span>REFERRED EXPERIENCE:</span>
  #      html_text2() concatenates sibling inline text without spaces, so the
  #      rendered plain text correctly reads "ACADEMIC CREDENTIALS".
  #   2. div/span heading format: <div><p><span data-contrast="auto">HEADING:</span></p></div>
  #      with content in the next sibling <div> (80717) — plain_p regex times out on
  #      the long HTML, but html_text2 on the full document works fine.
  #
  # This fallback runs unconditionally (no plain-text gate) so it catches both cases
  # even when the per-pattern plain-text checks fail (fragmented words fool them).
  # It uses LINE-ANCHORED splits so "requirements" in a sentence is never mistaken for
  # a "REQUIREMENTS" section heading.
  amd_boilerplate_dom <- paste0(
    "(?i)(This role is not|not eligible for visa sponsorship|AMD is committed|AMD does not accept|",
    "it is our policy to provide|Benefits offered|",
    "Keysight is an Equal Opportunity|Careers Privacy Statement|",
    "US Employees may be eligible|This role is eligible for Keysight Results Bonus|",
    "Pay Range.*(?:MIN|MAX|USD|\\$)|USD\\s+\\$[0-9]|",
    "requires access to technology.*export control|U\\.S\\. citizen.*export control|",
    "export control.*U\\.S\\. Government|",
    "level of role will be based on applicable experience|Salary Range listed below)"
  )
  dom_result <- tryCatch({
    page_doc <- read_html(paste0("<body>", html_string, "</body>"))
    # Normalize non-breaking space variants so \s matches them in line anchors:
    #   U+00A0 (no-break space) and U+202F (narrow no-break space, used by Word/CKEditor)
    full_text <- gsub("[\u00a0\u202f]", " ", html_text2(page_doc))

    for (pattern in heading_patterns) {
      # Only split when the heading appears as the ENTIRE content of a line.
      # This prevents matching heading text that occurs mid-sentence in bullets.
      sp_line <- paste0("(?im)^\\s*", pattern, "\\s*:?\\s*$\\n?")
      parts <- tryCatch(strsplit(full_text, sp_line, perl = TRUE)[[1]], error = function(e) NULL)
      if (is.null(parts) || length(parts) < 2) next

      after_text <- parts[2]

      # Stop at next known AMD heading on its own line
      nhp <- paste0("(?im)^\\s*(?:", known_headings_re, ")\\s*:?\\s*$")
      stop_pos <- regexpr(nhp, after_text, perl = TRUE)
      if (stop_pos > 0) after_text <- substr(after_text, 1, stop_pos - 1)

      # Remove boilerplate
      bp_pos <- regexpr(amd_boilerplate_dom, after_text, perl = TRUE)
      if (bp_pos > 0) after_text <- substr(after_text, 1, bp_pos - 1)

      lines <- str_split(after_text, "\n")[[1]]
      lines <- str_squish(lines)
      lines <- lines[nchar(lines) > 3]
      lines <- lines[!grepl(amd_boilerplate_dom, lines, perl = TRUE)]

      if (length(lines) > 0) return(paste(lines, collapse = "\n"))
    }
    NA_character_
  }, error = function(e) NA_character_)

  return(dom_result)
}

#' Parse job details from already-fetched HTML string
#'
#' Extracts data from window.jobDescriptionConfig JS object embedded in the page,
#' which contains richer data than JSON-LD (category, salary via tags5/tags6).
#'
#' @param page_html Raw HTML string of the job detail page
#' @param job_url The URL (for reference)
#' @return List with structured job detail fields
parse_icims_job_html <- function(page_html, job_url) {

  na_result <- list(
    job_title = NA_character_,
    location = NA_character_,
    country = NA_character_,
    job_category = NA_character_,
    posting_date = NA_Date_,
    job_responsibilities = NA_character_,
    min_education = NA_character_,
    min_experience = NA_character_,
    preferred_qualifications = NA_character_,
    salary_range = NA_character_,
    essential_skills = NA_character_,
    job_url = job_url
  )

  # --- Extract window.jobDescriptionConfig JSON ---
  # This JS object has richer data than JSON-LD: category, salary (tags5/tags6), etc.
  config_match <- regexpr(
    "window\\.jobDescriptionConfig\\s*=\\s*\\{",
    page_html, perl = TRUE
  )
  if (config_match < 0) {
    # Fallback: try JSON-LD if jobDescriptionConfig not found
    return(parse_icims_job_jsonld(page_html, job_url))
  }

  # Extract the JSON object — find the end using the known terminator pattern
  # The config ends with "};\n" or "}; " before the next <script> or statement
  json_start <- config_match + attr(config_match, "match.length") - 1
  raw_after <- substr(page_html, json_start, min(nchar(page_html), json_start + 500000))

  # The jobDescriptionConfig JSON ends with "};" — find it by matching
  # We know the JSON starts with { and need the matching }, followed by ;
  # Use a fast C-level approach: split on "};" and try parsing progressively
  # Most reliable: use the pattern that the config is followed by </script> or ;\n
  end_match <- regexpr("\\};\\s*</script>", raw_after, perl = TRUE)
  if (end_match < 0) {
    # Fallback: try finding }; followed by newline or var/window
    end_match <- regexpr("\\};\\s*(?:\n|var |window\\.|$)", raw_after, perl = TRUE)
  }
  if (end_match < 0) return(na_result)

  json_str <- substr(raw_after, 1, end_match)
  config <- tryCatch(fromJSON(json_str, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(config) || is.null(config$job)) return(na_result)

  job <- config$job

  # --- Basic fields ---
  job_title <- job$title %||% NA_character_
  location <- job$full_location %||% job$short_location %||% NA_character_
  country <- job$country %||% NA_character_

  # Category (e.g., " Engineering" — trim whitespace)
  job_category <- NA_character_
  if (!is.null(job$category) && length(job$category) > 0) {
    job_category <- str_trim(paste(unlist(job$category), collapse = ", "))
  }

  # Posting date
  posting_date <- NA_Date_
  if (!is.null(job$date_posted)) {
    posting_date <- tryCatch(as.Date(job$date_posted), error = function(e) NA_Date_)
  }
  # Fallback to JSON-LD datePosted
  if (is.na(posting_date)) {
    date_match <- regmatches(page_html, regexpr('"datePosted":"[^"]*"', page_html))
    if (length(date_match) > 0) {
      date_str <- sub('"datePosted":"([^"]*)"', "\\1", date_match)
      posting_date <- tryCatch(as.Date(date_str), error = function(e) NA_Date_)
    }
  }

  # --- Salary from tags5 (Hiring Target Min) and tags6 (Hiring Target Max) ---
  salary <- NA_character_
  tags5 <- job$tags5  # e.g., ["USD $126,640.00/Yr."]
  tags6 <- job$tags6  # e.g., ["USD $189,960.00/Yr."]

  if (!is.null(tags5) && length(tags5) > 0 && !is.null(tags6) && length(tags6) > 0) {
    min_salary_str <- tags5[[1]]
    max_salary_str <- tags6[[1]]
    # Extract dollar amounts
    min_match <- regmatches(min_salary_str, regexpr("\\$[0-9,]+", min_salary_str, perl = TRUE))
    max_match <- regmatches(max_salary_str, regexpr("\\$[0-9,]+", max_salary_str, perl = TRUE))
    if (length(min_match) > 0 && length(max_match) > 0) {
      salary <- paste0(min_match, "-", max_match)
    }
  }

  # Fallback: Keysight embeds salary as text in the description
  # e.g. "Colorado pay range: MIN $89,600- MAX $149,330"
  #      "California Pay Range MIN $114,620.00 MIDPOINT $152,820.00 MAX $191,030.00"
  #      "Pay Range: USD $55,680.00 - USD $92,800.00 Year" (Musts/Qualities format)
  if (is.na(salary)) {
    sal_plain <- str_replace_all(job$description %||% "", "<[^>]+>", " ")
    # Pattern 1: MIN $X ... MAX $Z (skip MIDPOINT's $ by using non-greedy match to MAX)
    ks_sal_m <- str_match(sal_plain,
      "(?i)(?:pay range|salary range)[^\\n]*?MIN\\s*\\$([0-9,]+(?:\\.\\d{2})?)(?:[^\\n]*?MAX\\s*\\$([0-9,]+(?:\\.\\d{2})?))?")
    if (!is.na(ks_sal_m[1, 1]) && !is.na(ks_sal_m[1, 2])) {
      if (!is.na(ks_sal_m[1, 3])) {
        salary <- paste0("$", ks_sal_m[1, 2], "-$", ks_sal_m[1, 3])
      } else {
        salary <- paste0("$", ks_sal_m[1, 2], "+")
      }
    }
    # Pattern 2: "Pay Range: USD $X - USD $Y Year"
    if (is.na(salary)) {
      usd_sal_m <- str_match(sal_plain,
        "(?i)pay\\s+range[^\\n]*?USD\\s+\\$([0-9,]+(?:\\.\\d{2})?)\\s*[-\u2013]\\s*USD\\s+\\$([0-9,]+(?:\\.\\d{2})?)")
      if (!is.na(usd_sal_m[1, 1])) {
        salary <- paste0("$", usd_sal_m[1, 2], "-$", usd_sal_m[1, 3])
      }
    }
    # Pattern 3: "Pay Range: $X Hourly-$Y Hourly" (intern/hourly roles)
    if (is.na(salary)) {
      hr_sal_m <- str_match(sal_plain,
        "(?i)(?:pay range|salary range)[^\\n]*?\\$([0-9,]+(?:\\.\\d{2})?)\\s*[Hh]ourly?\\s*[-\u2013]\\s*\\$([0-9,]+(?:\\.\\d{2})?)\\s*[Hh]ourly?")
      if (!is.na(hr_sal_m[1, 1])) {
        salary <- paste0("$", hr_sal_m[1, 2], "-$", hr_sal_m[1, 3], "/hr")
      }
    }
  }

  # --- Parse description HTML for sections ---
  desc_html <- job$description %||% ""
  desc_html <- strip_icims_boilerplate(desc_html)

  # Responsibilities: combine JOB SUMMARY + THE ROLE + KEY RESPONSIBILITIES
  # Note: THE PERSON describes candidate profile (experience), not responsibilities
  job_summary_text <- extract_icims_section(desc_html, c(
    "JOB\\s+SUMMARY",
    "Job\\s+Summary"
  ))
  role_text <- extract_icims_section(desc_html, c("THE\\s+ROLE"))
  person_text <- extract_icims_section(desc_html, c("THE\\s+PERSON"))
  key_resp_text <- extract_icims_section(desc_html, c(
    "KEY\\s+RESPONSIBILITIES",
    "RESPONSIBILITIES",
    "Key\\s+Responsibilities"
  ))

  resp_parts <- c()
  if (!is.na(job_summary_text)) resp_parts <- c(resp_parts, job_summary_text)
  if (!is.na(role_text))        resp_parts <- c(resp_parts, role_text)
  if (!is.na(key_resp_text))    resp_parts <- c(resp_parts, key_resp_text)
  responsibilities <- if (length(resp_parts) > 0) paste(resp_parts, collapse = "\n") else NA_character_

  # Fallback: try other responsibility patterns
  if (is.na(responsibilities)) {
    responsibilities <- extract_icims_section(desc_html, c(
      "What\\s+You.ll\\s+Do",
      "What\\s+You.ll\\s+Be\\s+Doing",
      "JOB\\s+DESCRIPTION",
      "Job\\s+Description"
    ))
  }

  # --- Education from ACADEMIC CREDENTIALS ---
  # Extract raw text first, then check for "Bachelor's or Master's degree (preferred)" pattern.
  # If "(preferred)" is present: Bachelor's → min_education, Master's → preferred_qualifications.
  # Routing side-effect: when a preferred degree is found, PREFERRED EXPERIENCE → min_experience
  #                      instead of preferred_qualifications.
  academic_cred_raw <- extract_icims_section(desc_html, c(
    "EDUCATION\\s+REQUIREMENTS",
    "Education\\s+Requirements",
    "ACADEMIC\\s+CREDENTIALS",
    "Academic\\s+Credentials",
    "EDUCATION",
    "Education\\s+(?:&|and)\\s+Experience",
    "Required\\s+Education"
  ))

  # Fallback: some AMD postings put heading + content on the same line inside one <p>.
  # e.g. <p>ACADEMIC CREDENTIALS: Bachelor's degree in EE</p>
  # extract_icims_section returns NA in this case (splits on the <p>, leaving nothing after).
  # Plain-text extraction catches it: find the heading then take everything after the colon.
  if (is.na(academic_cred_raw)) {
    plain_text_ac <- str_squish(str_replace_all(
      str_replace_all(desc_html, "<[^>]+>", " "), "&[a-zA-Z0-9#]+;", " "
    ))
    ac_inline_match <- regmatches(plain_text_ac, regexpr(
      "(?i)ACADEMIC\\s+CREDENTIALS\\s*:\\s*(.{10,})",
      plain_text_ac, perl = TRUE
    ))
    if (length(ac_inline_match) > 0) {
      ac_inline_content <- sub("(?i)ACADEMIC\\s+CREDENTIALS\\s*:\\s*", "", ac_inline_match, perl = TRUE)
      # Stop at next known section heading (all-caps word on its own or after a colon)
      stop_m <- regexpr("(?i)\\b(?:PREFERRED|REQUIRED|MINIMUM|LOCATION|BENEFITS|THE ROLE|KEY RESPONSIBILITIES)\\b", ac_inline_content, perl = TRUE)
      if (stop_m > 0) ac_inline_content <- substr(ac_inline_content, 1, stop_m - 1)
      ac_inline_content <- str_squish(ac_inline_content)
      if (nchar(ac_inline_content) > 5) academic_cred_raw <- ac_inline_content
    }
  }

  min_education <- NA_character_
  preferred_from_credentials <- NA_character_

  if (!is.na(academic_cred_raw)) {
    # Detect "Bachelor's or Master's degree (preferred) in X" — split if "(preferred)" present
    m_cred <- regmatches(academic_cred_raw, regexec(
      "(?i)^(bachelor[^,]+?)\\s+or\\s+(master[^(,]+?)(?:\\s*\\(preferred\\))?\\s+(in\\s+.+)$",
      academic_cred_raw, perl = TRUE
    ))[[1]]

    if (length(m_cred) == 4 && grepl("(?i)\\(preferred\\)", academic_cred_raw, perl = TRUE)) {
      bach_part <- str_squish(m_cred[2])
      mast_part <- str_squish(m_cred[3])
      field_part <- str_squish(m_cred[4])
      # Add "degree" to Bachelor's part if not already there
      deg_word <- regmatches(mast_part, regexpr("\\b(degree|diploma)\\b", mast_part,
                                                 ignore.case = TRUE, perl = TRUE))
      if (length(deg_word) == 0 || nchar(deg_word[1]) == 0) deg_word <- "degree"
      if (!grepl("\\b(degree|diploma)\\b", bach_part, ignore.case = TRUE, perl = TRUE)) {
        min_education <- str_squish(paste(bach_part, deg_word[1], field_part))
      } else {
        min_education <- str_squish(paste(bach_part, field_part))
      }
      preferred_from_credentials <- str_squish(paste(mast_part, field_part))
    } else {
      min_education <- academic_cred_raw
    }
  }

  preferred_edu_found <- !is.na(preferred_from_credentials)

  # --- Extract PREFERRED EXPERIENCE separately so we can route it correctly ---
  pref_exp_text <- extract_icims_section(desc_html, c(
    "PREFERRED\\s+EXPERIENCE",
    "Preferred\\s+Experience"
  ))

  # --- Experience from dedicated minimum experience/qualifications sections ---
  min_experience <- extract_icims_section(desc_html, c(
    "JOB\\s+QUALIFICATIONS",
    "Job\\s+Qualifications",
    "REQUIRED\\s+QUALIFICATIONS",
    "Required\\s+Qualifications",
    "MINIMUM\\s+QUALIFICATIONS",
    "Minimum\\s+Qualifications",
    "REQUIRED\\s+EXPERIENCE",
    "Required\\s+Experience",
    "REQUIREMENTS",
    "Requirements",
    "What\\s+We\\s+Need\\s+To\\s+See"   # AMD internship/entry-level format
  ))

  # --- Standard preferred section (extracted early so routing can check it) ---
  standard_preferred <- extract_icims_section(desc_html, c(
    "PREFERRED\\s+QUALIFICATIONS",
    "Preferred\\s+Qualifications",
    "NICE\\s+TO\\s+HAVE",
    "Nice\\s+to\\s+Have",
    "Ways\\s+To\\s+Stand\\s+Out\\s+From\\s+The\\s+Crowd"  # AMD internship/entry-level format
  ))

  # Route PREFERRED EXPERIENCE → min_experience when there is no dedicated minimum
  # experience section. This covers two cases:
  #   1. "(preferred)" degree split fired (preferred_edu_found=TRUE): PREFERRED EXPERIENCE
  #      is the actual required experience for that posting (e.g. 80965, 81108).
  #   2. No dedicated min section AND no separate preferred section: the posting uses
  #      PREFERRED EXPERIENCE as its sole experience requirement (e.g. 81108-style postings
  #      that only have ACADEMIC CREDENTIALS + PREFERRED EXPERIENCE sections).
  if (!is.na(pref_exp_text) && is.na(min_experience) &&
      (preferred_edu_found || is.na(standard_preferred))) {
    min_experience <- pref_exp_text
    pref_exp_text <- NA_character_  # consumed — don't also add to preferred_quals
  }

  # Fallback: extract "The ideal candidate will/should have..." sentence from role text.
  # AMD embeds core experience requirements as a sentence in THE ROLE description paragraph.
  if (is.na(min_experience) && !is.na(responsibilities)) {
    ideal_m <- regmatches(
      responsibilities,
      regexpr(
        "(?i)The ideal candidate will (?:have|need)[^.]+\\.",
        responsibilities, perl = TRUE
      )
    )
    if (length(ideal_m) > 0 && nchar(ideal_m) > 20) {
      min_experience <- str_squish(ideal_m)
    }
  }

  # If no separate education section, try to split education bullets out of the
  # qualifications/requirements text.
  #
  # Design notes:
  #   - "associate" alone is too broad (matches "associates" meaning colleagues).
  #     Only match "associate's degree" / "associate degree".
  #   - High school diploma / GED are not extracted as min_education for semiconductor
  #     roles — they stay in min_experience as a general requirement bullet.
  #   - B.S/M.S abbreviations REQUIRE the dot after the first letter (\\bB\\.S... /
  #     \\bM\\.S...) so "MS Suite", "BS degree" (standalone) don't false-match.
  #   - BSc/MSc (no dots) are handled by the explicit [c] branch.
  #   - BSEE, BSCS, MSEE, etc. are matched by the [A-Z]{2,4} branch.
  ms_prod_re <- paste0("(?:Office|Excel|Word|PowerPoint|Outlook|Teams|Azure|SQL|Access|",
                       "Windows|Project|Visio|Copilot|Visual\\s+Studio|SQL\\s+Server|",
                       "Dynamics|SharePoint)")
  edu_pattern <- paste0(
    "(?i)(\\bbachelor|\\bmaster(?!ing)|\\bassociate'?s?\\s+degree|ph\\.?d|\\bdegree(?!\\s+of)\\b|",
    "college|technical\\s+school)|",
    "(?-i:\\bB\\.S[c]?\\.?\\b|\\bM\\.S[c]?\\.?\\b|",   # B.S, B.Sc, M.S, M.Sc (dot required)
    "\\bB\\.A\\.?\\b|\\bM\\.A\\.?\\b|",                 # B.A, M.A (dot required)
    "\\bBS[A-Z]{2,4}\\b|\\bMS[A-Z]{2,4}\\b|",           # BSEE, BSCS, MSEE, MSCS, etc.
    "\\bBA[A-Z]{2,4}\\b|\\bMA[A-Z]{2,4}\\b|",           # BACS, MACS, etc.
    "\\bBSc\\b|\\bMSc\\b|",                              # BSc, MSc (lowercase-c variant)
    "\\bBS(?!\\s+", ms_prod_re, ")\\b|",                 # plain BS (not BS Office etc.)
    "\\bMS(?!\\s+", ms_prod_re, ")\\b)"                  # plain MS (not MS Office etc.)
  )

  if (is.na(min_education) && !is.na(min_experience)) {
    bullets <- str_split(min_experience, "\n")[[1]]
    bullets <- str_squish(bullets)
    bullets <- bullets[nchar(bullets) > 0]

    edu_idx <- which(grepl(edu_pattern, bullets, perl = TRUE))
    if (length(edu_idx) > 0) {
      min_education <- paste(bullets[edu_idx], collapse = "\n")
      remaining <- bullets[-edu_idx]
      if (length(remaining) > 0) {
        min_experience <- paste(remaining, collapse = "\n")
      } else {
        min_experience <- NA_character_
      }
    }
  }

  # Append THE PERSON section to min_experience (candidate profile describes required experience)
  if (!is.na(person_text)) {
    min_experience <- if (is.na(min_experience)) person_text else paste(min_experience, person_text, sep = "\n")
  }

  # --- Keysight / generic "Qualifications" block fallback ---
  # Keysight uses a "Qualifications" section (not "Minimum Qualifications") which may contain
  # a plain bullet list or Required:/Desired: sub-sections.
  # A separate "Desired Qualifications" section maps to preferred_qualifications.
  if (is.na(min_education) && is.na(min_experience)) {
    qual_block <- extract_icims_section(desc_html, c(
      "Qualifications\\s*:?",
      "QUALIFICATIONS\\s*:?"
    ))

    if (!is.na(qual_block)) {
      qual_lines <- str_split(qual_block, "\n")[[1]]
      qual_lines <- str_squish(qual_lines)
      qual_lines <- qual_lines[nchar(qual_lines) > 0]

      req_idx <- which(grepl("^Required\\s*:?$", qual_lines, ignore.case = TRUE))
      des_idx <- which(grepl("^(?:Desired|Preferred)\\s*:?$|^(?:Desired|Preferred)\\s+Qualifications\\s*:?$",
                             qual_lines, ignore.case = TRUE, perl = TRUE))

      req_lines <- character(0)
      des_lines <- character(0)

      if (length(req_idx) > 0) {
        req_start <- req_idx[1] + 1
        req_end   <- if (length(des_idx) > 0 && des_idx[1] > req_idx[1]) des_idx[1] - 1 else length(qual_lines)
        req_lines <- qual_lines[seq(req_start, req_end)]
      } else if (length(des_idx) > 0) {
        req_lines <- qual_lines[seq_len(des_idx[1] - 1)]
      } else {
        req_lines <- qual_lines
      }

      if (length(des_idx) > 0 && des_idx[1] < length(qual_lines)) {
        des_lines <- qual_lines[seq(des_idx[1] + 1, length(qual_lines))]
      }

      # Filter sub-heading labels (short lines ending with colon), salary/boilerplate text
      req_lines <- req_lines[!grepl("^(Required|Desired|Preferred|Musts?|Qualities|Skills?)\\s*:?$",
                                    req_lines, ignore.case = TRUE)]
      req_lines <- req_lines[!grepl("^[A-Za-z &/]{2,40}:\\s*$", req_lines, perl = TRUE)]
      req_lines <- req_lines[!grepl("(?i)^pay\\s+range|^USD\\s+\\$|^\\$[0-9,]+.*(?:Year|Hourly)",
                                    req_lines, perl = TRUE)]
      req_lines <- req_lines[!grepl("(?i)Visa Sponsorship is not available|sponsorship for employment visa",
                                    req_lines, perl = TRUE)]

      # Lines with enrollment-eligibility language are not the education requirement itself
      # (e.g. "must be enrolled in accredited college/university") — keep them in exp_idx
      enrollment_note_re <- "(?i)(\\benrolled\\b.*college|college/university|accredited college|must be enrolled)"

      if (length(req_lines) > 0) {
        # Bullets with a degree keyword AND "preferred" → preferred_qualifications
        pref_edu_idx <- which(grepl(edu_pattern, req_lines, perl = TRUE) &
                              grepl("\\bpreferred\\b", req_lines, ignore.case = TRUE, perl = TRUE))
        req_edu_idx  <- which(grepl(edu_pattern, req_lines, perl = TRUE) &
                              !grepl("\\bpreferred\\b", req_lines, ignore.case = TRUE, perl = TRUE) &
                              !grepl(enrollment_note_re, req_lines, perl = TRUE))
        exp_idx      <- setdiff(seq_along(req_lines), c(pref_edu_idx, req_edu_idx))

        if (length(pref_edu_idx) > 0) {
          des_lines <- c(req_lines[pref_edu_idx], des_lines)
        }
        if (length(req_edu_idx) > 0) {
          min_education <- paste(req_lines[req_edu_idx], collapse = "\n")
        }
        if (length(exp_idx) > 0) {
          min_experience <- paste(req_lines[exp_idx], collapse = "\n")
        }
      }

      # Filter boilerplate from des_lines (export control notice, salary note, etc.)
      ks_boilerplate <- paste0(
        "(?i)(^pay\\s+range|^USD\\s+\\$|^\\$[0-9,]+.*Year$|Salary Range listed below|",
        "level of role will be based on applicable experience|",
        "requires access to technology.*export control|",
        "export control.*U\\.S\\. Government)"
      )
      des_lines <- des_lines[!grepl(ks_boilerplate, des_lines, perl = TRUE)]

      if (length(des_lines) > 0 && is.na(standard_preferred)) {
        standard_preferred <- paste(des_lines, collapse = "\n")
      }
    }
  }

  # Keysight "Desired Qualifications" as a standalone preferred section
  if (is.na(standard_preferred)) {
    desired_qual_block <- extract_icims_section(desc_html, c(
      "Desired\\s+Qualifications\\s*:?",
      "DESIRED\\s+QUALIFICATIONS\\s*:?"
    ))
    if (!is.na(desired_qual_block)) standard_preferred <- desired_qual_block
  }

  # Handle "Experience:" section: "required" lines → min_experience/min_education,
  # "preferred" lines → standard_preferred. Handles Keysight 52268-style structure.
  exp_section_raw <- extract_icims_section(desc_html, c("Experience\\s*:?", "EXPERIENCE\\s*:?"))
  if (!is.na(exp_section_raw)) {
    exp_lines <- str_split(exp_section_raw, "\n")[[1]]
    exp_lines <- str_squish(exp_lines)
    exp_lines <- exp_lines[nchar(exp_lines) > 0]

    pref_exp_lines <- exp_lines[grepl("\\bpreferred\\b", exp_lines, ignore.case = TRUE, perl = TRUE) &
                                !grepl("\\brequired\\b",  exp_lines, ignore.case = TRUE, perl = TRUE)]
    req_exp_lines  <- exp_lines[grepl("\\brequired\\b",   exp_lines, ignore.case = TRUE, perl = TRUE)]

    if (length(req_exp_lines) > 0) {
      req_edu_exp_idx <- which(grepl(edu_pattern, req_exp_lines, perl = TRUE))
      if (length(req_edu_exp_idx) > 0) {
        new_edu <- paste(req_exp_lines[req_edu_exp_idx], collapse = "\n")
        min_education <- if (is.na(min_education)) new_edu else paste(c(min_education, new_edu), collapse = "\n")
      }
      rem_req <- req_exp_lines[setdiff(seq_along(req_exp_lines), req_edu_exp_idx)]
      if (length(rem_req) > 0) {
        min_experience <- if (is.na(min_experience)) paste(rem_req, collapse = "\n") else paste(c(min_experience, rem_req), collapse = "\n")
      }
    }
    if (length(pref_exp_lines) > 0) {
      standard_preferred <- if (is.na(standard_preferred)) paste(pref_exp_lines, collapse = "\n") else paste(c(standard_preferred, pref_exp_lines), collapse = "\n")
    }
  }

  # --- Preferred qualifications ---
  # Combine: preferred credential (Master's from split) + unconsumed pref_exp + standard preferred
  pref_parts <- character(0)
  if (!is.na(preferred_from_credentials)) pref_parts <- c(pref_parts, preferred_from_credentials)
  if (!is.na(pref_exp_text))              pref_parts <- c(pref_parts, pref_exp_text)
  if (!is.na(standard_preferred))         pref_parts <- c(pref_parts, standard_preferred)
  preferred_quals <- if (length(pref_parts) > 0) paste(pref_parts, collapse = "\n") else NA_character_

  # --- Skills ---
  essential_skills <- extract_icims_section(desc_html, c(
    "SKILLS",
    "Skills",
    "TECHNICAL\\s+SKILLS",
    "Technical\\s+Skills",
    "KEY\\s+SKILLS",
    "Key\\s+Skills"
  ))

  return(list(
    job_title = job_title,
    location = location,
    country = country,
    job_category = job_category,
    posting_date = posting_date,
    job_responsibilities = responsibilities,
    min_education = min_education,
    min_experience = min_experience,
    preferred_qualifications = preferred_quals,
    salary_range = salary,
    essential_skills = essential_skills,
    job_url = job_url
  ))
}

#' Fallback parser using JSON-LD when jobDescriptionConfig is not available
#'
#' @param page_html Raw HTML string
#' @param job_url URL for reference
#' @return List with structured job detail fields
parse_icims_job_jsonld <- function(page_html, job_url) {

  na_result <- list(
    job_title = NA_character_,
    location = NA_character_,
    country = NA_character_,
    job_category = NA_character_,
    posting_date = NA_Date_,
    job_responsibilities = NA_character_,
    min_education = NA_character_,
    min_experience = NA_character_,
    preferred_qualifications = NA_character_,
    salary_range = NA_character_,
    essential_skills = NA_character_,
    job_url = job_url
  )

  page <- tryCatch(read_html(page_html), error = function(e) NULL)
  if (is.null(page)) return(na_result)

  json_ld_nodes <- page %>% html_elements("script[type='application/ld+json']")
  job_data <- NULL
  for (node in json_ld_nodes) {
    json_text <- html_text(node)
    parsed <- tryCatch(fromJSON(json_text, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(parsed) && !is.null(parsed[["@type"]]) && parsed[["@type"]] == "JobPosting") {
      job_data <- parsed
      break
    }
  }
  if (is.null(job_data)) return(na_result)

  loc <- job_data$jobLocation$address
  country <- loc$addressCountry %||% ""
  location <- paste(loc$addressLocality %||% "", loc$addressRegion %||% "", sep = ", ")
  location <- sub("^, |, $", "", location)

  posting_date <- tryCatch(as.Date(job_data$datePosted), error = function(e) NA_Date_)

  return(list(
    job_title = job_data$title %||% NA_character_,
    location = if (nchar(location) > 0) location else NA_character_,
    country = country,
    job_category = NA_character_,
    posting_date = posting_date,
    job_responsibilities = NA_character_,
    min_education = NA_character_,
    min_experience = NA_character_,
    preferred_qualifications = NA_character_,
    salary_range = NA_character_,
    essential_skills = NA_character_,
    job_url = job_url
  ))
}

#' Main function to scrape an iCIMS company
#'
#' Uses parallel batch fetching for speed, then parses locally.
#'
#' @param company_name Name of the company
#' @param base_url Base URL of career site
#' @param fetch_details Whether to fetch full job details
#' @param batch_size Number of concurrent HTTP connections per batch
#' @return Dataframe of US jobs with all available details
scrape_icims_company <- function(company_name, base_url, fetch_details = TRUE) {

  log_message(paste("=== Starting iCIMS scrape for", company_name, "==="))
  start_time <- Sys.time()

  # Step 1: Fetch all job URLs from sitemap
  job_urls <- fetch_icims_sitemap(base_url)

  if (length(job_urls) == 0) {
    log_message(paste("No job URLs found for", company_name), level = "WARN")
    return(data.frame())
  }

  log_message(paste("Found", length(job_urls), "total job URLs"))

  # Step 2: Fetch each page, parse JSON-LD, filter to US
  # Uses fast curl fetching with 0.5s delay (~30 min for 1700 jobs)
  log_message(paste("Fetching and parsing", length(job_urls), "pages (0.5s delay, ~30 min)"))

  detail_results <- list()
  us_count <- 0
  non_us_count <- 0
  fetch_errors <- 0

  pb <- txtProgressBar(min = 0, max = length(job_urls), style = 3, file = stderr())

  for (i in seq_along(job_urls)) {
    url <- job_urls[i]

    page_html <- fetch_url_fast(url)

    if (is.null(page_html)) {
      fetch_errors <- fetch_errors + 1
      setTxtProgressBar(pb, i)
      Sys.sleep(0.5)
      next
    }

    details <- tryCatch(
      parse_icims_job_html(page_html, url),
      error = function(e) {
        fetch_errors <<- fetch_errors + 1
        NULL
      }
    )

    if (!is.null(details) && !is.na(details$country) &&
        grepl("United States", details$country, ignore.case = TRUE)) {
      detail_results[[length(detail_results) + 1]] <- details
      us_count <- us_count + 1
    } else {
      non_us_count <- non_us_count + 1
    }

    setTxtProgressBar(pb, i)

    # Log progress every 100 jobs
    if (i %% 100 == 0) {
      log_message(paste0("Progress: ", i, "/", length(job_urls),
                         " (US: ", us_count, ", non-US: ", non_us_count,
                         ", errors: ", fetch_errors, ")"))
    }

    Sys.sleep(0.5)
  }
  close(pb)

  log_message(paste("Found", us_count, "US jobs,", non_us_count, "non-US,", fetch_errors, "errors"))

  if (length(detail_results) == 0) {
    log_message(paste("No US jobs found for", company_name), level = "WARN")
    return(data.frame())
  }

  # Build dataframe
  jobs <- tibble(
    job_title = map_chr(detail_results, ~ .x$job_title %||% NA_character_),
    job_url = map_chr(detail_results, ~ .x$job_url %||% NA_character_),
    location = map_chr(detail_results, ~ .x$location %||% NA_character_),
    job_category = map_chr(detail_results, ~ .x$job_category %||% NA_character_),
    posting_date = as.Date(map_dbl(detail_results, ~ {
      d <- .x$posting_date
      if (is.null(d) || is.na(d)) NA_real_ else as.numeric(d)
    }), origin = "1970-01-01"),
    job_responsibilities = map_chr(detail_results, ~ .x$job_responsibilities %||% NA_character_),
    min_education = map_chr(detail_results, ~ .x$min_education %||% NA_character_),
    min_experience = map_chr(detail_results, ~ .x$min_experience %||% NA_character_),
    preferred_qualifications = map_chr(detail_results, ~ .x$preferred_qualifications %||% NA_character_),
    salary_range = map_chr(detail_results, ~ .x$salary_range %||% NA_character_),
    essential_skills = map_chr(detail_results, ~ .x$essential_skills %||% NA_character_),
    company_name = company_name,
    scraped_at = Sys.time()
  )

  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "mins"))

  log_message(paste("Completed", company_name, "in", round(duration, 2), "minutes"))
  log_message(paste("Total US jobs:", nrow(jobs)))

  # Summary stats
  for (col in c("job_category", "job_responsibilities", "min_education", "min_experience",
                "preferred_qualifications", "salary_range", "essential_skills")) {
    filled <- sum(!is.na(jobs[[col]]))
    log_message(paste0("  ", col, ": ", filled, "/", nrow(jobs)))
  }

  return(jobs)
}
