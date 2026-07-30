# Utility Functions for Semiconductor Jobs Scraper
# ==================================================

library(httr)
library(stringr)
library(lubridate)
library(glue)

#' Log a message with timestamp and level
#'
#' @param message Message to log
#' @param level Log level: DEBUG, INFO, WARN, ERROR
log_message <- function(message, level = "INFO") {
  
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste0("[", timestamp, "] [", level, "] ", message)
  
  # Print to console
  if (SCRAPER_CONFIG$verbose) {
    cat(log_entry, "\n")
  }
  
  # Write to log file
  log_file <- file.path(LOG_DIR, paste0("scrape_", format(Sys.Date(), "%Y%m%d"), ".log"))
  write(log_entry, file = log_file, append = TRUE)
  
  return(invisible(NULL))
}

#' Fetch URL with retry logic and error handling
#'
#' @param url URL to fetch
#' @param max_retries Maximum number of retry attempts
#' @return HTTP response object or NULL on failure
fetch_with_retry <- function(url, max_retries = SCRAPER_CONFIG$max_retries) {
  
  for (attempt in 1:max_retries) {
    
    tryCatch({
      
      # Make request with timeout and user agent
      response <- GET(
        url,
        timeout(SCRAPER_CONFIG$request_timeout),
        user_agent(SCRAPER_CONFIG$user_agent),
        add_headers(
          "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
          "Accept-Language" = "en-US,en;q=0.5",
          "Accept-Encoding" = "gzip, deflate, br",
          "Connection" = "keep-alive"
        )
      )
      
      # Check status
      if (status_code(response) == 200) {
        log_message(paste("Successfully fetched:", url), level = "DEBUG")
        return(response)
      } else {
        log_message(paste("HTTP", status_code(response), "for:", url), level = "WARN")
      }
      
    }, error = function(e) {
      log_message(paste("Attempt", attempt, "failed for", url, ":", e$message), level = "WARN")
    })
    
    # Wait before retry
    if (attempt < max_retries) {
      Sys.sleep(SCRAPER_CONFIG$delay_between_requests * 2)
    }
  }
  
  log_message(paste("Failed to fetch after", max_retries, "attempts:", url), level = "ERROR")
  return(NULL)
}

#' Extract job description sections using pattern matching
#'
#' @param description_text Full job description text
#' @return List with extracted sections
extract_job_sections <- function(description_text) {
  
  if (is.na(description_text) || nchar(description_text) == 0) {
    return(list(
      job_responsibilities = NA_character_,
      min_education = NA_character_,
      min_experience = NA_character_,
      preferred_qualifications = NA_character_,
      salary_range = NA_character_
    ))
  }
  
  # Clean up text
  text <- str_replace_all(description_text, "\\s+", " ")
  
  # Extract responsibilities
  responsibilities <- extract_section(
    text,
    patterns = c(
      "Responsibilities:?(.+?)(?=Qualifications|Requirements|Education|Experience|$)",
      "Job Description:?(.+?)(?=Qualifications|Requirements|Education|$)",
      "What You'll Do:?(.+?)(?=What You'll Need|Qualifications|$)"
    )
  )
  
  # Extract minimum education
  education <- extract_section(
    text,
    patterns = c(
      "Education:?(.+?)(?=Experience|Qualifications|Skills|$)",
      "Minimum Education:?(.+?)(?=Experience|Preferred|$)",
      "Required Education:?(.+?)(?=Experience|Preferred|$)",
      "(Bachelor's|Master's|PhD|Associate's).{0,100}(?:degree|required)",
      "(?:BS|MS|PhD).{0,50}in.{0,50}(?:Engineering|Science|Computer)"
    )
  )
  
  # Extract minimum experience
  experience <- extract_section(
    text,
    patterns = c(
      "Experience:?(.+?)(?=Education|Skills|Qualifications|$)",
      "Minimum Experience:?(.+?)(?=Education|Preferred|$)",
      "Required Experience:?(.+?)(?=Preferred|$)",
      "(\\d+\\+?\\s*years?).{0,100}(?:experience|of experience)"
    )
  )
  
  # Extract preferred qualifications
  preferred <- extract_section(
    text,
    patterns = c(
      "Preferred Qualifications:?(.+?)(?=Salary|Benefits|Equal|$)",
      "Nice to Have:?(.+?)(?=Salary|Benefits|$)",
      "Preferred:?(.+?)(?=Salary|Benefits|Equal|$)"
    )
  )
  
  # Extract salary (often not present)
  salary <- extract_section(
    text,
    patterns = c(
      "Salary Range:?(.+?)(?=Benefits|Equal|$)",
      "Compensation:?(.+?)(?=Benefits|Equal|$)",
      "\\$[0-9,]+\\s*-\\s*\\$[0-9,]+",
      "\\$[0-9,.]+K?\\s*-\\s*\\$[0-9,.]+K?"
    )
  )
  
  return(list(
    job_responsibilities = clean_extracted_text(responsibilities),
    min_education = clean_extracted_text(education),
    min_experience = clean_extracted_text(experience),
    preferred_qualifications = clean_extracted_text(preferred),
    salary_range = clean_extracted_text(salary)
  ))
}

#' Extract text section using multiple regex patterns
#'
#' @param text Full text to search
#' @param patterns Vector of regex patterns to try
#' @return Extracted text or NA
extract_section <- function(text, patterns) {
  
  for (pattern in patterns) {
    matches <- str_match(text, regex(pattern, ignore_case = TRUE))
    if (ncol(matches) >= 2 && !is.na(matches[1, 2])) {
      return(str_trim(matches[1, 2]))
    } else if (!is.na(matches[1, 1])) {
      return(str_trim(matches[1, 1]))
    }
  }
  
  return(NA_character_)
}

#' Clean extracted text section
#'
#' @param text Text to clean
#' @return Cleaned text or NA
clean_extracted_text <- function(text) {
  
  if (is.na(text)) {
    return(NA_character_)
  }
  
  # Remove extra whitespace
  text <- str_squish(text)
  
  # Limit length
  if (nchar(text) > 5000) {
    text <- substr(text, 1, 5000)
  }
  
  # Remove if too short
  if (nchar(text) < 3) {
    return(NA_character_)
  }
  
  return(text)
}

#' Parse various date string formats
#'
#' @param date_string Date string to parse
#' @return Date object or NA
parse_date_string <- function(date_string) {
  
  if (is.na(date_string) || nchar(date_string) == 0) {
    return(NA_Date_)
  }
  
  # Try different date formats
  formats <- c(
    "%Y-%m-%d",
    "%m/%d/%Y",
    "%d/%m/%Y",
    "%B %d, %Y",
    "%b %d, %Y",
    "%Y-%m-%dT%H:%M:%S"
  )
  
  for (fmt in formats) {
    parsed <- tryCatch({
      as.Date(date_string, format = fmt)
    }, error = function(e) NA)
    
    if (!is.na(parsed)) {
      return(parsed)
    }
  }
  
  # Handle "Posted Today" or "Posted Yesterday"
  if (str_detect(date_string, "(?i)today")) {
    return(Sys.Date())
  }
  if (str_detect(date_string, "(?i)yesterday")) {
    return(Sys.Date() - days(1))
  }

  # Handle relative dates (e.g., "Posted 2 days ago", "Posted 30+ Days Ago")
  if (str_detect(date_string, "(?i)(day|week|month)s? ago")) {

    number <- as.numeric(str_extract(date_string, "\\d+"))
    if (is.na(number)) number <- 1

    if (str_detect(date_string, "(?i)day")) {
      return(Sys.Date() - days(number))
    } else if (str_detect(date_string, "(?i)week")) {
      return(Sys.Date() - weeks(number))
    } else if (str_detect(date_string, "(?i)month")) {
      return(Sys.Date() - months(number))
    }
  }

  # If all else fails, use today
  log_message(paste("Could not parse date:", date_string), level = "DEBUG")
  return(Sys.Date())
}

#' Check if robots.txt allows scraping
#'
#' @param base_url Base URL of the site
#' @param path Path to check (default: /careers)
#' @return TRUE if allowed, FALSE otherwise
check_robots_txt <- function(base_url, path = "/careers") {
  
  robots_url <- paste0(base_url, "/robots.txt")
  
  tryCatch({
    response <- GET(robots_url, timeout(10))
    
    if (status_code(response) == 200) {
      robots_content <- content(response, "text")
      
      # Simple check for Disallow rules
      # Note: This is a basic implementation
      if (str_detect(robots_content, paste0("(?i)Disallow:\\s*", path))) {
        log_message(paste("robots.txt disallows scraping", path), level = "WARN")
        return(FALSE)
      }
    }
  }, error = function(e) {
    log_message(paste("Could not fetch robots.txt:", e$message), level = "DEBUG")
  })
  
  return(TRUE)
}

#' Sanitize string for safe database insertion
#'
#' @param text Text to sanitize
#' @return Sanitized text
sanitize_text <- function(text) {
  
  if (is.na(text)) {
    return(NA_character_)
  }
  
  # Remove null bytes and other problematic characters
  text <- str_replace_all(text, "\\x00", "")
  text <- str_replace_all(text, "[\\x01-\\x1F\\x7F]", " ")
  
  # Normalize whitespace
  text <- str_squish(text)
  
  return(text)
}

#' Create a safe filename from company name
#'
#' @param company_name Company name
#' @return Safe filename string
safe_filename <- function(company_name) {
  
  name <- tolower(company_name)
  name <- str_replace_all(name, "[^a-z0-9]+", "_")
  name <- str_replace_all(name, "_+", "_")
  name <- str_remove_all(name, "^_|_$")
  
  return(name)
}

#' Get current quarter information
#'
#' @return List with quarter number and year
get_current_quarter <- function() {
  
  current_month <- month(Sys.Date())
  current_year <- year(Sys.Date())
  
  quarter <- ceiling(current_month / 3)
  
  return(list(
    quarter = quarter,
    year = current_year,
    label = paste0("Q", quarter, "_", current_year)
  ))
}
