# Main Scraper Orchestration Script
# ==================================
# Coordinates scraping across all companies and platforms

library(DBI)
library(RSQLite)
library(dplyr)
library(readr)
library(here)

# Source configuration and utilities
source(here("config/scraper_config.R"))
source(here("scrapers/utils.R"))
source(here("scrapers/workday_scraper.R"))
source(here("scrapers/oracle_scraper.R"))
source(here("scrapers/successfactors_scraper.R"))
source(here("scrapers/talentbrew_scraper.R"))
source(here("scrapers/icims_scraper.R"))
source(here("scrapers/dayforce_scraper.R"))
source(here("scrapers/boeing_scraper.R"))

# Initialize database
initialize_database <- function() {
  
  log_message("Initializing database")
  
  # Create database if it doesn't exist
  con <- dbConnect(SQLite(), DB_PATH)
  
  # Execute schema
  schema_file <- file.path(CONFIG_DIR, "db_schema.sql")
  if (file.exists(schema_file)) {
    schema <- read_file(schema_file)
    
    # Execute each statement separately
    statements <- str_split(schema, ";")[[1]]
    for (stmt in statements) {
      stmt <- str_trim(stmt)
      if (nchar(stmt) > 0) {
        dbExecute(con, stmt)
      }
    }
  }
  
  # Migrate: add new columns if they don't exist yet
  existing_cols <- dbListFields(con, "jobs")
  new_cols <- c("job_identification", "job_category", "degree_level", "ecl_gtc_required", "essential_skills")
  for (col in new_cols) {
    if (!(col %in% existing_cols)) {
      dbExecute(con, paste("ALTER TABLE jobs ADD COLUMN", col, "TEXT"))
      log_message(paste("Added column to jobs table:", col))
    }
  }

  dbDisconnect(con)
  log_message("Database initialized successfully")
}

# Load companies from configuration
load_companies <- function() {
  
  companies_file <- file.path(CONFIG_DIR, "companies.csv")
  
  if (!file.exists(companies_file)) {
    stop("Companies configuration file not found: ", companies_file)
  }
  
  companies <- read_csv(companies_file, show_col_types = FALSE)
  
  # Filter active companies
  companies <- companies %>%
    filter(active == TRUE)
  
  log_message(paste("Loaded", nrow(companies), "active companies"))
  
  return(companies)
}

# Ensure companies are in database
sync_companies_to_db <- function(companies) {
  
  con <- dbConnect(SQLite(), DB_PATH)
  
  for (i in 1:nrow(companies)) {
    company <- companies[i, ]
    
    # Check if company exists
    existing <- dbGetQuery(
      con,
      "SELECT company_id FROM companies WHERE company_name = ?",
      params = list(company$company_name)
    )
    
    if (nrow(existing) == 0) {
      # Insert new company
      dbExecute(
        con,
        "INSERT INTO companies (company_name, careers_url, platform, active)
         VALUES (?, ?, ?, ?)",
        params = list(
          company$company_name,
          company$careers_url,
          company$platform,
          1
        )
      )
      log_message(paste("Added company to database:", company$company_name))
    }
  }
  
  dbDisconnect(con)
}

# Start a new scrape run
start_scrape_run <- function(company_name) {
  
  con <- dbConnect(SQLite(), DB_PATH)
  
  # Get company ID
  company_id <- dbGetQuery(
    con,
    "SELECT company_id FROM companies WHERE company_name = ?",
    params = list(company_name)
  )$company_id[1]
  
  # Insert new run
  dbExecute(
    con,
    "INSERT INTO scrape_runs (company_id, run_date, status)
     VALUES (?, ?, 'running')",
    params = list(company_id, Sys.time())
  )
  
  run_id <- dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
  
  dbDisconnect(con)
  
  return(run_id)
}

# Complete a scrape run
complete_scrape_run <- function(run_id, jobs_found, jobs_new, jobs_updated, status = "completed", error = NULL) {
  
  con <- dbConnect(SQLite(), DB_PATH)
  
  # Calculate duration
  run_info <- dbGetQuery(
    con,
    "SELECT run_date FROM scrape_runs WHERE run_id = ?",
    params = list(run_id)
  )
  
  duration <- as.numeric(difftime(Sys.time(), run_info$run_date[1], units = "secs"))
  
  # Update run
  dbExecute(
    con,
    "UPDATE scrape_runs 
     SET jobs_found = ?, jobs_new = ?, jobs_updated = ?, 
         status = ?, error_message = ?, duration_seconds = ?
     WHERE run_id = ?",
    params = list(jobs_found, jobs_new, jobs_updated, status, if (is.null(error)) NA_character_ else error, duration, run_id)
  )
  
  dbDisconnect(con)
}

# Save jobs to database
save_jobs_to_db <- function(jobs_df, company_name, run_id) {
  
  if (nrow(jobs_df) == 0) {
    return(list(new = 0, updated = 0))
  }
  
  con <- dbConnect(SQLite(), DB_PATH)
  
  # Get company ID
  company_id <- dbGetQuery(
    con,
    "SELECT company_id FROM companies WHERE company_name = ?",
    params = list(company_name)
  )$company_id[1]
  
  jobs_new <- 0
  jobs_updated <- 0
  
  for (i in 1:nrow(jobs_df)) {
    job <- jobs_df[i, ]
    
    # Check if job exists
    existing <- dbGetQuery(
      con,
      "SELECT job_id, updated_at FROM jobs WHERE job_url = ?",
      params = list(job$job_url)
    )
    
    # Helper to safely get a column value (NA if column doesn't exist)
    safe_col <- function(df, col_name) {
      if (col_name %in% names(df)) df[[col_name]] else NA_character_
    }

    if (nrow(existing) == 0) {
      # Insert new job
      dbExecute(
        con,
        "INSERT INTO jobs (
          company_id, job_title, job_url, location,
          job_responsibilities, min_education, min_experience,
          preferred_qualifications, salary_range,
          job_identification, job_category, degree_level, ecl_gtc_required,
          essential_skills, posting_date, scrape_run_id
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          company_id,
          sanitize_text(job$job_title),
          job$job_url,
          sanitize_text(job$location),
          sanitize_text(safe_col(job, "job_responsibilities")),
          sanitize_text(safe_col(job, "min_education")),
          sanitize_text(safe_col(job, "min_experience")),
          sanitize_text(safe_col(job, "preferred_qualifications")),
          sanitize_text(safe_col(job, "salary_range")),
          sanitize_text(safe_col(job, "job_identification")),
          sanitize_text(safe_col(job, "job_category")),
          sanitize_text(safe_col(job, "degree_level")),
          sanitize_text(safe_col(job, "ecl_gtc_required")),
          sanitize_text(safe_col(job, "essential_skills")),
          job$posting_date,
          run_id
        )
      )
      jobs_new <- jobs_new + 1

    } else {
      # Update existing job — use COALESCE for detail fields so a failed
      # fetch (NULL) does not overwrite previously-good data in the DB.
      dbExecute(
        con,
        "UPDATE jobs SET
          job_title = ?, location = ?,
          job_responsibilities    = COALESCE(?, job_responsibilities),
          min_education           = COALESCE(?, min_education),
          min_experience          = COALESCE(?, min_experience),
          preferred_qualifications= COALESCE(?, preferred_qualifications),
          salary_range            = COALESCE(?, salary_range),
          job_identification = ?, job_category = ?, degree_level = ?, ecl_gtc_required = ?,
          essential_skills        = COALESCE(?, essential_skills),
          posting_date = ?, updated_at = CURRENT_TIMESTAMP
         WHERE job_id = ?",
        params = list(
          sanitize_text(job$job_title),
          sanitize_text(job$location),
          sanitize_text(safe_col(job, "job_responsibilities")),
          sanitize_text(safe_col(job, "min_education")),
          sanitize_text(safe_col(job, "min_experience")),
          sanitize_text(safe_col(job, "preferred_qualifications")),
          sanitize_text(safe_col(job, "salary_range")),
          sanitize_text(safe_col(job, "job_identification")),
          sanitize_text(safe_col(job, "job_category")),
          sanitize_text(safe_col(job, "degree_level")),
          sanitize_text(safe_col(job, "ecl_gtc_required")),
          sanitize_text(safe_col(job, "essential_skills")),
          job$posting_date,
          existing$job_id[1]
        )
      )
      jobs_updated <- jobs_updated + 1
    }
  }
  
  dbDisconnect(con)
  
  log_message(paste("Saved", jobs_new, "new jobs and updated", jobs_updated, "existing jobs"))
  
  return(list(new = jobs_new, updated = jobs_updated))
}

# Scrape a single company
scrape_company <- function(company_name, fetch_details = TRUE) {
  
  log_message(paste("\n========================================"))
  log_message(paste("Starting scrape for:", company_name))
  log_message(paste("========================================\n"))

  # Ensure database is initialized
  initialize_database()

  # Load company info
  companies <- load_companies()
  company_info <- companies %>% filter(company_name == !!company_name)
  
  if (nrow(company_info) == 0) {
    log_message(paste("Company not found:", company_name), level = "ERROR")
    return(NULL)
  }
  
  company_info <- company_info[1, ]

  # Ensure company exists in database
  sync_companies_to_db(company_info)

  # Start scrape run
  run_id <- start_scrape_run(company_name)
  
  # Scrape based on platform
  jobs <- tryCatch({
    
    if (company_info$platform == "workday") {
      scrape_workday_company(company_name, company_info$careers_url, fetch_details)
      
    } else if (company_info$platform == "oracle") {
      scrape_oracle_company(company_name, company_info$careers_url, fetch_details)

    } else if (company_info$platform == "successfactors") {
      scrape_successfactors_company(company_name, company_info$careers_url, fetch_details)

    } else if (company_info$platform == "talentbrew") {
      scrape_talentbrew_company(company_name, company_info$careers_url, fetch_details)

    } else if (company_info$platform == "icims") {
      scrape_icims_company(company_name, company_info$careers_url, fetch_details)

    } else if (company_info$platform == "dayforce") {
      scrape_dayforce_company(company_name, company_info$careers_url, fetch_details)

    } else if (company_info$platform == "boeing") {
      scrape_boeing_company(company_name)

    } else {
      # Custom scrapers
      log_message(paste("Platform", company_info$platform, "not yet implemented"), level = "WARN")
      data.frame()
    }
    
  }, error = function(e) {
    log_message(paste("Error scraping", company_name, ":", e$message), level = "ERROR")
    complete_scrape_run(run_id, 0, 0, 0, "failed", e$message)
    return(data.frame())
  })
  
  # Save to database
  if (nrow(jobs) > 0) {
    save_result <- save_jobs_to_db(jobs, company_name, run_id)
    complete_scrape_run(run_id, nrow(jobs), save_result$new, save_result$updated)
  } else {
    complete_scrape_run(run_id, 0, 0, 0)
  }
  
  # Export to CSV
  export_jobs_csv(jobs, company_name)
  
  log_message(paste("Completed scrape for:", company_name))
  
  return(jobs)
}

# Scrape all active companies
scrape_all_companies <- function(fetch_details = TRUE) {
  
  log_message("\n========================================")
  log_message("STARTING QUARTERLY SCRAPE")
  log_message(paste("Quarter:", get_current_quarter()$label))
  log_message("========================================\n")
  
  start_time <- Sys.time()
  
  # Initialize database
  initialize_database()
  
  # Load and sync companies
  companies <- load_companies()
  sync_companies_to_db(companies)
  
  # Scrape each company
  all_jobs <- list()
  
  for (i in 1:nrow(companies)) {
    company <- companies$company_name[i]
    
    jobs <- scrape_company(company, fetch_details)
    
    if (nrow(jobs) > 0) {
      all_jobs[[company]] <- jobs
    }
    
    # Delay between companies
    if (i < nrow(companies)) {
      log_message(paste("Waiting", SCRAPER_CONFIG$delay_between_companies, "seconds before next company..."))
      Sys.sleep(SCRAPER_CONFIG$delay_between_companies)
    }
  }
  
  # Create combined export
  if (length(all_jobs) > 0) {
    combined_jobs <- bind_rows(all_jobs)
    export_jobs_csv(combined_jobs, "ALL_COMPANIES")
  }
  
  end_time <- Sys.time()
  total_duration <- difftime(end_time, start_time, units = "mins")
  
  log_message("\n========================================")
  log_message("SCRAPING COMPLETED")
  log_message(paste("Total duration:", round(total_duration, 2), "minutes"))
  log_message(paste("Total jobs:", nrow(combined_jobs)))
  log_message("========================================\n")
  
  return(combined_jobs)
}

# Export jobs to CSV
export_jobs_csv <- function(jobs_df, company_name) {
  
  if (nrow(jobs_df) == 0) {
    return(invisible(NULL))
  }
  
  # Create filename
  date_str <- format(Sys.Date(), "%Y%m%d")
  filename <- paste0("jobs_", safe_filename(company_name), "_", date_str, ".csv")
  filepath <- file.path(EXPORT_DIR, filename)
  
  # Write CSV
  write_csv(jobs_df, filepath)
  log_message(paste("Exported jobs to:", filepath))
  
  return(filepath)
}

# Main entry point when script is sourced
if (!interactive()) {
  # Run from command line
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) > 0 && args[1] == "all") {
    scrape_all_companies()
  } else if (length(args) > 0) {
    scrape_company(args[1])
  } else {
    message("Usage: Rscript main.R [company_name|all]")
  }
}
