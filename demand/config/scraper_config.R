# Global Configuration for Semiconductor Jobs Scraper
# =====================================================

# Project Paths
PROJECT_ROOT <- here::here()
DATA_DIR <- file.path(PROJECT_ROOT, "data")
LOG_DIR <- file.path(PROJECT_ROOT, "logs")
EXPORT_DIR <- file.path(DATA_DIR, "exports")
CONFIG_DIR <- file.path(PROJECT_ROOT, "config")

# Database
DB_PATH <- file.path(DATA_DIR, "semiconductor_jobs.db")

# Scraping Parameters
SCRAPER_CONFIG <- list(
  # Rate limiting
  delay_between_requests = 3,  # seconds between requests
  delay_between_companies = 10, # seconds between different companies
  max_retries = 3,              # number of retries on failure
  
  # Timeouts
  request_timeout = 30,         # seconds
  page_load_timeout = 20,       # seconds
  
  # User Agent
  user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36 SemiconductorJobsScraper/1.0 (+research@example.com)",
  
  # Pagination
  max_pages_per_company = 50,   # safety limit
  jobs_per_page = 20,           # typical default
  
  # Data Quality
  min_job_title_length = 3,
  max_job_title_length = 200,
  
  # Logging
  log_level = "INFO",            # DEBUG, INFO, WARN, ERROR
  verbose = TRUE
)

# Keywords for semiconductor-specific filtering (optional)
SEMICONDUCTOR_KEYWORDS <- c(
  "semiconductor", "wafer", "fab", "lithography", "etching",
  "deposition", "CMP", "metrology", "yield", "process engineer",
  "device", "IC", "chip", "ASIC", "analog", "digital", "mixed-signal",
  "CMOS", "BiCMOS", "memory", "logic", "power semiconductor"
)

# Field mappings for different platforms
# These help standardize data across different job board formats

WORKDAY_SELECTORS <- list(
  job_list = "ul[data-automation-id='jobResults'] li",
  job_title = "[data-automation-id='jobTitle']",
  job_location = "[data-automation-id='locations']",
  job_link = "a[data-automation-id='jobTitle']",
  next_page = "button[data-uxi-widget-type='paginationNext']",
  job_description = "[data-automation-id='jobPostingDescription']",
  posting_date = "[data-automation-id='postedOn']"
)

ORACLE_API_FIELDS <- list(
  # REST API endpoint paths (appended to base URL root)
  job_list_endpoint = "/hcmRestApi/resources/latest/recruitingCEJobRequisitions",
  job_detail_endpoint = "/hcmRestApi/resources/latest/recruitingCEJobRequisitionDetails",
  # JSON response field names
  job_id = "Id",
  job_title = "Title",
  posting_date = "PostedDate",
  primary_location = "PrimaryLocation",
  work_location = "workLocation",
  total_count = "TotalJobsCount",
  requisition_list = "requisitionList",
  # Detail endpoint field names
  description = "ExternalDescriptionStr",
  qualifications = "ExternalQualificationsStr",
  responsibilities = "ExternalResponsibilitiesStr",
  # Pagination
  page_size = 25
)

# Export settings
EXPORT_CONFIG <- list(
  csv_delimiter = ",",
  include_timestamp = TRUE,
  compress_exports = FALSE,  # set TRUE to gzip exports
  backup_database = TRUE,    # backup DB before major updates
  backup_retention_days = 90
)

# Email notifications (optional - configure SMTP settings)
EMAIL_CONFIG <- list(
  enabled = FALSE,
  smtp_server = "smtp.gmail.com",
  smtp_port = 587,
  from_email = "your-email@example.com",
  to_email = "recipient@example.com",
  send_on_error = TRUE,
  send_on_completion = FALSE
)

# Quarterly schedule
QUARTER_START_MONTHS <- c(1, 4, 7, 10)  # January, April, July, October

# Create directories if they don't exist
create_directories <- function() {
  dirs <- c(DATA_DIR, LOG_DIR, EXPORT_DIR, CONFIG_DIR)
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      message(paste("Created directory:", dir))
    }
  }
}

# Initialize on source
create_directories()

# Print configuration on load (optional)
if (SCRAPER_CONFIG$verbose) {
  message("Semiconductor Jobs Scraper Configuration Loaded")
  message(paste("Database:", DB_PATH))
  message(paste("Request delay:", SCRAPER_CONFIG$delay_between_requests, "seconds"))
}
