# Quick Start Guide

## Installation (5 minutes)

### 1. Clone the Repository
```bash
git clone <your-repo-url>
cd semiconductor_jobs_scraper
```

### 2. Install R Packages
```r
# Start R or RStudio
install.packages(c(
  "rvest", "httr", "jsonlite", "DBI", "RSQLite",
  "dplyr", "lubridate", "readr", "purrr", "stringr",
  "glue", "xml2", "here"
))
```

### 3. Test Installation
```r
source("scrapers/main.R")
```

You should see: "Semiconductor Jobs Scraper Configuration Loaded"

## First Scrape (10-15 minutes)

### Test with One Company

```r
# Start R in project root
source("scrapers/main.R")

# Scrape Applied Materials (Workday - usually works well)
jobs <- scrape_company("Applied Materials", fetch_details = FALSE)

# View results
View(jobs)
```

This will:
- ✅ Create the database
- ✅ Scrape job listings
- ✅ Save to SQLite and CSV
- ✅ Create logs

**Expected output:**
```
[2026-02-14 10:30:00] [INFO] Starting scrape for: Applied Materials
[2026-02-14 10:30:01] [INFO] Starting Workday scrape for Applied Materials
[2026-02-14 10:30:03] [INFO] Found 127 total jobs for Applied Materials
...
```

### Review Results

**Database:**
```r
library(DBI)
con <- dbConnect(RSQLite::SQLite(), "data/semiconductor_jobs.db")

# View jobs
dbGetQuery(con, "SELECT * FROM vw_jobs_full LIMIT 10")

# Stats
dbGetQuery(con, "SELECT * FROM vw_scrape_stats")

dbDisconnect(con)
```

**CSV Export:**
```r
# Check exports folder
list.files("data/exports")

# Read CSV
library(readr)
jobs_csv <- read_csv("data/exports/jobs_applied_materials_20260214.csv")
View(jobs_csv)
```

**Logs:**
```bash
cat logs/scrape_20260214.log
```

## Full Scrape (1-2 hours)

### With Full Job Details

```r
source("scrapers/main.R")

# This will take 1-2 hours depending on job volumes
all_jobs <- scrape_all_companies(fetch_details = TRUE)
```

### Without Full Details (Faster)

```r
# Just get listings, skip individual job page scraping
all_jobs <- scrape_all_companies(fetch_details = FALSE)
```

## Customization

### Change Companies

Edit `config/companies.csv`:
```csv
company_name,careers_url,platform,active
Applied Materials,https://amat.wd1.myworkdayjobs.com/External,workday,TRUE
My Company,https://example.com/careers,custom,FALSE
```

### Adjust Rate Limiting

Edit `config/scraper_config.R`:
```r
SCRAPER_CONFIG <- list(
  delay_between_requests = 5,  # Increase to 5 seconds
  delay_between_companies = 15, # Increase to 15 seconds
  ...
)
```

### Filter Jobs

```r
library(dplyr)

# Get only engineering roles
engineering_jobs <- all_jobs %>%
  filter(str_detect(job_title, "(?i)engineer|scientist"))

# Get specific locations
austin_jobs <- all_jobs %>%
  filter(str_detect(location, "(?i)austin|texas"))

# Get roles requiring PhD
phd_jobs <- all_jobs %>%
  filter(str_detect(min_education, "(?i)phd|doctorate"))
```

## Quarterly Schedule

### Option 1: GitHub Actions (Recommended)

Already configured in `.github/workflows/quarterly_scrape.yml`

Just push to GitHub and it will run automatically on:
- January 1
- April 1  
- July 1
- October 1

### Option 2: Cron Job (Linux/Mac)

```bash
# Edit crontab
crontab -e

# Add this line (runs at 2 AM on first day of quarter)
0 2 1 1,4,7,10 * cd /path/to/semiconductor_jobs_scraper && Rscript scrapers/main.R all
```

### Option 3: Windows Task Scheduler

1. Open Task Scheduler
2. Create Basic Task
3. Trigger: Monthly, on day 1 of January, April, July, October
4. Action: Start a program
5. Program: `Rscript.exe`
6. Arguments: `C:\path\to\scrapers\main.R all`
7. Start in: `C:\path\to\semiconductor_jobs_scraper`

## Troubleshooting

### "Package 'rvest' not found"
```r
install.packages("rvest")
```

### "Connection timeout"
```r
# Increase timeout in config/scraper_config.R
request_timeout = 60  # Increase from 30 to 60
```

### "No jobs found"
- Website structure may have changed
- Check if site is accessible in browser
- Review logs: `cat logs/scrape_*.log`
- Try with `fetch_details = FALSE` first

### "Database is locked"
```bash
# Close any SQLite browser tools
# Kill any running R processes
pkill -9 R
```

### Jobs look incomplete
```r
# Run with fetch_details = TRUE (slower but more complete)
jobs <- scrape_company("Applied Materials", fetch_details = TRUE)
```

## Analysis Examples

### Jobs by Company
```r
library(DBI)
con <- dbConnect(RSQLite::SQLite(), "data/semiconductor_jobs.db")

dbGetQuery(con, "
  SELECT company_name, COUNT(*) as jobs
  FROM vw_jobs_full
  GROUP BY company_name
  ORDER BY jobs DESC
")
```

### Top Locations
```r
dbGetQuery(con, "
  SELECT location, COUNT(*) as jobs
  FROM vw_jobs_full
  GROUP BY location
  ORDER BY jobs DESC
  LIMIT 10
")
```

### Recent Postings
```r
dbGetQuery(con, "
  SELECT job_title, company_name, location, posting_date
  FROM vw_jobs_full
  WHERE posting_date >= date('now', '-30 days')
  ORDER BY posting_date DESC
")
```

### Education Requirements
```r
dbGetQuery(con, "
  SELECT 
    CASE 
      WHEN min_education LIKE '%PhD%' THEN 'PhD'
      WHEN min_education LIKE '%Master%' THEN 'Masters'
      WHEN min_education LIKE '%Bachelor%' THEN 'Bachelors'
      ELSE 'Not Specified'
    END as education_level,
    COUNT(*) as jobs
  FROM vw_jobs_full
  GROUP BY education_level
")
```

## Next Steps

1. ✅ Run your first scrape
2. ✅ Explore the database
3. ✅ Customize configuration
4. ✅ Set up quarterly automation
5. ✅ Read legal considerations in `docs/legal_considerations.md`
6. ✅ Push to GitHub for version control

## Getting Help

- Review README.md for full documentation
- Check logs in `logs/` directory
- Review `docs/legal_considerations.md` before production use
- Adjust selectors in scraper files if sites change

---

**Ready to scrape!** Start with one company, verify results, then scale up.
