# Semiconductor Jobs Database Scraper

Automated quarterly scraping tool for semiconductor job postings across major companies.

## 📋 Project Overview

This R-based scraper collects job posting data from major semiconductor companies:
- Samsung Austin Semiconductor
- Texas Instruments
- Applied Materials
- NXP Semiconductors
- SkyWater Technology
- Tokyo Electron

### Data Captured
- Job Title
- Location
- Job Responsibilities
- Minimum Education Requirements
- Minimum Experience Required
- Preferred Qualifications
- Salary Range (when available)
- Posting Date
- Company
- Job URL

## 🚀 Quick Start

### Prerequisites
```r
# Required R packages
install.packages(c(
  "rvest",       # Web scraping
  "httr",        # HTTP requests
  "jsonlite",    # JSON parsing
  "DBI",         # Database interface
  "RSQLite",     # SQLite database
  "dplyr",       # Data manipulation
  "lubridate",   # Date handling
  "readr",       # CSV reading/writing
  "purrr",       # Functional programming
  "stringr",     # String manipulation
  "glue",        # String interpolation
  "xml2"         # XML parsing
))
```

### Installation
```bash
git clone <your-repo-url>
cd semiconductor_jobs_scraper
```

### First Run
```r
# Source the main script
source("scrapers/main.R")

# Run scraper for one company (recommended for testing)
scrape_company("Applied Materials")

# Or run all companies
scrape_all_companies()
```

## 📁 Project Structure

```
semiconductor_jobs_scraper/
├── README.md                 # This file
├── config/
│   ├── companies.csv        # Company URLs and metadata
│   ├── db_schema.sql        # Database schema definition
│   └── scraper_config.R     # Global configuration
├── scrapers/
│   ├── main.R              # Main scraping orchestration
│   ├── workday_scraper.R   # Scraper for Workday platforms
│   ├── oracle_scraper.R    # Scraper for Oracle CX
│   ├── samsung_scraper.R   # Samsung-specific scraper
│   ├── tel_scraper.R       # Tokyo Electron scraper
│   ├── skywater_scraper.R  # SkyWater scraper
│   └── utils.R             # Shared utility functions
├── data/
│   ├── semiconductor_jobs.db  # SQLite database
│   └── exports/               # CSV exports by date
├── logs/
│   └── scrape_YYYYMMDD.log   # Daily logs
└── docs/
    ├── quarterly_schedule.md  # Scraping schedule
    └── legal_considerations.md # Legal/ethical notes
```

## 🗄️ Database Schema

The SQLite database contains three main tables:

### `jobs`
Primary job listings table with all extracted fields

### `companies`
Company metadata and scraping configuration

### `scrape_runs`
Tracking of each scraping session

## ⚙️ Configuration

Edit `config/companies.csv` to update company URLs or add new companies:

```csv
company_name,careers_url,platform,active
Applied Materials,https://amat.wd1.myworkdayjobs.com/External,workday,TRUE
Texas Instruments,https://edbz.fa.us2.oraclecloud.com/hcmUI/CandidateExperience/en/sites/CX/jobs,oracle,TRUE
```

## 📊 Data Export

Exports are automatically saved to `data/exports/`:
- `jobs_YYYYMMDD.csv` - All jobs from that run
- `jobs_full_YYYYMMDD.csv` - Complete database export

## 🔄 Quarterly Automation

### Option 1: GitHub Actions (Recommended)
See `.github/workflows/quarterly_scrape.yml`

### Option 2: Cron Job
```bash
# Add to crontab (runs first day of quarter at 2am)
0 2 1 1,4,7,10 * cd /path/to/project && Rscript scrapers/main.R
```

### Option 3: Manual
```r
source("scrapers/main.R")
scrape_all_companies()
```

## ⚖️ Legal & Ethical Considerations

**IMPORTANT**: Web scraping may violate terms of service. This tool is for:
- Personal research
- Academic studies
- Situations where you have explicit permission

**Best Practices Implemented:**
- Respects `robots.txt`
- Rate limiting (2-5 second delays)
- User-Agent identification
- Polite scraping behavior
- No credential harvesting

**Consider Alternative Approaches:**
- Company APIs (if available)
- RSS feeds
- Official data exports
- Manual data collection

## 🐛 Troubleshooting

### Common Issues

**"Connection timeout"**
- Increase delay in `config/scraper_config.R`
- Check network connection
- Verify company website is accessible

**"No jobs found"**
- Website structure may have changed
- Check scraper selectors in company-specific scraper file
- Review logs in `logs/` directory

**"Database locked"**
- Close any SQLite browser tools
- Ensure no other scraping process is running

## 📈 Data Analysis

Example queries:

```r
library(DBI)
library(dplyr)

con <- dbConnect(RSQLite::SQLite(), "data/semiconductor_jobs.db")

# Jobs by company
dbGetQuery(con, "
  SELECT company, COUNT(*) as job_count
  FROM jobs
  GROUP BY company
")

# Jobs requiring specific education
dbGetQuery(con, "
  SELECT job_title, company, location
  FROM jobs
  WHERE min_education LIKE '%PhD%'
")

dbDisconnect(con)
```

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly
5. Submit a pull request

## 📝 License

[Your chosen license]

## 🔗 Resources

- [rvest documentation](https://rvest.tidyverse.org/)
- [Workday API docs](https://community.workday.com/api)
- [robots.txt tester](https://developers.google.com/search/docs/crawling-indexing/robots/create-robots-txt)

## 📧 Contact

[Your contact information]

---

**Last Updated**: February 2026
**Version**: 1.0.0
