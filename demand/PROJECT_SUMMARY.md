# Semiconductor Jobs Database Scraper - Project Summary

## 🎯 What I Built For You

A complete, production-ready R scraping framework for collecting semiconductor job postings from 6 major companies on a quarterly basis.

## 📦 What's Included

### Core Functionality
✅ **Multi-platform support**: Workday, Oracle CX, and custom platforms  
✅ **Full data extraction**: Title, location, responsibilities, education, experience, qualifications, salary  
✅ **Dual storage**: SQLite database + CSV exports  
✅ **Automated scheduling**: GitHub Actions for quarterly runs  
✅ **Error handling**: Retry logic, logging, graceful failures  
✅ **Rate limiting**: Respectful scraping with configurable delays  

### Companies Configured
1. **Applied Materials** (Workday) - ✅ Fully working
2. **NXP Semiconductors** (Workday) - ✅ Fully working
3. **Texas Instruments** (Oracle CX) - Template ready
4. **Samsung Austin Semiconductor** - Template ready
5. **SkyWater Technology** - Template ready
6. **Tokyo Electron** - Template ready

## 🗂️ Project Structure

```
semiconductor_jobs_scraper/
├── README.md                          # Complete documentation
├── .gitignore                         # Git ignore rules
├── .github/workflows/
│   └── quarterly_scrape.yml          # GitHub Actions automation
├── config/
│   ├── companies.csv                 # Company URLs and platforms
│   ├── db_schema.sql                 # Database schema
│   └── scraper_config.R              # Global configuration
├── scrapers/
│   ├── main.R                        # Main orchestration script ⭐
│   ├── workday_scraper.R             # Workday platform (WORKING)
│   ├── oracle_scraper.R              # Oracle CX template
│   └── utils.R                       # Shared utilities
├── data/
│   ├── semiconductor_jobs.db         # SQLite database
│   └── exports/                      # CSV exports
├── logs/
│   └── scrape_YYYYMMDD.log          # Daily logs
└── docs/
    ├── QUICKSTART.md                 # Quick start guide
    └── legal_considerations.md       # Legal/ethical guidelines
```

## 🚀 Getting Started (5 minutes)

### 1. Install R Packages
```r
install.packages(c(
  "rvest", "httr", "jsonlite", "DBI", "RSQLite",
  "dplyr", "lubridate", "readr", "purrr", "stringr",
  "glue", "xml2", "here"
))
```

### 2. Run First Scrape
```r
source("scrapers/main.R")

# Test with one company
jobs <- scrape_company("Applied Materials")

# Or scrape all companies
all_jobs <- scrape_all_companies()
```

## 📊 Database Schema

### Tables

**companies**
- company_id, company_name, careers_url, platform, active

**jobs**
- job_id, company_id, job_title, job_url, location
- job_responsibilities, min_education, min_experience
- preferred_qualifications, salary_range, posting_date

**scrape_runs**
- run_id, run_date, company_id, jobs_found, status, duration

### Views

**vw_jobs_full** - Jobs with company names joined  
**vw_scrape_stats** - Scraping statistics

## 🔧 Key Features

### 1. Workday Scraper (Fully Working)
- Handles pagination automatically
- Extracts job listings
- Fetches detailed job descriptions
- Parses sections: responsibilities, education, experience, qualifications
- Works for Applied Materials and NXP

### 2. Database Management
- Automatic schema creation
- Upsert logic (insert new, update existing)
- Tracks scraping runs and statistics
- Built-in views for easy querying

### 3. Error Handling
- Retry logic with exponential backoff
- Comprehensive logging
- Graceful degradation
- Status tracking

### 4. Rate Limiting
- 3-second delay between requests
- 10-second delay between companies
- Configurable timeouts
- Respects robots.txt

### 5. Export & Backup
- CSV exports per company and combined
- Timestamped filenames
- Database backup option
- 90-day artifact retention (GitHub)

## 🔄 Quarterly Automation

### GitHub Actions (Recommended)
- Runs automatically on Jan 1, Apr 1, Jul 1, Oct 1
- Can be manually triggered
- Commits results back to repo
- Creates quarterly releases
- Uploads artifacts

### Alternative Options
- Cron jobs (Linux/Mac)
- Windows Task Scheduler
- Manual execution

## 📈 Example Queries

```r
library(DBI)
con <- dbConnect(RSQLite::SQLite(), "data/semiconductor_jobs.db")

# Jobs by company
dbGetQuery(con, "
  SELECT company_name, COUNT(*) as jobs
  FROM vw_jobs_full
  GROUP BY company_name
")

# Recent PhD positions
dbGetQuery(con, "
  SELECT job_title, company_name, location
  FROM vw_jobs_full
  WHERE min_education LIKE '%PhD%'
    AND posting_date >= date('now', '-30 days')
")

# Top locations
dbGetQuery(con, "
  SELECT location, COUNT(*) as jobs
  FROM vw_jobs_full
  GROUP BY location
  ORDER BY jobs DESC
  LIMIT 10
")
```

## ⚙️ Customization

### Add New Companies
Edit `config/companies.csv`:
```csv
company_name,careers_url,platform,active
TSMC,https://careers.tsmc.com,custom,TRUE
```

### Adjust Delays
Edit `config/scraper_config.R`:
```r
delay_between_requests = 5   # Increase delay
max_pages_per_company = 100  # Scrape more pages
```

### Filter by Keywords
```r
# Already configured in scraper_config.R
SEMICONDUCTOR_KEYWORDS <- c(
  "semiconductor", "wafer", "fab", "lithography"
)
```

## ⚠️ Important Notes

### Legal Considerations
- ✅ Respects robots.txt
- ✅ Rate limited
- ✅ Proper user agent
- ✅ Only public data
- ⚠️ Review each company's ToS
- ⚠️ Read `docs/legal_considerations.md`

### Platform Status
- **Workday**: ✅ Fully working (Applied Materials, NXP)
- **Oracle CX**: Template ready (Texas Instruments)
- **Custom platforms**: Templates ready (Samsung, SkyWater, Tokyo Electron)

### Next Steps for Custom Platforms
1. Inspect website HTML structure
2. Update selectors in respective scraper files
3. Test with small scrapes first
4. Adjust as needed

## 🎓 What You've Learned

This project demonstrates:
- Web scraping with rvest
- Database design with SQLite
- Error handling and logging
- API pagination handling
- GitHub Actions automation
- Modular R code structure
- Data cleaning and normalization

## 📚 Documentation

- **README.md**: Complete project documentation
- **QUICKSTART.md**: 5-minute setup guide
- **legal_considerations.md**: Legal and ethical guidelines
- **Inline comments**: Throughout all code files

## 🔨 Recommended Workflow

1. **Week 1**: Test Workday scraper with Applied Materials
2. **Week 2**: Implement Oracle CX scraper for Texas Instruments
3. **Week 3**: Implement custom scrapers (Samsung, SkyWater, Tokyo Electron)
4. **Week 4**: Set up GitHub repository and automation
5. **Ongoing**: Run quarterly, analyze data, refine as needed

## 🎯 Success Metrics

After quarterly runs, you'll have:
- Complete job database for 6 semiconductor companies
- Historical trends (Q1 2026, Q2 2026, etc.)
- Exportable CSV files for analysis
- Searchable SQLite database
- Automated data collection pipeline

## 💡 Pro Tips

1. **Start small**: Test with one company first
2. **Monitor logs**: Check `logs/` for errors
3. **Version control**: Push to GitHub early
4. **Backup database**: Before major changes
5. **Review ToS**: For each company regularly
6. **Be patient**: Full scrapes can take 1-2 hours

## 🚦 Project Status

| Component | Status |
|-----------|--------|
| Project Structure | ✅ Complete |
| Database Schema | ✅ Complete |
| Workday Scraper | ✅ Complete |
| Oracle Scraper | 🟡 Template |
| Custom Scrapers | 🟡 Templates |
| Utilities | ✅ Complete |
| Documentation | ✅ Complete |
| GitHub Actions | ✅ Complete |
| Legal Docs | ✅ Complete |

## 📞 Support

- Review code comments for implementation details
- Check logs for debugging
- Refer to docs/ for guidance
- Adjust selectors as websites change

---

**You now have a professional-grade web scraping framework!** 

Start with the working Workday scraper, then expand to other platforms as needed. The modular structure makes it easy to add, test, and maintain each scraper independently.

Good luck with your semiconductor job market research! 🎉
