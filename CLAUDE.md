# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repo has two halves, feeding a semiconductor workforce supply/demand dashboard (`app.R`, repo root):

- **Labor demand** (`demand/`): R-based web scraping framework for collecting semiconductor job postings from company career pages. Targets Texas-area semiconductor companies (Applied Materials, NXP, TI, Samsung, SkyWater, TSMC, GlobalFoundries, Cadence, Synopsys, AMD, Intel, Lam Research, Entegris, Micron, Keysight, Boeing). Uses SQLite for storage, with quarterly automated runs via GitHub Actions.
- **Labor supply** (`supply/`): pulls historical enrollment and completions data for semiconductor-relevant programs from the Texas Higher Education Coordinating Board (THECB).

`supply/` and `demand/` are each self-contained R projects — their internal scripts use relative paths (supply, via plain relative paths) or `here::here()` anchored to their own `.here`/`.Rproj` marker (demand), so each resolves correctly as long as the R session's working directory is inside that subfolder when its scripts run. Don't `source()` a `demand/scrapers/*.R` file from a session whose working directory is the repo root — open `demand/` as its own project (or `setwd("demand")`) first. Same for `supply/` — open `supply/supply.Rproj` or `setwd("supply")` before running `01_scrape_thecb_data.R` / `02_clean_combine_data.R`.

## Running the demand-side scraper

```bash
# All companies
Rscript demand/scrapers/main.R all

# Single company
Rscript demand/scrapers/main.R "Applied Materials"
```

From R/RStudio (working directory must be `demand/`):
```r
source("scrapers/main.R")
scrape_company("Applied Materials")   # single company
scrape_all_companies()                # all companies
```

## Installing Dependencies

```r
install.packages(c("rvest", "httr", "jsonlite", "DBI", "RSQLite",
                    "dplyr", "lubridate", "readr", "purrr", "stringr",
                    "glue", "xml2", "here", "chromote", "janitor",
                    "shiny", "bslib", "plotly", "DT"))
```

## Demand-side architecture

All scraper code lives under `demand/`.

**Execution flow:** `scrapers/main.R` orchestrates everything — initializes the SQLite DB, loads companies from `config/companies.csv`, dispatches to platform-specific scrapers based on each company's `platform` field, saves results, and exports CSVs.

**Platform scrapers** are routed by the `platform` column in `companies.csv`:
- `workday` → `scrapers/workday_scraper.R` (production-ready, handles pagination)
- `oracle` → `scrapers/oracle_scraper.R`
- `successfactors` → `scrapers/successfactors_scraper.R`
- `talentbrew` → `scrapers/talentbrew_scraper.R`
- `icims` → `scrapers/icims_scraper.R`
- `dayforce` → `scrapers/dayforce_scraper.R`
- `boeing` → `scrapers/boeing_scraper.R` (Boeing-specific)

**Key files:**
- `config/scraper_config.R` — all tunable parameters: rate limits, timeouts, CSS selectors, user-agent. Sets `PROJECT_ROOT <- here::here()`, which resolves to `demand/` via its `.here` marker file.
- `config/db_schema.sql` — SQLite schema with 3 tables (`companies`, `jobs`, `scrape_runs`) and 2 views
- `scrapers/utils.R` — shared utilities: logging, HTTP fetch with retry/backoff, robots.txt checking, date parsing, text sanitization
- `scrapers/main.R` — orchestration, DB init, upsert logic, CSV export

**Data outputs (gitignored — see `demand/.gitignore`):**
- SQLite DB: `demand/data/semiconductor_jobs.db`
- CSV exports: `demand/data/exports/jobs_<company>_YYYYMMDD.csv`
- Logs: `demand/logs/scrape_YYYYMMDD.log`

## Database

Jobs are deduplicated by `job_url` uniqueness constraint. The `save_jobs_to_db()` function in `main.R` implements upsert logic, using `COALESCE` on detail fields so a failed re-fetch (NULL) doesn't overwrite previously-good data. Two convenience views exist: `vw_jobs_full` (jobs joined with company names) and `vw_scrape_stats` (scraping run statistics).

## Adding a New Company

1. Add a row to `demand/config/companies.csv` with the company name, careers URL, and platform type
2. If the platform type already has a scraper, it works automatically
3. For a new platform, create a new scraper file following the pattern in `workday_scraper.R` and add routing in `scrape_company()` in `main.R`

## Rate Limiting & Ethics

The scraper respects robots.txt (checked via `check_robots_txt()` in `utils.R`) and enforces configurable delays between requests (default 3s) and between companies (default 10s). These are set in `config/scraper_config.R`.

## CI/CD

GitHub Actions workflow at `demand/.github/workflows/quarterly_scrape.yml` runs quarterly (Jan/Apr/Jul/Oct 1st at 2AM UTC). Supports manual dispatch with company selection. Auto-commits results and creates quarterly releases.

## Labor supply pipeline (THECB enrollment/completions)

Full brief: `supply/CLAUDE_CODE_INSTRUCTIONS.md`. Three phases, run in order (working directory must be `supply/`):

1. **`01_scrape_thecb_data.R`** — pulls six raw reports (University/Community College/TSTC × Enrollment/Completions) from THECB's Interactive Reports tool (`txhigheredaccountability.org/AcctPublic/InteractiveReport/AddReport`) using `chromote` (real headless Chrome — the site is jQuery/AJAX-driven, not a simple form). Institutions are pulled in small batches (`BATCH_SIZE`) because the server 500s on large combined queries. Selector map and empirical findings are documented at the top of the file. Saves to `supply/data-raw/downloads/*.csv` (gitignored — regenerate via this script rather than expecting them in the repo).
2. **`02_clean_combine_data.R`** — standardizes CIP codes, filters to `semiconductor_cip_codes` (defined in `cip_codes.R`), reshapes to two tidy long tables, saves `data/enrollment.rds` and `data/completions.rds` relative to `supply/` (these ARE committed — small, and expensive to regenerate).
3. **`app.R`** (repo root, not under `supply/`) — 4-tab Shiny dashboard (enrollment trends, completions trends, enrollment-vs-completions pipeline rate, filterable/downloadable data table). Reads `supply/data/enrollment.rds` and `supply/data/completions.rds` at top level, not per-session. Uses `bslib`/`plotly`/`DT`.

**Data grain:** one row per (year × semester × classification/major-type × CIP code × institution). The `count` column holds headcount (enrollment) or awards (completions) depending on `metric_type` — the two tables share this column name.

**Re-running the scrape:** `01_scrape_thecb_data.R` is written for a one-time historical pull, not a repeated/scheduled scraper (see the ethical caveat in `supply/CLAUDE_CODE_INSTRUCTIONS.md`) — re-run manually when fresh data is needed, don't wire it into CI without reconsidering that.
