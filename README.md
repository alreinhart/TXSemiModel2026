# TXSemiModel

Texas semiconductor workforce supply & demand dashboard.

Combines two data pipelines into one Shiny dashboard (`app.R`, at the repo root):

- **`supply/`** — historical enrollment and completions data for semiconductor-relevant
  programs at Texas public universities, community colleges, and TSTCs, pulled from the
  Texas Higher Education Coordinating Board (THECB).
- **`demand/`** — semiconductor job postings scraped from Texas-area employer career
  pages (Applied Materials, TI, Samsung, NXP, TSMC, and others), with quarterly
  automated runs via GitHub Actions.

See `CLAUDE.md` for the technical architecture of each half, and each subfolder's own
README/instructions file for how to run its pipeline.

## Running the dashboard

```r
shiny::runApp()   # from the repo root
```

Reads `supply/data/enrollment.rds` and `supply/data/completions.rds`. Job-postings
(demand-side) data is not yet wired into the dashboard itself — currently `app.R` covers
the supply side (enrollment/completions trends); the jobs data lives in
`demand/data/exports/`.

## Project layout

```
TXSemiModel/
├── app.R                        -- the dashboard (repo root, reads from supply/data/)
├── supply/                      -- labor supply: THECB enrollment & completions
│   ├── 01_scrape_thecb_data.R
│   ├── 02_clean_combine_data.R
│   ├── cip_codes.R
│   ├── data/                    -- cleaned .rds outputs (committed)
│   └── data-raw/downloads/      -- raw THECB pulls (gitignored, regenerate via 01_)
└── demand/                      -- labor demand: employer job postings
    ├── scrapers/
    ├── config/
    ├── data/                    -- SQLite DB + CSV exports (gitignored, see demand/.gitignore)
    └── .github/workflows/       -- quarterly automated scrape
```

`supply/` and `demand/` are each self-contained R projects (open `supply/supply.Rproj`
or run from within `demand/`, respectively) so their internal scripts' relative paths
resolve correctly regardless of where the parent repo lives.
