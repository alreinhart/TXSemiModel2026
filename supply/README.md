# Supply: THECB enrollment and completions

Pulls six reports (universities, community colleges, TSTC × completions and enrollment) from the THECB Accountability System Interactive Reports and filters them to semiconductor-related CIP codes (`cip_codes.R`).

| Step | Script | Output |
|---|---|---|
| 1. Pull | `01_scrape_thecb_data.R` (headless Chrome via `chromote`) | `data-raw/downloads/*.csv` (gitignored) |
| 2. Clean and combine | `02_clean_combine_data.R` | `data/completions.rds`, `data/enrollment.rds` |

Run from this folder (open `supply.Rproj`, or `setwd("supply")`).

## Access note

The Accountability site's `robots.txt` disallows generic crawlers. The Interactive Reports tool exists so the public can build and export these reports, and the script does what a person would do by hand, in small batches with a 5-second delay. Keep it to occasional one-time historical pulls, not a scheduled scraper. For routine refreshes, export the six reports by hand from the site and save them under the same file names.

The raw CSVs feed the analysis pipeline in the TexasSemiWFDModeling repository (`txlmi ingest manual thecb_accountability`), which records each file's hash and vintage.
