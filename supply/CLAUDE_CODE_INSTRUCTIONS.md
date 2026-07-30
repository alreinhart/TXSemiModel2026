# Instructions for Claude Code: TX Semiconductor Workforce Dashboard

Paste this whole file (or point Claude Code at it — `claude "follow the plan in CLAUDE_CODE_INSTRUCTIONS.md"`) as the working brief for this project. It has three phases: (1) pull six reports from the THECB Accountability System, (2) clean/combine/filter them to semiconductor-relevant CIP codes, (3) build the R Shiny dashboard.

Work through the phases **in order** and check in with me after each one — don't build the Shiny app before the data pipeline is validated.

---

## Before you start: one important caveat

The target site (`txhigheredaccountability.org`) returns a `robots.txt` disallow for generic crawlers. It's a public Texas state government data portal explicitly designed to let the public build and export custom reports (that's the entire purpose of the "Interactive Reports" tool), so a scripted browser session doing exactly what a human would do by hand is very likely fine — but confirm you're comfortable with that, and keep the automation to a handful of one-time pulls (not a scheduled/repeated scraper) rather than hammering the server. If in doubt, note it and I'll make the call.

---

## Phase 1 — Pull the six CSVs

The report builder at `https://www.txhigheredaccountability.org/AcctPublic/InteractiveReport/ManageReports` is a server-rendered ASP.NET Web Forms app (dropdowns trigger postbacks). It is **not** a simple query-string API, so this needs real browser automation, not `rvest`/`httr`.

**Use the `chromote` package** (drives headless Chrome via the DevTools Protocol) rather than `RSelenium` — no Java/Selenium server dependency, easier to debug from Claude Code.

```r
install.packages(c("chromote", "rvest", "dplyr", "purrr", "readr", "janitor", "stringr"))
```

### Step 1a — Explore first, don't guess selectors

THECB's exact HTML element IDs aren't knowable in advance (I couldn't inspect the live JS-rendered form). Have Claude Code do this exploration step **first** and print/report back what it finds before writing the full automation loop:

```r
library(chromote)

b <- ChromoteSession$new()
b$Page$navigate("https://www.txhigheredaccountability.org/AcctPublic/InteractiveReport/ManageReports")
b$Page$loadEventFired()

# Find and click "Create a Report"
b$Runtime$evaluate('document.querySelector("a[href*=\'AddReport\']")?.click() ||
                     [...document.querySelectorAll("a,button,input")]
                       .find(el => /create a report/i.test(el.textContent||el.value||""))?.click()')
Sys.sleep(2)

# Dump every select/input/label on the resulting page so we can identify:
#  - Institution Type dropdown
#  - Institution multi-select / "select all" control
#  - "What data would you like to see?" dropdown
#  - View Report button
#  - Create CSV button
b$Runtime$evaluate('
  JSON.stringify([...document.querySelectorAll("select,input,button")].map(el => ({
    tag: el.tagName, id: el.id, name: el.name, type: el.type, text: el.textContent?.trim()
  })))
')$result$value
```

Use that output to map each field to a CSS/ID selector. Save the mapping as comments at the top of `R/01_scrape_thecb_data.R`.

### Step 1b — Automate the six pulls

For each of these six combinations, the sequence is: set Institution Type → set Institution to "select all" → set report type → click **View Report** → click **Create CSV** → capture the downloaded file → rename it descriptively → return to the report builder for the next combo.

| # | Institution Type | Report | Save as |
|---|---|---|---|
| 1 | Public Universities | Degrees and Certificates Awarded by Curriculum Area | `univ_completions.csv` |
| 2 | Public Universities | Enrollment by Curriculum Area | `univ_enrollment.csv` |
| 3 | Community Colleges | Degrees and Certificates Awarded by Curriculum Area | `cc_completions.csv` |
| 4 | Community Colleges | Enrollment by Curriculum Area | `cc_enrollment.csv` |
| 5 | Texas State Technical Colleges | Degrees and Certificates Awarded by Curriculum Area | `tstc_completions.csv` |
| 6 | Texas State Technical Colleges | Enrollment by Curriculum Area | `tstc_enrollment.csv` |

Save all six raw files to `data-raw/downloads/`. To catch the CSV download with `chromote`, set the download behavior before clicking "Create CSV":

```r
b$Browser$setDownloadBehavior(
  behavior = "allow",
  downloadPath = normalizePath("data-raw/downloads")
)
```

A starter script with this structure (selectors left as `TODO` for Claude Code to fill in from Step 1a) is in `R/01_scrape_thecb_data.R`.

**Validate before moving on:** open each CSV and confirm it has a CIP code column, an institution column, a year column, and the metric (headcount or awards). Show me the `head()` of each before Phase 2.

---

## Phase 2 — Clean, combine, and filter to semiconductor-relevant CIP codes

The full list of semiconductor-related CIP codes to keep is in `R/cip_codes.R` as `semiconductor_cip_codes` (97 codes, with a rough broad-category tag you should review and adjust once you see how THECB labels "Curriculum Area" — their categories may already roughly match CIP 2-digit families, in which case prefer THECB's own labels over my guessed categories).

`R/02_clean_combine_data.R` should:

1. Read all six raw CSVs with `janitor::clean_names()`.
2. Standardize the CIP code column to a consistent character format (e.g. `"11.0701"`, zero-padded, as character — **not numeric**, since numeric will drop trailing zeros like `15.0000`).
3. Filter each to rows where CIP code is in `semiconductor_cip_codes$cip_code`.
4. Add columns: `institution_sector` (University / Community College / TSTC — from which pull it came) and `metric_type` (Enrollment / Completions — from which pull it came).
5. Reshape/union into two tidy long-format tables: `enrollment_long` and `completions_long`, each with consistent columns across sectors (institution name, sector, CIP code, curriculum area label, academic year, headcount/awards, plus any demographic breakdowns THECB includes).
6. Join in the CIP category tag from `cip_codes.R`.
7. Save both as `data/enrollment.rds` and `data/completions.rds` (RDS loads faster than CSV for the Shiny app and preserves types).

Flag anything odd — e.g. inconsistent year formats across the three institution types, or CIP codes present in the raw data with formatting THECB truncates differently — before Phase 3.

---

## Phase 3 — Build the R Shiny dashboard

Once `data/enrollment.rds` and `data/completions.rds` are validated, build `app.R` (skeleton provided) with:

- **Audience**: workforce/education policymakers and engineering-adjacent technical staff — favor clear labeled axes, exportable tables, and CIP code visible alongside plain-language program names, not jargon-heavy UI chrome.
- **Filters**: institution sector (University / CC / TSTC / All), academic year range, CIP category, individual institution (optional drill-down).
- **Tabs**:
  1. *Enrollment trends* — line/area chart over time, by sector and/or CIP category, using `plotly` for hover detail.
  2. *Completions trends* — same structure for degrees/certificates awarded.
  3. *Enrollment vs. completions* — a combined view (e.g. completions as % of enrollment by CIP area) to surface pipeline throughput, which is the metric policymakers usually ask about.
  4. *Data table* — filtered `DT::datatable` with CSV download button (`DT` + `downloadHandler`), since your audience will want to pull numbers into their own memos.
- Use `bslib` for a clean theme (`bs_theme(version = 5)`) rather than default Shiny styling.
- Load the two `.rds` files once at the top of `app.R`, not inside `server()`, so they aren't re-read per session.

---

## File structure this brief assumes

```
semiconductor_dashboard/
├── CLAUDE_CODE_INSTRUCTIONS.md
├── R/
│   ├── cip_codes.R
│   ├── 01_scrape_thecb_data.R
│   └── 02_clean_combine_data.R
├── data-raw/
│   └── downloads/        # raw CSVs land here
├── data/
│   ├── enrollment.rds     # created in Phase 2
│   └── completions.rds    # created in Phase 2
└── app.R
```
