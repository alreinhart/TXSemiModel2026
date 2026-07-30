# Phase 1: Pull six reports (by institution-type x report-type) from the THECB
# Interactive Reports tool.
#
# ---- Step 1a findings (selector map) ----------------------------------------
# The form is jQuery/AJAX-driven (bootstrap-multiselect + $.get/$.post to JSON
# endpoints), NOT classic ASP.NET postback. Confirmed via live DOM exploration
# on https://www.txhigheredaccountability.org/AcctPublic/InteractiveReport/AddReport:
#
#   #InstTypeID   - Institution Type <select>. Setting .value + dispatching a
#                   "change" event fires 3 async AJAX calls that populate
#                   #InstID, #FactTableId, #CurrentYearID. Allow ~3s.
#   #InstID       - Institution <select multiple>, wrapped by a bootstrap-
#                   multiselect widget. A "Select all institutions" pseudo-
#                   option (value="-98") lives INSIDE the select, not as a
#                   separate button. Selecting a real institution option +
#                   firing "change" invokes the page's own InstChange(), which
#                   re-fires the Fact Table AJAX call. That second call is slow
#                   (~5-10s).
#   #FactTableId  - Report type <select>, populated after #InstID's AJAX
#                   resolves. Confirmed values: 181 = "Degrees and
#                   Certificates Awarded by Curriculum Area", 178 =
#                   "Enrollment by Curriculum Area". Selecting a value fires
#                   loadDimensionList() (~5-6s) which injects further
#                   dimension multiselects (#Year, #Semester,
#                   #Classification, ...) under #dimensionDiv -- these default
#                   to ALL values selected, so no separate year-looping is
#                   needed. The form's submit handler validates against
#                   #Year's presence (via checkDimYearListFilter()), so we
#                   must wait for #dimensionDiv to populate before submitting.
#   #CurrentYearID- A single-select "Year" field that also exists outside the
#                   dimension system. It's a required field but does not
#                   appear to restrict the underlying report grain (that's
#                   controlled by the #Year dimension, which defaults to all
#                   years). We just set it to any valid value to satisfy
#                   required-field validation.
#   #previewReport- "View Report" <button type="submit">. Triggers a
#                   $.post to /AcctPublic/InteractiveReport/SubmitReport.
#   a[href="/AcctPublic/InteractiveReport/ExportCsv"]
#                 - "Create CSV" link. NOT AJAX-driven -- a plain GET that
#                   relies on server-side session state set by the View
#                   Report submit. Must click AFTER a successful submit.
#
# ---- Scale finding that reshaped Step 1b -------------------------------------
# The report grain is one row per (year x semester x classification x CIP
# code x institution). Submitting with ALL ~41 Public Universities selected
# at once returned HTTP 500 after ~18s -- the combined query is too large for
# the server to compute in one request. A single institution succeeded (200,
# ~2s). A batch of 5 institutions also succeeded (200, ~6s, ~93k rows for
# Enrollment alone). So Step 1b below pulls institutions in small batches per
# report and relies on the CSV export reflecting whatever combination is
# currently "live" in the report session -- batches are exported and
# concatenated into six final files matching the brief's naming.
#
# This means far more than "6 requests" happen against the server (roughly
# 40-50 total across all institution-type x report-type combinations, since
# larger institution types need multiple batches). This is still a one-time,
# non-repeated historical pull (matches the ethical caveat in
# CLAUDE_CODE_INSTRUCTIONS.md), just spread across more, smaller requests
# rather than one huge one. A generous delay is kept between requests.

library(chromote)
library(purrr)

options(chromote.timeout = 60)  # default is too short for this server's slower AJAX calls

DOWNLOAD_DIR <- normalizePath("data-raw/downloads", mustWork = FALSE)
dir.create(DOWNLOAD_DIR, showWarnings = FALSE, recursive = TRUE)

BASE_URL   <- "https://www.txhigheredaccountability.org/AcctPublic/InteractiveReport"
ADDREPORT_URL <- paste0(BASE_URL, "/AddReport")

REQUEST_DELAY_SEC <- 5   # courtesy delay between report submissions
BATCH_SIZE <- 5          # institutions per batch; drop lower if a batch still 500s

# ---- Low-level helpers -------------------------------------------------------

js_escape <- function(x) gsub('"', '\\\\"', x)

set_select_by_regex <- function(session, selector, pattern) {
  js <- sprintf('
    (function() {
      const el = document.querySelector("%s");
      const opt = [...el.options].find(o => /%s/i.test(o.textContent));
      if (!opt) return "NOT FOUND";
      el.value = opt.value;
      el.dispatchEvent(new Event("change", { bubbles: true }));
      return "OK: " + opt.textContent;
    })()
  ', selector, pattern)
  session$Runtime$evaluate(js)$result$value
}

get_institution_options <- function(session) {
  # #InstID's <option> list mixes real institutions with aggregate "group"
  # rows (system names like "Texas A&M University System", and tier rollups
  # like "Doctoral"/"Comprehensive"). Selecting those returns aggregate data,
  # not per-institution data, and corrupted the first live run. The page's
  # own InstIDObject JS variable (set by loadInstList()'s AJAX callback) has
  # an IsGroup flag we can filter on to get only real institutions.
  js <- '
    JSON.stringify(
      (window.InstIDObject || [])
        .filter(o => o.IsGroup === false)
        .map(o => ({value: String(o.Value), text: o.Text}))
    )
  '
  jsonlite_txt <- session$Runtime$evaluate(js)$result$value
  jsonlite::fromJSON(jsonlite_txt)
}

select_institution_batch <- function(session, values) {
  ids_js <- paste0('[', paste(sprintf('"%s"', values), collapse = ","), ']')
  js <- sprintf('
    (function() {
      const el = document.querySelector("#InstID");
      [...el.options].forEach(o => { o.selected = false; });
      const ids = %s;
      let found = [];
      ids.forEach(id => {
        const opt = el.querySelector(\'option[value="\' + id + \'"]\');
        if (opt) { opt.selected = true; found.push(opt.textContent); }
      });
      el.dispatchEvent(new Event("change", { bubbles: true }));
      return found.join(", ");
    })()
  ', ids_js)
  session$Runtime$evaluate(js)$result$value
}

# Poll until a predicate JS expression returns a truthy/non-empty value, or
# timeout. Used because the site's AJAX calls have highly variable latency
# (single institution ~2s, five institutions ~6s, all institutions can hang
# for 18s+ before failing) -- fixed Sys.sleep() calls are not reliable here.
poll_until <- function(session, js_check, timeout_sec = 40, interval_sec = 2) {
  elapsed <- 0
  repeat {
    val <- session$Runtime$evaluate(js_check)$result$value
    if (!is.null(val) && !identical(val, FALSE) && !identical(val, 0) && !identical(val, "")) {
      return(val)
    }
    if (elapsed >= timeout_sec) return(NULL)
    Sys.sleep(interval_sec)
    elapsed <- elapsed + interval_sec
  }
}

# ---- Report-building steps for one (institution type, institution batch,
# ---- report type) combination -------------------------------------------

navigate_fresh <- function(session, max_attempts = 3) {
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch({
      session$Page$navigate(ADDREPORT_URL)
      session$Page$loadEventFired()
      Sys.sleep(2)
      TRUE
    }, error = function(e) {
      message("    navigate_fresh attempt ", attempt, " failed: ", conditionMessage(e))
      FALSE
    })
    if (isTRUE(result)) return(invisible(TRUE))
    Sys.sleep(5)
  }
  stop("navigate_fresh failed after ", max_attempts, " attempts")
}

set_institution_type <- function(session, inst_type_text) {
  res <- set_select_by_regex(session, "#InstTypeID", js_escape(inst_type_text))
  message("  Institution type: ", res)
  # Wait for #InstID to be populated by the resulting AJAX call.
  poll_until(session, 'document.querySelectorAll("#InstID option").length > 1', timeout_sec = 20)
}

set_report_type <- function(session, report_text) {
  res <- set_select_by_regex(session, "#FactTableId", js_escape(report_text))
  message("  Report type: ", res)
  # Wait for the dimension filters (#Year etc.) to be injected -- required
  # for the submit handler's validation to pass.
  poll_until(session, 'document.querySelector("#Year") !== null', timeout_sec = 20)
  # #CurrentYearID is a required field independent of the dimension system;
  # just give it a value (any valid one) to satisfy validation.
  set_select_by_regex(session, "#CurrentYearID", "current")
}

submit_and_wait <- function(session, timeout_sec = 40) {
  session$Runtime$evaluate('document.querySelector("#previewReport").click()')
  ok <- poll_until(
    session,
    'document.querySelectorAll("#reportGrid thead th").length > 0 ? "OK" : ""',
    timeout_sec = timeout_sec
  )
  !is.null(ok)
}

download_csv <- function(session, timeout_sec = 30) {
  before <- list.files(DOWNLOAD_DIR)
  session$Runtime$evaluate(
    'document.querySelector(\'a[href="/AcctPublic/InteractiveReport/ExportCsv"]\').click()'
  )
  elapsed <- 0
  repeat {
    after <- list.files(DOWNLOAD_DIR)
    new_files <- setdiff(after, before)
    # Ignore partial/incomplete Chrome download artifacts.
    new_files <- new_files[!grepl("\\.crdownload$", new_files)]
    if (length(new_files) > 0) return(file.path(DOWNLOAD_DIR, new_files[1]))
    if (elapsed >= timeout_sec) return(NA_character_)
    Sys.sleep(1)
    elapsed <- elapsed + 1
  }
}

# Run a single batch end-to-end on a brand-new ChromoteSession (a fresh
# browser tab). Returns a data.frame, or NULL if the batch could not be
# completed on this session.
run_one_batch <- function(inst_type_text, report_text, batch_vals) {
  b <- ChromoteSession$new()
  on.exit(b$close(), add = TRUE)
  b$Browser$setDownloadBehavior(behavior = "allow", downloadPath = DOWNLOAD_DIR)

  navigate_fresh(b)
  set_institution_type(b, inst_type_text)
  sel_res <- select_institution_batch(b, batch_vals)
  message("    Selected: ", sel_res)

  # Wait for the (slower) post-selection Fact Table AJAX refresh before
  # picking the report type.
  poll_until(b, 'document.querySelectorAll("#FactTableId option").length > 1', timeout_sec = 25)
  set_report_type(b, report_text)

  ok <- submit_and_wait(b, timeout_sec = 40)
  if (!ok) {
    warning("    Batch did not return data (timeout or server error).")
    return(NULL)
  }

  csv_path <- download_csv(b)
  if (is.na(csv_path)) {
    warning("    CSV download did not complete.")
    return(NULL)
  }

  # The site's CSV export puts a one-column report-title line before the
  # real comma-separated header (e.g. "Enrollment by Curriculum Area,"
  # then "DimYear,InstTypeList,...,Count,"). Without skip=1, read_csv
  # guesses a 2-column shape from the title line and mangles every
  # subsequent row -- confirmed by inspecting the raw downloaded file.
  # The export is UTF-8 EXCEPT for a stray raw 0x92 byte (a Windows-1252
  # curly apostrophe, likely un-converted mojibake in THECB's own database)
  # in "Alamo CCD-St. Philip's College" -- confirmed by inspecting raw bytes.
  # That single invalid byte corrupts UTF-8 parsing of the whole file, but
  # the file is NOT globally Windows-1252 (forcing that locale garbles every
  # other multi-byte character), so patch just that byte to its correct
  # UTF-8 encoding (U+2019, E2 80 99) before parsing.
  raw_bytes <- readBin(csv_path, "raw", n = file.info(csv_path)$size)
  bad_byte <- as.raw(0x92)
  bad_idx <- which(raw_bytes == bad_byte)
  if (length(bad_idx) > 0) {
    fixed_bytes <- raw_bytes
    utf8_right_single_quote <- as.raw(c(0xe2, 0x80, 0x99))  # U+2019 in UTF-8
    for (idx in rev(bad_idx)) {
      fixed_bytes <- c(fixed_bytes[seq_len(idx - 1)],
                        utf8_right_single_quote,
                        fixed_bytes[(idx + 1):length(fixed_bytes)])
    }
    writeBin(fixed_bytes, csv_path)
  }
  df <- readr::read_csv(csv_path, skip = 1, show_col_types = FALSE)
  file.remove(csv_path)
  # Drop the trailing all-NA column produced by the export's trailing comma.
  df[, !grepl("^\\.\\.\\.", names(df)) | colSums(!is.na(df)) > 0]
}

# Pull one report (institution type x report type), batching institutions,
# and return a combined data.frame of all batches' CSVs.
#
# A long-lived ChromoteSession/browser reused across many navigations was
# observed to degrade and then fail *every* subsequent batch for the rest of
# a report (confirmed from a live run: once the shared Chrome process broke,
# navigate_fresh's retries never recovered it, silently dropping dozens of
# institutions). To avoid that, every batch gets its own fresh
# ChromoteSession (run_one_batch), and a batch that still fails gets retried
# with another fresh session up to batch_max_attempts times before being
# given up on.
pull_report_batched <- function(inst_type_text, report_text, batch_size = BATCH_SIZE,
                                 batch_max_attempts = 3) {
  b0 <- ChromoteSession$new()
  b0$Browser$setDownloadBehavior(behavior = "allow", downloadPath = DOWNLOAD_DIR)
  navigate_fresh(b0)
  set_institution_type(b0, inst_type_text)
  insts <- get_institution_options(b0)
  b0$close()
  message("  Found ", nrow(insts), " institutions for '", inst_type_text, "'")

  batches <- split(insts$value, ceiling(seq_along(insts$value) / batch_size))

  combined <- list()
  failed_batches <- integer(0)
  for (i in seq_along(batches)) {
    batch_vals <- batches[[i]]
    message("  Batch ", i, "/", length(batches), " (", length(batch_vals), " institutions)")

    batch_df <- NULL
    for (attempt in seq_len(batch_max_attempts)) {
      batch_df <- tryCatch(
        run_one_batch(inst_type_text, report_text, batch_vals),
        error = function(e) {
          message("    Batch ", i, " attempt ", attempt, " errored: ", conditionMessage(e))
          NULL
        }
      )
      if (!is.null(batch_df)) break
      Sys.sleep(REQUEST_DELAY_SEC)
    }

    if (!is.null(batch_df)) {
      combined[[length(combined) + 1]] <- batch_df
    } else {
      warning("    Batch ", i, " for '", inst_type_text, "' / '", report_text,
              "' failed after ", batch_max_attempts, " attempts -- skipping.")
      failed_batches <- c(failed_batches, i)
    }
    Sys.sleep(REQUEST_DELAY_SEC)
  }

  if (length(failed_batches) > 0) {
    message("  WARNING: ", length(failed_batches), "/", length(batches),
            " batches failed for '", inst_type_text, "' / '", report_text, "': batches ",
            paste(failed_batches, collapse = ", "))
  }

  if (length(combined) == 0) {
    warning("No batches succeeded for '", inst_type_text, "' / '", report_text, "'")
    return(NULL)
  }
  dplyr::bind_rows(combined)
}

# ---- The six pulls ------------------------------------------------------------

pulls <- list(
  list(inst_type = "Public Universities",          report = "Degrees and Certificates Awarded by Curriculum Area", file = "univ_completions.csv"),
  list(inst_type = "Public Universities",          report = "Enrollment by Curriculum Area",                       file = "univ_enrollment.csv"),
  list(inst_type = "Community Colleges",            report = "Degrees and Certificates Awarded by Curriculum Area", file = "cc_completions.csv"),
  list(inst_type = "Community Colleges",            report = "Enrollment by Curriculum Area",                       file = "cc_enrollment.csv"),
  list(inst_type = "Texas State Technical Colleges", report = "Degrees and Certificates Awarded by Curriculum Area", file = "tstc_completions.csv"),
  list(inst_type = "Texas State Technical Colleges", report = "Enrollment by Curriculum Area",                       file = "tstc_enrollment.csv")
)

run_all_pulls <- function() {
  for (p in pulls) {
    out_path <- file.path(DOWNLOAD_DIR, p$file)
    if (file.exists(out_path)) {
      message("=== Skipping (already exists): ", p$inst_type, " / ", p$report, " ===")
      next
    }
    message("=== Pulling: ", p$inst_type, " / ", p$report, " ===")
    df <- tryCatch(
      pull_report_batched(p$inst_type, p$report),
      error = function(e) {
        message("  Report-level error, skipping this report entirely: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(df)) {
      readr::write_csv(df, out_path)
      message("  Saved ", nrow(df), " rows to ", out_path)
    }
  }
  message("Done. Verify all six files landed in data-raw/downloads/ before Phase 2.")
}

# Run interactively once selectors/timings above have been spot-checked:
# run_all_pulls()
