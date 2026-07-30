# Phase 2: Clean, combine, and filter the six raw THECB pulls to
# semiconductor-related CIP codes.
#
# ---- Findings from inspecting the raw data (informs everything below) -------
#
# CIP code format: THECB's CIPDesc column looks like
#   "11070100 - Computer Science"
# i.e. an 8-digit code + " - " + description. That 8-digit code is the
# standard federal 6-digit CIP code (as used in cip_codes.R, e.g. "11.0701")
# with two extra trailing zeros appended -- confirmed empirically: stripping
# the last 2 digits and inserting a decimal after the first 2 (e.g.
# "11070100" -> "11.0701") lines up with real federal CIP codes/descriptions
# ("Computer Science", "Computer Engineering, General", etc.). Of the 88
# codes in semiconductor_cip_codes, 64 have at least one matching row
# somewhere in the six files; the other 24 (e.g. several 40.xxxx Physical
# Science Technologies, several narrow 15.xxxx Engineering Technology codes)
# simply have no Texas public-institution offerings in this data -- that's
# real program-availability signal, not a scraping gap.
#
# A handful of rows (7 across univ_completions/univ_enrollment/cc_enrollment)
# have a CIPDesc with NO numeric code prefix at all, e.g. just
# "Cybersecurity Defense Strategy/Policy" -- a THECB data-quality gap, not a
# parsing bug (confirmed the raw export itself already lacks the code for
# these). None of them are semiconductor-relevant by name, and
# standardize_cip() below returns NA for them, which the semiconductor CIP
# filter naturally drops.
#
# Column shape differs by report type in a way that isn't a mistake:
#   - All three *_completions files share: DimYear, InstTypeList, InstList,
#     MajorTypeDesc, LevelGroupDesc, CIPGroupDesc, CIPDesc, Count.
#   - univ_enrollment has ClassificationDesc (Freshman/Sophomore/.../Doctors
#     Professional Practice) instead of MajorTypeDesc.
#   - cc_enrollment and tstc_enrollment have MajorTypeDesc (Academic/
#     Technical) instead of ClassificationDesc.
#   THECB tracks student classification for universities and academic-vs-
#   technical program type for two-year institutions -- these aren't the
#   same concept, so they're kept as two separate columns in
#   enrollment_long, NA where not applicable to that sector, rather than
#   forced into one column.
#
# Year format, institution naming, and Count are all consistent across the
# six files -- no additional cleanup needed there. Count is occasionally NA
# (<0.1% of rows in every file) -- almost certainly small-count privacy
# suppression rather than missing data, so it's left as NA (not coerced to
# 0) for downstream code to decide how to treat.

library(dplyr)
library(readr)
library(janitor)
library(purrr)
library(stringr)

source("cip_codes.R")

RAW_DIR <- "data-raw/downloads"

# ---- standardize CIP code: "11070100 - Computer Science" -> "11.0701" ------
standardize_cip <- function(cip_desc) {
  code8 <- str_trim(sub(" -.*$", "", cip_desc))
  valid <- grepl("^[0-9]{8}$", code8)
  dplyr::if_else(
    valid,
    paste0(substr(code8, 1, 2), ".", substr(code8, 3, 6)),
    NA_character_
  )
}

read_thecb_csv <- function(path) {
  read_csv(path, show_col_types = FALSE) |>
    clean_names()
}

# ---- Load raw files ---------------------------------------------------------
univ_completions <- read_thecb_csv(file.path(RAW_DIR, "univ_completions.csv"))
univ_enrollment  <- read_thecb_csv(file.path(RAW_DIR, "univ_enrollment.csv"))
cc_completions   <- read_thecb_csv(file.path(RAW_DIR, "cc_completions.csv"))
cc_enrollment    <- read_thecb_csv(file.path(RAW_DIR, "cc_enrollment.csv"))
tstc_completions <- read_thecb_csv(file.path(RAW_DIR, "tstc_completions.csv"))
tstc_enrollment  <- read_thecb_csv(file.path(RAW_DIR, "tstc_enrollment.csv"))

# ---- Shared cleanup + semiconductor CIP filter ------------------------------
clean_and_filter <- function(df, sector, metric_type) {
  df |>
    mutate(
      cip_code = standardize_cip(cip_desc),
      academic_year = as.integer(dim_year),
      institution_sector = sector,
      metric_type = metric_type
    ) |>
    filter(cip_code %in% semiconductor_cip_codes$cip_code) |>
    left_join(semiconductor_cip_codes, by = "cip_code") |>
    rename(
      institution_type = inst_type_list,
      institution_name = inst_list,
      cip_description = cip_desc
    )
}

completions_long <- bind_rows(
  clean_and_filter(univ_completions, "University", "Completions"),
  clean_and_filter(cc_completions,   "Community College", "Completions"),
  clean_and_filter(tstc_completions, "TSTC", "Completions")
) |>
  select(
    academic_year, institution_sector, institution_type, institution_name,
    major_type = major_type_desc, level_group = level_group_desc,
    cip_code, cip_family, cip_category, cip_group_desc, cip_description,
    count, metric_type
  )

enrollment_long <- bind_rows(
  clean_and_filter(univ_enrollment, "University", "Enrollment"),
  clean_and_filter(cc_enrollment,   "Community College", "Enrollment"),
  clean_and_filter(tstc_enrollment, "TSTC", "Enrollment")
) |>
  select(
    academic_year, institution_sector, institution_type, institution_name,
    semester = semester_desc,
    classification = classification_desc,   # University only; NA for CC/TSTC
    major_type = major_type_desc,           # CC/TSTC only; NA for University
    cip_code, cip_family, cip_category, cip_group_desc, cip_description,
    count, metric_type
  )

# ---- Sanity checks before saving --------------------------------------------
stopifnot(nrow(completions_long) > 0, nrow(enrollment_long) > 0)
stopifnot(!anyNA(completions_long$cip_code), !anyNA(enrollment_long$cip_code))

message("Completions rows by sector:")
print(count(completions_long, institution_sector))
message("Enrollment rows by sector:")
print(count(enrollment_long, institution_sector))

message("Distinct semiconductor CIP codes present -- completions: ",
        n_distinct(completions_long$cip_code),
        " / enrollment: ", n_distinct(enrollment_long$cip_code),
        " (of ", nrow(semiconductor_cip_codes), " in the target list)")

dir.create("data", showWarnings = FALSE)
saveRDS(enrollment_long, "data/enrollment.rds")
saveRDS(completions_long, "data/completions.rds")

message("Saved data/enrollment.rds and data/completions.rds")
