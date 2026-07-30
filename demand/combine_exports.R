setwd("C:/Users/grege/OneDrive/Documents/TXSemiModel/semiconductor_jobs_scraper")
library(readr)
library(dplyr)

export_dir <- "data/exports"

# Get all per-company CSVs (exclude existing combined files)
all_files <- list.files(export_dir, pattern = "^jobs_.+\\.csv$", full.names = TRUE)
all_files <- all_files[!grepl("jobs_all", all_files)]

# For each file, extract the company slug and date so we can pick the latest per company
file_info <- data.frame(
  path = all_files,
  basename = basename(all_files),
  stringsAsFactors = FALSE
)
# Strip leading "jobs_" and trailing "_YYYYMMDD*.csv" to get company slug
file_info$slug <- sub("^jobs_(.+)_\\d{8}[^.]*\\.csv$", "\\1", file_info$basename)
file_info$date <- sub("^jobs_.+_(\\d{8})[^.]*\\.csv$", "\\1", file_info$basename)

# Normalize NXP slugs — treat "nxp" and "nxp_semiconductors" as the same company
file_info$slug <- sub("^nxp_semiconductors$", "nxp", file_info$slug)

# Keep only the most recent file per company slug
latest_files <- file_info %>%
  group_by(slug) %>%
  slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
  ungroup()

cat("Files to combine:\n")
print(latest_files[, c("slug", "date", "basename")])

# Read and combine — bind_rows fills missing columns with NA
# Coerce job_identification to character to avoid type conflicts across files
all_jobs <- bind_rows(lapply(latest_files$path, function(f) {
  df <- read_csv(f, show_col_types = FALSE)
  df <- mutate(df, across(any_of("job_identification"), as.character))
  cat("  Loaded", nrow(df), "rows from", basename(f), "\n")
  df
}))

cat("\nTotal rows:", nrow(all_jobs), "\n")
cat("Columns:", paste(names(all_jobs), collapse = ", "), "\n")
cat("Companies:\n")
print(sort(table(all_jobs$company_name), decreasing = TRUE))

# Export
out_file <- file.path(export_dir, paste0("jobs_all_companies_", format(Sys.Date(), "%Y%m%d"), ".csv"))
write_csv(all_jobs, out_file)
cat("\nExported to:", out_file, "\n")
