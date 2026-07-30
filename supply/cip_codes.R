# Semiconductor-related CIP codes used to filter the THECB enrollment/completions pulls.
#
# IMPORTANT: keep cip_code as character, not numeric — codes like "15.0000" and
# trailing-zero codes ("14.1099") will silently corrupt if read/stored as numeric.

semiconductor_cip_codes_raw <- c(
  "11.0701","14.0901","14.1001","15.1501","52.0205","11.0201","14.1099","14.3501",
  "14.4101","15.1201","15.1202","15.1503","11.0101","11.0103","11.0202","14.0902",
  "14.0903","14.1901","14.2701","14.3601","14.4701","15.0001","15.0303","15.0306",
  "15.0616","15.1204","30.0801","30.7101","40.1001","47.0303","48.0508","52.0201",
  "52.0216","52.0304","52.1201","11.0102","11.0104","11.0501","11.0902","11.1005",
  "14.0101","14.0701","14.0799","14.0999","14.1003","14.1101","14.1201","14.1801",
  "14.2001","14.3701","14.4201","15.0304","15.0399","15.0403","15.0406","15.0612",
  "15.0613","15.0614","15.0615","15.0699","15.0702","15.0703","15.0705","15.1203",
  "15.1299","15.1301","15.1302","15.1305","15.1307","15.1502","30.3001","30.7001",
  "30.7104","40.1002","40.1099","47.0105","48.0501","48.0503","52.0203","52.0301",
  "52.0303","52.0305","52.0409","52.0801","52.1206","52.1299","52.1301","15.0000"
)

# Rough broad-category tag based on CIP 2-digit family, as a starting point for
# dashboard grouping/filtering. Review against how THECB itself labels "Curriculum
# Area" in the raw pulls — if THECB's own categories are more useful to your
# audience, prefer those and treat this column as a secondary/backup grouping.
cip_family_labels <- c(
  "11" = "Computer & Information Sciences",
  "14" = "Engineering",
  "15" = "Engineering/Engineering-Related Technologies",
  "30" = "Multi/Interdisciplinary Studies",
  "40" = "Physical Sciences",
  "47" = "Mechanic & Repair Technologies",
  "48" = "Precision Production",
  "52" = "Business, Management & Marketing"
)

semiconductor_cip_codes <- data.frame(
  cip_code = semiconductor_cip_codes_raw,
  cip_family = substr(semiconductor_cip_codes_raw, 1, 2),
  stringsAsFactors = FALSE
)
semiconductor_cip_codes$cip_category <- cip_family_labels[semiconductor_cip_codes$cip_family]

# Sanity check
stopifnot(!anyNA(semiconductor_cip_codes$cip_category))
stopifnot(length(semiconductor_cip_codes_raw) == length(unique(semiconductor_cip_codes_raw)))
