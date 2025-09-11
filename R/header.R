library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)

# Define base Box path
BOX_PATH <- "C:/Users/hshaik00/Box/Bangladesh Study/Data"

# Build sub-paths for each data type
ECG_ROOT    <- file.path(BOX_PATH, "Firstbeat Bodyguard 3 data/Wave 1 data/1. Recruitment Pregnant Women")
EDA_ROOT    <- file.path(BOX_PATH, "eSense data/Wave 1 data/1. Recruitment Pregnant Women")
MIRAGE_ROOT <- file.path(BOX_PATH, "MIRAGE")

cat("--- Valid PIDs Available from: 11th March, 2023 to 29th Feb, 2024 ---", "\n")
# Example inputs
search.year.num  <- as.integer(readline(prompt = "Enter year (YYYY): "))
search.month.num <- as.integer(readline(prompt = "Enter month (1-12): "))
search.day.num   <- as.integer(readline(prompt = "Enter day (1-31): "))

# Always format month/day with 2 digits (03, 04, …)
month_str <- sprintf("%02d", search.month.num)
day_str   <- sprintf("%02d", search.day.num)

# Validate day for the given month/year
max_day <- lubridate::days_in_month(lubridate::ymd(paste0(search.year.num, "-", month_str, "-01")))
if (is.na(search.day.num) || search.day.num < 1 || search.day.num > max_day) {
  stop(sprintf("Day must be between 1 and %d for %04d-%s.", max_day, search.year.num, month_str))
}

# Create folder prefix YYYYMM and cutoff YYYYMMDD
folder_prefix <- paste0(search.year.num, month_str)
cutoff_date   <- lubridate::ymd(paste0(search.year.num, month_str, day_str))

# List immediate subfolders
all_ecg_subfolders <- list.dirs(ECG_ROOT, full.names = FALSE, recursive = FALSE)
all_eda_subfolders <- list.dirs(EDA_ROOT, full.names = FALSE, recursive = FALSE)

# Helper to pull a leading YYYYMMDD from folder name -> Date
extract_folder_date <- function(x) {
  m <- regexpr("^\\d{8}", x)
  if (m == -1) return(as.Date(NA))
  d <- substr(x, m, m + attr(m, "match.length") - 1)
  suppressWarnings(lubridate::ymd(d))
}

# Filter to this month, then keep only folders AFTER cutoff date (strict >)
month_ecg_subfolders <- all_ecg_subfolders[grepl(paste0("^", folder_prefix), all_ecg_subfolders)]
if (length(month_ecg_subfolders)) {
  ecg_dates  <- vapply(month_ecg_subfolders, extract_folder_date, as.Date(NA))
  keep_idx   <- which(!is.na(ecg_dates) & ecg_dates >= cutoff_date)   # use >= for inclusive
  if (length(keep_idx)) {
    ord <- order(ecg_dates[keep_idx], month_ecg_subfolders[keep_idx])
    month_ecg_subfolders <- month_ecg_subfolders[keep_idx[ord]]
  } else {
    month_ecg_subfolders <- character(0)
  }
}

month_eda_subfolders <- all_eda_subfolders[grepl(paste0("^", folder_prefix), all_eda_subfolders)]
if (length(month_eda_subfolders)) {
  eda_dates  <- vapply(month_eda_subfolders, extract_folder_date, as.Date(NA))
  keep_idx   <- which(!is.na(eda_dates) & eda_dates >= cutoff_date)   # use >= for inclusive
  if (length(keep_idx)) {
    ord <- order(eda_dates[keep_idx], month_eda_subfolders[keep_idx])
    month_eda_subfolders <- month_eda_subfolders[keep_idx[ord]]
  } else {
    month_eda_subfolders <- character(0)
  }
}

# Get all CSV files from MIRAGE_ROOT (top-level only)
mirage_folders <- list.files(
  MIRAGE_ROOT,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = FALSE
)
