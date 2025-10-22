library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(data.table)

source("R/box-path.R")

# Roots
ECG_ROOT    <- file.path(BOX_PATH, "Firstbeat Bodyguard 3 data/Wave 1 data/1. Recruitment Pregnant Women")
EDA_ROOT    <- file.path(BOX_PATH, "eSense data/Wave 1 data/1. Recruitment Pregnant Women")
MIRAGE_ROOT <- file.path(BOX_PATH, "MIRAGE")

# ----------------------------
# User Input
# ----------------------------
cat("\n--- Valid PIDs Available from: 11th March, 2023 to 29th Feb, 2024 ---\n")
search.year.num  <- as.integer(readline("Enter year (YYYY): "))
search.month.num <- as.integer(readline("Enter month (1-12): "))
search.day.num   <- as.integer(readline("Enter day (1-31): "))

# Build a real date once; derive per-modality folder formats
search_date      <- lubridate::make_date(search.year.num, search.month.num, search.day.num)
ecg_date_folder  <- format(search_date, "%Y%m%d") # ECG uses YYYY-MM-DD
eda_date_folder  <- format(search_date, "%Y%m%d")   # EDA uses YYYYMMDD

ecg_date_path <- file.path(ECG_ROOT, ecg_date_folder)
eda_date_path <- file.path(EDA_ROOT, eda_date_folder)

cat("\nUsing date:", format(search_date, "%Y%m%d"),
    "\nECG path:", normalizePath(ecg_date_path, mustWork = FALSE),
    "\nEDA path:", normalizePath(eda_date_path,  mustWork = FALSE),
    "\nMIRAGE root:", normalizePath(MIRAGE_ROOT, mustWork = FALSE), "\n")

# ----------------------------
# Step 1: ECG PIDs for that date (5-digit dirs only)
# ----------------------------
if (dir.exists(ecg_date_path)) {
  ecg_entries <- list.files(ecg_date_path, all.files = FALSE, no.. = TRUE, recursive = FALSE)
  # keep only directories
  ecg_entries <- ecg_entries[dir.exists(file.path(ecg_date_path, ecg_entries))]
  # keep only 5-digit PIDs
  day_ecg_subfolders <- grep("^\\d{5}$", ecg_entries, value = TRUE)
} else {
  day_ecg_subfolders <- character(0)
}

# ----------------------------
# Step 2: EDA files for that date (CSV/XLS/XLSX)
# ----------------------------
if (dir.exists(eda_date_path)) {
  day_eda_excel_files <- list.files(
    eda_date_path,
    pattern   = "(?i)\\.csv$",
    full.names = TRUE,
    recursive  = FALSE
  )
} else {
  day_eda_excel_files <- character(0)
}

# ----------------------------
# Step 3: MIRAGE CSVs (scan all subfolders)
# ----------------------------
mirage_folders <- if (dir.exists(MIRAGE_ROOT)) {
  list.files(MIRAGE_ROOT, pattern = "(?i)\\.csv$", full.names = TRUE, recursive = TRUE)
} else character(0)

# ----------------------------
# Quick summary
# ----------------------------
cat("\n--------------- Header Scan Summary ---------------\n")
cat("ECG PID folders found: ", length(day_ecg_subfolders), "\n")
cat("EDA files found:       ", length(day_eda_excel_files), "\n")
cat("MIRAGE CSV files:      ", length(mirage_folders), "\n")
cat("----------------------------------------------------\n")
