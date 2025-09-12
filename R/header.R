library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)

# ----------------------------
# Define base Box path
# ----------------------------
BOX_PATH <- "C:/Users/hshaik00/Box/Bangladesh Study/Data"

# Build sub-paths for each data type
ECG_ROOT    <- file.path(BOX_PATH, "Firstbeat Bodyguard 3 data/Wave 1 data/1. Recruitment Pregnant Women")
EDA_ROOT    <- file.path(BOX_PATH, "eSense data/Wave 1 data/1. Recruitment Pregnant Women")
MIRAGE_ROOT <- file.path(BOX_PATH, "MIRAGE")

# ----------------------------
# User Input
# ----------------------------
cat("\n--- Valid PIDs Available from: 11th March, 2023 to 29th Feb, 2024 ---", "\n")
search.year.num  <- as.integer(readline(prompt = "Enter year (YYYY): "))
search.month.num <- as.integer(readline(prompt = "Enter month (1-12): "))
search.day.num   <- as.integer(readline(prompt = "Enter day (1-31): "))

# Format YYYYMMDD
date_folder <- paste0(
  search.year.num,
  sprintf("%02d", search.month.num),
  sprintf("%02d", search.day.num)
)

# ----------------------------
# Step 1: ECG PIDs for that date
# ----------------------------
ecg_date_path <- file.path(ECG_ROOT, date_folder)
if (dir.exists(ecg_date_path)) {
  day_ecg_subfolders <- list.files(ecg_date_path, full.names = FALSE, recursive = FALSE)
} else {
  day_ecg_subfolders <- character(0)
}

# ----------------------------
# Step 2: EDA Excel files for that date
# ----------------------------
eda_date_path <- file.path(EDA_ROOT, date_folder)
if (dir.exists(eda_date_path)) {
  day_eda_excel_files <- list.files(
    eda_date_path,
    pattern = "\\.(csv|xls|xlsx)$",   # pick Excel/CSV files
    full.names = FALSE,
    recursive = FALSE
  )
} else {
  day_eda_excel_files <- character(0)
}

# ----------------------------
# Step 3: Mirage CSVs
# ----------------------------
mirage_folders <- list.files(
  MIRAGE_ROOT,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = FALSE
)
