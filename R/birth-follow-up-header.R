
suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(purrr)
  library(tidyr)
  library(stringr)
  library(data.table)
  library(openxlsx)
  library(readxl)
})

# ------------------------------------------------
# 0) Paths and Date Input
# ------------------------------------------------
source("R/box-path.R")

# Build sub-paths for each data type
ECG_ROOT    <- file.path(BOX_PATH, "Firstbeat Bodyguard 3 data/Wave 1 data/1. Recruitment Pregnant Women")
EDA_ROOT    <- file.path(BOX_PATH, "eSense data/Wave 1 data/1. Recruitment Pregnant Women")
MIRAGE_ROOT <- file.path(BOX_PATH, "MIRAGE")

cat("\n--- Dates range: 2023-04-05 to 2024-10-22 ---\n")
search.year.num  <- as.integer(readline(prompt = "Enter year (YYYY): "))
search.month.num <- as.integer(readline(prompt = "Enter month (1-12): "))
search.day.num   <- as.integer(readline(prompt = "Enter day (1-31): "))

search_date <- make_date(search.year.num, search.month.num, search.day.num)
date_folder <- sprintf("%04d%02d%02d", search.year.num, search.month.num, search.day.num)

skip_child_until <- as.Date("2024-10-05")  # inclusive
process_child <- search_date > skip_child_until

# Convenience paths for the selected date
ecg_date_path <- file.path(ECG_ROOT, date_folder)
eda_date_path <- file.path(EDA_ROOT, date_folder)

print(date_folder)
print(search_date)
print(skip_child_until)
print(process_child)
print(ecg_date_path)
print(eda_date_path)