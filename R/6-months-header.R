library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(data.table)

# Build sub-paths for each data type
ECG_ROOT    <- file.path(BOX_PATH, "Firstbeat Bodyguard 3 data/Wave 1 data/7. 6-month follow up Mothers & Children")
EDA_ROOT    <- file.path(BOX_PATH, "eSense data/Wave 1 data/7. 6-month follow up Mothers")
MIRAGE_ROOT <- file.path(BOX_PATH, "MIRAGE")

# ----------------------------
# User Input
# ----------------------------
cat("\n--- Dates range: 5th April, 2023 to 1st Feb, 2024 ---", "\n")
search.year.num  <- as.integer(readline(prompt = "Enter year (YYYY): "))
search.month.num <- as.integer(readline(prompt = "Enter month (1-12): "))
search.day.num   <- as.integer(readline(prompt = "Enter day (1-31): "))

# Format YYYYMMDD
date_folder <- paste0(
  search.year.num,
  sprintf("%02d", search.month.num),
  sprintf("%02d", search.day.num)
)