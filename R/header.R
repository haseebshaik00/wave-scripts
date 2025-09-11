# Define base Box path
BOX_PATH <- "C:/Users/hshaik00/Box/Bangladesh Study/Data"

# Build sub-paths for each data type
ECG_ROOT    <- file.path(BOX_PATH, "Firstbeat Bodyguard 3 data/Wave 1 data/1. Recruitment Pregnant Women")
EDA_ROOT    <- file.path(BOX_PATH, "eSense data/Wave 1 data/1. Recruitment Pregnant Women")
MIRAGE_ROOT <- file.path(BOX_PATH, "MIRAGE")

# Example inputs
search.year.num  <- as.integer(readline(prompt = "Enter year (YYYY): "))
search.month.num <- as.integer(readline(prompt = "Enter month (1-12): "))

# Always format month with 2 digits (03, 04, …)
month_str <- sprintf("%02d", search.month.num)

# Create folder prefix YYYYMM
folder_prefix <- paste0(search.year.num, month_str)

# Example: list all subfolders for that month (any day)
all_ecg_subfolders <- list.dirs(ECG_ROOT, full.names = FALSE, recursive = FALSE)
all_eda_subfolders <- list.dirs(EDA_ROOT, full.names = FALSE, recursive = FALSE)
month_ecg_subfolders <- all_ecg_subfolders[grepl(folder_prefix, all_ecg_subfolders)]
month_eda_subfolders <- all_eda_subfolders[grepl(folder_prefix, all_eda_subfolders)]

# Get all Excel files from MIRAGE_ROOT
mirage_folders <- list.files(MIRAGE_ROOT, 
  pattern = "\\.csv$",   # only Excel files
  full.names = TRUE,      # return full path
  recursive = FALSE)       # only top level

print(month_ecg_subfolders)
print(month_eda_subfolders)
print(mirage_folders)