
# Brings in BOX_PATH and ECG_ROOT and other libs
source("R/box-path.R")

# Build sub-paths for each data type
ECG_ROOT    <- file.path(BOX_PATH, "Firstbeat Bodyguard 3 data/Wave 1 data/1. Recruitment Pregnant Women")

get_script_dir <- function() {
  # RStudio (interactive)
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(p)) return(normalizePath(dirname(p)))
  }
  # Rscript (command line)
  ca <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", ca[grepl("^--file=", ca)])
  if (length(file_arg)) return(normalizePath(dirname(file_arg[1])))
  # Knit/Rmd
  if (requireNamespace("knitr", quietly = TRUE)) {
    ki <- tryCatch(knitr::current_input(), error = function(e) NULL)
    if (!is.null(ki)) return(normalizePath(dirname(ki)))
  }
  # Fallback
  normalizePath(getwd())
}

# --- Collect date folders (exact YYYYMMDD names) ---
stopifnot(dir.exists(ECG_ROOT))

date_folders <- list.dirs(ECG_ROOT, recursive = FALSE, full.names = FALSE)
date_folders <- date_folders[grepl("^\\d{8}$", date_folders)] |> unique()
date_folders <- date_folders[order(as.Date(date_folders, "%Y%m%d"))]

out_df <- tibble::tibble(`Recruitment Date (YYYYMMDD)` = date_folders)

# --- Write Excel next to this script ---
script_dir <- get_script_dir()
out_path   <- file.path(script_dir, "All Recruitment Dates.xlsx")

openxlsx::write.xlsx(
  x = list("All Recruitment Dates" = out_df),
  file = out_path,
  overwrite = TRUE
)

cat("Wrote", nrow(out_df), "dates to:", normalizePath(out_path), "\n")