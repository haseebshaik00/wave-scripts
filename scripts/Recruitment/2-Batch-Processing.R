# ---------------------------------------------------------------
# Batch runner for Recruitment baseline day processing
#   - Reads dates (YYYYMMDD) from an Excel file
#   - For each date, runs your existing "2-main-script.R"
#   - No changes required to 1-R/recruitment-header.R
# ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(readxl)
  library(stringr)
})

# --------- CONFIG (edit if needed) ------------------------------------------
EXCEL_PATH <- "T:/Bangladesh/Haseeb/Summer '25/4-wave-scripts/wave-scripts/scripts/Recruitment/All Recruitment Dates.xlsx"
SHEET_NAME <- "All Recruitment Dates"          # sheet tab name (with spaces)
DATE_COL   <- "Recruitment Date (YYYYMMDD)"    # column header in A1
MAIN_PATH  <- "T:/Bangladesh/Haseeb/Summer '25/4-wave-scripts/wave-scripts/scripts/Recruitment/1-Recruitment-Main-Script.R"
LOG_PATH   <- file.path("output", "Recruitment", "_batch_log.txt")
# ----------------------------------------------------------------------------

# ensure output root exists for logs
dir.create(file.path("output", "Recruitment"), recursive = TRUE, showWarnings = FALSE)

log_line <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste0(..., collapse = ""))
  cat(msg, "\n")
  cat(msg, file = LOG_PATH, append = TRUE, sep = "\n")
}

# ---- Read all candidate dates as character YYYYMMDD -------------------------
raw <- readxl::read_excel(EXCEL_PATH, sheet = SHEET_NAME, col_types = "text")
if (is.numeric(DATE_COL)) {
  dates_chr <- raw[[DATE_COL]]
} else {
  dates_chr <- raw[[DATE_COL]]
}
# clean: keep only 8-digit strings
dates_chr <- str_trim(dates_chr)
dates_chr <- gsub("\\D", "", dates_chr)                 # strip non-digits
dates_chr <- dates_chr[nchar(dates_chr) == 8]           # keep YYYYMMDD only
dates_chr <- unique(dates_chr)                          # dedupe

if (!length(dates_chr)) {
  stop("No valid YYYYMMDD values found in the Excel file/column.")
}

log_line("Batch starting for ", length(dates_chr), " dates from: ", normalizePath(EXCEL_PATH, winslash = "/"))

# -------- Function: run main script once for one date ------------------------
run_for_date <- function(yyyymmdd) {
  yr <- as.integer(substr(yyyymmdd, 1, 4))
  mo <- as.integer(substr(yyyymmdd, 5, 6))
  dy <- as.integer(substr(yyyymmdd, 7, 8))
  label <- sprintf("%04d-%02d-%02d", yr, mo, dy)
  
  # Private environment for this run (prevents variable leakage)
  env <- new.env(parent = globalenv())
  
  # 1) Provide the three readline answers the header expects (year, month, day)
  #    We override readline() only within this env.
  answers <- c(as.character(yr), as.character(mo), as.character(dy))
  env$`__readline_i` <- 0L
  env$readline <- local({
    i <- 0L
    function(prompt = "") {
      i <<- i + 1L
      if (i <= length(answers)) return(answers[[i]])
      return("")  # default if asked more times
    }
  })
  
  # 2) Ensure that when the main script sources the header, it is sourced into
  #    THIS env (not the global env). We shadow base::source().
  env$source <- function(file, ...) {
    base::sys.source(file, envir = parent.frame())
  }
  
  # 3) Predefine cutoff_date so your main script’s early cat() lines won’t error.
  env$cutoff_date <- as.Date(label)
  
  # 4) Run the main script inside this env
  base::sys.source(MAIN_PATH, envir = env)
  
  # Optional: return something (e.g., a small summary)
  invisible(TRUE)
}

# -------------------------- Loop with robust error handling -------------------
ok <- 0L; fail <- 0L
for (d in dates_chr) {
  lab <- paste0(substr(d, 1, 4), "-", substr(d, 5, 6), "-", substr(d, 7, 8))
  log_line("▶ Running ", d, " (", lab, ")")
  tryCatch({
    run_for_date(d)
    ok <- ok + 1L
    log_line("✔ Finished ", d)
  }, error = function(e) {
    fail <- fail + 1L
    log_line("✖ FAILED  ", d, " — ", conditionMessage(e))
  })
}

log_line("Batch complete: success=", ok, ", failed=", fail)
