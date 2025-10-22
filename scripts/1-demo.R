options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(purrr)
  library(tidyr)
  library(data.table)
  library(stringr)
  library(tibble)
  library(openxlsx)
  quietly <- capture.output(requireNamespace("readxl", quietly = TRUE))
})
# ===========================
# T-drive file acquisition
# ===========================

# --- 1) Config (hardcode your run date here) ---
SEARCH_DATE <- as.Date("2023-03-22")  # << change me

T_ROOT      <- file.path("T:", "Bangladesh", "Raw data")
HRV_ROOT    <- file.path(T_ROOT, "HRV", "Wave 1 data", "1. Recruitment Pregnant Women")
EDA_ROOT    <- file.path(T_ROOT, "EDA", "1. Recruitment Pregnant Women")
MIRAGE_ROOT <- file.path(T_ROOT, "MIRAGE")

# Derive folder names used on disk
month_label      <- format(SEARCH_DATE, "%B %Y")   # e.g., "March 2023"
date_folder      <- format(SEARCH_DATE, "%Y%m%d")  # e.g., "20230311"
search_date      <- SEARCH_DATE
search.year.num  <- as.integer(format(SEARCH_DATE, "%Y"))
search.month.num <- as.integer(format(SEARCH_DATE, "%m"))
search.day.num   <- as.integer(format(SEARCH_DATE, "%d"))

# Final date paths
ecg_date_path <- file.path(HRV_ROOT, month_label, date_folder)  # HRV/ECG PIDs live here
eda_date_path <- file.path(EDA_ROOT, month_label, date_folder)  # EDA PID files live here

cat("\n--- T-drive paths ---\n")
cat("ECG path :", normalizePath(ecg_date_path, winslash = "/", mustWork = FALSE), "\n")
cat("EDA path :", normalizePath(eda_date_path, winslash = "/", mustWork = FALSE), "\n")
cat("MIRAGE   :", normalizePath(MIRAGE_ROOT, winslash = "/", mustWork = FALSE), "\n")

# --- 2) ECG/HRV: list 5-digit PID subfolders for the date ---
day_ecg_subfolders <- if (dir.exists(ecg_date_path)) {
  entries <- list.files(ecg_date_path, all.files = FALSE, no.. = TRUE)
  entries <- entries[dir.exists(file.path(ecg_date_path, entries))]
  grep("^\\d{5}$", entries, value = TRUE)
} else character(0)

# --- 3) EDA: pick only direct PID files (exclude *_converted, *_userpreferences, .mw) ---
day_eda_excel_files <- if (dir.exists(eda_date_path)) {
  all  <- list.files(eda_date_path, full.names = TRUE, recursive = FALSE)
  keep <- grep("(?i)^(?:.*/)?\\d{5}\\.(csv|xlsx|xls)$", all, perl = TRUE, value = TRUE)
  keep[!grepl("(?i)converted|userpreferences|\\.mw$", keep)]
} else character(0)

# --- 4) MIRAGE: collect all CSVs (filter by date later in your code) ---
mirage_folders <- if (dir.exists(MIRAGE_ROOT)) {
  list.files(MIRAGE_ROOT, pattern = "(?i)\\.csv$", full.names = TRUE, recursive = TRUE)
} else character(0)

# --- 5) Quick peek (same style as your Output #1) ---
cat("\n--------------- Output #1 ---------------\n")
cat("Selected date            :", format(search_date, "%Y-%m-%d"), "\n")
cat("ECG folder (YYYYMMDD)    :", basename(ecg_date_path),
    " | exists? ", dir.exists(ecg_date_path), "\n")
cat("ECG PID subfolders found :", length(day_ecg_subfolders), "\n")
print(day_ecg_subfolders)

cat("\nEDA folder (YYYYMMDD)    :", basename(eda_date_path),
    " | exists? ", dir.exists(eda_date_path), "\n")
cat("EDA files found          :", length(day_eda_excel_files), "\n")
print(basename(day_eda_excel_files))

cat("\nTotal MIRAGE CSV files   :", length(mirage_folders), " (top 5 below)\n")
print(head(mirage_folders, 5))
cat("----------------------------------------------------\n")


# --------------- Step - 2: Filter ECG excel files ---------------
# Issue #2: After confirmation will change this to --> ECG start time closest to Mirage time filtering idea
cat("\n --- [Processing] ECG Files Filtering ---------------\n")

# Function to get the largest ECG file (pref 2–10 MB, fallback largest overall)
get_largest_ecg_file <- function(pid_folder) {
  # Full path to this participant's folder (under the specific date)
  full_pid_path <- file.path(ecg_date_path, pid_folder)
  
  # List all files in this PID folder (recursive in case of nested structure)
  files <- list.files(full_pid_path, pattern = "ecg", full.names = TRUE, recursive = TRUE)
  
  if (length(files) == 0) return(NA)  # if no ECG files
  
  # Get file sizes in bytes
  file_sizes <- file.info(files)$size
  
  # [CHANGE] - These [CHANGE] tags can be changed according to your convenience!
  # Define MB range - ECG File Size
  min_size <- 2 * 1024 * 1024   # 2 MB
  max_size <- 10 * 1024 * 1024  # 10 MB
  
  # Filter files in the valid range
  valid_idx <- which(file_sizes >= min_size & file_sizes <= max_size)
  
  if (length(valid_idx) > 0) {
    # If at least one file is in range, pick the largest among them
    return(files[valid_idx[which.max(file_sizes[valid_idx])]])
  } else {
    # Otherwise, fallback to the largest file overall
    return(files[which.max(file_sizes)])
  }
}

selected_ecg_files <- sapply(day_ecg_subfolders, get_largest_ecg_file, USE.NAMES = FALSE)
selected_ecg_files <- selected_ecg_files[!is.na(selected_ecg_files)]

# Print results
print(selected_ecg_files)

cat("--------------- Output #2 ---------------\n")
cat("Total ECG Excel Files found =", length(selected_ecg_files), "\n")
cat("-----------------------------------------\n")

# --------------- Step - 3: Filter MIRAGE Data ---------------
cat("\n --- [Processing] Mirage Files Filtering ---------------\n")
# ---- Read and merge all Mirage CSVs ----
mirage.merged <- mirage_folders %>%
  lapply(read.csv) %>%
  bind_rows()

# ---- Filter for 'baseline' markers and specific month/year ----
mirage.long <- mirage.merged %>%
  filter(grepl("baseline", marker, ignore.case = TRUE)) %>%   # case-insensitive match
  filter(month(ymd_hms(jotDateTime)) == search.month.num) %>%
  filter(year(ymd_hms(jotDateTime)) == search.year.num) %>%
  filter(day(ymd_hms(jotDateTime)) == search.day.num) %>%     # <-- new day filter
  select(participantID, marker, UTC, jotDateTime)

# ---- Clean participant IDs (remove "PID" prefix, make numeric) ----
mirage.long$participantID <- gsub("PID", "", mirage.long$participantID) %>% as.numeric()

cat("--------------- Output #3 ---------------\n")
cat("Mirage Data Found =", nrow(mirage.long), "\n")
print(mirage.long)
cat("Unique PIDs List in Mirage =", nrow(unique.id), "\n")
print(head(unique.id))
cat("-----------------------------------------\n")

# --------------- Step - 4: Handle Mirage baseline start/stop markers ---------------
cat("\n --- [Processing] Mirage Files Cleansing + Formatting ---------------\n")
# Separate start vs stop events
mirage.bl.strt <- mirage.long %>%
  filter(grepl("baseline_start", marker, ignore.case = TRUE)) %>%
  select(participantID, marker, UTC, jotDateTime)

mirage.bl.stp <- mirage.long %>%
  filter(grepl("baseline_stop", marker, ignore.case = TRUE)) %>%
  select(participantID, marker, UTC, jotDateTime)

# Merge start/stop by participant
mirage.marker.merged <- merge(mirage.bl.strt, mirage.bl.stp, by = "participantID", all = TRUE, sort = FALSE)

# Rename for clarity
colnames(mirage.marker.merged) <- c("participantID", "start.marker", "baseline_start",
                                    "start.jotdatetime", "stop.marker", "baseline_stop",
                                    "stop.jotdatetime")

# Calculate durations
mirage.markers <- mirage.marker.merged %>%
  select(participantID, start.jotdatetime, baseline_start,
         stop.jotdatetime, baseline_stop) %>%
  mutate(mirage.duration = difftime(stop.jotdatetime, start.jotdatetime, units = "mins"))

# Keep only sessions of reasonable length (4–7 mins)
mirage.markers <- subset(mirage.markers, mirage.duration > 4 & mirage.duration < 7)

# ---- Detect duplicates (multiple baseline starts/stops) ----
mirage.wide <- pivot_wider(mirage.long, names_from = "marker", values_from = c("UTC"), values_fn = list)
listofdupes <- c()
for (q in 1:nrow(mirage.wide)) {
  if (length(unlist(mirage.wide$baseline_start[[q]])) > 1) {
    listofdupes <- append(listofdupes, q)
    # print(mirage.wide$baseline_start[[q]])  # print duplicates for review
  } else if (length(unlist(mirage.wide$baseline_stop[[q]])) > 1) {
    listofdupes <- append(listofdupes, q)
    # print(mirage.wide$baseline_stop[[q]])   # print duplicates for review
  }
}

# Idea #1: Filtered mirage data according to PIDs as well and it gives the same output
# so if anything arises we can switch to that as well, but idt there would be any difference
# in the output as such!

# Issue #1 : Collect duplicate cases and stores in mirage.issues variable
# so if any doubts with mirage data we can check this particular variable
# which states if any duplicate mirage data of a particular mirage id is present or not
mirage.issue <- mirage.wide[listofdupes, ]

# ---- Corrected dataset: keep only distinct start times per participant whose time is closest to 5 mins ----
mirage.correct <- mirage.markers %>%
  group_by(participantID) %>%
  slice_min(abs(as.numeric(mirage.duration, units = "mins") - 5), with_ties = FALSE) %>% 
  ungroup() %>%
  arrange(participantID)


# Clean participantID (remove "PID", make numeric)
mirage.correct$participantID <- gsub("PID", "", mirage.correct$participantID) %>% as.numeric()

cat("--------------- Output #4 ---------------\n")
cat("Total Mirage sessions with duplicates = ", nrow(mirage.issue), "\n")
print(mirage.issue)
cat("Toal Clean Mirage sessions = ", nrow(mirage.correct), "\n")
print(mirage.correct)
cat("-----------------------------------------\n")

# You can ignore [DEBUG] code blocks!
# # --------------- [Debug] Mirage Data Not Available (check raw Excel files) ---------------
# cat("\n --- [Processing] Mirage Data Not Available Check (vs raw Excel files) ---------------\n")
# 
# # Read all raw Mirage files (not filtered by date/marker yet)
# mirage.raw <- mirage_folders %>%
#   lapply(read.csv) %>%
#   bind_rows()
# 
# # Clean participantID field from raw data (remove "PID", force numeric)
# mirage.raw$participantID <- gsub("PID", "", mirage.raw$participantID) %>% as.numeric()
# 
# # Unique Mirage PIDs across all raw Excel files
# mirage_pid_list_raw <- unique(mirage.raw$participantID)
# 
# # ECG PIDs from today
# ecg_pid_list <- as.numeric(day_ecg_subfolders)
# 
# # Find ECG PIDs that do not appear anywhere in Mirage Excel files
# missing_mirage_pids <- setdiff(ecg_pid_list, mirage_pid_list_raw)
# 
# # Output
# cat("Total ECG PIDs =", length(ecg_pid_list), "\n")
# cat("Total Mirage PIDs (raw Excel) =", length(mirage_pid_list_raw), "\n")
# cat("Mirage data NOT available for", length(missing_mirage_pids), "PIDs\n")
# 
# print(missing_mirage_pids)
# cat("-----------------------------------------\n")
# 
# # --------------- Debug: Print rows for PID 45001 from Mirage raw files (with file path) ---------------
# cat("\n --- Extracting Mirage rows for PID 45001 (with source file path) ---------------\n")
# # This is for the data 2023-03-11 (11th March, 2025)
# pid_to_check <- "45001"
# 
# # Read all raw Mirage files and add file path as a column
# mirage.raw <- lapply(mirage_folders, function(f) {
#   df <- read.csv(f)
#   df$source_file <- f   # add source file path column
#   return(df)
# }) %>%
#   bind_rows()
# 
# # Clean participantID column
# mirage.raw$participantID <- gsub("PID", "", mirage.raw$participantID)
# 
# # Filter for the PID of interest
# mirage_pid_rows <- mirage.raw %>%
#   filter(participantID == pid_to_check)
# 
# # Output
# cat("--------------- PID Check Output ---------------\n")
# cat("Total rows found for PID", pid_to_check, "=", nrow(mirage_pid_rows), "\n")
# print(mirage_pid_rows)
# cat("-----------------------------------------------\n")



# --------------- Step - 5: ANS (Autonomic Nervous System) merging pipeline (Total 5 blocks) ---------------
################################################################################################
### Block 1: Prepare Mirage Data (already filtered + cleaned earlier) ###
################################################################################################

# Convert UTC (milliseconds) → Clock time
mirage.correct$Mirage.Start.UTC <- as.numeric(mirage.correct$baseline_start)
mirage.correct$Mirage.Stop.UTC  <- as.numeric(mirage.correct$baseline_stop)

mirage.correct <- mirage.correct %>%
  mutate(
    Mirage.Start.Clock.Original = as_datetime(Mirage.Start.UTC / 1000),
    Mirage.Stop.Clock.Original  = as_datetime(Mirage.Stop.UTC / 1000)
  )

################################################################################################
### Block 2: Process ECG Files ###
################################################################################################
cat("\n --- [Processing] ECG Files Processing ---------------\n")
# Extract 5-digit PID folder (works with / or \ on Windows)
extract_pid <- function(path) {
  pid <- stringr::str_extract(path, "(?<=/|\\\\)\\d{5}(?=/|\\\\)")
  as.numeric(pid)
}

# Decide epoch unit from magnitude (seconds / ms / µs)
.guess_epoch_scale <- function(x_num) {
  m <- suppressWarnings(stats::median(as.numeric(x_num), na.rm = TRUE))
  if (!is.finite(m)) return(1)           # default seconds
  if (m > 1e14) return(1e6)              # microseconds
  if (m > 1e11) return(1e3)              # milliseconds
  1                                       # seconds
}

# Read a Firstbeat ECG CSV and return one summary row
summarize_ecg_file <- function(fp) {
  tryCatch({
    ecg.data <- suppressWarnings(read.csv(fp, skip = 11, header = TRUE, check.names = FALSE))
    if (nrow(ecg.data) == 0) stop("no rows after skip=11")
    
    # Standardize first columns safely
    nm <- names(ecg.data)
    if (length(nm) >= 1) nm[1] <- "timestamp"
    if (length(nm) >= 2) nm[2] <- "ecg"
    if (length(nm) >= 3) nm[3] <- "ecg_mV"
    names(ecg.data) <- nm
    
    # Pick unit (s / ms / µs)
    scale <- .guess_epoch_scale(ecg.data$timestamp)
    
    # Start/stop (raw + clocks)
    ts_start_raw <- suppressWarnings(as.numeric(ecg.data$timestamp[1]))
    ts_end_raw   <- suppressWarnings(as.numeric(ecg.data$timestamp[nrow(ecg.data)]))
    
    start_clock_utc <- lubridate::as_datetime(ts_start_raw / scale, tz = "UTC")
    stop_clock_utc  <- lubridate::as_datetime(ts_end_raw   / scale, tz = "UTC")
    
    tibble::tibble(
      participantID = extract_pid(fp),
      ECG.File      = fp,
      ECG.Size.KB   = round(file.info(fp)$size / 1024, 2),
      
      # keep raw epoch from file (unscaled)
      ECG.Start.UTC = ts_start_raw,
      ECG.Stop.UTC  = ts_end_raw,
      
      # clock times
      ECG.Start.Clock.Original = start_clock_utc,
      ECG.Stop.Clock.Original  = stop_clock_utc,
      ECG.Start.Clock.Dhaka    = lubridate::with_tz(start_clock_utc, "Asia/Dhaka"),
      ECG.Stop.Clock.Dhaka     = lubridate::with_tz(stop_clock_utc,  "Asia/Dhaka"),
      
      ECG.Duration.Min = as.numeric(difftime(stop_clock_utc, start_clock_utc, units = "mins")),
      ECG.Sample.Count = nrow(ecg.data),
      .error = NA_character_
    )
  }, error = function(e) {
    tibble::tibble(
      participantID = extract_pid(fp),
      ECG.File      = fp,
      ECG.Size.KB   = round(file.info(fp)$size / 1024, 2),
      ECG.Start.UTC = NA_real_, ECG.Stop.UTC = NA_real_,
      ECG.Start.Clock.Original = as.POSIXct(NA),
      ECG.Stop.Clock.Original  = as.POSIXct(NA),
      ECG.Start.Clock.Dhaka    = as.POSIXct(NA),
      ECG.Stop.Clock.Dhaka     = as.POSIXct(NA),
      ECG.Duration.Min = NA_real_,
      ECG.Sample.Count = NA_integer_,
      .error = paste0("read_failed: ", conditionMessage(e))
    )
  })
}

# Build ECG index -----------------------------------------------------------
ecg.index <- selected_ecg_files |>
  lapply(summarize_ecg_file) |>
  dplyr::bind_rows() |>
  dplyr::arrange(participantID)

# Quick logs
cat("--------------- Output #5.1 - ECG Data ---------------\n")
cat("Total ECG files processed =", nrow(ecg.index), "\n")
cat("With read errors =", sum(!is.na(ecg.index$.error)), "\n")
print(ecg.index)

# Optional: list problematic files
ecg.errors <- ecg.index %>%
  dplyr::filter(!is.na(.error) |
                  dplyr::if_any(c(ECG.Start.UTC, ECG.Stop.UTC,
                                  ECG.Start.Clock.Original, ECG.Stop.Clock.Original), is.na))

ecg.index <- ecg.index %>%
  dplyr::filter(is.na(.error)) %>%
  dplyr::filter(dplyr::if_all(c(ECG.Start.UTC, ECG.Stop.UTC,
                                ECG.Start.Clock.Original, ECG.Stop.Clock.Original), ~ !is.na(.)))
cat("Total ECG files with errors [Review] =", nrow(ecg.errors), "\n")
print(ecg.errors)
cat("-----------------------------------------\n")

################################################################################################
### Block 3: Process EDA Files (robust to metadata rows and delimiters)
################################################################################################

# Helper to read one EDA file and return start/stop clock times
read_eda_summary <- function(f) {
  lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
  if (length(lines) == 0) {
    warning("Empty file: ", f); return(NULL)
  }
  
  # 1) Find the header row of the data block
  #    Prefer a line that starts with SECOND (typical eSense export).
  hdr_idx <- grep("^\\s*SECOND[;,\\t]", lines, perl = TRUE)
  if (length(hdr_idx) == 0) {
    # fallback: any line containing TIMESTAMP looks like a header
    hdr_idx <- grep("TIMESTAMP", lines, fixed = TRUE)
  }
  if (length(hdr_idx) == 0) {
    warning("No header row found in: ", f); return(NULL)
  }
  hdr_idx <- hdr_idx[1]
  
  # 2) Detect delimiter from the header line
  hdr_line <- lines[hdr_idx]
  sep <- if (grepl(";", hdr_line)) ";" else if (grepl(",", hdr_line)) "," else "\t"
  
  # 3) Read the table beginning at the header
  df <- tryCatch(
    read.table(
      f, header = TRUE, sep = sep, skip = hdr_idx - 1,
      quote = "", comment.char = "", fill = TRUE, stringsAsFactors = FALSE
    ),
    error = function(e) { warning("Failed to read: ", f, " — ", e$message); NULL }
  )
  if (is.null(df) || !nrow(df)) return(NULL)
  
  # 4) Identify the timestamp column
  ts_col <- intersect(c("TIMESTAMP", "Timestamp", "TIME", "Time"), names(df))
  if (!length(ts_col)) { warning("No TIMESTAMP column in: ", f); return(NULL) }
  ts <- as.character(df[[ts_col[1]]])
  
  # 5) Extract first & last non-empty timestamps; keep only HH:MM:SS
  nz <- which(!is.na(ts) & nzchar(ts))
  if (!length(nz)) { warning("No timestamp values in: ", f); return(NULL) }
  start_raw <- ts[min(nz)]
  stop_raw  <- ts[max(nz)]
  
  extract_hms <- function(x) {
    m <- regmatches(x, regexpr("\\b\\d{2}:\\d{2}:\\d{2}\\b", x))
    if (!length(m)) NA_character_ else m
  }
  eda_start <- extract_hms(start_raw)
  eda_stop  <- extract_hms(stop_raw)
  
  # 6) PID from filename (your files are .../YYYYMMDD/41001.csv)
  pid <- suppressWarnings(as.numeric(tools::file_path_sans_ext(basename(f))))
  
  data.frame(
    participantID = pid,
    EDA.File = f,
    EDA.Start.Clock.Original = eda_start,
    EDA.Stop.Clock.Original  = eda_stop,
    stringsAsFactors = FALSE
  )
}

# Iterate over all day EDA files (vector of full paths)
eda.list <- lapply(day_eda_excel_files, read_eda_summary)
eda.list <- Filter(Negate(is.null), eda.list)

# Combine
eda.merged <- dplyr::bind_rows(eda.list)

# Quick diagnostics and cleaning
eda.error  <- eda.merged %>%
  dplyr::filter(is.na(EDA.Start.Clock.Original) | is.na(EDA.Stop.Clock.Original))

# (Optionally keep only rows with valid HH:MM:SS)
eda.merged <- eda.merged %>%
  dplyr::filter(!is.na(EDA.Start.Clock.Original) & !is.na(EDA.Stop.Clock.Original))

# Peek
cat("\n--------------- Output #5.2 - EDA Data ---------------\n")
cat("Total EDA files read: ", length(day_eda_excel_files), "\n")
cat("Merged rows: ", nrow(eda.merged), "\n")
if (nrow(eda.error)) {
  cat("Rows with missing/invalid times: ", nrow(eda.error), "\n")
  print(eda.error)
}
print(eda.merged)
cat("-------------------------------------------\n")

# ################################################################################################
# ### Block 4: Merge ECG + EDA + Mirage ###
# ################################################################################################
ecg_id    <- ecg.index      %>% distinct(participantID, .keep_all = TRUE)
eda_id    <- eda.merged     %>% distinct(participantID, .keep_all = TRUE)
mirage_id <- mirage.correct %>% distinct(participantID, .keep_all = TRUE)

# inner join = only participants present in all three
ans.mirage <- ecg_id %>%
  inner_join(eda_id,    by = "participantID") %>%
  inner_join(mirage_id, by = "participantID")

View(ans.mirage)

# ################################################################################################
# ### Block 5: Identify Missing Data ###
# ################################################################################################

ecg_ids    <- unique(ecg_id$participantID)
eda_ids    <- unique(eda_id$participantID)
mirage_ids <- unique(mirage_id$participantID)

missing_ecg_ids    <- setdiff(intersect(eda_ids, mirage_ids), ecg_ids)
missing_eda_ids    <- setdiff(intersect(ecg_ids, mirage_ids), eda_ids)
missing_mirage_ids <- setdiff(intersect(ecg_ids, eda_ids),   mirage_ids)

missing_ecg <- tibble(participantID = missing_ecg_ids) %>%
  left_join(eda_id,    by = "participantID") %>%
  left_join(mirage_id, by = "participantID")

missing_eda <- tibble(participantID = missing_eda_ids) %>%
  left_join(ecg_id,    by = "participantID") %>%
  left_join(mirage_id, by = "participantID")

missing_mirage <- tibble(participantID = missing_mirage_ids) %>%
  left_join(ecg_id, by = "participantID") %>%
  left_join(eda_id, by = "participantID")

View(missing_ecg)
View(missing_eda)
View(missing_mirage)
# -------------------------------------------

# --------------- Step - 6: Baseline Event-Window Math (Total 8 blocks) ---------------

# If your inner-join result is empty (e.g., 2023-03-11), this section will no-op gracefully
# The final data is stored in complete.ans.mirage variable!
complete.ans.mirage <- ans.mirage

# ----------------------------
# Baseline event-window math
# ----------------------------

# If your inner-join result is empty (e.g., 2023-03-11), this section will no-op gracefully
complete.ans.mirage <- ans.mirage

# Helper for file name label (avoids relying on search.month)
search_date_label <- sprintf("%04d-%02d-%02d", search.year.num, search.month.num, search.day.num)

if (nrow(complete.ans.mirage) > 0) {
  
  # ----------------------------
  # Block A — Add base durations & dates (UTC)
  # ----------------------------
  addDuration.ECGMirage <- complete.ans.mirage %>%
    mutate(
      ECG.Date    = as.Date(ECG.Start.Clock.Original,  tz = "UTC"),
      Mirage.Date = as.Date(Mirage.Start.Clock.Original, tz = "UTC"),
      
      ECG.Duration.Original    = as.numeric(difftime(ECG.Stop.Clock.Original,    ECG.Start.Clock.Original,    units = "mins")),
      Mirage.Duration.Original = as.numeric(difftime(Mirage.Stop.Clock.Original, Mirage.Start.Clock.Original, units = "mins")),
      ECG2Mirage.Duration.Original = as.numeric(difftime(Mirage.Start.Clock.Original, ECG.Start.Clock.Original, units = "mins"))
    )
  
  # ----------------------------
  # Block B — Bring EDA to UTC (EDA is local Dhaka time ≈ UTC+6)
  # Use ECG.Date to attach the date to EDA HH:MM:SS
  # ----------------------------
  adjust.eda <- addDuration.ECGMirage %>%
    mutate(
      EDA.Start.Clock.Adjusted = lubridate::ymd_hms(paste(ECG.Date, EDA.Start.Clock.Original), tz = "UTC") - hours(6),
      EDA.Stop.Clock.Adjusted  = lubridate::ymd_hms(paste(ECG.Date, EDA.Stop.Clock.Original),  tz = "UTC") - hours(6)
    ) %>%
    # Safety: if EDA stop rolled past midnight (stop < start), add 1 day to stop
    mutate(
      EDA.Stop.Clock.Adjusted = ifelse(
        EDA.Stop.Clock.Adjusted < EDA.Start.Clock.Adjusted,
        EDA.Stop.Clock.Adjusted + days(1),
        EDA.Stop.Clock.Adjusted
      ) %>% as.POSIXct(origin = "1970-01-01", tz = "UTC")
    )
  
  addDuration.eda <- adjust.eda %>%
    mutate(
      EDA.Duration.Original          = as.numeric(difftime(EDA.Stop.Clock.Adjusted, EDA.Start.Clock.Adjusted, units = "mins")),
      EDA2Mirage.Duration.Original   = round(as.numeric(difftime(Mirage.Start.Clock.Original, EDA.Start.Clock.Adjusted, units = "mins")), 1),
      Mirage2EDA.Duration.Original   = round(as.numeric(difftime(EDA.Stop.Clock.Adjusted,   Mirage.Stop.Clock.Original,  units = "mins")), 1)
    )
  
  # Keep rows where ECG/MIRAGE are same calendar date (typical baseline day)
  ANSMIR.clean <- addDuration.eda %>% filter(ECG.Date == Mirage.Date)
  
  # ----------------------------
  # Block C — Compute baseline window (MW) start/stop in seconds
  # relative to each file’s start
  # ----------------------------
  calc.MWStart <- ANSMIR.clean %>% mutate(
    ECG.MWStart.Original = as.numeric(difftime(Mirage.Start.Clock.Original, ECG.Start.Clock.Original,  units = "secs")),
    EDA.MWStart.Original = as.numeric(difftime(Mirage.Start.Clock.Original, EDA.Start.Clock.Adjusted, units = "secs"))
  )
  
  calc.MWStop <- calc.MWStart %>%
    mutate(
      ECG.MWStop.Original = ECG.MWStart.Original + (Mirage.Duration.Original * 60),
      EDA.MWStop.Original = EDA.MWStart.Original + (Mirage.Duration.Original * 60)
    )
  
  # ----------------------------
  # Block D — Fix ECG that’s off by 6h (timezone mis-tag)
  # If Mirage appears to start "before" the ECG started (negative MW), shift ECG -6h
  # ----------------------------
  adjust.ecg <- calc.MWStop %>%
    mutate(
      ECG.Start.Clock.Adjusted = dplyr::case_when(
        ECG.MWStart.Original < 0 ~ lubridate::ymd_hms(ECG.Start.Clock.Original) - hours(6),
        TRUE                     ~ ECG.Start.Clock.Original
      ),
      ECG.Stop.Clock.Adjusted = dplyr::case_when(
        ECG.MWStop.Original < 0 ~ lubridate::ymd_hms(ECG.Stop.Clock.Original) - hours(6),
        TRUE                    ~ ECG.Stop.Clock.Original
      )
    )
  
  # ----------------------------
  # Block E — Nudge MIRAGE by 30 min (or 1 min) when it’s slightly ahead of EDA
  # Uses Mirage2EDA (EDA.stop - MIRAGE.stop); negative means MIRAGE stops later
  # ----------------------------
  adjust.mirage <- adjust.ecg %>%
    mutate(
      Mirage.Start.Clock.Adjusted = dplyr::case_when(
        dplyr::between(Mirage2EDA.Duration.Original, -30.01, -1.01) ~ lubridate::ymd_hms(Mirage.Start.Clock.Original) - minutes(30),
        dplyr::between(Mirage2EDA.Duration.Original,  -1.00, -0.02) ~ lubridate::ymd_hms(Mirage.Start.Clock.Original) - minutes(1),
        TRUE ~ Mirage.Start.Clock.Original
      ),
      Mirage.Stop.Clock.Adjusted = dplyr::case_when(
        dplyr::between(Mirage2EDA.Duration.Original, -30.01, -1.01) ~ lubridate::ymd_hms(Mirage.Stop.Clock.Original) - minutes(30),
        dplyr::between(Mirage2EDA.Duration.Original,  -1.00, -0.02) ~ lubridate::ymd_hms(Mirage.Stop.Clock.Original) - minutes(1),
        TRUE ~ Mirage.Stop.Clock.Original
      )
    )
  
  # ----------------------------
  # Block F — Recompute everything after adjustments
  # ----------------------------
  recalc.MWStart <- adjust.mirage %>%
    mutate(
      ECG.MWStart.Adjusted = as.numeric(difftime(Mirage.Start.Clock.Adjusted, ECG.Start.Clock.Adjusted, units = "secs")),
      EDA.MWStart.Adjusted = as.numeric(difftime(Mirage.Start.Clock.Adjusted, EDA.Start.Clock.Adjusted, units = "secs")),
      
      ECG.Duration.Adjusted    = as.numeric(difftime(ECG.Stop.Clock.Adjusted,    ECG.Start.Clock.Adjusted,    units = "mins")),
      EDA.Duration.Adjusted    = as.numeric(difftime(EDA.Stop.Clock.Adjusted,    EDA.Start.Clock.Adjusted,    units = "mins")),
      Mirage.Duration.Adjusted = as.numeric(difftime(Mirage.Stop.Clock.Adjusted, Mirage.Start.Clock.Adjusted, units = "mins")),
      
      ECG2Mirage.Duration.Adjusted = as.numeric(difftime(Mirage.Start.Clock.Adjusted, ECG.Start.Clock.Adjusted, units = "mins")),
      EDA2Mirage.Duration.Adjusted = as.numeric(difftime(Mirage.Start.Clock.Adjusted, EDA.Start.Clock.Adjusted, units = "mins")),
      Mirage2EDA.Duration.Adjusted = as.numeric(difftime(Mirage.Stop.Clock.Adjusted,  EDA.Stop.Clock.Adjusted,  units = "mins"))
    )
  
  recalc.MWStop <- recalc.MWStart %>%
    mutate(
      ECG.MWStop.Adjusted = ECG.MWStart.Adjusted + (Mirage.Duration.Adjusted * 60),
      EDA.MWStop.Adjusted = EDA.MWStart.Adjusted + (Mirage.Duration.Adjusted * 60)
    )
  
  # ----------------------------
  # Block G — QC filters (duration & proximity sanity checks)
  # ----------------------------
  recalc.clean <- recalc.MWStop %>%
    filter(
      ECG.Duration.Adjusted > 8, ECG.Duration.Adjusted < 60,
      ECG2Mirage.Duration.Adjusted < 120
    ) %>%
    select(-ECG.Date)
  
  recalc.clean <- recalc.clean %>%
    mutate(
      ECG.MWStart.Adjusted = round(ECG.MWStart.Adjusted, 3),
      ECG.MWStop.Adjusted  = round(ECG.MWStop.Adjusted,  3),
      EDA.MWStart.Adjusted = round(EDA.MWStart.Adjusted, 3),
      EDA.MWStop.Adjusted  = round(EDA.MWStop.Adjusted,  3)
    )
  
  # NOTE on -30.02: keep as-is if you still want to allow up to a 30-min MIRAGE–EDA stop offset.
  # Tighten to -1 if you only want ~clock-second misalignments.
  MWTimes.adjusted <- recalc.clean %>%
    select(
      Mirage.Date, participantID,
      ECG.MWStart.Adjusted, ECG.MWStop.Adjusted,
      EDA.MWStart.Adjusted, EDA.MWStop.Adjusted,
      ECG.Start.Clock.Adjusted, EDA.Start.Clock.Adjusted, Mirage.Start.Clock.Adjusted, Mirage.Stop.Clock.Adjusted,
      EDA.Stop.Clock.Adjusted,  ECG.Stop.Clock.Adjusted,
      ECG.Duration.Adjusted, EDA.Duration.Adjusted, Mirage.Duration.Adjusted,
      ECG2Mirage.Duration.Adjusted, EDA2Mirage.Duration.Adjusted, Mirage2EDA.Duration.Adjusted,
      ECG.File, EDA.File
    ) %>%
    filter(
      ECG.MWStart.Adjusted > 0, ECG.MWStop.Adjusted > 0,
      EDA.MWStart.Adjusted > 0, EDA.MWStop.Adjusted > 0,
      Mirage2EDA.Duration.Adjusted > -30.02, Mirage2EDA.Duration.Adjusted < 10
    )
  
  MWTimes.original <- recalc.MWStop %>%
    select(
      Mirage.Date, participantID,
      ECG.MWStart.Original, ECG.MWStop.Original,
      EDA.MWStart.Original, EDA.MWStop.Original,
      ECG.Start.Clock.Original, EDA.Start.Clock.Original, Mirage.Start.Clock.Original, Mirage.Stop.Clock.Original,
      EDA.Stop.Clock.Original,  ECG.Stop.Clock.Original,
      Mirage.Start.UTC, Mirage.Stop.UTC,
      ECG.Duration.Original, EDA.Duration.Original, Mirage.Duration.Original,
      ECG2Mirage.Duration.Original, EDA2Mirage.Duration.Original, Mirage2EDA.Duration.Original,
      ECG.File, EDA.File
    ) %>%
    mutate(
      ECG.MWStart.Original = round(ECG.MWStart.Original, 3),
      ECG.MWStop.Original  = round(ECG.MWStop.Original,  3),
      EDA.MWStart.Original = round(EDA.MWStart.Original, 3),
      EDA.MWStop.Original  = round(EDA.MWStop.Original,  3)
    )
  
  MWTimes.review <- recalc.clean %>%
    select(
      Mirage.Date, participantID,
      ECG.MWStart.Adjusted, ECG.MWStop.Adjusted,
      EDA.MWStart.Adjusted, EDA.MWStop.Adjusted,
      ECG.Start.Clock.Adjusted, EDA.Start.Clock.Adjusted, Mirage.Start.Clock.Adjusted, Mirage.Stop.Clock.Adjusted,
      EDA.Stop.Clock.Adjusted,  ECG.Stop.Clock.Adjusted,
      ECG.Duration.Adjusted, EDA.Duration.Adjusted, Mirage.Duration.Adjusted,
      ECG2Mirage.Duration.Adjusted, EDA2Mirage.Duration.Adjusted, Mirage2EDA.Duration.Adjusted,
      ECG.File, EDA.File
    ) %>%
    filter(
      ECG.MWStart.Adjusted < 0 | ECG.MWStop.Adjusted < 0 |
        EDA.MWStart.Adjusted < 0 | EDA.MWStop.Adjusted < 0 |
        Mirage2EDA.Duration.Adjusted < -30.02 | Mirage2EDA.Duration.Adjusted > 10
    )
  
  colnames(MWTimes.review) <- c(
    "Mirage.Date","participantID",
    "ECG.MWStart.Review","ECG.MWStop.Review",
    "EDA.MWStart.Review","EDA.MWStop.Review",
    "ECG.Start.Clock.Review","EDA.Start.Clock.Review","Mirage.Start.Clock.Review","Mirage.Stop.Clock.Review",
    "EDA.Stop.Clock.Review","ECG.Stop.Clock.Review",
    "ECG.Duration.Review","EDA.Duration.Review","Mirage.Duration.Review",
    "ECG2Mirage.Duration.Review","EDA2Mirage.Duration.Review","Mirage2EDA.Duration.Review",
    "ECG.File","EDA.File"
  )
  
} else {
  # If no complete triads, create empty frames for the export
  MWTimes.adjusted <- MWTimes.original <- MWTimes.review <- complete.ans.mirage
}

# ----------------------------
# Block H — Export workbook
# ----------------------------
export.list <- list(
  "Adjusted"                      = MWTimes.adjusted,
  "Needs Review"                  = MWTimes.review,
  "Original"                      = MWTimes.original,
  "Duplicate or Missing Mirage" = mirage.issue,
  "Mirage does not exist"         = missing_mirage,
  "EDA file does not exist"       = missing_eda,
  "ECG file does not exist"       = missing_ecg
)

# Build nice labels from the chosen date
search_date        <- lubridate::make_date(search.year.num, search.month.num, search.day.num)
search_date_label  <- format(search_date, "%Y-%m-%d")   # e.g., "2023-03-11"
month_year_label   <- format(search_date, "%B %Y")      # e.g., "March 2023"

# ensure the Month Year folder exists: output/Recruitment/March 2023/
dir.create(file.path("output", "Recruitment", month_year_label), recursive = TRUE, showWarnings = FALSE)

# build the full output path (file still includes the precise selected date)
out_path <- file.path(
  "output", "Recruitment", month_year_label,
  paste0(search_date_label, " MW Baseline Event Marker Times ", Sys.Date(), ".xlsx")
)

# write the file
openxlsx::write.xlsx(
  export.list,
  file = out_path,
  overwrite = TRUE
)

cat("File written to:", normalizePath(out_path), "\n")