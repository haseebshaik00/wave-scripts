
# --------------- Step 1 - Include header file - which has access to raw data ---------------
# This stores raw data paths into variables mentioned below

source("R/header.R")

# Below are the variables that has raw data's info
# mirage_folders has access to all mirage excel files whereas
# day_ecg_subfolders and day_eda_excel_files folders has 
# the PID data folders - to be specific

# Quick peek
cat("\n--------------- Output #1 ---------------")
cat("\nTotal ECG PIDs subfolders found on ", format(cutoff_date, "%Y-%m-%d"),  " = ", length(day_ecg_subfolders), "\n")
print(day_ecg_subfolders)

cat("\nTotal EDA PIDs excel files found on ", format(cutoff_date, "%Y-%m-%d"), " = ", length(day_eda_excel_files), "\n")
print(day_eda_excel_files)

cat("\n Total Mirage Excel Files = ", length(mirage_folders), " (Top 5 represented below) \n")
print(head(mirage_folders))
cat("-----------------------------------------\n")

# --------------- Step - 2: Filter ECG excel files ---------------
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
  
  # Define MB range
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

# # Apply to all PID subfolders from the chosen date
# selected_ecg_files <- lapply(month_ecg_subfolders, get_largest_ecg_file)
# selected_ecg_files <- unlist(selected_ecg_files, use.names = FALSE)
# # Drop missing (NA) entries
# selected_ecg_files <- Filter(Negate(is.na), selected_ecg_files)

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

# ---- Create unique PID list (Reliability log) ----
unique.id <- mirage.long %>%
  distinct(participantID, .keep_all = TRUE) %>%
  arrange(participantID)

cat("--------------- Output #3 ---------------\n")
cat("Mirage Data Found =", nrow(mirage.long), "\n")
print(head(mirage.long))
cat("Realibility Logs PIDs List =", nrow(unique.id), "\n")
print(head(unique.id))
cat("-----------------------------------------\n")

# --------------- Step - 4: Handle Mirage baseline start/stop markers ---------------
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
cat("Mirage marker merged", nrow(mirage.marker.merged), "\n")
print(mirage.marker.merged)

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


# --------------- Step - 5: ANS (Autonomic Nervous System) merging pipeline ---------------
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
parse_pid_from_path <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  parent <- basename(dirname(path))
  pid <- suppressWarnings(as.integer(parent))
  if (!is.na(pid)) return(pid)
  hits <- regmatches(path, gregexpr("\\b\\d{5,}\\b", path))[[1]]
  if (length(hits)) return(as.integer(hits[1]))
  NA_integer_
}

to_posix <- function(x) {
  as.POSIXct(ifelse(x > 1e12, x/1000, x), origin = "1970-01-01", tz = "UTC")
}

ecg.toMerge <- c()
for (i in 1:length(selected_ecg_files)) {
  print(paste0(i, "/", length(selected_ecg_files)))  # progress counter
  
  # Read ECG CSV (skip first 11 lines for metadata)
  ecg.data <- read.csv(selected_ecg_files[i], skip = 11, header = TRUE)
  colnames(ecg.data) <- c("timestamp", "ecg", "ecg_mV")
  
  # Extract participant ID (from path position 6)
  #ecg.id <- strsplit(selected_ecg_files[i], "/")[[1]][6] %>% as.numeric()
  ecg.id <- parse_pid_from_path(selected_ecg_files[i])
  
  # Get recording start & end timestamps
  ecg.start <- ecg.data$timestamp[1]
  ecg.end   <- ecg.data$timestamp[length(ecg.data$timestamp)]
  
  # Create dataframe for this ECG file
  ecg.df <- data.frame(
    participantID = ecg.id,
    ECG.File      = selected_ecg_files[i],
    ECG.UTC.Start = ecg.start,
    ECG.UTC.Stop  = ecg.end,
    ECG.Start.Clock.Original = to_posix(ecg.start),
    ECG.Stop.Clock.Original  = to_posix(ecg.end),
    stringsAsFactors = FALSE
  )
  
  ecg.toMerge <- c(ecg.toMerge, list(ecg.df))
}

# Combine all ECG files into one dataframe
#ecg.merged.with.dupe <- subset(rbindlist(ecg.toMerge, id="participantID", use.names=FALSE), select=-1)
ecg.merged.with.dupe <- dplyr::bind_rows(ecg.toMerge)

print(ecg.merged.with.dupe)

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
cat("\n--------------- EDA Summary ---------------\n")
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
# 
# # First merge ECG + EDA
# ecg.eda <- merge(ecg.merged.with.dupe, eda.merged,
#                  by = "participantID", all = TRUE, sort = FALSE)
# 
# # Then merge with Mirage
# ans.mirage <- merge(ecg.eda, mirage.correct,
#                     by = "participantID", all = TRUE, sort = FALSE)
# 
# ################################################################################################
# ### Block 5: Identify Missing Data ###
# ################################################################################################
# 
# missing.mirage <- ans.mirage %>% filter(is.na(Mirage.Start.UTC))
# missing.ecg    <- ans.mirage %>% filter(is.na(ECG.File))
# missing.eda    <- ans.mirage %>% filter(is.na(EDA.File))
# 
# # Complete cases (all three present)
# complete.ans.mirage <- ans.mirage %>%
#   filter(!is.na(Mirage.Start.UTC) & !is.na(ECG.File) & !is.na(EDA.File))
# 
# ################################################################################################
