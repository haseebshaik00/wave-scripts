
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

# Apply to all PID subfolders from the chosen date
selected_ecg_files <- lapply(month_ecg_subfolders, get_largest_ecg_file)

# Drop missing (NA) entries
selected_ecg_files <- Filter(Negate(is.na), selected_ecg_files)

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
# cat("\n --- [Debug] Extracting Mirage rows for PID 45001 (with source file path) ---------------\n")
# 
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

ecg.toMerge <- c()
for (i in 1:length(selected_ecg_files)) {
  print(paste0(i, "/", length(selected_ecg_files)))  # progress counter
  
  # Read ECG CSV (skip first 11 lines for metadata)
  ecg.data <- read.csv(selected_ecg_files[i], skip = 11, header = TRUE)
  colnames(ecg.data) <- c("timestamp", "ecg", "ecg_mV")
  
  # Extract participant ID (from path position 6)
  ecg.id <- strsplit(selected_ecg_files[i], "/")[[1]][6] %>% as.numeric()
  
  # Get recording start & end timestamps
  ecg.start <- ecg.data$timestamp[1]
  ecg.end   <- ecg.data$timestamp[length(ecg.data$timestamp)]
  
  # Create dataframe for this ECG file
  ecg.df <- data.frame(
    participantID = ecg.id,
    ECG.File      = selected_ecg_files[i],
    ECG.UTC.Start = ecg.start,
    ECG.UTC.Stop  = ecg.end,
    ECG.Start.Clock.Original = as_datetime(ecg.start),
    ECG.Stop.Clock.Original  = as_datetime(ecg.end),
    stringsAsFactors = FALSE
  )
  
  ecg.toMerge <- c(ecg.toMerge, list(ecg.df))
}

# Combine all ECG files into one dataframe
ecg.merged.with.dupe <- subset(rbindlist(ecg.toMerge, id="participantID", use.names=FALSE), select=-1)

################################################################################################
### Block 3: Process EDA Files ###
################################################################################################

eda.toMerge <- c()
for (k in 1:length(month.eda.list)) {
  # Read EDA CSV
  eda.data <- read.csv(month.eda.list[k], header = TRUE)
  
  # Extract start & end clock times (drop milliseconds using substr)
  eda.start <- substr(eda.data$TIMESTAMP[1], 1, 8)
  eda.end   <- substr(eda.data$TIMESTAMP[length(eda.data$TIMESTAMP)], 1, 8)
  
  # Extract participant ID from filename (position 5, remove "_converted.csv")
  eda.id <- gsub("_converted.csv", "", strsplit(month.eda.list[k], "/")[[1]][5]) %>% as.numeric()
  
  # Create dataframe for this EDA file
  eda.df <- data.frame(
    participantID = eda.id,
    EDA.File = month.eda.list[k],
    EDA.Start.Clock.Original = eda.start,
    EDA.Stop.Clock.Original  = eda.end,
    stringsAsFactors = FALSE
  )
  
  print(eda.df)  # check progress
  
  eda.toMerge <- c(eda.toMerge, list(eda.df))
}

# Combine all EDA files into one dataframe
eda.merged <- subset(rbindlist(eda.toMerge, id="participantID", use.names=FALSE), select=-1)

# Quick cleaning: remove corrupted/misformatted timestamps
eda.error  <- eda.merged %>% filter(nchar(EDA.Start.Clock.Original) != 8)
eda.merged <- eda.merged %>% filter(nchar(EDA.Stop.Clock.Original) == 8)

################################################################################################
### Block 4: Merge ECG + EDA + Mirage ###
################################################################################################

# First merge ECG + EDA
ecg.eda <- merge(ecg.merged.with.dupe, eda.merged,
                 by = "participantID", all = TRUE, sort = FALSE)

# Then merge with Mirage
ans.mirage <- merge(ecg.eda, mirage.correct,
                    by = "participantID", all = TRUE, sort = FALSE)

################################################################################################
### Block 5: Identify Missing Data ###
################################################################################################

missing.mirage <- ans.mirage %>% filter(is.na(Mirage.Start.UTC))
missing.ecg    <- ans.mirage %>% filter(is.na(ECG.File))
missing.eda    <- ans.mirage %>% filter(is.na(EDA.File))

# Complete cases (all three present)
complete.ans.mirage <- ans.mirage %>%
  filter(!is.na(Mirage.Start.UTC) & !is.na(ECG.File) & !is.na(EDA.File))

################################################################################################
