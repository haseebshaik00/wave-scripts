
# --------------- Step 1 - Include header file - which has access to raw data ---------------
# This stores raw data paths into variables mentioned below

source("R/header.R")

# Below are the variables that has raw data's folder info
# mirage_folders has access to all mirage files whereas
# month_ecg_subfolders and month_eda_subfolders folders has 
# access to specific year and months mentioned by the client!

# Quick peek
cat("Total ECG subfolders found on or after", format(cutoff_date, "%Y-%m-%d"), length(month_ecg_subfolders), ":\n")
print(month_ecg_subfolders)

cat("\nEDA subfolders found on or after", format(cutoff_date, "%Y-%m-%d"), length(month_eda_subfolders), ":\n")
print(month_eda_subfolders)
#print(mirage_folders)

# --------------- Step - 2: Filter ECG excel files ---------------
print("--------------- ECG Files Filtering ---------------")
# Function to get the largest ECG file (pref 2–7 MB, fallback largest overall)
get_largest_ecg_file <- function(folder_path) {
  # List all files in this folder (including subfolders if needed)
  files <- list.files(file.path(ECG_ROOT, folder_path), 
                      pattern = "ecg", 
                      full.names = TRUE, 
                      recursive = TRUE)
  
  if (length(files) == 0) return(NA)  # if no ecg file
  
  # Get file sizes in bytes
  file_sizes <- file.info(files)$size
  
  # Define MB range
  min_size <- 2 * 1024 * 1024
  max_size <- 10 * 1024 * 1024
  
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

# Apply to all ECG subfolders
selected_ecg_files <- lapply(month_ecg_subfolders, get_largest_ecg_file)
selected_ecg_files <- Filter(Negate(is.na), selected_ecg_files)
# Print results
print(selected_ecg_files)
cat("Total ECG Excel Files found =", length(selected_ecg_files), "\n")

# --------------- Step - 3: Filter MIRAGE Data ---------------
# ---- Read and merge all Mirage CSVs ----
mirage.merged <- mirage_folders %>%
  lapply(read.csv) %>%
  bind_rows()

# ---- Filter for 'baseline' markers and specific month/year ----
mirage.long <- mirage.merged %>%
  filter(grepl("baseline", marker, ignore.case = TRUE)) %>%   # case-insensitive match
  filter(month(ymd_hms(jotDateTime)) == search.month.num) %>%
  filter(year(ymd_hms(jotDateTime)) == search.year.num) %>%
  select(participantID, marker, UTC, jotDateTime)

# ---- Clean participant IDs (remove "PID" prefix, make numeric) ----
mirage.long$participantID <- gsub("PID", "", mirage.long$participantID) %>% as.numeric()

# ---- Create unique PID list (Reliability log) ----
unique.id <- mirage.long %>%
  distinct(participantID, .keep_all = TRUE) %>%
  arrange(participantID)

# ---- Preview ----
print("--------------- Mirage Data ---------------")
print(head(mirage.long))
print("--------------- Realibility Logs PIDs List ---------------")
print(head(unique.id))

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

# Calculate durations
mirage.markers <- mirage.marker.merged %>%
  select(participantID, start.jotdatetime, baseline_start,
         stop.jotdatetime, baseline_stop) %>%
  mutate(mirage.duration = difftime(stop.jotdatetime, start.jotdatetime, units = "mins"))

# Keep only sessions of reasonable length (4–7 mins)
mirage.markers <- subset(mirage.markers, mirage.duration > 4 & mirage.duration < 6)

# ---- Detect duplicates (multiple baseline starts/stops) ----
mirage.wide <- pivot_wider(mirage.long, names_from = "marker", values_from = c("UTC"), values_fn = list)

listofdupes <- c()
for (q in 1:nrow(mirage.wide)) {
  if (length(unlist(mirage.wide$baseline_start[[q]])) > 1) {
    listofdupes <- append(listofdupes, q)
    print(mirage.wide$baseline_start[[q]])  # print duplicates for review
  } else if (length(unlist(mirage.wide$baseline_stop[[q]])) > 1) {
    listofdupes <- append(listofdupes, q)
    print(mirage.wide$baseline_stop[[q]])   # print duplicates for review
  }
}

# Collect duplicate cases
mirage.issue <- mirage.wide[listofdupes, ]

# ---- Corrected dataset: keep only distinct start times per participant ----
mirage.correct <- mirage.markers %>%
  distinct(participantID, baseline_start, .keep_all = TRUE) %>%
  arrange(participantID)

# Clean participantID (remove "PID", make numeric)
mirage.correct$participantID <- gsub("PID", "", mirage.correct$participantID) %>% as.numeric()

# ---- Final outputs ----
print("Mirage sessions with duplicates:")
print(mirage.issue)

print("Clean Mirage sessions:")
print(head(mirage.correct))

# --------------- Step - 4: ANS (Autonomic Nervous System) merging pipeline ---------------
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
