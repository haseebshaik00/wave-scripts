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
  # readxl only used if some EDA are .xlsx
  quietly <- capture.output(requireNamespace("readxl", quietly = TRUE))
})

# ----------------------------
# 0) CONFIG 
# ----------------------------
SEARCH_DATE <- as.Date("2023-03-11")    # <-- hardcode your run date
T_ROOT      <- file.path("T:", "Bangladesh", "Raw data")

# Folder roots (match your screenshots)
HRV_ROOT    <- file.path(T_ROOT, "HRV", "Wave 1 data", "1. Recruitment Pregnant Women")
EDA_ROOT    <- file.path(T_ROOT, "EDA", "1. Recruitment Pregnant Women")
MIRAGE_ROOT <- file.path(T_ROOT, "MIRAGE")

# Derived “month folder” and “date folder”
month_label <- format(SEARCH_DATE, "%B %Y")   # e.g., "March 2023"
date_folder <- format(SEARCH_DATE, "%Y%m%d")  # e.g., "20230311"

HRV_DATE_PATH <- file.path(HRV_ROOT, month_label, date_folder)  # ECG/HRV PIDs live here
EDA_DATE_PATH <- file.path(EDA_ROOT, month_label, date_folder)  # EDA files live here

cat("\n--- Paths ---\n")
cat("HRV date path :", normalizePath(HRV_DATE_PATH, winslash = "/", mustWork = FALSE), "\n")
cat("EDA date path :", normalizePath(EDA_DATE_PATH, winslash = "/", mustWork = FALSE), "\n")
cat("MIRAGE root   :", normalizePath(MIRAGE_ROOT, winslash = "/", mustWork = FALSE), "\n\n")

## pick files
# --- ECG ---
# find 5-digit PID directories for this date
day_ecg_subfolders <- character(0)
if (dir.exists(HRV_DATE_PATH)) {
  entries <- list.files(HRV_DATE_PATH, all.files = FALSE, no.. = TRUE)
  entries <- entries[dir.exists(file.path(HRV_DATE_PATH, entries))]
  day_ecg_subfolders <- grep("^\\d{5}$", entries, value = TRUE)
}

cat("ECG/HRV PID folders:", length(day_ecg_subfolders), "\n")
if (length(day_ecg_subfolders)) print(head(day_ecg_subfolders))

# pick a CSV from each PID folder — prefer 2–10 MB, else the largest
get_largest_csv <- function(pid_folder) {
  full_pid_path <- file.path(HRV_DATE_PATH, pid_folder)
  files <- list.files(full_pid_path, pattern = "(?i)\\.csv$", full.names = TRUE, recursive = TRUE)
  if (!length(files)) return(NA_character_)
  
  sz <- file.info(files)$size
  in_range <- which(sz >= 2*1024^2 & sz <= 10*1024^2)
  if (length(in_range)) files[in_range[which.max(sz[in_range])]] else files[which.max(sz)]
}

selected_ecg_files <- vapply(day_ecg_subfolders, get_largest_csv, "", USE.NAMES = FALSE)
selected_ecg_files <- selected_ecg_files[!is.na(selected_ecg_files)]

cat("\nECG CSVs selected:", length(selected_ecg_files), "\n")
if (length(selected_ecg_files)) print(head(selected_ecg_files))
# --- ECG ---

# --- EDA ---

# --- EDA ---

# --- MIRAGE ---
# --- MIRAGE ---




# --------------------------------------------------------------------
# 1) ECG/HRV — discover PIDs and pick 1 file per PID (largest CSV)
# -------------------------------------------------------------------
# ---- read & summarize one Firstbeat CSV robustly ----
# Guess epoch unit (s / ms / µs) based on magnitude
.guess_epoch_scale <- function(x_num) {
  m <- suppressWarnings(stats::median(as.numeric(x_num), na.rm = TRUE))
  if (!is.finite(m)) return(1)
  if (m > 1e14) return(1e6)  # microseconds
  if (m > 1e11) return(1e3)  # milliseconds
  1                           # seconds
}

# Extract 5-digit PID from path
extract_pid <- function(p) {
  pid <- stringr::str_extract(p, "(?<=/|\\\\)\\d{5}(?=/|\\\\)")
  as.numeric(pid)
}

read_ecg_csv_summary <- function(fp) {
  tryCatch({
    # Try Bodyguard raw (common export) — header ~11 lines
    df <- suppressWarnings(read.csv(fp, skip = 11, header = TRUE, check.names = FALSE))
    if (!nrow(df)) stop("no rows after skip=11")
    
    nm <- names(df)
    if (length(nm) >= 1) nm[1] <- "timestamp"
    if (length(nm) >= 2) nm[2] <- "ecg"
    if (length(nm) >= 3) nm[3] <- "ecg_mV"
    names(df) <- nm
    
    scale <- .guess_epoch_scale(df$timestamp)
    
    ts_start_raw <- suppressWarnings(as.numeric(df$timestamp[1]))
    ts_end_raw   <- suppressWarnings(as.numeric(df$timestamp[nrow(df)]))
    
    start_utc <- lubridate::as_datetime(ts_start_raw/scale, tz = "UTC")
    stop_utc  <- lubridate::as_datetime(ts_end_raw/scale,  tz = "UTC")
    
    tibble(
      participantID = extract_pid(fp),
      ECG.File      = fp,
      ECG.Size.KB   = round(file.info(fp)$size/1024, 2),
      ECG.Start.UTC = ts_start_raw,
      ECG.Stop.UTC  = ts_end_raw,
      ECG.Start.Clock.Original = start_utc,
      ECG.Stop.Clock.Original  = stop_utc,
      ECG.Start.Clock.Dhaka    = with_tz(start_utc, "Asia/Dhaka"),
      ECG.Stop.Clock.Dhaka     = with_tz(stop_utc,  "Asia/Dhaka"),
      ECG.Duration.Min         = as.numeric(difftime(stop_utc, start_utc, "mins")),
      ECG.Sample.Count         = nrow(df),
      .error = NA_character_
    )
  }, error = function(e) {
    tibble(
      participantID = extract_pid(fp),
      ECG.File      = fp,
      ECG.Size.KB   = round(file.info(fp)$size/1024, 2),
      ECG.Start.UTC = NA_real_, ECG.Stop.UTC = NA_real_,
      ECG.Start.Clock.Original = as.POSIXct(NA),
      ECG.Stop.Clock.Original  = as.POSIXct(NA),
      ECG.Start.Clock.Dhaka    = as.POSIXct(NA),
      ECG.Stop.Clock.Dhaka     = as.POSIXct(NA),
      ECG.Duration.Min         = NA_real_,
      ECG.Sample.Count         = NA_integer_,
      .error = paste0("read_failed: ", conditionMessage(e))
    )
  })
}

ecg.index <- selected_ecg_files %>% lapply(read_ecg_csv_summary) %>% bind_rows() %>% arrange(participantID)

cat("\n[ECG index] rows:", nrow(ecg.index), " | errors:", sum(!is.na(ecg.index$.error)), "\n")
if (nrow(ecg.index)) print(head(ecg.index, 3))

# Keep only good rows
ecg.errors <- ecg.index %>% filter(!is.na(.error) |
                                     if_any(c(ECG.Start.UTC, ECG.Stop.UTC, ECG.Start.Clock.Original, ECG.Stop.Clock.Original), is.na))
ecg.index  <- ecg.index  %>% filter(is.na(.error)) %>%
  filter(if_all(c(ECG.Start.UTC, ECG.Stop.UTC, ECG.Start.Clock.Original, ECG.Stop.Clock.Original), ~ !is.na(.)))

# --------------------------------------------------------------------
# 2) EDA — files are exactly the PID (e.g., 41001.csv / 41001.xlsx)
# --------------------------------------------------------------------
day_eda_files <- character(0)
if (dir.exists(EDA_DATE_PATH)) {
  # Only base PID files; exclude *_converted, *_userpreferences, *.mw
  # Accept csv/xlsx/xls
  all <- list.files(EDA_DATE_PATH, full.names = TRUE, recursive = FALSE)
  keep <- grep("(?i)^(?:.*/)?\\d{5}\\.(csv|xlsx|xls)$", all, perl = TRUE, value = TRUE)
  # drop converted / userpreferences explicitly if they sneak in
  keep <- keep[!grepl("(?i)converted|userpreferences|\\.mw$", keep)]
  day_eda_files <- keep
}

cat("\nEDA files picked:", length(day_eda_files), "\n")
if (length(day_eda_files)) print(basename(head(day_eda_files)))

read_eda_summary <- function(f) {
  ext <- tools::file_ext(f)
  if (tolower(ext) == "csv") {
    lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
    if (!length(lines)) { warning("Empty EDA csv: ", f); return(NULL) }
    
    # header line where columns start — prefer one that starts with SECOND
    hdr_idx <- grep("^\\s*SECOND[;,\\t]", lines, perl = TRUE)
    if (!length(hdr_idx)) hdr_idx <- grep("TIMESTAMP", lines, fixed = TRUE)
    if (!length(hdr_idx)) { warning("No header row in: ", f); return(NULL) }
    hdr_idx <- hdr_idx[1]
    
    hdr_line <- lines[hdr_idx]
    sep <- if (grepl(";", hdr_line)) ";" else if (grepl(",", hdr_line)) "," else "\t"
    
    df <- tryCatch(
      read.table(f, header = TRUE, sep = sep, skip = hdr_idx - 1,
                 quote = "", comment.char = "", fill = TRUE, stringsAsFactors = FALSE),
      error = function(e) { warning("Failed EDA csv: ", f, " — ", e$message); NULL }
    )
    if (is.null(df) || !nrow(df)) return(NULL)
    
    ts_col <- intersect(c("TIMESTAMP","Timestamp","TIME","Time"), names(df))
    if (!length(ts_col)) { warning("No TIMESTAMP in: ", f); return(NULL) }
    ts <- as.character(df[[ts_col[1]]])
    
    nz <- which(!is.na(ts) & nzchar(ts)); if (!length(nz)) { warning("No timestamps in: ", f); return(NULL) }
    start_raw <- ts[min(nz)]; stop_raw <- ts[max(nz)]
    
    extract_hms <- function(x) { m <- regmatches(x, regexpr("\\b\\d{2}:\\d{2}:\\d{2}\\b", x)); if (!length(m)) NA_character_ else m }
    eda_start <- extract_hms(start_raw); eda_stop <- extract_hms(stop_raw)
    
  } else {
    # xlsx/xls
    if (!requireNamespace("readxl", quietly = TRUE)) { warning("readxl not installed for: ", f); return(NULL) }
    df <- tryCatch(readxl::read_excel(f, .name_repair = "minimal"), error = function(e) NULL)
    if (is.null(df) || !nrow(df)) { warning("Could not read EDA xlsx: ", f); return(NULL) }
    # look for time-like column
    cand <- grep("time|timestamp", names(df), ignore.case = TRUE, value = TRUE)
    if (!length(cand)) return(NULL)
    ts <- as.character(df[[cand[1]]])
    nz <- which(!is.na(ts) & nzchar(ts)); if (!length(nz)) return(NULL)
    start_raw <- ts[min(nz)]; stop_raw <- ts[max(nz)]
    eda_start <- str_extract(start_raw, "\\b\\d{2}:\\d{2}:\\d{2}\\b")
    eda_stop  <- str_extract(stop_raw,  "\\b\\d{2}:\\d{2}:\\d{2}\\b")
  }
  
  pid <- suppressWarnings(as.numeric(tools::file_path_sans_ext(basename(f))))
  data.frame(
    participantID = pid,
    EDA.File = f,
    EDA.Start.Clock.Original = eda_start,
    EDA.Stop.Clock.Original  = eda_stop,
    stringsAsFactors = FALSE
  )
}

eda.list   <- lapply(day_eda_files, read_eda_summary) %>% Filter(Negate(is.null), .)
eda.merged <- dplyr::bind_rows(eda.list)

cat("\n[EDA merged] rows:", nrow(eda.merged), "\n")
if (nrow(eda.merged)) print(head(eda.merged, 3))

# --------------------------------------------------------------------
# 3) MIRAGE — scan all CSVs, keep baseline_start/stop for SEARCH_DATE
# --------------------------------------------------------------------
mirage_files <- if (dir.exists(MIRAGE_ROOT))
  list.files(MIRAGE_ROOT, pattern = "(?i)\\.csv$", full.names = TRUE, recursive = TRUE) else character(0)

mirage.merged <- if (length(mirage_files)) mirage_files %>% lapply(read.csv) %>% bind_rows() else data.frame()

mirage.long <- mirage.merged %>%
  filter(grepl("baseline", marker, ignore.case = TRUE)) %>%
  mutate(jotDateTime = ymd_hms(jotDateTime, quiet = TRUE)) %>%
  filter(as_date(jotDateTime) == SEARCH_DATE) %>%
  select(participantID, marker, UTC, jotDateTime)

# clean PID format "PID45001" -> 45001 numeric
mirage.long$participantID <- gsub("PID", "", mirage.long$participantID) %>% as.numeric()
unique.id <- unique(mirage.long$participantID)

cat("\n[MIRAGE] rows:", nrow(mirage.long), " | unique PIDs:", length(unique.id), "\n")

# Split start/stop + pair them
mirage.bl.strt <- mirage.long %>% filter(grepl("baseline_start", marker, ignore.case = TRUE)) %>%
  select(participantID, marker, UTC, jotDateTime)
mirage.bl.stp  <- mirage.long %>% filter(grepl("baseline_stop", marker,  ignore.case = TRUE)) %>%
  select(participantID, marker, UTC, jotDateTime)

mirage.marker.merged <- merge(mirage.bl.strt, mirage.bl.stp, by = "participantID", all = TRUE, sort = FALSE)
colnames(mirage.marker.merged) <- c("participantID", "start.marker", "baseline_start",
                                    "start.jotdatetime", "stop.marker", "baseline_stop",
                                    "stop.jotdatetime")

mirage.markers <- mirage.marker.merged %>%
  transmute(
    participantID,
    start.jotdatetime, baseline_start,
    stop.jotdatetime,  baseline_stop,
    mirage.duration = difftime(stop.jotdatetime, start.jotdatetime, units = "mins")
  ) %>%
  filter(mirage.duration > 4 & mirage.duration < 7)   # keep ~5 min sessions

# detect duplicate starts/stops
mirage.wide <- pivot_wider(mirage.long, names_from = "marker", values_from = c("UTC"), values_fn = list)
dupes <- c()
for (q in seq_len(nrow(mirage.wide))) {
  if (length(unlist(mirage.wide$baseline_start[[q]])) > 1 ||
      length(unlist(mirage.wide$baseline_stop[[q]]))  > 1) dupes <- c(dupes, q)
}
mirage.issue <- mirage.wide[dupes, ]

mirage.correct <- mirage.markers %>%
  group_by(participantID) %>%
  slice_min(abs(as.numeric(mirage.duration, units = "mins") - 5), with_ties = FALSE) %>%
  ungroup() %>%
  arrange(participantID)

# add UTC numeric & clocks
mirage.correct$Mirage.Start.UTC <- as.numeric(mirage.correct$baseline_start)
mirage.correct$Mirage.Stop.UTC  <- as.numeric(mirage.correct$baseline_stop)
mirage.correct <- mirage.correct %>%
  mutate(
    Mirage.Start.Clock.Original = as_datetime(Mirage.Start.UTC/1000),
    Mirage.Stop.Clock.Original  = as_datetime(Mirage.Stop.UTC/1000)
  )

cat("\n[MIRAGE cleaned] rows:", nrow(mirage.correct), " | dupes:", nrow(mirage.issue), "\n")

# --------------------------------------------------------------------
# 4) Merge ECG + EDA + MIRAGE triads
# --------------------------------------------------------------------
ecg_id    <- ecg.index      %>% distinct(participantID, .keep_all = TRUE)
eda_id    <- eda.merged     %>% distinct(participantID, .keep_all = TRUE)
mirage_id <- mirage.correct %>% distinct(participantID, .keep_all = TRUE)

ans.mirage <- ecg_id %>% inner_join(eda_id, by = "participantID") %>% inner_join(mirage_id, by = "participantID")

# Missing sets
ecg_ids    <- unique(ecg_id$participantID)
eda_ids    <- unique(eda_id$participantID)
mirage_ids <- unique(mirage_id$participantID)

missing_ecg_ids    <- setdiff(intersect(eda_ids, mirage_ids), ecg_ids)
missing_eda_ids    <- setdiff(intersect(ecg_ids, mirage_ids), eda_ids)
missing_mirage_ids <- setdiff(intersect(ecg_ids, eda_ids),   mirage_ids)

missing_ecg    <- tibble(participantID = missing_ecg_ids)    %>% left_join(eda_id, by="participantID")    %>% left_join(mirage_id, by="participantID")
missing_eda    <- tibble(participantID = missing_eda_ids)    %>% left_join(ecg_id, by="participantID")    %>% left_join(mirage_id, by="participantID")
missing_mirage <- tibble(participantID = missing_mirage_ids) %>% left_join(ecg_id, by="participantID")    %>% left_join(eda_id, by="participantID")

cat("\n[Triads] complete:", nrow(ans.mirage),
    " | missing ECG:", length(missing_ecg_ids),
    " | missing EDA:", length(missing_eda_ids),
    " | missing MIRAGE:", length(missing_mirage_ids), "\n")

# --------------------------------------------------------------------
# 5) Baseline event-window math (+ timezone nudges)
# --------------------------------------------------------------------
complete.ans.mirage <- ans.mirage  # copy so names match earlier blocks

if (nrow(complete.ans.mirage) > 0) {
  # A) Base durations & dates (UTC)
  addDuration.ECGMirage <- complete.ans.mirage %>%
    mutate(
      ECG.Date    = as.Date(ECG.Start.Clock.Original, tz = "UTC"),
      Mirage.Date = as.Date(Mirage.Start.Clock.Original, tz = "UTC"),
      ECG.Duration.Original    = as.numeric(difftime(ECG.Stop.Clock.Original,    ECG.Start.Clock.Original,    "mins")),
      Mirage.Duration.Original = as.numeric(difftime(Mirage.Stop.Clock.Original, Mirage.Start.Clock.Original, "mins")),
      ECG2Mirage.Duration.Original = as.numeric(difftime(Mirage.Start.Clock.Original, ECG.Start.Clock.Original, "mins"))
    )
  
  # B) EDA to UTC (files recorded in Dhaka, attach ECG.Date then -6h)
  adjust.eda <- addDuration.ECGMirage %>%
    mutate(
      EDA.Start.Clock.Adjusted = ymd_hms(paste(ECG.Date, EDA.Start.Clock.Original), tz = "UTC") - hours(6),
      EDA.Stop.Clock.Adjusted  = ymd_hms(paste(ECG.Date, EDA.Stop.Clock.Original),  tz = "UTC") - hours(6)
    ) %>%
    mutate(
      EDA.Stop.Clock.Adjusted = ifelse(
        EDA.Stop.Clock.Adjusted < EDA.Start.Clock.Adjusted,
        EDA.Stop.Clock.Adjusted + days(1),
        EDA.Stop.Clock.Adjusted
      ) %>% as.POSIXct(origin = "1970-01-01", tz = "UTC")
    )
  
  addDuration.eda <- adjust.eda %>%
    mutate(
      EDA.Duration.Original        = as.numeric(difftime(EDA.Stop.Clock.Adjusted, EDA.Start.Clock.Adjusted, "mins")),
      EDA2Mirage.Duration.Original = round(as.numeric(difftime(Mirage.Start.Clock.Original, EDA.Start.Clock.Adjusted, "mins")), 1),
      Mirage2EDA.Duration.Original = round(as.numeric(difftime(EDA.Stop.Clock.Adjusted,   Mirage.Stop.Clock.Original,  "mins")), 1)
    )
  
  # keep same calendar date
  ANSMIR.clean <- addDuration.eda %>% filter(ECG.Date == Mirage.Date)
  
  # C) Baseline window relative starts/stops (seconds)
  calc.MWStart <- ANSMIR.clean %>%
    mutate(
      ECG.MWStart.Original = as.numeric(difftime(Mirage.Start.Clock.Original, ECG.Start.Clock.Original,  "secs")),
      EDA.MWStart.Original = as.numeric(difftime(Mirage.Start.Clock.Original, EDA.Start.Clock.Adjusted, "secs"))
    )
  calc.MWStop <- calc.MWStart %>%
    mutate(
      ECG.MWStop.Original = ECG.MWStart.Original + (Mirage.Duration.Original * 60),
      EDA.MWStop.Original = EDA.MWStart.Original + (Mirage.Duration.Original * 60)
    )
  
  # D) Fix ECG that’s off by ~6h
  adjust.ecg <- calc.MWStop %>%
    mutate(
      ECG.Start.Clock.Adjusted = case_when(
        ECG.MWStart.Original < 0 ~ ymd_hms(ECG.Start.Clock.Original) - hours(6),
        TRUE ~ ECG.Start.Clock.Original
      ),
      ECG.Stop.Clock.Adjusted = case_when(
        ECG.MWStop.Original < 0 ~ ymd_hms(ECG.Stop.Clock.Original) - hours(6),
        TRUE ~ ECG.Stop.Clock.Original
      )
    )
  
  # E) Nudge MIRAGE when slightly ahead of EDA
  adjust.mirage <- adjust.ecg %>%
    mutate(
      Mirage.Start.Clock.Adjusted = case_when(
        between(Mirage2EDA.Duration.Original, -30.01, -1.01) ~ ymd_hms(Mirage.Start.Clock.Original) - minutes(30),
        between(Mirage2EDA.Duration.Original,  -1.00, -0.02) ~ ymd_hms(Mirage.Start.Clock.Original) - minutes(1),
        TRUE ~ Mirage.Start.Clock.Original
      ),
      Mirage.Stop.Clock.Adjusted = case_when(
        between(Mirage2EDA.Duration.Original, -30.01, -1.01) ~ ymd_hms(Mirage.Stop.Clock.Original) - minutes(30),
        between(Mirage2EDA.Duration.Original,  -1.00, -0.02) ~ ymd_hms(Mirage.Stop.Clock.Original) - minutes(1),
        TRUE ~ Mirage.Stop.Clock.Original
      )
    )
  
  # F) Recompute after adjustments
  recalc.MWStart <- adjust.mirage %>%
    mutate(
      ECG.MWStart.Adjusted = as.numeric(difftime(Mirage.Start.Clock.Adjusted, ECG.Start.Clock.Adjusted, "secs")),
      EDA.MWStart.Adjusted = as.numeric(difftime(Mirage.Start.Clock.Adjusted, EDA.Start.Clock.Adjusted, "secs")),
      ECG.Duration.Adjusted    = as.numeric(difftime(ECG.Stop.Clock.Adjusted,    ECG.Start.Clock.Adjusted,    "mins")),
      EDA.Duration.Adjusted    = as.numeric(difftime(EDA.Stop.Clock.Adjusted,    EDA.Start.Clock.Adjusted,    "mins")),
      Mirage.Duration.Adjusted = as.numeric(difftime(Mirage.Stop.Clock.Adjusted, Mirage.Start.Clock.Adjusted, "mins")),
      ECG2Mirage.Duration.Adjusted = as.numeric(difftime(Mirage.Start.Clock.Adjusted, ECG.Start.Clock.Adjusted, "mins")),
      EDA2Mirage.Duration.Adjusted = as.numeric(difftime(Mirage.Start.Clock.Adjusted, EDA.Start.Clock.Adjusted, "mins")),
      Mirage2EDA.Duration.Adjusted = as.numeric(difftime(Mirage.Stop.Clock.Adjusted,  EDA.Stop.Clock.Adjusted,  "mins"))
    )
  recalc.MWStop <- recalc.MWStart %>%
    mutate(
      ECG.MWStop.Adjusted = ECG.MWStart.Adjusted + (Mirage.Duration.Adjusted * 60),
      EDA.MWStop.Adjusted = EDA.MWStart.Adjusted + (Mirage.Duration.Adjusted * 60)
    )
  
  # G) QC
  recalc.clean <- recalc.MWStop %>%
    filter(ECG.Duration.Adjusted > 8, ECG.Duration.Adjusted < 60,
           ECG2Mirage.Duration.Adjusted < 120) %>%
    mutate(
      ECG.MWStart.Adjusted = round(ECG.MWStart.Adjusted, 3),
      ECG.MWStop.Adjusted  = round(ECG.MWStop.Adjusted,  3),
      EDA.MWStart.Adjusted = round(EDA.MWStart.Adjusted, 3),
      EDA.MWStop.Adjusted  = round(EDA.MWStop.Adjusted,  3)
    )
  
  MWTimes.adjusted <- recalc.clean %>%
    select(
      Mirage.Date, participantID,
      ECG.MWStart.Adjusted, ECG.MWStop.Adjusted,
      EDA.MWStart.Adjusted, EDA.MWStop.Adjusted,
      ECG.Start.Clock.Adjusted, EDA.Start.Clock.Adjusted,
      Mirage.Start.Clock.Adjusted, Mirage.Stop.Clock.Adjusted,
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
  MWTimes.adjusted <- MWTimes.original <- MWTimes.review <- complete.ans.mirage
}

# --------------------------------------------------------------------
# 6) Export workbook (same naming & Month folder convention)
# --------------------------------------------------------------------
search_date_label <- format(SEARCH_DATE, "%Y-%m-%d")
month_year_label  <- format(SEARCH_DATE, "%B %Y")

dir.create(file.path("output", "Recruitment", month_year_label), recursive = TRUE, showWarnings = FALSE)
out_path <- file.path("output", "Recruitment", month_year_label,
                      paste0(search_date_label, " MW Baseline Event Marker Times ", Sys.Date(), ".xlsx"))

export.list <- list(
  "Adjusted"                      = MWTimes.adjusted,
  "Needs Review"                  = MWTimes.review,
  "Original"                      = MWTimes.original,
  "Duplicate or Missing Mirage"   = mirage.issue,
  "Mirage does not exist"         = missing_mirage,
  "EDA file does not exist"       = missing_eda,
  "ECG file does not exist"       = missing_ecg
)

openxlsx::write.xlsx(export.list, file = out_path, overwrite = TRUE)
cat("\nFile written to:", normalizePath(out_path, winslash = "/"), "\n")
# =====================================================================