# ================================================================
# Birth Follow-up ANS processing (Mothers & Children)
#   • MIRAGE-first: clean baseline, then pick ECG by closest start time
#   • Mother + Child (child skipped for dates <= 2024-10-05)
#   • EDA only for Mothers (per EDA_ROOT)
# ================================================================
source("R/birth-follow-up-header.R")
# ------------------------------------------------
# 1) Utilities
# ------------------------------------------------

# Extract 5-digit PID & role from an arbitrary path string.
# Role resolution order:
#  - explicit folder hints ("child"/"mother")
#  - a 5-digit number followed by M/C in a path segment (e.g., "48037C_...") 
#  - otherwise NA (role unknown)
.extract_pid_role_from_path <- function(p) {
  # default
  role <- NA_character_
  
  # folder hints
  if (grepl("(?i)child", p))  role <- "C"
  if (grepl("(?i)mother", p)) role <- "M"
  
  # PID: last 5-digit block in path
  pid_num <- stringr::str_extract_all(p, "\\d{5}")[[1]]
  pid     <- if (length(pid_num)) as.numeric(tail(pid_num, 1)) else NA_real_
  
  # Trailing letter M/C next to a 5-digit block in any segment
  mc_match <- stringr::str_match_all(p, "(\\d{5})([MC])")[[1]]
  if (nrow(mc_match)) {
    # take the last match as "closest" to file
    role <- mc_match[nrow(mc_match), 3]
    pid  <- as.numeric(mc_match[nrow(mc_match), 2])
  }
  
  list(pid = pid, role = role)
}

# Extract 5-digit PID only (fallback)
.extract_pid_from_path <- function(p) {
  pid_num <- stringr::str_extract_all(p, "\\d{5}")[[1]]
  if (length(pid_num)) as.numeric(tail(pid_num, 1)) else NA_real_
}

# Decide epoch unit (s/ms/us) from the median magnitude
.guess_epoch_scale <- function(x_num) {
  m <- suppressWarnings(stats::median(as.numeric(x_num), na.rm = TRUE))
  if (!is.finite(m)) return(1)
  if (m > 1e14) return(1e6)  # microseconds
  if (m > 1e11) return(1e3)  # milliseconds
  1                           # seconds
}

# Read a Firstbeat ECG CSV and return one summary row
.summarize_ecg_file <- function(fp) {
  pid_role <- .extract_pid_role_from_path(fp)
  pid <- pid_role$pid
  role <- pid_role$role
  
  tryCatch({
    ecg.data <- suppressWarnings(read.csv(fp, skip = 11, header = TRUE, check.names = FALSE))
    if (!nrow(ecg.data)) stop("no rows after skip=11")
    
    nm <- names(ecg.data)
    if (length(nm) >= 1) nm[1] <- "timestamp"
    if (length(nm) >= 2) nm[2] <- "ecg"
    if (length(nm) >= 3) nm[3] <- "ecg_mV"
    names(ecg.data) <- nm
    
    scale <- .guess_epoch_scale(ecg.data$timestamp)
    
    ts_start_raw <- suppressWarnings(as.numeric(ecg.data$timestamp[1]))
    ts_end_raw   <- suppressWarnings(as.numeric(ecg.data$timestamp[nrow(ecg.data)]))
    
    start_clock_utc <- lubridate::as_datetime(ts_start_raw / scale, tz = "UTC")
    stop_clock_utc  <- lubridate::as_datetime(ts_end_raw   / scale, tz = "UTC")
    
    tibble::tibble(
      participantID = pid,
      role          = role,
      ECG.File      = fp,
      ECG.Size.KB   = round(file.info(fp)$size / 1024, 2),
      ECG.Start.UTC = ts_start_raw,
      ECG.Stop.UTC  = ts_end_raw,
      ECG.Start.Clock.Original = start_clock_utc,
      ECG.Stop.Clock.Original  = stop_clock_utc,
      ECG.Start.Clock.Dhaka    = with_tz(start_clock_utc, "Asia/Dhaka"),
      ECG.Stop.Clock.Dhaka     = with_tz(stop_clock_utc,  "Asia/Dhaka"),
      ECG.Duration.Min         = as.numeric(difftime(stop_clock_utc, start_clock_utc, units = "mins")),
      ECG.Sample.Count         = nrow(ecg.data),
      .error = NA_character_
    )
  }, error = function(e) {
    tibble::tibble(
      participantID = pid, role = role, ECG.File = fp,
      ECG.Size.KB = round(file.info(fp)$size / 1024, 2),
      ECG.Start.UTC = NA_real_, ECG.Stop.UTC = NA_real_,
      ECG.Start.Clock.Original = as.POSIXct(NA),
      ECG.Stop.Clock.Original  = as.POSIXct(NA),
      ECG.Start.Clock.Dhaka    = as.POSIXct(NA),
      ECG.Stop.Clock.Dhaka     = as.POSIXct(NA),
      ECG.Duration.Min = NA_real_, ECG.Sample.Count = NA_integer_,
      .error = paste0("read_failed: ", conditionMessage(e))
    )
  })
}

# EDA reader (robust to metadata rows & separators); returns HH:MM:SS + PID
# --- EDA readers: CSV and Excel, PID = filename, role = 'M' ---

.read_eda_summary_csv <- function(f) {
  lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
  if (!length(lines)) return(NULL)
  
  # Find header row
  hdr_idx <- grep("^\\s*SECOND[;,\\t]", lines, perl = TRUE)
  if (!length(hdr_idx)) hdr_idx <- grep("TIMESTAMP|^\\s*TIME\\b", lines, ignore.case = TRUE)
  if (!length(hdr_idx)) return(NULL)
  hdr_idx <- hdr_idx[1]
  
  # Detect delimiter
  hdr_line <- lines[hdr_idx]
  sep <- if (grepl(";", hdr_line)) ";" else if (grepl(",", hdr_line)) "," else "\t"
  
  # Read table
  df <- tryCatch(
    read.table(f, header = TRUE, sep = sep, skip = hdr_idx - 1,
               quote = "", comment.char = "", fill = TRUE, stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  if (is.null(df) || !nrow(df)) return(NULL)
  
  # Choose a time column
  ts_col <- intersect(c("TIMESTAMP","Timestamp","TIME","Time","SECOND","Second"), names(df))
  if (!length(ts_col)) return(NULL)
  ts <- as.character(df[[ts_col[1]]])
  
  nz <- which(!is.na(ts) & nzchar(ts))
  if (!length(nz)) return(NULL)
  
  extract_hms <- function(x) {
    m <- regmatches(x, regexpr("\\b\\d{2}:\\d{2}:\\d{2}\\b", x))
    if (!length(m)) NA_character_ else m
  }
  
  start_raw <- ts[min(nz)]
  stop_raw  <- ts[max(nz)]
  pid <- suppressWarnings(as.numeric(tools::file_path_sans_ext(basename(f))))
  
  data.frame(
    participantID = pid, role = "M", EDA.File = f,
    EDA.Start.Clock.Original = extract_hms(start_raw),
    EDA.Stop.Clock.Original  = extract_hms(stop_raw),
    stringsAsFactors = FALSE
  )
}

.read_eda_summary_xlsx <- function(f) {
  # Peek raw to find the header row
  raw <- tryCatch(readxl::read_excel(f, sheet = 1, col_names = FALSE), error = function(e) NULL)
  if (is.null(raw) || !nrow(raw)) return(NULL)
  
  has_hdr <- apply(raw, 1, function(r)
    any(grepl("^(SECOND|TIMESTAMP|TIME)$", as.character(r), ignore.case = TRUE))
  )
  hdr_idx <- which(has_hdr)
  if (!length(hdr_idx)) return(NULL)
  hdr_idx <- hdr_idx[1]
  
  df <- tryCatch(readxl::read_excel(f, sheet = 1, skip = hdr_idx - 1, col_names = TRUE),
                 error = function(e) NULL)
  if (is.null(df) || !nrow(df)) return(NULL)
  
  ts_col <- intersect(c("TIMESTAMP","Timestamp","TIME","Time","SECOND","Second"), names(df))
  if (!length(ts_col)) return(NULL)
  ts <- as.character(df[[ts_col[1]]])
  
  nz <- which(!is.na(ts) & nzchar(ts))
  if (!length(nz)) return(NULL)
  
  extract_hms <- function(x) {
    m <- regmatches(x, regexpr("\\b\\d{2}:\\d{2}:\\d{2}\\b", x))
    if (!length(m)) NA_character_ else m
  }
  
  start_raw <- ts[min(nz)]
  stop_raw  <- ts[max(nz)]
  pid <- suppressWarnings(as.numeric(tools::file_path_sans_ext(basename(f))))
  
  data.frame(
    participantID = pid, role = "M", EDA.File = f,
    EDA.Start.Clock.Original = extract_hms(start_raw),
    EDA.Stop.Clock.Original  = extract_hms(stop_raw),
    stringsAsFactors = FALSE
  )
}

.read_eda_summary <- function(f) {
  ext <- tolower(tools::file_ext(f))
  if (ext %in% c("xlsx","xls")) return(.read_eda_summary_xlsx(f))
  .read_eda_summary_csv(f)
}

# ------------------------------------------------
# 2) MIRAGE — read, filter to date, clean baseline
# ------------------------------------------------
cat("\n--- [Processing] MIRAGE (clean baseline, pick ~5 min) ---\n")

mirage_files <- list.files(MIRAGE_ROOT, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
if (!length(mirage_files)) stop("No MIRAGE CSV files found under: ", MIRAGE_ROOT)

mirage_all <- mirage_files %>%
  lapply(read.csv, stringsAsFactors = FALSE) %>%
  bind_rows()

# Derive participantID (numeric) and role (M/C) if present
mirage_all <- mirage_all %>%
  mutate(
    pid_raw = gsub("PID", "", as.character(participantID)),
    participantID = suppressWarnings(as.numeric(stringr::str_extract(pid_raw, "\\d{5}"))),
    role = {
      r <- stringr::str_extract(pid_raw, "[MC]$")
      r[is.na(r)] <- NA_character_; r
    },
    jot_dt = ymd_hms(jotDateTime, quiet = TRUE, tz = "UTC")
  )

# Filter date + baseline markers
mirage_long <- mirage_all %>%
  filter(!is.na(jot_dt)) %>%
  filter(year(jot_dt)  == year(search_date),
         month(jot_dt) == month(search_date),
         day(jot_dt)   == day(search_date)) %>%
  filter(grepl("baseline", marker, ignore.case = TRUE)) %>%
  select(participantID, role, marker, UTC, jot_dt)

# If skipping child for this date, drop C rows now
if (!process_child) {
  mirage_long <- mirage_long %>% filter(is.na(role) | role != "C")
  cat("NOTE: Child processing is disabled for ", as.character(search_date), " per rule (<= 2024-10-05).\n", sep = "")
}

# Split start vs stop
mir_start <- mirage_long %>%
  filter(grepl("baseline_start", marker, ignore.case = TRUE)) %>%
  select(participantID, role, marker, UTC_start = UTC, jot_start = jot_dt)

mir_stop <- mirage_long %>%
  filter(grepl("baseline_stop", marker, ignore.case = TRUE)) %>%
  select(participantID, role, marker, UTC_stop = UTC, jot_stop = jot_dt)

# Merge and compute durations
mir_merged <- full_join(mir_start %>% select(-marker),
                        mir_stop  %>% select(-marker),
                        by = c("participantID","role"))

mir_markers <- mir_merged %>%
  mutate(
    Mirage.Start.UTC = as.numeric(UTC_start),
    Mirage.Stop.UTC  = as.numeric(UTC_stop),
    Mirage.Start.Clock.Original = as_datetime(Mirage.Start.UTC/1000, tz="UTC"),
    Mirage.Stop.Clock.Original  = as_datetime(Mirage.Stop.UTC/1000, tz="UTC"),
    mirage.duration = as.numeric(difftime(Mirage.Stop.Clock.Original, Mirage.Start.Clock.Original, units="mins"))
  ) %>%
  # keep plausible 4–7 min blocks
  filter(!is.na(mirage.duration),
         mirage.duration > 4, mirage.duration < 7)

# Handle duplicates: choose duration closest to 5 min per (PID, role)
mirage.correct <- mir_markers %>%
  group_by(participantID, role) %>%
  slice_min(abs(mirage.duration - 5), with_ties = FALSE) %>%
  ungroup() %>%
  arrange(participantID, role)

# For review: any (PID,role) with >1 start/stop pair before dedupe
mirage.issue <- mir_markers %>%
  add_count(participantID, role, name = "n_per_pid_role") %>%
  filter(n_per_pid_role > 1)

cat("MIRAGE sessions kept: ", nrow(mirage.correct), " | duplicates flagged: ", nrow(mirage.issue), "\n", sep = "")

# ------------------------------------------------
# 3) ECG — find candidates under the date; summarize
#     We do NOT size-filter; we will choose by closest start to MIRAGE
# ------------------------------------------------
cat("\n--- [Processing] ECG (choose file by closest start to MIRAGE) ---\n")

if (!dir.exists(ecg_date_path)) stop("ECG date folder not found: ", ecg_date_path)

ecg_candidates <- list.files(ecg_date_path,
                             pattern = "(?i)(-|_)ecg(\\.csv)?$",
                             full.names = TRUE, recursive = TRUE)

if (!length(ecg_candidates)) {
  warning("No ECG files found under: ", ecg_date_path)
}

ecg.index <- ecg_candidates %>%
  lapply(.summarize_ecg_file) %>%
  bind_rows()

# Keep only good rows
ecg.errors <- ecg.index %>% filter(!is.na(.error) |
                                     if_any(c(ECG.Start.Clock.Original, ECG.Stop.Clock.Original), is.na))
ecg.index  <- ecg.index  %>% filter(is.na(.error)) %>%
  filter(!is.na(participantID))

cat("ECG files processed: ", nrow(ecg.index), " | with errors: ", nrow(ecg.errors), "\n", sep="")

# ------------------------------------------------
# 4) Pick, for each (PID, role) from MIRAGE, the ECG file whose
#    start is closest to the MIRAGE start (also test -6h shift)
# ------------------------------------------------
pick_closest_ecg <- function(mdf, ecg_df) {
  # mdf: mirage.correct for one (participantID, role)
  # returns one row from ecg_df joined with delta info
  pid  <- mdf$participantID[1]
  role <- mdf$role[1]
  
  # restrict ECG candidates to the same PID; if role is known, try to match it
  cand <- ecg_df %>% filter(participantID == pid)
  if (!is.na(role)) {
    role_cand <- cand %>% filter(is.na(role) | role == !!role)
    if (nrow(role_cand)) cand <- role_cand
  }
  if (!nrow(cand)) return(NULL)
  
  ms <- mdf$Mirage.Start.Clock.Original[1]
  
  cand <- cand %>%
    mutate(
      # delta with original ECG clock
      delta_min_raw = abs(as.numeric(difftime(ECG.Start.Clock.Original, ms, units="mins"))),
      # delta if ECG was mis-tagged by +6h (so shift -6h)
      ECG.Start.Clock.Shifted = ymd_hms(ECG.Start.Clock.Original) - hours(6),
      delta_min_shift = abs(as.numeric(difftime(ECG.Start.Clock.Shifted, ms, units="mins"))),
      # choose best of the two
      delta_min = pmin(delta_min_raw, delta_min_shift),
      used_shift = ifelse(delta_min_shift < delta_min_raw, TRUE, FALSE),
      ECG.Start.Clock.Selected = ifelse(used_shift, ECG.Start.Clock.Shifted, ECG.Start.Clock.Original) %>% as.POSIXct(origin="1970-01-01", tz="UTC")
    ) %>%
    arrange(delta_min)
  
  cand %>% slice(1)
}

ecg.selected <- mirage.correct %>%
  group_split(participantID, role, .keep = TRUE) %>%
  map_dfr(~ pick_closest_ecg(.x, ecg.index))

# If any MIRAGE rows found no ECG, they’ll be reported later as missing_ecg
cat("ECG files selected by proximity: ", nrow(ecg.selected), "\n", sep="")

# ------------------------------------------------
# 5) EDA (Mothers only) — summarize start/stop HH:MM:SS
# ------------------------------------------------
cat("\n--- [Processing] EDA (mothers) ---\n")

eda.merged <- data.frame()

if (dir.exists(eda_date_path)) {
  eda_files <- list.files(
    eda_date_path,
    pattern = "\\.(csv|xlsx?|CSV)$",
    full.names = TRUE, recursive = TRUE, ignore.case = TRUE
  )
  if (length(eda_files)) {
    eda_list <- lapply(eda_files, .read_eda_summary) %>% Filter(Negate(is.null), .)
    if (length(eda_list)) eda.merged <- dplyr::bind_rows(eda_list)
  }
} else {
  warning("EDA date folder not found: ", eda_date_path)
}

cat("EDA rows merged: ", nrow(eda.merged), "\n", sep = "")

# ------------------------------------------------
# 6) Join (ECG + EDA + MIRAGE)
#     • Mother rows should have all three (inner-join triad)
#     • Child rows will typically miss EDA → show under 'missing EDA'
# ------------------------------------------------
mirage_id <- mirage.correct %>% distinct(participantID, role, .keep_all = TRUE)
ecg_id    <- ecg.selected  %>% distinct(participantID, role, .keep_all = TRUE)
eda_id    <- eda.merged    %>% distinct(participantID, role, .keep_all = TRUE)

# Inner join (triads)
ans.mirage <- ecg_id %>%
  inner_join(eda_id,    by = c("participantID","role")) %>%
  inner_join(mirage_id, by = c("participantID","role"))

# Identify missing sets (helpful tabs)
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

cat("\nTriads found (ECG+EDA+MIRAGE): ", nrow(ans.mirage), "\n", sep="")

# ------------------------------------------------
# 7) Baseline event-window math  (same as recruitment, now with role)
#     Only runs on triads (mostly Mothers). Children will land in missing EDA.
# ------------------------------------------------
complete.ans.mirage <- ans.mirage

if (nrow(complete.ans.mirage) > 0) {
  # A — base durations/dates
  addDuration.ECGMirage <- complete.ans.mirage %>%
    mutate(
      ECG.Date    = as.Date(ECG.Start.Clock.Original,  tz = "UTC"),
      Mirage.Date = as.Date(Mirage.Start.Clock.Original, tz = "UTC"),
      ECG.Duration.Original    = as.numeric(difftime(ECG.Stop.Clock.Original,    ECG.Start.Clock.Original,    units = "mins")),
      Mirage.Duration.Original = as.numeric(difftime(Mirage.Stop.Clock.Original, Mirage.Start.Clock.Original, units = "mins")),
      ECG2Mirage.Duration.Original = as.numeric(difftime(Mirage.Start.Clock.Original, ECG.Start.Clock.Original, units = "mins"))
    )
  
  # B — bring EDA to UTC using ECG.Date (EDA HH:MM:SS is Dhaka local)
  adjust.eda <- addDuration.ECGMirage %>%
    mutate(
      EDA.Start.Clock.Adjusted = ymd_hms(paste(ECG.Date, EDA.Start.Clock.Original), tz="UTC") - hours(6),
      EDA.Stop.Clock.Adjusted  = ymd_hms(paste(ECG.Date, EDA.Stop.Clock.Original),  tz="UTC") - hours(6)
    ) %>%
    mutate(
      EDA.Stop.Clock.Adjusted = ifelse(
        EDA.Stop.Clock.Adjusted < EDA.Start.Clock.Adjusted,
        EDA.Stop.Clock.Adjusted + days(1),
        EDA.Stop.Clock.Adjusted
      ) %>% as.POSIXct(origin="1970-01-01", tz="UTC")
    )
  
  addDuration.eda <- adjust.eda %>%
    mutate(
      EDA.Duration.Original        = as.numeric(difftime(EDA.Stop.Clock.Adjusted, EDA.Start.Clock.Adjusted, units = "mins")),
      EDA2Mirage.Duration.Original = round(as.numeric(difftime(Mirage.Start.Clock.Original, EDA.Start.Clock.Adjusted, units = "mins")), 1),
      Mirage2EDA.Duration.Original = round(as.numeric(difftime(Mirage.Stop.Clock.Original,  EDA.Stop.Clock.Adjusted,  units = "mins")), 1)
    )
  
  ANSMIR.clean <- addDuration.eda %>% filter(ECG.Date == Mirage.Date)
  
  # C — event window secs relative to file starts
  calc.MWStart <- ANSMIR.clean %>% mutate(
    ECG.MWStart.Original = as.numeric(difftime(Mirage.Start.Clock.Original, ECG.Start.Clock.Original,  units = "secs")),
    EDA.MWStart.Original = as.numeric(difftime(Mirage.Start.Clock.Original, EDA.Start.Clock.Adjusted, units = "secs"))
  )
  
  calc.MWStop <- calc.MWStart %>%
    mutate(
      ECG.MWStop.Original = ECG.MWStart.Original + (Mirage.Duration.Original * 60),
      EDA.MWStop.Original = EDA.MWStart.Original + (Mirage.Duration.Original * 60)
    )
  
  # D — fix ECG off by +6h (negative MW)
  adjust.ecg <- calc.MWStop %>%
    mutate(
      ECG.Start.Clock.Adjusted = case_when(
        ECG.MWStart.Original < 0 ~ ymd_hms(ECG.Start.Clock.Original) - hours(6),
        TRUE                     ~ ECG.Start.Clock.Original
      ),
      ECG.Stop.Clock.Adjusted = case_when(
        ECG.MWStop.Original < 0 ~ ymd_hms(ECG.Stop.Clock.Original) - hours(6),
        TRUE                    ~ ECG.Stop.Clock.Original
      )
    )
  
  # E — nudge MIRAGE vs EDA when slightly ahead
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
  
  # F — recompute after adjustments
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
  
  # G — QC
  recalc.clean <- recalc.MWStop %>%
    filter(
      ECG.Duration.Adjusted > 8, ECG.Duration.Adjusted < 60,
      ECG2Mirage.Duration.Adjusted < 120
    ) %>%
    mutate(
      ECG.MWStart.Adjusted = round(ECG.MWStart.Adjusted, 3),
      ECG.MWStop.Adjusted  = round(ECG.MWStop.Adjusted,  3),
      EDA.MWStart.Adjusted = round(EDA.MWStart.Adjusted, 3),
      EDA.MWStop.Adjusted  = round(EDA.MWStop.Adjusted,  3)
    )
  
  MWTimes.adjusted <- recalc.clean %>%
    select(
      Mirage.Date, participantID, role,
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
      Mirage.Date, participantID, role,
      ECG.MWStart.Original, ECG.MWStop.Original,
      EDA.MWStart.Original, EDA.MWStop.Original,
      ECG.Start.Clock.Original, EDA.Start.Clock.Original, 
      Mirage.Start.Clock.Original, Mirage.Stop.Clock.Original,
      EDA.Stop.Clock.Original,  ECG.Stop.Clock.Original,
      Mirage.Start.UTC, Mirage.Stop.UTC,
      ECG.Duration.Original, EDA.Duration.Original, Mirage.Duration.Original,
      ECG2Mirage.Duration.Original, EDA2Mirage.Duration.Original, 
      Mirage2EDA.Duration.Original,
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
      Mirage.Date, participantID, role,
      ECG.MWStart.Adjusted, ECG.MWStop.Adjusted,
      EDA.MWStart.Adjusted, EDA.MWStop.Adjusted,
      ECG.Start.Clock.Adjusted, EDA.Start.Clock.Adjusted, 
      Mirage.Start.Clock.Adjusted, Mirage.Stop.Clock.Adjusted,
      EDA.Stop.Clock.Adjusted,  ECG.Stop.Clock.Adjusted,
      ECG.Duration.Adjusted, EDA.Duration.Adjusted, Mirage.Duration.Adjusted,
      ECG2Mirage.Duration.Adjusted, EDA2Mirage.Duration.Adjusted, 
      Mirage2EDA.Duration.Adjusted,
      ECG.File, EDA.File
    ) %>%
    filter(
      ECG.MWStart.Adjusted < 0 | ECG.MWStop.Adjusted < 0 |
        EDA.MWStart.Adjusted < 0 | EDA.MWStop.Adjusted < 0 |
        Mirage2EDA.Duration.Adjusted < -30.02 | Mirage2EDA.Duration.Adjusted > 10
    ) %>%
    rename(
      ECG.MWStart.Review = ECG.MWStart.Adjusted,
      ECG.MWStop.Review  = ECG.MWStop.Adjusted,
      EDA.MWStart.Review = EDA.MWStart.Adjusted,
      EDA.MWStop.Review  = EDA.MWStop.Adjusted,
      ECG.Start.Clock.Review    = ECG.Start.Clock.Adjusted,
      EDA.Start.Clock.Review    = EDA.Start.Clock.Adjusted,
      Mirage.Start.Clock.Review = Mirage.Start.Clock.Adjusted,
      Mirage.Stop.Clock.Review  = Mirage.Stop.Clock.Adjusted,
      EDA.Stop.Clock.Review     = EDA.Stop.Clock.Adjusted,
      ECG.Stop.Clock.Review     = ECG.Stop.Clock.Adjusted,
      ECG.Duration.Review       = ECG.Duration.Adjusted,
      EDA.Duration.Review       = EDA.Duration.Adjusted,
      Mirage.Duration.Review    = Mirage.Duration.Adjusted,
      ECG2Mirage.Duration.Review = ECG2Mirage.Duration.Adjusted,
      EDA2Mirage.Duration.Review = EDA2Mirage.Duration.Adjusted,
      Mirage2EDA.Duration.Review = Mirage2EDA.Duration.Adjusted
    )
} else {
  MWTimes.adjusted <- MWTimes.original <- MWTimes.review <- complete.ans.mirage
}

# ------------------------------------------------
# 8) Export workbook
# ------------------------------------------------
export.list <- list(
  "Adjusted"                        = MWTimes.adjusted,
  "Needs Review"                    = MWTimes.review,
  "Original"                        = MWTimes.original,
  "Duplicate or Missing Mirage"     = mirage.issue,
  "Mirage does not exist"           = missing_mirage,
  "EDA file does not exist"         = missing_eda,
  "ECG file does not exist"         = missing_ecg,
  "ECG selected (by proximity)"     = ecg.selected
)

month_year_label  <- format(search_date, "%B %Y")
search_date_label <- format(search_date, "%Y-%m-%d")

dir.create(file.path("output", "Birth Follow up", month_year_label), recursive = TRUE, showWarnings = FALSE)

out_path <- file.path(
  "output", "Birth Follow up", month_year_label,
  paste0(search_date_label, " MW Baseline Event Marker Times (Birth Follow up) ", Sys.Date(), ".xlsx")
)

openxlsx::write.xlsx(export.list, file = out_path, overwrite = TRUE)
cat("\nFile written to: ", normalizePath(out_path), "\n", sep = "")

