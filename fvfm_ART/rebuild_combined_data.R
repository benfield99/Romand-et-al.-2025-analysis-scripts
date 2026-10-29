# ==============================================================================
# Figures 3C-D and S6 - Rebuild combinedresults_normalized.txt from the per-line FluorCam files
# Input:   per_line_data/<replicate>/<time>_<line>.TXT, one file per time point and line
#          (FluorCam 7 numeric export for one row of leaves: rows = variables, columns = leaves)
# Output:  combinedresults_normalized_rebuilt.txt, compared with combinedresults_normalized.txt
# Method:  for each leaf, Size, Fo, Fm, Fv and QY_max are read and Fv_Fm = Fv / Fm is computed.
#          Leaves are numbered in plate order (measure_1, measure_2, ...).
#          Normalisation: value / mean of the same variable at T0 for the same line and replicate
#          (average_t0 and normalized_value for Fv/Fm; Fo_average_t0, Fo_norm, Fm_average_t0, Fm_norm).
# Run:     with the working directory set to this folder.
# ==============================================================================

# ---- User settings -----------------------------------------------------------
# Per-line files of each experimental replicate
EXPERIMENTS <- c(
  rep1 = "per_line_data/rep1_2022_05_03",
  rep2 = "per_line_data/rep2_2022_09_06",
  rep3 = "per_line_data/rep3_2022_04_26"
)

# Variables read from the FluorCam files (Fv_Fm is computed as Fv / Fm)
VARS <- c("Size", "Fo", "Fm", "Fv", "QY_max")

# Variables normalised to the T0 mean, in addition to Fv_Fm
NORM_VARS <- c("Fo", "Fm")

# Time codes in the file names -> time codes in the data file
TIME_MAP <- c("T0" = "T0", "T2H-HL" = "T2h-HL", "T0.5-R" = "T0.5-r",
              "T2H-R" = "T2h-r", "T4H-R" = "T4h-r")

OUT_FILE   <- "combinedresults_normalized_rebuilt.txt"
CHECK_FILE <- "combinedresults_normalized.txt"

# ---- Read one per-line file: one row per leaf --------------------------------
read_per_line <- function(file) {
  lines <- readLines(file, warn = FALSE)[-1]          # first line = area names
  fields <- strsplit(lines, "\t", fixed = TRUE)
  row_names <- sapply(fields, `[`, 1)
  row_names[row_names %in% c("Size [pixels]", "Size")] <- "Size"
  get_row <- function(nm) {
    x <- fields[[match(nm, row_names)]][-1]
    suppressWarnings(as.numeric(x[nzchar(trimws(x))]))
  }
  df <- as.data.frame(lapply(setNames(VARS, VARS), get_row))
  df$Fv_Fm <- df$Fv / df$Fm
  df
}

# ---- Read all files ----------------------------------------------------------
df_all <- do.call(rbind, lapply(names(EXPERIMENTS), function(rep) {
  files <- list.files(EXPERIMENTS[[rep]], pattern = "^T[0-9].*_.*\\.TXT$")
  do.call(rbind, lapply(files, function(f) {
    nm <- sub("\\.TXT$", "", f)
    time_raw <- toupper(sub("_.*$", "", nm))
    line <- tolower(sub("^[^_]*_", "", nm))
    if (line == "wt") line <- "WT"
    if (!time_raw %in% names(TIME_MAP)) stop("Unknown time point in file name: ", f)
    d <- read_per_line(file.path(EXPERIMENTS[[rep]], f))
    data.frame(time = TIME_MAP[[time_raw]], line = line, rep = rep,
               measurement = paste0("measure_", seq_len(nrow(d))), d,
               stringsAsFactors = FALSE)
  }))
}))

# ---- Normalise to the T0 mean of each line x replicate -----------------------
t0_mean <- function(var) {
  is_t0 <- df_all$time == "T0"
  means <- tapply(df_all[[var]][is_t0], paste(df_all$line, df_all$rep)[is_t0], mean, na.rm = TRUE)
  as.numeric(means[paste(df_all$line, df_all$rep)])
}

df_all$average_t0 <- t0_mean("Fv_Fm")
df_all$normalized_value <- df_all$Fv_Fm / df_all$average_t0
for (v in NORM_VARS) {
  df_all[[paste0(v, "_average_t0")]] <- t0_mean(v)
  df_all[[paste0(v, "_norm")]] <- df_all[[v]] / df_all[[paste0(v, "_average_t0")]]
}

col_order <- c("time", "line", "rep", "measurement", "Fv_Fm", "average_t0", "normalized_value",
               unlist(lapply(NORM_VARS, function(v) c(v, paste0(v, "_average_t0"), paste0(v, "_norm")))),
               setdiff(VARS, NORM_VARS))
df_all <- df_all[, col_order]

write.table(df_all, file = OUT_FILE, sep = "\t", quote = FALSE, row.names = FALSE)
cat("Wrote ", OUT_FILE, ": ", nrow(df_all), " leaf measurements\n", sep = "")

# ---- Compare with the data file used by the figures --------------------------
if (file.exists(CHECK_FILE)) {
  ref <- read.table(CHECK_FILE, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  key_ref <- paste(ref$time, ref$line, ref$rep, ref$measurement)
  key_new <- paste(df_all$time, df_all$line, df_all$rep, df_all$measurement)
  m <- match(key_ref, key_new)
  same <- nrow(ref) == nrow(df_all) && !anyNA(m) && identical(names(ref), names(df_all)) &&
    all(sapply(names(ref)[-(1:4)], function(col) isTRUE(all.equal(ref[[col]], df_all[[col]][m], tolerance = 1e-12))))
  cat("Comparison with ", CHECK_FILE, ": ", if (same) "identical values" else "*** DIFFERENT ***", "\n", sep = "")
}
