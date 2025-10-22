#############################
# 77 K chlorophyll spectra — qGAM analysis
# Author: Ben Field
# - Load multiple CSV runs per line (col, h1, g8)
# - Savitzky–Golay smoothing; normalize to PSII peak (650–700 nm)
# - Collapse technical replicates to the median (per biological replicate)
# - qGAM (τ = 0.5) within a wavelength band
# - Difference spectra vs col with simultaneous 95% CIs
# - Peak position (λ) and amplitude with uncertainty
# - Band-wise global p-values for contrasts
#############################

## =========================
## Packages
## =========================
needed <- c("prospectr","tidyr","readxl","dplyr","purrr","ggplot2","qgam","MASS","matrixStats", "scales","stringr","readr")
to_install <- setdiff(needed, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)
invisible(lapply(needed, library, character.only = TRUE))

# Do NOT load Rmisc/plyr (conflicts with dplyr).

## =========================
## Settings
## =========================
files <- list(
  g8  = c("g8_1.csv","g8_2.csv","g8_3.csv","g8_4.csv"),
  wt  = c("wt1.csv","wt2.csv","wt3.csv","wt4.csv"),
  h   = c("h1_1.csv","h1_2.csv","h1_3.csv","h1_4.csv")
)

MovingPoint <- 10         # even; SG window = MovingPoint + 1
RepTechNb   <- 6          # technical replicates per biological replicate
band_min    <- 660        # analysis band
band_max    <- 800

band_label  <- sprintf("%d-%dnm", band_min, band_max)

## =========================
## CSV loader
## =========================
# Expects first column = wavelength, remaining columns = signals.
# Keeps numeric rows, drops empty/duplicate columns, coerces to numeric.
f_load_data_csv <- function(csvFile, prefix = NULL, round_to = 2, dec = ".") {
  lines <- readLines(csvFile, warn = FALSE, encoding = "UTF-8")
  lines <- iconv(lines, from = "", to = "UTF-8", sub = "")
  keep  <- grepl("^\\s*\\d+(?:[\\.,]\\d+)?\\s*,", lines, perl = TRUE)
  if (!any(keep)) stop("No numeric data lines detected in: ", csvFile)
  
  df <- read.table(text = lines[keep], header = FALSE, sep = ",", dec = dec, check.names = FALSE)
  
  empty_col <- vapply(df, function(x) all(is.na(x) | x == ""), logical(1))
  if (any(empty_col)) df <- df[, !empty_col, drop = FALSE]
  df[] <- lapply(df, function(x) suppressWarnings(as.numeric(x)))
  
  colnames(df)[1] <- "nm"
  if (ncol(df) > 1) {
    if (is.null(prefix)) prefix <- tools::file_path_sans_ext(basename(csvFile))
    colnames(df)[-1] <- paste0(prefix, "_", seq_len(ncol(df) - 1))
  }
  
  dup_nm <- vapply(df[-1], function(x) identical(x, df[[1]]), logical(1))
  if (any(dup_nm)) df <- df[, c(TRUE, !dup_nm), drop = FALSE]
  
  df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., round_to)))
}

## =========================
## Load runs per line, merge by wavelength, fill down
## =========================
load_line <- function(fvec, line_tag) {
  lst <- purrr::imap(fvec, ~ f_load_data_csv(.x, prefix = paste0(line_tag, "_", .y)))
  out <- purrr::reduce(lst, dplyr::full_join, by = "nm")
  tidyr::fill(out, -nm, .direction = "down")
}

Liste_data <- list(
  g8 = load_line(files$g8, "g8"),
  wt = load_line(files$wt, "wt"),
  h  = load_line(files$h,  "h")
)

## =========================
## Smoothing, normalization, and aggregation (median)
## =========================
f_process_data <- function(DATA, LINE){
  stopifnot("nm" %in% names(DATA))
  half <- MovingPoint / 2
  if (half != floor(half)) stop("MovingPoint must be even.")
  
  # Savitzky–Golay: samples in rows, wavelengths in columns
  X  <- t(as.matrix(DATA[,-1]))
  sg <- prospectr::savitzkyGolay(X, p = 3, w = MovingPoint + 1, m = 0)
  
  if (ncol(sg) <= MovingPoint) stop("Too few wavelength points for this window.")
  keep_cols <- (half + 1):(ncol(sg) - half)
  sg <- sg[, keep_cols, drop = FALSE]
  lambda <- DATA$nm[keep_cols]
  
  df_smooth <- as.data.frame(t(sg))
  df_smooth$y <- lambda
  df_smooth <- df_smooth[order(df_smooth$y), ] %>% dplyr::select(y, dplyr::everything())
  
  # Normalize to max in 650–700 nm (PSII)
  in_band <- df_smooth$y >= 650 & df_smooth$y < 700
  if (!any(in_band)) stop("No wavelengths in 650–700 nm after trimming.")
  max_vec <- apply(df_smooth[in_band, -1, drop = FALSE], 2, max, na.rm = TRUE)
  df_norm <- sweep(df_smooth[,-1, drop = FALSE], 2, max_vec, "/")
  
  # Median across technical replicates → one column per biological replicate
  nb_rep_bio <- ncol(df_norm) / RepTechNb
  if (nb_rep_bio != floor(nb_rep_bio)) stop("Column count not a multiple of RepTechNb.")
  df_mean <- data.frame(lambda = df_smooth$y)
  for (repBioCount in seq_len(nb_rep_bio)) {
    cols <- ((repBioCount - 1) * RepTechNb + 1):(repBioCount * RepTechNb)
    med_vec <- matrixStats::rowMedians(as.matrix(df_norm[, cols, drop = FALSE]), na.rm = TRUE)
    df_mean[[sprintf("%s_%d", LINE, repBioCount)]] <- med_vec
  }
  
  list(SMOOTH = df_smooth, NORM = df_norm, MEAN = df_mean)
}

result_processed <- purrr::imap(Liste_data, ~ f_process_data(.x, .y))

## =========================
## Assemble biological medians (wide → long)
## =========================
Lmean <- purrr::map(result_processed, "MEAN")
complete_mean <- purrr::reduce(Lmean, dplyr::full_join, by = "lambda")

complete_mean_long <- complete_mean %>%
  tidyr::pivot_longer(!lambda, names_to = "Line", values_to = "value") %>%
  tidyr::separate(Line, c("Line", "rep"), sep = "_", remove = TRUE) %>%
  dplyr::mutate(
    Line   = dplyr::recode(Line, wt = "col", h = "h1", g8 = "g8"),
    Line   = factor(Line, levels = c("col","h1","g8")),
    lambda = as.numeric(lambda),
    value  = as.numeric(value)
  ) %>%
  dplyr::filter(is.finite(lambda), is.finite(value))

## =========================
## qGAM fit (factor-specific smooths with shared baseline)
## =========================
dat_band <- complete_mean_long %>%
  dplyr::filter(lambda >= band_min, lambda <= band_max) %>%
  dplyr::filter(lambda > band_min + 0.5, lambda < band_max - 0.5) %>% # small edge trim
  droplevels()

nlam   <- length(unique(dat_band$lambda))
k_base <- max(10, min(40, floor(nlam / 1.2)))  # shared baseline
k_dev  <- max(5,  min(25, floor(nlam / 1.5)))  # per-line deviations

set.seed(1)
m_q <- qgam::qgam(
  value ~ Line +
    s(lambda,            k = k_base) +          # shared baseline
    s(lambda, by = Line, k = k_dev),            # per-line deviations
  data = dat_band,
  qu   = 0.5
)

# Prediction grid within the fitted range
grid_lambda <- seq(min(dat_band$lambda), max(dat_band$lambda), by = 0.2)
newgrid <- expand.grid(lambda = grid_lambda, Line = levels(dat_band$Line))

pred <- predict(m_q, newdata = newgrid, se.fit = TRUE)
pred_df <- cbind(newgrid,
                 fit = as.numeric(pred$fit),
                 se  = pmax(as.numeric(pred$se.fit), 1e-8)) %>%  # small SE floor
  dplyr::mutate(lwr = fit - 1.96*se, upr = fit + 1.96*se)

# Plot qGAM medians with pointwise 95% CIs
p_qgam <- ggplot(pred_df, aes(x = lambda, y = fit, colour = Line, fill = Line)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25, linewidth = 0) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = c(col = "black", g8 = "#006400", h1 = "#228B22")) +
  scale_fill_manual(values = c(col = "grey70", g8 = "#228B22", h1 = "#90EE90")) +
  scale_x_continuous(
    limits = c(band_min, band_max),
    breaks = scales::extended_breaks(n = 5) )+
  labs(x = "Wavelength (nm)", y = "Normalized emission (τ = 0.5)",
       title = sprintf("77 K spectra (qGAM median) — %s", band_label)) +
  theme_classic()

ggplot2::ggsave(sprintf("qgam_median_%s.pdf", band_label), p_qgam, width = 5, height = 4)
ggplot2::ggsave(sprintf("qgam_median_%s.svg", band_label), p_qgam, width = 5, height = 4)

## =========================
## Difference vs col with simultaneous 95% CIs
## =========================
difference_curve <- function(model, newgrid, A, B = "col", nsim = 1500) {
  gA <- newgrid; gA$Line <- factor(A, levels = levels(newgrid$Line))
  gB <- newgrid; gB$Line <- factor(B, levels = levels(newgrid$Line))
  XA <- predict(model, newdata = gA, type = "lpmatrix")
  XB <- predict(model, newdata = gB, type = "lpmatrix")
  Xd <- XA - XB
  b  <- coef(model); Vb <- vcov(model)
  
  fit <- as.numeric(Xd %*% b)
  se  <- pmax(sqrt(rowSums((Xd %*% Vb) * Xd)), 1e-8)   # SE floor
  
  sims <- MASS::mvrnorm(n = nsim, mu = rep(0, length(b)), Sigma = Vb)
  sim_curves <- sims %*% t(Xd)
  sim_std <- sweep(sim_curves, 2, se, "/")
  crit <- stats::quantile(apply(abs(sim_std), 1, max), 0.95, na.rm = TRUE)
  
  dplyr::tibble(
    lambda = newgrid$lambda,
    contrast = paste0(A, " - ", B),
    fit = fit, se = se,
    lwr_pw  = fit - 1.96*se,
    upr_pw  = fit + 1.96*se,
    lwr_sim = fit - as.numeric(crit)*se,
    upr_sim = fit + as.numeric(crit)*se
  )
}

grid0 <- data.frame(lambda = grid_lambda,
                    Line   = factor("col", levels = levels(dat_band$Line)))

diff_all <- dplyr::bind_rows(
  difference_curve(m_q, grid0, A = "h1", nsim = 1500),
  difference_curve(m_q, grid0, A = "g8", nsim = 1500)
) %>%
  dplyr::mutate(sig = (lwr_sim > 0) | (upr_sim < 0)) %>%
  dplyr::arrange(contrast, lambda) %>%
  dplyr::mutate(fit_sig = ifelse(sig, fit, NA_real_))

# symmetric y-limits from simultaneous CI envelope
y_lim <- with(diff_all, max(abs(c(lwr_sim, upr_sim)), na.rm = TRUE))

# compute CI bounds
ymin <- min(diff_all$lwr_sim, na.rm = TRUE)
ymax <- max(diff_all$upr_sim, na.rm = TRUE)

# breathing room (5%)
y_pad <- 0.05 * (ymax - ymin)
ymin <- ymin - y_pad
ymax <- ymax + y_pad



p_diff <- ggplot2::ggplot(diff_all, aes(x = lambda)) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, linetype = "dashed") +
  ggplot2::geom_ribbon(aes(ymin = lwr_sim, ymax = upr_sim, fill = contrast),
                       alpha = 0.20, colour = NA) +
  ggplot2::geom_line(aes(y = fit, colour = contrast, group = contrast),
                     linewidth = 0.8, lineend = "round") +
  ggplot2::geom_line(aes(y = fit_sig, colour = contrast, group = contrast),
                     linewidth = 1.0, lineend = "round", show.legend = FALSE) +
  ggplot2::scale_colour_manual(
    values = c(
      `g8 - col` = "#006400",  # dark green
      `h1 - col` = "#228B22"   # medium green
    )
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      `g8 - col` = "#228B22",  # lighter than h1 line
      `h1 - col` = "#90EE90"   # pale green
    )
  ) +
  ggplot2::scale_x_continuous(
    limits = c(band_min, band_max),
    breaks = scales::extended_breaks(n = 5),
    expand = expansion(mult = c(0, 0.01))
  ) +
  ggplot2::scale_y_continuous(
    limits = c(ymin, ymax),
    breaks = scales::extended_breaks(n = 5)
  ) +
  ggplot2::labs(
    x = "Wavelength (nm)",
    y = "Difference in median emission (τ = 0.5)",
    title = sprintf("Difference vs col (simultaneous 95%% CIs) — %s", band_label)
  ) +
  ggplot2::theme_classic()

ggplot2::ggsave(sprintf("qgam_differences_vs_wt_%s.pdf", band_label), p_diff, width = 5, height = 4)
ggplot2::ggsave(sprintf("qgam_differences_vs_wt_%s.svg", band_label), p_diff, width = 5, height = 4)

## =========================
## Peak location and amplitude (with uncertainty)
## =========================
peak_diff <- function(model, base_grid, A, B = "col", nsim = 4000) {
  line_levels <- levels(dat_band$Line)
  gB <- base_grid; gB$Line <- factor(B, levels = line_levels)
  gA <- base_grid; gA$Line <- factor(A, levels = line_levels)
  XA <- predict(model, newdata = gA, type = "lpmatrix")
  XB <- predict(model, newdata = gB, type = "lpmatrix")
  Xd <- XA - XB
  b  <- coef(model); Vb <- vcov(model); lam <- base_grid$lambda
  
  fit <- as.numeric(Xd %*% b)
  i0  <- which.max(abs(fit))
  peak_lambda_hat <- lam[i0]
  peak_value_hat  <- fit[i0]
  
  set.seed(42)
  beta_sims  <- MASS::mvrnorm(n = nsim, mu = b, Sigma = Vb)
  sim_curves <- beta_sims %*% t(Xd)
  idx <- apply(abs(sim_curves), 1, which.max)
  sim_peak_lambda <- lam[idx]
  sim_peak_value  <- sim_curves[cbind(seq_len(nsim), idx)]
  
  lambda_ci <- stats::quantile(sim_peak_lambda, c(0.025, 0.975), na.rm = TRUE)
  value_ci  <- stats::quantile(sim_peak_value,  c(0.025, 0.975), na.rm = TRUE)
  
  dplyr::tibble(
    contrast = paste0(A, " - ", B),
    peak_lambda = peak_lambda_hat,
    peak_lambda_lwr = as.numeric(lambda_ci[1]),
    peak_lambda_upr = as.numeric(lambda_ci[2]),
    peak_value  = peak_value_hat,
    peak_value_lwr = as.numeric(value_ci[1]),
    peak_value_upr = as.numeric(value_ci[2])
  )
}

base_grid <- data.frame(lambda = seq(band_min, band_max, by = 0.2))
peaks_tbl <- dplyr::bind_rows(
  peak_diff(m_q, base_grid, A = "g8"),
  peak_diff(m_q, base_grid, A = "h1")
)

## =========================
## Global p-values (max |z| over band)
## =========================
difference_pval <- function(model, lambda_seq, A, B = "col", nsim = 4000) {
  line_levels <- levels(dat_band$Line)
  base <- data.frame(lambda = lambda_seq, Line = factor(B, levels = line_levels))
  gA   <- base; gA$Line <- factor(A, levels = line_levels)
  
  XA <- predict(model, newdata = gA,   type = "lpmatrix")
  XB <- predict(model, newdata = base, type = "lpmatrix")
  Xd <- XA - XB
  
  b  <- coef(model); Vb <- vcov(model)
  fit <- as.numeric(Xd %*% b)
  se  <- sqrt(rowSums((Xd %*% Vb) * Xd))
  z   <- fit / se
  obs_max <- max(abs(z), na.rm = TRUE)
  
  sims <- MASS::mvrnorm(n = nsim, mu = rep(0, length(b)), Sigma = Vb)
  sim_curves <- sims %*% t(Xd)
  sim_std    <- sweep(sim_curves, 2, se, "/")
  max_abs_sim <- apply(abs(sim_std), 1, max)
  p_global_raw <- mean(max_abs_sim >= obs_max)
  p_label <- if (p_global_raw == 0) paste0("< ", signif(1/nsim, 2)) else signif(p_global_raw, 3)
  
  dplyr::tibble(contrast = paste0(A, " - ", B),
                obs_max = obs_max, p_global = p_global_raw, p_label = p_label, nsim = nsim)
}

pvals_tbl <- dplyr::bind_rows(
  difference_pval(m_q, base_grid$lambda, "g8", nsim = 4000),
  difference_pval(m_q, base_grid$lambda, "h1", nsim = 4000)
)

## =========================
## Significant wavelength ranges
## =========================
report_sig_tbl <- diff_all %>%
  dplyr::group_by(contrast) %>%
  dplyr::summarise(
    nm_ranges = {
      x <- lambda[(lwr_sim > 0) | (upr_sim < 0)]
      if (length(x) == 0) "none" else {
        brks <- c(0, which(diff(x) > 0.21), length(x))  # grid step ≈ 0.2 nm
        paste(purrr::map2_chr(brks[-length(brks)] + 1, brks[-1],
                              ~ sprintf("[%.1f–%.1f]", x[.x], x[.y])),
              collapse = ", ")
      }
    },
    .groups = "drop"
  )

## =========================
## Save tables
## =========================
summary_tbl <- peaks_tbl %>%
  dplyr::left_join(report_sig_tbl, by = "contrast") %>%
  dplyr::left_join(pvals_tbl,    by = "contrast") %>%
  dplyr::mutate(band = band_label, .before = 1)

# write.csv(peaks_tbl,      sprintf("peaks_%s.csv", band_label), row.names = FALSE)
# write.csv(report_sig_tbl, sprintf("sig_ranges_%s.csv", band_label), row.names = FALSE)
# write.csv(pvals_tbl,      sprintf("contrast_pvals_%s.csv", band_label), row.names = FALSE)
write.csv(summary_tbl,    sprintf("summary_%s.csv", band_label), row.names = FALSE)

message("Saved: ",
        paste(c(sprintf("peaks_%s.csv", band_label),
                sprintf("sig_ranges_%s.csv", band_label),
                sprintf("contrast_pvals_%s.csv", band_label),
                sprintf("summary_%s.csv", band_label)), collapse = ", "))
