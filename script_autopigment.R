# ==============================================================================
# Pigment-by-pigment bar + beeswarm + CI + CLD, with per-pigment stats
#Author: Ben Field
# - Parametric (ANOVA + Tukey) when normal; else Kruskal (+ Dunn if k>=3)
# - Automatic pigment ordering (amounts ranked; ratios at end: VAZ then DEPS)
# - Publication-friendly styling & combined multipanel export (PNG/PDF/SVG)
# ==============================================================================

# ------------------------- User settings --------------------------------------
COLUMN_LINE_COLOUR <- "dark grey"
COLUMN_FILL_COLOUR <- "ivory1"
POINT_COLOR        <- "black"
START_Y_AT_ZERO    <- TRUE

DATA <- "example_script_autopigment.xlsx"

# Factors
var1 <- "Treatment"   # facets
var2 <- "Line"        # bars within facet

# Restrict pigments? (NULL = auto-detect numeric pigment columns)
PIGMENT_INCLUDE <- NULL

# Display order for factors
order1 <- c("GL")
order2 <- c("WT","OXRSH3.1","npq1","OXRSH3npq1")

# Output layout
OUTPUT_BASE   <- "out_by_pigment"
plot_width    <- 2     # inches per subplot
plot_height   <- 4     # inches per subplot
combined_ncol <- 4     # columns in combined figure

# ------------------------- Packages -------------------------------------------
pkgs <- c(
  "dplyr","tidyr","readxl","rstatix","rcompanion","ggplot2",
  "ggbeeswarm","RColorBrewer","Rmisc","forcats","multcompView",
  "rlang","patchwork"
)
to_install <- pkgs[!sapply(pkgs, require, character.only = TRUE)]
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, require, character.only = TRUE))

# ------------------------- Small helpers --------------------------------------
sanitize_name <- function(x) gsub("[^A-Za-z0-9_\\-]+","_", x)

check_normality <- function(shapiro_df) {
  if (!nrow(shapiro_df)) return(TRUE)
  all(is.na(shapiro_df$p) | shapiro_df$p > 0.05)
}
safe_shapiro <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3) return(1)
  if (length(unique(x)) < 3) return(1)
  out <- try(stats::shapiro.test(x), silent = TRUE)
  if (inherits(out, "try-error")) return(1)
  as.numeric(out$p.value)
}

generate_cld_parametric <- function(tukey_df, var1_col, var2_col) {
  tukey_df %>%
    dplyr::group_by(!!rlang::sym(var1_col)) %>%
    dplyr::summarise(
      cld = list(multcompView::multcompLetters(
        setNames(p.adj, paste(group1, group2, sep = "-")), Letters = letters
      )$Letters),
      .groups = "drop"
    ) %>%
    tidyr::unnest_longer(cld) %>%
    dplyr::mutate(!!rlang::sym(var2_col) := names(cld)) %>%
    dplyr::select(!!rlang::sym(var1_col), !!rlang::sym(var2_col), cld)
}
generate_cld_nonparametric <- function(dunn_df, var1_col, var2_col) {
  dunn_df %>%
    dplyr::group_by(!!rlang::sym(var1_col)) %>%
    dplyr::summarise(
      cld = list(multcompView::multcompLetters(
        setNames(p.adj, paste(group1, group2, sep = "-")), Letters = letters
      )$Letters),
      .groups = "drop"
    ) %>%
    tidyr::unnest_longer(cld) %>%
    dplyr::mutate(!!rlang::sym(var2_col) := names(cld)) %>%
    dplyr::select(!!rlang::sym(var1_col), !!rlang::sym(var2_col), cld)
}

set_factor_levels <- function(df, var1, var2, order1, order2) {
  df %>%
    dplyr::mutate(
      !!rlang::sym(var1) := factor(.data[[var1]], levels = order1),
      !!rlang::sym(var2) := factor(.data[[var2]], levels = order2)
    )
}
detect_pigments <- function(df, var1, var2, include = NULL) {
  if (!is.null(include)) return(include[include %in% names(df)])
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  setdiff(num_cols, c(var1, var2))
}

normalize_pigment <- function(p) tolower(gsub("[^a-z0-9]+", "", p))
is_chl_pigment <- function(p) normalize_pigment(p) %in% c("chla","chlb","chlorophylla","chlorophyllb")
is_vaz_or_deps <- function(p) normalize_pigment(p) %in% c("vaz","deps")
is_grey_ratio <- function(p) {
  normalize_pigment(p) %in% c("ratioab","ratiochlcar")
}

# Prefer VAZ before DEPS among ratios (case/variant-insensitive)
prefer_order <- function(actual, preferred_norm) {
  act_norm <- normalize_pigment(actual)
  idx <- match(preferred_norm, act_norm, nomatch = 0)
  first <- actual[idx[idx > 0]]
  c(first, setdiff(actual, first))
}

# Brewer palettes (dark half only) for bars by Line
DARK_FRAC <- 0.58
colors_for <- function(pigment, levels_v2) {
  n <- normalize_pigment(pigment)
  
  if (is_chl_pigment(pigment)) {
    base <- RColorBrewer::brewer.pal(9, "Greens")
    ramp <- grDevices::colorRampPalette(rev(base))  # darkest first
  } else if (is_vaz_or_deps(pigment)) {
    base <- RColorBrewer::brewer.pal(9, "RdPu")
    ramp <- grDevices::colorRampPalette(rev(base))
  } else if (exists("is_grey_ratio") && is_grey_ratio(pigment)) {
    ramp <- grDevices::colorRampPalette(c("grey30", "grey80"))  # light grey gradient
  } else {
    base <- RColorBrewer::brewer.pal(9, "Oranges")
    ramp <- grDevices::colorRampPalette(rev(base))
  }
  
  m    <- 512
  full <- ramp(m)
  # --- FIX: only use the first DARK_FRAC of the palette, not m*m
  upper_idx <- max(2, floor(m * DARK_FRAC))
  idx       <- round(seq(1, upper_idx, length.out = length(levels_v2)))
  cols      <- full[idx]
  
  # safety in case anything weird happens
  if (anyNA(cols)) cols[is.na(cols)] <- full[pmax(1, upper_idx - 1)]
  
  names(cols) <- levels_v2
  cols
}

# Space above bars for CLD letters
VARIANCE_EPS <- 1e-8
CLD_HEADROOM_MULT <- 0.18
limits_from <- function(y_vals, ymin, ymax,
                        start_at_zero = START_Y_AT_ZERO,
                        headroom     = CLD_HEADROOM_MULT) {
  point_min <- min(y_vals, na.rm = TRUE)
  point_max <- max(y_vals, na.rm = TRUE)
  band_min  <- suppressWarnings(min(ymin, na.rm = TRUE))
  band_max  <- suppressWarnings(max(ymax, na.rm = TRUE))
  data_min  <- min(point_min, band_min)
  data_max  <- max(point_max, band_max)
  base_rng  <- data_max - data_min
  lower     <- if (start_at_zero) 0 else data_min - 0.02 * base_rng
  upper     <- data_max + 0.05 * base_rng
  rng       <- upper - lower
  upper     <- upper + headroom * rng
  list(lower = lower, upper = upper, cld_y = upper - 0.02 * (upper - lower))
}

# ------------------------- Load & order pigments -------------------------------
df <- readxl::read_excel(DATA) %>% set_factor_levels(var1, var2, order1, order2)

pigment_cols <- detect_pigments(df, var1, var2, PIGMENT_INCLUDE)
stopifnot("No pigment columns detected." = length(pigment_cols) > 0)

# Rank amounts by mean/median at a chosen treatment; ratios (VAZ, DEPS) appended last
RANK_TREATMENT <- "GL"
RANK_STAT      <- "mean"  # or "median"

# split into amount, VAZ/DEPS ratios, and grey ratios
ratio_vaz_deps <- pigment_cols[vapply(pigment_cols, is_vaz_or_deps, logical(1))]
ratio_grey     <- pigment_cols[vapply(pigment_cols, is_grey_ratio,   logical(1))]
amount_pigs    <- setdiff(pigment_cols, c(ratio_vaz_deps, ratio_grey))

rank_treat_eff <- if (RANK_TREATMENT %in% levels(df[[var1]])) RANK_TREATMENT else levels(df[[var1]])[1]

if (length(amount_pigs)) {
  rank_tbl_amount <- df %>%
    dplyr::filter(!!rlang::sym(var1) == rank_treat_eff) %>%
    tidyr::pivot_longer(dplyr::all_of(amount_pigs), names_to = "pigment", values_to = "value") %>%
    dplyr::group_by(pigment) %>%
    { if (RANK_STAT == "median")
      dplyr::summarise(., rank_value = median(value, na.rm = TRUE), .groups = "drop")
      else
        dplyr::summarise(., rank_value = mean(value,   na.rm = TRUE), .groups = "drop")
    } %>%
    dplyr::arrange(dplyr::desc(rank_value))
  
  if (nrow(rank_tbl_amount) == 0 || all(!is.finite(rank_tbl_amount$rank_value))) {
    rank_tbl_amount <- df %>%
      tidyr::pivot_longer(dplyr::all_of(amount_pigs), names_to = "pigment", values_to = "value") %>%
      dplyr::group_by(pigment) %>%
      dplyr::summarise(rank_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(rank_value))
  }
  amount_order <- rank_tbl_amount$pigment
} else {
  amount_order <- character(0)
}

# ratios go to the end: VAZ→DEPS first, then grey ratios
ratio_vaz_deps_order <- if (length(ratio_vaz_deps)) prefer_order(ratio_vaz_deps, c("vaz","deps")) else character(0)
ratio_order <- c(ratio_vaz_deps_order, ratio_grey)

pigment_order <- c(amount_order, ratio_order)
pigment_cols  <- pigment_order

# ------------------------- Core: analyze one pigment ---------------------------
analyze_one_pigment <- function(df_data, pigment, out_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  dfp <- df_data %>% dplyr::mutate(y = .data[[pigment]])
  
  # Force non-parametric if sample size small / variance ~ 0 in any group
  grp_chk <- dfp %>%
    dplyr::group_by(!!rlang::sym(var1), !!rlang::sym(var2)) %>%
    dplyr::summarise(n = sum(is.finite(y)),
                     sd = stats::sd(y, na.rm = TRUE),
                     .groups = "drop")
  force_nonparam <- any(grp_chk$n < 3 | is.na(grp_chk$sd) | grp_chk$sd <= VARIANCE_EPS)
  
  # Normality check (per Line × Treatment)
  shapiro_df <- dfp %>%
    dplyr::group_by(!!rlang::sym(var2), !!rlang::sym(var1)) %>%
    dplyr::summarise(p = safe_shapiro(y), .groups = "drop")
  is_normal <- (!force_nonparam) && check_normality(shapiro_df)
  
  # Colors for bars (by Line), points use constant POINT_COLOR
  lev2    <- levels(df_data[[var2]])
  col_map <- colors_for(pigment, lev2)
  fill_aes   <- ggplot2::aes(fill = !!rlang::sym(var2))
  fill_scale <- ggplot2::scale_fill_manual(values = col_map)
  bar_layer  <- ggplot2::geom_col(width = 0.9, colour = COLUMN_LINE_COLOUR)
  
  if (is_normal) {
    # --- Parametric: mean ± t-CI; ANOVA; Tukey; CLD
    my_summary <- Rmisc::summarySE(dfp, measurevar = "y", groupvars = c(var2, var1)) %>%
      dplyr::rename(Mean = y)
    anova_result <- dfp %>%
      dplyr::group_by(!!rlang::sym(var1)) %>%
      rstatix::anova_test(reformulate(var2, "y")) %>%
      dplyr::ungroup() %>% tibble::as_tibble()
    tukey_results <- dfp %>%
      dplyr::group_by(!!rlang::sym(var1)) %>%
      rstatix::tukey_hsd(as.formula(paste("y ~", var2))) %>%
      dplyr::ungroup() %>% tibble::as_tibble()
    cld_tbl <- generate_cld_parametric(tukey_results, var1, var2)
    
    df2 <- dplyr::inner_join(my_summary, cld_tbl, by = c(var2, var1))
    
    df2[[var2]] <- factor(df2[[var2]], levels = levels(df_data[[var2]]))
    
    lims <- limits_from(dfp$y, df2$Mean - df2$ci, df2$Mean + df2$ci)
    df2$cld_y <- lims$cld_y
    
    p <- ggplot2::ggplot(df2, ggplot2::aes(x = !!rlang::sym(var2), y = Mean)) +
      fill_aes + bar_layer +
      ggbeeswarm::geom_quasirandom(
        data = dfp, ggplot2::aes(x = !!rlang::sym(var2), y = y),
        colour = POINT_COLOR, shape = 16, size = 2, width = 0.3, alpha = 0.6,
        inherit.aes = FALSE
      ) +
      ggplot2::geom_segment(
        ggplot2::aes(xend = !!rlang::sym(var2), y = Mean - ci, yend = Mean + ci),
        colour = "black"
      ) +
      ggplot2::geom_text(ggplot2::aes(y = cld_y, label = cld), size = 3) +
      ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0.6, 0.25))) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0))) +
      ggplot2::coord_cartesian(ylim = c(lims$lower, lims$upper), clip = "off") +
      fill_scale +
      ggplot2::facet_wrap(as.formula(paste("~", var1)), nrow = 1, scales = "free_y") +
      ggplot2::labs(x = NULL, y = paste(pigment, "mean ± CI"), title = pigment) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        plot.title      = ggplot2::element_text(hjust = 0.5),
        axis.text.x     = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "none",
        strip.background= ggplot2::element_blank(),
        strip.placement = "outside",
        strip.text      = ggplot2::element_text(size = 10),
        panel.spacing   = grid::unit(2, "mm"),
        plot.margin     = ggplot2::margin(3, 3, 3, 3)
      )
    
    ggplot2::ggsave(file.path(out_dir, paste0("plot_", sanitize_name(pigment), "_normal.svg")),
                    p, width = plot_width, height = plot_height, bg = "transparent")
    ggplot2::ggsave(file.path(out_dir, paste0("plot_", sanitize_name(pigment), "_normal.png")),
                    p, width = plot_width, height = plot_height, dpi = 300, bg = "white")
    
    utils::write.table(cld_tbl,      file.path(out_dir, paste0("CLD_",    sanitize_name(pigment), "_parametric.txt")),
                       sep = "\t", row.names = FALSE, quote = FALSE)
    utils::write.table(my_summary,   file.path(out_dir, paste0("Summary_",sanitize_name(pigment), ".txt")),
                       sep = "\t", row.names = FALSE, quote = FALSE)
    utils::write.table(anova_result, file.path(out_dir, paste0("Anova_",  sanitize_name(pigment), ".txt")),
                       sep = "\t", row.names = FALSE, quote = FALSE)
    utils::write.table(tukey_results[, c(1,3,4,9,10)], file.path(out_dir, paste0("Tukey_", sanitize_name(pigment), ".txt")),
                       sep = "\t", row.names = FALSE, quote = FALSE)
    return(p)
  }
  
  # --- Non-parametric: Kruskal; Dunn CLD when k >= 3; KW 2-group CLD otherwise
  conf_int <- rcompanion::groupwiseMedian(
    data = dfp, var = "y", group = c(var2, var1),
    conf = 0.95, R = 5000, percentile = TRUE, bca = FALSE, digits = 6
  )
  kw_tbl <- dfp %>%
    dplyr::group_by(!!rlang::sym(var1)) %>%
    rstatix::kruskal_test(as.formula(paste("y ~", var2))) %>%
    dplyr::ungroup() %>%
    dplyr::select(!!rlang::sym(var1), p)
  
  k <- nlevels(dfp[[var2]])
  if (k >= 3) {
    dunn_tbl <- dfp %>%
      dplyr::group_by(!!rlang::sym(var1)) %>%
      rstatix::dunn_test(as.formula(paste("y ~", var2)), p.adjust.method = "BH") %>%
      dplyr::ungroup()
    cld_tbl <- generate_cld_nonparametric(dunn_tbl, var1, var2)
  } else {
    lv2 <- levels(dfp[[var2]])
    cld_rows <- lapply(levels(dfp[[var1]]), function(facet) {
      pval <- kw_tbl$p[kw_tbl[[var1]] == facet]
      letters <- if (length(pval) == 0 || is.na(pval) || pval >= 0.05) c("a","a") else c("a","b")
      data.frame(facet_val = facet, var2_val = lv2, cld = letters, stringsAsFactors = FALSE)
    })
    cld_tbl <- do.call(rbind, cld_rows)
    names(cld_tbl) <- c(var1, var2, "cld")
  }
  
  df2 <- dplyr::inner_join(conf_int, cld_tbl, by = c(var2, var1))
  
  df2[[var2]] <- factor(df2[[var2]], levels = levels(df_data[[var2]]))
  
  
  lims <- limits_from(dfp$y, df2$Percentile.lower, df2$Percentile.upper)
  df2$cld_y <- lims$cld_y
  
  p <- ggplot2::ggplot(df2, ggplot2::aes(x = !!rlang::sym(var2), y = Median)) +
    fill_aes + bar_layer +
    ggbeeswarm::geom_quasirandom(
      data = dfp, ggplot2::aes(x = !!rlang::sym(var2), y = y),
      colour = POINT_COLOR, shape = 16, size = 2, width = 0.3, alpha = 0.6,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = !!rlang::sym(var2), y = Percentile.lower, yend = Percentile.upper),
      colour = "black"
    ) +
    ggplot2::geom_text(ggplot2::aes(y = cld_y, label = cld), size = 3) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0.6, 0.25))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::coord_cartesian(ylim = c(lims$lower, lims$upper), clip = "off") +
    fill_scale +
    ggplot2::facet_wrap(as.formula(paste("~", var1)), nrow = 1, scales = "free_y") +
    ggplot2::labs(x = NULL, y = paste(pigment, "median ± CI"), title = pigment) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(hjust = 0.5),
      axis.text.x     = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = "none",
      strip.background= ggplot2::element_blank(),
      strip.placement = "outside",
      strip.text      = ggplot2::element_text(size = 10),
      panel.spacing   = grid::unit(2, "mm"),
      plot.margin     = ggplot2::margin(3, 3, 3, 3)
    )
  
  ggplot2::ggsave(file.path(out_dir, paste0("plot_", sanitize_name(pigment), "_nonpara.svg")),
                  p, width = plot_width, height = plot_height, bg = "transparent")
  ggplot2::ggsave(file.path(out_dir, paste0("plot_", sanitize_name(pigment), "_nonpara.png")),
                  p, width = plot_width, height = plot_height, dpi = 300, bg = "white")
  
  utils::write.table(kw_tbl,   file.path(out_dir, paste0("Kruskal_", sanitize_name(pigment), ".txt")),
                     sep = "\t", row.names = FALSE, quote = FALSE)
  utils::write.table(conf_int, file.path(out_dir, paste0("Summary_",  sanitize_name(pigment), ".txt")),
                     sep = "\t", row.names = FALSE, quote = FALSE)
  if (k >= 3) {
    utils::write.table(dunn_tbl, file.path(out_dir, paste0("Dunn_", sanitize_name(pigment), ".txt")),
                       sep = "\t", row.names = FALSE, quote = FALSE)
    utils::write.table(cld_tbl,  file.path(out_dir, paste0("CLD_",  sanitize_name(pigment), "_nonparametric.txt")),
                       sep = "\t", row.names = FALSE, quote = FALSE)
  } else {
    utils::write.table(cld_tbl,  file.path(out_dir, paste0("CLD_",  sanitize_name(pigment), "_kw2groups.txt")),
                       sep = "\t", row.names = FALSE, quote = FALSE)
  }
  return(p)
}

# ------------------------- Run per pigment & collect ---------------------------
dir.create(OUTPUT_BASE, showWarnings = FALSE, recursive = TRUE)

plots <- list()
for (pig in pigment_cols) {
  out_dir <- file.path(OUTPUT_BASE, sanitize_name(pig))
  p_try <- try(analyze_one_pigment(df, pig, out_dir), silent = TRUE)
  if (inherits(p_try, "try-error") || is.null(p_try) || !inherits(p_try, "ggplot")) {
    message("Skipping plot for pigment: ", pig, " (no valid ggplot).")
  } else {
    plots[[pig]] <- p_try
  }
}

# ------------------------- Combine into a single page --------------------------
stopifnot("No plots were generated." = length(plots) > 0)

valid_names   <- pigment_order[pigment_order %in% names(plots)]
if (!length(valid_names)) valid_names <- names(plots)
plots_ordered <- Filter(function(x) inherits(x, "ggplot"), plots[valid_names])
if (!length(plots_ordered)) plots_ordered <- Filter(function(x) inherits(x, "ggplot"), plots)
stopifnot("All generated plots were invalid or missing." = length(plots_ordered) > 0)

if (length(plots_ordered) == 1) {
  single <- plots_ordered[[1]]
  page_w <- plot_width
  page_h <- plot_height
  ggplot2::ggsave(file.path(OUTPUT_BASE, "ALL_pigments_bar_CLD.pdf"),
                  single, width = page_w, height = page_h,
                  device = grDevices::cairo_pdf, bg = "white")
  ggplot2::ggsave(file.path(OUTPUT_BASE, "ALL_pigments_bar_CLD.png"),
                  single, width = page_w, height = page_h, dpi = 300, bg = "white")
  ggplot2::ggsave(file.path(OUTPUT_BASE, "ALL_pigments_bar_CLD.svg"),
                  single, width = page_w, height = page_h, bg = "transparent")
} else {
  combined <- NULL
  if (requireNamespace("patchwork", quietly = TRUE)) {
    combined <- try(patchwork::wrap_plots(plotlist = plots_ordered, ncol = combined_ncol), silent = TRUE)
  }
  if (inherits(combined, "try-error") || is.null(combined)) {
    if (!requireNamespace("cowplot", quietly = TRUE)) {
      install.packages("cowplot", repos = "https://cloud.r-project.org")
    }
    combined <- cowplot::plot_grid(plotlist = plots_ordered, ncol = combined_ncol, align = "hv")
  }
  
  n_plots <- length(plots_ordered)
  n_rows  <- ceiling(n_plots / combined_ncol)
  
  # Preserve individual plot aspect ratio on the page
  page_w <- combined_ncol * plot_width
  page_h <- n_rows        * plot_height
  
  ggplot2::ggsave(file.path(OUTPUT_BASE, "ALL_pigments_bar_CLD.pdf"),
                  combined, width = page_w, height = page_h,
                  device = grDevices::cairo_pdf, bg = "white")
  ggplot2::ggsave(file.path(OUTPUT_BASE, "ALL_pigments_bar_CLD.png"),
                  combined, width = page_w, height = page_h, dpi = 300, bg = "white")
  ggplot2::ggsave(file.path(OUTPUT_BASE, "ALL_pigments_bar_CLD.svg"),
                  combined, width = page_w, height = page_h, bg = "transparent")
}
