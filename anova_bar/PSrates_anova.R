# ==============================================================================
# Figure S3 - Photochemical rates of PSI and PSII and PSI fraction (WT, OX:RSH3.1, OX:RSH3.2)
# Family:  ANOVA-bar, multi-measure version: the version-4 analysis applied to several measure
#          columns, with the panels combined into one figure. The same script is used for Fig 4C
#          (settings differ only); Fig S9 uses an extended version.
# Input:   PSrates.xlsx (columns PSIIrate, PSIrate, PSIRCfraction)
# Output:  out_PSrates/ALL_PSrates_bar_CLD.svg/.pdf/.png; one subfolder per measure with plot_*,
#          Anova_*.txt, Tukey_*.txt, CLD_parametric_*.txt, Summary_*.txt
# Method:  for each measure, Shapiro-Wilk test on the pooled ANOVA residuals. Normal: ANOVA,
#          Tukey HSD, CLD letters, mean +/- t-based 95% CI. Otherwise: Kruskal-Wallis, Dunn CLD
#          when there are 3 or more groups (2-group Kruskal-Wallis letters otherwise),
#          median +/- bootstrap 95% CI.
# Run:     with the working directory set to this folder.
# ==============================================================================

# ---- User settings -----------------------------------------------------------
COLUMN_LINE_COLOUR <- "dark grey"
PALETTE            <- "Greens"
POINT_COLOR        <- "black"
START_Y_AT_ZERO    <- TRUE

DATA <- "PSrates.xlsx"

# Factors
var1 <- "Treatment"   # facets
var2 <- "Line"        # bars within facet

# Measures to plot, in panel order (NULL = all numeric columns, in file order)
MEASURE_INCLUDE <- c("PSIIrate", "PSIrate", "PSIRCfraction")

# Optional y-axis labels per measure (NULL / missing name = "<measure> mean ± CI" etc.)
Y_LABELS <- c(PSIIrate = "PSII rate", PSIrate = "PSI rate", PSIRCfraction = "PSI RC fraction")

# Display order for factors (NULL = alphabetical)
order1 <- NULL
order2 <- NULL

# Output layout
OUTPUT_BASE   <- "out_PSrates"
plot_width    <- 2     # inches per subplot
plot_height   <- 4     # inches per subplot
combined_ncol <- 3     # columns in combined figure

# ---- Packages ----------------------------------------------------------------
pkgs <- c(
  "dplyr","tidyr","readxl","rstatix","rcompanion","ggplot2",
  "ggbeeswarm","RColorBrewer","Rmisc","multcompView","rlang","patchwork","svglite"
)
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(setdiff(pkgs, c("Rmisc", "svglite")), library, character.only = TRUE))

# ---- Functions ---------------------------------------------------------------
sanitize_name <- function(x) gsub("[^A-Za-z0-9_-]+", "_", x)

# Normality is checked on the pooled ANOVA residuals (the actual ANOVA assumption):
# one-way ANOVA of var2 within each var1 facet, all residuals tested together.
residual_shapiro_p <- function(dfp) {
  res <- unlist(lapply(split(dfp, dfp[[var1]], drop = TRUE), function(g) {
    g <- g[is.finite(g$y), ]
    if (length(unique(g[[var2]])) < 2) return(g$y - mean(g$y))
    stats::residuals(stats::lm(stats::reformulate(var2, "y"), data = g))
  }))
  if (length(res) < 3 || length(unique(res)) < 3) return(1)
  out <- try(stats::shapiro.test(res), silent = TRUE)
  if (inherits(out, "try-error")) return(1)
  as.numeric(out$p.value)
}

generate_cld <- function(pairwise_df, var1_col, var2_col) {
  pairwise_df %>%
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
  lv1 <- if (is.null(order1)) sort(unique(df[[var1]])) else order1
  lv2 <- if (is.null(order2)) sort(unique(df[[var2]])) else order2
  df %>%
    dplyr::mutate(
      !!rlang::sym(var1) := factor(.data[[var1]], levels = lv1),
      !!rlang::sym(var2) := factor(.data[[var2]], levels = lv2)
    )
}
detect_measures <- function(df, var1, var2, include = NULL) {
  if (!is.null(include)) return(include[include %in% names(df)])
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  setdiff(num_cols, c(var1, var2))
}
y_label_for <- function(measure, stat) {
  if (!is.null(Y_LABELS) && measure %in% names(Y_LABELS)) return(Y_LABELS[[measure]])
  paste(measure, stat, "± CI")
}

# Space above bars for CLD letters
VARIANCE_EPS <- 1e-8
CLD_HEADROOM_MULT <- 0.12
limits_from <- function(y_vals, ymin, ymax,
                        start_at_zero = START_Y_AT_ZERO,
                        headroom     = CLD_HEADROOM_MULT) {
  data_min <- min(c(y_vals, ymin), na.rm = TRUE)
  data_max <- max(c(y_vals, ymax), na.rm = TRUE)
  base_rng <- data_max - data_min
  lower    <- if (start_at_zero) 0 else data_min - 0.02 * base_rng
  upper    <- data_max + 0.05 * base_rng
  upper    <- upper + headroom * (upper - lower)
  list(lower = lower, upper = upper, cld_y = upper - 0.04 * (upper - lower))
}

# ---- SVG / PDF export --------------------------------------------------------
# svglite puts default fill/stroke in a CSS <style> block that Affinity and Inkscape ignore,
# so shapes without an explicit fill (e.g. transparent backgrounds) show up as black squares.
# Write fill/stroke on every shape.
make_svg_editor_safe <- function(svg_file) {
  txt <- paste(readLines(svg_file, warn = FALSE), collapse = "\n")
  fix_tag <- function(tag) {
    style <- gsub("^style='|'$", "", regmatches(tag, regexpr("style='[^']*'", tag)))
    has   <- function(prop) grepl(paste0("(^|;)[[:space:]]*", prop, ":"), style)
    if (!has("fill"))            style <- paste(style, "fill: none;")
    if (!has("stroke"))          style <- paste(style, "stroke: #000000;")
    if (!has("stroke-linecap"))  style <- paste(style, "stroke-linecap: round;")
    if (!has("stroke-linejoin")) style <- paste(style, "stroke-linejoin: round;")
    sub("style='[^']*'", paste0("style='", style, "'"), tag)
  }
  m <- gregexpr("<(rect|line|polyline|polygon|path|circle) [^>]*?style='[^']*'", txt, perl = TRUE)
  regmatches(txt, m) <- lapply(regmatches(txt, m), function(tags) vapply(tags, fix_tag, character(1), USE.NAMES = FALSE))
  writeLines(txt, svg_file)
}

# Saves <base>.svg (editor-safe), <base>.pdf and <base>.png
save_plot <- function(p, base, width, height) {
  ggplot2::ggsave(paste0(base, ".svg"), p, width = width, height = height,
                  device = svglite::svglite, bg = "transparent")
  make_svg_editor_safe(paste0(base, ".svg"))
  ggplot2::ggsave(paste0(base, ".pdf"), p, width = width, height = height,
                  device = grDevices::cairo_pdf, bg = "transparent")
  ggplot2::ggsave(paste0(base, ".png"), p, width = width, height = height,
                  dpi = 300, bg = "white")
}

# ---- Plot builder ------------------------------------------------------------
build_plot <- function(df2, dfp, y_col, lo_col, hi_col, lims, y_lab, title) {
  ggplot2::ggplot(df2, ggplot2::aes(x = !!rlang::sym(var2), y = .data[[y_col]],
                                    fill = !!rlang::sym(var2))) +
    ggplot2::geom_col(colour = COLUMN_LINE_COLOUR, width = 0.6) +
    ggplot2::scale_fill_brewer(palette = PALETTE) +
    ggbeeswarm::geom_quasirandom(
      data = dfp, ggplot2::aes(x = !!rlang::sym(var2), y = y),
      colour = POINT_COLOR, width = 0.3, alpha = 0.6, inherit.aes = FALSE
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = !!rlang::sym(var2),
                   y = pmax(0, .data[[lo_col]]), yend = .data[[hi_col]]),
      colour = "black"
    ) +
    ggplot2::geom_text(ggplot2::aes(y = cld_y, label = cld), size = 3) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::coord_cartesian(ylim = c(lims$lower, lims$upper), clip = "off") +
    ggplot2::facet_wrap(as.formula(paste("~", var1)), nrow = 1, scales = "fixed") +
    ggplot2::labs(x = NULL, y = y_lab, title = title) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(hjust = 0.5),
      axis.text.x      = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1),
      axis.line        = ggplot2::element_line(linewidth = 0.5),
      panel.background = ggplot2::element_blank(),
      plot.background  = ggplot2::element_blank(),
      legend.position  = "none",
      strip.background = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(size = 10),
      panel.spacing    = grid::unit(2, "mm"),
      plot.margin      = ggplot2::margin(3, 3, 3, 3)
    )
}

# ---- Core: analyze one measure -----------------------------------------------
analyze_one_measure <- function(df_data, measure, out_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out  <- function(prefix) file.path(out_dir, paste0(prefix, "_", sanitize_name(measure), ".txt"))
  save <- function(tbl, prefix) utils::write.table(tbl, out(prefix), sep = "\t", row.names = FALSE, quote = FALSE)
  dfp <- df_data %>% dplyr::mutate(y = as.numeric(.data[[measure]]))

  # Force non-parametric if sample size small / variance ~ 0 in any group
  grp_chk <- dfp %>%
    dplyr::group_by(!!rlang::sym(var1), !!rlang::sym(var2)) %>%
    dplyr::summarise(n = sum(is.finite(y)), sd = stats::sd(y, na.rm = TRUE), .groups = "drop")
  force_nonparam <- any(grp_chk$n < 3 | is.na(grp_chk$sd) | grp_chk$sd <= VARIANCE_EPS)

  # Normality check on the pooled ANOVA residuals
  shapiro_p <- residual_shapiro_p(dfp)
  message(measure, ": Shapiro-Wilk on pooled residuals p = ", signif(shapiro_p, 3))
  is_normal <- (!force_nonparam) && shapiro_p > 0.05

  if (is_normal) {
    # Parametric: mean ± t-CI; ANOVA; Tukey; CLD
    my_summary <- Rmisc::summarySE(as.data.frame(dfp), measurevar = "y", groupvars = c(var2, var1)) %>%
      dplyr::rename(Mean = y)
    anova_result <- dfp %>%
      dplyr::group_by(!!rlang::sym(var1)) %>%
      rstatix::anova_test(reformulate(var2, "y")) %>%
      tibble::as_tibble()
    tukey_results <- dfp %>%
      dplyr::group_by(!!rlang::sym(var1)) %>%
      rstatix::tukey_hsd(as.formula(paste("y ~", var2))) %>%
      dplyr::ungroup() %>% tibble::as_tibble()
    cld_tbl <- generate_cld(tukey_results, var1, var2)

    df2 <- dplyr::inner_join(my_summary, cld_tbl, by = c(var2, var1))
    df2$lo <- df2$Mean - df2$ci
    df2$hi <- df2$Mean + df2$ci
    lims <- limits_from(dfp$y, df2$lo, df2$hi)
    df2$cld_y <- lims$cld_y

    p <- build_plot(df2, dfp, "Mean", "lo", "hi", lims, y_label_for(measure, "mean"), measure)
    save_plot(p, file.path(out_dir, paste0("plot_", sanitize_name(measure), "_normal")), plot_width, plot_height)

    save(cld_tbl,      "CLD_parametric")
    save(my_summary,   "Summary")
    save(anova_result, "Anova")
    save(tukey_results[, c(1,3,4,9,10)], "Tukey")
    return(p)
  }

  # Non-parametric: median ± bootstrap CI; Kruskal; Dunn CLD when k >= 3; KW 2-group CLD otherwise
  conf_int <- rcompanion::groupwiseMedian(
    data = as.data.frame(dfp), var = "y", group = c(var2, var1),
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
    cld_tbl <- generate_cld(dunn_tbl, var1, var2)
  } else {
    lv2 <- levels(dfp[[var2]])
    cld_rows <- lapply(levels(dfp[[var1]]), function(facet) {
      pval <- kw_tbl$p[kw_tbl[[var1]] == facet]
      lets <- if (length(pval) == 0 || is.na(pval) || pval >= 0.05) c("a","a") else c("a","b")
      data.frame(facet_val = facet, var2_val = lv2, cld = lets, stringsAsFactors = FALSE)
    })
    cld_tbl <- do.call(rbind, cld_rows)
    names(cld_tbl) <- c(var1, var2, "cld")
  }

  df2 <- dplyr::inner_join(conf_int, cld_tbl, by = c(var2, var1))
  df2[[var2]] <- factor(df2[[var2]], levels = levels(df_data[[var2]]))
  df2[[var1]] <- factor(df2[[var1]], levels = levels(df_data[[var1]]))
  lims <- limits_from(dfp$y, df2$Percentile.lower, df2$Percentile.upper)
  df2$cld_y <- lims$cld_y

  p <- build_plot(df2, dfp, "Median", "Percentile.lower", "Percentile.upper", lims,
                  y_label_for(measure, "median"), measure)
  save_plot(p, file.path(out_dir, paste0("plot_", sanitize_name(measure), "_nonpara")), plot_width, plot_height)

  save(kw_tbl,   "Kruskal")
  save(conf_int, "Summary")
  if (k >= 3) {
    save(dunn_tbl, "Dunn")
    save(cld_tbl,  "CLD_nonparametric")
  } else {
    save(cld_tbl,  "CLD_kw2groups")
  }
  return(p)
}

# ---- Load data ---------------------------------------------------------------
df <- readxl::read_excel(DATA) %>% set_factor_levels(var1, var2, order1, order2)

measure_cols <- detect_measures(df, var1, var2, MEASURE_INCLUDE)
stopifnot("No measure columns detected." = length(measure_cols) > 0)

# ---- Run per measure & collect -----------------------------------------------
dir.create(OUTPUT_BASE, showWarnings = FALSE, recursive = TRUE)

plots <- list()
for (m in measure_cols) {
  p_try <- try(analyze_one_measure(df, m, file.path(OUTPUT_BASE, sanitize_name(m))), silent = TRUE)
  if (inherits(p_try, "try-error") || !inherits(p_try, "ggplot")) {
    message("Skipping plot for measure: ", m, "\n", p_try)
  } else {
    plots[[m]] <- p_try
  }
}

# ---- Combine into a single figure --------------------------------------------
stopifnot("No plots were generated." = length(plots) > 0)

ncol_eff <- min(combined_ncol, length(plots))
n_rows   <- ceiling(length(plots) / ncol_eff)
combined <- patchwork::wrap_plots(plotlist = plots, ncol = ncol_eff)

save_plot(combined, file.path(OUTPUT_BASE, "ALL_PSrates_bar_CLD"),
          width = ncol_eff * plot_width, height = n_rows * plot_height)
