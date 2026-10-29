# ==============================================================================
# Figure 1B - Photobleaching after 48 h excess light (WT, OX:RSH3.1, OX:RSH3.2)
# Family:  bleaching-ART (one time point). The same script is used for Fig 2D and Fig S7C;
#          the copies differ in the settings block only (Fig S7C also harmonises line names).
# Input:   rep1_long.csv, rep2_long.csv, rep3_long.csv (one file per experiment; column pct_bleached)
# Output:  bleach_panel_48h23.svg, Summary_median_CI_48h.txt, ART_Anova_48h_line.txt,
#          ART_emm_pairs_line_48h.txt, ART_CLD_line_48h.txt
# Method:  median and bootstrap 95% CI per line; aligned-ranks (ART) mixed model
#          pct_bleached ~ line + (1|rep) at TARGET_TIME; BH-adjusted pairwise contrasts; CLD letters.
# Run:     paths are resolved relative to the script folder.
# ==============================================================================

# ---- User settings -----------------------------------------------------------
DATA_FILES <- c(
  rep1 = "rep1_long.csv",
  rep2 = "rep2_long.csv",
  rep3 = "rep3_long.csv"
)

TARGET_TIME <- "48h"
MEASURE_COL <- "pct_bleached"
var_line    <- "line"
var_time    <- "time"
var_rep     <- "rep"

order_line <- c("col", "h1", "g8")

plot_width  <- 2
plot_height <- 4


# ---- Resolve paths robustly for Rscript and IDE runs -------------------------
cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- cmd_args[grep("^--file=", cmd_args)]
if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE)
  script_dir  <- dirname(script_path)
} else {
  script_dir <- getwd()
}

data_file_names <- names(DATA_FILES)
DATA_FILES <- stats::setNames(file.path(script_dir, unname(DATA_FILES)), data_file_names)
out_path <- function(filename) file.path(script_dir, filename)


# ---- Packages ----------------------------------------------------------------
if (!require(dplyr))      install.packages("dplyr")
if (!require(ggplot2))    install.packages("ggplot2")
if (!require(ggbeeswarm)) install.packages("ggbeeswarm")
if (!require(rcompanion)) install.packages("rcompanion")
if (!require(ARTool))     install.packages("ARTool")
if (!require(emmeans))    install.packages("emmeans")
if (!require(multcomp))   install.packages("multcomp")
if (!require(rlang))      install.packages("rlang")
if (!require(svglite))    install.packages("svglite")

library(dplyr)
library(ggplot2)
library(ggbeeswarm)
library(rcompanion)
library(ARTool)
library(emmeans)
library(multcomp)
library(rlang)
library(svglite)


set_levels_preserve <- function(x, target_levels) {
  x <- as.factor(x)
  extras <- setdiff(levels(x), target_levels)
  factor(x, levels = c(target_levels, extras))
}

read_one_input <- function(path, rep_name) {
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  need <- c(var_line, var_time, MEASURE_COL)
  miss <- setdiff(need, names(df))
  if (length(miss) > 0) {
    stop("Missing required columns in ", basename(path), ": ", paste(miss, collapse = ", "))
  }
  df[[var_rep]] <- rep_name
  df
}

missing_files <- DATA_FILES[!file.exists(DATA_FILES)]
if (length(missing_files) > 0) {
  stop("Missing input file(s): ", paste(unname(missing_files), collapse = ", "))
}

df_data <- bind_rows(
  lapply(seq_along(DATA_FILES), function(i) {
    read_one_input(DATA_FILES[[i]], names(DATA_FILES)[i])
  })
)

df_data <- df_data %>%
  mutate(
    !!var_line := as.factor(.data[[var_line]]),
    !!var_time := as.factor(.data[[var_time]]),
    !!var_rep  := as.factor(.data[[var_rep]]),
    !!MEASURE_COL := as.numeric(.data[[MEASURE_COL]])
  ) %>%
  filter(!is.na(.data[[MEASURE_COL]]), !is.na(.data[[var_time]]), .data[[var_time]] == TARGET_TIME)

if (nrow(df_data) == 0) {
  stop("No rows found for TARGET_TIME = ", TARGET_TIME)
}

df_data[[var_line]] <- set_levels_preserve(df_data[[var_line]], order_line)


# Summary (median + bootstrap CI)
conf_int <- tryCatch(
  rcompanion::groupwiseMedian(
    data       = df_data,
    var        = MEASURE_COL,
    group      = var_line,
    conf       = 0.95,
    R          = 5000,
    percentile = TRUE,
    bca        = TRUE,
    digits     = 3
  ),
  error = function(e) {
    message("BCa confidence intervals failed; retrying with percentile intervals only. Details: ", e$message)
    rcompanion::groupwiseMedian(
      data       = df_data,
      var        = MEASURE_COL,
      group      = var_line,
      conf       = 0.95,
      R          = 5000,
      percentile = TRUE,
      bca        = FALSE,
      digits     = 3
    )
  }
)

write.table(conf_int, out_path("Summary_median_CI_48h.txt"), quote = FALSE, row.names = FALSE, sep = "\t")


# ART mixed model at 48h only
art_formula <- as.formula(paste(MEASURE_COL, "~", var_line, "+ (1|", var_rep, ")"))
art_model <- art(art_formula, data = df_data)
art_anova <- anova(art_model)

write.table(
  as.data.frame(art_anova),
  out_path("ART_Anova_48h_line.txt"),
  quote = FALSE, row.names = TRUE, sep = "\t"
)

emm_line <- emmeans(artlm(art_model, var_line), specs = as.formula(paste("~", var_line)))
pairs_line <- pairs(emm_line, adjust = "BH")

write.table(
  as.data.frame(pairs_line),
  out_path("ART_emm_pairs_line_48h.txt"),
  quote = FALSE, row.names = FALSE, sep = "\t"
)

cld_line <- multcomp::cld(emm_line, Letters = letters, adjust = "BH")
cld_df <- as.data.frame(cld_line) %>%
  mutate(.group = gsub("\\s+", "", .group)) %>%
  rename(!!var_line := !!sym(var_line))

write.table(
  cld_df,
  out_path("ART_CLD_line_48h.txt"),
  quote = FALSE, row.names = FALSE, sep = "\t"
)


# Plot table
plot_df <- conf_int %>%
  rename(
    Median = Median,
    Lower  = Percentile.lower,
    Upper  = Percentile.upper
  ) %>%
  left_join(cld_df %>% dplyr::select(!!sym(var_line), .group), by = var_line)

plot_df[[var_line]] <- set_levels_preserve(plot_df[[var_line]], order_line)

data_min <- min(df_data[[MEASURE_COL]], na.rm = TRUE)
data_max <- max(df_data[[MEASURE_COL]], na.rm = TRUE)
data_range <- max(data_max - data_min, 1e-9)
lower_limit <- 0
upper_limit <- data_max + 0.08 * data_range
cld_y <- data_max + 0.04 * data_range

p <- ggplot(plot_df, aes(x = !!sym(var_line), y = Median)) +
  geom_col(width = 0.6, colour = "dark grey", fill = "white", linewidth = 0.3) +
  ggbeeswarm::geom_quasirandom(
    data = df_data,
    aes(
      x = !!sym(var_line),
      y = !!sym(MEASURE_COL),
      shape = !!sym(var_rep),
      color = !!sym(var_rep)
    ),
    width = 0.3, alpha = 0.75, size = 1.2, stroke = 0.3
  ) +
  scale_color_manual(values = c(rep1 = "#006400", rep2 = "#32CD32", rep3 = "#98FB98")) +
  geom_segment(aes(xend = !!sym(var_line), y = Lower, yend = Upper), colour = "black", linewidth = 0.3) +
  geom_text(aes(label = .group), y = cld_y, size = 3) +
  scale_x_discrete(limits = order_line, expand = expansion(mult = c(0.2, 0.1))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks = scales::pretty_breaks()) +
  coord_cartesian(ylim = c(lower_limit, upper_limit)) +
  labs(x = var_line, y = paste0(MEASURE_COL, " (48h, median \u00b1 CI)")) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, colour = "black"),
    axis.text.y = element_text(colour = "black"),
    axis.title.x = element_text(colour = "black"),
    axis.title.y = element_text(colour = "black"),
    panel.background = element_rect(fill = NA, colour = NA),
    plot.background = element_rect(fill = NA, colour = NA),
    legend.position = "none"
  )

ggsave(out_path("bleach_panel_48h23.svg"), plot = p, width = plot_width, height = plot_height, bg = "transparent")

cat(
  "\nDONE. Files written:\n",
  "- Summary_median_CI_48h.txt\n",
  "- ART_Anova_48h_line.txt\n",
  "- ART_emm_pairs_line_48h.txt\n",
  "- ART_CLD_line_48h.txt\n",
  "- bleach_panel_48h.svg\n"
)
