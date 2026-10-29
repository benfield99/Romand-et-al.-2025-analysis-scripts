# ==============================================================================
# Figure 3D - Normalised Fv/Fm (0 h = 1) during excess light and recovery, overlay plots
# Family:  Fv/Fm-overlay, companion to the Fv/Fm-ART script for the same figure. The same script
#          is used for Fig S8C; the copies differ in line names and panel groupings only.
# Input:   combinedresults_normalized.txt (column normalized_value)
# Output:  FvFm_overlays_spline.svg
# Method:  points are means per line and time point with BCa bootstrap 95% CI (R = 5000);
#          lines are natural cubic splines through the means, shown as a visual guide.
# Run:     with the working directory set to this folder.
# ==============================================================================

if (!require(dplyr))     install.packages("dplyr")
if (!require(ggplot2))   install.packages("ggplot2")
if (!require(patchwork)) install.packages("patchwork")
if (!require(svglite))   install.packages("svglite")
if (!require(boot))      install.packages("boot")

library(dplyr)
library(ggplot2)
library(patchwork)
library(boot)

# Load data
df <- read.table("combinedresults_normalized.txt", header = TRUE, sep = "\t")

# Time factor in experimental order; numeric index for spline fitting
time_order  <- c("T0", "T2h-HL", "T0.5-r", "T2h-r", "T4h-r")
df$time     <- factor(df$time, levels = time_order)
df$time_num <- as.numeric(df$time)   # 1–5, preserving factor order

df <- df |> filter(!is.na(normalized_value))

line_order <- c("WT", "h1", "npq1", "npq4", "h1npq1", "h1npq4")

df$line <- factor(df$line, levels = line_order)

my_colors <- c(
  "WT"     = "#2D6A4F",
  "h1"     = "#52B788",
  "npq1"   = "#74C69D",
  "npq4"   = "#40916C",
  "h1npq1" = "#95D5B2",
  "h1npq4" = "#B7E4C7"
)

my_shapes <- c(
  "WT"     = 21,
  "h1"     = 24,
  "npq1"   = 22,
  "npq4"   = 23,
  "h1npq1" = 25,
  "h1npq4" = 8
)

base_theme <- theme_classic(base_size = 9) +
  theme(
    axis.text.x           = element_text(angle = 45, hjust = 1, size = 8),
    legend.title          = element_blank(),
    legend.key.size       = unit(0.35, "cm"),
    legend.text           = element_text(size = 7.5),
    legend.position       = c(0.99, 0.03),
    legend.justification  = c(1, 0),
    legend.background     = element_rect(fill = alpha("white", 0.85), color = NA),
    legend.box.background = element_rect(
      fill = alpha("white", 0.85), color = "grey80", linewidth = 0.25
    ),
    legend.margin = margin(3, 5, 3, 5)
  )

set.seed(42)

# Fine x-grid for smooth predictions (200 points across the time range)
time_grid <- seq(1, length(time_order), length.out = 200)

# BCa bootstrap 95% CI for the mean at each line × time point (R = 5000)
boot_stats <- function(data, nboot = 5000) {
  data |>
    group_by(line, time_num) |>
    group_modify(~ {
      x  <- .x$normalized_value
      b  <- boot(x, function(d, i) mean(d[i]), R = nboot)
      ci <- boot.ci(b, type = "bca", conf = 0.95)
      data.frame(
        mean_y = mean(x),
        lwr    = ci$bca[4],
        upr    = ci$bca[5]
      )
    }) |>
    ungroup()
}

# Natural cubic spline through per-timepoint means, evaluated on a fine grid.
fit_spline_preds <- function(data) {
  data |>
    group_by(line, time_num) |>
    summarise(mean_y = mean(normalized_value), .groups = "drop") |>
    group_by(line) |>
    group_modify(~ {
      dat    <- .x |> arrange(time_num)
      fit_fn <- splinefun(dat$time_num, dat$mean_y, method = "natural")
      data.frame(time_num = time_grid, fit = fit_fn(time_grid))
    }) |>
    ungroup()
}

make_panel <- function(lines_to_show, show_y_label = FALSE) {
  d      <- df |> filter(line %in% lines_to_show)
  d$line <- factor(d$line, levels = lines_to_show)

  preds      <- fit_spline_preds(d)
  preds$line <- factor(preds$line, levels = lines_to_show)

  stats      <- boot_stats(d)
  stats$line <- factor(stats$line, levels = lines_to_show)

  cols   <- my_colors[lines_to_show]
  shapes <- my_shapes[lines_to_show]

  ggplot(preds, aes(x = time_num, group = line, color = line)) +
    geom_line(aes(y = fit), linewidth = 0.9) +
    geom_errorbar(
      data = stats,
      aes(y = mean_y, ymin = lwr, ymax = upr),
      width = 0.15, linewidth = 0.45
    ) +
    geom_point(
      data = stats,
      aes(y = mean_y, shape = line, fill = line),
      size = 1.8, stroke = 0.6
    ) +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    scale_shape_manual(values = shapes) +
    scale_x_continuous(breaks = seq_along(time_order), labels = time_order) +
    scale_y_continuous(breaks = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
    coord_cartesian(ylim = c(0.4, 1.1)) +
    labs(x = NULL, y = if (show_y_label) "Normalized Fv/Fm" else NULL) +
    base_theme
}

p1 <- make_panel(c("WT", "h1"),              show_y_label = TRUE)
p2 <- make_panel(c("WT", "npq1", "npq4"),   show_y_label = FALSE)
p3 <- make_panel(c("WT", "h1npq1", "h1npq4"), show_y_label = FALSE)

combined <- p1 + p2 + p3 + plot_layout(ncol = 3)

ggsave("FvFm_overlays_spline.svg", combined, width = 8, height = 3)
message("Saved FvFm_overlays_spline.svg")
