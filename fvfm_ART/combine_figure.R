# ==============================================================================
# Figure S6 - Assembly of the six plots on one A4 landscape page (panel A: raw, panel B: normalised)
# Input:   <RESPONSE>/<RESPONSE>_plot.rds for the six RESPONSE values, written by
#          fvfm_F0_Fm_detached_leaf_ART.r (run that script once for each RESPONSE first)
# Output:  combined_figure_A4_landscape.svg (editor-safe for Affinity / Inkscape) and .pdf
# Method:  one row per variable (Fv/Fm, F0, Fm); raw values on the left (panel A), values relative
#          to the mean at 0 h on the right (panel B). Line and time codes are replaced by the names
#          used in the figure; time points are labelled on the top row only and the replicate
#          symbols are explained in the figure legend, not on the figure.
#          Sized to print at 100% on A4 landscape (270 x 150 mm, leaving room for the legend),
#          text 6-7 pt.
# Run:     with the working directory set to this folder.
# ==============================================================================

# ---- User settings -----------------------------------------------------------
# Panels in reading order (left to right, top to bottom): folder name and y-axis title
PANELS <- list(
  list(folder = "Fv_Fm",            ylab = "Fv/Fm"),
  list(folder = "normalized_value", ylab = "Fv/Fm (relative to 0 h)"),
  list(folder = "Fo",               ylab = "F0"),
  list(folder = "Fo_norm",          ylab = "F0 (relative to 0 h)"),
  list(folder = "Fm",               ylab = "Fm"),
  list(folder = "Fm_norm",          ylab = "Fm (relative to 0 h)")
)
N_COL <- 2

# Order of the lines on the x axis (as in the analysis script)
target_order <- c("WT", "h1", "npq1", "npq4", "h1npq1", "h1npq4")

# Names shown on the x axis; lines not listed keep their own name
LINE_LABELS <- c(h1 = "OXRSH3.1", h1npq1 = "OXRSH3.1 npq1", h1npq4 = "OXRSH3.1 npq4")

# Time point names shown above each group of bars
TIME_LABELS <- c("T0" = "0h", "T2h-HL" = "2h-EL", "T0.5-r" = "0.5h-R", "T2h-r" = "2h-R", "T4h-r" = "4h-R")

# Panel letters, one per plot in the order of PANELS ("" = no letter)
TAGS <- c("A", "B", "", "", "", "")

# Page size (inches) and sizes at final print size
FIG_WIDTH  <- 270 / 25.4
FIG_HEIGHT <- 150 / 25.4
TEXT_PT    <- 7      # axis titles, strips, legend
LETTER_PT  <- 6      # CLD letters
POINT_SIZE <- 0.5    # individual leaves

OUT_BASE <- "combined_figure_A4_landscape"

# ---- Packages ----------------------------------------------------------------
library(ggplot2)
library(patchwork)
library(svglite)

# ---- SVG export --------------------------------------------------------------
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

# ---- Load and restyle each panel ---------------------------------------------
relabel_lines <- function(x) ifelse(x %in% names(LINE_LABELS), LINE_LABELS[x], x)

load_panel <- function(panel, is_top, is_bottom) {
  rds <- file.path(panel$folder, paste0(panel$folder, "_plot.rds"))
  if (!file.exists(rds)) stop("Missing ", rds, ": run the analysis script with RESPONSE <- \"", panel$folder, "\"")
  p <- readRDS(rds)
  for (i in seq_along(p$layers)) {
    geom <- class(p$layers[[i]]$geom)[1]
    if (geom == "GeomText")  p$layers[[i]]$aes_params$size <- LETTER_PT / .pt
    if (geom == "GeomPoint") p$layers[[i]]$aes_params$size <- POINT_SIZE
  }
  p <- p +
    scale_x_discrete(limits = target_order, labels = relabel_lines) +
    facet_wrap(~time, nrow = 1, scales = "free_x", labeller = as_labeller(TIME_LABELS)) +
    labs(y = panel$ylab, x = NULL, color = "Replicate", shape = "Replicate") +
    theme(text         = element_text(size = TEXT_PT),
          axis.title.y = element_text(size = TEXT_PT),
          axis.text    = element_text(size = TEXT_PT - 1),
          strip.text   = element_text(size = TEXT_PT, margin = margin(1, 0, 1, 0)),
          legend.text  = element_text(size = TEXT_PT),
          legend.title = element_text(size = TEXT_PT),
          plot.margin  = margin(2, 2, 2, 2))
  # Line names are written under the bottom row only
  if (!is_bottom) p <- p + theme(axis.text.x = element_blank())
  # Time point names are written above the top row only
  if (!is_top) p <- p + theme(strip.text = element_blank(), strip.background = element_blank())
  p
}

plots <- lapply(seq_along(PANELS), function(i) load_panel(PANELS[[i]], is_top = i <= N_COL, is_bottom = i > length(PANELS) - N_COL))

fig <- wrap_plots(plots, ncol = N_COL, byrow = TRUE) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = list(TAGS)) &
  theme(plot.tag = element_text(face = "bold", size = TEXT_PT + 2),
        legend.position = "none")   # replicate symbols are described in the legend

# ---- Save --------------------------------------------------------------------
ggsave(paste0(OUT_BASE, ".svg"), fig, width = FIG_WIDTH, height = FIG_HEIGHT,
       device = svglite::svglite, bg = "transparent")
make_svg_editor_safe(paste0(OUT_BASE, ".svg"))
ggsave(paste0(OUT_BASE, ".pdf"), fig, width = FIG_WIDTH, height = FIG_HEIGHT,
       device = grDevices::cairo_pdf, bg = "transparent")
cat("Saved ", OUT_BASE, ".svg and ", OUT_BASE, ".pdf\n", sep = "")
