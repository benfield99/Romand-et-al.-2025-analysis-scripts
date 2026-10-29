# ==============================================================================
# Figure S1 - ppGpp levels in WT, OX:RSH3 lines and OX:RSH3.1 npq1 / npq4 / ch1
# Family:  ANOVA-bar, version 4 (as Fig 4E), extended with: automatic swap of var1/var2 when var2
#          has a single level, factor order taken from the Excel file, and SVG/PDF/PNG export.
# Input:   data.xlsx
# Output:  out_ppGpp/plot_ppGpp_normal.svg/.pdf/.png; out_ppGpp/Shapiro_residuals.txt, Anova.txt,
#          Tukey.txt, cld_table_parametric.txt, Summary.txt (Kruskal/Dunn tables otherwise)
# Method:  Shapiro-Wilk test on the pooled ANOVA residuals. Normal: one-way ANOVA, Tukey HSD,
#          CLD letters, mean +/- 95% CI. Otherwise: Kruskal-Wallis, Dunn test (BH), CLD letters,
#          median +/- bootstrap 95% CI.
# Run:     with the working directory set to this folder.
# ==============================================================================

# ---- User settings -----------------------------------------------------------
# Parameters for plot
COLUMN_LINE_COLOUR <- "dark grey"
COLUMN_FILL_COLOUR <- "white"
POINT_COLOR <- "black" 
PALETTE <- "Greens"  # Will use PALETTE if uncommented, else use COLUMN_FILL_COLOUR

# File to analyse
DATA <- "data.xlsx"

# Global parameters
# var1 = facet (one panel per level), var2 = groups compared by the stats (x axis).
# If var2 has only one level inside each var1 group (e.g. a single Treatment), the two
# are swapped automatically so that the var1 groups (e.g. Lines) are compared instead.
var1 <- "Line"
var2 <- "Treatment"
MEASURE_COL <- "ppGpp"
Y_LABEL <- "ppGpp (pmol/g)"

# Output
OUTPUT_DIR <- "out_ppGpp"

# Plot dimensions
plot_width <- 4
plot_height <- 5

# Reference 'var1': the group against which the statistical tests are performed
RefLine <- "WT"  # a string between ""


# ---- Packages ----------------------------------------------------------------
if (!require(dplyr)) { install.packages("dplyr") }
if (!require(tidyverse)) { install.packages("tidyverse") }
if (!require(devtools)) { install.packages("devtools") }
if (!require(rstatix)) { install.packages("rstatix", repos = "https://cloud.r-project.org") }
if (!require(ggbeeswarm)) { install.packages("ggbeeswarm") }
if (!require(RColorBrewer)) { install.packages("RColorBrewer") }
if (!require(rcompanion)) { install.packages("rcompanion") }
if (!require(readxl)) { install.packages("readxl") }
if (!requireNamespace("svglite", quietly = TRUE)) { install.packages("svglite") }


# ---- Functions ---------------------------------------------------------------


# Perform Dunn test
# Return the p-values in a data frame
test_dunn <- function(df_data, var1, var2, MEASURE_COL) {
  pval <- df_data %>%
    group_by(!!sym(var1)) %>%
    dunn_test(formula = as.formula(paste(MEASURE_COL, "~", var2)), p.adjust.method = "BH") %>%
    as.data.frame()

  return(pval)
}


# SVG / PDF export
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


# ---- Load data ---------------------------------------------------------------
library(readxl)
df_data <- read_excel(DATA)
df_data[[MEASURE_COL]] <- as.numeric(df_data[[MEASURE_COL]])
df_data <- df_data[!is.na(df_data[[MEASURE_COL]]), ]

# Kruskal-Wallis / ANOVA need at least 2 groups of var2 inside each var1 panel.
# With a single var2 level (e.g. one Treatment), compare the var1 groups instead.
n_groups <- tapply(df_data[[var2]], df_data[[var1]], function(x) length(unique(x)))
if (all(n_groups < 2)) {
  message("Only one '", var2, "' per '", var1, "': comparing ", var1, " groups instead.")
  tmp <- var1; var1 <- var2; var2 <- tmp
}

dir.create(OUTPUT_DIR, showWarnings = FALSE)

# ---- Statistical analysis ----------------------------------------------------

library(ggbeeswarm) # for function geom_quasirandom
library(RColorBrewer) # to define colors
library(rstatix)  # for statistical test
library(rcompanion) # to compute confidence interval for non parametric data
library(forcats)

# Convert var1 and var2 into factors to keep the order of the Excel file in the plot
df_data[[var1]] <- factor(df_data[[var1]], levels = unique(df_data[[var1]]))
df_data[[var2]] <- factor(df_data[[var2]], levels = unique(df_data[[var2]]))
# Restore that order after merge(), which returns characters
set_order <- function(df) {
  df[[var1]] <- factor(df[[var1]], levels = levels(df_data[[var1]]))
  df[[var2]] <- factor(df[[var2]], levels = levels(df_data[[var2]]))
  df
}

# Normality is checked on the pooled ANOVA residuals (the actual ANOVA assumption),
# not group by group. With several var1 panels, the var2 x var1 model gives the same
# residuals as the per-panel ANOVAs (each value minus its group mean).
model_formula <- if (nlevels(df_data[[var1]]) > 1) {
  reformulate(paste(var2, "*", var1), MEASURE_COL)
} else {
  reformulate(var2, MEASURE_COL)
}
model <- lm(model_formula, data = df_data)
shapiro_res <- shapiro.test(residuals(model))
flag_normal <- shapiro_res$p.value > 0.05
print(shapiro_res)
write.table(data.frame(W = shapiro_res$statistic, p = shapiro_res$p.value),
            file = file.path(OUTPUT_DIR, "Shapiro_residuals.txt"), quote = FALSE, row.names = FALSE, sep = '\t')

# Data treatment according to normality status
if(flag_normal == TRUE) {
  print("Datas follow a normal law")

  # Summary
  if (!require(Rmisc)) {install.packages("Rmisc")}
  library(plyr) # rmisc dependency
  library(Rmisc) # for the command summarySE
  my_summary <- summarySE(df_data, measurevar = MEASURE_COL, groupvars = c(var2, var1))

  # Detach packages
  detach(package:Rmisc)
  detach(package:plyr)

  # Stats
  anova_result <- df_data %>%
    group_by(!!sym(var1)) %>%
    rstatix::anova_test(formula = reformulate(var2, MEASURE_COL))

  # Explicitly select the columns to ensure grouping works correctly
  df_data_selected <- df_data %>%
    dplyr::select(!!sym(var2), !!sym(var1), !!sym(MEASURE_COL))

  # Perform Tukey HSD Test
  tukey_results <- df_data_selected%>%
    group_by(!!sym(var1)) %>% 
    rstatix::tukey_hsd(as.formula(paste(MEASURE_COL, "~", var2)))


    # Generate CLD letters for the parametric test within each var2 level
    cld_table_parametric <- tukey_results %>%
      group_by(!!sym(var1)) %>%
      summarise(
        cld = list(multcompView::multcompLetters(setNames(p.adj, paste(group1, group2, sep = "-")), Letters = letters)$Letters),
        .groups = 'drop'
      ) %>%
      unnest_longer(cld) %>%
      mutate(!!var2 := names(cld))

    # Save CLD table for parametric test to a text file
    write.table(cld_table_parametric, file = file.path(OUTPUT_DIR, "cld_table_parametric.txt"), quote = FALSE, row.names = FALSE, sep = '\t')


  # Save the files
  write.table(my_summary, file = file.path(OUTPUT_DIR, "Summary.txt"), 
              quote = FALSE, row.names = FALSE, sep = '\t')
  write.table(anova_result, file = file.path(OUTPUT_DIR, "Anova.txt"), 
              quote = FALSE, row.names = FALSE, sep = '\t')
  write.table(tukey_results[, c(1,3,4, 9, 10)], file = file.path(OUTPUT_DIR, "Tukey.txt"), 
                quote = FALSE, row.names = FALSE, sep = '\t')


  # Prepare data to add CLD letters on error bar

  df2 <- set_order(merge(my_summary, cld_table_parametric, by.x = c(var2, var1), by.y = c(var2, var1)))
  # CLD letters just above the top of the error bar
  df2$cld_y <- df2[[MEASURE_COL]] + df2$ci + 0.05 * max(df2[[MEASURE_COL]] + df2$ci, na.rm = TRUE)

  # Plotting
  p <- df2 %>%
    ggplot(aes(x = !!sym(var2), y = !!sym(MEASURE_COL), fill = !!sym(var2))) +
    geom_col(color = COLUMN_LINE_COLOUR, width = 0.6, position = position_dodge2(padding = 0.05)) +
    # Conditionally apply fill color scaling
    { 
      if (exists("PALETTE")) {
        scale_fill_brewer(palette = PALETTE, direction = -1)  # first group (WT) darkest
      } else {
        scale_fill_manual(values = rep(COLUMN_FILL_COLOUR, length(unique(df2[[var2]]))))
      }
    } +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.1))    )  +
    geom_quasirandom(
      data = df_data,
      aes(x = !!sym(var2), y = !!sym(MEASURE_COL)),
      color = POINT_COLOR, width = 0.3, alpha = 0.6
    ) +
    geom_segment(
      aes(x = !!sym(var2), xend = !!sym(var2), y = pmax(0, !!sym(MEASURE_COL) - ci), yend = !!sym(MEASURE_COL) + ci),
      color = "black"
    ) +
    geom_text(
      aes(x = !!sym(var2), y = cld_y, label = cld),
      size = 3, inherit.aes = TRUE
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      panel.grid.minor.y = element_blank(),
      axis.line.x = element_line(linewidth = 0.5),
      axis.line.y = element_line(linewidth = 0.5),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = 'transparent', color = NA),
      plot.background = element_rect(fill = 'transparent', color = NA),
      axis.text.y = element_text(vjust = 1),
      legend.position = "none"
    ) +
    facet_wrap(as.formula(paste("~", var1)), nrow = 1) +
    labs(x = NULL, y = Y_LABEL)

  # Print the plot
  print(p)


  save_plot(p, file.path(OUTPUT_DIR, paste0("plot_", MEASURE_COL, "_normal")), plot_width, plot_height)


} else {
  print("Data is not normally distributed")

  # Summary
  # Calculate confidence intervals using groupwiseMedian
  conf_int <- groupwiseMedian(
    data = df_data,
    var = MEASURE_COL,
    group = c(var2, var1),
    conf = 0.95,
    R = 5000,
    percentile = TRUE,
    bca = FALSE,
    digits = 3
  )

  # Stats
  kruskal_pval <- df_data %>%
    group_by(!!sym(var1)) %>%
    kruskal_test(as.formula(paste(MEASURE_COL, "~", var2))) %>%
    dplyr::select(all_of(var1), p)

  # Check if there are significant results (p < 0.05)
  significant <- any(kruskal_pval$p < 0.05)

  if (significant) {
    pval_dunn <- test_dunn(df_data, var1, var2, MEASURE_COL)


    # Generate CLD letters for non-parametric test by each var2 group
    cld_table_nonparametric <- pval_dunn %>%
      group_by(!!sym(var1)) %>%
      summarise(
        cld = list(multcompView::multcompLetters(setNames(p.adj, paste(group1, group2, sep = "-")), Letters = letters)$Letters),
        .groups = 'drop'
      ) %>%
      unnest_longer(cld) %>%
      mutate(!!var2 := names(cld))

    # Save CLD table for non-parametric test to a text file
    write.table(cld_table_nonparametric, file = file.path(OUTPUT_DIR, "cld_table_nonparametric.txt"), quote = FALSE, row.names = FALSE, sep = '\t')
  }

  # Save Kruskal-Wallis and confidence interval results
  write.table(conf_int, file = file.path(OUTPUT_DIR, "Summary.txt"), quote = FALSE, row.names = FALSE, sep = '\t')
  write.table(kruskal_pval, file = file.path(OUTPUT_DIR, "Kruskal.txt"), quote = FALSE, row.names = FALSE, sep = '\t')

  # Save Dunn's Test results if significant
  if (significant) {
    write.table(pval_dunn[, c(1, 3, 4, 8, 9, 10)], file = file.path(OUTPUT_DIR, "Dunn.txt"), quote = FALSE, row.names = FALSE, sep = '\t')

    # Prepare data to add CLD letters to error bars
    df2 <- merge(conf_int, cld_table_nonparametric, by.x = c(var2, var1), by.y = c(var2, var1))


    # Ensure that var1 and var2 are factors in the Excel order
    df2 <- set_order(df2)


    # Plotting
    p <- df2 %>%
      ggplot(aes(x = !!sym(var2), y = Median, fill = !!sym(var2))) +
      geom_col(color = COLUMN_LINE_COLOUR, width = 0.6, position = position_dodge2(padding = 0.05)) +
      # Use PALETTE if uncommented, else use COLUMN_FILL_COLOUR
      # Conditionally apply fill color scaling
      { 
        if (exists("PALETTE")) {
          scale_fill_brewer(palette = PALETTE, direction = -1)  # first group (WT) darkest
        } else {
          scale_fill_manual(values = rep(COLUMN_FILL_COLOUR, length(unique(df2[[var2]]))))
        }
      } +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.1))) +
      geom_quasirandom(data = df_data, aes(x = !!sym(var2), y = !!sym(MEASURE_COL)), 
                       color = POINT_COLOR, width = 0.3, alpha = 0.6) +  # Set points to POINT_COLOR
      geom_segment(aes(x = !!sym(var2), xend = !!sym(var2), y = pmax(0, Percentile.lower), yend = Percentile.upper), 
                   color = "black") +  # Error bars are black
      geom_text(aes(x = !!sym(var2), y = Percentile.upper + (0.15 * Median), 
                    label = cld), size = 3, inherit.aes = TRUE) +  # Increase CLD letters by 15% of CI
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(linewidth = 0.5),
        axis.line.y = element_line(linewidth = 0.5),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = 'transparent', color = NA),
        plot.background = element_rect(fill = 'transparent', color = NA),
        axis.text.y = element_text(vjust = 1),
        legend.position = "none"  # Remove legend from the plot
      ) +
      facet_wrap(as.formula(paste("~", var1)), nrow = 1, scales = "free_y") +
      labs(x = NULL, y = Y_LABEL)

    # Print the plot
    print(p)

    save_plot(p, file.path(OUTPUT_DIR, paste0("plot_", MEASURE_COL, "_nonpara")), plot_width, plot_height)

  } else {
    print("Data are not significantly different, the Dunn test was not performed.")
  }
}
