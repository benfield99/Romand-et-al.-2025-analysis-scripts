# ==============================================================================
# Figure 4E - ppGpp content in WT under growth light and after excess light
# Family:  ANOVA-bar, version 4: generalised to any measure column with two factors
#          (var1 = panel, var2 = groups compared). Fig S1 uses an extended copy.
# Input:   ppgpp.xlsx
# Output:  plot_normal.svg (parametric) or plot_nonpara.svg (non-parametric); Shapiro_residuals.txt,
#          Anova.txt, Tukey.txt, cld_table_parametric.txt, Summary.txt (Kruskal/Dunn tables otherwise)
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
DATA <- "ppgpp.xlsx"

# Global parameters
var1 <- "Line"
var2 <- "Treatment"
MEASURE_COL <- "ppGpp"

# Plot dimensions
plot_width <- 3
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


# ---- Load data ---------------------------------------------------------------
library(readxl)
df_data <- read_excel(DATA)
df_data[[MEASURE_COL]] <- as.numeric(df_data[[MEASURE_COL]])

# ---- Statistical analysis ----------------------------------------------------

library(ggbeeswarm) # for function geom_quasirandom
library(RColorBrewer) # to define colors
library(rstatix)  # for statistical test
library(rcompanion) # to compute confidence interval for non parametric data
library(forcats)

# Convert 'var1' name into factor to force the order in the plot
df_data[[var1]] <- as.factor(df_data[[var1]])

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
            file = "Shapiro_residuals.txt", quote = FALSE, row.names = FALSE, sep = '\t')

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
    write.table(cld_table_parametric, file = "cld_table_parametric.txt", quote = FALSE, row.names = FALSE, sep = '\t')


  # Save the files
  write.table(my_summary, file = "Summary.txt", 
              quote = FALSE, row.names = FALSE, sep = '\t')
  write.table(anova_result, file = "Anova.txt", 
              quote = FALSE, row.names = FALSE, sep = '\t')
  write.table(tukey_results[, c(1,3,4, 9, 10)], file = "Tukey.txt", 
                quote = FALSE, row.names = FALSE, sep = '\t')


  # Prepare data to add CLD letters on error bar

  df2 <- merge(my_summary, cld_table_parametric, by.x = c(var2, var1), by.y = c(var2, var1))

  # Plotting
  p <- df2 %>%
    mutate(
      !!sym(var1) := as.factor(!!sym(var1)),
      !!sym(var2) := as.factor(!!sym(var2))
    ) %>%
    ggplot(aes(x = !!sym(var2), y = !!sym(MEASURE_COL), fill = !!sym(var2))) +
    geom_col(color = COLUMN_LINE_COLOUR, width = 0.6, position = position_dodge2(padding = 0.05)) +
    # Conditionally apply fill color scaling
    { 
      if (exists("PALETTE")) {
        scale_fill_brewer(palette = PALETTE)
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
      aes(x = !!sym(var2), y = !!sym(MEASURE_COL) + (0.15 * max(df2[[MEASURE_COL]], na.rm = TRUE)), label = cld),
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
    labs(x = "Treatment", y = "Median Value")

  # Print the plot
  print(p)


  ggsave("plot_normal.svg", width = plot_width, height = plot_height)


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
      mutate({{ var2 }} := names(cld))

    # Save CLD table for non-parametric test to a text file
    write.table(cld_table_nonparametric, file = "cld_table_nonparametric.txt", quote = FALSE, row.names = FALSE, sep = '\t')
  }

  # Save Kruskal-Wallis and confidence interval results
  write.table(conf_int, file = "Summary.txt", quote = FALSE, row.names = FALSE, sep = '\t')
  write.table(kruskal_pval, file = "Kruskal.txt", quote = FALSE, row.names = FALSE, sep = '\t')

  # Save Dunn's Test results if significant
  if (significant) {
    write.table(pval_dunn[, c(1, 3, 4, 8, 9, 10)], file = "Dunn.txt", quote = FALSE, row.names = FALSE, sep = '\t')

    # Prepare data to add CLD letters to error bars
    df2 <- merge(conf_int, cld_table_nonparametric, by.x = c(var2, var1), by.y = c(var2, var1))


    # Ensure that var1 and var2 are set as factors
    df2 <- df2 %>%
      mutate(!!sym(var1) := as.factor(!!sym(var1)),
             !!sym(var2) := as.factor(!!sym(var2)))


    # Plotting
    p <- df2 %>%
      ggplot(aes(x = !!sym(var2), y = Median, fill = !!sym(var2))) +
      geom_col(color = COLUMN_LINE_COLOUR, width = 0.6, position = position_dodge2(padding = 0.05)) +
      # Use PALETTE if uncommented, else use COLUMN_FILL_COLOUR
      # Conditionally apply fill color scaling
      { 
        if (exists("PALETTE")) {
          scale_fill_brewer(palette = PALETTE)
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
      labs(x = "Treatment", y = "Median Value")

    # Print the plot
    print(p)

    ggsave("plot_nonpara.svg", width = plot_width, height = plot_height)

  } else {
    print("Data are not significantly different, the Dunn test was not performed.")
  }
}
