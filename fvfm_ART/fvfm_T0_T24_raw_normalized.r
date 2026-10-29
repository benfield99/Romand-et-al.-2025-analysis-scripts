# ==============================================================================
# Figure S7D-E - Fv/Fm at 0 and 24 h excess light, raw (D) and normalised to 0 h (E)
# Family:  Fv/Fm-ART, derived from the 4-panel version (Fig 2E-F): time points T0 and T24 only,
#          with one analysis function applied to both raw and normalised Fv/Fm.
# Input:   combinedresults_normalized.txt (columns time, line, rep, Fv_Fm, normalized_value)
# Output:  fvfm_T0_T24_raw_normalized.svg (also fvfm_raw_T0_T24.svg, fvfm_normalized_T0_T24.svg);
#          art_anova_raw.txt, art_anova_normalized.txt, emmeans_posthoc_raw_*.txt,
#          emmeans_posthoc_normalized_*.txt
# Method:  normality is tested for each line x time group. If all groups are normal:
#          linear mixed model (Fv/Fm ~ line + (1|rep)) per time point, emmeans contrasts, CLD letters.
#          Otherwise: aligned-ranks ANOVA (ART) with the same model per time point, emmeans
#          contrasts on the aligned ranks, CLD letters.
# Run:     with the working directory set to this folder.
# ==============================================================================

# ---- User settings -----------------------------------------------------------
RefLine      <- "h1"
target_order <- c("col0", "h1", "ch1", "h1ch1", "glk12")
time_order   <- c("T0", "T24")
DATA         <- "combinedresults_normalized.txt"

# Install packages
if (!require(tidyr))       install.packages("tidyr")
if (!require(dplyr))       install.packages("dplyr")
if (!require(tidyverse))   install.packages("tidyverse")
if (!require(devtools))    install.packages("devtools")
if (!require(rstatix))     install.packages("rstatix", repos = "https://cloud.r-project.org")
if (!require(ggbeeswarm))  install.packages("ggbeeswarm")
if (!require(RColorBrewer))install.packages("RColorBrewer")
if (!require(rcompanion))  install.packages("rcompanion")
if (!require(svglite))     install.packages("svglite")
if (!require(patchwork))   install.packages("patchwork")

# ---- Functions ---------------------------------------------------------------

check_normality <- function(shapiro_df) {
  for (i in 1:nrow(shapiro_df)) {
    if (shapiro_df[i, "p.value"] <= 0.05) return(FALSE)
  }
  return(TRUE)
}

check_anova <- function(anova_results) {
  flag_anova <- FALSE
  for (i in 1:nrow(anova_results)) {
    if (anova_results$`Pr(>F)`[i] < 0.05) {
      print(paste0("Anova: p-value for group ", anova_results$time[i], " < 0.05 — post hoc performed."))
      flag_anova <- TRUE
    } else {
      print(paste0("Anova: p-value for group ", anova_results$time[i], " > 0.05 — no significant difference."))
    }
  }
  return(flag_anova)
}

theme_fvfm <- function() {
  theme_classic() +
    theme(
      axis.text.x        = element_text(angle = 45, vjust = 1, hjust = 1),
      panel.grid         = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.line.x        = element_line(size = 0.5),
      axis.line.y        = element_line(size = 0.5),
      panel.background   = element_rect(fill = "transparent", color = NA),
      plot.background    = element_rect(fill = "transparent", color = NA),
      axis.text.y        = element_text(vjust = 1)
    )
}

# ---- Load data ---------------------------------------------------------------

df_data_long <- read.table(DATA, header = TRUE, sep = "\t", fill = TRUE)

library(ggbeeswarm)
library(RColorBrewer)
library(rstatix)
library(rcompanion)
library(forcats)
library(patchwork)

df_data_long$line            <- factor(df_data_long$line, levels = target_order)
df_data_long$time            <- factor(df_data_long$time, levels = time_order)
df_data_long$Fv_Fm           <- as.numeric(df_data_long$Fv_Fm)
df_data_long$normalized_value <- as.numeric(df_data_long$normalized_value)

rep_colors <- c("#006400", "#32CD32", "#98FB98")

p_main <- NULL
p_norm <- NULL

# Generic analysis function: run parametric or non-parametric stats per time,
# return a ggplot with T0 | T24 facets.
# yvar   : column name (string) — "Fv_Fm" or "normalized_value"
# ylab   : y-axis label
# ybreaks: numeric vector for scale_y breaks
# prefix : prefix for output file names

build_faceted_plot <- function(df, yvar, ylab, ybreaks, prefix) {

  df[[yvar]] <- as.numeric(df[[yvar]])

  shapiro_df <- df %>%
    dplyr::group_by(time, line) %>%
    summarise(statistic = shapiro.test(.data[[yvar]])$statistic,
              p.value   = shapiro.test(.data[[yvar]])$p.value, .groups = "drop")

  flag_normal <- check_normality(shapiro_df)

# ---- Parametric --------------------------------------------------------------
  if (flag_normal) {

    print(paste0(ylab, ": normally distributed — parametric tests"))

    if (!require(Rmisc))        install.packages("Rmisc")
    if (!require(multcompView)) install.packages("multcompView")
    library(plyr); library(Rmisc)
    my_summary <- summarySE(df, measurevar = yvar, groupvars = c("time", "line"))
    if ("package:Rmisc" %in% search()) detach("package:Rmisc", unload = TRUE)
    if ("package:plyr"  %in% search()) detach("package:plyr",  unload = TRUE)

    if (!require(lme4))    install.packages("lme4")
    if (!require(emmeans)) install.packages("emmeans")
    library(lme4); library(emmeans); library(dplyr); library(multcompView)

    anova_results <- df %>%
      group_by(time) %>%
      group_modify(~ {
        model <- lmer(reformulate("line + (1 | rep)", response = yvar), data = .x)
        anova(model)
      })

    flag_anova <- check_anova(anova_results)

    if (flag_anova) {
      emmeans_results <- df %>%
        group_by(time) %>%
        group_modify(~ {
          model <- lmer(reformulate("line + (1 | rep)", response = yvar), data = .x)
          ep    <- emmeans(model, pairwise ~ line)
          write.table(as.data.frame(ep$contrasts),
                      file  = paste0("emmeans_posthoc_", prefix, "_", unique(.x$time), ".txt"),
                      quote = FALSE, row.names = FALSE, sep = "\t")
          as.data.frame(cld(ep$emmeans, Letters = letters))
        })

      cld_table <- do.call(rbind, emmeans_results)
      write.table(cld_table,    paste0("cld_", prefix, "_parametric.txt"),  quote = FALSE, row.names = FALSE, sep = "\t")
      write.table(my_summary,   paste0("summary_", prefix, ".txt"),         quote = FALSE, row.names = FALSE, sep = "\t")
      write.table(anova_results, paste0("anova_", prefix, ".txt"),          quote = FALSE, row.names = FALSE, sep = "\t")

      df2 <- merge(my_summary, cld_table, by = c("time", "line"))
      df2$time <- factor(df2$time, levels = time_order)

      ci_col <- df2[[yvar]]
      cld_y  <- df2[[yvar]] + df2$ci + diff(range(ybreaks)) * 0.12

      p <- df2 %>%
        mutate(line = factor(line, levels = target_order)) %>%
        ggplot(aes(x = line, y = .data[[yvar]], group = time)) +
        geom_col(color = "dark grey", fill = "white") +
        scale_x_discrete(limits = target_order) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15)), breaks = ybreaks) +
        geom_quasirandom(data = df,
                         aes(x = line, y = .data[[yvar]],
                             color = as.factor(rep), shape = as.factor(rep)),
                         width = 0.3, alpha = 0.6) +
        scale_color_manual(values = rep_colors) +
        geom_segment(aes(x = line, xend = line,
                         y = .data[[yvar]] - ci, yend = .data[[yvar]] + ci),
                     color = "black") +
        geom_text(aes(x = line,
                      y = .data[[yvar]] + ci + diff(range(ybreaks)) * 0.12,
                      label = .group), size = 3) +
        labs(y = ylab) +
        theme_fvfm() +
        facet_wrap(~time, nrow = 1, scales = "free_x")

      return(p)

    } else {
      print(paste0(ylab, ": no significant differences — post hoc skipped."))
      return(NULL)
    }

# ---- Non-parametric (ART ANOVA) ----------------------------------------------
  } else {

    print(paste0(ylab, ": not normally distributed — non-parametric tests"))
    df <- na.omit(df)

    conf_int <- groupwiseMedian(data      = df,
                                var       = yvar,
                                group     = c("time", "line"),
                                conf      = 0.95, R = 5000,
                                percentile = TRUE, bca = TRUE, digits = 3)

    filtered_data <- df %>%
      group_by(time, line) %>% filter(n() > 1) %>%
      ungroup() %>% filter(!is.na(time))

    if (!require(ARTool))  install.packages("ARTool")
    if (!require(emmeans)) install.packages("emmeans")
    if (!require(multcomp)) install.packages("multcomp")
    library(ARTool); library(emmeans); library(multcomp); library(dplyr)

    art_pval <- filtered_data %>%
      group_by(time) %>%
      group_modify(~ {
        current_time <- unique(.x$time)
        m <- tryCatch(art(reformulate("line + (1 | rep)", response = yvar), data = .x),
                      error = function(e) {
                        warning(paste("ART failed for", current_time, ":", e$message)); NULL
                      })
        if (is.null(m)) data.frame(p = NA)
        else            data.frame(p = anova(m)$`Pr(>F)`[1])
      })

    write.table(art_pval, paste0("art_anova_", prefix, ".txt"),
                quote = FALSE, row.names = FALSE, sep = "\t")
    flag_art <- any(!is.na(art_pval$p) & art_pval$p < 0.05)

    if (flag_art) {
      emmeans_cld <- filtered_data %>%
        mutate(line = factor(line, levels = target_order)) %>%
        group_by(time) %>%
        group_modify(~ {
          m  <- art(reformulate("line + (1 | rep)", response = yvar), data = .x)
          ep <- emmeans(artlm(m, "line"), pairwise ~ line)
          write.table(as.data.frame(ep$contrasts),
                      file  = paste0("emmeans_posthoc_", prefix, "_", unique(.x$time), ".txt"),
                      quote = FALSE, row.names = FALSE, sep = "\t")
          as.data.frame(cld(ep$emmeans, Letters = letters))
        })

      conf_int$time <- factor(conf_int$time, levels = time_order)
      df2 <- merge(conf_int, emmeans_cld, by = c("time", "line"))
      df2$time <- factor(df2$time, levels = time_order)

      p <- df2 %>%
        mutate(line = factor(line, levels = target_order)) %>%
        ggplot(aes(x = line, y = Median, group = time)) +
        geom_col(color = "dark grey", fill = "white") +
        scale_x_discrete(limits = target_order) +
        scale_y_continuous(breaks = ybreaks,
                           limits = c(0, max(df2$Percentile.upper, na.rm = TRUE) +
                                        diff(range(ybreaks)) * 0.2),
                           expand = c(0, 0)) +
        geom_quasirandom(data = df,
                         aes(x = line, y = .data[[yvar]],
                             color = as.factor(rep), shape = as.factor(rep)),
                         width = 0.3, alpha = 0.6) +
        scale_color_manual(values = rep_colors) +
        geom_segment(aes(x = line, xend = line,
                         y = Percentile.lower, yend = Percentile.upper),
                     color = "black") +
        geom_text(aes(x = line,
                      y = Percentile.upper + diff(range(ybreaks)) * 0.08,
                      label = .group), size = 3) +
        labs(y = ylab) +
        theme_fvfm() +
        facet_wrap(~time, nrow = 1, scales = "free_x")

      return(p)

    } else {
      print(paste0(ylab, ": no significant differences — post hoc skipped."))
      return(NULL)
    }
  }
}

# ---- PART 1: Raw Fv/Fm — T0, T24 ---------------------------------------------

p_main <- build_faceted_plot(
  df      = df_data_long,
  yvar    = "Fv_Fm",
  ylab    = "Fv/Fm",
  ybreaks = seq(0, 1, 0.2),
  prefix  = "raw"
)

# ---- PART 2: Normalized Fv/Fm — T0, T24 --------------------------------------

p_norm <- build_faceted_plot(
  df      = df_data_long,
  yvar    = "normalized_value",
  ylab    = "Normalized Fv/Fm",
  ybreaks = seq(0, 1.4, 0.2),
  prefix  = "normalized"
)

# ---- PART 3: Combine and save ------------------------------------------------

library(svglite)

if (!is.null(p_main) && !is.null(p_norm)) {
  combined_plot <- p_main + p_norm +
    plot_layout(widths = c(1, 1), guides = "collect")
  print(combined_plot)
  ggsave("fvfm_T0_T24_raw_normalized.svg", combined_plot, width = 11, height = 4)
  print("Saved: fvfm_T0_T24_raw_normalized.svg")
} else {
  if (!is.null(p_main)) {
    print(p_main); library(svglite)
    ggsave("fvfm_raw_T0_T24.svg", p_main, width = 6, height = 4)
    warning("p_norm could not be created — only raw panel saved.")
  }
  if (!is.null(p_norm)) {
    print(p_norm); library(svglite)
    ggsave("fvfm_normalized_T0_T24.svg", p_norm, width = 6, height = 4)
    warning("p_main could not be created — only normalized panel saved.")
  }
}
