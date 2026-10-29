# ==============================================================================
# Figure 2E-F - Fv/Fm at 0, 24 and 48 h excess light (E) and Fv/Fm at 48 h normalised to 0 h (F)
# Family:  Fv/Fm-ART, 4-panel version derived from version 7 (Fig 3C): panels 1-3 are raw Fv/Fm
#          at T0, T24 and T48; panel 4 is normalised Fv/Fm at T48 with its own y-axis.
# Input:   combinedresults_normalized.txt (columns time, line, rep, Fv_Fm, normalized_value)
# Output:  fvfm_4panel_normalized.svg; art_anova_raw.txt, emmeans_posthoc_raw_*.txt,
#          art_anova_normalized_T48.txt, emmeans_posthoc_normalized_T48.txt
# Method:  normality is tested for each line x time group. If all groups are normal:
#          linear mixed model (Fv/Fm ~ line + (1|rep)) per time point, emmeans contrasts, CLD letters.
#          Otherwise: aligned-ranks ANOVA (ART) with the same model per time point, emmeans
#          contrasts on the aligned ranks, CLD letters.
# Run:     with the working directory set to this folder.
# ==============================================================================

# ---- User settings -----------------------------------------------------------
RefLine <- "h1"
target_order <- c("col0", "H1", "npq1", "npq4", "H1npq1", "H1npq4")
time_order   <- c("T0", "T24", "T48")
DATA <- "combinedresults_normalized.txt"

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

# Shared ggplot theme for visual consistency across all panels
theme_fvfm <- function() {
  theme_classic() +
    theme(
      axis.text.x  = element_text(angle = 45, vjust = 1, hjust = 1),
      panel.grid   = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.line.x  = element_line(size = 0.5),
      axis.line.y  = element_line(size = 0.5),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background  = element_rect(fill = "transparent", color = NA),
      axis.text.y  = element_text(vjust = 1)
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

df_data_long$line            <- factor(df_data_long$line)
df_data_long$Fv_Fm           <- as.numeric(df_data_long$Fv_Fm)
df_data_long$normalized_value <- as.numeric(df_data_long$normalized_value)

rep_colors <- c("#006400", "#32CD32", "#98FB98", "#A9DFBF")

p_main <- NULL
p_norm <- NULL

# ---- PART 1: Raw Fv/Fm — T0, T24, T48 ----------------------------------------

shapiro_df <- df_data_long %>%
  dplyr::group_by(time, line) %>%
  summarise(statistic = shapiro.test(Fv_Fm)$statistic,
            p.value   = shapiro.test(Fv_Fm)$p.value, .groups = "drop")

flag_normal <- check_normality(shapiro_df)

if (flag_normal) {

  print("Raw Fv/Fm: normally distributed — parametric tests")

  if (!require(Rmisc))        install.packages("Rmisc")
  if (!require(multcompView)) install.packages("multcompView")
  library(plyr); library(Rmisc)
  my_summary <- summarySE(df_data_long, measurevar = "Fv_Fm", groupvars = c("time", "line"))
  if ("package:Rmisc" %in% search()) detach("package:Rmisc", unload = TRUE)
  if ("package:plyr"  %in% search()) detach("package:plyr",  unload = TRUE)

  if (!require(lme4))   install.packages("lme4")
  if (!require(emmeans)) install.packages("emmeans")
  library(lme4); library(emmeans); library(dplyr); library(multcompView)

  anova_results <- df_data_long %>%
    group_by(time) %>%
    group_modify(~ { model <- lmer(Fv_Fm ~ line + (1 | rep), data = .x); anova(model) })

  flag_anova <- check_anova(anova_results)

  if (flag_anova) {
    emmeans_results <- df_data_long %>%
      group_by(time) %>%
      group_modify(~ {
        model <- lmer(Fv_Fm ~ line + (1 | rep), data = .x)
        ep    <- emmeans(model, pairwise ~ line)
        write.table(as.data.frame(ep$contrasts),
                    file  = paste0("emmeans_posthoc_raw_", unique(.x$time), ".txt"),
                    quote = FALSE, row.names = FALSE, sep = "\t")
        as.data.frame(cld(ep$emmeans, Letters = letters))
      })

    cld_table <- do.call(rbind, emmeans_results)
    write.table(cld_table,    "cld_raw_parametric.txt", quote = FALSE, row.names = FALSE, sep = "\t")
    write.table(my_summary,   "summary_raw.txt",        quote = FALSE, row.names = FALSE, sep = "\t")
    write.table(anova_results,"anova_raw.txt",           quote = FALSE, row.names = FALSE, sep = "\t")

    df2 <- merge(my_summary, cld_table, by = c("time", "line"))

    p_main <- df2 %>%
      mutate(line = fct_relevel(line, target_order)) %>%
      ggplot(aes(x = line, y = Fv_Fm, group = time)) +
      geom_col(color = "dark grey", fill = "white") +
      scale_x_discrete(limits = target_order) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = seq(0, 1, 0.2)) +
      geom_quasirandom(data = df_data_long,
                       aes(x = line, y = Fv_Fm, color = as.factor(rep), shape = as.factor(rep)),
                       width = 0.3, alpha = 0.6) +
      scale_color_manual(values = rep_colors) +
      geom_segment(aes(x = line, xend = line, y = Fv_Fm - ci, yend = Fv_Fm + ci), color = "black") +
      geom_text(aes(x = line, y = Fv_Fm + ci + 0.15, label = .group), size = 3) +
      labs(y = "Fv/Fm") +
      theme_fvfm() +
      facet_wrap(~time, nrow = 1, scales = "free_x")

  } else {
    print("Raw Fv/Fm: no significant differences — post hoc skipped.")
  }

} else {

  # Non-parametric path (ART ANOVA)
  df_data_long <- na.omit(df_data_long)

  conf_int <- groupwiseMedian(data = df_data_long,
                               var       = "Fv_Fm",
                               group     = c("time", "line"),
                               conf      = 0.95, R = 5000,
                               percentile = TRUE, bca = TRUE, digits = 3)

  filtered_data <- df_data_long %>%
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
      m <- tryCatch(art(Fv_Fm ~ line + (1 | rep), data = .x),
                    error = function(e) { warning(paste("ART failed for", current_time, ":", e$message)); NULL })
      if (is.null(m)) data.frame(p = NA)
      else            data.frame(p = anova(m)$`Pr(>F)`[1])
    })

  write.table(art_pval, "art_anova_raw.txt", quote = FALSE, row.names = FALSE, sep = "\t")
  flag_art <- any(!is.na(art_pval$p) & art_pval$p < 0.05)

  if (flag_art) {
    emmeans_cld <- filtered_data %>%
      mutate(line = factor(line, levels = target_order)) %>%
      group_by(time) %>%
      group_modify(~ {
        m  <- art(Fv_Fm ~ line + (1 | rep), data = .x)
        ep <- emmeans(artlm(m, "line"), pairwise ~ line)
        write.table(as.data.frame(ep$contrasts),
                    file  = paste0("emmeans_posthoc_raw_", unique(.x$time), ".txt"),
                    quote = FALSE, row.names = FALSE, sep = "\t")
        as.data.frame(cld(ep$emmeans, Letters = letters))
      })

    df2 <- merge(conf_int, emmeans_cld, by = c("time", "line"))

    p_main <- df2 %>%
      mutate(line = factor(line, levels = target_order)) %>%
      ggplot(aes(x = line, y = Median, group = time)) +
      geom_col(color = "dark grey", fill = "white") +
      geom_text(aes(x = line, y = Percentile.upper + 0.15, label = .group), size = 3) +
      scale_y_continuous(breaks  = seq(0, 1, 0.2),
                         limits  = c(0, max(df2$Percentile.upper, na.rm = TRUE) + 0.2),
                         expand  = c(0, 0)) +
      geom_quasirandom(data = df_data_long,
                       aes(x = line, y = Fv_Fm, color = as.factor(rep), shape = as.factor(rep)),
                       width = 0.3, alpha = 0.6) +
      scale_color_manual(values = rep_colors) +
      geom_segment(aes(x = line, xend = line, y = Percentile.lower, yend = Percentile.upper), color = "black") +
      labs(y = "Fv/Fm") +
      theme_fvfm() +
      facet_wrap(~time, nrow = 1, scales = "free_x")

  } else {
    print("Raw Fv/Fm: no significant differences — post hoc skipped.")
  }
}

# ---- PART 2: Normalized Fv/Fm — T48 only -------------------------------------

df_t48 <- df_data_long %>%
  filter(time == "T48") %>%
  mutate(line = factor(line, levels = target_order))

shapiro_norm <- df_t48 %>%
  group_by(line) %>%
  summarise(statistic = shapiro.test(normalized_value)$statistic,
            p.value   = shapiro.test(normalized_value)$p.value, .groups = "drop")

flag_normal_norm <- check_normality(shapiro_norm)

if (flag_normal_norm) {

  print("Normalized T48: normally distributed — parametric tests")

  if (!require(Rmisc))        install.packages("Rmisc")
  if (!require(multcompView)) install.packages("multcompView")
  library(plyr); library(Rmisc)
  my_summary_norm <- summarySE(df_t48, measurevar = "normalized_value", groupvars = "line")
  if ("package:Rmisc" %in% search()) detach("package:Rmisc", unload = TRUE)
  if ("package:plyr"  %in% search()) detach("package:plyr",  unload = TRUE)

  if (!require(lme4))   install.packages("lme4")
  if (!require(emmeans)) install.packages("emmeans")
  library(lme4); library(emmeans); library(dplyr); library(multcompView)

  model_norm  <- lmer(normalized_value ~ line + (1 | rep), data = df_t48)
  anova_norm  <- anova(model_norm)
  write.table(as.data.frame(anova_norm), "anova_normalized_T48.txt",
              quote = FALSE, row.names = TRUE, sep = "\t")

  flag_anova_norm <- anova_norm$`Pr(>F)`[1] < 0.05

  if (flag_anova_norm) {
    ep_norm     <- emmeans(model_norm, pairwise ~ line)
    cld_norm_df <- as.data.frame(cld(ep_norm$emmeans, Letters = letters))
    write.table(as.data.frame(ep_norm$contrasts), "emmeans_posthoc_normalized_T48.txt",
                quote = FALSE, row.names = FALSE, sep = "\t")

    df2_norm <- merge(my_summary_norm, cld_norm_df, by = "line")

    p_norm <- df2_norm %>%
      mutate(line = factor(line, levels = target_order), panel_label = "T48 (norm.)") %>%
      ggplot(aes(x = line, y = normalized_value)) +
      geom_col(color = "dark grey", fill = "white") +
      scale_x_discrete(limits = target_order) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)), breaks = seq(0, 2, 0.2)) +
      geom_quasirandom(data = mutate(df_t48, panel_label = "T48 (norm.)"),
                       aes(x = line, y = normalized_value,
                           color = as.factor(rep), shape = as.factor(rep)),
                       width = 0.3, alpha = 0.6) +
      scale_color_manual(values = rep_colors) +
      geom_segment(aes(x = line, xend = line,
                       y = normalized_value - ci, yend = normalized_value + ci), color = "black") +
      geom_text(aes(x = line, y = normalized_value + ci + 0.05, label = .group), size = 3) +
      labs(y = "Normalized Fv/Fm") +
      theme_fvfm() +
      facet_wrap(~panel_label)

  } else {
    print("Normalized T48: no significant differences — post hoc skipped.")
    p_norm <- df_t48 %>%
      mutate(panel_label = "T48 (norm.)") %>%
      ggplot(aes(x = line, y = normalized_value)) +
      geom_quasirandom(aes(color = as.factor(rep), shape = as.factor(rep)), width = 0.3, alpha = 0.6) +
      scale_color_manual(values = rep_colors) +
      scale_x_discrete(limits = target_order) +
      labs(y = "Normalized Fv/Fm") +
      theme_fvfm() +
      facet_wrap(~panel_label)
  }

} else {

  # Non-parametric path for normalized T48
  print("Normalized T48: not normally distributed — non-parametric tests")
  df_t48 <- na.omit(df_t48)

  conf_int_norm <- groupwiseMedian(data = df_t48,
                                    var       = "normalized_value",
                                    group     = "line",
                                    conf      = 0.95, R = 5000,
                                    percentile = TRUE, bca = TRUE, digits = 3)

  filtered_t48 <- df_t48 %>% group_by(line) %>% filter(n() > 1) %>% ungroup()

  if (!require(ARTool))  install.packages("ARTool")
  if (!require(emmeans)) install.packages("emmeans")
  library(ARTool); library(emmeans); library(dplyr)

  art_model_norm <- tryCatch(
    art(normalized_value ~ line + (1 | rep), data = filtered_t48),
    error = function(e) { warning(paste("ART ANOVA failed for normalized T48:", e$message)); NULL }
  )

  flag_art_norm <- FALSE
  if (!is.null(art_model_norm)) {
    anova_art_norm <- anova(art_model_norm)
    p_val_norm     <- anova_art_norm$`Pr(>F)`[1]
    write.table(as.data.frame(anova_art_norm), "art_anova_normalized_T48.txt",
                quote = FALSE, row.names = FALSE, sep = "\t")
    flag_art_norm <- !is.na(p_val_norm) && p_val_norm < 0.05
  }

  if (flag_art_norm) {
    ep_norm     <- emmeans(artlm(art_model_norm, "line"), pairwise ~ line)
    cld_norm_df <- as.data.frame(cld(ep_norm$emmeans, Letters = letters))
    write.table(as.data.frame(ep_norm$contrasts), "emmeans_posthoc_normalized_T48.txt",
                quote = FALSE, row.names = FALSE, sep = "\t")

    df2_norm <- merge(conf_int_norm, cld_norm_df, by = "line")

    p_norm <- df2_norm %>%
      mutate(line = factor(line, levels = target_order), panel_label = "T48 (norm.)") %>%
      ggplot(aes(x = line, y = Median)) +
      geom_col(color = "dark grey", fill = "white") +
      scale_x_discrete(limits = target_order) +
      scale_y_continuous(breaks = seq(0, 2, 0.2),
                         limits = c(0, max(df2_norm$Percentile.upper, na.rm = TRUE) + 0.15),
                         expand = c(0, 0)) +
      geom_quasirandom(data = mutate(df_t48, panel_label = "T48 (norm.)"),
                       aes(x = line, y = normalized_value,
                           color = as.factor(rep), shape = as.factor(rep)),
                       width = 0.3, alpha = 0.6) +
      scale_color_manual(values = rep_colors) +
      geom_segment(aes(x = line, xend = line, y = Percentile.lower, yend = Percentile.upper), color = "black") +
      geom_text(aes(x = line, y = Percentile.upper + 0.05, label = .group), size = 3) +
      labs(y = "Normalized Fv/Fm") +
      theme_fvfm() +
      facet_wrap(~panel_label)

  } else {
    print("Normalized T48: no significant differences — post hoc skipped.")
    p_norm <- df_t48 %>%
      mutate(panel_label = "T48 (norm.)") %>%
      ggplot(aes(x = line, y = normalized_value)) +
      geom_quasirandom(aes(color = as.factor(rep), shape = as.factor(rep)), width = 0.3, alpha = 0.6) +
      scale_color_manual(values = rep_colors) +
      scale_x_discrete(limits = target_order) +
      labs(y = "Normalized Fv/Fm") +
      theme_fvfm() +
      facet_wrap(~panel_label)
  }
}

# ---- PART 3: Combine plots and save ------------------------------------------

if (!is.null(p_main) && !is.null(p_norm)) {
  combined_plot <- p_main + p_norm +
    plot_layout(widths = c(3, 1), guides = "collect")
  print(combined_plot)
  library(svglite)
  ggsave("fvfm_4panel_normalized.svg", combined_plot, width = 11, height = 3)
  print("Saved: fvfm_4panel_normalized.svg")
} else {
  if (!is.null(p_main)) {
    print(p_main); library(svglite)
    ggsave("fvfm_raw_3panel.svg", p_main, width = 8, height = 3)
    warning("p_norm could not be created — only raw 3-panel saved.")
  }
  if (!is.null(p_norm)) {
    print(p_norm); library(svglite)
    ggsave("fvfm_normalized_T48.svg", p_norm, width = 3, height = 3)
    warning("p_main could not be created — only normalized T48 panel saved.")
  }
}
