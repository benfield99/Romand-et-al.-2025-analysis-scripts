# ==============================================================================
# Figure 4B - ELIP2 relative protein abundance (WT, OX:RSH3.1)
# Family:  ANOVA-bar, version 2 (adapted 10/01/23 from the group's Fv/Fm analysis template).
#          Version 3 is used for Fig 1D; later versions generalise it (Figs 4C, 4E, S1, S3, S9).
# Input:   Book1.xlsx (column ratio = ELIP2/Ponceau, normalised to the WT mean)
# Output:  fvfm_plot2.svg, Shapiro_residuals.txt, Anova.txt, Tukey.txt, Summary.txt
# Method:  Shapiro-Wilk test on the pooled ANOVA residuals. Normal: one-way ANOVA, Tukey HSD,
#          CLD letters, mean +/- 95% CI. Otherwise: Kruskal-Wallis, Dunn test (BH), CLD letters,
#          median +/- bootstrap 95% CI.
# Run:     with the working directory set to this folder.
# ==============================================================================

library(readxl)

# ---- User settings -----------------------------------------------------------
# Reference line: the line against which the statistical tests are performed
RefLine <- "Col"  #a string between ""

# ## Order in wich the lines will appear on the plot
target_order <- c("Col", "H1")   # your lines names


# File to analyse
# Import from Excel
DATA <- read_excel("Book1.xlsx")

# ---- Packages ----------------------------------------------------------------
if (!require(tidyr)) { install.packages("tidyr") }
if (!require(dplyr)) { install.packages("dplyr") }
if (!require(tidyverse)) { install.packages("tidyverse") }
if (!require(devtools)) { install.packages("devtools") }
if (!require(rstatix)) { install.packages("rstatix", repos = "https://cloud.r-project.org") }
if (!require(ggbeeswarm)) { install.packages("ggbeeswarm") }
if (!require(RColorBrewer)) { install.packages("RColorBrewer") }
if (!require(rcompanion)) { install.packages("rcompanion") }
if (!require(readxl)) { install.packages("readxl") }


# ---- Functions ---------------------------------------------------------------


# Take the results of Anova test
# Return TRUE if at least one mean is significantly different
check_anova <- function(anova_results) {
  flag_anova <- FALSE

  for (i in 1 : nrow(anova_results)) {
    if (anova_results$p[i] < 0.05) {
      print (paste0("Anova test compares the mean, pratio for the group ", anova_results$treatment[i] , " is < 0.05 this means that at least one of the medians is significantly different from the others, a post hoc Tukey test is performed"))
      flag_anova <- TRUE
    } else {
      print (paste0("Anova test compares the mean, pratio for the group", anova_results$treatment[i] , " is > 0.05 this means that there is no significant difference between means"))
    }
  }    

  return(flag_anova)
}

# Perform Kruskal-Wallis test
# Return TRUE if one of the median is significantly different
check_kruskal <- function(kruskal_pval) {
  flag_kruskal <- FALSE

  for (i in 1 : nrow(kruskal_pval)) {
    if (kruskal_pval$p[i] < 0.05) {
      print (paste0("Kruskall Wallis test compares medians, pratio for the group ", kruskal_pval$treatment[i] , " is < 0.05 this means that at least one of the medians is significantly different from the others, a post hoc Dunn test is performed"))
      flag_kruskal <- TRUE
    } else {
      print (paste0("Kruskall Wallis test compares medians, pratio for the group ", kruskal_pval$treatment[i] , " is > 0.05 this means that there is no significant difference between medians"))
    }
  }    

  return(flag_kruskal)
}

# Perform Dunn test
# Return the p-values in a data frame
test_dunn <- function() {
  pval <- as.data.frame(df_data_long%>%
                          mutate(line = fct_relevel(line, target_order)) %>% 
                          group_by(treatment) %>% dunn_test(ratio ~ line, p.adjust.method = "BH"))
  return(pval)
}


# ---- Load data ---------------------------------------------------------------
df_data <- DATA

df_data_long <- df_data

# ---- Statistical analysis ----------------------------------------------------

library(ggbeeswarm) # for funcnction geom_quasirandom
library(RColorBrewer) # to define colors
library(rstatix)  # for statistical test
library(rcompanion) # to compute confidence interval for non parametric data
library(forcats)


# Convert line's name into factor to force the order in the plot
df_data_long$line <- as.factor(df_data_long$line)


# Determining data normality status on the pooled ANOVA residuals (the actual ANOVA assumption)
model_formula <- if (length(unique(df_data_long$treatment)) > 1) ratio ~ line * treatment else ratio ~ line
model <- lm(model_formula, data = df_data_long)
shapiro_res <- shapiro.test(residuals(model))
print(shapiro_res)
flag_normal <- shapiro_res$p.value > 0.05
write.table(data.frame(W = shapiro_res$statistic, p = shapiro_res$p.value), file = "Shapiro_residuals.txt",
            quote = FALSE, row.names = FALSE, sep = '\t')

# Data treatment according to normality status
if(flag_normal == TRUE) {
  print("Datas follow a normal law")

  # Summary
  if (!require(Rmisc)) {install.packages("Rmisc")}
  library(plyr) # rmisc dependance
  library(Rmisc) # for the command summarySE
  my_summary <- summarySE(df_data_long, measurevar="ratio", groupvars=c("treatment", "line"))

  # Detach these two packages: they can stop group_by from working properly
  detach(package:Rmisc)
  detach(package:plyr)


  # Stats
  anova_results <- df_data_long %>% group_by(treatment) %>% anova_test(ratio ~ line)
  flag_anova <- check_anova(anova_results)
  if (flag_anova == TRUE) {
    tukey_results <- as.data.frame(df_data_long %>% mutate(line = fct_relevel(line, target_order)) %>%
                                     group_by(treatment) %>%  tukey_hsd(ratio ~ line))
  }

  # Save the files
  write.table(my_summary, file = "Summary.txt", 
              quote = FALSE, row.names = FALSE, sep = '\t')
  write.table(anova_results, file = "Anova.txt", 
              quote = FALSE, row.names = FALSE, sep = '\t')
  if (flag_anova == TRUE) {
    write.table(tukey_results[, c(1,3,4, 9, 10)], file = "Tukey.txt", 
                quote = FALSE, row.names = FALSE, sep = '\t')
  } 

  # Prepare data to add stars on error bar
  if(flag_anova == TRUE) {
    df <- dplyr::filter(tukey_results, grepl(RefLine, group1))

    for (i in levels(as.factor(df$treatment))) {
      x <- c(i, "line", RefLine, RefLine, rep(x="", 6))
      df <- rbind(x, df)
    }

    df <- df[ with(df, order(treatment, group2)),]

    df2 <- cbind(my_summary, df$p.adj.signif)
    names(df2)[names(df2) == "df$p.adj.signif"] <- "Labels"

    # Plot
    p<-df2 %>% mutate(line = fct_relevel(line, target_order)) %>%  
      ggplot(aes(x=treatment, y=ratio, group = line))+
      geom_col(aes(x=treatment, y=ratio), color="dark grey", fill="white")+
      geom_quasirandom(data = df_data_long, aes(x = treatment, y = ratio, color = line), 
                       color="green", width = 0.3, alpha = 0.6)+
      geom_errorbar(aes(x = as.factor(treatment), ymin=ratio-ci, ymax=ratio+ci), 
                    width=.2, position=position_dodge(0.9), color = "black") +
      geom_text(aes(x = as.factor(treatment), y= ratio+ci + 0.018 , 
                    label = Labels),
                size = 3, position = position_dodge(0.9), inherit.aes = TRUE)+
      facet_wrap(~line, nrow=1)+
      theme_classic()

    print(p)

    ggsave("fvfm_plot2.svg", width=2, height=3)

  }else {
    print("Datas are not significantly different, The Tukey test was not performed.")
  }


} else {
  print("Datas don't follow a normal law")

  # Summary
  conf_int <- groupwiseMedian(data = df_data_long,
                              var = "ratio",
                              group = c("treatment", "line"),
                              conf       = 0.95,
                              R          = 5000,
                              percentile = TRUE,
                              bca        = FALSE,
                              digits     = 3)


  # Stats
  kruskal_pval <- (df_data_long %>% group_by(treatment)%>%kruskal_test(ratio ~ line)) %>% select(treatment, p)

  flag_kruskal <- check_kruskal(kruskal_pval)
  if (flag_kruskal == TRUE) { pval_dunn <- test_dunn() }

  # Save the files
  write.table(conf_int, file = "Summary.txt", 
              quote = FALSE, row.names = FALSE, sep = '\t')
  write.table(kruskal_pval, file = "Kruskal.txt", 
              quote = FALSE, row.names = FALSE, sep = '\t')

  if (flag_kruskal == TRUE) {
    write.table(pval_dunn[, c(1, 3, 4, 8, 9, 10)], file = "Dunn.txt", 
                quote = FALSE, row.names = FALSE, sep = '\t')
  } 

  # Prepare data to add stars on error bar
  if(flag_kruskal == TRUE) {
    df <- dplyr::filter(pval_dunn, grepl(RefLine, group1))

    for (i in levels(as.factor(df$treatment))) {
      x <- c(i, "line", RefLine, RefLine, rep(x="", 6))
      df <- rbind(x, df)
    }

    df <- df[ with(df, order(treatment, group2)),]

    df2 <- cbind(conf_int, df$p.adj.signif)
    names(df2)[names(df2) == "df$p.adj.signif"] <- "Labels"

    # Plot
    p<-df2 %>% mutate(line = fct_relevel(line, target_order)) %>%  
      ggplot(aes(x=treatment, y=Median, group = line))+
      geom_col(aes(x=treatment, y=Median), color="dark grey", fill="white")+
      geom_quasirandom(data = df_data_long, aes(x = treatment, y = ratio, color = line), 
                       color="green", width = 0.3, alpha = 0.6)+
      geom_errorbar(aes(x = as.factor(treatment), ymin=Percentile.lower, ymax=Percentile.upper), 
                    width=.2, position=position_dodge(0.9), color = "black") +
      geom_text(aes(x = as.factor(treatment), y= Percentile.upper + 0.018 , 
                    label = Labels),
                size = 3, position = position_dodge(0.9), inherit.aes = TRUE)+
      facet_wrap(~line, nrow=1)+
      theme_classic()

    print(p)

    ggsave("fvfm_plot2.svg", width=2, height=3)

  }else {
    print("Datas are not significantly different, The Dunn test was not performed.")
  }
}
