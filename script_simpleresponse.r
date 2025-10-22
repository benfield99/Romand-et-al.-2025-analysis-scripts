# Script for HOTE data analysis with statistical analysis (adapted 10/01/23)
#==============================================================================

# Define Variables
## Reference line : the line against which the statistical tests will be performed
RefLine <- "01_WT"  # a string between ""

## Order in which the lines will appear on the plot
target_order <- c("01_WT", "02_H1", "03_G8")  # your lines names

# File to analyse
DATA <- "example_script_simpleresponse.xlsx"

## Order for the measuring treatment point in the graph
treatment_order <- c("HL")

#==============================================================================
# Installation of packages
if (!require(dplyr)) { install.packages("dplyr") }
if (!require(tidyverse)) { install.packages("tidyverse") }
if (!require(devtools)) { install.packages("devtools") }
if (!require(rstatix)) { install.packages("rstatix", repos = "https://cloud.r-project.org") }
if (!require(ggbeeswarm)) { install.packages("ggbeeswarm") }
if (!require(RColorBrewer)) { install.packages("RColorBrewer") }
if (!require(rcompanion)) { install.packages("rcompanion") }
if (!require(readxl)) { install.packages("readxl") }

################################################################################################################################

# Function used in the script

#=======================================================================================================================================
# Check data normality. Get out of the loop if at least one of the group of data does not follow a normal law.
# Return TRUE if data follow a normal law
#=======================================================================================================================================
check_normality <- function(shapiro_df) {
  # The normality is assumed to be true
  flag_normal <- TRUE
  
  for (i in 1 : nrow(shapiro_df)) {
    if(shapiro_df[i, 4] > 0.05) {
      # Data follows normal distribution
    } else {
      # If data don't follow a normal law, don't go further
      flag_normal <- FALSE
      break
    }
  }
  return(flag_normal)
}

#=======================================================================================================================================
# Take the results of Anova test
# Return TRUE if at least one mean is significantly different 
#=======================================================================================================================================
check_anova <- function(anova_results) {
  flag_anova <- FALSE
  
  for (i in 1 : nrow(anova_results)) {
    if (anova_results$p[i] < 0.05) {
      print (paste0("Anova test compares the mean, pROS for the group ", anova_results$treatment[i] , " is < 0.05 this means that at least one of the medians is significantly different from the others, a post hoc Tukey test is performed"))
      flag_anova <- TRUE
    } else {
      print (paste0("Anova test compares the mean, pROS for the group ", anova_results$treatment[i] , " is > 0.05 this means that there is no significant difference between means"))
    }
  }    
  
  return(flag_anova)
}

#=======================================================================================================================================
# Perform Kruskal-Wallis test
# Return TRUE if one of the median is significantly different
#=======================================================================================================================================
check_kruskal <- function(kruskal_pval) {
  flag_kruskal <- FALSE
  
  for (i in 1 : nrow(kruskal_pval)) {
    if (kruskal_pval$p[i] < 0.05) {
      print (paste0("Kruskall Wallis test compares medians, pROS for the group ", kruskal_pval$treatment[i] , " is < 0.05 this means that at least one of the medians is significantly different from the others, a post hoc Dunn test is performed"))
      flag_kruskal <- TRUE
    } else {
      print (paste0("Kruskall Wallis test compares medians, pROS for the group ", kruskal_pval$treatment[i] , " is > 0.05 this means that there is no significant difference between medians"))
    }
  }    
  
  return(flag_kruskal)
}

#=======================================================================================================================================
# Perform Dunn test
# Return the pROS in a dataframe
#=======================================================================================================================================
test_dunn <- function() {
  pval <- as.data.frame(df_data %>%
                          mutate(line = fct_relevel(line, target_order)) %>% 
                          group_by(treatment) %>% dunn_test(ROS ~ line, p.adjust.method = "BH"))
  return(pval)
}

#==============================================================================

# Load the data
library(readxl)
df_data <- read_excel(DATA)

#___________________________________________________________________________________________________________________________________
# Statistical analysis
#___________________________________________________________________________________________________________________________________

library(ggbeeswarm) # for function geom_quasirandom
library(RColorBrewer) # to define colors
library(rstatix)  # for statistical test
library(rcompanion) # to compute confidence interval for non parametric data
library(forcats)

# Convert line's name into factor to force the order in the plot 
df_data$line <- as.factor(df_data$line)

# Determining data normality status
shapiro_df <- df_data %>%
  dplyr::group_by(treatment, line) %>%
  summarise(statistic = shapiro.test(ROS)$statistic, 
            p.ROS = shapiro.test(ROS)$p.value)

flag_normal <- check_normality(shapiro_df)

# Data treatment according to normality status
if(flag_normal == TRUE) {
  print("Datas follow a normal law")
  
  # Summary
  if (!require(Rmisc)) {install.packages("Rmisc")}
  library(plyr) # rmisc dependency
  library(Rmisc) # for the command summarySE
  my_summary <- summarySE(df_data, measurevar="ROS", groupvars=c("treatment", "line"))
  
  # Detach packages
  detach(package:Rmisc)
  detach(package:plyr)
  
  # Stats
  anova_results <- df_data %>% group_by(treatment) %>% anova_test(ROS ~ line)
  flag_anova <- check_anova(anova_results)
  if (flag_anova == TRUE) {
    tukey_results <- df_data %>% group_by(treatment) %>% tukey_hsd(ROS ~ line)
    
    # Generate CLD letters for parametric test by treatment point
    cld_table_parametric <- tukey_results %>%
      group_by(treatment) %>%
      summarise(cld = list(multcompView::multcompLetters(setNames(p.adj, paste(group1, group2, sep = "-")), Letters = letters)$Letters)) %>%
      unnest_longer(cld) %>%
      mutate(line = names(cld))
    
    # Save CLD table for parametric test to a text file
    write.table(cld_table_parametric, file = "cld_table_parametric.txt", quote = FALSE, row.names = FALSE, sep = '\t')
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
  
  # Prepare data to add CLD letters on error bar
  if(flag_anova == TRUE) {
    df2 <- merge(my_summary, cld_table_parametric, by.x = c("treatment", "line"), by.y = c("treatment", "line"))
    
    # Plot
    p <- df2 %>% mutate(line = fct_relevel(line, target_order)) %>%  
      ggplot(aes(x=line, y=ROS, group = treatment))+
      geom_col(aes(x=line, y=ROS), color="dark grey", fill="white", width=0.6, position=position_dodge2(padding=0.05))+
      scale_x_discrete(limits=target_order, expand = expansion(mult = c(0.2, 0.1)))+
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = seq(0, max(df2$ROS + df2$ci, na.rm = TRUE) + 5, 5))+
      geom_quasirandom(data = df_data, aes(x = line, y = ROS, color = treatment), 
                       color="green", width = 0.3, alpha = 0.6)+
      geom_segment(aes(x = as.factor(line), xend = as.factor(line), y = pmax(0, ROS - ci), yend = ROS + ci), color = "black") +
      geom_text(aes(x = as.factor(line), y= ROS + ci + 0.5, 
                    label = cld),
                size = 3, inherit.aes = TRUE)+
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            panel.grid.minor.y = element_blank(),
            axis.line.x = element_line(size = 0.5),
            axis.line.y = element_line(size = 0.5),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = 'transparent', color = NA),
            plot.background = element_rect(fill = 'transparent', color = NA),
            axis.text.y = element_text(vjust = 1))+
      facet_wrap(~treatment, nrow=1)
    
    print(p)
    
    ggsave("ros_plot.svg", width=4, height=5)
  } else {
    print("Datas are not significantly different, the Tukey test was not performed.")
  }
  
} else {
  print("Datas don't follow a normal law")
  
  # Summary
  conf_int <- groupwiseMedian(data = df_data,
                              var = "ROS",
                              group = c("treatment", "line"),
                              conf       = 0.95,
                              R          = 5000,
                              percentile = TRUE,
                              bca        = FALSE,
                              digits     = 3)
  
  # Stats
  kruskal_pval <- (df_data %>% group_by(treatment)%>%kruskal_test(ROS ~ line)) %>% dplyr::select(treatment, p)
  
  flag_kruskal <- check_kruskal(kruskal_pval)
  if (flag_kruskal == TRUE) { 
    pval_dunn <- test_dunn()
    
    # Generate CLD letters for non-parametric test by treatment point
    cld_table_nonparametric <- pval_dunn %>%
      group_by(treatment) %>%
      summarise(cld = list(multcompView::multcompLetters(setNames(p.adj, paste(group1, group2, sep = "-")), Letters = letters)$Letters)) %>%
      unnest_longer(cld) %>%
      mutate(line = names(cld))
    
    # Save CLD table for non-parametric test to a text file
    write.table(cld_table_nonparametric, file = "cld_table_nonparametric.txt", quote = FALSE, row.names = FALSE, sep = '\t')
  }
  
  # Save the files
  write.table(conf_int, file = "Summary.txt", 
              quote = FALSE, row.names = FALSE, sep = '\t')
  write.table(kruskal_pval, file = "Kruskal.txt", 
              quote = FALSE, row.names = FALSE, sep = '\t')
  
  if (flag_kruskal == TRUE) {
    write.table(pval_dunn[, c(1, 3, 4, 8, 9, 10)], file = "Dunn.txt", 
                quote = FALSE, row.names = FALSE, sep = '\t')
  } 
  
  # Prepare data to add CLD letters on error bar
  if(flag_kruskal == TRUE) {
    df2 <- merge(conf_int, cld_table_nonparametric, by.x = c("treatment", "line"), by.y = c("treatment", "line"))
    
    # Plot
    p <- df2 %>% mutate(line = fct_relevel(line, target_order)) %>%  
      ggplot(aes(x=line, y=Median, group = treatment))+
      geom_col(aes(x=line, y=Median), color="dark grey", fill="white", width=0.6, position=position_dodge2(padding=0.1))+
      scale_x_discrete(limits=target_order, expand = expansion(mult = c(0.1, 0.1)))+
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = seq(0, max(df2$Median + df2$Percentile.upper, na.rm = TRUE) + 5, 5))+
      geom_quasirandom(data = df_data, aes(x = line, y = ROS, color = treatment), 
                       color="green", width = 0.3, alpha = 0.6)+
      geom_segment(aes(x = as.factor(line), xend = as.factor(line), y = pmax(0, Percentile.lower), yend = Percentile.upper), color = "black") +
      geom_text(aes(x = as.factor(line), y= Percentile.upper + 0.5, 
                    label = cld),
                size = 3, inherit.aes = TRUE)+
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            panel.grid.minor.y = element_blank(),
            axis.line.x = element_line(size = 0.5),
            axis.line.y = element_line(size = 0.5),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = 'transparent', color = NA),
            plot.background = element_rect(fill = 'transparent', color = NA),
            axis.text.y = element_text(vjust = 1))+
      facet_wrap(~treatment, nrow=1, scales = "free_x")
    
    print(p)
    
    ggsave("ros_plot.svg", width=4, height=5)
  } else {
    print("Datas are not significantly different, the Dunn test was not performed.")
  }
}
