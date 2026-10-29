# Script to analyze NPQ data with confidence interval calculation
#Author: Cécile Lecampion
#Date: June 2024
#==============================================================================

# Variable definitions

# Desired comparisons
COMP1 <- c("WT", "NPQ1")
COMP2 <- c("NPQ1", "H1_NPQ1", "H1")
COMP3 <- c("NPQ4", "H1_NPQ4", "H1")
COMP4 <- c("WT", "NPQ4")

COMP <- list(COMP1, COMP2, COMP3, COMP4)


#==============================================================================
# Package installation
if (!require(tidyr)) { install.packages("tidyr") }
if (!require(dplyr)) { install.packages("dplyr") }
if (!require(tidyverse)) { install.packages("tidyverse") }
if (!require(devtools)) { install.packages("devtools") }
if (!require(rstatix)) { install.packages("rstatix", repos = "https://cloud.r-project.org") }
if (!require(ggbeeswarm)) { install.packages("ggbeeswarm") }
if (!require(RColorBrewer)) { install.packages("RColorBrewer") }
if (!require(rcompanion)) { install.packages("rcompanion") }

################################################################################################################################

# Functions used in the script

#=======================================================================================================================================
# Checks data normality. Exits the loop if at least one data group does not follow 
# a normal distribution.
# Returns TRUE if the data follows a normal distribution
#=======================================================================================================================================
check_normality <- function(shapiro_df) {
  # Assume data is normal
  flag_normal <- TRUE
  
  for (i in 1 : nrow(shapiro_df)) {
    if(shapiro_df[i, 4] > 0.05) {
      # print(paste0("data ",shapiro_df$grouping_factor[i],"-", 
      # shapiro_df$plant_line[i], " follows a normal distribution"), quote = FALSE)
      
    } else {
      # print(paste0("data ",shapiro_df$grouping_factor[i],"-", 
      #          shapiro_df$plant_line[i], " does not follow a normal distribution"), quote = FALSE)
      
      # Actually the data is not normal, no need to go further
      flag_normal <- FALSE
      break
    }
  }
  return(flag_normal)
}

#=======================================================================================================================================
# Performs NPQ data analysis
# returns plots and statistical tables
#=======================================================================================================================================
f_analyseNPQ <- function(df){
  # Determining data normality status
  
  shapiro_df <- df[5:nrow(df),] %>%
    dplyr::group_by(line, time) %>%
    summarise(statistic = shapiro.test(value)$statistic,
              p.value = shapiro.test(value)$p.value)
  
  flag_normal <- check_normality(shapiro_df)
  flag_normal <- TRUE
  
  
  # Data treatment according to normality status
  if(flag_normal == TRUE) {
    print("Data follows a normal distribution")
    
    # Summary
    if (!require(Rmisc)) {install.packages("Rmisc")}
    library(plyr) # dependency of rmisc
    library(Rmisc) # for summarySE command
    my_summary <- summarySE(df, measurevar="value", groupvars=c("time", "line"))
    
    detach(package:Rmisc)
    detach(package:plyr)
    
    # Plot
    
    for (comp in COMP) {
      p<-dplyr::filter(my_summary, line %in% comp)%>%
        ggplot(aes(x=time, y=value, group = line))+
        geom_ribbon(aes(ymin=value-(ci*0.5), ymax=value+(ci*0.5), fill=line),alpha=0.3, linetype=0)+
        scale_fill_manual(values = my_colors)+
        geom_point(aes(x=time, y=value, color=line))+
        geom_line(aes(x=time, y=value, color=line), linewidth=1)+
        scale_color_manual(values = my_colors) +
        scale_y_continuous(limits = c(0.25,0.9), breaks=c(0.25, 0.5, 0.75, 0.9))+
        theme_classic()+
        xlab("Time")+
        theme(plot.title=element_text(hjust = 1))
      print(p)
    }
    
    
    
    
    
  } else {
    print("Data does not follow a normal distribution")
    
    # Summary
    conf_int <- groupwiseMedian(data = df,
                                var = "value",
                                group = c("time", "line"),
                                conf       = 0.95,
                                R          = 5000,
                                percentile = TRUE,
                                bca        = FALSE,
                                digits     = 3)
    
    
    # Plot
    
    for (comp in COMP) {
      
      p<-dplyr::filter(conf_int, line %in% comp) %>% 
        ggplot(aes(x=time, y=Median, group = line, color = line))+
        geom_ribbon(aes(ymin=(Percentile.lower*0.5), ymax=(Percentile.upper*0.5), fill=line),alpha=0.3, linetype=0)+
        scale_fill_manual(values = my_colors)+
        geom_point(aes(x=time, y=Median, color=line))+
        geom_line(aes(x=time, y=Median, color=line), linewidth=1)+
        scale_color_manual(values = my_colors) +
        theme_classic()+
        xlab("Time")+
        theme(plot.title=element_text(hjust = 1))
      print(p) 
    }
  }
  #return(plot = p)  
}


#==============================================================================
# Set working directory
setwd("~/OneDrive - Aix-Marseille Université/bio-informatique/fluorcam/Hl_Ben_Shanna")

# Load data
df_data <- read.table("example_script_QY.txt", header = FALSE, sep = "\t")
colnames(df_data) <- c("line", "variable", "time", "value")

my_factors <- as.factor(df_data$line)

my_colors <- c("#FFCC33", "#FF99FF", "#CCCCFF",  "#FFCCFF", "#9966FF", "#FF9933")
names(my_colors) <- levels(my_factors) 

# Perform analysis
f_analyseNPQ(df_data)

