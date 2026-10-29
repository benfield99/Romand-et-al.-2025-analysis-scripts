# ==============================================================================
# Figures 1E, 2A and S5 - NPQ kinetics (FluorCam) for all lines at T0, T24h and T48h
# Family:  standalone.
# Author:  Cecile Lecampion
# Input:   raw_data_npq2.txt (time, line, measurement code, replicate values),
#          temps_mesure_NPQ.txt (measurement code -> seconds)
# Output:  NPQ_plots.pdf (one page per time point), Shapiro_<time>.txt,
#          Summary_Normal_<time>.txt or Summary_NonNormal_<time>.txt
# Method:  Shapiro-Wilk test per line x time. Normal: mean +/- 95% CI (Rmisc::summarySE).
#          Otherwise: median +/- bootstrap 95% CI (rcompanion::groupwiseMedian, R = 5000).
# Run:     with the working directory set to this folder.
# ==============================================================================

# File to process
DATA <- "raw_data_npq2.txt"

# File for mapping code / measurement time
CODE <- "temps_mesure_NPQ.txt"

# ---- Packages ----------------------------------------------------------------
if (!require(tidyr)) { install.packages("tidyr") }
if (!require(dplyr)) { install.packages("dplyr") }
if (!require(rcompanion)) { install.packages("rcompanion") }
if (!require(ggplot2)) { install.packages("ggplot2") }

library(dplyr)
library(rcompanion)
library(ggplot2)


# ---- Functions ---------------------------------------------------------------

# Check data normality.
# Stops the loop if at least one group does not follow a normal distribution.
# Returns TRUE if all groups follow a normal distribution.
check_normality <- function(shapiro_df) {
  # Assume data is normal
  flag_normal <- TRUE

  for (i in 1 : nrow(shapiro_df)) {
    if(shapiro_df[i, 4] > 0.05) {

    } else {

      # Data is not normal, no need to go further
      flag_normal <- FALSE
      break
    }
  }
  return(flag_normal)
}

# NPQ data analysis
# Returns plots and statistical tables
f_analyseNPQ <- function(df, time_id){
  # Determining data normality status

  shapiro_df <- df %>%
    dplyr::group_by(line, secondes) %>%
    summarise(statistic = shapiro.test(value)$statistic,
              p.value = shapiro.test(value)$p.value,
              .groups="drop")

  flag_normal <- check_normality(shapiro_df)

  # Export Shapiro results
  write.table(shapiro_df, file=paste0("Shapiro_", time_id, ".txt"), 
              sep="\t", row.names=FALSE, quote=FALSE)

  # Data treatment according to normality status
  if(flag_normal == TRUE) {
    print("Data follow a normal distribution")

    # Summary
    if (!require(Rmisc)) {install.packages("Rmisc")}
    library(plyr) # dependency for Rmisc
    library(Rmisc) # for summarySE function
    my_summary <- summarySE(df, measurevar="value", groupvars=c("line", "secondes"))

    # Export summary results
    write.table(my_summary, file=paste0("Summary_Normal_", time_id, ".txt"), 
                sep="\t", row.names=FALSE, quote=FALSE)

    detach(package:Rmisc)
    detach(package:plyr)

    # Plot
    p<-my_summary %>%
      ggplot(aes(x=secondes, y=value, group = line))+
      geom_ribbon(aes(ymin=value-ci, ymax=value+ci, fill=line),alpha=0.3, linetype=0)+
      scale_fill_manual(values=my_colours)+
      geom_point(aes(x=secondes, y=value, color=line))+
      geom_line(aes(x=secondes, y=value, color=line), size=1)+
      scale_colour_manual(values=my_colours) +
      facet_wrap(~line)+
      theme_classic()+
      theme(legend.position= "none")+
      ggtitle(levels(as.factor(df$time)))+
      theme(plot.title=element_text(hjust = 1))

  } else {
    print("Data do NOT follow a normal distribution")

    # Summary using medians + bootstrapped confidence intervals
    conf_int <- groupwiseMedian(data = df,
                                var = "value",
                                group = c("line", "secondes"),
                                conf       = 0.95,
                                R          = 5000,
                                percentile = TRUE,
                                bca        = FALSE,
                                digits     = 3)

    # Export summary results
    write.table(conf_int, file=paste0("Summary_NonNormal_", time_id, ".txt"), 
                sep="\t", row.names=FALSE, quote=FALSE)

    # Plot
    p<-conf_int %>%   
      ggplot(aes(x=secondes, y=Median, group = line))+
      geom_ribbon(aes(ymin=Percentile.lower, ymax=Percentile.upper, fill=line),alpha=0.3, linetype=0)+
      scale_fill_manual(values=my_colours)+
      geom_point(aes(x=secondes, y=Median, color=line))+
      geom_line(aes(x=secondes, y=Median, color=line), size=1)+
      scale_colour_manual(values=my_colours) +
      facet_wrap(~line)+
      theme_classic()+
      theme(legend.position= "none")+
      ggtitle(levels(as.factor(df$time)))+
      theme(plot.title=element_text(hjust = 1))

  }
  return(plot = p)
}

# Load data
df_data <- read.table(DATA, header = FALSE, sep = "\t")
colnames(df_data) <- c("time", "line", "secondes", "rep1", "rep2", "rep3", "rep4")

# Convert to long format
library(tidyr)
df_data_long <- df_data %>% pivot_longer(cols = starts_with("rep"), names_to = "replicat")

# Load measurement time code
code <- read.table(CODE, header = TRUE, sep = '\t')

# Replace values in column "secondes" using mapping code
df_data_long$secondes <- code$secondes[match(df_data_long$secondes, code$code)]

# Split into a list of dataframes, one per "time"
df_list <- split(df_data_long, f = df_data_long$time)

# Define colors
my_colours = rep("chartreuse3", each = length(levels(as.factor(df_data$line))))

# Run analysis for each subset
plots <- mapply(f_analyseNPQ, df_list, names(df_list), SIMPLIFY=FALSE)

# Save plots to PDF
pdf("NPQ_plots.pdf", width=10, height=8)
for(p in plots){ print(p) }
dev.off()
