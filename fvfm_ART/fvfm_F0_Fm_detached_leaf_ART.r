# ==============================================================================
# Figure S6 - Fv/Fm, F0 and Fm of detached leaves during 2 h excess light and 4 h recovery,
#             raw values and values relative to the mean at 0 h (one variable per run)
# Family:  Fv/Fm-ART, version 8: version 7 (Fig 3C) with a choice of response variable (RESPONSE),
#          results saved in a subfolder named after RESPONSE, CLD letters placed above the points,
#          and editor-safe SVG plus PDF output. Run once for each of the six RESPONSE values.
# Input:   combinedresults_normalized.txt (columns time, line, rep, and the RESPONSE column)
# Output:  <RESPONSE>/<RESPONSE>_plot.svg, .pdf and .rds (plot object used by combine_figure.R),
#          <RESPONSE>/art_anova_results.txt, emmeans_posthoc_results_*.txt
#          (summary, ANOVA and CLD tables when the parametric branch is used)
# Method:  normality is tested for each line x time group. If all groups are normal:
#          linear mixed model (response ~ line + (1|rep)) per time point, emmeans contrasts, CLD letters.
#          Otherwise: aligned-ranks ANOVA (ART) with the same model per time point, emmeans
#          contrasts on the aligned ranks, CLD letters.
# Run:     with the working directory set to this folder.
# ==============================================================================

# Define Variables
## Reference line: not used by this version (all pairwise comparisons between lines are made)
RefLine <- "h1"  # a string between ""

## Order in which the lines will appear on the plot
target_order <- c("WT", "h1", "npq1", "npq4", "h1npq1", "h1npq4")

## Order for the measuring time point in the graph
time_order <- c("T0", "T2h-HL", "T0.5-r", "T2h-r", "T4h-r")

# Working directory : Where the files are and results will be saved

# File to analyse
DATA <- "combinedresults_normalized.txt"

## Response variable to analyse : one column of DATA
##   Fv/Fm : "Fv_Fm" (absolute) or "normalized_value" (normalised to T0)
##   F0    : "Fo"    (absolute) or "Fo_norm"
##   Fm    : "Fm"    (absolute) or "Fm_norm"
RESPONSE <- "Fv_Fm"


#==============================================================================
# Installation des packages
if (!require(tidyr)) { install.packages("tidyr") }
if (!require(dplyr)) { install.packages("dplyr") }
if (!require(tidyverse)) { install.packages("tidyverse") }
if (!require(devtools)) { install.packages("devtools") }
if (!require(rstatix)) { install.packages("rstatix", repos = "https://cloud.r-project.org") }
if (!require(ggbeeswarm)) { install.packages("ggbeeswarm") }
if (!require(RColorBrewer)) { install.packages("RColorBrewer") }
if (!require(rcompanion)) { install.packages("rcompanion") }
if (!require(svglite)) {install.packages("svglite")}

################################################################################################################################

# Function used in the script

#=======================================================================================================================================
# Check data normality. Get out of the loop if at least one of the group of data does not follow a normal law.
# return TRUE if data follow a normal law
#=======================================================================================================================================
check_normality <- function(shapiro_df) {
  # The normality is assumed to be true
  flag_normal <- TRUE
  
  for (i in 1 : nrow(shapiro_df)) {
    if(shapiro_df[i, "p.value"] <= 0.05) {
      # Data do not follow normal distribution
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
  
  for (i in 1:nrow(anova_results)) {
    # Adjust for the correct column name Pr(>F)
    if (anova_results$`Pr(>F)`[i] < 0.05) {
      print(paste0("Anova test compares the mean, p-value for the group ", anova_results$time[i], " is < 0.05, indicating that at least one of the medians is significantly different from the others. A post hoc Tukey test is performed."))
      flag_anova <- TRUE
    } else {
      print(paste0("Anova test compares the mean, p-value for the group ", anova_results$time[i], " is > 0.05, indicating no significant difference between means."))
    }
  }    
  
  return(flag_anova)
}
#=======================================================================================================================================
# Check ART ANOVA results
# Return TRUE if one of the means is significantly different
#=======================================================================================================================================
check_art_anova <- function(art_pval) {
  flag_art <- FALSE
  
  for (i in 1:nrow(art_pval)) {
    if (art_pval$p[i] < 0.05) {
      print(paste0("ART ANOVA test compares means, p-value for the group ", art_pval$time[i], " is < 0.05, indicating that at least one of the means is significantly different from the others. A post hoc test is performed."))
      flag_art <- TRUE
    } else {
      print(paste0("ART ANOVA test compares means, p-value for the group ", art_pval$time[i], " is > 0.05, indicating no significant difference between means."))
    }
  }
  
  return(flag_art)
}

#=======================================================================================================================================
# SVG / PDF export
# svglite puts default fill/stroke in a CSS <style> block that Affinity and Inkscape ignore,
# so shapes without an explicit fill (e.g. transparent backgrounds) show up as black squares.
# Write fill/stroke on every shape.
#=======================================================================================================================================
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

# Saves <base>.svg (editor-safe), <base>.pdf and <base>.rds (the ggplot object, for combine_figure.R)
save_plot <- function(p, base, width, height) {
  saveRDS(p, paste0(base, ".rds"))
  ggplot2::ggsave(paste0(base, ".svg"), p, width = width, height = height,
                  device = svglite::svglite, bg = "transparent")
  make_svg_editor_safe(paste0(base, ".svg"))
  ggplot2::ggsave(paste0(base, ".pdf"), p, width = width, height = height,
                  device = grDevices::cairo_pdf, bg = "transparent")
}

#==============================================================================
# Define Working directory

# Load the data
df_data_long <- read.table(DATA, header = TRUE, sep = "\t", fill=TRUE)

#___________________________________________________________________________________________________________________________________
# Statistical analysis
#___________________________________________________________________________________________________________________________________

library(ggbeeswarm) # for function geom_quasirandom
library(RColorBrewer) # to define colors
library(rstatix)  # for statistical test
library(rcompanion) # to compute confidence interval for non parametric data
library(forcats)

# Convert line's name into factor to force the order in the plot
df_data_long$line <- as.factor(df_data_long$line)
# The rest of the script works on the column "Fv_Fm": fill it with the chosen response
if (!RESPONSE %in% names(df_data_long)) stop("Column '", RESPONSE, "' not found in ", DATA)
df_data_long$Fv_Fm <- as.numeric(df_data_long[[RESPONSE]])

# Plot scaling, so axis breaks suit the range of the response
y_max <- max(df_data_long$Fv_Fm, na.rm = TRUE)
Y_BREAKS <- if (y_max <= 2) seq(0, ceiling(y_max * 5) / 5, 0.2) else waiver()

# CLD letters go just above the highest point (or error bar) of each time x line group
point_max <- aggregate(Fv_Fm ~ time + line, data = df_data_long, FUN = max)
names(point_max)[3] <- "point_max"
LETTER_GAP <- 0.06 * y_max

# Results are saved in a subfolder named after the response
dir.create(RESPONSE, showWarnings = FALSE)
setwd(RESPONSE)
df_data_long$time <- factor(df_data_long$time, levels = time_order)

# Determining data normality status
shapiro_df <- df_data_long %>%
  dplyr::group_by(time, line) %>%
  summarise(statistic = shapiro.test(Fv_Fm)$statistic, 
            p.value = shapiro.test(Fv_Fm)$p.value)

flag_normal <- check_normality(shapiro_df)

# Data treatment according to normality status
if(flag_normal == TRUE) {
  print("Data follow a normal law")
  
  # Summary
  if (!require(Rmisc)) {install.packages("Rmisc")}
  library(plyr) # rmisc dependency
  library(Rmisc) # for the command summarySE
  my_summary <- summarySE(df_data_long, measurevar="Fv_Fm", groupvars=c("time", "line"))
  
  # Detach conflicting packages
  detach("package:Rmisc", unload=TRUE)
  detach("package:plyr", unload=TRUE)
  if (!require(lme4)) {install.packages("lme4")}
  if (!require(emmeans)) {install.packages("emmeans")}
  library(lme4)
  library(emmeans)
  library(dplyr)
  library(multcompView)
  
  # Group by time and perform the ANOVA using the mixed-effects model
  anova_results <- df_data_long %>%
    group_by(time) %>%
    group_modify(~ {
      model <- lmer(Fv_Fm ~ line + (1 | rep), data = .x)
      anova(model)
    })
  
  # Check if ANOVA indicates significant differences
  flag_anova <- check_anova(anova_results)
  
  # If significant differences are found, perform pairwise comparisons using emmeans and generate CLDs. 
  # This is more accurate than Tukey because it takes into account the replicates
  if (flag_anova == TRUE) {
    
    # Step 1: Generate pairwise comparisons and CLDs from emmeans
    emmeans_results <- df_data_long %>%
      group_by(time) %>%
      group_modify(~ {
        # Fit linear mixed-effects model with random effect for 'rep'
        model <- lmer(Fv_Fm ~ line + (1 | rep), data = .x)
        
        # Obtain emmeans for pairwise contrasts
        emmeans_pairwise <- emmeans(model, pairwise ~ line)
        
        # Convert the pairwise comparison results to a data frame
        pairwise_df <- as.data.frame(emmeans_pairwise$contrasts)
        
        # Save pairwise comparison results for each group
        write.table(pairwise_df, file = paste0("emmeans_posthoc_results_parametric_", unique(.x$time), ".txt"), quote = FALSE, row.names = FALSE, sep = '\t')
        
        # Calculate the CLD for the emmeans of each group within 'time'
        cld <- cld(emmeans_pairwise$emmeans, Letters = letters)
        
        # Return the CLD results as a data frame for further use
        return(as.data.frame(cld))
      })
    
    # Create a combined CLD data frame for all time points
    cld_table_parametric <- do.call(rbind, emmeans_results)
    
    # Save combined CLD table for parametric test to a text file
    write.table(cld_table_parametric, file = "cld_table_parametric.txt", quote = FALSE, row.names = FALSE, sep = '\t')
  }
  
  # Save summary and anova results
  write.table(my_summary, file = "Summary.txt", 
              quote = FALSE, row.names = FALSE, sep = '\t')
  write.table(anova_results, file = "Anova.txt", 
              quote = FALSE, row.names = FALSE, sep = '\t')
  if (flag_anova == TRUE) {
    write.table(emmeans_results, file = "Emmeans_posthoc.txt", 
                quote = FALSE, row.names = FALSE, sep = '\t')
  } 
  
  # Prepare data to add CLD letters on error bar
  if(flag_anova == TRUE) {
    df2 <- merge(my_summary, cld_table_parametric, by.x = c("time", "line"), by.y = c("time", "line"))
    df2 <- merge(df2, point_max, by = c("time", "line"))
    df2$label_y <- pmax(df2$Fv_Fm + df2$ci, df2$point_max) + LETTER_GAP
    df2$cld <- trimws(df2$cld)
    
    # Plot
    p<-df2 %>% mutate(line = fct_relevel(line, target_order)) %>%
      ggplot(aes(x=line, y=Fv_Fm, group = time))+
      geom_col(aes(x=line, y=Fv_Fm), color="dark grey", fill="white")+
      scale_x_discrete(limits=target_order)+
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks=Y_BREAKS)+
      ylab(RESPONSE)+
      geom_quasirandom(data = df_data_long, aes(x = line, y = Fv_Fm, color = as.factor(rep), shape = as.factor(rep)),
                       width = 0.3, alpha = 0.6)+
      scale_color_manual(values = c("#006400", "#32CD32", "#98FB98", "#A9DFBF")) +
      geom_segment(aes(x = as.factor(line), xend = as.factor(line), y = Fv_Fm - ci, yend = Fv_Fm + ci), color = "black") +
      geom_text(aes(x = as.factor(line), y= label_y ,
                    label = cld),
                size = 3, position = position_identity(), inherit.aes = TRUE)+
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            panel.grid.minor.y = element_blank(),
            axis.line.x = element_line(size = 0.5),
            axis.line.y = element_line(size = 0.5),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = 'transparent', color = NA),
            plot.background = element_rect(fill = 'transparent', color = NA),
            axis.text.y = element_text(vjust = 1))+
      facet_wrap(~time, nrow=1, scales = "free_x")
    
    print(p)
    library(svglite)
    save_plot(p, paste0(RESPONSE, "_plot"), width=8, height=3)
    
  } else {
    print("Data are not significantly different; the Tukey test was not performed.")
  }
  
} else {
  
  df_data_long <- na.omit(df_data_long)
  
  # Summary
  conf_int <- groupwiseMedian(data = df_data_long,
                              var = "Fv_Fm",
                              group = c("time", "line"),
                              conf       = 0.95,
                              R          = 5000,
                              percentile = TRUE,
                              bca        = TRUE,
                              digits     = 3)
  
  # Filter out groups with only one unique value before Kruskal-Wallis test
  filtered_data <- df_data_long %>%
    group_by(time, line) %>%
    filter(n() > 1) %>%
    ungroup()
  
  # Filter out rows where 'time' is NA
  filtered_data <- filtered_data %>% filter(!is.na(time))
  
  # Install and load required packages
  if (!require(ARTool)) install.packages("ARTool")
  if (!require(emmeans)) install.packages("emmeans")
  library(ARTool)
  library(emmeans)
  library(multcomp)
  library(dplyr)
  
  # Filter out groups with only one unique value before the test
  filtered_data <- df_data_long %>%
    group_by(time, line) %>%
    filter(n() > 1) %>%
    ungroup()
  
  # Filter out rows where 'time' is NA
  filtered_data <- filtered_data %>% filter(!is.na(time))
  
  # Step 1: Run ART ANOVA for each time group
  art_pval <- filtered_data %>%
    group_by(time) %>%
    group_modify(~ {
      # Extract the current time value for this group
      current_time <- unique(.x$time)
      print(paste("Processing time:", current_time))
      
      # Fit ART model (aligned ranks transformation)
      art_model <- tryCatch({
        art(Fv_Fm ~ line + (1 | rep), data = .x)  # ART model with random effect for 'rep'
      }, error = function(e) {
        warning(paste("Error in ART ANOVA for time:", current_time, " - ", e$message))
        return(NULL)
      })
      
      # If ART fails, return NA
      if (is.null(art_model)) {
        print(paste("ART ANOVA failed for time:", current_time))
        return(data.frame(p = NA))
      } else {
        # Perform ANOVA on the aligned ranks
        anova_results <- anova(art_model)
        p_value <- anova_results$`Pr(>F)`[1]  # Get p-value for 'line' effect
        
        return(data.frame(p = p_value))  # Return the p-value
      }
    })
  
  # Save ART ANOVA results to a text file
  write.table(art_pval, file = "art_anova_results.txt", quote = FALSE, row.names = FALSE, sep = '	')
  
  # Step 2: Check if ART ANOVA indicates significant differences
  flag_art <- FALSE  # Initialize flag
  if (!is.null(art_pval) && nrow(art_pval) > 0) {
    for (i in 1:nrow(art_pval)) {
      if (!is.na(art_pval$p[i]) && art_pval$p[i] < 0.05) {
        print(paste0("ART ANOVA test compares means, p-value for the group ", art_pval$time[i], " is < 0.05, indicating that at least one of the means is significantly different. Post-hoc test performed."))
        flag_art <- TRUE
      } else {
        print(paste0("ART ANOVA test compares means, p-value for the group ", art_pval$time[i], " is > 0.05, indicating no significant difference."))
      }
    }
  }
  
  # Step 3: Post-hoc test if ART is significant
  if (flag_art == TRUE) {
    # Integrate pairwise comparisons and CLDs from emmeans for significant groups
    
    emmeans_results <- filtered_data %>%
      # Reorder 'line' factor based on target_order before grouping
      mutate(line = factor(line, levels = target_order)) %>%
      group_by(time) %>%
      group_modify(~ {
    
            # Fit ART model with random effect for 'rep'
        model <- art(Fv_Fm ~ line + (1 | rep), data = .x)  
        
        # Obtain emmeans for pairwise contrasts
        emmeans_pairwise <- emmeans(artlm(model, "line"), pairwise ~ line)
        
        # Convert the pairwise comparison results to a data frame
        pairwise_df <- as.data.frame(emmeans_pairwise$contrasts)
        
        # Save pairwise comparison results for each group
        write.table(pairwise_df, file = paste0("emmeans_posthoc_results_", unique(.x$time), ".txt"), quote = FALSE, row.names = FALSE, sep = '\t')
        
        # Return pairwise comparison data frame for further use
        return(pairwise_df)
      })
    
    # Create a combined data frame for all pairwise comparisons
    emmeans_results_pairwise <- do.call(rbind, emmeans_results)
    
    # Save combined pairwise comparisons to a text file
    write.table(emmeans_results_pairwise, file = "emmeans_posthoc_results_combined_pairwise.txt", quote = FALSE, row.names = FALSE, sep = '\t')
    
    # Generate CLDs for significant groups
    emmeans_results_cld <- filtered_data %>%
      # Reorder 'line' factor based on target_order before grouping
      mutate(line = factor(line, levels = target_order)) %>%
      group_by(time) %>%
      group_modify(~ {
        # Fit ART model with random effect for 'rep'
        model <- art(Fv_Fm ~ line + (1 | rep), data = .x)  
        
        # Obtain emmeans for pairwise contrasts
        emmeans_pairwise <- emmeans(artlm(model, "line"), pairwise ~ line)
        
        # Calculate the CLD for the emmeans of each group within 'time'
        cld <- cld(emmeans_pairwise$emmeans, Letters = letters)
        
        # Return the CLD results for further use
        return(as.data.frame(cld))
      })
    
    
  }
  

# Prepare data to add CLD letters on error bar if ART is significant
if(flag_art == TRUE) {
  df2 <- merge(conf_int, emmeans_results_cld, by.x = c("time", "line"), by.y = c("time", "line"))
  df2 <- merge(df2, point_max, by = c("time", "line"))
  df2$label_y <- pmax(df2$Percentile.upper, df2$point_max) + LETTER_GAP
  df2$.group <- trimws(df2$.group)
  
  # Plot
  p <- df2 %>%
    mutate(line = factor(line, levels = target_order)) %>%
    ggplot(aes(x = line, y = Median, group = time)) +
    geom_col(aes(x = line, y = Median), color = "dark grey", fill = "white") +
    geom_text(aes(x = as.factor(line), y = label_y, label = .group), size = 3, position = position_identity(), inherit.aes = TRUE) +
    # Set y-axis with specific breaks and expand to ensure x-axis aligns with y = 0
    scale_y_continuous(breaks = Y_BREAKS, limits = c(0, max(df2$label_y, na.rm = TRUE) + LETTER_GAP), expand = c(0, 0)) +
    ylab(RESPONSE) +
    geom_quasirandom(data = df_data_long, aes(x = line, y = Fv_Fm, color = as.factor(rep), shape = as.factor(rep)),
                     width = 0.3, alpha = 0.6) +
    scale_color_manual(values = c("#006400", "#32CD32", "#98FB98", "#A9DFBF")) +
    geom_segment(aes(x = as.factor(line), xend = as.factor(line), y = Percentile.lower, yend = Percentile.upper), color = "black") +

    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_line(size = 0.5),
          axis.line.y = element_line(size = 0.5),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = 'transparent', color = NA),
          plot.background = element_rect(fill = 'transparent', color = NA),
          axis.text.y = element_text(vjust = 1)) +
    facet_wrap(~time, nrow = 1, scales = "free_x")
  
  print(p)
    library(svglite)
    save_plot(p, paste0(RESPONSE, "_plot"), width=8, height=3)
    
  } else {
    print("Data are not significantly different; the posthoc test was not performed.")
  }
}

# Return to the folder of the script
setwd("..")
