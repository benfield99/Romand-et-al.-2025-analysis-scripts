Script for Fv/Fm Data Analysis Overview

This R script analyses chlorophyll fluorescence (Fv/Fm) data from detached leaves under different treatments. It accounts for biological replicates, tests normality, applies either parametric or non-parametric statistics, and produces summary tables, post-hoc comparisons, compact letter displays (CLD), and publication-ready plots. The workflow is fully reproducible and adaptable to similar experiments.

Workflow summary

Normality check:
Shapiro–Wilk test applied to each time × line combination.

If all groups are normal → parametric path

Otherwise → non-parametric path

Parametric path:

Linear mixed-effects ANOVA (lmer), with line as fixed and rep as random effect

Post-hoc pairwise comparisons via emmeans (Tukey-adjusted)

CLD letters summarising significant group differences

Outputs:
Summary.txt, Anova.txt, emmeans_posthoc_results_combined_parametric.txt,
cld_table_parametric.txt, fvfm_plot.svg

Non-parametric path:

Median and bootstrapped 95% CI (groupwiseMedian)

ART ANOVA (ARTool) accounting for replicates

Post-hoc emmeans and CLD letters for significant groups

Outputs:
art_anova_results.txt, emmeans_posthoc_results_combined_pairwise.txt,
emmeans_results_cld.txt, fvfm_plot.svg

Input

File: combinedresults_normalized.txt (tab-delimited)

Columns:
time (treatment/time point), line (genotype), rep (replicate), value (Fv/Fm)

User parameters:
RefLine, target_order, and time_order set at the top of the script.

Output figure

File: fvfm_plot.svg
Shows mean (or median) ± CI per genotype and time point, replicate points (colored), and CLD letters above bars indicating statistically distinct groups.

Dependencies

Automatically installs if missing:
tidyr, dplyr, tidyverse, rstatix, ggbeeswarm, RColorBrewer,
rcompanion, svglite, lme4, emmeans, ARTool, multcomp.

Notes

Replicates are included as random effects.

Reference line and plotting order are easily adjustable.

