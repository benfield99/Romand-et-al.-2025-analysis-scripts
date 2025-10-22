R Markdown: Statistical Analysis and Plotting

This R Markdown script performs statistical analysis and visualization for experimental data grouped by two categorical factors (e.g. Treatment and Line). It automatically detects whether data follow a normal distribution and applies parametric or non-parametric methods accordingly.

Input:
A data file (e.g. .xlsx, .csv, .txt) with three columns — two grouping factors (var1, var2) and one numeric response (MEASURE_COL). These variable names and basic plot parameters (point color, palette) are set at the start of the script.

Analysis overview:

Normality (Shapiro–Wilk) and variance (Levene) tests are performed per subgroup.

If all groups are normal, the script runs a one-way ANOVA (per var2) followed by Tukey HSD post-hoc tests.

If at least one group is non-normal, it runs a Kruskal–Wallis test followed by Dunn’s post-hoc tests (BY correction).

Compact Letter Displays (CLDs) are generated from post-hoc results to summarise statistical groupings.

Plots:
Bar plots show the mean ± 95 % CI (parametric) or median ± bootstrapped 95 % CI (non-parametric), overlaid with jittered raw data points. Colors and facets are automatically assigned by var1 and var2.

Outputs:

HTML report with summary tables, test results, and plots

Vector and PDF figures (my_ggplot1.svg, my_ggplot1.pdf)

Tables of ANOVA/Kruskal, Tukey/Dunn, and CLD results

Packages:
Key R packages include ggplot2, rstatix, emmeans, boot, Rmisc, multcompView, and dependencies (auto-installed if missing).

Notes:
The analysis branch depends solely on normality. Groups with identical values are treated as normal. Bootstrapping uses 10,000 resamples with a fixed random seed for reproducibility.