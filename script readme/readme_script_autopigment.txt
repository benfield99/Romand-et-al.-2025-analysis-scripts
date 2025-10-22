Pigment-by-Pigment Analysis Script

This R script performs pigment-by-pigment statistical analysis and plotting from an Excel dataset containing pigment quantities and ratios across treatments and plant lines.
It automatically detects numeric pigment columns, determines the appropriate statistical test for each pigment, and produces publication-quality figures and tables.

Overview

For each pigment:

Tests data normality (Shapiro–Wilk) per Treatment × Line group.

Chooses analysis path:

Parametric: one-way ANOVA followed by Tukey HSD and compact letter display (CLD).

Non-parametric: Kruskal–Wallis test followed by Dunn’s test (if ≥3 groups) and CLD.

Computes summary statistics:

Mean ± CI (parametric) or

Median ± bootstrapped 95 % CI (non-parametric, R = 5000).

Generates bar + beeswarm plots with confidence intervals and group letters.

Combines all pigment plots into a multi-panel figure (PDF/PNG/SVG).

Inputs

DATA — Excel file (default: pigments_short.xlsx)
containing columns for factors (Treatment, Line) and pigment values.

Optional parameters at the top of the script allow customization of:

colour scheme (COLUMN_LINE_COLOUR, COLUMN_FILL_COLOUR, etc.)

pigment inclusion (PIGMENT_INCLUDE)

plotting order (order1, order2)

output layout and size.

Outputs

For each pigment (in /out_by_pigment):

Summary tables (Summary_*.txt)

Statistical results (Anova_*.txt, Tukey_*.txt, Kruskal_*.txt, Dunn_*.txt)

CLD tables (CLD_*.txt)

Individual plots (plot_*_normal.png/svg or plot_*_nonpara.png/svg)

Combined multi-panel figure:

ALL_pigments_bar_CLD.pdf/png/svg

Dependencies

Automatically installs and loads:
dplyr, tidyr, readxl, rstatix, rcompanion, ggplot2,
ggbeeswarm, RColorBrewer, Rmisc, forcats, multcompView, patchwork.

Notes

Pigments are ordered automatically by abundance (means or medians),
with ratio pigments (VAZ, DEPS) placed last.

Each pigment is analysed independently with per-pigment CLDs.

Figures use consistent publication-style aesthetics (light bars, dark points, transparent backgrounds).