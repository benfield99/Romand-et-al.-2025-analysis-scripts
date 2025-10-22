README — QY Data Analysis Across Multiple Assays 
Purpose

This R script analyzes the response variable (in this case quantum yield (QY)) time-series data across multiple assays (values grouped by time). For each assay, it checks normality per line × secondes, then plots QY versus time with confidence intervals using either parametric (mean ± CI) or non-parametric (median ± bootstrapped CI) summaries. Facets are ordered by target_order.

Inputs

Working directory: WORKDIR (script reads files here).

Data files: tab-delimited files named results_npq* (historical name; metric is QY).

Expected columns after the script’s simple column-bind:
time, line, secondes, rep1, rep2, …

Time-code map (optional): CODE with columns code and secondes.

If your files already contain numeric seconds in secondes, the mapping step is effectively a no-op (identity).

Line order: target_order controls facet order.

Note: The script column-binds matching files and then pivots replicates to long format. It does not perform complex merging or validation; ensure your input files are consistent.

Workflow (per assay = each time level)

Normality: Shapiro–Wilk per line × secondes.

If normal: mean ± CI via Rmisc::summarySE; ribbon + points + lines.

If non-normal: median ± bootstrapped 95% CI via rcompanion::groupwiseMedian (R = 5000); ribbon + points + lines.

Plot: QY vs secondes, faceted by line (ordered by target_order).

Return: a list of ggplot objects (one per assay).

To save: wrap the lapply(..., print) in a graphics device (e.g., pdf("QY_plots.pdf"); ...; dev.off()).

Dependencies

Auto-installed/loaded if missing:
tidyr, dplyr, rcompanion, ggplot2, and (for normal data) Rmisc, plyr.

Notes

File pattern results_npq* is historical; the analysis here is QY.

The script expects consistent column structure across files; adjust as needed if your data are pre-merged or already in long format.

No between-line inferential tests are performed—figures summarise trajectories over time per line.