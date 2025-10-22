Figure 1D.
Shows WT (Col-0) vs OX:RSH3.1 (H1) comparison from plants grown under growth light.
NPQ mutants and double mutants shown in Figure 2A.
Full experiment with NPQ mutants and double mutants and effects of high light exposure shown in Supplementary figure S6.

NPQ Data Analysis Script

This R script analyses non-photochemical quenching (NPQ) data across plant lines and time points. It tests data normality, computes confidence intervals, and generates plots summarising NPQ kinetics.

Input:
Two text files:

DATA — raw NPQ measurements (time, line, secondes, replicates).

CODE — mapping file linking time codes to seconds.

Workflow:

Shapiro–Wilk tests assess normality for each line × secondes group.

If data are normal → mean ± CI via Rmisc::summarySE.
If not → median ± bootstrapped 95 % CI via rcompanion::groupwiseMedian (R = 5000).

Summary tables and plots are generated for each time point.

Outputs:

Shapiro test tables (Shapiro_*.txt)

Summary statistics (Summary_Normal_*.txt or Summary_NonNormal_*.txt)

Combined PDF of plots (NPQ_plots.pdf) showing curves with shaded confidence intervals

Dependencies:
tidyr, dplyr, rcompanion, ggplot2, and (for normal data) Rmisc, plyr.

Notes:
The script automatically switches between parametric and non-parametric summaries based on data distribution. Each time point is analysed separately, and results are saved with consistent filenames for reproducibility.