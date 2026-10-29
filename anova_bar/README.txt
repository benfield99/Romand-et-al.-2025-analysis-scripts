ANOVA-bar family - bar plots with parametric or non-parametric tests

All scripts test normality with a Shapiro-Wilk test on the pooled ANOVA residuals.
  Normal:     one-way ANOVA, Tukey HSD, CLD letters, mean +/- 95% CI.
  Not normal: Kruskal-Wallis, Dunn test (BH adjustment), CLD letters, median +/- bootstrap 95% CI.
              (The multi-measure versions run the Dunn test only with 3 or more groups and take the
              letters from the Kruskal-Wallis test when there are 2.)

Scripts, from the first to the most general version
  ELIP2_anova.r            Figure 4B. Version 2. One measure (ratio) by line within treatment.
  HOTE_anova.r             Figure 1D. Version 3. Same template as version 2.
  ppGpp_anova.r            Figure 4E. Version 4: generalised to any measure column (MEASURE_COL) with two
                           factors, var1 (panels) and var2 (groups compared).
  ppGpp_anova_extended.r   Figure S1. Version 4 extended: var1 and var2 are swapped automatically when var2
                           has a single level, factor order is taken from the Excel file, and SVG / PDF /
                           PNG are written to an output folder. (In the data repository this script is
                           Fig_S1/ppGpp_anova.r.)
  tocopherols_anova.R      Figure 4C. Multi-measure version: the version-4 analysis applied to several
                           measure columns, with the panels combined into one figure.
  PSrates_anova.R          Figure S3. Same script as tocopherols_anova.R; only the settings block differs.
  pigments_anova.R         Figures S9 and 4D. Multi-measure version extended for pigments: automatic
                           ordering (amounts ranked, then VAZ, DEPS and other ratios), colour palettes per
                           pigment class, and a combined multipanel page.

Input
  An Excel file with one row per sample, the grouping columns named in the settings block
  (e.g. line / Line and treatment / Treatment) and one or more numeric measure columns.

Output
  Plot(s) as SVG (and PDF / PNG for the later versions), Summary, Shapiro, Anova, Tukey and CLD tables
  (Kruskal / Dunn tables when non-parametric). The multi-measure scripts write one subfolder per measure.

Example data
  example_data/Fig4B   Book1.xlsx      ELIP2/Ponceau ratio, WT vs OX:RSH3.1
  example_data/Fig1D   ros.xlsx        HOTE (column ROS), nmol/g fresh weight
  example_data/Fig4E   ppgpp.xlsx      ppGpp, pmol/g fresh weight, EL time course in WT
  example_data/FigS1   data.xlsx       ppGpp and GTP in all lines
  example_data/Fig4C   toco.xlsx       alpha- and gamma-tocopherol, PC-8, nmol/g fresh weight
  example_data/FigS3   PSrates.xlsx    PSI and PSII photochemical rates, PSI fraction
  example_data/FigS9   pigments.xlsx   HPLC pigments, mol per 100 mol Chl a+b
  Copy the script into the example folder and run it there.

Line codes: 01_WT / Col / WT = wild type, 02_H1 / H1 / OXRSH3.1 = OX:RSH3.1, 03_G8 / OXRSH3.2 = OX:RSH3.2.
