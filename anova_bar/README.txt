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

What you can change, and what is fixed in the code
  Common to all (fixed): significance level 0.05; bootstrap 5000 resamples, 95% CI.

  ELIP2_anova.r (version 2)
    Settings: RefLine, target_order, input file (in the read_excel() line).
    Fixed: columns ratio, line and treatment; output fvfm_plot2.svg, 2 x 3 in.

  HOTE_anova.r (version 3)
    Settings: RefLine, target_order, DATA, treatment_order.
    Fixed: columns ROS, line and treatment; y-axis label ROS; outputs ros_plot_24.svg (parametric)
    or ros_plot_2.svg (non-parametric), 2 x 4 in.

  ppGpp_anova.r (version 4)
    Settings: COLUMN_LINE_COLOUR, COLUMN_FILL_COLOUR, POINT_COLOR, PALETTE (RColorBrewer palette for the
    bars; comment it out to use COLUMN_FILL_COLOUR), DATA, var1, var2, MEASURE_COL, plot_width,
    plot_height, RefLine.
    Fixed: axis labels "Treatment" and "Median Value"; output names plot_normal.svg / plot_nonpara.svg.

  ppGpp_anova_extended.r
    Settings: as version 4, plus Y_LABEL (y-axis label) and OUTPUT_DIR (output folder).
    Fixed: factor order is taken from the order in the Excel file; var1 and var2 are swapped when var2
    has a single level.

  tocopherols_anova.R, PSrates_anova.R (multi-measure)
    Settings: COLUMN_LINE_COLOUR, PALETTE, POINT_COLOR, START_Y_AT_ZERO, DATA, var1 (panels),
    var2 (bars), MEASURE_INCLUDE (measures and their order; NULL = all numeric columns), Y_LABELS,
    order1, order2 (factor order; NULL = alphabetical), OUTPUT_BASE, plot_width, plot_height
    (per panel), combined_ncol.
    Fixed: name of the combined figure (ALL_toco_bar_CLD / ALL_PSrates_bar_CLD, near the end of the
    script); Dunn test only with 3 or more groups.

  pigments_anova.R
    Settings: COLUMN_LINE_COLOUR, COLUMN_FILL_COLOUR, POINT_COLOR, START_Y_AT_ZERO, DATA, var1, var2,
    PIGMENT_INCLUDE (NULL = all numeric columns), order1, order2, OUTPUT_BASE, plot_width,
    plot_height, combined_ncol.
    Fixed: pigment ordering and colour classes are based on the column names: amounts first, then vaz
    and deps, then the grey ratios ratio_ab and ratio_chlcar (a ratio with another name is treated as
    an amount); y-axis labels "<pigment> mean ± CI" or "median ± CI";
    combined figure name ALL_pigments_bar_CLD.
