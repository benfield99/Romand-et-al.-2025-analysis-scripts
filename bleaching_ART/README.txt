Bleaching-ART family - photobleaching at one time point

Scripts
  bleaching_ART.r      Figures 1B and 2D (48 h EL). The Fig 2D copy differs only in its settings block.
  bleach_panel_24h.r   Figure S7C. Same analysis, generalised: the time point is set by TARGET_TIME,
                       output file names follow it, and LINE_RENAME harmonises line names between
                       experiments.

Input
  One CSV file per experimental replicate, one row per plant, with at least the columns
  line, time and pct_bleached (% of plant area photobleached). The files are listed in DATA_FILES
  at the top of the script; the replicate name is taken from the list names (rep1, rep2, ...).
  Rows are filtered to TARGET_TIME.

Method
  Median and bootstrap 95% CI (R = 5000) per line.
  Aligned-ranks transformation (ART) mixed model: pct_bleached ~ line + (1|rep).
  Pairwise contrasts with Benjamini-Hochberg adjustment; compact letter display (CLD).

Output (in the script folder)
  bleach_panel_<time>.svg (bleaching_ART.r: bleach_panel_48h23.svg)
  Summary_median_CI_<time>.txt, ART_Anova_<time>_line.txt, ART_emm_pairs_line_<time>.txt,
  ART_CLD_line_<time>.txt

Example data
  example_data/Fig1B   rep1_long.csv, rep2_long.csv, rep3_long.csv   (WT, OX:RSH3.1, OX:RSH3.2)
  example_data/FigS7C  20221214rep1.csv, 20221019rep2.csv            (WT, OX:RSH3.1, ch1, OX:RSH3.1 ch1, glk1,2)
  Copy the script into the example folder and run it there.

Line codes: col / Col / WT = wild type, h1 / H1 = OX:RSH3.1, g8 = OX:RSH3.2, CH1 = ch1,
CH1H1 = OX:RSH3.1 ch1, GLK = glk1,2.
