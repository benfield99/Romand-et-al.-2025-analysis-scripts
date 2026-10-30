Fv/Fm-ART family - chlorophyll fluorescence over time

Scripts
  fvfm_detached_leaf_ART.r         Figures 3C and S8B. Version 7: raw Fv/Fm of detached leaves at each
                                   time point. The Fig S8B copy differs only in its settings block.
  FvFm_overlays_spline.R           Figures 3D and S8C. Normalised Fv/Fm (0 h = 1): means with BCa bootstrap
                                   95% CI, joined by natural cubic splines as a visual guide. The Fig S8C
                                   copy differs only in line names and panel groupings.
  fvfm_F0_Fm_detached_leaf_ART.r   Figure S6. Version 8: version 7 with a choice of response variable
                                   (RESPONSE). Run once for each of Fv_Fm, normalized_value, Fo, Fo_norm,
                                   Fm, Fm_norm; each run writes a subfolder named after RESPONSE.
  combine_figure.R                 Figure S6. Assembles the six panels saved by the runs above (.rds)
                                   into one A4 landscape figure.
  rebuild_combined_data.R          Figures 3C-D and S6. Rebuilds combinedresults_normalized.txt from the
                                   per-leaf FluorCam exports in per_line_data/ and checks that the result
                                   is identical (writes combinedresults_normalized_rebuilt.txt).
  fvfm_raw_and_normalised_ART.r    Figures 2E-F. 4-panel version derived from version 7: whole plants at
                                   T0, T24 and T48 (raw) plus normalised Fv/Fm at T48.
  fvfm_T0_T24_raw_normalized.r     Figures S7D-E. Derived from the 4-panel version: T0 and T24 only, one
                                   analysis function applied to raw and normalised Fv/Fm.

Input
  combinedresults_normalized.txt (tab-separated), one row per leaf or plant and time point:
  time, line, rep, Fv_Fm (or value), normalized_value (relative to the mean at T0 of the same line and
  replicate). The Fig 3C-D / S6 file also has Fo, Fm, Fv, Fo_norm, Fm_norm, *_average_t0, Size, QY_max.
  per_line_data/<replicate>/<time>_<line>.TXT: FluorCam 7 numeric exports, rows = variables,
  columns = leaves.

Method
  Normality is tested for each line x time group. If all groups are normal: linear mixed model
  (response ~ line + (1|rep)) per time point, emmeans contrasts, CLD letters. Otherwise: aligned-ranks
  ANOVA (ART) with the same model per time point, emmeans contrasts on the aligned ranks, CLD letters.
  Bars show means (parametric branch) or medians (ART branch) with 95% CI; all figures in the
  manuscript used the ART branch.

Example data
  example_data/Fig3CD_S6   detached leaves, WT, OX:RSH3.1, npq1, npq4 and double mutants
                           (fvfm_detached_leaf_ART.r, FvFm_overlays_spline.R, the Fig S6 scripts)
  example_data/Fig2EF      whole plants, 0 / 24 / 48 h EL (fvfm_raw_and_normalised_ART.r)
  example_data/FigS7DE     whole plants, 0 / 24 h EL, ch1 and glk1,2 (fvfm_T0_T24_raw_normalized.r)
  Copy the script into the example folder and run it there.

Time codes: T0 = before EL, T2h-HL = after 2 h EL, T0.5-r / T2h-r / T4h-r = after 0.5 / 2 / 4 h recovery;
T24 / T48 = after 24 / 48 h EL.
Line codes: WT / col0 = wild type, h1 / H1 = OX:RSH3.1, h1npq1 / h1npq4 = OX:RSH3.1 npq1 / npq4,
h1ch1 = OX:RSH3.1 ch1, glk12 = glk1,2.

What you can change, and what is fixed in the code
  Common to the ART scripts (fixed): column names time, line and rep; replicate colours for up to four
  replicates (three in fvfm_T0_T24_raw_normalized.r); significance level 0.05; bootstrap 5000
  resamples, 95% CI.

  fvfm_detached_leaf_ART.r
    Settings: RefLine, target_order (lines, in plot order), time_order, DATA (input file).
    Fixed: response column Fv_Fm; y axis 0-1 with breaks every 0.2; output fvfm_plot.svg, 8 x 3 in.

  fvfm_F0_Fm_detached_leaf_ART.r
    Settings: target_order, time_order, DATA, RESPONSE (column analysed). RefLine is not used.
    Fixed: output folder and file names follow RESPONSE; y-axis label = RESPONSE (relabelled by
    combine_figure.R).

  combine_figure.R
    Settings: PANELS (folder and y-axis title of each panel, in reading order), N_COL, target_order,
    LINE_LABELS (x-axis names), TIME_LABELS (time point names), TAGS (panel letters), FIG_WIDTH,
    FIG_HEIGHT, TEXT_PT, LETTER_PT, POINT_SIZE, OUT_BASE (output file name).
    Fixed: reads <folder>/<folder>_plot.rds written by fvfm_F0_Fm_detached_leaf_ART.r.

  rebuild_combined_data.R
    Settings: EXPERIMENTS (folder of each replicate), VARS (FluorCam variables kept), NORM_VARS
    (variables normalised to T0), TIME_MAP (time codes in file names -> data), OUT_FILE, CHECK_FILE.
    Fixed: FluorCam 7 numeric export format; file names <time>_<line>.TXT.

  fvfm_raw_and_normalised_ART.r
    Settings: RefLine, target_order, time_order, DATA.
    Fixed: columns Fv_Fm and normalized_value; the normalised panel shows T48; outputs
    fvfm_4panel_normalized.svg (11 x 3 in), fvfm_raw_3panel.svg, fvfm_normalized_T48.svg.

  fvfm_T0_T24_raw_normalized.r
    Settings: RefLine, target_order, time_order, DATA.
    Fixed: columns Fv_Fm and normalized_value; outputs fvfm_T0_T24_raw_normalized.svg (11 x 4 in),
    fvfm_raw_T0_T24.svg and fvfm_normalized_T0_T24.svg (6 x 4 in).

  FvFm_overlays_spline.R
    No separate settings block; edit the values near the top of the script: input file name,
    time_order, line_order, colours (my_colors) and point shapes (my_shapes) for each line.
    Further down: the lines shown in each of the three panels (make_panel() calls), the y axis
    (0.4-1.1) and the output file (FvFm_overlays_spline.svg, 8 x 3 in). Fixed: column normalized_value;
    bootstrap seed 42.
