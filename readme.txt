Repository - Data analysis scripts for Romand et al.
"Guanosine tetraphosphate (ppGpp) promotes tolerance to excess light in Arabidopsis thaliana"

This repository contains the R scripts used for the analyses in the manuscript, grouped by type of
analysis, with example data for each script. The intention is analytical transparency and reuse.

Version: revised manuscript (2026). The scripts for the preprint version (2025) are in the git history
(commit c01fbaa) and, for the two scripts no longer used, in archive/.

Complete raw and processed datasets, with one folder per figure panel, are archived in the
French National Data Repository (Recherche Data Gouv):
DOI: https://doi.org/10.57745/YZCG1W


REPOSITORY STRUCTURE

Each folder holds one family of scripts (scripts sharing the same analysis template), a README.txt
describing the scripts, and example_data/ with real input data from the figures.

bleaching_ART/   Photobleaching (% of plant area) at one time point: median and bootstrap CI,
                 aligned-ranks (ART) mixed model with experimental replicate as random effect.
fvfm_ART/        Chlorophyll fluorescence (Fv/Fm, F0, Fm) of plants or detached leaves over time:
                 normality test, then linear mixed model or ART ANOVA per time point, CLD letters;
                 overlay plots of normalised values; data rebuilding from FluorCam exports.
anova_bar/       Bar plots with one-way ANOVA + Tukey or Kruskal-Wallis + Dunn, chosen from a normality
                 test on the ANOVA residuals; single-measure and multi-measure (e.g. pigments) versions.
npq/             NPQ kinetics from FluorCam exports, with mean or median and confidence intervals.
spectra_77K/     77 K chlorophyll fluorescence spectra: qGAM fits, difference spectra vs wild type with
                 simultaneous CIs, peak position and band-wise global p-values.
archive/         Scripts used for the preprint version but not for the revised manuscript.


SCRIPT FOR EACH FIGURE

Figure      Script                                         Example data
1B, 2D      bleaching_ART/bleaching_ART.r                  bleaching_ART/example_data/Fig1B
S7C         bleaching_ART/bleach_panel_24h.r               bleaching_ART/example_data/FigS7C
1D          anova_bar/HOTE_anova.r                         anova_bar/example_data/Fig1D
1E, 2A, S5  npq/script_npq.R                               npq/example_data
1F-H        spectra_77K/script_77K_qgam.R                  spectra_77K/example_data
2E-F        fvfm_ART/fvfm_raw_and_normalised_ART.r         fvfm_ART/example_data/Fig2EF
3C, S8B     fvfm_ART/fvfm_detached_leaf_ART.r              fvfm_ART/example_data/Fig3CD_S6
3D, S8C     fvfm_ART/FvFm_overlays_spline.R                fvfm_ART/example_data/Fig3CD_S6
4B          anova_bar/ELIP2_anova.r                        anova_bar/example_data/Fig4B
4C          anova_bar/tocopherols_anova.R                  anova_bar/example_data/Fig4C
4D, S9      anova_bar/pigments_anova.R                     anova_bar/example_data/FigS9
4E          anova_bar/ppGpp_anova.r                        anova_bar/example_data/Fig4E
S1          anova_bar/ppGpp_anova_extended.r               anova_bar/example_data/FigS1
S3          anova_bar/PSrates_anova.R                      anova_bar/example_data/FigS3
S6          fvfm_ART/fvfm_F0_Fm_detached_leaf_ART.r,       fvfm_ART/example_data/Fig3CD_S6
            fvfm_ART/combine_figure.R,
            fvfm_ART/rebuild_combined_data.R
S7D-E       fvfm_ART/fvfm_T0_T24_raw_normalized.r          fvfm_ART/example_data/FigS7DE

Where one script serves several figures, the copies used for the other figures differ only in their
settings block (line names, order, file names, plot size). The exact copy used for each panel is in
the data repository.


USAGE

1. Clone or download this repository.
2. Copy the script into the folder that contains the input data (for example, its example_data folder),
   or put your own data, formatted like the example, in the same folder as the script.
3. Open the script in R or RStudio and set the working directory to that folder.
4. Adjust the settings block at the top of the script if necessary: input file name, line names and
   order, time points, plot size.
5. Run the script. Missing packages are installed automatically by most scripts.
6. Output tables and plots are written to the working directory (or to the output subfolder named in
   the script header).

Each script starts with a header giving the figure(s) it was used for, its inputs and outputs and the
statistical method. Each family folder has a README.txt with more detail.


REQUIREMENTS

R >= 4.1

Packages used across the scripts:
ARTool, boot, cowplot, dplyr, emmeans, forcats, ggbeeswarm, ggplot2, lme4, MASS, matrixStats, multcomp,
multcompView, patchwork, prospectr, purrr, qgam, rcompanion, readr, readxl, rlang, RColorBrewer, Rmisc,
rstatix, scales, stringr, svglite, tidyr, tidyverse.

The scripts were tested with R 4.4.1 on the example data in this repository; their statistical
outputs match those of the figures.


CHANGES FROM THE PREPRINT VERSION

- Scripts grouped into family folders, each with its README.txt and example data from the figures.
- The scripts are the versions used for the revised manuscript. In the ANOVA-based scripts, normality
  is now tested on the pooled ANOVA residuals rather than group by group.
- Added: bleaching ART scripts; detached-leaf F0 and Fm analysis (version 8) with the figure assembly
  and data rebuilding scripts; Fv/Fm overlay plots; the two derived Fv/Fm scripts; the multi-measure
  ANOVA scripts (tocopherols, PS rates).
- Moved to archive/: script_QY_plot.R and script_PSrates.Rmd (not used in the revised manuscript).


LICENSE AND CITATION

This repository is released under CC-BY 4.0.

If you use these scripts, please cite the associated manuscript and the dataset DOI.
