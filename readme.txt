Repository — Data Analysis Scripts for Romand et al. Guanosine tetraphosphate (ppGpp) signalling promotes high-light tolerance in Arabidopsis thaliana.

This repository contains the R scripts used for the analyses presented in the manuscript Romand et al. 2025.
 DOI: https://doi.org/10.57745/YZCG1W

Each script performs a specific analysis used in the study, covering chlorophyll fluorescence, pigment quantification, 77 K emission spectra, and simple response data (e.g. metabolites or immunoblot quantification). The repository is intended to ensure analytical transparency and reproducibility.

Repository structure
File	Description
script_fvfm.r	Analysis of Fv/Fm data — statistical tests (parametric/non-parametric), compact letter display (CLD), and figure generation.

script_npq.r	Analysis of NPQ (non-photochemical quenching) data with bootstrapped confidence intervals.

script_QY_plot.R	Analysis of QY (quantum yield) time-series data across multiple assays, selecting parametric or non-parametric summaries based on normality.
script_77K_qgam.R	Analysis of 77 K fluorescence emission spectra using generalized additive models (qGAM) to detect differences in emission bands.
script_autopigment.R	Quantification and comparison of pigment absorbance spectra; used to visualize differences between genotypes or treatments.
script_PSrates.Rmd	Markdown report for photosynthetic rate analysis with embedded plots and summary tables.
script_simpleresponse.r	General script for analyzing simple responses such as metabolite concentrations or single-band intensities, including summary statistics and group comparisons.


Folders

example data/ – small example datasets for testing each script.

script readme/ – individual short documentation files describing workflow, inputs, and outputs for each script.


Usage

1. Clone or download this repository.

2. Open the desired script in RStudio.

3. Adjust the working directory and input file names at the top of the script if necessary.

4. Run the script; all required packages will be installed automatically if missing.

5. Output tables and plots are written to the working directory.

Each script is self-contained and can be executed independently.


The complete raw and processed datasets are archived in the French National Data Repository (Recherche Data Gouv):

DOI:  https://doi.org/10.57745/YZCG1W

Only minimal example data are included here for demonstration.


Requirements

R ≥ 4.3

Core packages used across scripts:
tidyverse, dplyr, rstatix, rcompanion, ARTool, emmeans, lme4, Rmisc, ggplot2, svglite, mgcv.

Each script automatically installs missing dependencies.


License and citation

This repository is released under  CC-BY 4.0.

If you use these scripts, please cite the associated manuscript and dataset DOI.
