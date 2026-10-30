77 K chlorophyll fluorescence spectra - qGAM analysis

Script
  script_77K_qgam.R   Figures 1F-H. Author: Ben Field.

What it does
  - Reads raw spectrometer CSV exports: one file per biological replicate, six technical replicate
    spectra per file (pairs of columns Wavelength (nm), Intensity (a.u.)).
  - Savitzky-Golay smoothing; normalisation of each trace to the PSII peak (650-700 nm).
  - Median of the technical replicates for each biological replicate.
  - Quantile GAM (qGAM, tau = 0.5) fits of the median spectrum per line, with 95% CIs.
  - Difference spectra (line - wild type) with simultaneous 95% CIs, significant wavelength ranges,
    peak position and amplitude of the difference with uncertainty, and band-wise global p-values.

Settings
  files      input files per line (wild type = wt, OX:RSH3.1 = h, OX:RSH3.2 = g8)
  band_min / band_max   wavelength band analysed: 660-800 nm for Fig 1F, 660-685 nm for Figs 1G-H

Output (per band)
  qgam_median_<band>.svg / .pdf           fitted spectra
  qgam_differences_vs_wt_<band>.svg / .pdf difference spectra
  summary_<band>.csv                      peak estimates, significant ranges and global p-values

Example data
  example_data/  wt1-4.csv, h1_1-4.csv, g8_1-4.csv
  Copy the script into the example folder and run it there.

Colours: wild type black; OX:RSH3.1 light green; OX:RSH3.2 dark green.

What you can change
  Settings at the top:
    files                  input files for each line
    MovingPoint            Savitzky-Golay window (window = MovingPoint + 1; even number)
    RepTechNb              number of technical replicate spectra per file
    band_min, band_max     wavelength band analysed (nm)

Fixed in the code (edit the code to change)
  - Line names: the list names in files must be wt (wild type), h (OX:RSH3.1) and g8 (OX:RSH3.2). They
    are renamed col, h1 and g8, and col is the reference for the difference spectra. Colours are set
    for these three names.
  - Normalisation window for the PSII peak: 650-700 nm.
  - Simulations: 1500 (difference curves) and 4000 (peaks, p-values), with fixed random seeds.
  - Plot size 5 x 4 in; axis labels.
