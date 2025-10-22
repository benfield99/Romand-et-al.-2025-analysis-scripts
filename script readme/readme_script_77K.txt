77 K Chlorophyll Spectra Analysis Script

This script processes raw chlorophyll fluorescence spectra from wild type (col) and mutant (h1 (OXRSH3.1), g8 (OXRSH3.2)) plants measured at 77 K.


What it does

Read raw CSVs: imports spectra, cleans and formats them.

Smooth & normalize: applies Savitzky–Golay filtering and normalizes each trace to the PSII peak (650–700 nm).

Replicate handling: collapses 6 technical replicates into a median trace per biological replicate.

Model fitting: uses quantile GAM (qGAM, τ = 0.5) to fit smooth median spectra with 95% confidence intervals.

Difference analysis: computes mutant – wild type difference curves, identifies significant wavelength ranges, and estimates the peak wavelength and amplitude of the difference.

Statistics: reports global p-values that account for testing across the wavelength band.

Outputs

Figures: full spectra, zoomed PSII region (660–685 nm) (or entire spectra), and difference spectra.

Tables (CSV): peak estimates, significant ranges, global p-values, and a combined summary table.

Notes

Default analysis range = 660–685 nm (antenna band), but can be changed to full spectra.

Line colors: col = black/grey, h1 = dark green, g8 = light green.