NPQ kinetics - FluorCam

Script
  script_npq.R   Figures 1E, 2A and S5. Author: Cecile Lecampion.

Input (tab-separated text)
  DATA - raw_data_npq2.txt: no header; columns time point (T0, T24h, T48h), line, measurement code
         (L1-L9 under actinic light, D1-D2 in the dark), then NPQ of each replicate plant.
  CODE - temps_mesure_NPQ.txt: columns code and secondes, mapping each measurement code to seconds.
  The FluorCam light protocol is fluorcam_protocol.p (see readme_fluorcam_protocol.txt).

Method
  Shapiro-Wilk test for each line x time group.
  If the data are normal: mean +/- CI (Rmisc::summarySE).
  Otherwise: median +/- bootstrapped 95% CI (rcompanion::groupwiseMedian, R = 5000).
  Summary tables and plots are produced for each time point.

Output
  Shapiro_<time>.txt, Summary_Normal_<time>.txt or Summary_NonNormal_<time>.txt,
  NPQ_plots.pdf (one page per time point, one facet per line).
  In the manuscript, Fig 1E shows the WT and OX:RSH3.1 facets of T0 overlaid, Fig 2A the six T0 facets,
  and Fig S5 the three time points.

Example data
  example_data/raw_data_npq2.txt, example_data/temps_mesure_NPQ.txt
  Copy the script into the example folder and run it there.

Line codes: WT1 = wild type, H1 = OX:RSH3.1, H1npq1 / H1npq4 = OX:RSH3.1 npq1 / npq4.

What you can change
  Settings at the top: DATA (NPQ file), CODE (code -> seconds file).

Fixed in the code (edit the code to change)
  - The NPQ file has no header and exactly four replicate columns (named rep1-rep4 by the script):
    time, line, code, rep1, rep2, rep3, rep4.
  - Line colour (chartreuse3); one facet per line; one page per time point.
  - Output file NPQ_plots.pdf, 10 x 8 in; significance level for the Shapiro-Wilk test 0.05;
    bootstrap 5000 resamples, 95% CI.
