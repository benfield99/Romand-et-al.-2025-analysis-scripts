README — PSI FluorCam NPQ Protocol

File: PSI_NPQ_protocol.txt
Author: Stefano Caffarri (Dec 2016)
Instrument: PSI FluorCam

This protocol defines the light sequence used to measure non-photochemical quenching (NPQ) with the PSI FluorCam. It includes:

A dark-adapted Fo and Fm measurement,

An actinic light phase with nine time points (L1–L9, 15–300 s), each followed by a saturating pulse,

A dark-recovery phase with two additional pulses (D1 and D2).

The protocol controls timing, light intensity, and checkpoint labels used for data export.
It was used to acquire the fluorescence data analyzed by the accompanying R script (script_npq.R).

To use, load the file in the PSI FluorCam software under Protocol → Open, then run measurements as normal.