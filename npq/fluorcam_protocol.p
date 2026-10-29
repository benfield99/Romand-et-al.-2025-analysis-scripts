;NPQ protocol
; version Dec 2016 stefano Caffarri
TS=20ms
include default.inc  ;Includes standard options, do not remove it !
include light.inc  ;Includes standard options, do not remove it !
Shutter=2
Sensitivity=50
Act1=0
Act2=100
Super=100
FAR=0
;-------------------------------------------------------------------------------------------------------------
;*** Fo Measurement ******************************************************************************************
;-------------------------------------------------------------------------------------------------------------
F0duration = 5s
F0period = 1s
<0,F0period..F0duration>=>mfmsub  
;
;Fo definition
<0s>=>checkPoint,"startFo"
<F0duration - F0period>=>checkPoint,"endFo"
;
;-------------------------------------------------------------------------------------------------------------
;*** Saturating Pulse & Fm Measurement ***********************************************************************
;-------------------------------------------------------------------------------------------------------------
PulseDuration=800ms;#####
a1=F0duration + 2*mfmsub_length  ;#####
;
<a1>=>SatPulse(PulseDuration)  
;
<a1>=>mpulse  
;
;Fm definition
<a1 + 6*TS>=>checkPoint,"startFm"
<a1 + PulseDuration - mfmsub_length>=>checkPoint,"endFm"  
;
;Visual frame definition                                               ## Image shown in pre-processing window
<a1 + TS>=>checkPoint,"timeVisual"
;
;-------------------------------------------------------------------------------------------------------------
;******* Dark Relaxation *************************************************************************
;-------------------------------------------------------------------------------------------------------------
DarkRelaxation1=1s;#####
;
;-------------------------------------------------------------------------------------------------------------
;******* NPQ *******************************************
;-------------------------------------------------------------------------------------------------------------
ALperiod=301s; use a time corresponding to the last Light point
L1=15s
L2=30s
L3=45s
L4=60s
L5=90s
L6=120s
L7=150s
L8=200s
L9=300s
D1=330s
D2=420s
;D3=390s; to add this point to the protocol, delete ";" here and in the corresponding code below
;D4=450s; to add this point to the protocol, delete ";" here and in the corresponding code below
;
c1= a1 + PulseDuration + DarkRelaxation1 + mfmsub_length; #####
<c1>=>act2(ALperiod); ##AL on
;
;******* Saturating Pulses - Fm_L1 ***********************************
sp1=c1+<L1>;#####
sp11=sp1#<mfmsub_length,2*mfmsub_length ..PulseDuration - mfmsub_length>;#####
sp11=>mfmsub
sp1 + mfmsub_length=>checkPoint,"startFt_L1"
sp1 + PulseDuration - mfmsub_length=>checkPoint,"endFt_L1"
;
sp2=sp1 + PulseDuration;#####
sp2=>SatPulse(PulseDuration);
sp2=>mpulse
sp2 + 6*TS =>checkPoint,"startFm_L1"
sp2 + PulseDuration - mfmsub_length=>checkPoint,"endFm_L1"
;
;******* Saturating Pulses - Fm_L2 **********************************
sp3=c1+<L2>;#####
sp31=sp3#<mfmsub_length,2*mfmsub_length ..PulseDuration - mfmsub_length>;#####
sp31=>mfmsub
sp3 + mfmsub_length=>checkPoint,"startFt_L2"
sp3 + PulseDuration - mfmsub_length=>checkPoint,"endFt_L2"
;
sp4=sp3 + PulseDuration;#####
sp4=>SatPulse(PulseDuration);
sp4=>mpulse
sp4 + 6*TS =>checkPoint,"startFm_L2"
sp4 + PulseDuration - mfmsub_length=>checkPoint,"endFm_L2"
;
;******* Saturating Pulses - Fm_L3 ***********************************
sp5=c1+<L3>;#####
sp51=sp5#<mfmsub_length,2*mfmsub_length ..PulseDuration - mfmsub_length>;#####
sp51=>mfmsub
sp5 + mfmsub_length=>checkPoint,"startFt_L3"
sp5 + PulseDuration - mfmsub_length=>checkPoint,"endFt_L3"
;
sp6=sp5 + PulseDuration;#####
sp6=>SatPulse(PulseDuration);
sp6=>mpulse
sp6 + 6*TS =>checkPoint,"startFm_L3"
sp6 + PulseDuration - mfmsub_length=>checkPoint,"endFm_L3"
;
;******* Saturating Pulses - Fm_L4 *******************************
sp7=c1+<L4>;#####
sp71=sp7#<mfmsub_length,2*mfmsub_length ..PulseDuration - mfmsub_length>;#####
sp71=>mfmsub
sp7 + mfmsub_length=>checkPoint,"startFt_L4"
sp7 + PulseDuration - mfmsub_length=>checkPoint,"endFt_L4"
;
sp8=sp7 + PulseDuration;#####
sp8=>SatPulse(PulseDuration);
sp8=>mpulse
sp8 + 6*TS =>checkPoint,"startFm_L4"
sp8 + PulseDuration - mfmsub_length=>checkPoint,"endFm_L4"
;
;******* Saturating Pulses - Fm_L5 *****************************
sp9=c1+<L5>;#####
sp91=sp9#<mfmsub_length,2*mfmsub_length ..PulseDuration - mfmsub_length>;#####
sp91=>mfmsub
sp9 + mfmsub_length=>checkPoint,"startFt_L5"
sp9 + PulseDuration - mfmsub_length=>checkPoint,"endFt_L5"
;
sp10=sp9 + PulseDuration;#####
sp10=>SatPulse(PulseDuration);
sp10=>mpulse
sp10 + 6*TS =>checkPoint,"startFm_L5"
sp10 + PulseDuration - mfmsub_length=>checkPoint,"endFm_L5"
;
;******* Saturating Pulses - Fm_L6 ***************************
sp011=c1+<L6>;#####
sp0111=sp011#<mfmsub_length,2*mfmsub_length ..PulseDuration - mfmsub_length>;#####
sp0111=>mfmsub
sp011 + mfmsub_length=>checkPoint,"startFt_L6"
sp011 + PulseDuration - mfmsub_length=>checkPoint,"endFt_L6"
;
sp012=sp011 + PulseDuration;#####
sp012=>SatPulse(PulseDuration);
sp012=>mpulse
sp012 + 6*TS =>checkPoint,"startFm_L6"
sp012 + PulseDuration - mfmsub_length=>checkPoint,"endFm_L6"
;
;******* Saturating Pulses - Fm_L7 ***************************
sp013=c1+<L7>;#####
sp0131=sp013#<mfmsub_length,2*mfmsub_length ..PulseDuration - mfmsub_length>;#####
sp0131=>mfmsub
sp013 + mfmsub_length=>checkPoint,"startFt_L7"
sp013 + PulseDuration - mfmsub_length=>checkPoint,"endFt_L7"
;
sp014=sp013 + PulseDuration;#####
sp014=>SatPulse(PulseDuration);
sp014=>mpulse
sp014 + 6*TS =>checkPoint,"startFm_L7"
sp014 + PulseDuration - mfmsub_length=>checkPoint,"endFm_L7"
;
;******* Saturating Pulses - Fm_L8 ***************************
sp015=c1+<L8>;#####
sp0151=sp015#<mfmsub_length,2*mfmsub_length ..PulseDuration - mfmsub_length>;#####
sp0151=>mfmsub
sp015 + mfmsub_length=>checkPoint,"startFt_L8"
sp015 + PulseDuration - mfmsub_length=>checkPoint,"endFt_L8"
;
sp016=sp015 + PulseDuration;#####
sp016=>SatPulse(PulseDuration);
sp016=>mpulse
sp016 + 6*TS =>checkPoint,"startFm_L8"
sp016 + PulseDuration - mfmsub_length=>checkPoint,"endFm_L8"
;
;******* Saturating Pulses - Fm_L9 ***************************
sp017=c1+<L9>;#####
sp0171=sp017#<mfmsub_length,2*mfmsub_length ..PulseDuration - mfmsub_length>;#####
sp0171=>mfmsub
sp017 + mfmsub_length=>checkPoint,"startFt_L9"
sp017 + PulseDuration - mfmsub_length=>checkPoint,"endFt_L9"
;
sp018=sp017 + PulseDuration;#####
sp018=>SatPulse(PulseDuration);
sp018=>mpulse
sp018 + 6*TS =>checkPoint,"startFm_L9"
sp018 + PulseDuration - mfmsub_length=>checkPoint,"endFm_L9"
;
;-------------------------------------------------------------------------------------------------------------
;*** Fo' Measurement ******************************************************************************************
;-------------------------------------------------------------------------------------------------------------
;endAL= a1 + PulseDuration + DarkRelaxation1 + mfmsub_length + ALperiod + 1s
;FRtime=5s
;<endAL>=>FAR(FRtime)
;endLight=endAL + FRtime + 1s
;<endLight, endLight + F0period .. endLight + F0duration>=>mfmsub  
;Fo_L1 definition
;<endLight + F0duration>=>checkPoint,"startFo_L1"
;<endLight + F0duration - F0period>=>checkPoint,"endFo_L1"
;
;******* Saturating Pulses - Fm_D1 ***************************
sp019=c1+<D1>;#####
sp0191=sp019#<mfmsub_length,2*mfmsub_length ..PulseDuration - mfmsub_length>;#####
sp0191=>mfmsub
sp019 + mfmsub_length=>checkPoint,"startFt_D1"
sp019 + PulseDuration - mfmsub_length=>checkPoint,"endFt_D1"
;
sp020=sp019 + PulseDuration;#####
sp020=>SatPulse(PulseDuration);
sp020=>mpulse
sp020 + 6*TS =>checkPoint,"startFm_D1"
sp020 + PulseDuration - mfmsub_length=>checkPoint,"endFm_D1"
;
;******* Saturating Pulses - Fm_D2 ***************************
sp021=c1+<D2>;#####
sp0211=sp021#<mfmsub_length,2*mfmsub_length ..PulseDuration - mfmsub_length>;#####
sp0211=>mfmsub
sp021 + mfmsub_length=>checkPoint,"startFt_D2"
sp021 + PulseDuration - mfmsub_length=>checkPoint,"endFt_D2"
;
sp022=sp021 + PulseDuration;#####
sp022=>SatPulse(PulseDuration);
sp022=>mpulse
sp022 + 6*TS =>checkPoint,"startFm_D2"
sp022 + PulseDuration - mfmsub_length=>checkPoint,"endFm_D2"
;
;******* Saturating Pulses - Fm_D3 ***************************
;sp023=c1+<D3>;#####
;sp0231=sp023#<mfmsub_length,2*mfmsub_length ..PulseDuration - mfmsub_length>;#####
;sp0231=>mfmsub
;sp023 + mfmsub_length=>checkPoint,"startFt_D3"
;sp023 + PulseDuration - mfmsub_length=>checkPoint,"endFt_D3"
;
;sp024=sp023 + PulseDuration;#####
;sp024=>SatPulse(PulseDuration);
;sp024=>mpulse
;sp024+ 6*TS =>checkPoint,"startFm_D3"
;sp024 + PulseDuration - mfmsub_length=>checkPoint,"endFm_D3"
;
;******* Saturating Pulses - Fm_D4 ***************************
;sp025=c1+<D4>;#####
;sp0251=sp025#<mfmsub_length,2*mfmsub_length ..PulseDuration - mfmsub_length>;#####
;sp0251=>mfmsub
;sp025 + mfmsub_length=>checkPoint,"startFt_D4"
;sp025 + PulseDuration - mfmsub_length=>checkPoint,"endFt_D4"
;
;sp026=sp025 + PulseDuration;#####
;sp026=>SatPulse(PulseDuration);
;sp026=>mpulse
;sp026 + 6*TS =>checkPoint,"startFm_D4"
;sp026 + PulseDuration - mfmsub_length=>checkPoint,"endFm_D4"
;
;END ******************************************************************