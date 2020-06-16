!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   CALB   B32   ! create write display delete ! Monhist fit results for mySQL
!
!COL ATT-name FMT Min    Max   !Comments
   1 meanRFgoodtag  F   -2.       2. ! RF offset - all photons 
   2 sigmaRFgoodtag F    0.      20. ! Time resolution - best photon (RF)
   3 meanRFalltag   F   -2.       2. ! RF offset - all photons 
   4 sigmaRFalltag  F    0.      20. ! Time resolution - all photons (RF)
   5 meanRFprot     F   -2.       2. ! Mean of dt distribution for protons
   6 sigmaRFprot    F    0.      20. ! Time resolution for protons
   7 meanRFpip      F   -2.       2. ! Mean of dt distribution for protons
   8 sigmaRFpip     F    0.      20. ! Time resolution for positive pions
   9 meanRFpim      F   -2.       2. ! Mean of dt distribution for protons
  10 sigmaRFpim     F    0.      20. ! Time resolution for negative pions
  11 meanSTpip      F   -2.       2. ! Mean of dt distribution for protons
  12 sigmaSTpip     F    0.      20. ! Time resolution for positive pions
  13 meanSTpim      F   -2.       2. ! Mean of dt distribution for protons
  14 sigmaSTpim     F    0.      20. ! Time resolution for negative pions
  15 meanECt        F    0.      20. ! Mean of Time Distribution of EC, tEC(e)-tSC(e) 
  16 sigmaECt	    F    0.      20. ! Sigma of Time Distribution of EC, tEC(e)-tSC(e) 
  17 meanECb        F    0.      20. ! Mean of beta distribution for gammas
  18 sigmaECb	    F    0.      20. ! Sigma of beta distribution for gammas
  19 meanECm        F    0.      20. ! Mean of 2 photon invariant mass distribution
  20 sigmaECm	    F    0.      20. ! Mean of 2 photon invariant mass distribution
  21 xbeam  	    F   -5.       5. ! centroid in x of beam position from MVRT
  22 ybeam  	    F   -5.       5. ! centroid in y of beam position from MVRT
  23 sig_xbeam	    F    0.       5. ! sigma of x distribution from MVRT
  24 sig_ybeam	    F    0.       5. ! sigma of y distribution from MVRT
  25 mm_p_pip	    F	 0.	 20. ! pi- peak position from (p pi+) missing mass
  26 smm_p_pip      F	 0.	 20. ! sigma of the pi- peak from (p pi+) missing mass
  27 mm_pip_pim     F	 0.	 20. ! proton peak position from (pi+ pi-) missing mass
  28 smm_pip_pim    F	 0.	 20. ! sigma of the proton peak from (pi+ pi-) missing mass
  29 mm_kp_lambda   F	 0.	 20. ! Lambda peak position from the (K+) missing mass
  30 smm_kp_lambda  F	 0.	 20. ! sigma of the Lambda peak from the (K+) missing mass
  31 mm_kp_sigma    F	 0.	 20. ! Sigma peak position from the (K+) missing mass
  32 smm_kp_sigma   F	 0.	 20. ! sigma of the Sigma peak from the (K+) missing mass
  33 ResSL1_ave     F	 0.   10000. ! DC residuals in R1 (all sectors)
  34 ResSL2_ave     F	 0.   10000. ! DC residuals in R2 (all sectors)
  35 ResSL3_ave     F	 0.   10000. ! DC residuals in R3 (all sectors)
  36 ResSL4_ave     F	 0.   10000. ! DC residuals in R1 (all sectors)
  37 ResSL5_ave     F	 0.   10000. ! DC residuals in R2 (all sectors)
  38 ResSL6_ave     F	 0.   10000. ! DC residuals in R3 (all sectors)
  39 ResSL1_sig     F	 0.   10000. ! DC residuals in R1 (all sectors)
  40 ResSL2_sig     F	 0.   10000. ! DC residuals in R2 (all sectors)
  41 ResSL3_sig     F	 0.   10000. ! DC residuals in R3 (all sectors)
  42 ResSL4_sig     F	 0.   10000. ! DC residuals in R1 (all sectors)
  43 ResSL5_sig     F	 0.   10000. ! DC residuals in R2 (all sectors)
  44 ResSL6_sig     F	 0.   10000. ! DC residuals in R3 (all sectors)
  45 ResSL1S1_ave   F	 0.   10000. ! DC residuals in R1 (sector 1)
  46 ResSL1S2_ave   F	 0.   10000. ! DC residuals in R1 (sector 2)
  47 ResSL1S3_ave   F	 0.   10000. ! DC residuals in R1 (sector 3)
  48 ResSL1S4_ave   F	 0.   10000. ! DC residuals in R1 (sector 4)
  49 ResSL1S5_ave   F	 0.   10000. ! DC residuals in R1 (sector 5)
  50 ResSL1S6_ave   F	 0.   10000. ! DC residuals in R1 (sector 6)
  51 ResSL1S1_sig   F	 0.   10000. ! DC residuals in R1 (sector 1)
  52 ResSL1S2_sig   F	 0.   10000. ! DC residuals in R1 (sector 2)
  53 ResSL1S3_sig   F	 0.   10000. ! DC residuals in R1 (sector 3)
  54 ResSL1S4_sig   F	 0.   10000. ! DC residuals in R1 (sector 4)
  55 ResSL1S5_sig   F	 0.   10000. ! DC residuals in R1 (sector 5)
  56 ResSL1S6_sig   F	 0.   10000. ! DC residuals in R1 (sector 6)
  57 ResSL2S1_ave   F	 0.   10000. ! DC residuals in R2 (sector 1)
  58 ResSL2S2_ave   F	 0.   10000. ! DC residuals in R2 (sector 2)
  59 ResSL2S3_ave   F	 0.   10000. ! DC residuals in R2 (sector 3)
  60 ResSL2S4_ave   F	 0.   10000. ! DC residuals in R2 (sector 4)
  61 ResSL2S5_ave   F	 0.   10000. ! DC residuals in R2 (sector 5)
  62 ResSL2S6_ave   F	 0.   10000. ! DC residuals in R2 (sector 6)
  63 ResSL2S1_sig   F	 0.   10000. ! DC residuals in R2 (sector 1)
  64 ResSL2S2_sig   F	 0.   10000. ! DC residuals in R2 (sector 2)
  65 ResSL2S3_sig   F	 0.   10000. ! DC residuals in R2 (sector 3)
  66 ResSL2S4_sig   F	 0.   10000. ! DC residuals in R2 (sector 4)
  67 ResSL2S5_sig   F	 0.   10000. ! DC residuals in R2 (sector 5)
  68 ResSL2S6_sig   F	 0.   10000. ! DC residuals in R2 (sector 6)
  69 ResSL3S1_ave   F	 0.   10000. ! DC residuals in R3 (sector 1)
  70 ResSL3S2_ave   F	 0.   10000. ! DC residuals in R3 (sector 2)
  71 ResSL3S3_ave   F	 0.   10000. ! DC residuals in R3 (sector 3)
  72 ResSL3S4_ave   F	 0.   10000. ! DC residuals in R3 (sector 4)
  73 ResSL3S5_ave   F	 0.   10000. ! DC residuals in R3 (sector 5)
  74 ResSL3S6_ave   F	 0.   10000. ! DC residuals in R3 (sector 6)
  75 ResSL3S1_sig   F	 0.   10000. ! DC residuals in R3 (sector 1)
  76 ResSL3S2_sig   F	 0.   10000. ! DC residuals in R3 (sector 2)
  77 ResSL3S3_sig   F	 0.   10000. ! DC residuals in R3 (sector 3)
  78 ResSL3S4_sig   F	 0.   10000. ! DC residuals in R3 (sector 4)
  79 ResSL3S5_sig   F	 0.   10000. ! DC residuals in R3 (sector 5)
  80 ResSL3S6_sig   F	 0.   10000. ! DC residuals in R3 (sector 6)
  81 ResSL4S1_ave   F	 0.   10000. ! DC residuals in R4 (sector 1)
  82 ResSL4S2_ave   F	 0.   10000. ! DC residuals in R4 (sector 2)
  83 ResSL4S3_ave   F	 0.   10000. ! DC residuals in R4 (sector 3)
  84 ResSL4S4_ave   F	 0.   10000. ! DC residuals in R4 (sector 4)
  85 ResSL4S5_ave   F	 0.   10000. ! DC residuals in R4 (sector 5)
  86 ResSL4S6_ave   F	 0.   10000. ! DC residuals in R4 (sector 6)
  87 ResSL4S1_sig   F	 0.   10000. ! DC residuals in R4 (sector 1)
  88 ResSL4S2_sig   F	 0.   10000. ! DC residuals in R4 (sector 2)
  89 ResSL4S3_sig   F	 0.   10000. ! DC residuals in R4 (sector 3)
  90 ResSL4S4_sig   F	 0.   10000. ! DC residuals in R4 (sector 4)
  91 ResSL4S5_sig   F	 0.   10000. ! DC residuals in R4 (sector 5)
  92 ResSL4S6_sig   F	 0.   10000. ! DC residuals in R4 (sector 6)
  93 ResSL5S1_ave   F	 0.   10000. ! DC residuals in R5 (sector 1)
  94 ResSL5S2_ave   F	 0.   10000. ! DC residuals in R5 (sector 2)
  95 ResSL5S3_ave   F	 0.   10000. ! DC residuals in R5 (sector 3)
  96 ResSL5S4_ave   F	 0.   10000. ! DC residuals in R5 (sector 4)
  97 ResSL5S5_ave   F	 0.   10000. ! DC residuals in R5 (sector 5)
  98 ResSL5S6_ave   F	 0.   10000. ! DC residuals in R5 (sector 6)
  99 ResSL5S1_sig   F	 0.   10000. ! DC residuals in R5 (sector 1)
 100 ResSL5S2_sig   F	 0.   10000. ! DC residuals in R5 (sector 2)
 101 ResSL5S3_sig   F	 0.   10000. ! DC residuals in R5 (sector 3)
 102 ResSL5S4_sig   F	 0.   10000. ! DC residuals in R5 (sector 4)
 103 ResSL5S5_sig   F	 0.   10000. ! DC residuals in R5 (sector 5)
 104 ResSL5S6_sig   F	 0.   10000. ! DC residuals in R5 (sector 6)
 105 ResSL6S1_ave   F	 0.   10000. ! DC residuals in R6 (sector 1)
 106 ResSL6S2_ave   F	 0.   10000. ! DC residuals in R6 (sector 2)
 107 ResSL6S3_ave   F	 0.   10000. ! DC residuals in R6 (sector 3)
 108 ResSL6S4_ave   F	 0.   10000. ! DC residuals in R6 (sector 4)
 109 ResSL6S5_ave   F	 0.   10000. ! DC residuals in R6 (sector 5)
 110 ResSL6S6_ave   F	 0.   10000. ! DC residuals in R6 (sector 6)
 111 ResSL6S1_sig   F	 0.   10000. ! DC residuals in R6 (sector 1)
 112 ResSL6S2_sig   F	 0.   10000. ! DC residuals in R6 (sector 2)
 113 ResSL6S3_sig   F    0.   10000. ! DC residuals in R6 (sector 3)
 114 ResSL6S4_sig   F    0.   10000. ! DC residuals in R6 (sector 4)
 115 ResSL6S5_sig   F    0.   10000. ! DC residuals in R6 (sector 5)
 116 ResSL6S6_sig   F    0.   10000. ! DC residuals in R6 (sector 6)
!
 END TABLE
