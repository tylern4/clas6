!********************************************************************
!
!                   CLAS detector BOS banks
!                   =======================
!
!********************************************************************
!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   BEAM   B32   ! create write display delete ! BEAM bank for mySQL
!
!COL ATT-name FMT Min    Max   !Comments
   1 ENERGY    F  0.   99999.  ! Electron beam energy in GeV
   2 ITORUS    F  0.   99999.  ! Torus magnet current in Amps
   3 IMINI     F  0.   99999.  ! Mini-torus magnet current in Amps
   4 ITAG      F  0.   99999.  ! Tagger magnet current in Amps
!
 END TABLE
!
!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  BMPR  ! create write display delete ! BEAM parameters bank
!
! 
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  Q_TOT         F      0.     1.E+35	! Total Charge (Coulomb)
  2  Q_TOT_LV      F      0.     1.E+35	! Total charge in Live Time (Coulomb) 
  3  TL_TU         F      0.     1.E+35 ! ( Time Live ) / ( Time Ungated )
  4  CURRENT       F      0.     1.E+35	! Current in Ampers
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  BREP          ! create write display delete ! Begin run epics values
!
!   ATTributes:
!   -----------
!COL ATT-name                 FMT       Min    Max     ! Comments
!
  1  MBSY2C_energy    F    -999999.0 999999.0 ! beam energy
  2  IGT0I00BIASET    F    -999999.0 999999.0 ! thermionic gun
  3  IGL1I00DAC2      F    -999999.0 999999.0 ! B polarized gun
  4  SMRPOSA          F    -999999.0 999999.0 ! A slit position
  5  SMRPOSB          F    -999999.0 999999.0 ! B slit position
  6  SMRPOSC          F    -999999.0 999999.0 ! C slit position
  7  Harp             F    -999999.0 999999.0 ! harp
  8  hallb_sf_xy560   F    -999999.0 999999.0 ! torus current
  9  MTSETI           F    -999999.0 999999.0 ! Mini set current
 10  MTIRBCK          F    -999999.0 999999.0 ! Mini current readback
 11  MTVRBCK          F    -999999.0 999999.0 ! Mini voltage readback
 12  TMSETI           F    -999999.0 999999.0 ! Tagger set current
 13  TMIRBCK          F    -999999.0 999999.0 ! Tagger current readback
 14  TMVRBCK          F    -999999.0 999999.0 ! Tagger voltage readback
 15  Cryo_pressure    F    -999999.0 999999.0 ! cryotarget pressure
 16  Cryo_temperature F    -999999.0 999999.0 ! cryotarget temperature
 17  Cryo_status      F    -999999.0 999999.0 ! cryotarget status
 18  VCG2C24          F    -999999.0 999999.0 ! upstream beam vacuum
 19  VCG2H01          F    -999999.0 999999.0 ! target vacuum
 20  scalerS2o        F    -999999.0 999999.0 ! Halo UP upstream
 21  scalerS3o        F    -999999.0 999999.0 ! Halo DOWN upstream
 22  scalerS4o        F    -999999.0 999999.0 ! Halo LEFT upstream
 23  scalerS5o        F    -999999.0 999999.0 ! Halo RIGHT upstream
 24  scalerS6o        F    -999999.0 999999.0 ! Halo UP downstream
 25  scalerS7o        F    -999999.0 999999.0 ! Halo DOWN downstream
 26  scalerS8o        F    -999999.0 999999.0 ! Halo LEFT downstream
 27  scalerS9o        F    -999999.0 999999.0 ! Halo RIGHT downstream
 28  IPM2H01_XPOS     F    -999999.0 999999.0 ! bpm 1 x
 29  IPM2H01_YPOS     F    -999999.0 999999.0 ! bpm 1 y
 30  IPM2H01          F    -999999.0 999999.0 ! bpm 1 current
 31  IPM2C24A_XPOS    F    -999999.0 999999.0 ! bpm 2 x
 32  IPM2C24A_YPOS    F    -999999.0 999999.0 ! bpm 2 y
 33  IPM2C24A         F    -999999.0 999999.0 ! bpm 2 current
 34  IPM2C22A_XPOS    F    -999999.0 999999.0 ! bpm 3 x
 35  IPM2C22A_YPOS    F    -999999.0 999999.0 ! bpm 3 y
 36  IPM2C22A         F    -999999.0 999999.0 ! bpm 3 current
!
 END TABLE!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CALL      B16   ! create write display delete ! Catch-ALL event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I     1      10  ! catch-all element(RF,laser diode,etc)
  2  TDC      I     0   65536  ! tdc information (channels)
  3  ADC      I     0   65536  ! adc information (channels)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CC01     ! create write display delete ! Cerenkov Counter hits bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  id       I     0    36     ! the address of the hit detector element
  2  time     F     0   100000  ! time(ns) 
  3  n_pe     F     0   100000  ! number of photoelectrons
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CC1      B16   ! create write display delete ! Large angle Cerenkov counters event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I     1      36  ! the address of the hit detector element
  2  TDC      I     0  100000  ! tdc information (channels)
  3  ADC      I     0  100000  ! adc information (channels)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CC       B16 ! create write display delete ! Forward Cerenkov counters event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I     1      36  ! the address of the hit detector element
  2  TDC      I     0  100000  ! tdc information (channels)
  3  ADC      I     0  100000  ! adc information (channels)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CCDI       ! create write display delete ! CC channel discriminator thresholds
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1   crate       I     0  10      ! CAMAC crate number
  1   slot        I     0  30      ! slot
  1   mask        I     0  999999  ! mask
  1   threshold   I     0  100000  ! actual threshold value (mV)
  1   width       I     0  100000  ! actual width value
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CCH          ! create  display delete ! GSIM Cerenkov hit info
!
!   ATTributes:
!   -----------
!COL ATT-name   FMT Min    Max          ! Comments
!    
  1  tknum       i  0      0xFFFF     ! track number, 1000*istak+itra
  2  id          i  -4000  4000       ! track PDG id
  3  nhits       i  0      400        ! number of CC hits per track
  4  sector      i  1      6          ! sector number of track
  5  segment     i  1      18         ! cc segment number of track
  6  pmom        i  0.     20.        ! track momentum 
  7  xin         F  -1000. 1000.      ! x pos track entry into cerenkov
  8  yin         F  -1000. 1000.      ! y pos
  9  zin         F  -1000. 1000.      ! z pos
 10  xout        F  -1000. 1000.      ! x pos track exit from cerenkov
 11  yout        F  -1000. 1000.      ! y pos
 12  zout        F  -1000. 1000.      ! z pos
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CCMT        ! create write display delete ! Mean CC pretrigger thresholds
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1   mean_high    I     0  100000  ! mean high threshold (mV)
  2   mean_lo      I     0  100000  ! mean lo threshold (mV)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CCPB    ! create write display delete ! CC hits involved in the event
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  ScSgHt   I     0   700      ! 100*sector + Cluster # in CCRC
  2  Nphe     F     0. 1000.     ! Number of photo-electrons*10
  3  Time     F    -5.  150.     ! Flight time relative to the evnt start time
  4  Path     F     0.  600.     ! Path lenght from target (from tracking)
  5  Chi2CC   F     0.   10.     ! Geometrical matching: angle between CC hit and
!                                  nearest SC hit (in rad)
  6  Status   I     0   0xFFFF   ! Status word - now 10*(CC segment number) 
!                                ! + 1000*( 1 + phy_index).
!                                ! PHY_INDEX = -1 : left  PMT (1,3,5...35)
!                                ! PHY_INDEX = +1 : right PMT (2,4,6...36)
!            ! PHY_INDEX =  0 : both left and right (electr. near midplane)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CCPE     ! create write display delete ! Translated CC pedestal bank, measured during the pedestal run.
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID        I     1      36  ! the address of the hit detector element
  2  mean      I     0  100000  ! adc pedestal mean value (channel)
  3  sigma     I     0  100000  ! sigma of the pedestal distribution (channel)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CCRC           ! create write display delete ! Cerenkov reconstruction bank
!
!   ATTributes:
!   -----------
!COL ATT-name   FMT Min    Max          ! Comments
!    
  1  nrsect      I  1      6      ! Sector #
  2  nrsegm      I  0      180    ! 10 * Mean segment #
  3  nrsegm_p    I  0      180    ! 10 * Max segment # in the cluster
  4  nrsegm_m    I  0      180    ! 10 * Min segment # in the cluster
  5  nrphe       I  0      100    ! Number of photoelectrons obtained
  6  nrtime      I  0      1000   ! TOF in channels (50psec/channel)
  7  nrthet      I  0      180    ! Estimated angle Theta
  8  nrdthet     I  0      180    ! Estimated error of angle Theta
  9  nrphy       I  -1     1      ! Phy index 
!            ( -1 if Phy < 0 ; 1 if Phy > 0 ; 0 if Phy ~ 0)
 10   nriec      I  -500   500    ! Estimated i-coordinate in EC 
 11   nrdiec     I  0      500    ! Estimated error of i-coord. in EC
 12   nrstat     I  -10    10     ! Status  word (yet unclear) 
!
!  All distances in cm , all angles in degrees
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CCS     ! create write display delete ! Cerenkov scaler bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1   CCS1      I     0    9999999  ! scaler sector 1
  2   CCS2      I     0    9999999  ! scaler sector 1
  3   CCS3      I     0    9999999  ! scaler sector 1
  4   CCS4      I     0    9999999  ! scaler sector 1
  5   CCS5      I     0    9999999  ! scaler sector 1
  6   CCS6      I     0    9999999  ! scaler sector 1
  7   CCS7      I     0    9999999  ! scaler sector 1
  8   CCS8      I     0    9999999  ! scaler sector 1
  9   CCS9      I     0    9999999  ! scaler sector 1
  10  CCS10     I     0    9999999  ! scaler sector 1
  11  CCS11     I     0    9999999  ! scaler sector 1
  12  CCS12     I     0    9999999  ! scaler sector 1
  13  CCS13     I     0    9999999  ! scaler sector 1
  14  CCS14     I     0    9999999  ! scaler sector 1
  15  CCS15     I     0    9999999  ! scaler sector 1
  16  CCS16     I     0    9999999  ! scaler sector 1
  17  CCS17     I     0    9999999  ! scaler sector 2
  18  CCS18     I     0    9999999  ! scaler sector 2
  19  CCS19     I     0    9999999  ! scaler sector 2
  20  CCS20     I     0    9999999  ! scaler sector 2
  21  CCS21     I     0    9999999  ! scaler sector 2
  22  CCS22     I     0    9999999  ! scaler sector 2
  23  CCS23     I     0    9999999  ! scaler sector 2
  24  CCS24     I     0    9999999  ! scaler sector 2
  25  CCS25     I     0    9999999  ! scaler sector 2
  26  CCS26     I     0    9999999  ! scaler sector 2
  27  CCS27     I     0    9999999  ! scaler sector 2
  28  CCS28     I     0    9999999  ! scaler sector 2
  29  CCS29     I     0    9999999  ! scaler sector 2
  30  CCS30     I     0    9999999  ! scaler sector 2
  31  CCS31     I     0    9999999  ! scaler sector 2
  32  CCS32     I     0    9999999  ! scaler sector 2
  33  CCS33     I     0    9999999  ! scaler sector 3
  34  CCS34     I     0    9999999  ! scaler sector 3
  35  CCS35     I     0    9999999  ! scaler sector 3
  36  CCS36     I     0    9999999  ! scaler sector 3
  37  CCS37     I     0    9999999  ! scaler sector 3
  38  CCS38     I     0    9999999  ! scaler sector 3
  39  CCS39     I     0    9999999  ! scaler sector 3
  40  CCS40     I     0    9999999  ! scaler sector 3
  41  CCS41     I     0    9999999  ! scaler sector 3
  42  CCS42     I     0    9999999  ! scaler sector 3
  43  CCS43     I     0    9999999  ! scaler sector 3
  44  CCS44     I     0    9999999  ! scaler sector 3
  45  CCS45     I     0    9999999  ! scaler sector 3
  46  CCS46     I     0    9999999  ! scaler sector 3
  47  CCS47     I     0    9999999  ! scaler sector 3
  48  CCS48     I     0    9999999  ! scaler sector 3
  49  CCS49     I     0    9999999  ! scaler sector 4
  50  CCS50     I     0    9999999  ! scaler sector 4
  51  CCS51     I     0    9999999  ! scaler sector 4
  52  CCS52     I     0    9999999  ! scaler sector 4
  53  CCS53     I     0    9999999  ! scaler sector 4
  54  CCS54     I     0    9999999  ! scaler sector 4
  55  CCS55     I     0    9999999  ! scaler sector 4
  56  CCS56     I     0    9999999  ! scaler sector 4
  57  CCS57     I     0    9999999  ! scaler sector 4
  58  CCS58     I     0    9999999  ! scaler sector 4
  59  CCS59     I     0    9999999  ! scaler sector 4
  60  CCS60     I     0    9999999  ! scaler sector 4
  61  CCS61     I     0    9999999  ! scaler sector 4
  62  CCS62     I     0    9999999  ! scaler sector 4
  63  CCS63     I     0    9999999  ! scaler sector 4
  64  CCS64     I     0    9999999  ! scaler sector 4
  65  CCS65     I     0    9999999  ! scaler sector 5
  66  CCS66     I     0    9999999  ! scaler sector 5
  67  CCS67     I     0    9999999  ! scaler sector 5
  68  CCS68     I     0    9999999  ! scaler sector 5
  69  CCS69     I     0    9999999  ! scaler sector 5
  70  CCS70     I     0    9999999  ! scaler sector 5
  71  CCS71     I     0    9999999  ! scaler sector 5
  72  CCS72     I     0    9999999  ! scaler sector 5
  73  CCS73     I     0    9999999  ! scaler sector 5
  74  CCS74     I     0    9999999  ! scaler sector 5
  75  CCS75     I     0    9999999  ! scaler sector 5
  76  CCS76     I     0    9999999  ! scaler sector 5
  77  CCS77     I     0    9999999  ! scaler sector 5
  78  CCS78     I     0    9999999  ! scaler sector 5
  79  CCS79     I     0    9999999  ! scaler sector 5
  80  CCS80     I     0    9999999  ! scaler sector 5
  81  CCS81     I     0    9999999  ! scaler sector 6
  82  CCS82     I     0    9999999  ! scaler sector 6
  83  CCS83     I     0    9999999  ! scaler sector 6
  84  CCS84     I     0    9999999  ! scaler sector 6
  85  CCS85     I     0    9999999  ! scaler sector 6
  86  CCS86     I     0    9999999  ! scaler sector 6
  87  CCS87     I     0    9999999  ! scaler sector 6
  88  CCS88     I     0    9999999  ! scaler sector 6
  89  CCS89     I     0    9999999  ! scaler sector 6
  90  CCS90     I     0    9999999  ! scaler sector 6
  91  CCS91     I     0    9999999  ! scaler sector 6
  92  CCS92     I     0    9999999  ! scaler sector 6
  93  CCS93     I     0    9999999  ! scaler sector 6
  94  CCS94     I     0    9999999  ! scaler sector 6
  95  CCS95     I     0    9999999  ! scaler sector 6
  96  CCS96     I     0    9999999  ! scaler sector 6
!
 END TABLE
!
!-----------------------------------------------------------------------
! Kinematic Fitter Information bank.
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CHI2          ! create write display delete !Kinematic Fitter Info Bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
1       chi2   F  0.0    1000.0   ! overall chi2 for event  
2       cl     F  0.0    1.0      ! percentage (confidence level)
3       ndf    I  0      100      ! number of effective degrees of freedom
4       iter   I  0      1000     | number of iterations to converge
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CL01    ! create write display delete ! Catch-ALL event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ac_amp       I     0   65536  ! 60 Hz AC amplitude (pedestal subtracted)
  2  fc_diode_amp I     0   65536  ! Forward Carriage diode amplitude (ped sub.)
  3  fc_diode_t   F     0   65536  ! Forward Carriage diode time
  4  nc_diode_amp I     0   65536  ! North Clamshell diode amplitude (ped sub.)
  5  nc_diode_t   F     0   65536  ! Forward  diode time
  6  sc_diode_amp I     0   65536  ! Forward Carriage diode amplitude (ped sub.)
  7  sc_diode_t   F     0   65536  ! Forward Carriage diode time(ns)
  8  sf_diode_amp I     0   65536  ! Forward Carriage diode amplitude (ped sub.)
  9  sf_diode_t   F     0   65536  ! Forward Carriage diode time(ns)
 10  rf1	  F     0   65536  ! RF time 1 (ns)
 11  rf2	  F     0   65536  ! RF time 2 (ns)
 12  rf	 	  F     0   65536  ! GOOD RF time (ns)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!
 TABLE  CLST  ! Cluster bank
!
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max    ! Comments
!
  1  Clust     I       0     65536  ! bit packed,  see: include/bosddl.h, clasCLST_t
! Bit packed data structure follows:
! typedef struct {
!  unsigned int layer    : 4;
!  unsigned int clustern : 4;
!  unsigned int csegment : 5;
!  unsigned int cclust   : 4;
!  unsigned int clustp   : 15;
!} clst_t;
!
!typedef struct {
!  bankHeader_t bank;
!  clst_t clst[1];  
!} clasCLST_t;
!
!     
!
 END TABLE
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CPED       ! create write display delete ! Untranslated pedestal bank, measured during the pedestal run.
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1   slot        I     0  30      ! ADC slot
  2   channel     I     0  70      ! ADC channel
  3   mean        I     0  100000  ! adc pedestal mean value (channel)
  4   sigma       F     0  100000  ! sigma of the pedestal distribution (channel)
  5   offset      I     0  100000  ! offset for sparsification threshold calculation
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   CSQL   B32   ! create write display delete ! Data bank for mySQL
!
!COL ATT-name FMT Min    Max   !Comments
   1 EVID     I    1    100000 ! Event ID (number of triggers)
   2 NPROC    I    1    100000 ! Number of processed triggers
   3 CPU      F    0.    99999.! CPU used (sec) 
   4 FC       F    0.      999.! Faraday Cup (K)
   5 FCG      F    0.      999.! Faraday Cup Gated (K)
   6 TG       F    0.      999.! Clock Gated
   7 IBEAM    F    0.      999.! Beam current 
   8 NpipS1   I    0    100000 !  Number of pi+ in sect 1
   9 NpipS2   I    0    100000 !  Number of pi+ in sect 2
  10 NpipS3   I    0    100000 !  Number of pi+ in sect 3 
  11 NpipS4   I    0    100000 !  Number of pi+ in sect 4 
  12 NpipS5   I    0    100000 !  Number of pi+ in sect 5  
  13 NpipS6   I    0    100000 !  Number of pi+ in sect 6
  14 Nhb      I    0   1000000 ! Number of HB 
  15 Ntb      I    0   1000000 ! Number of TB
  16 Nprot    I    0   1000000 ! Number of protons
  17 Npip     I    0   1000000 ! number of pip
  18 Ndeut    I    0   1000000 ! number of deutrons
  19 Nphot    I    0   1000000 ! number of photons	
  20 Nepiphp  I    0   1000000 ! Number of pi+ at pos. Helic.
  21 Npiphn   I    0   1000000 ! Number of pi+ at neg. helic.
  22 Ngamma   I    0   1000000 ! Number of photons in the tagger from scaler
  23 Ng_norm  I    0   1000000 ! Number of photons from gflux
!
 END TABLE
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DC0       B16  ! create write display delete ! Drift chamber event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I   257    9408  ! the address of the hit detector element
  2  TDC      I     0   65536  ! tdc information (channels)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DC1     ! create write display delete ! Drift chamber hits bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I   257    9408  ! the address of the hit detector element
  2  time     F     0   65536  ! time(ns) 
!
 END TABLE
!
!-------------------------------------------------------------------------------
 TABLE  DCDW  ! create   delete ! Drift Chamber Dead Wire bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID         I    257 9408   ! ID_wire 
  2  TIDLY      F  -500. 500.   ! TIme DeLaY (ns) 		 
  3  STAT       I      0   10   ! wire status word 
!
 END TABLE
!
!-------------------------------------------------------------------------------
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DCGM       ! create display delete ! DC geometry info by layer - in sector coordinate system
!
!   ATTributes:
!   -----------
!COL ATT-name   FMT   Min    Max             ! Comments
  1  x_curve     F  -1000.0  1000.0          ! x center of curvature (cm)
  2  y_curve     F  -1000.0  1000.0          ! y center of curvature (cm)
  3  z_curve     F  -1000.0  1000.0          ! z center of curvature (cm)
  4  r_curve     F       0.0  1000.0         ! radius of curvature (cm)
  5  theta_start F  -1000.0  1000.0          ! angle of first logical wire WRT the center of curvature (rad)
  6  d_theta     F  -1000.0  1000.0          ! delta theta between wires WRT center of curvature
  7  x_nmid      F  -1000.0  1000.0          ! x normal vector to the midplane
  8  y_nmid      F  -1000.0  1000.0          ! y normal vector to the midplane
  9  z_nmid      F  -1000.0  1000.0          ! z normal vector to the midplane
 10  theta_min   F  -1000.0  1000.0          ! theta of first physical wire (rad)
 11  theta_max   F  -1000.0  1000.0          ! theta of last physical wire (rad)
 12  min_wire    I    -1000  1000            ! minimum physical wire number
 13  max_wire    I    -1000  1000            ! maximum physical wire number
 14  stereo      F  -1000.0  1000.0          ! approximate stereo angle
 15  cell_size   F  -1000.0  1000.0          ! cell size (cm)
 16  x_norm      F  -1000.0  1000.0          ! x normal vector to the plane(region 1)
 17  y_norm      F  -1000.0  1000.0          ! y normal vector to the plane(region 1)
 18  z_norm      F  -1000.0  1000.0          ! z normal vector to the plane(region 1)
 19  p_dist      F  -1000.0  1000.0          ! distance of plane to origin(cm) (region 1)
 20  p_sep       F  -1000.0  1000.0          ! planar separation(cm) (region 1)
 21  max_cylw    I    -1000  1000            ! maximum cylindrical wire
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DCGW   ! create display delete ! DC geometry info by wire - in sector coordinate system
!
!   ATTributes:
!   -----------
!COL ATT-name FMT   Min    Max          ! Comments
  1  x_mid     F  -1000.0  1000.0          ! x at midplane of wire(cm)
  2  y_mid     F  -1000.0  1000.0          ! y at midplane of wire(cm)
  3  z_mid     F  -1000.0  1000.0          ! z at midplane of wire(cm)
  4  x_dir     F  -1.0000  1.0000          ! x direction cosine along wire (cm)
  5  y_dir     F  -1.0000  1.0000          ! y direction cosine along wire (cm)
  6  z_dir     F  -1.0000  1.0000          ! z direction cosine along wire (cm)
  7  w_len     F   0.0     1000.0          ! wire length from midplane to amplifier (cm)
  8  w_len_hv  F   0.0     1000.0          ! wire length from midplane to HV (cm)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DCH         ! create write display delete ! GSIM Drift chamber hits
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
  1  x     F   -1000.   2000.  ! x at layer center
  2  y     F   -1000.   2000.  ! y at layer center
  3  z     F   -1000.   2000.  ! z at layer center
  4  step  F     0.     20.    ! step size through layer
  5  dedx  F     0.     1.     ! energy deposit in layer
  6  pmom  F     0.    20.     ! track momentum at layer center 
  7  time  F     0.    100.    ! time of hit at layer center
  8  cx    F    -1.     1.     ! track x dir cosine at layer center
  9  cy    F    -1.     1.     ! track y dir cosine at layer center
 10  cz    F    -1.     1.     ! track z dir cosine at layer center
 11  track I     0    0xFFFF   ! track number
 12  id    I  -5000   5000     ! track PDG id
 13  layer I  -5000   5000     ! layer number
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   DCMN   B32   ! DC Monitoring bank for mySQL
!
!COL ATT-name    FMT Min    Max   !Comments
   1 HBT_evt_1    F   0.  99999.  ! Hit Based tracks per event for Sector 1 
   2 TBT_evt_1    F   0.  99999.  ! Time Based tracks per event for Sector 1 
   3 HBT_evt_2    F   0.  99999.  ! Hit Based tracks per event for Sector 2
   4 TBT_evt_2    F   0.  99999.  ! Time Based tracks per event for Sector 2 
   5 HBT_evt_3    F   0.  99999.  ! Hit Based tracks per event for Sector 3 
   6 TBT_evt_3    F   0.  99999.  ! Time Based tracks per event for Sector 3 
   7 HBT_evt_4    F   0.  99999.  ! Hit Based tracks per event for Sector 4 
   8 TBT_evt_4    F   0.  99999.  ! Time Based tracks per event for Sector 4 
   9 HBT_evt_5    F   0.  99999.  ! Hit Based tracks per event for Sector 5 
  10 TBT_evt_5    F   0.  99999.  ! Time Based tracks per event for Sector 5 
  11 HBT_evt_6    F   0.  99999.  ! Hit Based tracks per event for Sector 6 
  12 TBT_evt_6    F   0.  99999.  ! Time Based tracks per event for Sector 6 
  13 HBT_evt_all  F   0.  99999.  ! Hit Based tracks per event for all sec.  
  14 TBT_evt_all  F   0.  99999.  ! Time Based tracks per event for all sec. 
  15 Res_s1_sl1   F   0.  99999.  ! Residual rms for sec 1, superlayer 1  
  16 Res_s1_sl2   F   0.  99999.  ! Residual rms for sec 1, superlayer 2  
  17 Res_s1_sl3   F   0.  99999.  ! Residual rms for sec 1, superlayer 3  
  18 Res_s1_sl4   F   0.  99999.  ! Residual rms for sec 1, superlayer 4  
  19 Res_s1_sl5   F   0.  99999.  ! Residual rms for sec 1, superlayer 5  
  20 Res_s1_sl6   F   0.  99999.  ! Residual rms for sec 1, superlayer 6  
  21 Res_s2_sl1   F   0.  99999.  ! Residual rms for sec 2, superlayer 1  
  22 Res_s2_sl2   F   0.  99999.  ! Residual rms for sec 2, superlayer 2  
  23 Res_s2_sl3   F   0.  99999.  ! Residual rms for sec 2, superlayer 3  
  24 Res_s2_sl4   F   0.  99999.  ! Residual rms for sec 2, superlayer 4  
  25 Res_s2_sl5   F   0.  99999.  ! Residual rms for sec 2, superlayer 5  
  26 Res_s2_sl6   F   0.  99999.  ! Residual rms for sec 2, superlayer 6  
  27 Res_s3_sl1   F   0.  99999.  ! Residual rms for sec 3, superlayer 1  
  28 Res_s3_sl2   F   0.  99999.  ! Residual rms for sec 3, superlayer 2  
  29 Res_s3_sl3   F   0.  99999.  ! Residual rms for sec 3, superlayer 3  
  30 Res_s3_sl4   F   0.  99999.  ! Residual rms for sec 3, superlayer 4  
  31 Res_s3_sl5   F   0.  99999.  ! Residual rms for sec 3, superlayer 5  
  32 Res_s3_sl6   F   0.  99999.  ! Residual rms for sec 3, superlayer 6  
  33 Res_s4_sl1   F   0.  99999.  ! Residual rms for sec 4, superlayer 1  
  34 Res_s4_sl2   F   0.  99999.  ! Residual rms for sec 4, superlayer 2  
  35 Res_s4_sl3   F   0.  99999.  ! Residual rms for sec 4, superlayer 3  
  36 Res_s4_sl4   F   0.  99999.  ! Residual rms for sec 4, superlayer 4  
  37 Res_s4_sl5   F   0.  99999.  ! Residual rms for sec 4, superlayer 5  
  38 Res_s4_sl6   F   0.  99999.  ! Residual rms for sec 4, superlayer 6  
  39 Res_s5_sl1   F   0.  99999.  ! Residual rms for sec 5, superlayer 1  
  40 Res_s5_sl2   F   0.  99999.  ! Residual rms for sec 5, superlayer 2  
  41 Res_s5_sl3   F   0.  99999.  ! Residual rms for sec 5, superlayer 3  
  42 Res_s5_sl4   F   0.  99999.  ! Residual rms for sec 5, superlayer 4  
  43 Res_s5_sl5   F   0.  99999.  ! Residual rms for sec 5, superlayer 5  
  44 Res_s5_sl6   F   0.  99999.  ! Residual rms for sec 5, superlayer 6  
  45 Res_s6_sl1   F   0.  99999.  ! Residual rms for sec 6, superlayer 1  
  46 Res_s6_sl2   F   0.  99999.  ! Residual rms for sec 6, superlayer 2  
  47 Res_s6_sl3   F   0.  99999.  ! Residual rms for sec 6, superlayer 3  
  48 Res_s6_sl4   F   0.  99999.  ! Residual rms for sec 6, superlayer 4  
  49 Res_s6_sl5   F   0.  99999.  ! Residual rms for sec 6, superlayer 5  
  50 Res_s6_sl6   F   0.  99999.  ! Residual rms for sec 6, superlayer 6  
!
 END TABLE
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DCPB    ! create write display delete ! DC tracks involved in the event
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  ScTr     I     0  6010    ! 100*sector+track_ID in *BTR  
  2  x_SC     F  -999.  999.   ! x coordinate of track intersection with SC plane 
  3  y_SC     F  -999.  999.   ! y coordinate of track intersection with SC plane
  4  z_SC     F  -999.  999.   ! z coordinate of track intersection with SC plane
  5  CX_SC    F    -1.    1.   ! X dir cosine at (x_SC,y_SC,z_SC)
  6  CY_SC    F    -1.    1.   ! y dir cosine at (x_SC,y_SC,z_SC)
  7  CZ_SC    F    -1.    1.   ! z dir cosine at (x_SC,y_SC,z_SC)
  8  X_v      F  -999.  999.   ! vertex X after fiting to the beam position
  9  Y_v      F  -999.  999.   ! vertex Y after fiting to the beam position
 10  Z_v      F  -999.  999.   ! vertex Z after fiting to the beam position
 11  R_v      F     0.  180.   ! distance from production vertex to the bemam. 
 12  Chi2     F     0.  100.   ! Chisquare of track fitting
 13  Status   I     0   0xFFFF ! Status word
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!
!-----------------------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DCV1  !  CREATE WRITE C_WRITE ! parameters for T->D
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min    Max   ! Comments
!
  1  TsR1       F      0.  80.   ! Tsmear 
  2  V0R1       F      0.  80.   ! drift velocity (slope)
  3  TmR1       F      0.  80.   ! Maximum drift time (Tmax)
  4  sp1R1      F      0.  80.   ! spare  
  5  sp2R1      F      0.  80.   ! spare	 
!
 END TABLE
!-----------------------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DCV2  !  CREATE WRITE C_WRITE ! parameters for T->D
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min    Max   ! Comments
!
  1  Ts1R2      F      0.  80.   ! Tsmear 
  2  V01R2      F      0.  80.   ! drift velocity (slope)
  3  VA1R2      F      0.  80.   ! drift velocity function parameter
  4  VB1R2      F      0.  80.   ! drift velocity function parameter  
  5  Tm1R2      F      0.  80.   ! Maximum drift time (Tmax) 		 
  6  TA1R2      F      0.  80.   ! Tmax function parameter
  7  TB1R2      F      0.  80.   ! Tmax function parameter
  8  Ts2R2      F      0.  80.   ! Tsmear 
  9  V02R2      F      0.  80.   ! drift velocity (slope)
 10  VA2R2      F      0.  80.   ! drift velocity function parameter
 11  VB2R2      F      0.  80.   ! drift velocity function parameter  
 12  Tm2R2      F      0.  80.   ! Maximum drift time (Tmax) 		 
 13  TA2R2      F      0.  80.   ! Tmax function parameter
 14  TB2R2      F      0.  80.   ! Tmax function parameter
!
 END TABLE
!----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DCV3  !  CREATE WRITE C_WRITE ! parameters for T->D
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min    Max   ! Comments
!
  1  TsR3       F      0.  80.   ! Tsmear 
  2  V0R3       F      0.  80.   ! drift velocity (slope)
  3  TmR3       F      0.  80.   ! Maximum drift time (Tmax)
  4  sp1R3      F      0.  80.   ! spare  
  5  sp2R3      F      0.  80.   ! spare	 
!
 END TABLE
!-------------------------------------------------------------------------------
 TABLE  DDLY  ! create   delete ! Drift chamber time DeLaY bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID         I    257 9408   ! ID_wire 
  2  TIDLY      F  -500. 500.   ! TIme DeLaY (ns) 		 
  3  STAT       I      0  900   ! wire STATus = 100*C + 10*A + B
                                ! A: 1 - good wire
                                !    2 - bad wire no data both on pulsing in & out
                                !    3 - bad wire no data on pulsing out only
                                !    4 - bad wire no data on pulsing in only
                                !    5 - good wire warning both on pulsing in & out
                                !    6 - good wire warning on pulsing out only
                                !    7 - good wire warning on pulsing in only
                                ! B: Information on slope_out - slope_in
                                !   1 - between   10% and 100% (negative status)
                                !   0 - between    1% and  10%
                                !   1 - between   .1% and   1%
                                !   2 - between  .01% and  .1%
                                !   3 - between .001% and .01%
                                ! C:Information on bad wires
                                !   0 - no know problem
                                !   1 - dead : isolated
                                !   2 - dead : muxed pair
                                !   3 - dead : HV single layer
                                !   4 - hot  : isolated
                                !   5 - hot  : muxed pair
                                !   6 - dead : signal cable disconnect
                                !   7 - dead : consequative mux channels 
                                !   8 - dead : HV multiple layers
                                !   9 - low gain
!
 END TABLE
!
 ARRAY JW
!-------------------------------------------------------------------------------
 TABLE  DGEO  ! create   delete ! Drift chamber GEOmetry bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID_sec     I      1   18   ! ID_sector 
  2  ID_reg     I      1   18   ! ID_region 
  3  xpos       F  -100. 100.   ! x misalignment 		 
  4  ypos       F  -100. 100.   ! y misalignment 		 
  5  zpos       F  -100. 100.   ! z misalignment 		 
  6  sxpos      F    -1.   1.   ! sx sine of little x angle 		 
  7  sypos      F    -1.   1.   ! sy sine of little y angle 
  8  szpos      F    -1.   1.   ! sz sine of little z angle 
!
 END TABLE
!-------------------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DHCL       B08  ! create write display delete ! drift chamber hit cluster
!
! record# = Sector
!  all hit clusters within a superlayer are reported, 
!  those that are used in tracks are marked:  BTRK=2**(track_in_sector-1)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  SLY      I     1       6  ! superlayer
  2  BTRK     I     0     255  ! track_in_sector# (bit set)
  3  TRKS1    I     0     255  ! combinations to track segments
  4  TRKS2    I     0     255  !   "             "       "
  5  WIRE1    I     1     192  ! 1.wire# in this cluster in 1.layer
  6  BWIR1    I     0     255  ! hits in this layer (starting from WIRE1) (bit st)
  7  WIRE2    I     1     192  ! 1.wire# in this cluster in 1.layer
  8  BWIR2    I     0     255  ! hits in this layer (starting from WIRE1) (bit st)
  9  WIRE3    I     1     192  ! 1.wire# in this cluster in 1.layer
 10  BWIR3    I     0     255  ! hits in this layer (starting from WIRE1) (bit st)
 11  WIRE4    I     1     192  ! 1.wire# in this cluster in 1.layer
 12  BWIR4    I     0     255  ! hits in this layer (starting from WIRE1) (bit st)
 13  WIRE5    I     1     192  ! 1.wire# in this cluster in 1.layer
 14  BWIR5    I     0     255  ! hits in this layer (starting from WIRE1) (bit st)
 15  WIRE6    I     1     192  ! 1.wire# in this cluster in 1.layer
 16  BWIR6    I     0     255  ! hits in this layer (starting from WIRE1) (bit st)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DITM    ! create write display delete ! Discriminator calibration time
!
!   ATTributes:
!   -----------
!COL ATT-name 	   	    FMT  Min     Max     ! Comments
!     	      
  1 time                      I    0    9999999  ! time of discriminator calibration
!
 END TABLE
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DOCA       B16  ! create write display delete ! Drift chamber event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I   257      9408  ! the address of the hit detector element
  2  DOCA     I  -32767   32768  ! doca from GSIM (micron)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DPCP   ! create write display delete ! Translated pair counter 
! pedestal bank, measured during the pedestal run.
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1   ID        I     1  999999  ! the address of the hit detector element
  2   MN_mean   I     0  100000  ! adc pedestal mean value (channel)
  3   MN_sigma  F     0  100000  ! sigma of the pedestal distribution (channel)
  4   LT_mean   I     0  100000  ! adc pedestal mean value (channel)
  5   LT_sigma  F     0  100000  ! sigma of the pedestal distribution (channel)
  6   RB_mean   I     0  100000  ! adc pedestal mean value (channel)
  7   RB_sigma  F     0  100000  ! sigma of the pedestal distribution (channel)
  8   LB_mean   I     0  100000  ! adc pedestal mean value (channel)
  9   LB_sigma  F     0  100000  ! sigma of the pedestal distribution (channel)
  10  RT_mean   I     0  100000  ! adc pedestal mean value (channel)
  11  RT_sigma  F     0  100000  ! sigma of the pedestal distribution (channel)
  12  VT_mean   I     0  100000  ! adc pedestal mean value (channel)
  13  VT_sigma  F     0  100000  ! sigma of the pedestal distribution (channel)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DPSP       ! create write display delete ! Translated pair specrometer 
! pedestal bank, measured during the pedestal run.
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID        I     1  999999  ! the address of the hit detector element
  2  mean      I     0  100000  ! adc pedestal mean value (channel)
  3  sigma     F     0  100000  ! sigma of the pedestal distribution (channel)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DSPC       B16  ! create write display delete ! Down Stream Pair Counter event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  PCID     I     0       1  ! Id compelled by DAQ (always 1)
  2  TDCPC    I     0    4096  ! tdc information ( scintillator)
  3  ADCMN    I     0   65536  ! adc information (main)
  4  ADCLT    I     0   65536  ! adc information (left top)
  5  ADCRB    I     0   65536  ! adc information (right bottom)
  6  ADCLB    I     0   65536  ! adc information (left bottom)
  7  ADCRT    I     0   65536  ! adc information (right top)
  8  ADCVE    I     0   65536  ! adc information (veto)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DSPS       B16  ! create write display delete ! Down Stream Pair Spectrometer event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I    1    8     ! paddle ID (left = 1 to 4)( right = 5 to 8)
  2  TDC      I    0    4096  ! tdc information
  3  ADC      I    0   65536  ! adc information 
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DSTC       B16  ! create write display delete   ! Down Stream Total Absorption Shower Counter event bank
!		
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  TACID    I     0    1     ! ID compelled by DAQ (always 1)
  2  TDCTAC   I     0    4096  ! tdc information (on sum)
  3  ADCLT    I     0   65536  ! adc information (left top)
  4  ADCRT    I     0   65536  ! adc information (right top)
  5  ADCLB    I     0   65536  ! adc information (left bottom)
  6  ADCRB    I     0   65536  ! adc information (right bottom)
  7  ADCSUM1  I     0   65536  ! adc information (sum scale1)
  8  ADCSUM2  I     0   65536  ! adc information (sum scale2)
  9  ADCSUM3  I     0   65536  ! adc information (sum scale3)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DTCP      ! create write display delete ! Translated TAC  
! pedestal bank, measured during the pedestal run.
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1   ID        I     1  999999  ! the address of the hit detector element
  2   LT_mean   I     0  100000  ! adc pedestal mean value (channel)
  3   LT_sigma  F     0  100000  ! sigma of the pedestal distribution (channel)
  4   RT_mean   I     0  100000  ! adc pedestal mean value (channel)
  5   RT_sigma  F     0  100000  ! sigma of the pedestal distribution (channel)
  6   LB_mean   I     0  100000  ! adc pedestal mean value (channel)
  7   LB_sigma  F     0  100000  ! sigma of the pedestal distribution (channel)
  8   RB_mean   I     0  100000  ! adc pedestal mean value (channel)
  9   RB_sigma  F     0  100000  ! sigma of the pedestal distribution (channel)
  10  sum1_mean I     0  100000  ! adc pedestal mean value (channel)
  11  sum1_sigma F     0  100000  ! sigma of the pedestal distribution (channel)
  12  sum2_mean I     0  100000  ! adc pedestal mean value (channel)
  13  sum2_sigma F     0  100000  ! sigma of the pedestal distribution (channel)
  14  sum3_mean I     0  100000  ! adc pedestal mean value (channel)
  15  sum3_sigma F     0  100000  ! sigma of the pedestal distribution (channel)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DTRK     ! create write display delete ! Drift chamber tracks
                 ! CED display bank of drift chamber tracks
                 ! record number = sector# + 16*(track number) +
                 !                          512*(particle ID)
                 !
                 ! X,Y,Z are the XYZ positions of a track in the sector
                 ! coordinate system,  one (XYZ) coordinate per tracking
                 ! layer, plus one coordinate for the CC SC EC.
                 !
                 ! Each track gets its own DTRK bank (one bank/track)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  X        F   -999999.  999999. !
  2  Y        F   -999999.  999999. !
  3  Z        F   -999999.  999999. !
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  EC01     ! create write display delete ! Scintillation counter hits bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I     1      48  ! the address of the hit detector element
  2  time    F     0   100000  ! time for left paddle(ns) 
  3  energy    F     0   65536  ! energy in left paddle(MeV) 
  
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  EC1      B16   ! create write display delete ! Large angle calorimeter event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I   257    1064  ! the address of the hit detector element
  2  TDCL     I     0  100000  ! tdc information (channels)
  3  ADCL     I     0  100000  ! adc information (channels)
  4  TDCR     I     0  100000  ! tdc information (channels)
  5  ADCR     I     0  100000  ! adc information (channels)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!       BANKname BANKtype      ! Comments
 TABLE  EC1P               ! create write display delete 
!                               EC1 plane for track matching geometry bank 
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
1    N1x        F   -1.0   +1.0      ! x component of outward normal to plane
2    N1y        F   -1.0   +1.0      ! y component of outward normal to plane
3    N1z        F   -1.0   +1.0      ! z component of outward normal to plane
4    R1n        F   0.0  9999.9      ! distanse in cm from origin to plane
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!       BANKname BANKtype      ! Comments
 TABLE  EC1R     ! create write display delete ! LA calorimeter result bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  E_tot    F   0.0     6.0  ! cluster energy (sum of inner+outer)
  2  dE_tot   F   0.0     6.0  !  error on the cluster energy
  3  t_tot    F   0.0  9999.0  ! time
  4  dt_tot   F   0.0  9999.0  ! error on the time
  5  x_m      F -999.0  9999.0 ! x-in CLAS frame
  6  y_m      F -999.0  9999.0 ! y-in CLAS frame
  7  z_m      F -999.0  9999.0 ! z-in CLAS frame
  8  dx_m     F   0.0  9999.0  ! error on x
  9  dy_m     F   0.0  9999.0  ! error on y
 10  dz_m     F   0.0  9999.0  ! error on z
 11  E_in     F   0.0     6.0  !  cluster energy in inner layer
 12  t_in     F -999.0 9999.0  ! time from inner layer
 13  x_in     F -999.0 9999.0  ! lab coordinate , inner layer
 14  y_in     F -999.0 9999.0  ! lab coordinate , inner layer
 15  x_out    F -999.0 9999.0  ! lab coordinate , outer layer
 16  y_out    F -999.0 9999.0  ! lab coordinate , outer layer
 17  x2_in_l  F  -0.5   0.5    ! second moment of x inner left
 18  x2_in_r  F  -0.5   0.5    ! second moment of x inner right 
 19  y2_in_l  F  -0.5   0.5    ! second moment of y inner left
 20  y2_in_r  F  -0.5   0.5    ! second moment of y inner right
 21  x2_out_l F  -0.5   0.5    ! second moment of x outer hit left
 22  x2_out_r F  -0.5   0.5    ! second moment of x outer hit right
 23  y2_out_l F  -0.5   0.5    ! second moment of y outer hit left
 24  y2_out_r F  -0.5   0.5    ! second moment of y outer hit right
 25  i_in     I   1    40      ! cluster center in X inner short layer
 26  j_in     I   1    24      ! cluster center in Y inner long layer
 27  i_out    I   1    40      ! cluster center in X outer layers
 28  j_out    I   1    24      ! cluster center in Y outer layer
 29  a_in_xl  F   0.0   6.0    ! energy sum in inner short left pmts
 30  a_in_xr  F   0.0   6.0    ! energy sum in inner short right pmts
 31  a_in_yl  F   0.0   6.0    ! energy sum in inner long  left pmts
 32  a_in_yr  F   0.0   6.0    ! energy sum in inner long  right pmts
 33  a_out_xl F   0.0   6.0    ! energy sum in outer short left pmts
 34  a_out_xr F   0.0   6.0    ! energy sum in outer short right pmts
 35  a_out_yl F   0.0   6.0    ! energy sum in outer long left pmts
 36  a_out_yr F   0.0   6.0    ! energy sum in outer long right pmts
 37  t_in_xs  F   0.0   9999.0 ! tdc sum in inner short righ+left  for cluster center
 38  t_in_xd  F -9999.0 9999.0 ! tdc dif in inner short right-left 
 39  t_in_ys  F   0.0   9999.0 ! tdc sum in inner long  righ+left
 40  t_in_yd  F -9999.0 9999.0 ! tdc dif in inner long  right-left
 41  t_out_xs F   0.0   9999.0 ! tdc sum in outer short righ+left 
 42  t_out_xd F -9999.0 9999.0 ! tdc dif in outer short right-left 
 43  t_out_ys F   0.0   9999.0 ! tdc sum in outer long righ+left
 44  t_out_yd F -9999.0 9999.0 ! tdc dif in outer long right-left 
 45  ibl      I    1    2      ! LAC  block number
 46  ncluster I    1    10000  ! 1000xNclust4+100xNclust3+10xNclust2+Nclust1 
 47  pmtfired I    0    512    ! Number of fired pmt (more than threshold) 
 48  z_in     F   0.0  99999.  ! Z in CLAS frame
 49  z_out    F   0.0  99999.  ! Z out in CLAS frame
 50  istat    I    0    0xFFFF ! status word
!!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECCA                 ! create write display delete ! Forward EC Calibration Bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT   Min    Max   ! Comments
  1  id       I       1     216  ! PMT ID
  2  aPED     F       0  1000.0  ! ADC pedestals (channels)
  3  aSIG     F       0  1000.0  ! ADC pedestal variance (channels)
  4  aMIP     F       0   100.0  ! ADC calibration from MIP (N.I.M.P/ch.)
  5  aMIPu    F       0   100.0  ! aMIP Error
  6  aSHR     F       0   100.0  ! ADC calibration from showers (GeV/ch.)
  7  aSHRu    F       0   100.0  ! aSHR Error
  8  stat     I       0  0xFFFF  ! 4 byte status word    
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECCL                 ! create write display delete ! Forward EC Calibration Bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT   Min    Max   ! Comments
  1  id       I       1     216  ! PMT ID
  2  lDB      F       0 99999.0  ! Stack atten length (database)(cm)
  3  lDBu     F       0 99999.0  ! lDB Error (cm)
  4  lMIP     F       0 99999.0  ! Stack atten length (MinIonPart)(cm) 
  5  lMIPu    F       0 99999.0  ! lMIP Error (cm)
  6  lSHR     F       0 99999.0  ! Stack atten length (showers)(cm)  
  7  lSHRu    F       0 99999.0  ! lSHR Error (cm) 
  8  stat     I       0  0xFFFF  ! 4 byte status word    
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECCT                 ! create write display delete ! Forward EC Calibration Bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT   Min    Max   ! Comments
  1  id       I       1     216  ! PMT ID
  2  tOFF     F       0 99999.0  ! TDC offset (channels)
  3  tOFFu    F       0 99999.0  ! tOFF Error
  4  tGAIN    F       0  1000.0  ! TDC conversion gain (nS/channel)
  5  tGAINu   F       0  1000.0  ! tGAIN Error
  6  tW0      F -9999.0  9999.0  ! Time walk constant (channels)
  7  tW0u     F -9999.0  9999.0  ! tWOu Error
  8  tW1      F -9999.0  9999.0  ! Time walk correction parameter 
  9  tW1u     F -9999.0  9999.0  ! tW1u Error
 10  tVEF     F       0    10.0  ! Effective velocity of light (cm/ns)
 11  tVEFu    F       0    10.0  ! tVEFu Error
 12  stat     I       0  0xFFFF  ! 4 byte status word    
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  EC       B16   ! create write display delete ! Forward calorimeter event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I   257    1572  ! the address of the hit detector element
  2  TDC      I     0   65536  ! tdc information (channels)
  3  ADC      I     0   65536  ! adc information (channels)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECDI        ! create write display delete ! EC channel discriminator thresholds
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1   crate       I     0  10      ! CAMAC crate number
  1   slot        I     0  30      ! slot
  1   mask        I     0  999999  ! mask
  1   threshold   I     0  100000  ! actual threshold value (mV)
  1   width       I     0  100000  ! actual width value
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECG        ! create write display delete ! Calorimeter geometry bank
!                                       
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  L0        F     0      10  ! distance from the target 
  2  THE0      F     0   9999.  ! angle between the beam and perpendicular
  3  YLOW      F     0   9999.  ! 
  4  YHI       F  -999.     0.  ! 
  5  DYLOW     F     0     99.  ! 
  6  DYHI      F     0     99.  ! 
  6  LRTH      F     0     99.  ! thickness of the single layer
  7  TANGR     F     0    999.  ! 
  8  SECTOR    I     1      6   ! 
  9  PHISEC    F     0    999.  ! 
 10  X0OFF     F  -999.   999.  ! 
 11  Y0OFF     F  -999.   999.  ! 
 12  Z0OFF     F  -999.   999.  ! 
 13  ROTM11    F  -999.   999.  ! 
 14  ROTM12    F  -999.   999.  ! 
 15  ROTM13    F  -999.   999.  ! 
 16  ROTM21    F  -999.   999.  ! 
 17  ROTM22    F  -999.   999.  ! 
 18  ROTM23    F  -999.   999.  ! 
 19  ROTM31    F  -999.   999.  ! 
 20  ROTM32    F  -999.   999.  ! 
 21  ROTM33    F  -999.   999.  ! 
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECHB     ! create write display delete ! Forward calorimeter result bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  Sect      I    0   0xFFFF  ! Sector number & Layer number
  2  E__hit    F   0.0     6.0  ! energy found 
  3  dE_hit    F   0.0     6.0  ! error on the energy found 
  4  t_hit     F   0.0  9999.0  ! time found 
  5  dt_hi     F   0.0  9999.0  ! error time found 
  6  i_hit     F   0.0  9999.0  ! sector rectangular coordinate 
  7  j_hit     F   0.0  9999.0  ! sector rectangular coordinate 
  8  di_hit    F   0.0  9999.0  ! sector rectangular coordinate error,
  9  dj_hit    F   0.0  9999.0  ! sector rectangular coordinate error,
 10  x_hit     F   0.0  9999.0  ! lab coordinate,
 11  y_hit     F   0.0  9999.0  ! lab coordinate,
 12  z_hit     F   0.0  9999.0  ! lab coordinate,
 13  dx_hit    F   0.0  9999.0  ! lab coordinate error,
 14  dy_hit    F   0.0  9999.0  ! lab coordinate error, 
 15  dz_hit    F   0.0  9999.0  ! lab coordinate error,
 16  u2_hit    F   0.0  9999.0  ! second moment of u  _hit pattern
 17  v2_hit    F   0.0  9999.0  ! second moment of v  _hit pattern
 18  w2_hit    F   0.0  9999.0  ! second moment of w  _hit pattern
 19  u3_hit    F -9999.0  9999.0  ! third moment of u  _hit pattern 
 20  v3_hit    F -9999.0  9999.0  ! third moment of v  _hit pattern 
 21  w3_hit    F -9999.0  9999.0  ! third moment of w  _hit pattern 
 22  u4_hit    F -9999.0  9999.0  ! forth moment of u  _hit pattern 
 23  v4_hit    F -9999.0  9999.0  ! forth moment of v  _hit pattern 
 24  w4_hit    F -9999.0  9999.0  ! forth moment of w  _hit pattern 
 25  centr_U   F     0.0  9999.0  ! peak position on U axis 
 26  centr_V   F     0.0  9999.0  ! peak position on V axis 
 27  centr_W   F     0.0  9999.0  ! peak position on W axis 
 28  path_U    F     0.0  9999.0  ! path length from hit position to U axis 
 29  path_V    F     0.0  9999.0  ! path length from hit position to V axis 
 30  path_W    F     0.0  9999.0  ! path length from hit position to W axis 
 31  Nstrp_U   I     0      36  ! Number of U strips in the hit
 32  Nstrp_V   I     0      36  ! Number of V strips in the hit
 33  Nstrp_W   I     0      36  ! Number of W strips in the hit
 34  MatchID1  I     0      30  ! Id of matched hit in the layer1
 35  CH21      F     0.    999. ! Quality measure of matching with layer1
 36  MatchID2  I     0      30  ! Id of matched hit in the layer2
 37  CH22      F     0.    999. ! Quality measure of matching with layer2
 38  istat     I    0    0xFFFF ! Number of hits & hit ID
!!
! For matching if current layer is WHOLE then layer1=INNER and layer2=OUTER
!              if current layer is INNER then layer1=WHOLE and layer2=OUTER
!              if current layer is OUTER then layer1=WHOLE and layer2=INNER
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECH            ! create write display delete ! GSIM Calorimeter track hit info
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  x       F   -1000.   2000. ! x of hit
  2  y       F   -1000.   2000. ! y of hit
  3  z       F   -1000.   2000. ! z of hit
  4  cx      F    -1.     1.    ! track x dir cosine
  5  cy      F    -1.     1.    ! track y dir cosine
  6  cz      F    -1.     1.    ! track z dir cosine
  7  pmom    F     0.    20.    ! track momentum
  8  id      I  -5000   5000    ! track PDG id
  9  tof     F     0.   1000.   ! time of flight
 10  edepin  F     0.    20.    ! deposit energy (inner part)
 11  edepout F     0.    20.    ! deposit energy (outer part)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECMT       ! create write display delete ! Mean EC pretrigger thresholds
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1   in_high      I     0  100000  ! inner high threshold (mV)
  2   out_high     I     0  100000  ! outer high threshold (mV)
  3   total_high   I     0  100000  ! total high threshold (mV)
  4   in_lo        I     0  100000  ! inner lo threshold (mV)
  5   out_lo       I     0  100000  ! outer lo threshold (mV)
  6   total_lo     I     0  100000  ! total lo threshold (mV)

!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECP1       ! create write display delete ! Translated LAC pedestal bank, measured during the pedestal run. 
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID            I   257    1064  ! the address of the hit detector element
  2  mean_left     I     0  100000  ! left adc pedestal mean value (channel)
  3  sigma_left    F     0  100000  ! sigma of the pedestal distribution for left adc (channel)
  4  mean_right    I     0  100000  ! right adc pedestal mean value (channel)
  5  sigma_right   F     0  100000  ! sigma of the pedestal distribution for right adc (channel
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECPB    ! create write display delete ! EC hits involved in the event
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  ScHt     I     0  6010      ! 100*sector+Whole_Hit_ID in ECHB 
  2  Etot     F     0.   20.     ! Reconstructed total energy
  3  Ein      F     0.   20.     ! Inner energy
  4  Eout     F     0.   20.     ! Outer energy 
  5  Time     F    -5.  150.     ! Flight time relative to the evnt start time
  6  Path     F     0.  600.     ! Path lenght from target
  7  X        F    -1.    1.     ! x coordinate of hit 
  8  Y        F    -1.    1.     ! y coordinate of hit
  9  Z        F    -1.    1.     ! z coordinate of hit
 10  M2_hit   F   0.0  9999.0    ! second moment of _hit pattern
 11  M3_hit   F -9999.0  9999.   ! third moment of  _hit pattern 
 12  M4_hit   F     0.  9999.    ! forth moment of  _hit pattern 
 13  InnStr   I     0. 40000.    ! 10000*UI+100*VI+WI 
 14  OutStr   I     0. 40000.    ! 10000*UO+100*VO+WO 
 15  Chi2EC   F     0.   100.    ! Quality measure of geometrical matching
 16  Status   I     0   0xFFFF   ! Status word (not implemented yet)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECPC         ! create write display delete ! EC Particle Calibration bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I   101     336  ! layer(1-3) * 100 + strip ID
  2  TDC      F  -100   65536  ! tdc (channels)
  3  ADC      F  -100   65536  ! adc - pedestal (channels)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECP               ! create write display delete 
!                               EC plane for track matching geometry bank 
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
1    N1x        F   -1.0   +1.0      ! x component of outward normal to plane
2    N1y        F   -1.0   +1.0      ! y component of outward normal to plane
3    N1z        F   -1.0   +1.0      ! z component of outward normal to plane
4    R1n        F   0.0  9999.9      ! distanse in cm from origin to plane
5    PlW        F   0.0  9999.9      ! whole plane depth
6    PlI        F   0.0  9999.9      ! inner plane depth
7    PlO        F   0.0  9999.9      ! outer plane depth
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECPE       ! create write display delete ! Translated EC pedestal bank, measured during the pedestal run.
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID         I   257    1572  ! the address of the hit detector element
  2  mean       I     0   65536  ! adc pedestal mean value (channel)
  3  sigma      F     0   65536  ! sigma of the pedestal distribution (channel)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECPI        ! create write display delete ! Calorimeter pixels for DISPLAY only
!                                       
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID        I     1       6  ! sector number
  2  Layer     I     1       6  ! layer number, note 1-inner, 2-outer, 3-whole
  3  HITID     I     1      10  ! number of hits (first 16 bits) and hit ID (last 16 bits)
  4  iloc      F  -999.  9999.  ! position of the hit in the local coordinate system
  5  jloc      F  -999.  9999.  ! position of the hit in the local coordinate system
  6  diloc     F  -999.  9999.  ! i width of the hit
  7  djloc     F  -999.  9999.  ! j width of the hit 
  8  R         F     0.  9999.  ! radius of the shower
  9  E         F     0.  9999.  ! energy
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECPO        ! create write display delete ! Forward EC hit pointerw
!                                                   to the strips.
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  UID1       I     1   999999  ! Pointer to the U-strips 1-18 for hit
  2  UID2       I     1   999999  ! Pointer to the U-strips 19-3 for hit
  3  VID1       I     1   999999  ! Pointer to the V-strips 1-18 for hit
  4  VID2       I     1   999999  ! Pointer to the V-strips 19-3 for hit
  5  WID1       I     1   999999  ! Pointer to the W-strips 1-18 for hit 
  6  WID2       I     1   999999  ! Pointer to the W-strips 19-36 for hit 
  7  SLH        I     1   999999  ! Sector*1000+Layer*100+Hit
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECRB     ! create write display delete ! Forward calorimeter result bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  E_in     F   0.0     6.0  ! energy found for the inner layer
  2  E_out    F   0.0     6.0  ! energy found for the outer layer
  3  dE_in    F   0.0     6.0  ! error on the energy found for the inner layer
  4  dE_out   F   0.0     6.0  ! error on the energy found for the outer layer
  5  t_in     F   0.0  9999.0  ! time found for the inner layer
  6  t_out    F   0.0  9999.0  ! time found for the outer layer
  7  dt_in    F   0.0  9999.0  ! error on the time found for the inner layer
  8  dt_out   F   0.0  9999.0  ! error on the time found for the outer layer
  9  i_in     F   0.0  9999.0  ! sector rectangular coordinate for the inner layer
 10  j_in     F   0.0  9999.0  ! sector rectangular coordinate for the inner layer
 11  i_out    F   0.0  9999.0  ! sector rectangular coordinate for the outer layer
 12  j_out    F   0.0  9999.0  ! sector rectangular coordinate for the outer layer
 13  di_in    F   0.0  9999.0  ! sector rectangular coordinate error, inner layer
 14  dj_in    F   0.0  9999.0  ! sector rectangular coordinate error, inner layer
 15  di_out   F   0.0  9999.0  ! sector rectangular coordinate error, outer layer
 16  dj_out   F   0.0  9999.0  ! sector rectangular coordinate error, outer layer
 17  x_in     F   0.0  9999.0  ! lab coordinate, inner layer
 18  y_in     F   0.0  9999.0  ! lab coordinate, inner layer
 19  z_in     F   0.0  9999.0  ! lab coordinate, inner layer
 20  x_out    F   0.0  9999.0  ! lab coordinate, outer layer
 21  y_out    F   0.0  9999.0  ! lab coordinate, outer layer
 22  z_out    F   0.0  9999.0  ! lab coordinate, outer layer
 23  dx_in    F   0.0  9999.0  ! lab coordinate error, inner layer
 24  dy_in    F   0.0  9999.0  ! lab coordinate error, inner layer
 25  dz_in    F   0.0  9999.0  ! lab coordinate error, inner layer
 26  dx_out   F   0.0  9999.0  ! lab coordinate error, outer layer
 27  dy_out   F   0.0  9999.0  ! lab coordinate error, outer layer
 28  dz_out   F   0.0  9999.0  ! lab coordinate error, outer layer
 29  u2_in    F   0.0  9999.0  ! second moment of u inner hit pattern
 30  v2_in    F   0.0  9999.0  ! second moment of v inner hit pattern
 31  w2_in    F   0.0  9999.0  ! second moment of w inner hit pattern
 32  u2_out   F   0.0  9999.0  ! second moment of u outer hit pattern
 33  v2_out   F   0.0  9999.0  ! second moment of v outer hit pattern
 34  w2_out   F   0.0  9999.0  ! second moment of w outer hit pattern
 35  u3_in    F   0.0  9999.0  ! third moment of u inner hit pattern 
 36  v3_in    F   0.0  9999.0  ! third moment of v inner hit pattern 
 37  w3_in    F   0.0  9999.0  ! third moment of w inner hit pattern 
 38  u3_out   F   0.0  9999.0  ! third moment of u outer hit pattern 
 39  v3_out   F   0.0  9999.0  ! third moment of v outer hit pattern 
 40  w3_out   F   0.0  9999.0  ! third moment of w outer hit pattern 
 41  i2       F   0.0  9999.0  ! second moment of overall shower, sector coordinates
 42  j2       F   0.0  9999.0  ! second moment of overall shower, sector coordinates
 43  i3       F   0.0  9999.0  ! third moment of overall shower, sector coordinates
 44  j3       F   0.0  9999.0  ! third moment of overall shower, sector coordinates
 45  spare1   F  -9999. 99999. ! spare
 46  spare2   F  -9999. 99999. ! spare
 47  spare3   F  -9999. 99999. ! spare
 48  spare4   F  -9999. 99999. ! spare
 49  istat    I    0    0xFFFF ! status word
!!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ECS     ! create write display delete ! Calorimeter scaler bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1   ECS1      I     0    9999999  ! scaler sector 1
  2   ECS2      I     0    9999999  ! scaler sector 1
  3   ECS3      I     0    9999999  ! scaler sector 1
  4   ECS4      I     0    9999999  ! scaler sector 1
  5   ECS5      I     0    9999999  ! scaler sector 1
  6   ECS6      I     0    9999999  ! scaler sector 1
  7   ECS7      I     0    9999999  ! scaler sector 1
  8   ECS8      I     0    9999999  ! scaler sector 1
  9   ECS9      I     0    9999999  ! scaler sector 1
  10  ECS10     I     0    9999999  ! scaler sector 1
  11  ECS11     I     0    9999999  ! scaler sector 1
  12  ECS12     I     0    9999999  ! scaler sector 1
  13  ECS13     I     0    9999999  ! scaler sector 1
  14  ECS14     I     0    9999999  ! scaler sector 1
  15  ECS15     I     0    9999999  ! scaler sector 1
  16  ECS16     I     0    9999999  ! scaler sector 1
  17  ECS17     I     0    9999999  ! scaler sector 2
  18  ECS18     I     0    9999999  ! scaler sector 2
  19  ECS19     I     0    9999999  ! scaler sector 2
  20  ECS20     I     0    9999999  ! scaler sector 2
  21  ECS21     I     0    9999999  ! scaler sector 2
  22  ECS22     I     0    9999999  ! scaler sector 2
  23  ECS23     I     0    9999999  ! scaler sector 2
  24  ECS24     I     0    9999999  ! scaler sector 2
  25  ECS25     I     0    9999999  ! scaler sector 2
  26  ECS26     I     0    9999999  ! scaler sector 2
  27  ECS27     I     0    9999999  ! scaler sector 2
  28  ECS28     I     0    9999999  ! scaler sector 2
  29  ECS29     I     0    9999999  ! scaler sector 2
  30  ECS30     I     0    9999999  ! scaler sector 2
  31  ECS31     I     0    9999999  ! scaler sector 2
  32  ECS32     I     0    9999999  ! scaler sector 2
  33  ECS33     I     0    9999999  ! scaler sector 3
  34  ECS34     I     0    9999999  ! scaler sector 3
  35  ECS35     I     0    9999999  ! scaler sector 3
  36  ECS36     I     0    9999999  ! scaler sector 3
  37  ECS37     I     0    9999999  ! scaler sector 3
  38  ECS38     I     0    9999999  ! scaler sector 3
  39  ECS39     I     0    9999999  ! scaler sector 3
  40  ECS40     I     0    9999999  ! scaler sector 3
  41  ECS41     I     0    9999999  ! scaler sector 3
  42  ECS42     I     0    9999999  ! scaler sector 3
  43  ECS43     I     0    9999999  ! scaler sector 3
  44  ECS44     I     0    9999999  ! scaler sector 3
  45  ECS45     I     0    9999999  ! scaler sector 3
  46  ECS46     I     0    9999999  ! scaler sector 3
  47  ECS47     I     0    9999999  ! scaler sector 3
  48  ECS48     I     0    9999999  ! scaler sector 3
  49  ECS49     I     0    9999999  ! scaler sector 4
  50  ECS50     I     0    9999999  ! scaler sector 4
  51  ECS51     I     0    9999999  ! scaler sector 4
  52  ECS52     I     0    9999999  ! scaler sector 4
  53  ECS53     I     0    9999999  ! scaler sector 4
  54  ECS54     I     0    9999999  ! scaler sector 4
  55  ECS55     I     0    9999999  ! scaler sector 4
  56  ECS56     I     0    9999999  ! scaler sector 4
  57  ECS57     I     0    9999999  ! scaler sector 4
  58  ECS58     I     0    9999999  ! scaler sector 4
  59  ECS59     I     0    9999999  ! scaler sector 4
  60  ECS60     I     0    9999999  ! scaler sector 4
  61  ECS61     I     0    9999999  ! scaler sector 4
  62  ECS62     I     0    9999999  ! scaler sector 4
  63  ECS63     I     0    9999999  ! scaler sector 4
  64  ECS64     I     0    9999999  ! scaler sector 4
  65  ECS65     I     0    9999999  ! scaler sector 5
  66  ECS66     I     0    9999999  ! scaler sector 5
  67  ECS67     I     0    9999999  ! scaler sector 5
  68  ECS68     I     0    9999999  ! scaler sector 5
  69  ECS69     I     0    9999999  ! scaler sector 5
  70  ECS70     I     0    9999999  ! scaler sector 5
  71  ECS71     I     0    9999999  ! scaler sector 5
  72  ECS72     I     0    9999999  ! scaler sector 5
  73  ECS73     I     0    9999999  ! scaler sector 5
  74  ECS74     I     0    9999999  ! scaler sector 5
  75  ECS75     I     0    9999999  ! scaler sector 5
  76  ECS76     I     0    9999999  ! scaler sector 5
  77  ECS77     I     0    9999999  ! scaler sector 5
  78  ECS78     I     0    9999999  ! scaler sector 5
  79  ECS79     I     0    9999999  ! scaler sector 5
  80  ECS80     I     0    9999999  ! scaler sector 5
  81  ECS81     I     0    9999999  ! scaler sector 6
  82  ECS82     I     0    9999999  ! scaler sector 6
  83  ECS83     I     0    9999999  ! scaler sector 6
  84  ECS84     I     0    9999999  ! scaler sector 6
  85  ECS85     I     0    9999999  ! scaler sector 6
  86  ECS86     I     0    9999999  ! scaler sector 6
  87  ECS87     I     0    9999999  ! scaler sector 6
  88  ECS88     I     0    9999999  ! scaler sector 6
  89  ECS89     I     0    9999999  ! scaler sector 6
  90  ECS90     I     0    9999999  ! scaler sector 6
  91  ECS91     I     0    9999999  ! scaler sector 6
  92  ECS92     I     0    9999999  ! scaler sector 6
  93  ECS93     I     0    9999999  ! scaler sector 6
  94  ECS94     I     0    9999999  ! scaler sector 6
  95  ECS95     I     0    9999999  ! scaler sector 6
  96  ECS96     I     0    9999999  ! scaler sector 6
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  EID0           ! create write display delete ! Fast electron identification
!                                         output bank
!
!   ATTributes:
!   -----------
!COL ATT-name   FMT Min    Max          ! Comments
!    
  1  jeid0sec    I  1      6       !  Sector # for electron candidate
  2  jeid0cc     I  1      100     !  Pointer to the hit in CCRC bank
  3  jeid0ec     I  1      100     !  Pointer to the hit in ECHB bank
  4  jeid0sc     I  1      100     !  Pointer to the hit in SCRX bank
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  EPIC          ! create write display delete 
!
!   Online epics channel values
!   record no.=0 :  read out of lots of epics channels (every 20 sec)
!   record no.=1 :  read out of BPMs and Active Colemator (every 2 sec)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  value    F   -999999.0   999999.0 ! value of the epics channel
  2  char1    A    0          255      ! char[32] name;
  3  char2    A    0          255      ! 
  4  char3    A    0          255      ! 
  5  char4    A    0          255      ! 
  6  char5    A    0          255      ! 
  7  char6    A    0          255      ! 
  8  char7    A    0          255      ! 
  9  char8    A    0          255      ! 
!
 END TABLE!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  EVNT    ! create write display delete ! RECSIS reconstructed event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  ID       I  -5000   5000    ! Particle Data Group ID (from SEB) 
  2  Pmom     F     0.   20.     ! momentum (from tracking)
  3  Mass     F     0.   10.     ! mass squared (from SEB =p**2(1.-betta**2)/beta**2) 
  4  Charge   I    -1     1      ! charge (from tracking)
  5  Betta    F     0.    1.     ! Particle velocity in the units of c (=R_trk/TOF/c)
  6  Cx       F    -1.    1.     ! x dir cosine at track origin 
  7  cy       F    -1.    1.     ! y dir cosine at track origin
  8  cz       F    -1.    1.     ! z dir cosine at track origin
  9  X        F  -100.  100.     ! X coordinate of vertex (cm)
 10  Y        F  -100.  100.     ! Y coordinate of vertex (cm)
 11  Z        F  -100.  100.     ! Z coordinate of vertex (cm)
 12  DCstat   I     0    50      ! Pointer to DCPB bank (=0 if DC is not involved) 
 13  CCstat   I     0    50      ! Pointer to CCPB bank (=0 if CC is not involved) 
 14  SCstat   I     0    50      ! Pointer to SCPB bank (=0 if SC is not involved)  
 15  ECstat   I     0    50      ! Pointer to ECPB bank (=0 if EC is not involved) 
 16  LCstat   I     0    50      ! Pointer to LCPB bank (=0 if LAC is not involved) 
 17  STstat   I     0    50      ! Pointer to STPB bank (=0 if ST is not involved) 
 18  Status   I     0   0xFFFF   ! Status word (=0 out of time particle)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  FBPM     B16   ! create    ! Fast BPM bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID     I     0       7  ! the address of the hit detector element
  2  TDC    I     0   65536  ! tdc information (channels)
  3  ADC    I     0   65536  ! adc information (channels)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  G1SL     ! create write display delete ! Gated1 T Photon Flux Scaler bank 
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1   G1T1      I     0    9999999  ! 
  2   G1T2      I     0    9999999  ! 
  3   G1T3      I     0    9999999  ! 
  4   G1T4      I     0    9999999  ! 
  5   G1T5      I     0    9999999  ! 
  6   G1T6      I     0    9999999  ! 
  7   G1T7      I     0    9999999  ! 
  8   G1T8      I     0    9999999  ! 
  9   G1T9      I     0    9999999  ! 
  10  G1T10     I     0    9999999  ! 
  11  G1T11     I     0    9999999  ! 
  12  G1T12     I     0    9999999  ! 
  13  G1T13     I     0    9999999  ! 
  14  G1T14     I     0    9999999  ! 
  15  G1T15     I     0    9999999  ! 
  16  G1T16     I     0    9999999  ! 
  17  G1T17     I     0    9999999  ! 
  18  G1T18     I     0    9999999  ! 
  19  G1T19     I     0    9999999  ! 
  20  G1T20     I     0    9999999  ! 
  21  G1T21     I     0    9999999  ! 
  22  G1T22     I     0    9999999  ! 
  23  G1T23     I     0    9999999  ! 
  24  G1T24     I     0    9999999  ! 
  25  G1T25     I     0    9999999  ! 
  26  G1T26     I     0    9999999  ! 
  27  G1T27     I     0    9999999  ! 
  28  G1T28     I     0    9999999  ! 
  29  G1T29     I     0    9999999  ! 
  30  G1T30     I     0    9999999  ! 
  31  G1T31     I     0    9999999  ! 
  32  G1T32     I     0    9999999  ! 
  33  G1T33     I     0    9999999  ! 
  34  G1T34     I     0    9999999  ! 
  35  G1T35     I     0    9999999  ! 
  36  G1T36     I     0    9999999  ! 
  37  G1T37     I     0    9999999  ! 
  38  G1T38     I     0    9999999  ! 
  39  G1T39     I     0    9999999  ! 
  40  G1T40     I     0    9999999  ! 
  41  G1T41     I     0    9999999  ! 
  42  G1T42     I     0    9999999  ! 
  43  G1T43     I     0    9999999  ! 
  44  G1T44     I     0    9999999  ! 
  45  G1T45     I     0    9999999  ! 
  46  G1T46     I     0    9999999  ! 
  47  G1T47     I     0    9999999  ! 
  48  G1T48     I     0    9999999  ! 
  49  G1T49     I     0    9999999  ! 
  50  G1T50     I     0    9999999  ! 
  51  G1T51     I     0    9999999  ! 
  52  G1T52     I     0    9999999  ! 
  53  G1T53     I     0    9999999  ! 
  54  G1T54     I     0    9999999  ! 
  55  G1T55     I     0    9999999  ! 
  56  G1T56     I     0    9999999  ! 
  57  G1T57     I     0    9999999  ! 
  58  G1T58     I     0    9999999  ! 
  59  G1T59     I     0    9999999  ! 
  60  G1T60     I     0    9999999  ! 
  61  G1T61     I     0    9999999  ! 
  62  G1T62     I     0    9999999  ! 
  63  G1T63     I     0    9999999  ! 
  64  G1T64     I     0    9999999  ! 
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  G2SL     ! create write display delete ! Gated2 T Photon Flux Scaler bank 
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1   G2T1      I     0    9999999  ! 
  2   G2T2      I     0    9999999  ! 
  3   G2T3      I     0    9999999  ! 
  4   G2T4      I     0    9999999  ! 
  5   G2T5      I     0    9999999  ! 
  6   G2T6      I     0    9999999  ! 
  7   G2T7      I     0    9999999  ! 
  8   G2T8      I     0    9999999  ! 
  9   G2T9      I     0    9999999  ! 
  10  G2T10     I     0    9999999  ! 
  11  G2T11     I     0    9999999  ! 
  12  G2T12     I     0    9999999  ! 
  13  G2T13     I     0    9999999  ! 
  14  G2T14     I     0    9999999  ! 
  15  G2T15     I     0    9999999  ! 
  16  G2T16     I     0    9999999  ! 
  17  G2T17     I     0    9999999  ! 
  18  G2T18     I     0    9999999  ! 
  19  G2T19     I     0    9999999  ! 
  20  G2T20     I     0    9999999  ! 
  21  G2T21     I     0    9999999  ! 
  22  G2T22     I     0    9999999  ! 
  23  G2T23     I     0    9999999  ! 
  24  G2T24     I     0    9999999  ! 
  25  G2T25     I     0    9999999  ! 
  26  G2T26     I     0    9999999  ! 
  27  G2T27     I     0    9999999  ! 
  28  G2T28     I     0    9999999  ! 
  29  G2T29     I     0    9999999  ! 
  30  G2T30     I     0    9999999  ! 
  31  G2T31     I     0    9999999  ! 
  32  G2T32     I     0    9999999  ! 
  33  G2T33     I     0    9999999  ! 
  34  G2T34     I     0    9999999  ! 
  35  G2T35     I     0    9999999  ! 
  36  G2T36     I     0    9999999  ! 
  37  G2T37     I     0    9999999  ! 
  38  G2T38     I     0    9999999  ! 
  39  G2T39     I     0    9999999  ! 
  40  G2T40     I     0    9999999  ! 
  41  G2T41     I     0    9999999  ! 
  42  G2T42     I     0    9999999  ! 
  43  G2T43     I     0    9999999  ! 
  44  G2T44     I     0    9999999  ! 
  45  G2T45     I     0    9999999  ! 
  46  G2T46     I     0    9999999  ! 
  47  G2T47     I     0    9999999  ! 
  48  G2T48     I     0    9999999  ! 
  49  G2T49     I     0    9999999  ! 
  50  G2T50     I     0    9999999  ! 
  51  G2T51     I     0    9999999  ! 
  52  G2T52     I     0    9999999  ! 
  53  G2T53     I     0    9999999  ! 
  54  G2T54     I     0    9999999  ! 
  55  G2T55     I     0    9999999  ! 
  56  G2T56     I     0    9999999  ! 
  57  G2T57     I     0    9999999  ! 
  58  G2T58     I     0    9999999  ! 
  59  G2T59     I     0    9999999  ! 
  60  G2T60     I     0    9999999  ! 
  61  G2T61     I     0    9999999  ! 
  62  G2T62     I     0    9999999  ! 
  63  G2T63     I     0    9999999  ! 
  64  G2T64     I     0    9999999  ! 
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  G3SL     ! create write display delete ! Gated 3 T Photon Flux Scaler bank 
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1   G3T1      I     0    9999999  ! 
  2   G3T2      I     0    9999999  ! 
  3   G3T3      I     0    9999999  ! 
  4   G3T4      I     0    9999999  ! 
  5   G3T5      I     0    9999999  ! 
  6   G3T6      I     0    9999999  ! 
  7   G3T7      I     0    9999999  ! 
  8   G3T8      I     0    9999999  ! 
  9   G3T9      I     0    9999999  ! 
  10  G3T10     I     0    9999999  ! 
  11  G3T11     I     0    9999999  ! 
  12  G3T12     I     0    9999999  ! 
  13  G3T13     I     0    9999999  ! 
  14  G3T14     I     0    9999999  ! 
  15  G3T15     I     0    9999999  ! 
  16  G3T16     I     0    9999999  ! 
  17  G3T17     I     0    9999999  ! 
  18  G3T18     I     0    9999999  ! 
  19  G3T19     I     0    9999999  ! 
  20  G3T20     I     0    9999999  ! 
  21  G3T21     I     0    9999999  ! 
  22  G3T22     I     0    9999999  ! 
  23  G3T23     I     0    9999999  ! 
  24  G3T24     I     0    9999999  ! 
  25  G3T25     I     0    9999999  ! 
  26  G3T26     I     0    9999999  ! 
  27  G3T27     I     0    9999999  ! 
  28  G3T28     I     0    9999999  ! 
  29  G3T29     I     0    9999999  ! 
  30  G3T30     I     0    9999999  ! 
  31  G3T31     I     0    9999999  ! 
  32  G3T32     I     0    9999999  ! 
  33  G3T33     I     0    9999999  ! 
  34  G3T34     I     0    9999999  ! 
  35  G3T35     I     0    9999999  ! 
  36  G3T36     I     0    9999999  ! 
  37  G3T37     I     0    9999999  ! 
  38  G3T38     I     0    9999999  ! 
  39  G3T39     I     0    9999999  ! 
  40  G3T40     I     0    9999999  ! 
  41  G3T41     I     0    9999999  ! 
  42  G3T42     I     0    9999999  ! 
  43  G3T43     I     0    9999999  ! 
  44  G3T44     I     0    9999999  ! 
  45  G3T45     I     0    9999999  ! 
  46  G3T46     I     0    9999999  ! 
  47  G3T47     I     0    9999999  ! 
  48  G3T48     I     0    9999999  ! 
  49  G3T49     I     0    9999999  ! 
  50  G3T50     I     0    9999999  ! 
  51  G3T51     I     0    9999999  ! 
  52  G3T52     I     0    9999999  ! 
  53  G3T53     I     0    9999999  ! 
  54  G3T54     I     0    9999999  ! 
  55  G3T55     I     0    9999999  ! 
  56  G3T56     I     0    9999999  ! 
  57  G3T57     I     0    9999999  ! 
  58  G3T58     I     0    9999999  ! 
  59  G3T59     I     0    9999999  ! 
  60  G3T60     I     0    9999999  ! 
  61  G3T61     I     0    9999999  ! 
  62  G3T62     I     0    9999999  ! 
  63  G3T63     I     0    9999999  ! 
  64  G3T64     I     0    9999999  ! 
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  G4SL     ! create write display delete ! Gated 4 T Photon Flux Scaler bank 
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1   G4T1      I     0    9999999  ! 
  2   G4T2      I     0    9999999  ! 
  3   G4T3      I     0    9999999  ! 
  4   G4T4      I     0    9999999  ! 
  5   G4T5      I     0    9999999  ! 
  6   G4T6      I     0    9999999  ! 
  7   G4T7      I     0    9999999  ! 
  8   G4T8      I     0    9999999  ! 
  9   G4T9      I     0    9999999  ! 
  10  G4T10     I     0    9999999  ! 
  11  G4T11     I     0    9999999  ! 
  12  G4T12     I     0    9999999  ! 
  13  G4T13     I     0    9999999  ! 
  14  G4T14     I     0    9999999  ! 
  15  G4T15     I     0    9999999  ! 
  16  G4T16     I     0    9999999  ! 
  17  G4T17     I     0    9999999  ! 
  18  G4T18     I     0    9999999  ! 
  19  G4T19     I     0    9999999  ! 
  20  G4T20     I     0    9999999  ! 
  21  G4T21     I     0    9999999  ! 
  22  G4T22     I     0    9999999  ! 
  23  G4T23     I     0    9999999  ! 
  24  G4T24     I     0    9999999  ! 
  25  G4T25     I     0    9999999  ! 
  26  G4T26     I     0    9999999  ! 
  27  G4T27     I     0    9999999  ! 
  28  G4T28     I     0    9999999  ! 
  29  G4T29     I     0    9999999  ! 
  30  G4T30     I     0    9999999  ! 
  31  G4T31     I     0    9999999  ! 
  32  G4T32     I     0    9999999  ! 
  33  G4T33     I     0    9999999  ! 
  34  G4T34     I     0    9999999  ! 
  35  G4T35     I     0    9999999  ! 
  36  G4T36     I     0    9999999  ! 
  37  G4T37     I     0    9999999  ! 
  38  G4T38     I     0    9999999  ! 
  39  G4T39     I     0    9999999  ! 
  40  G4T40     I     0    9999999  ! 
  41  G4T41     I     0    9999999  ! 
  42  G4T42     I     0    9999999  ! 
  43  G4T43     I     0    9999999  ! 
  44  G4T44     I     0    9999999  ! 
  45  G4T45     I     0    9999999  ! 
  46  G4T46     I     0    9999999  ! 
  47  G4T47     I     0    9999999  ! 
  48  G4T48     I     0    9999999  ! 
  49  G4T49     I     0    9999999  ! 
  50  G4T50     I     0    9999999  ! 
  51  G4T51     I     0    9999999  ! 
  52  G4T52     I     0    9999999  ! 
  53  G4T53     I     0    9999999  ! 
  54  G4T54     I     0    9999999  ! 
  55  G4T55     I     0    9999999  ! 
  56  G4T56     I     0    9999999  ! 
  57  G4T57     I     0    9999999  ! 
  58  G4T58     I     0    9999999  ! 
  59  G4T59     I     0    9999999  ! 
  60  G4T60     I     0    9999999  ! 
  61  G4T61     I     0    9999999  ! 
  62  G4T62     I     0    9999999  ! 
  63  G4T63     I     0    9999999  ! 
  64  G4T64     I     0    9999999  ! 
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  GPAR          ! create write display delete !  input parameter bank 
!     for gsim ( record_no.=0 )
!     or event generators ( record_no.>0 )
!
!  save input parameters (float OR int values (char as ascii))
!  (special codes:  fval=-1111.0 -> ival=logical(-1=T,0=F);
!                   fval=1111.0  -> ival=ascii; 
!                   fval=1112.0  -> ival=execution time (name=ascii time))
!                   fval=1114.0  -> ival=creation time  (name=ascii time);
!  (first 10 entries: program_name (incl. path)  [ival='PGM1'...'PGM5'];
!                     cvs tag version            [ival='TAG '];
!                     creation time              [fval=1114.0; ival=unix time];
!                     execution time             [fval=1112.0; ival=unix time];
!                     host name                  [ival='HOST'];
!                     user name                  [ival='USER'] )
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  fval     F  -1000000.    10000000.  ! float value
  2  ival     I  -1000000     10000000   ! int value 
  3  char1    A    0          255       ! char[20] name;
  4  char2    A    0          255       ! 
  5  char3    A    0          255       ! 
  6  char4    A    0          255       ! 
  7  char5    A    0          255       ! 
!
 END TABLE
!********************************************************************
!BANKname      BANKtype                    ! Comments
 TABLE  GPID ! create write display delete ! ST-SC PID bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max    ! Comments
!
  1  pid        I   0       100     ! particle id (GEANT)
  2  x          F   -100.   100.    ! vector3_t vert; Vertex position {x,y,z} 
  3  y          F   -100.   100.    ! y
  4  z          F   -500.   500.    ! z
  5  E          F   0.      16.     ! vector4_t p; Energy
  6  px         F   -16.    16.     ! momentum {x,y,z}
  7  py         F   -16.    16.     !  py
  8  pz         F   -16.    16.     ! pz
  9  q          I   -16     16      ! charge
 10  trkid      I   -16.    16.     ! index to TBID bank, counting from 1
 11  sec        I   1       6       ! Sector track is in
 12  paddle     I   0       50      ! Paddle hit
 13  dedx       F   -10000. 10000   ! Energy deposited in TOF
 14  beta       F   -1.     1.      ! beta pmag/E
 15  sc_stat    I    0      10      ! status of hit matching to SC: see sc.h
 16  sc_time    F   -10000. 10000.  ! SC calibrated time for this track (ns)
 17  sc_len     F   -10000. 10000.  ! track length [cm] from origin to SC
 18  st_stat    I   0       10      ! ST status
 19  st_time    F   -10000. 10000.  ! ST calibrated time for this track (ns) 
 20  st_len     F   -10000. 10000.  ! track length [cm] from origin to ST
 21  mass       F   0.      1000.   ! mass from time-of-flight
 22  mass_ref   I   -10     10      ! 0: mass calc from SC & TAG, 1: SC & ST; -1 neutral or no SC;2:from PART
 23  betam      F   -1.     1.      ! beta from time_of-flight
 24  epho       F   0.      12.     ! closest photon energy (GeV)
 25  tpho       F   -1000.  1000.   ! Time of the photon after RF correction
 26  tagrid     I   0       4096    ! index to TAGR bank, counting from 1
 27  ngrf       I   0       1000    ! number of photons in the same RF bucket
 28  ppid       I   0       100     ! pid as seen in PART bank
!
END TABLE
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  GP_X          ! create write display delete 
!
!   Gamma Profiler scint. fiber scaler (X-axis)
!   record no.=0 :  array of 64 values (count rate per horizontal fiber)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  value    F   0.0   999999999.0  ! count rate per channel
!
 END TABLE!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  GP_Y          ! create write display delete 
!
!   Gamma Profiler scint. fiber scaler (Y-axis)
!   record no.=0 :  array of 64 values (count rate per vertical fiber)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  value    F   0.0   999999999.0  ! count rate per channel
!
 END TABLE!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  HBER  ! create write display delete ! Hit Based Tracking ERror bank
!  record_no=0
!
!  Fit parameter and Covariance matrix: (Cij)
!
!  Track# = row#  (cf. HBTR bank)
!  note these are in the sda tracking coordinate system 
!           (x=beamline, y=radially outward, z=parallel to axial wires)
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  q_over_p   F      0.    100.   ! q/p
  2  lambda     F    -10.     10.   ! dip angle (pi/2 - theta)
  3  phi        F    -60.     60.   ! phi
  4  d0         F   -100.    100.   ! min.distance from (x=0,y=0,z=?)   [cm]
  5  z0         F   -100.    100.   ! z position of starting point  [cm]
  6  c11        F    -10.     10.   ! element C{1,1}
  7  c12        F    -10.     10.   ! element C{1,2}
  8  c13        F    -10.     10.   ! element C{1,3}
  9  c14        F    -10.     10.   ! element C{1,4}
 10  c15        F    -10.     10.   ! element C{1,5}
 11  c22        F    -10.     10.   ! element C{2,2}
 12  c23        F    -10.     10.   ! element C{2,3}
 13  c24        F    -10.     10.   ! element C{2,4}
 14  c25        F    -10.     10.   ! element C{2,5}
 15  c33        F    -10.     10.   ! element C{3,3}
 16  c34        F    -10.     10.   ! element C{3,4}
 17  c35        F    -10.     10.   ! element C{3,5}
 18  c44        F    -10.     10.   ! element C{4,4}
 19  c45        F    -10.     10.   ! element C{4,5}
 20  c55        F    -10.     10.   ! element C{5,5}
 21  chi2       F      0.     50.   ! Chisquare for this Track
 22  layinfo1   I      0.      0.   ! layerhit info
 23  layinfo2   I      0.      0.   ! layerhit info&sector&track#in sector
! the layer hit info is stored in the following way
! for layinfo1= sum over each layer used in track(layers 1-30) Of 2^(layer#-1)
! for layinfo2 = sum of 2^(layer#-31) for (layers 31-36)
!	 	+ 256 * track# in sector + 256^3 * sector
!RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!  
!
 END TABLE


!       BANKname BANKtype      ! Comments
 TABLE  HBID  ! create write display delete ! Hit Based particle ID
!
! Hit Based Tracking Particle ID
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max     ! Comments
!
  1  track      I       1      100   ! Track Candidate (ptr to HBTR)
  2  sec        I       1        6   ! Sector track is in
  3  beta       F      0.       1.   ! Beta of the track in units of c
  4  vtime      F      0.   10000.   ! vertex time of track
  5  sc_stat    I  -10000   100000   ! status of hit matching to SC: see sc.h
  6  sc_id      I       1       48   ! Pointer to SCRC bank
  7  sc_time    F -10000.   10000.   ! SC calibrated time for this track (ns)
  8  sc_qual    F      0.   10000.   ! quality of match for SC
  9  sc_vtime   F -10000.   10000.   ! time at vertex for SC(ns)
 10  sc_beta    F      0.       1.   ! Beta calculated from TOF from SC
 11  cc_stat    I  -10000   100000   ! status of hit matching to CC: see sc.h
 12  cc_id      I       1      100   ! pointer to CC01 bank
 13  cc_time    F -10000.   10000.   ! CC calibrated time for this track (ns)
 14  cc_qual    F      0.   10000.   ! quality of match for CC
 15  cc_vtime   F -10000.   10000.   ! time at vertex for CC(ns)
 16  cc_beta    F      0.       1.   ! Beta as calculated by the EC
 17  ec_stat    I  -10000   100000   ! status of hit matching to ec: see sc.h
 18  ec_id      I       1      100   ! Pointer to ECHB bank
 19  ec_time    F -10000.   10000.   ! EC calibrated time for this track (ns)
 20  ec_qual    F      0.   10000.   ! EC quality factor
 21  ec_vtime   F -10000.   10000.   ! time at vertex for EC(ns)
 22  ec_beta    F      0.       1.   ! Beta as calculated by the EC
 23  st_stat    I  -10000   100000   ! status of hit matching to ST
 24  st_id      I       1      100   ! Pointer to STR bank
 25  st_time    F -10000.   10000.   ! ST calibrated time for this track (ns)
 26  st_qual    F      0.   10000.   ! ST quality factor
 27  st_vtime   F -10000.   10000.   ! time at vertex for ST(ns)
 28  st_beta    F      0.       1.   ! Beta as calculated by the ST
 29  lac_stat   I  -10000   100000   ! status of hit matching to LAC
 30  lac_id     I       1      100   ! Pointer to EC1R bank
 31  lac_time   F -10000.   10000.   ! LAC calibrated time for this track (ns)
 32  lac_qual   F      0.   10000.   ! LAC quality factor
 33  lac_vtime  F -10000.   10000.   ! time at vertex for LAC(ns)
 34  lac_beta   F      0.       1.   ! Beta as calculated by the LAC
!
! see clasBID_t in pid.h
!
 END TABLE
!
!----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  HBLA  ! create write display delete ! Hit Based tracking LAyer bank
!                                             (output of HIt Based Tracking)
!  record_no = sector# 
!
!  34 rows (=number of DC layers) for every track in this sector
!      ['trk_plane' counts 46 planes: Origin(1plane),Start_Cnt.(2planes),
!                           DC(36planes),CC(1plane),SC(4planes),EC(2planes)]
!    4 Layers in Superlayer 1 (Region 1 Stereo)  [=trk_planes  4-7 ]
!    6 Layers in Superlayer 2 (Region 1 Axial)   [=trk_planes 10-15]
!    6 Layers in Superlayer 3 (Region 2 Axial)   [=trk_planes 16-21]
!    6 Layers in Superlayer 4 (Region 2 Stereo)  [=trk_planes 22-27]
!    6 Layers in Superlayer 5 (Region 3 Axial)   [=trk_planes 28-33]
!    6 Layers in Superlayer 6 (Region 3 Stereo)  [=trk_planes 34-39]
!
!   Col 1 (trk_pln) allows pointing to other track banks (track_number)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max     ! Comments
!
  1  trk_pln   I    101    2000   ! (track_number) *100 + Trk_plane_number
  2  x         F  -1000.   1000.  ! z coord [cm]  for track in this plane
  3  y         F  -1000.   1000.  ! y coord [cm]  for track in this plane
  4  z         F  -1000.   1000.  ! z coord [cm]  for track in this plane
  5  Bx        F   -100.    100.  ! B-field in x [kG] at coord.{x,y,z}
  6  By        F   -100.    100.  ! B-field in y [kG] at coord.{x,y,z}
  7  Bz        F   -100.    100.  ! B-field in z [kG] at coord.{x,y,z}
  8  tlen      F      0.   1000.  ! track length [cm] from origin to this plane
  9  dc1       I      0    99999  ! Pointer to DC1 bank
 10  stat      I      1      100  ! Status of the hit
 11  wire      I      1      192  ! Wire number 
 12  dtime     F   -100.    5000. ! drift time [ns]  (not corrected for start ime)
 13  alpha     F    -40.      40. ! track angle (relative to R of SL) [deg]
 14  wlen      F      0.     200. ! Wire length (hit pos. to preamp)  [cm]
 15  sgdoca    F   0.001       5. ! sigma DOCA
 16  fitdoca   F    -10.      10. ! Fitted DOCA
!
!RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!  
!
 END TABLE
!
!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  HBTB  ! create write display delete ! Hit Based to Time Based tracking
!                                             Intermediate bank
! record_no = Sector#
!
! THE COORDINATE SYSTEM USED IN THIS BANK IS THE DETECTOR COORDINATE SYSTEM.
!
! For Resolving the LeftRight Ambiguity all Segments in Clusters that matched
!   to the track are tested, therefore we have to keep these Segments if we
!   want to separate Hit & Time Based Tracking
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  trk       I       1     20    ! Track Candidate 
  2  sly       I       1      6    ! Superlayer
  3  icl       I       1     10    ! Cluster# that matched to the track
  4  isg       I       1     20    ! Segment# in this cluster
  5  id1       I       1  10000    ! Ptr. to DC1 entry  for 1.Layer in SL
  6  id2       I       1  10000    ! Ptr. to DC1 entry  for 2.Layer in SL
  7  id3       I       1  10000    ! Ptr. to DC1 entry  for 3.Layer in SL
  8  id4       I       1  10000    ! Ptr. to DC1 entry  for 4.Layer in SL
  9  id5       I       1  10000    ! Ptr. to DC1 entry  for 5.Layer in SL
 10  id6       I       1  10000    ! Ptr. to DC1 entry  for 6.Layer in SL
!
 END TABLE

!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  HBTR  ! create write display delete ! Hit Based Tracking Result bank
! record_no= 0
!
! THE COORDINATE SYSTEM USED IN THIS BANK IS THE DETECTOR COORDINATE SYSTEM.
!
! (changed F.Klein Nov.97)
! col.9: trackno_in_Sector = track# in the Sector based Tracking Banks (HBLA,HDPL..)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  x          F    -50.     50.   ! x
  2  y          F    -50.     50.   ! y 'Vertex' position {x,y,z}
  3  z          F    -50.     50.   ! z
  4  px         F    -10.     10.   ! Px
  5  py         F    -10.     10.   ! Py  momentum at 'vertex' {Px,Py,Pz}
  6  pz         F    -10.     10.   ! Pz
  7  q          F     -1.      1.   ! charge   (straight tracks: set q=0.)
  8  chi2       F      0.     50.   ! Chisquare for this Track
  9  itr_sec    I    101     620    ! Trackno_in_Sector + Sector*100
!
!RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!  
!
 END TABLE

!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  HCAL	! create   delete ! Header CALibration bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  VERSION    I   0         2 ! Version number
  2  RUN_LOW    I   1      1000 ! LOW RUN number		 
  3  RUN_HIGH   I   1      1000 ! HIGH RUN number
  4  CATIME     I 100 100000000 ! CAlibration TIME (unix time = 0 1/1/1970)		
  5  ROCCA      I   0        20 ! 32 bit ReadOut Controller CAlibration status
                                ! 1 - Drift Chamber Calibration	
                                ! 2 - 
!
 END TABLE
! 
!-------------------------------------------------------------------------------
!
!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  HDPL  ! create write display delete ! Hbt Detector PLane bank
!                                           ! output of Hit Based Tracking
!
! THE COORDINATE SYSTEM USED IN THIS BANK IS THE SECTOR COORDINATE SYSTEM.
! 
! Record_no = sector#
!
! 10 rows per track in a sector  [i.e. for 2 tracks in one sector: 20 rows]:
!        1 plane for 'Vertex position (#1): closest distance to beamline
!        2 planes for Start Counter     (#2 , #3)
!        1 plane for Cerenkov Counter   (#40)
!        4 planes for Scint.Counters    (#41(=SCplane 1), #42(=SCplane 2),
!                                       (#43(=SCplanes 3&4),#44(=SCplanes5&6))
!        2 planes for Elmag.Calorimeter (#45(=Forw.EC), #46(=LA EC))
!        (same structure as TDPL (for Time Based Tracking))
!
!   Col 1 (trk_pln) allows pointing to other track banks (track_number)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  trk_pln   I    101    2000   ! (track_number) *100 + plane_number
  2  x         F  -1000.   1000.  ! vector3_t pos;
  3  y         F  -1000.   1000.  !  (x, y, z coord. for track in on the plane)
  4  z         F  -1000.   1000.  ! 
  5  cx        F     -1.      1.  ! vector3_t dir;
  6  cy        F     -1.      1.  !  direction cosines (x,y,z) for track at coord.{x,y,z}
  7  cz        F     -1.      1.  ! 
  8  tlen      F      0.   1000.  ! track length [cm] from origin to this plane
!
!RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!  
!
 END TABLE
!

!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   HEAD      ! create write display delete ! Bank HEAD
!
!COL ATT-name FMT Min    Max   !Comments
   1 VERSION  I    0      2    ! Version Number
   2 NRUN     I    1      1000 ! Run Number (monotonically increasing)
   3 NEVENT   I    1      1000 ! Event Number (starting with 1 at run begin,
                               !   counting separately for physics and scalers events)
   4 TIME     I  100 100000000 ! Event Time (UNIX time = seconds as of January 1,1970)
   5 TYPE     I    0      1000 ! Event Type (Defined by on-line system or MC run:
                               !            = 1 - 9    - physics events:
                               !            =   2          - physics sync
                               !            =   4          - level2 late fail
                               !            = 10       - scaler events
                               !           >= 100      - database record events
                               !            = 0        - control events
                               !            < 0        - Monte Carlo Events:
                               !            =   -1          - SDA
                               !            =   -2          - GEANT
                               !            =   -3          - ClasSim
   6 ROC      I    0  20000000 !            = 0        - sync status is OK
                               !            > 0        - bit pattern of offending ROC's
   7 EVTCLASS I    1      20   ! Event Classification from DAQ:
                               !        0 Special Events (scaler, parameter, database, etc.)
                               !     1-15 Physics Events
                               !       16 Sync Event
                               !       17 Prestart Event
                               !       18 Go Event
                               !       19 Pause Event
                               !       20 End Event
   8 TRIGBITS I    0   9999999 ! Level 1 Trigger Latch Word (16 bits)
                               !    (see LATCH1 in tgbi.ddl)
 END TABLE
!




!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   HEVT      ! create write display delete !Reconstructed event, Head bank
!
!COL ATT-name FMT Min    Max   !Comments
   1 ESTATUS  I   -1        1  ! Event Statuse after reconstruction
   2 NRUN     I    1      1000 ! Run Number (monotonically increasing)
   3 NEVENT   I    1      1000 ! Event Number in the run NRUN 
   4 TYPE     I    0      1000 ! Event Type (Data or MC)
   5 NPGP     I    0     1010  ! Number of final reconstructed particles*100 +
!                              + Number of geometrically reconstructed particles
   6 TRGPRS   I    0    100000 ! Trigger type*10000 + 
!                              + Prescale factor for that trigger (Event Class)
   7 FC       F    0.      999.! Faraday Cup (K)
   8 FCG      F    0.      999.! Faraday Cup Gated (K)
   9 TG       F    0.      999.! Clock Gated
  10 STT      F    0.      999.! Event Start Time 
  11 RF1      F    0.      999.! RF Time 
  12 RF2      F    0.      999.! RF Time
  13 CON1     F    0.   100000.! Control Rates 
  14 CON2     F    0.   100000.! Control Rates  
  15 CON3     F    0.   100000.! Control Rates 
  16 PTIME    I 100  100000000 ! Event Processing Time (UNIX time = seconds)
!
 END TABLE
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  HLS      B32   ! create    ! Helicity scaler bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  S1       I     1  9000000 ! 10MHz clock
  2  S2       I     1  9000000 ! OTR-1
  3  S3       I     1  9000000 ! OTR-2
  4  S4       I     1  9000000 ! SLM
  5  S5       I     1  9000000 ! lvl-1 trigger rate
  6  S6       I     1  9000000 ! L.R Moller coincidences
  7  S7       I     1  9000000 ! L.R Moller accidentals
  8  S8       I     1  9000000 ! F-CUP
  9  S9       I     1  9000000 ! pmt-1
  10 S10      I     1  9000000 ! pmt-2
  11 S11      I     1  9000000 ! pmt-3
  12 S12      I     1  9000000 ! pmt-4
  13 S13      I     1  9000000 ! reserve
  14 S14      I     1  9000000 ! reserve
  15 S15      I     1  9000000 ! Helicity states accumulating counter
  16 S16      I     1  9000000 ! HLS banks accumulating counter
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  IC       B16   ! create write display delete ! Inner calorimeter event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I     1    8190  ! the address of the hit detector element
  2  TDC      I     0   65536  ! tdc information (channels)
  3  ADC      I     0   65536  ! adc information (channels)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ICHB     ! create write display delete ! Inner crystal calorimeter reconstruction bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  Eclust     F   0.0     20.0  ! Cluster energy 
  2  Eclmax     F   0.0     20.0  ! Max energy in a crystal of the cluster 
  3  Tclust     F -1000.0 1000.0  ! Reconstructed cluster time
  4  Tclmax     F -1000.0 1000.0  ! Time in a crystal with Max enrgy
  5  xclust     F -1000.0 1000.0  ! lab coordinate X,
  6  yclust     F -1000.0 1000.0  ! lab coordinate Y,
  7  zclust     F -1000.0 1000.0  ! lab coordinate Z,
  8  xclmax     F -1000.0 1000.0  ! lab coordinate X,
  9  yclmax     F -1000.0 1000.0  ! lab coordinate Y,
 10  zclmax     F -1000.0 1000.0  ! lab coordinate Z,
 11  res1       F -1000.0 1000.0  ! lab coordinate error,
 12  res2       F -1000.0 1000.0  ! lab coordinate error,
 13  res3       F -1000.0 1000.0  ! lab coordinate error,
 14  ncryst     I  0    1000      ! Number of crystal in the cluster
!
!
 END TABLE
!
!-----------------------------------------------------------------------
! Kinematic Fitter Result bank.
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  KFIT          ! create write display delete !Kinematic Fitter Bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
1     momenta_f  F  0  10.0         ! fitted momentum parameter (GeV)   
2     lambda_f   F  -1.0 1.0        ! fitted dip angle (radians) 
3     phi_f      F  -4.0 4.0        ! fitted phi angle (radians)
4     d0_f       F  -100.0 100.0    ! fitted d0 (cm)
5     z0_f       F  -100.0 100.0    ! fitted z0 (cm)
6     sigma11    F  0  10.0         !   error element of known
7     sigma12	 F  -10.0  10.0	    !   error element of known
8     sigma13    F  -10.0  10.0     !   error element of known
9     sigma14    F  -10.0  10.0     !   error element of known
10    sigma15    F  -10.0  10.0     !   error element of known
11    sigma22    F  0  10.0         !   error element of known   
12    sigma23    F  -10.0  10.0     !   error element of known
13    sigma24    F  -10.0  10.0     !   error element of known
14    sigma25    F  -10.0  10.0     !   error element of known
15    sigma33    F  0  10.0         !   error element of known 
16    sigma34    F  -10.0  10.0     !   error element of known 
17    sigma35    F  -10.0  10.0     !   error element of known 
18    sigma44    F  0  10.0         !   error element of known 
19    sigma45    F  -10.0  10.0     !   error element of known 
20    sigma55    F  0  10.0         !   error element of known 
21    cov11      F  -10.0  10.0     !   error element bewteen known and unknown
22    cov12	 F  -10.0  10.0     !   error element bewteen known and unknown
23    cov13      F  -10.0  10.0     !   error element bewteen known and unknown
24    cov14      F  -10.0  10.0     !   error element bewteen known and unknown
25    cov15      F  -10.0  10.0     !   error element bewteen known and unknown
26    cov21      F  -10.0  10.0     !   error element bewteen known and unknown
27    cov22      F  -10.0  10.0     !   error element bewteen known and unknown
28    cov23      F  -10.0  10.0     !   error element bewteen known and unknown
29    cov24      F  -10.0  10.0     !   error element bewteen known and unknown
30    cov25      F  -10.0  10.0     !   error element bewteen known and unknown
31    cov31      F  -10.0  10.0     !   error element bewteen known and unknown
32    cov32      F  -10.0  10.0     !   error element bewteen known and unknown
33    cov33      F  -10.0  10.0     !   error element bewteen known and unknown
34    cov34      F  -10.0  10.0     !   error element bewteen known and unknown
35    cov35      F  -10.0  10.0     !   error element bewteen known and unknown
36    cov41      F  -10.0  10.0     !   error element bewteen known and unknown
37    cov42      F  -10.0  10.0     !   error element bewteen known and unknown
38    cov43      F  -10.0  10.0     !   error element bewteen known and unknown
39    cov44      F  -10.0  10.0     !   error element bewteen known and unknown
40    cov45      F  -10.0  10.0     !   error element bewteen known and unknown
41    cov51      F  -10.0  10.0     !   error element bewteen known and unknown
42    cov52      F  -10.0  10.0     !   error element bewteen known and unknown
43    cov53      F  -10.0  10.0     !   error element bewteen known and unknown
44    cov54      F  -10.0  10.0     !   error element bewteen known and unknown
45    cov55      F  -10.0  10.0     !   error element bewteen known and unknown
46    chi2piece  F  -10.0  10.0     ! track contribution to chi2
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  L1PG    ! create write display delete ! Level 1 program
!
!   ATTributes:
!   -----------
!COL ATT-name 	   	    FMT  Min     Max     ! Comments
!     	      
  1 l1prog                    A    0    9999999  ! Level 1 program
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  L2H       B16  ! create write display delete ! Level 2 hit bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I   257    9408  ! Superlayer + 256*(hit# in TDC)
  2  TDC      I     0   65536  ! tdc information (channels)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  L2S       B16  ! create write display delete ! Level 2 report bank
!
! id = 1 - TL1 - reported TDC for Level 1
! id = 2 - L2S1 - reported 2 segments out of 4 from sector 1
! id = 3 - L2S2 - reported 2 segments out of 4 from sector 2
! id = 4 - L2S3 - reported 2 segments out of 4 from sector 3
! id = 5 - L2S4 - reported 2 segments out of 4 from sector 4
! id = 6 - L2S5 - reported 2 segments out of 4 from sector 5
! id = 7 - L2S6 - reported 2 segments out of 4 from sector 6
! id = 8 - L2OR - Level 2 final report (OR from all sectors)
! id = 9 - L2NOR - Level 2 fault
!   Attributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID      I   256    9408  ! ID+256*hit#
  2  TDC     I     0   65536  ! TDC value
!

 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype       ! Comments
 TABLE  LASR  ! create write display delete ! events from laser calibration of the EC
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I         1000       10000    ! identifier  0-1099  TOF 1100-10000 EC laser  
  2  stat1    I   -100000000   100000000    ! status words different for each id
  3  stat2    I   -100000000   100000000    !  "
  4  stat3    I   -100000000   100000000    !  "
! 
 END TABLE!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  LCDI        ! create write display delete ! LAC channel discriminator thresholds
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1   crate       I     0  10      ! CAMAC crate number
  1   slot        I     0  30      ! slot
  1   mask        I     0  999999  ! mask
  1   threshold   I     0  100000  ! actual threshold value (mV)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  LCPB    ! create write display delete ! LAC hits involved in the event
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  ScHt     I     0    10      ! 100*sector+Hit_ID in EC1R
  2  Etot     F     0.   20.     ! Reconstructed total energy
  3  Time     F    -5.  150.     ! Flight time relative to the evnt start time
  4  Path     F     0.  600.     ! Path lenght from target
  5  X        F  -999.  999.     ! x coordinate of the hit
  6  Y        F  -999.  999.     ! y coordinate of the hit
  7  Z        F  -999.  999.     ! z coordinate of the hit
  8  Chi2LC   F     0.  100.     ! Quality measure of geometrical matching
  9  Status   I     0   0xFFFF   ! Status word
 10  Ein      F     0.   20.     ! Reconstructed energy in the inner part
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  LOWQ  ! create write display delete ! FET reconstruction
!
! THE COORDINATE SYSTEM USED IN THIS BANK IS THE DETECTOR COORDINATE SYSTEM.
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  pid       I       0         10    ! particle id (GEANT)
  2  x          F     -100.     100.   ! vector3_t vert; Vertex position {x,y,z}
  3  y         F      -100.     100.   ! y
  4  z          F    -500.     500.    ! z
  5  E         F     0.     16.        ! vector4_t p; Energy
  6  px         F    -16.     16.      ! momentum {x,y,z}
  7  py        F     -16.      16.     !  py
  8  pz         F     -16.      16.   ! pz
  9  q          F     -16.      16.   ! charge
 10 qpid     F   -100.0 100.0 ! quality factor for the pid
 11 qtrk      F   -100.  100.0 ! quality factor for the trk
 12 flags     I    0         10000 ! set of flags defining track (ie, BEAM)
!
 END TABLE

!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  MCEV          ! create write display delete ! GSIM Monte Carlo event parameters
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  I1      I     0   65536  ! first geant random number seed for event
  2  i2      I     0   65536  ! second seed
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   MCHD      ! create write display delete ! MC (generator) HeaD Bank
!
!  comments: REACTYPE allows to specify the reaction 
!                     (e.g. resonance cascade in celeg)
!            PX...PZ_TARG !=0 for fermi motion of target part.
!
!COL ATT-name FMT Min    Max   !Comments
   1 NRUN     I    1      1000 ! Run Number
   2 NEVENT   I    1 100000000 ! Event Number
   3 TIME     I  100 100000000 ! Event Time (UNIX time)
   4 TYPE     I    0     -1000 ! Event Type (MUST be negative)
                               !            < 0 Monte Carlo Events:
                               !                 = -1 - SDA
                               !                 = -2 - GEANT
                               !                 = -3 - Celeg
                               !                 = -4 - ao_gen
                               !                 ....
   5 REACTYPE  I    0  100000   ! optional: user defined reaction type
   6 WEIGHT    F   0.      1.   ! event weight
   7 W         F   0.     15.   ! center_of_mass energy (inv.mass of hadronic states)
   8 Q2        F   0.     30.   ! Q2 (photon virtuallity)
   9 E_PHOT    F   0.     15.   ! energy of (real/virtual) photon
  10 PX_PHOT   F   0.     15.   ! p_x: momentum (in x) of (real/virtual) photon
  11 PY_PHOT   F   0.     15.   ! p_y: momentum (in y) of (real/virtual) photon
  12 PZ_PHOT   F   0.     15.   ! p_z: momentum (in z) of (real/virtual) photon
  13 E_TARG    F   0.     15.   ! energy of target particle
  14 PX_TARG   F   0.     15.   ! p_x: momentum (in x) of target part.
  15 PY_TARG   F   0.     15.   ! p_y: momentum (in y) of target part.
  16 PZ_TARG   F   0.     15.   ! p_z: momentum (in z) of target part.
!
 END TABLE
!!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  MCTK    ! create write display delete ! GSIM Monte Carlo track bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  cx       F    -1.   1.    ! x dir cosine at track origin
  2  cy       F    -1.   1.    ! y dir cosine
  3  cz       F    -1.   1.    ! z dir cosine
  4  pmom     F     0.   20.   ! momentum
  5  mass     F     0.   10.   ! mass
  6  charge   F    -1.    1.   ! charge
  7  id       I  -5000   5000  ! track Particle Data Group id
  8  flag     I     0  0xFFFF  ! track flag
  9  beg_vtx  I     0   65536  ! beginning vertex number 
 10  end_vtx  I     0   65536  ! ending vertex number
 11  parent   I     0   65536  ! parent track
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  MCVX        ! create write display delete ! GSIM Monte Carlo vertex parameters
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  x       F   -1000.  2000.    ! x of vertex
  2  y       F   -1000.  2000.    ! y
  3  z       F   -1000.  2000.    ! z
  4  tof     F     0.0   999999.   ! secs of flight
  5  flag    I       0   65536    ! vertex flag
!
 END TABLE
!
!       BANKname BANKtype      ! Comments
 TABLE  MTRK  ! create write display delete ! vertex Result bank
!
! THE COORDINATE SYSTEM USED IN THIS BANK IS THE DETECTOR COORDINATE SYSTEM.
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max    ! Comments
!
  1  sect        I       0    1000   ! Sector Number for track
  2  trkl1       I       0      10   ! track segment and track cluster for Superlayer 1 (lnk_t)
  3  trkl2       I       0      10   ! track segment and track cluster for Superlayer 2
  4  trkl3       I       0      10   ! track segment and track cluster for Superlayer 3
  5  trkl4       I       0      10   ! track segment and track cluster for Superlayer 4
  6  trkl5       I       0      10   ! track segment and track cluster for Superlayer 5
  7  trkl6       I       0      10   ! track segment and track cluster for Superlayer 6
!
! typedef struct {
!  short segm;
!  short clust;
!} lnk_t;
!
!typedef struct {
!  unsigned int sect;
!  lnk_t data[6];
!}mtrk_t;
!
!typedef struct {
!  bankHeader_t bank;
!  mtrk_t mtrk[1];
!} clasMTRK_t;
!     
!
 END TABLE
!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  MVRT  ! create write display delete ! vertex Result bank
!
! THE COORDINATE SYSTEM USED IN THIS BANK IS THE DETECTOR COORDINATE SYSTEM.
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max    ! Comments
!
  1  v_id     	I   -1000    1000   ! info about track ids
  2  ntrk       F   -100.    100.   ! number of tracks used to make vertex
  3  x          F  -1000.   1000.   ! x vector3_t vert{x,y,z}
  4  y          F  -1000.   1000.   ! y 
  5  z          F  -1000.   1000.   ! z
  6  chi2       F  -1000.   1000.   ! chi2
  7  cxx        F  -1000.   1000.   ! Covariance matrix array element
  8  cxy        F  -1000.   1000.   ! Covariance matrix array element
  9  cxz        F  -1000.   1000.   ! Covariance matrix array element
 10  cyy        F  -1000.   1000.   ! Covariance matrix array element
 11  cyz        F  -1000.   1000.   ! Covariance matrix array element
 12  czz        F  -1000.   1000.   ! Covariance matrix array element
 13  stat       I  -1000.   1000.   ! status integer, not used yet
!
! note v_id is based upon the track id used to make the
! vertex.  v_id = (summed over all tracks used) 2^(tber id of track(1-10)) 
!                   + 1 if beamline info used
 END TABLE


!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  PART  ! create write display delete ! Hit Based Tracking Result bank
!
! THE COORDINATE SYSTEM USED IN THIS BANK IS THE DETECTOR COORDINATE SYSTEM.
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  pid       I       0         10    ! particle id (GEANT)
  2  x          F     -100.     100.   ! vector3_t vert; Vertex position {x,y,z} 
  3  y         F      -100.     100.   ! y
  4  z          F    -500.     500.    ! z
  5  E         F     0.     16.        ! vector4_t p; Energy
  6  px         F    -16.     16.      ! momentum {x,y,z}
  7  py        F     -16.      16.     !  py
  8  pz         F     -16.      16.   ! pz
  9  q          F     -16.      16.   ! charge
 10  trkid   I     -16.      16.   ! index to TBID bank, counting from 1
 11 qpid     F   -100.0 100.0 ! quality factor for the pid
 12 qtrk      F   -100.  100.0 ! quality factor for the trk
 13 flags     I    0         10000 ! set of flags defining track (ie, BEAM)
!
 END TABLE


!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  PCO   ! create write display delete ! down stream output bank
!		
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  TIME     F    -999999.9  999999.9 !  TDC time 
  2  ELT      F    -999999.9  999999.9  ! energy deposit PC = left top
  3  ERB      F    -999999.9  999999.9  ! energy deposit PC = right bottom
  4  ELB      F    -999999.9  999999.9  ! energy deposit PC = left bottom 
  5  ERT      F    -999999.9  999999.9  ! energy deposit PC = right top
  6  EMAIN    F    -999999.9  999999.9  ! energy deposit PC = MAIN
  7  EVETO    F    -999999.9  999999.9  ! energy deposit PC = veto
  8  TID      I    0     121            ! T id of the corresponding T
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   PHTM   B32   ! create write display delete ! Photon_mon bank for mySQL
!
!COL ATT-name FMT Min    Max   !Comments
   1 EVID     I    1   1000000 ! Event ID (number of triggers)
   2 Nsprot   I    1   1000000 ! Number of single proton events
   3 Npip     I    1   1000000 ! Number of single pi+ events
   4 Npim     I    1   1000000 ! Number of single pi- events
   5 Nppippim I    1   1000000 ! Number of single proton pi+ pi- events
   6 Nppip    I    1   1000000 ! Number of single proton pi+ events
   7 Npippim  I    1   1000000 ! Number of single pi+ pi- events
   8 Nppim    I    1   1000000 ! Number of single proton pi- events
   9 Nkp      I    1   1000000 ! Number of single K+ events
  10 Npkp     I    1   1000000 ! Number of single proton K+ events
  11 tag_pi_v F   -9999. 9999. ! Mean of (tagger_time - pion_vertex_time)
!
 END TABLE
!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   PID1   B32   ! pid_mon sector 1 particle yields for mySQL
!
!COL ATT-name  FMT Min    Max   !Comments
   1 NHBpos_1    I    1   1000000 ! Number of pos Hit-Based tracks for sec=1
   2 NHBneg_1    I    1   1000000 ! Number of neg Hit-Based tracks for sec=1
   3 NTBpos_1    I    1   1000000 ! Number of pos Time-Based tracks  for sec=1
   4 NTBneg_1    I    1   1000000 ! Number of neg Time-Based tracks for sec=1
   5 chi2pos_1   F    0.    9999. ! Chi^2 for positive tracks for sec=1
   6 chi2neg_1   F    0.    9999. ! Chi^2 for negative tracks for sec=1
   7 Nunknown_1  I    1   1000000 ! Number of unknown particles for sec=1
   8 Ngamma_1    I    1   1000000 ! Number of photons for sec=1
   9 Nelec_1     I    1   1000000 ! Number of electrons for sec=1
  10 Npiplus_1   I    1   1000000 ! Number of pi+ for sec=1
  11 Npiminus_1  I    1   1000000 ! Number of pi- for sec=1
  12 Nprot_1     I    1   1000000 ! Number of proton for sec=1
  13 Nkplus_1    I    1   1000000 ! Number of K+ for sec=1
  14 Nkminus_1   I    1   1000000 ! Number of K- for sec=1
  15 Ndeuteron_1 I    1   1000000 ! Number of deuterons for sec=1
  16 Nneutron_1  I    1   1000000 ! Number of neutrons for sec=1 
!
 END TABLE
!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   PID2   B32   ! pid_mon sector 2 particle yields for mySQL
!
!COL ATT-name  FMT Min    Max   !Comments
   1 NHBpos_2    I    1   1000000 ! Number of pos Hit-Based tracks for sec=2
   2 NHBneg_2    I    1   1000000 ! Number of neg Hit-Based tracks for sec=2
   3 NTBpos_2    I    1   1000000 ! Number of pos Time-Based tracks  for sec=2
   4 NTBneg_2    I    1   1000000 ! Number of neg Time-Based tracks for sec=2
   5 chi2pos_2   F    0.    9999. ! Chi^2 for positive tracks for sec=2
   6 chi2neg_2   F    0.    9999. ! Chi^2 for negative tracks for sec=2
   7 Nunknown_2  I    1   1000000 ! Number of unknown particles for sec=2
   8 Ngamma_2    I    1   1000000 ! Number of photons for sec=2
   9 Nelec_2     I    1   1000000 ! Number of electrons for sec=2
  10 Npiplus_2   I    1   1000000 ! Number of pi+ for sec=2
  11 Npiminus_2  I    1   1000000 ! Number of pi- for sec=2
  12 Nprot_2     I    1   1000000 ! Number of proton for sec=2
  13 Nkplus_2    I    1   1000000 ! Number of K+ for sec=2
  14 Nkminus_2   I    1   1000000 ! Number of K- for sec=2
  15 Ndeuteron_2 I    1   1000000 ! Number of deuterons for sec=2
  16 Nneutron_2  I    1   1000000 ! Number of neutrons for sec=2
!
 END TABLE
!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   PID3   B32   ! pid_mon sector 3 particle yields for mySQL
!
!COL ATT-name  FMT Min    Max   !Comments
   1 NHBpos_3    I    1   1000000 ! Number of pos Hit-Based tracks for sec=3
   2 NHBneg_3    I    1   1000000 ! Number of neg Hit-Based tracks for sec=3
   3 NTBpos_3    I    1   1000000 ! Number of pos Time-Based tracks  for sec=3
   4 NTBneg_3    I    1   1000000 ! Number of neg Time-Based tracks for sec=3
   5 chi2pos_3   F    0.    9999. ! Chi^2 for positive tracks for sec=3
   6 chi2neg_3   F    0.    9999. ! Chi^2 for negative tracks for sec=3
   7 Nunknown_3  I    1   1000000 ! Number of unknown particles for sec=3
   8 Ngamma_3    I    1   1000000 ! Number of photons for sec=3
   9 Nelec_3     I    1   1000000 ! Number of electrons for sec=3
  10 Npiplus_3   I    1   1000000 ! Number of pi+ for sec=3
  11 Npiminus_3  I    1   1000000 ! Number of pi- for sec=3
  12 Nprot_3     I    1   1000000 ! Number of proton for sec=3
  13 Nkplus_3    I    1   1000000 ! Number of K+ for sec=3
  14 Nkminus_3   I    1   1000000 ! Number of K- for sec=3
  15 Ndeuteron_3 I    1   1000000 ! Number of deuterons for sec=3
  16 Nneutron_3  I    1   1000000 ! Number of neutrons for sec=3
!
 END TABLE
!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   PID4   B32   ! pid_mon sector 4 particle yields for mySQL
!
!COL ATT-name  FMT Min    Max   !Comments
   1 NHBpos_4    I    1   1000000 ! Number of pos Hit-Based tracks for sec=4
   2 NHBneg_4    I    1   1000000 ! Number of neg Hit-Based tracks for sec=4
   3 NTBpos_4    I    1   1000000 ! Number of pos Time-Based tracks  for sec=4
   4 NTBneg_4    I    1   1000000 ! Number of neg Time-Based tracks for sec=4
   5 chi2pos_4   F    0.    9999. ! Chi^2 for positive tracks for sec=4
   6 chi2neg_4   F    0.    9999. ! Chi^2 for negative tracks for sec=4
   7 Nunknown_4  I    1   1000000 ! Number of unknown particles for sec=4
   8 Ngamma_4    I    1   1000000 ! Number of photons for sec=4
   9 Nelec_4     I    1   1000000 ! Number of electrons for sec=4
  10 Npiplus_4   I    1   1000000 ! Number of pi+ for sec=4
  11 Npiminus_4  I    1   1000000 ! Number of pi- for sec=4
  12 Nprot_4     I    1   1000000 ! Number of proton for sec=4
  13 Nkplus_4    I    1   1000000 ! Number of K+ for sec=4
  14 Nkminus_4   I    1   1000000 ! Number of K- for sec=4
  15 Ndeuteron_4 I    1   1000000 ! Number of deuterons for sec=4
  16 Nneutron_4  I    1   1000000 ! Number of neutrons for sec=4   
!
 END TABLE
!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   PID5   B32   ! pid_mon sector 5 particle yields for mySQL
!
!COL ATT-name  FMT Min    Max   !Comments
   1 NHBpos_5    I    1   1000000 ! Number of pos Hit-Based tracks for sec=5
   2 NHBneg_5    I    1   1000000 ! Number of neg Hit-Based tracks for sec=5
   3 NTBpos_5    I    1   1000000 ! Number of pos Time-Based tracks  for sec=5
   4 NTBneg_5    I    1   1000000 ! Number of neg Time-Based tracks for sec=5
   5 chi2pos_5   F    0.    9999. ! Chi^2 for positive tracks for sec=5
   6 chi2neg_5   F    0.    9999. ! Chi^2 for negative tracks for sec=5
   7 Nunknown_5  I    1   1000000 ! Number of unknown particles for sec=5
   8 Ngamma_5    I    1   1000000 ! Number of photons for sec=5
   9 Nelec_5     I    1   1000000 ! Number of electrons for sec=5
  10 Npiplus_5   I    1   1000000 ! Number of pi+ for sec=5
  11 Npiminus_5  I    1   1000000 ! Number of pi- for sec=5
  12 Nprot_5     I    1   1000000 ! Number of proton for sec=5
  13 Nkplus_5    I    1   1000000 ! Number of K+ for sec=5
  14 Nkminus_5   I    1   1000000 ! Number of K- for sec=5
  15 Ndeuteron_5 I    1   1000000 ! Number of deuterons for sec=5
  16 Nneutron_5  I    1   1000000 ! Number of neutrons for sec=5 
!
 END TABLE
!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   PID6   B32   ! pid_mon sector 6 particle yields for mySQL
!
!COL ATT-name  FMT Min    Max   !Comments
   1 NHBpos_6    I    1   1000000 ! Number of pos Hit-Based tracks for sec=6
   2 NHBneg_6    I    1   1000000 ! Number of neg Hit-Based tracks for sec=6
   3 NTBpos_6    I    1   1000000 ! Number of pos Time-Based tracks  for sec=6
   4 NTBneg_6    I    1   1000000 ! Number of neg Time-Based tracks for sec=6
   5 chi2pos_6   F    0.    9999. ! Chi^2 for positive tracks for sec=6
   6 chi2neg_6   F    0.    9999. ! Chi^2 for negative tracks for sec=6
   7 Nunknown_6  I    1   1000000 ! Number of unknown particles for sec=6
   8 Ngamma_6    I    1   1000000 ! Number of photons for sec=6
   9 Nelec_6     I    1   1000000 ! Number of electrons for sec=6
  10 Npiplus_6   I    1   1000000 ! Number of pi+ for sec=6
  11 Npiminus_6  I    1   1000000 ! Number of pi- for sec=6
  12 Nprot_6     I    1   1000000 ! Number of proton for sec=6
  13 Nkplus_6    I    1   1000000 ! Number of K+ for sec=6
  14 Nkminus_6   I    1   1000000 ! Number of K- for sec=6
  15 Ndeuteron_6 I    1   1000000 ! Number of deuterons for sec=6
  16 Nneutron_6  I    1   1000000 ! Number of neutrons for sec=6 
!
 END TABLE
!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   PIDT   B32   ! pid_mon total particle yields for mySQL
!
!COL ATT-name  FMT Min    Max   !Comments
   1 Nunknown  I    1   1000000 ! Number of particles labelled unknown
   2 Ngamma    I    1   1000000 ! Number of photons
   3 Nelec     I    1   1000000 ! Number of electrons
   4 Npiplus   I    1   1000000 ! Number of pi+ 
   5 Npiminus  I    1   1000000 ! Number of pi-
   6 Nprot     I    1   1000000 ! Number of proton
   7 Nkplus    I    1   1000000 ! Number of K+
   8 Nkminus   I    1   1000000 ! Number of K-
   9 Ndeuteron I    1   1000000 ! Number of deuterons
  10 Nneutron  I    1   1000000 ! Number of neutrons
!
 END TABLE
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  PRTM    ! create write display delete ! Pretrigger calibration time
!
!   ATTributes:
!   -----------
!COL ATT-name 	   	    FMT  Min     Max     ! Comments
!     	      
  1 time                      I    0    9999999  ! time of pretrig calibration
!
 END TABLE
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  PSO  ! create write display delete ! down stream output bank
!		
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I    1     8             ! Counter Id ()
  2  TIME     F    -999999.9  999999.9 !  TDC time 
  3  ENER     F    -999999.9  999999.9  ! energy deposit
  4  TID      I    0     121            ! T id of the corresponding T
!
 END TABLE
!
!
 TABLE  PTDB  ! create write display delete !Polarized Target Data Bank
!
!   ATTributes:
!   -----------
!COL ATT-name  FMT  Min      Max        ! Comments
1   PBeam       I    0       10000      ! beam polarization (% x 100)
2   PB_DATE     I   100  100000000      ! date of measurement
3   TTYPE       I    0         100      ! Target Type 
4   B_TARG      I    0         600      ! Target holding field (Tesla x100)
5   PTarg       I    0       10000      ! Target polarization
6   PT_time     I   100  100000000      ! Time measured (according to PC)
7   HE_Level    I    0       10000      ! Liquid Helium Level(% x 100)
8   EIOF        I    0      140000      ! EIO microwave tube frequency (MHz)
9   TTempH      I    0       30000      ! Target temp measured by He Cell (K x100)
10  TTempC      I    0       30000      ! Target temp measured by Cernox (K x100)
11  AnealT      I   100  100000000      ! Date of last target anealing
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  RCST    ! create write display delete ! Roc status bank
!
!   ATTributes:
!   -----------
!COL ATT-name 	   	    FMT  Min     Max     ! Comments
!     	      
  1 down_count                I    0    9999999  ! download count
  2 prestart_count            I    0    9999999  ! prestart count
  3 go_count                  I    0    9999999  ! go count
  4 trig_count                I    0    9999999  ! total trigger count
  5 event_count               I    0    9999999  ! phys event count
  6 sync_count                I    0    9999999  ! force-sync event count
  7 run_trig_count            I    0    9999999  ! trigger count current run
  8 run_event_count           I    0    9999999  ! phys event count current run
  9 run_sync_count            I    0    9999999  ! force-sync event count current run
 10 pause_count               I    0    9999999  ! coda pause count
 11 end_count                 I    0    9999999  ! end count
 12 illegal_count             I    0    9999999  ! illegal count
 13 run_illegal_count         I    0    9999999  ! illegal count current run
 14 phys_sync_count           I    0    9999999  ! physics-sync event count
 15 run_phys_sync_count       I    0    9999999  ! physics-sync event count current run
!
 END TABLE
!
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  RF ! create write display delete ! RF result bank 
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  RF        F    0.  2.004  ! Best RF value in ns
  2  RF1       F    0.  200.   ! RF1 in ns
  3  RF2       F    0.  200.   ! RF2 in ns
!
 END TABLE
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  RGLK    ! create write display delete ! single region hits bank
!
! Positions and directions defined in CLAS coordinate system.
! Bank record is sector-wise.
!
!   ATTributes:
!   -----------
!COL ATT-name      FMT    Min    Max   ! Comments
!
  1  IREGION       I       1      3    ! region
  2  X             F    -999.   999.   ! X pos. of hit in CLAS (cm)
  3  Y             F    -999.   999.   ! Y pos. of hit in CLAS (cm)
  4  Z             F    -999.   999.   ! Z pos. of hit in CLAS (cm)
  5  THETA0        F    -999.   999.   ! polar angle of the link position (deg)
  6  PHI0          F    -999.   999.   ! azim. angle of the link position (deg)
  7  RTHETA        F    -999.   999.   ! polar angle of the link direction (deg)
  8  RPHI          F    -999.   999.   ! azim. angle of the link direction (deg)
  9  CHI2          F       0    999.   ! fit chi2        
 10  STATUS        I       0    100    ! MINUIT fit status (from 0=bad to 3=ok)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  RNLG    ! create write display delete ! Database archive bank 
!
!   ATTributes:
!   -----------
!COL ATT-name 	   	    FMT  Min     Max     ! Comments
!     	      
  1 sql                       A    0    9999999  ! Run log entry SQL statement
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  RNPE    ! create write display delete ! Calibration run index
!
!   ATTributes:
!   -----------
!COL ATT-name 	   	    FMT  Min     Max     ! Comments
!     	      
  1 run                       I    0    9999999  ! calibration run number
  2 bank                      A    0    9999999  ! bank name
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  RTSL     ! create write display delete ! Photon Flux Scaler bank 
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1   RAWT1      I     0    9999999  ! 
  2   RAWT2      I     0    9999999  ! 
  3   RAWT3      I     0    9999999  ! 
  4   RAWT4      I     0    9999999  ! 
  5   RAWT5      I     0    9999999  ! 
  6   RAWT6      I     0    9999999  ! 
  7   RAWT7      I     0    9999999  ! 
  8   RAWT8      I     0    9999999  ! 
  9   RAWT9      I     0    9999999  ! 
  10  RAWT10     I     0    9999999  ! 
  11  RAWT11     I     0    9999999  ! 
  12  RAWT12     I     0    9999999  ! 
  13  RAWT13     I     0    9999999  ! 
  14  RAWT14     I     0    9999999  ! 
  15  RAWT15     I     0    9999999  ! 
  16  RAWT16     I     0    9999999  ! 
  17  RAWT17     I     0    9999999  ! 
  18  RAWT18     I     0    9999999  ! 
  19  RAWT19     I     0    9999999  ! 
  20  RAWT20     I     0    9999999  ! 
  21  RAWT21     I     0    9999999  ! 
  22  RAWT22     I     0    9999999  ! 
  23  RAWT23     I     0    9999999  ! 
  24  RAWT24     I     0    9999999  ! 
  25  RAWT25     I     0    9999999  ! 
  26  RAWT26     I     0    9999999  ! 
  27  RAWT27     I     0    9999999  ! 
  28  RAWT28     I     0    9999999  ! 
  29  RAWT29     I     0    9999999  ! 
  30  RAWT30     I     0    9999999  ! 
  31  RAWT31     I     0    9999999  ! 
  32  RAWT32     I     0    9999999  ! 
  33  RAWT33     I     0    9999999  ! 
  34  RAWT34     I     0    9999999  ! 
  35  RAWT35     I     0    9999999  ! 
  36  RAWT36     I     0    9999999  ! 
  37  RAWT37     I     0    9999999  ! 
  38  RAWT38     I     0    9999999  ! 
  39  RAWT39     I     0    9999999  ! 
  40  RAWT40     I     0    9999999  ! 
  41  RAWT41     I     0    9999999  ! 
  42  RAWT42     I     0    9999999  ! 
  43  RAWT43     I     0    9999999  ! 
  44  RAWT44     I     0    9999999  ! 
  45  RAWT45     I     0    9999999  ! 
  46  RAWT46     I     0    9999999  ! 
  47  RAWT47     I     0    9999999  ! 
  48  RAWT48     I     0    9999999  ! 
  49  RAWT49     I     0    9999999  ! 
  50  RAWT50     I     0    9999999  ! 
  51  RAWT51     I     0    9999999  ! 
  52  RAWT52     I     0    9999999  ! 
  53  RAWT53     I     0    9999999  ! 
  54  RAWT54     I     0    9999999  ! 
  55  RAWT55     I     0    9999999  ! 
  56  RAWT56     I     0    9999999  ! 
  57  RAWT57     I     0    9999999  ! 
  58  RAWT58     I     0    9999999  ! 
  59  RAWT59     I     0    9999999  ! 
  60  RAWT60     I     0    9999999  ! 
  61  RAWT61     I     0    9999999  ! 
  62  RAWT62     I     0    9999999  ! 
  63  RAWT63     I     0    9999999  ! 
  64  RAWT64     I     0    9999999  ! 
!
 END TABLE
!
!       BANKname BANKtype      ! Comments
 TABLE  RUNC  ! create write display ! Run Control Map information
!
! Hit Based Tracking Particle ID
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max     ! Comments
!
  1  runno      I       1   999999   ! Run number extracted from map
  2  beam_e     F       1  99999.9   ! beam energy in MeV
  3  b_first    I       1  9999999   ! firsttime
  4  q_live     F       1  99999.9   ! Fcup * live time (1*E-10C)
  5  ql_first   I       1  9999999   ! firsttime	
  6  q_all      F       1  99999.9   ! Fcup (1*E-10C)
  5  qa_first   I       1  9999999   ! firsttime	
  6  tor_curr   F       1  99999.9   ! Torus Current (A)
  7  t_first    I       1  9999999   ! firsttime	
  8  mtor_curr  F       1  99999.9   ! Minitorus Current (A)
  9  mt_first   I       1  9999999   ! firsttime	
  8  tag_curr   F       1  99999.9   ! Tagger Current (A)
 10  tag_first  I       1  9999999   ! firsttime	
!
! see bosddl.h, clasRUNC_t containing 
! typedef struct { float val, int stat} runc_item_t;
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  S1ST    ! create write display delete ! Scaler bank 
!
!   ATTributes:
!   -----------
!COL ATT-name 	   	    FMT  Min     Max     ! Comments
!     	      
  1 latch1_bit1_count       I     0    9999999  ! Count trigger bit 1  latched events
  2 latch1_bit2_count       I     0    9999999  ! Count trigger bit 2  latched events
  3 latch1_bit3_count       I     0    9999999  ! Count trigger bit 3  latched events
  4 latch1_bit4_count       I     0    9999999  ! Count trigger bit 4  latched events
  5 latch1_bit5_count       I     0    9999999  ! Count trigger bit 5  latched events
  6 latch1_bit6_count       I     0    9999999  ! Count trigger bit 6  latched events
  7 latch1_bit7_count       I     0    9999999  ! Count trigger bit 7  latched events
  8 latch1_bit8_count       I     0    9999999  ! Count trigger bit 8  latched events
  9 latch1_bit9_count	    I     0    9999999  ! Count trigger bit 9  latched events
 10 latch1_bit10_count	    I     0    9999999  ! Count trigger bit 10 latched events
 11 latch1_bit11_count	    I     0    9999999  ! Count trigger bit 11 latched events
 12 latch1_bit12_count	    I     0    9999999  ! Count trigger bit 12 latched events
 13 event_count 	    I     0    9999999  ! Latched event count this run
 14 latch1_zero_count	    I     0    9999999  ! Latch1 zero count (illegal)
 15 latch1_empty_count	    I     0    9999999  ! Latch1 empty count (illegal)
 16 latch1_not_empty_count  I     0    9999999  ! Latch1 not empty on sync event (illegal)
 17 latch1_ok_count         I     0    9999999  ! Latch1 ok
 18 level2_sector1          I     0    9999999  ! Level2 sector1 count
 19 level2_sector2          I     0    9999999  ! Level2 sector2 count
 20 level2_sector3          I     0    9999999  ! Level2 sector3 count
 21 level2_sector4          I     0    9999999  ! Level2 sector4 count
 22 level2_sector5          I     0    9999999  ! Level2 sector5 count
 23 level2_sector6          I     0    9999999  ! Level2 sector6 count
 24 level2_pass             I     0    9999999  ! Level2 pass count
 25 level2_fail             I     0    9999999  ! Level2 fail count
 26 latch2_zero_count	    I     0    9999999  ! Latch2 zero count (illegal)
 27 latch2_empty_count	    I     0    9999999  ! Latch2 empty count (illegal)
 28 latch2_not_empty_count  I     0    9999999  ! Latch2 not empty on sync event (illegal)
 29 latch2_ok_count         I     0    9999999  ! Latch2 ok
 30 roc_13_count            I     0    9999999  ! Roc code 13 count (zero latch)
 31 roc_15_count            I     0    9999999  ! Roc code 15 count (illegal)
 32 l1l2_zero_count         I     0    9999999  ! (latch1==0)&&(latch2==0)
 33 l1zero_13_count         I     0    9999999  ! (latch1==0)&&(roc_code==13)
 34 l2zero_13_count         I     0    9999999  ! (latch2==0)&&(roc_code==13)
 35 l1l2zero_13_count       I     0    9999999  ! (latch1==0)&&(latch2==0)&&(roc_code==13)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SC1      ! create write display delete ! Scintillation counter hits bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       	I     1      48  ! the address of the hit detector element
  2  time_l     F     0   100000  ! time for left paddle(ns) 
  3  energy_l   F     0   65536  ! energy in left paddle(MeV) 
  4  time_r     F     0   65536  ! time for right paddle(ns) 
  5  energy_r   F     0   65536  ! energy in right paddle(MeV) 
  6  dtime_l    F   0  65536  ! uncertainty in time for left paddle(ns) 
  7  denergy_l  F   0  65536  ! uncertainty in energy in left paddle(MeV) 
  8  dtime_r    F   0  65536  ! uncertainty in time for right paddle(ns) 
  9  denergy_r  F   0  65536  ! uncertainty in energy in right paddle(MeV) 
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCC                 ! create write display delete ! GSIM & SC Scintillator calibration info
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
   1   id         I       1       0x00FF     ! paddle id# 
   2   date       I       0       0xFFFF     ! UNIX univiersal time of calibraton (32 bits) 
   3  version     I       0       0xFFFF     ! sequential version# of calibration 
   4  status      I       0       0xFFFF     ! 4 byte status word (see details) 
   5    TD0L      F      -999999.9 999999.9      ! (Left) gives 0 time at center for tube (ns) 
   6    TD0Lu     F      -999999.9 999999.9      ! (Left) uncertainty (ns) 
   7    TD0R      F      -999999.9 999999.9      ! (Right) gives 0 time at center for tube (ns) 
   8    TD0Ru     F      -999999.9 999999.9      ! (Right) uncertainty (ns) 
   9    TD1L      F      -999999.9 999999.9      ! (Left) TDC**1 coefficient (ns/ch) 
 10    TD1Lu      F      -999999.9 999999.9      ! (Left) uncertainty (ns/ch) 
 11    TD1R       F      -999999.9 999999.9      ! (Right) TDC**1 coefficient (ns/ch) 
 12    TD1Ru      F      -999999.9 999999.9      ! (Right) uncertainty (ns/ch) 
 13    TD2L       F      -999999.9 999999.9      ! (Left) TDC**2 coefficient 
 14    TD2Lu      F      -999999.9 999999.9      ! (Left) uncertainty (ns/ch) 
 15    TD2R       F      -999999.9 999999.9      ! (Right)TDC**2 coefficient 
 16    TD2Ru      F      -999999.9 999999.9      ! (Right) uncertainty (ns/ch) 
 17    TW0L       F      -999999.9 999999.9      ! (Left) time walk constant (ns) 
 18    TW0Lu      F      -999999.9 999999.9      ! (Left) uncertainty (ns) 
 19    TW0R       F      -999999.9 999999.9      ! (Right) time walk constant (ns) 
 20    TW0Ru      F      -999999.9 999999.9      ! (Right) uncertainty (ns) 
 21    TW1L       F      -999999.9 999999.9      ! (Left) time walk 1st factor 
 22    TW1Lu      F      -999999.9 999999.9      ! (Left) uncertainty 
 23    TW1R       F      -999999.9 999999.9      ! (Right) time walk 1st factor 
 24    TW1Ru      F      -999999.9 999999.9      ! (Right) uncertainty 
 25    TW2L       F      -999999.9 999999.9      ! (Left) time walk 2nd factor 
 26    TW2Lu      F      -999999.9 999999.9      ! (Left) uncertainty 
 27    TW2R       F      -999999.9 999999.9      ! (Right) time walk 2nd factor 
 28    TW2Ru      F      -999999.9 999999.9      ! (Right) uncertainty 
 29    ADPL       F      -999999.9 999999.9      ! (Left) ADC pedestal (ch) 
 30    ADPLu      F      -999999.9 999999.9      ! (Left) uncertainty 
 31    ADPR       F      -999999.9 999999.9      ! (Right) ADC pedestal (ch) 
 32    ADPRu      F      -999999.9 999999.9      ! (Right) uncertainty 
 33    M0L        F      -999999.9 999999.9      ! (Left) nmip adc channel 
 34    M0Lu       F      -999999.9 999999.9      ! (Left) uncertainty (ch) 
 35    M0R        F      -999999.9 999999.9      ! (Right)nmip adc channel 
 36    M0Ru       F      -999999.9 999999.9      ! (Right) uncertainty(ch) 
 37    VEFL       F      -999999.9 999999.9      ! (Left) effective velocity of light (cm/ns) 
 38    VEFLu      F      -999999.9 999999.9      ! (Left) uncertainty (cm/ns) 
 39    VEFR       F      -999999.9 999999.9      ! (Right) effective velocity of light (cm/ns) 
 40    VEFRu      F      -999999.9 999999.9      ! (Right) uncertainty (cm/ns) 
 41    ATNL       F      -999999.9 999999.9      ! (Left) attenuation length (cm) 
 42    ATNLu      F      -999999.9 999999.9      ! (Left) uncertainty (cm) 
 43    ATNR       F      -999999.9 999999.9      ! (Right) attenuation length (cm) 
 44    ATNRu      F      -999999.9 999999.9      ! (Right) uncertainty (cm) 
 45    TSIG0L     F      -999999.9 999999.9      ! (Left) 1st parameter of measured resolution (ns) 
 46    TSIG0R     F      -999999.9 999999.9      ! (Right) 1st parameter of measured resolution (ns) 
 47    TSIG1L     F      -999999.9 999999.9      ! (Left) 2nd parameter of measured resolution 
 48    TSIG1R     F      -999999.9 999999.9      ! (Right) 2nd parameter of measured resolution 
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SC       B16   ! create write display delete ! Scintillation counter event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I     1      48  ! the address of the hit detector element
  2  TDCL     I     0   65536  ! tdc information (channels)
  3  ADCL     I     0   65536  ! adc information (channels)
  4  TDCR     I     0   65536  ! tdc information (channels)
  5  ADCR     I     0   65536  ! adc information (channels)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCDI        ! create write display delete ! SC channel discriminator thresholds
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1   crate       I     0  10      ! CAMAC crate number
  1   slot        I     0  30      ! slot
  1   mask        I     0  999999  ! mask
  1   threshold   I     0  100000  ! actual threshold value (mV)
  1   width       I     0  100000  ! actual width value
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCGD  ! create write display delete !SC Geometry of paddles for DC (in SS) - M.K.
!
!   ATTributes:
!   -----------
!COL ATT-name  FMT  Min      Max        ! Comments
1   ID          I    0       0xFFFF     ! paddle # inside the plane (1-23(max))
2   NORM_Z      F  -999999.9 999999.9   ! Z(X') coordinate of unit normal to plane
3   NORM_X      F  -999999.9 999999.9   ! Z(X') coordinate of unit normal to plane
4   NORM_D      F  -999999.9 999999.9   ! distance to plane along unit vector
5   ALON_Z      F  -999999.9 999999.9   ! half width in X direction (along id's)
6   ALON_X      F  -999999.9 999999.9   ! half length of SC in Y direction
7   BEG_PD      F  -999999.9 999999.9   ! begin point of the paddle along id
8   END_PD      F  -999999.9 999999.9   ! end point of the paddle along id
9   PDL_SH      F  -999999.9 999999.9   ! shift of the paddle in respect to the Mid Plane
10  R_BEAM      F  -999999.9 999999.9   ! distance from the beam to the counte center in M.P.
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCG                 ! create write display delete ! GSIM & SC Scintillator geometry info
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
  1   id      I       1       0x00FF      !  paddle id#
  2   panel   I       1       0x00FF      ! panel number - to which plane (panel) the paddle belongs
  3  Xccw     F     -1000.     1000.      ! X center of CCW end of paddle in CLAS system
  4  Yccw     F     -1000.     1000.      ! Y center of CCW end 
  5  Zccw     F     -1000.     1000.      ! - Z center of CCW end
  6  Xcw      F    -1000.     1000.       ! X center of CW end 
  7  Ycw      F    -1000.     1000.       ! Y center of CW end
  8  Zcw      F    -1000.     1000.       !- Z center of CW end 
  9  WIDTH    F      0.         1         ! width (cm) (~|| CLAS theta) 
 10  THICK    F      0.         100.      ! thickness (cm) (~|| CLAS r) 
 11  DELTA    F    -100.      100.        ! the difference in length of the higher-theta slab minus the lower-theta slab divided by 4 (cm) 
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCH            ! create write display delete ! GSIM Scintillator hit info
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
  1  x     F   -1000.   2000. ! x of hit
  2  y     F   -1000.   2000. ! y of hit
  3  z     F   -1000.   2000. ! z of hit
  4  cx    F    -1.     1.    ! track x dir cosine
  5  cy    F    -1.     1.    ! track y dir cosine
  6  cz    F    -1.     1.    ! track z dir cosine
  7  pmom  F     0.    20.    ! track momentum
  8  track I     0    0xFFFF  ! track number
  9  id    I  -5000   5000    ! track PDG id
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCMD  ! create write display delete !Sc reconstruction bank for DC M.K.
!
!   ATTributes:
!   -----------
!COL ATT-name  FMT  Min      Max        ! Comments
1   ID          I    0       0xFFFF     ! paddle # 
2   STATUS      I    0       0xFFFF     ! general status information 
3   X_NORM      F  -999999.9 999999.9   ! X of unit vector normal to paddle
4   Y_NORM      F  -999999.9 999999.9   ! Y of unit vector normal to paddle
5   D_NORM      F    0.      999999.9   ! normal distance to paddle plane
6   X_PANEL     F  -999999.9 999999.9   ! X of unit vector along the panel
7   Y_PANEL     F  -999999.9 999999.9   ! Y of unit vector along the panel
8   B_PAD       F  -999999.9 999999.9   ! Begin of paddle along the panel
9   E_PAD       F  -999999.9 999999.9   ! End of paddle along the panel
10  TOF         F  -999999.9 999999.9   ! time of flight (nS)
11  EDP         F    0.      999999.9   ! energy deposited (GeV) 
12  P_TIME      F  -999999.9 999999.9   ! position i.r.t. mid.plane from time
13  P_ATLN      F  -999999.9 999999.9   ! position i.r.t. mid.plane from atten.
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCMT       ! create write display delete ! Mean SC pretrigger thresholds
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1   mean_thr    I     0  100000  ! mean threshold (mV)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCMW       ! create write display delete ! Mean SC pretrigger threshold width
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1   mean_width    I     0  100000  ! mean threshold width
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCPB    ! create write display delete ! EC hits involved in the event
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  ScPdHt   I     0    10      ! 10000*sector+100*SC_PD_ID+Hit_ID in SCR 
  2  Edep     F     0.    1.     ! Deposited energy (dE/dX)
  3  Time     F    -5.  150.     ! measured time 
  4  Path     F     0.  600.     ! Path lenght from target
  5  Chi2SC   F     0.   10.     ! Quality measure of geometrical matching
  6  Status   I     0   0xFFFF   ! Status word (not defined yet)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCP               ! create write display delete !Sc Scintillator panel geometry bank 
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
1    N1x        F   -1.0   +1.0      ! x component of outward normal to panel 1
2    N1y        F   -1.0   +1.0      ! y component of outward normal to panel 1
3    N1z        F   -1.0   +1.0      ! z component of outward normal to panel 1
4    R1n        F   0.0  9999.9      ! min.dist.inner(cm)from origin to panel 1
5    N2x        F   -1.0   +1.0      ! x component of outward normal to panel 2
6    N2y        F   -1.0   +1.0      ! y component of outward normal to panel 2
7    N2z        F   -1.0   +1.0      ! z component of outward normal to panel 2
8    R2n        F   0.0  9999.9      ! min.dist.inner(cm)from origin to panel 2
9    N3x        F   -1.0   +1.0      ! x component of outward normal to panel 3
10   N3y        F   -1.0   +1.0      ! y component of outward normal to panel 3
11   N3z        F   -1.0   +1.0      ! z component of outward normal to panel 3
12   R3n        F   0.0  9999.9      ! min.dist.inner(cm)from origin to panel 3
13   N4x        F   -1.0   +1.0      ! x component of outward normal to panel 4
14   N4y        F   -1.0   +1.0      ! y component of outward normal to panel 4
15   N4z        F   -1.0   +1.0      ! z component of outward normal to panel 4
16   R4n        F   0.0  9999.9      ! min.dist.inner(cm)from origin to panel 4
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCPE       ! create write display delete !Translated SC pedestal bank, measured during the pedestal run. 
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID              I     1      48  ! the address of the hit detector element
  2  mean_left       I     0   65536  ! left adc pedestal mean value (channel)
  3  sigma_left      F     0   65536  ! sigma of the pedestal distribution for left adc (channel)
  4  mean_right      I     0   65536  ! right adc pedestal mean value (channel)
  5  sigma_right     F     0   65536  ! sigma of the pedestal distribution for right adc (channel
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!    BANKname   BANKtype                    ! Comments
 TABLE  SCPS  ! create write display delete ! Sc plane geometry in SS
!
!   ATTributes:
!   -----------
!COL ATT-name  FMT  Min      Max        ! Comments
!---------------------------------------------------
1   ID          I    0       0xFFFF     ! Plane #(1-6):1-23,24-34,35-39,40-42,43-46,47-48
2   FIRSTPNUM   I    0       0xFFFF     ! The first paddle number in the plane
3   LASTPNUM    I    0       0xFFFF     ! The last paddle number in the plane
4   DISTANCE    F  -999999.9 999999.9   ! distance to the center of the plane
5   BEGALONSHFT F  -999999.9 999999.9   ! X-shift of Begin Plane poin along id of SC
6   ENDALONSHFT F  -999999.9 999999.9   ! X-shift of End of Plane poin along id of SC
7   LENGTHSHFT  F  -999999.9 999999.9   ! Y-shift of Plane SYS along the SC length
8   ALONGIDX   F  -999999.9 999999.9   ! XP.x direction for width in Sector System
9   ALONGIDY   F  -999999.9 999999.9   ! XP.y direction for width in Sector System
10  ALONGIDZ   F  -999999.9 999999.9   ! XP.z direction for width in Sector System
11  SLENGTHX   F  -999999.9 999999.9   ! YP.x direction for length in Sector System
12  SLENGTHY   F  -999999.9 999999.9   ! YP.y direction for length in Sector System
13  SLENGTHZ   F  -999999.9 999999.9   ! YP.z direction for length in Sector System
14  UNORMALX   F  -999999.9 999999.9   ! ZP.x direction for thickness in Sector System
15  UNORMALY   F  -999999.9 999999.9   ! ZP.y direction for thickness in Sector System
16  UNORMALZ   F  -999999.9 999999.9   ! ZP.z direction for thickness in Sector System
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
! Time-of-flight Cluster (final result) bank.
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCRC          ! create write display delete !Sc Scintillator reconstruction hit bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
1     id         I  0  48           ! cluster id   
2     energy     F  0  100000.0     ! cluster Energy (MeV) 
3     denergy    F  0  100000.0     ! error in cluster energy (ns)
4     time       F  0  100000.0     ! cluster (energy-weighted) time(ns) 
5     dtime	 F  0  100000.0	    ! error in cluster time (ns)
6     x       F -999999.9  999999.9  ! x position in sector coordinate system 
7     y       F -999999.9  999999.9  ! y position in sector coordinate system 
8     z       F -999999.9  999999.9  ! z position in sector coordinate system 
9     dx      F -999999.9  999999.9  ! x error in sector coordinate system 
10    dy      F -999999.9  999999.9  ! y error in sector coordinate system 
11    dz      F -999999.9  999999.9  ! z error in sector coordinate system 
12    status  I  -999999    999999   ! status word defined in sc.h
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCR          ! create write display delete !Sc Scintillator reconstruction hit bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
1     id         I  0  48           ! paddle id  
2     energy     F  0  100000.0     ! Energy (MeV) 
3     time       F  0  100000.0     ! time(ns)
4     x       F -999999.9  999999.9  ! x position in sector coodinate system 
5     y       F -999999.9  999999.9  ! y position in sector coodinate system 
6     z       F -999999.9  999999.9  ! z position in sector coodinate system 
7     dx      F -999999.9  999999.9  ! x error in sector coodinate system 
8     dy      F -999999.9  999999.9  ! y error in sector coodinate system 
9     dz      F -999999.9  999999.9  ! z error in sector coodinate system 
10    status  I  -999999    999999   ! status word defined in sc.h
11    denergy    F  0  100000.0     ! uncertainty in Energy (MeV) 
12    dtime      F  0  100000.0     ! uncertainty in time (ns) 
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SCS     ! create write display delete ! Time of Flight scaler bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1   SCS1      I     0    9999999  ! scaler sector 1
  2   SCS2      I     0    9999999  ! scaler sector 1
  3   SCS3      I     0    9999999  ! scaler sector 1
  4   SCS4      I     0    9999999  ! scaler sector 1
  5   SCS5      I     0    9999999  ! scaler sector 1
  6   SCS6      I     0    9999999  ! scaler sector 1
  7   SCS7      I     0    9999999  ! scaler sector 1
  8   SCS8      I     0    9999999  ! scaler sector 1
  9   SCS9      I     0    9999999  ! scaler sector 1
  10  SCS10     I     0    9999999  ! scaler sector 1
  11  SCS11     I     0    9999999  ! scaler sector 1
  12  SCS12     I     0    9999999  ! scaler sector 1
  13  SCS13     I     0    9999999  ! scaler sector 1
  14  SCS14     I     0    9999999  ! scaler sector 1
  15  SCS15     I     0    9999999  ! scaler sector 1
  16  SCS16     I     0    9999999  ! scaler sector 1
  17  SCS17     I     0    9999999  ! scaler sector 1
  18  SCS18     I     0    9999999  ! scaler sector 1
  19  SCS19     I     0    9999999  ! scaler sector 1
  20  SCS20     I     0    9999999  ! scaler sector 1
  21  SCS21     I     0    9999999  ! scaler sector 1
  22  SCS22     I     0    9999999  ! scaler sector 1
  23  SCS23     I     0    9999999  ! scaler sector 1
  24  SCS24     I     0    9999999  ! scaler sector 1
  25  SCS25     I     0    9999999  ! scaler sector 1
  26  SCS26     I     0    9999999  ! scaler sector 1
  27  SCS27     I     0    9999999  ! scaler sector 1
  28  SCS28     I     0    9999999  ! scaler sector 1
  29  SCS29     I     0    9999999  ! scaler sector 1
  30  SCS30     I     0    9999999  ! scaler sector 1
  31  SCS31     I     0    9999999  ! scaler sector 1
  32  SCS32     I     0    9999999  ! scaler sector 1
  33  SCS33     I     0    9999999  ! scaler sector 2
  34  SCS34     I     0    9999999  ! scaler sector 2
  35  SCS35     I     0    9999999  ! scaler sector 2
  36  SCS36     I     0    9999999  ! scaler sector 2
  37  SCS37     I     0    9999999  ! scaler sector 2
  38  SCS38     I     0    9999999  ! scaler sector 2
  39  SCS39     I     0    9999999  ! scaler sector 2
  40  SCS40     I     0    9999999  ! scaler sector 2
  41  SCS41     I     0    9999999  ! scaler sector 2
  42  SCS42     I     0    9999999  ! scaler sector 2
  43  SCS43     I     0    9999999  ! scaler sector 2
  44  SCS44     I     0    9999999  ! scaler sector 2
  45  SCS45     I     0    9999999  ! scaler sector 2
  46  SCS46     I     0    9999999  ! scaler sector 2
  47  SCS47     I     0    9999999  ! scaler sector 2
  48  SCS48     I     0    9999999  ! scaler sector 2
  49  SCS49     I     0    9999999  ! scaler sector 2
  50  SCS50     I     0    9999999  ! scaler sector 2
  51  SCS51     I     0    9999999  ! scaler sector 2
  52  SCS52     I     0    9999999  ! scaler sector 2
  53  SCS53     I     0    9999999  ! scaler sector 2
  54  SCS54     I     0    9999999  ! scaler sector 2
  55  SCS55     I     0    9999999  ! scaler sector 2
  56  SCS56     I     0    9999999  ! scaler sector 2
  57  SCS57     I     0    9999999  ! scaler sector 2
  58  SCS58     I     0    9999999  ! scaler sector 2
  59  SCS59     I     0    9999999  ! scaler sector 2
  60  SCS60     I     0    9999999  ! scaler sector 2
  61  SCS61     I     0    9999999  ! scaler sector 2
  62  SCS62     I     0    9999999  ! scaler sector 2
  63  SCS63     I     0    9999999  ! scaler sector 2
  64  SCS64     I     0    9999999  ! scaler sector 2
  65  SCS65     I     0    9999999  ! scaler sector 3
  66  SCS66     I     0    9999999  ! scaler sector 3
  67  SCS67     I     0    9999999  ! scaler sector 3
  68  SCS68     I     0    9999999  ! scaler sector 3
  69  SCS69     I     0    9999999  ! scaler sector 3
  70  SCS70     I     0    9999999  ! scaler sector 3
  71  SCS71     I     0    9999999  ! scaler sector 3
  72  SCS72     I     0    9999999  ! scaler sector 3
  73  SCS73     I     0    9999999  ! scaler sector 3
  74  SCS74     I     0    9999999  ! scaler sector 3
  75  SCS75     I     0    9999999  ! scaler sector 3
  76  SCS76     I     0    9999999  ! scaler sector 3
  77  SCS77     I     0    9999999  ! scaler sector 3
  78  SCS78     I     0    9999999  ! scaler sector 3
  79  SCS79     I     0    9999999  ! scaler sector 3
  80  SCS80     I     0    9999999  ! scaler sector 3
  81  SCS81     I     0    9999999  ! scaler sector 3
  82  SCS82     I     0    9999999  ! scaler sector 3
  83  SCS83     I     0    9999999  ! scaler sector 3
  84  SCS84     I     0    9999999  ! scaler sector 3
  85  SCS85     I     0    9999999  ! scaler sector 3
  86  SCS86     I     0    9999999  ! scaler sector 3
  87  SCS87     I     0    9999999  ! scaler sector 3
  88  SCS88     I     0    9999999  ! scaler sector 3
  89  SCS89     I     0    9999999  ! scaler sector 3
  90  SCS90     I     0    9999999  ! scaler sector 3
  91  SCS91     I     0    9999999  ! scaler sector 3
  92  SCS92     I     0    9999999  ! scaler sector 3
  93  SCS93     I     0    9999999  ! scaler sector 3
  94  SCS94     I     0    9999999  ! scaler sector 3
  95  SCS95     I     0    9999999  ! scaler sector 3
  96  SCS96     I     0    9999999  ! scaler sector 3
  97  SCS97     I     0    9999999  ! scaler sector 4
  98  SCS98     I     0    9999999  ! scaler sector 4
  99  SCS99     I     0    9999999  ! scaler sector 4
  100  SCS100     I     0    9999999  ! scaler sector 4
  101   SCS101      I     0    9999999  ! scaler sector 4
  102   SCS102      I     0    9999999  ! scaler sector 4
  103   SCS103      I     0    9999999  ! scaler sector 4
  104   SCS104      I     0    9999999  ! scaler sector 4
  105   SCS105      I     0    9999999  ! scaler sector 4
  106   SCS106      I     0    9999999  ! scaler sector 4
  107   SCS107      I     0    9999999  ! scaler sector 4
  108   SCS108      I     0    9999999  ! scaler sector 4
  109   SCS109      I     0    9999999  ! scaler sector 4
  110  SCS110     I     0    9999999  ! scaler sector 4
  111  SCS111     I     0    9999999  ! scaler sector 4
  112  SCS112     I     0    9999999  ! scaler sector 4
  113  SCS113     I     0    9999999  ! scaler sector 4
  114  SCS114     I     0    9999999  ! scaler sector 4
  115  SCS115     I     0    9999999  ! scaler sector 4
  116  SCS116     I     0    9999999  ! scaler sector 4
  117  SCS117     I     0    9999999  ! scaler sector 4
  118  SCS118     I     0    9999999  ! scaler sector 4
  119  SCS119     I     0    9999999  ! scaler sector 4
  120  SCS120     I     0    9999999  ! scaler sector 4
  121  SCS121     I     0    9999999  ! scaler sector 4
  122  SCS122     I     0    9999999  ! scaler sector 4
  123  SCS123     I     0    9999999  ! scaler sector 4
  124  SCS124     I     0    9999999  ! scaler sector 4
  125  SCS125     I     0    9999999  ! scaler sector 4
  126  SCS126     I     0    9999999  ! scaler sector 4
  127  SCS127     I     0    9999999  ! scaler sector 4
  128  SCS128     I     0    9999999  ! scaler sector 4
  129  SCS129     I     0    9999999  ! scaler sector 5
  130  SCS130     I     0    9999999  ! scaler sector 5
  131  SCS131     I     0    9999999  ! scaler sector 5
  132  SCS132     I     0    9999999  ! scaler sector 5
  133  SCS133     I     0    9999999  ! scaler sector 5
  134  SCS134     I     0    9999999  ! scaler sector 5
  135  SCS135     I     0    9999999  ! scaler sector 5
  136  SCS136     I     0    9999999  ! scaler sector 5
  137  SCS137     I     0    9999999  ! scaler sector 5
  138  SCS138     I     0    9999999  ! scaler sector 5
  139  SCS139     I     0    9999999  ! scaler sector 5
  140  SCS140     I     0    9999999  ! scaler sector 5
  141  SCS141     I     0    9999999  ! scaler sector 5
  142  SCS142     I     0    9999999  ! scaler sector 5
  143  SCS143     I     0    9999999  ! scaler sector 5
  144  SCS144     I     0    9999999  ! scaler sector 5
  145  SCS145     I     0    9999999  ! scaler sector 5
  146  SCS146     I     0    9999999  ! scaler sector 5
  147  SCS147     I     0    9999999  ! scaler sector 5
  148  SCS148     I     0    9999999  ! scaler sector 5
  149  SCS149     I     0    9999999  ! scaler sector 5
  150  SCS150     I     0    9999999  ! scaler sector 5
  151  SCS151     I     0    9999999  ! scaler sector 5
  152  SCS152     I     0    9999999  ! scaler sector 5
  153  SCS153     I     0    9999999  ! scaler sector 5
  154  SCS154     I     0    9999999  ! scaler sector 5
  155  SCS155     I     0    9999999  ! scaler sector 5
  156  SCS156     I     0    9999999  ! scaler sector 5
  157  SCS157     I     0    9999999  ! scaler sector 5
  158  SCS158     I     0    9999999  ! scaler sector 5
  159  SCS159     I     0    9999999  ! scaler sector 5
  160  SCS160     I     0    9999999  ! scaler sector 5
  161  SCS161     I     0    9999999  ! scaler sector 6
  162  SCS162     I     0    9999999  ! scaler sector 6
  163  SCS163     I     0    9999999  ! scaler sector 6
  164  SCS164     I     0    9999999  ! scaler sector 6
  165  SCS165     I     0    9999999  ! scaler sector 6
  166  SCS166     I     0    9999999  ! scaler sector 6
  167  SCS167     I     0    9999999  ! scaler sector 6
  168  SCS168     I     0    9999999  ! scaler sector 6
  169  SCS169     I     0    9999999  ! scaler sector 6
  170  SCS170     I     0    9999999  ! scaler sector 6
  171  SCS171     I     0    9999999  ! scaler sector 6
  172  SCS172     I     0    9999999  ! scaler sector 6
  173  SCS173     I     0    9999999  ! scaler sector 6
  174  SCS174     I     0    9999999  ! scaler sector 6
  175  SCS175     I     0    9999999  ! scaler sector 6
  176  SCS176     I     0    9999999  ! scaler sector 6
  177  SCS177     I     0    9999999  ! scaler sector 6
  178  SCS178     I     0    9999999  ! scaler sector 6
  179  SCS179     I     0    9999999  ! scaler sector 6
  180  SCS180     I     0    9999999  ! scaler sector 6
  181  SCS181     I     0    9999999  ! scaler sector 6
  182  SCS182     I     0    9999999  ! scaler sector 6
  183  SCS183     I     0    9999999  ! scaler sector 6
  184  SCS184     I     0    9999999  ! scaler sector 6
  185  SCS185     I     0    9999999  ! scaler sector 6
  186  SCS186     I     0    9999999  ! scaler sector 6
  187  SCS187     I     0    9999999  ! scaler sector 6
  188  SCS188     I     0    9999999  ! scaler sector 6
  189  SCS189     I     0    9999999  ! scaler sector 6
  190  SCS190     I     0    9999999  ! scaler sector 6
  191  SCS191     I     0    9999999  ! scaler sector 6
  192  SCS192     I     0    9999999  ! scaler sector 6
!
 END TABLE
!
 TABLE  SGMP  ! SEGMENTs BANK
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max    ! Comments
!
  1  is         I       0      10   ! superlayer number of the segment
  2  sgm1       I       0   65356   ! data structure see: include/bosddl.h, wire_t
  3  sgm2       I       0   65536   ! wire_t
  4  sgm3       I       0   65536   ! wire_t
  5  sgm4       I       0   65536   ! wire_t
  6  sgm5       I       0   65356   ! wire_t
  7  sgm6       I       0   65356   ! wire_t
!
!typedef struct {
!  short wireid;
!  short time;
!} wire_t;
!
!typedef struct {
!  int is;
!  wire_t data[6];
!} sgmp_t;
!
!typedef struct {
!  bankHeader_t bank;
!  sgmp_t sgmp[1];
!} clasSGMP_t;
!
!
 END TABLE
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SPAR       ! create write display delete ! Untranslated pedestal bank, read out from the hardware.
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1   slot        I     0  30      ! ADC slot
  2   channel     I     0  70      ! ADC channel
  3   spar        I     0  100000  ! sparsification threshold (channel)
  4   pedmean     I     0  100000  ! pedestal mean value measured, using internal 500ns gate (channel)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!
!       BANKname BANKtype      ! Comments
 TABLE  SPIN          ! create write display delete ! spin vector of the particle
!23
!   ATTributes:
!   -----------1
!COL ATT-name FMT Min    Max   ! Comments
!
  1  xspin       I    -1     1  ! x component of spin
  2  yspin       I    -1     1  ! y component of spin
  3  zspin       I    -1     1  ! z component of spin
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ST1          ! create write display delete ! Start counter intermediate results bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID      I     1       3  ! Pair id
  2  status  I     0     9999 ! status word
  3  TIME_1  F     -999999.9 999999.9 ! Time (ns) for side 1
  4  ADC_1   F     -999999.9 999999.9 ! Pedestal-subtracted adc for side 1
  5  TIME_2  F     -999999.9 999999.9 ! Time (ns) for side 2
  6  ADC_2   F     0.  999999.9   ! Pedestal-subtracted ADC  for side 2
!
 END TABLE
! 
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  ST       B16   ! create write display delete ! Start counter event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I     1      6  ! the address of the hit detector element
  2  TDC      I     0   65536  ! tdc information (channels)
  3  ADC      I     0   65536  ! adc information (channels)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  STG  ! create write display delete ! Start Counter Geometry bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  id        I     1         6      ! sector id
  2  leg_x_max F     -100.     100.   ! distance from beam axis to inner face of leg 
  3  leg_x_min F     -100.     100.   !
  4  leg_y_max F     -100.     100.   ! 
  5  leg_y_min F     -100.     100.   ! 
  6  leg_z_max F     -100.     100.   !
  7  let_z_min F     -100.     100.   ! 
  8  nose_x_max  F   -100.     100.   !  
  9  nose_x_min  F   -100.     100.   ! 
 10  nose_y_max  F   -100.     100.   !
 11  nose_y_min  F   -100.     100.   !
 12  nose_z_max  F   -100.     100.   !
 13  nose_z_min  F   -100.     100.   !
 14  noseAngle F     0.0       360.0  !
 15  lleg      F     0.0       100.0  !
 16  lnose     F     0.0       100.0  !
!
 END TABLE


!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  STH            ! create write display delete ! GSIM ST hit info
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
  1  x     F   -1000.   2000. ! x of hit
  2  y     F   -1000.   2000. ! y of hit
  3  z     F   -1000.   2000. ! z of hit
  4  cx    F    -1.     1.    ! track x dir cosine
  5  cy    F    -1.     1.    ! track y dir cosine
  6  cz    F    -1.     1.    ! track z dir cosine
  7  pmom  F     0.    20.    ! track momentum
  8  track I     0    0xFFFF  ! track number
  9  id    I  -5000   5000    ! track PDG id
 10  tof   F     0.     1000. ! flight time
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  STN0      B16   ! create write display delete ! NEW Start counter TDC bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I     1      24  ! detector element
  2  TDC      I     0   65536  ! tdc information (channels)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  STN1      B16   ! create write display delete ! NEW Start counter ADC bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I     1      24  ! detector element 
  2  ADC      I     0   65536  ! adc information (channels)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  STPB    ! create write display delete ! ST hits involved in the event
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  SThid    I     0    10      ! 100*sector+Hit_ID  
  2  Time     F    -5.  150.     ! Flight time relative to the evnt start time
  3  Path     F     0.  600.     ! Path lenght from target
  4  charge   I   -20.   20.     ! charge (get from tracking)
  5  Status   I     0   0xFFFF   ! Status word (not defined yet)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  STPE        ! create write display delete ! Translated start counter pedestal bank, measured during the pedestal run. 
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID        I     1      6   ! the address of the hit detector element
  2  mean      I     0   65536  ! adc pedestal mean value (channel)
  3  sigma     F     0   65536  ! sigma of the pedestal distribution (channel)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  STR          ! create write display delete ! Start counter results bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I     0       6  ! sector
  2  Trk_no   I     0     1000 ! Pointer to track in HBTR
  3  ST_TIME  F -999999.9  999999.9   ! flight time from ST (ns)
  4  ST_L     F     0.  999999.9   ! flight path from ST (cm)
  5  st_pos   F     0.  999999.9   ! position within the start counter
  6  status   I     0       10 	! Status word
!
 END TABLE 

!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  STS     ! create write display delete ! Start Counter scaler bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1   STS1      I     0    9999999  ! trigger rate sector 1-2
  2   STS2      I     0    9999999  ! trigger rate sector 3-4
  3   STS3      I     0    9999999  ! trigger rate sector 5-6
  4   STS4      I     0    9999999  ! 
  5   STS5      I     0    9999999  ! 
  6   STS6      I     0    9999999  ! 
  7   STS7      I     0    9999999  ! 
  8   STS8      I     0    9999999  ! 
  9   STS9      I     0    9999999  ! 
  10  STS10     I     0    9999999  ! 
  11  STS11     I     0    9999999  ! 
  12  STS12     I     0    9999999  ! 
  13  STS13     I     0    9999999  ! 
  14  STS14     I     0    9999999  ! 
  15  STS15     I     0    9999999  ! 
  16  STS16     I     0    9999999  ! 
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  STSN    ! create write display delete ! Start Counter scaler bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1   STSN1      I     0    9999999  !
  2   STSN2      I     0    9999999  !
  3   STSN3      I     0    9999999  !
  4   STSN4      I     0    9999999  ! 
  5   STSN5      I     0    9999999  ! 
  6   STSN6      I     0    9999999  ! 
  7   STSN7      I     0    9999999  ! 
  8   STSN8      I     0    9999999  ! 
  9   STSN9      I     0    9999999  ! 
  10  STSN10     I     0    9999999  ! 
  11  STSN11     I     0    9999999  ! 
  12  STSN12     I     0    9999999  ! 
  13  STSN13     I     0    9999999  ! 
  14  STSN14     I     0    9999999  ! 
  15  STSN15     I     0    9999999  ! 
  16  STSN16     I     0    9999999  ! 
  17  STSN17     I     0    9999999  ! 
  18  STSN18     I     0    9999999  ! 
  19  STSN19     I     0    9999999  ! 
  20  STSN20     I     0    9999999  ! 
  21  STSN21     I     0    9999999  ! 
  22  STSN22     I     0    9999999  ! 
  23  STSN23     I     0    9999999  ! 
  24  STSN24     I     0    9999999  ! 
  25  STOR       I     0    9999999  ! 
  26  STMULT     I     0    9999999  ! 
  27  STANDMOR   I     0    9999999  ! 
  28  MULTANDMOR I     0    9999999  ! 
  29  RES1       I     0    9999999  ! 
  30  RES2       I     0    9999999  ! 
  31  RES3       I     0    9999999  ! 
  32  RES4       I     0    9999999  ! 
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  SYNC     B16   ! create    ! Physics sync event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I     0      25  ! slot number
  2  TDCL     I     0   65536  ! count of missing responses
  3  ADCL     I     0   65536  ! count of extra buffers
!
 END TABLE

!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TACO  ! create write display delete ! down stream output bank
!		
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I    1          2         ! ID  1= TAC, 2 = USLG
  2  TIME     F    -999999.9  999999.9  !  TDC time 
  3  ELT      F    -999999.9  999999.9  ! energy deposit TAC = left top
  4  ERT      F    -999999.9  999999.9  ! energy deposit TAC = right top
  5  ELB      F    -999999.9  999999.9  ! energy deposit TAC = left bottom
  6  ERB      F    -999999.9  999999.9  ! energy deposit TAC = right bottom
  7  ESUM     F    -999999.9  999999.9  ! energy deposit TAC = sum1
  8  ESUM2    F    -999999.9  999999.9  ! energy deposit TAC = sum2
  9  ESUM3    F    -999999.9  999999.9  ! energy deposit TAC = sum3
 10  TID      I    0     121            ! T id of the corresponding T
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TAGE       B16  ! create write display delete 
!
! Tagger E-counter data event bank
! record no.: 1    (up to 16 entries per ID (multihit tdc))
! address ID:  | id1 (bit 8-15) | id2 (bit 0-7) |
!   for id1=0 or id1=1:   ID=(id1*256)+id2 = E-counter no.: 1...384
!   for id1=2:            id2=T-counter no.    (for id=1...61)
!                         id2=ST trigger for Sec.1&2,3&4,5&6  (for id2=62..64)
!   for id1=4:            id2=PS-counter       (for id2=1...32)
!                         id2=mid-level logic  (for id2=101-116)
!                
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I    1    1136   ! the address of the hit detector element
  2  TDC      I    0   32767   ! tdc information (channels) [multihit tdc]
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TAGI  ! create write display delete ! Tagger Intermediary event bank
!		
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  IDT      I    0     121    ! T id
  2  TIMEL    F    -999999.9 999999.9 ! time information (Left counters channels)
  3  TIMER    F    -999999.9 999999.9 ! time information (Right counters channels)
  4  IDE      I    1     767    ! E id
  5  TIMEE    F    -999999.9 999999.9 ! time information (E counters)
  6  TIMEMEAN F    -999999.9 999999.9 ! time information (left/right mean value | after alignement)
  7  TRF      F    -999999.9 999999.9 ! time information (mean val - RF | after alignement)
  8  NEXTTIME F    -999999.9 999999.9 ! time difference with the next T when in coincidence | after alignment)
!
 END TABLE
!
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TAGM  ! create write display delete ! Tagger result bank based on T-counter Multi-hit TDCs
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  energy  F         0.     10.  ! Energy of the photon in GeV
  2  t       F    -32000.  32000.  ! T-counter time (ns)
  3  e_t     F    -32000.  32000.  ! E-counter time (ns)
  4  status  I         0    4096   ! Status (not yet used) 
  5  tid     I         1     121   ! T channel Id
  6  eid     I         1     767   ! E channel Id
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TAGR  ! create write display delete ! Tagger result bank (AL-LYM-FR 9/29/1997 --- FYDW)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ERG       F    0.  10.     ! Energy of the photon in GeV
  2  TTAG      F    -20.  200.  ! Time of the photon has reconstructed in the Tagger
  3  TPHO      F    -20.  200.  ! Time of the photon after RF correction
  4  STAT      I    0   4096    ! Status ( 7 or 15 are Good) other values have problems (see tag_process_TAGR.F) 
  5  T_id      I    1   121     ! T counter Id
  6  E_id      I    1   767     ! E counter Id
!
 END TABLE
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TAGT       B16  ! create write display delete ! Tagger T countert event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I    1     62    ! the address of the hit detector element
  2  TDCL     I    0    4096   ! tdc information (Left counters channels)
  3  TDCR     I    0    4096   ! tdc information (Right counters channels)
!
 END TABLE
!
!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  TBER  ! create write display delete ! Time Based Tracking ERror bank
!  record_no=0
!
!  Fit parameter and Covariance matrix: (Cij)
!
!  Track# = row#  (cf. TBTR bank)
!  	note these are in the sda tracking coordinate system 
!           (x=beamline, y=radially outward, z=parallel to axial wires)
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  q_over_p   F      0.    100.   ! q/p
  2  lambda     F    -10.     10.   ! dip angle (pi/2 - theta)
  3  phi        F    -60.     60.   ! phi
  4  d0         F   -100.    100.   ! min.distance from (x=0,y=0,z=?)  [cm]
  5  z0         F   -100.    100.   ! z position of starting point     [cm] 
  6  c11        F    -10.     10.   ! element C{1,1}
  7  c12        F    -10.     10.   ! element C{1,2}
  8  c13        F    -10.     10.   ! element C{1,3}
  9  c14        F    -10.     10.   ! element C{1,4}
 10  c15        F    -10.     10.   ! element C{1,5}
 11  c22        F    -10.     10.   ! element C{2,2}
 12  c23        F    -10.     10.   ! element C{2,3}
 13  c24        F    -10.     10.   ! element C{2,4}
 14  c25        F    -10.     10.   ! element C{2,5}
 15  c33        F    -10.     10.   ! element C{3,3}
 16  c34        F    -10.     10.   ! element C{3,4}
 17  c35        F    -10.     10.   ! element C{3,5}
 18  c44        F    -10.     10.   ! element C{4,4}
 19  c45        F    -10.     10.   ! element C{4,5}
 20  c55        F    -10.     10.   ! element C{5,5}
 21  chi2       F      0.     50.   ! Chisquare for this Track
 22  layinfo1   I      0.      0.   ! layerhit info
 23  layinfo2   I      0.      0.   ! layerhit info&sector&track#in hber
! the layer hit info is stored in the following way
! for layinfo1= sum over each layer used in track(layers 1-30) Of 2^(layer#-1)
! for layinfo2 = sum of 2^(layer#-31) for (layers 31-36)
!	 	+ 256 * track# in sector+256^2*track# in hber 
!		+ 256^3 * sector
!
!RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!  
!
 END TABLE

!       BANKname BANKtype      ! Comments
 TABLE  TBID  ! create write display delete ! Time Based particle ID
!
! Hit Based Tracking Particle ID
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max     ! Comments
!
  1  track      I       1      100   ! Track (index to TBTR) or zero if neutral
  2  sec        I       1        6   ! Sector track is in
  3  beta       F      0.       1.   ! Beta of the track in units of c
  4  vtime      F      0.   10000.   ! vertex time of track
  5  sc_stat    I  -10000   100000   ! status of hit matching to SC: see sc.h
  6  sc_id      I       1       48   ! Pointer to SCRC bank
  7  sc_time    F -10000.   10000.   ! SC calibrated time for this track (ns)
  8  sc_qual    F      0.   10000.   ! quality of match for SC
  9  sc_vtime   F -10000.   10000.   ! time at vertex for SC(ns)
 10  sc_beta    F      0.       1.   ! Beta calculated from TOF from SC
 11  cc_stat    I  -10000   100000   ! status of hit matching to CC: see sc.h
 12  cc_id      I       1      100   ! pointer to CC01 bank
 13  cc_time    F -10000.   10000.   ! CC calibrated time for this track (ns)
 14  cc_qual    F      0.   10000.   ! quality of match for CC
 15  cc_vtime   F -10000.   10000.   ! time at vertex for CC(ns)
 16  cc_beta    F      0.       1.   ! Beta as calculated by the EC
 17  ec_stat    I  -10000   100000   ! status of hit matching to ec: see sc.h
 18  ec_id      I       1      100   ! Pointer to ECHB bank
 19  ec_time    F -10000.   10000.   ! EC calibrated time for this track (ns)
 20  ec_qual    F      0.   10000.   ! EC quality factor
 21  ec_vtime   F -10000.   10000.   ! time at vertex for EC(ns)
 22  ec_beta    F      0.       1.   ! Beta as calculated by the EC
 23  st_stat    I  -10000   100000   ! status of hit matching to ST
 24  st_id      I       1      100   ! Pointer to STR bank
 25  st_time    F -10000.   10000.   ! ST calibrated time for this track (ns)
 26  st_qual    F      0.   10000.   ! ST quality factor
 27  st_vtime   F -10000.   10000.   ! time at vertex for ST(ns)
 28  st_beta    F      0.       1.   ! Beta as calculated by the ST
 29  lac_stat    I  -10000   100000  ! status of hit matching to LAC
 30  lac_id      I       1      100  ! Pointer to EC1R bank
 31  lac_time    F -10000.   10000.  ! LAC calibrated time for this track (ns)
 32  lac_qual    F      0.   10000.  ! LAC quality factor
 33  lac_vtime   F -10000.   10000.  ! time at vertex for LAC(ns)
 34  lac_beta    F      0.       1.  ! Beta as calculated by the LAC
!
! see clasBID_t in pid.h
!
 END TABLE
!
!----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TBLA  ! create write display delete ! Time Based tracking LAyer bank
!                                             (output of Time Based Tracking)
!  record_no = sector# 
!
!  34 rows (=number of DC layers) for every track in this sector
!      ['trk_plane' counts 46 planes: Origin(1plane),Start_Cnt.(2planes),
!                           DC(36planes),CC(1plane),SC(4planes),EC(2planes)]
!    4 Layers in Superlayer 1 (Region 1 Stereo)  [=trk_planes  4-7 ]
!    6 Layers in Superlayer 2 (Region 1 Axial)   [=trk_planes 10-15]
!    6 Layers in Superlayer 3 (Region 2 Axial)   [=trk_planes 16-21]
!    6 Layers in Superlayer 4 (Region 2 Stereo)  [=trk_planes 22-27]
!    6 Layers in Superlayer 5 (Region 3 Axial)   [=trk_planes 28-33]
!    6 Layers in Superlayer 6 (Region 3 Stereo)  [=trk_planes 34-39]
!
!   Col 1 (trk_pln) allows pointing to other track banks (track_number)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max     ! Comments
!
  1  trk_pln   I    101    2000   ! (track_number) *100 + Trk_plane_number
  2  x         F  -1000.   1000.  ! z coord [cm]  for track in this plane
  3  y         F  -1000.   1000.  ! y coord [cm]  for track in this plane
  4  z         F  -1000.   1000.  ! z coord [cm]  for track in this plane
  5  Bx        F   -100.    100.  ! B-field in x [kG] at coord.{x,y,z}
  6  By        F   -100.    100.  ! B-field in y [kG] at coord.{x,y,z}
  7  Bz        F   -100.    100.  ! B-field in z [kG] at coord.{x,y,z}
  8  tlen      F      0.   1000.  ! track length [cm] from origin to this plane
  9  dc1       I      0    99999  ! Pointer to DC1 bank
 10  stat      I      1      100  ! Status of the hit
 11  wire      I      1      192  ! Wire number 
 12  dtime     F   -100.    5000. ! drift time  [ns]
 13  alpha     F    -40.      40. ! track angle (relative to R of SL) [deg]
 14  wlen      F      0.     200. ! Wire length (hit pos. to preamp)  [cm]
 15  sgdoca    F   0.001       5. ! sigma DOCA  [cm]
 16  fitdoca   F    -10.      10. ! Fitted DOCA [cm]
 17  calcdoca  F    -10.      10. ! calculated DOCA (via dtime)  [cm]
!
!RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!  
!
 END TABLE
!
!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  TBTR  ! create write display delete ! Time Based Tracking Result bank
! record_no= 0
!
! THE COORDINATE SYSTEM USED IN THIS BANK IS THE DETECTOR COORDINATE SYSTEM.
!
! (changed F.Klein Nov.97)
! col.9: trackno_in_Sector = track# in the Sector based Tracking Banks (TBLA,TDPL..)
! col.10: itr_hbt = track# for this track in the Hit Based Tracking Banks
!
!  ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  x          F    -50.     50.   ! x
  2  y          F    -50.     50.   ! y 'Vertex' position {x,y,z}
  3  z          F    -50.     50.   ! z
  4  px         F    -10.     10.   ! Px
  5  py         F    -10.     10.   ! Py  momentum at 'vertex' {Px,Py,Pz}
  6  pz         F    -10.     10.   ! Pz
  7  q          F     -1.      1.   ! charge   (straight tracks: set q=0.) 
  8  chi2       F      0.     50.   ! Chisquare for this Track
  9  itr_sec    I    101     620    ! Trackno_in_Sector + Sector*100
 10  itr_hbt    I      1      20    ! Trackno in HBTR for this track
!
!RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!  
!
 END TABLE

!-------------------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TCSB  ! create   delete ! Toroidal Coordinate System Bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  xpos       F  -100. 100.   ! x misalignment of TCS in HCS 		 
  2  ypos       F  -100. 100.   ! y misalignment of TCS in HCS	 
  3  zpos       F  -100. 100.   ! z misalignment of TCS in HCS	 
  4  sxpos      F    -1.   1.   ! sx sine of x-axis misorientation of TCS vs HCS		 
  5  sypos      F    -1.   1.   ! sy sine of y-axis misorientation of TCS vs HCS
  6  szpos      F    -1.   1.   ! sz sine of z-axis misorientation of TCS vs HCS
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
 END TABLE
!
!
!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  TDPL  ! create write display delete ! Tbt Detector PLane bank
!                                             (output of Time Based Tracking)
!
! THE COORDINATE SYSTEM USED IN THIS BANK IS THE SECTOR COORDINATE SYSTEM.
! 
! Record_no = sector#
!
! 10 rows per track in a sector  [i.e. for 2 tracks in one sector: 20 rows]:
!        1 plane for 'Vertex position (#1): closest distance to beamline
!        2 planes for Start Counter     (#2 , #3)
!        1 plane for Cerenkov Counter   (#40)
!        4 planes for Scint.Counters    (#41(=SCplane 1), #42(=SCplane 2),
!                                       (#43(=SCplanes 3&4),#44(=SCplanes5&6))
!        2 planes for Elmag.Calorimeter (#45(=Forw.EC), #46(=LA EC))
!      (same structure as HDPL (for Hit Based Tracking))
!
!   Col 1 (trk_pln) allows pointing to other track banks (track_number)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  trk_pln   I    101    2000   ! (track_number) *100 + plane_number
  2  x         F  -1000.   1000.  ! vector3_t pos;
  3  y         F  -1000.   1000.  !  (x, y, z coord. for track in on the plane)
  4  z         F  -1000.   1000.  ! 
  5  cx        F     -1.      1.  ! vector3_t dir;
  6  cy        F     -1.      1.  !  direction cosines (x,y,z) for track at coord.{x,y,z}
  7  cz        F     -1.      1.  ! 
  8  tlen      F      0.   1000.  ! track length [cm] from origin to this plane
!
!RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!  
!
 END TABLE
!

!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TESC          ! create write display delete 
!
!   TAG E-counter SCaler bank 
!   record no.=0 :  array of 384 values (count rate per E-counter channel)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  value    F   0.0   999999999.0  ! count rate per E-counter channel
!
 END TABLE!-----------------------------------------------------------------------
!       BANKname BANKtype                ! Comments
 TABLE  TGBI   B32  ! create write display delete ! Trigger bank
!
!   ATTributes:
!   -----------
!COL ATT-name 	    FMT  Min     Max     ! Comments
!     	      	    
  1 latch1          I      0    9999999  ! level1 trigger latch word (16 bits)
                       			 !   bits    descr
                       			 !   ----    -----
                       			 !    1-12   Level 1 input bits to trigger supervisor
                       			 !     13    not used
                       			 !     14    not used
                       			 !     15    Helicity clock
                       			 !     16    Helicity state
  2 helicity_scaler I      0    9999999  ! helicity interval count
  3 interrupt_time  I      0    9999999  ! interrupt time from microsec clock
  4 latch2          I      0    9999999  ! level2 trigger latch word (16 bits)
                                         !   bits    descr
                                         !   ----    -----
                                         !    1-8    level 2 sector bits
                                         !    9-12   not used
                                         !     13    level 2 fail
                                         !     14    level 2 pass
                                         !     15    level 2 fail
                                         !     16    level 2 pass
!
  5 level3          I      0    9999999  ! level3 trigger word (32 bits)
                                         !   bits    descr
                                         !   ----    -----
                                         !    1-6    level 3 sector bits
                                         !    7-24   not used
                                         !    25-32  version number
!
 END TABLE
!
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TGEO  ! create write display delete ! Target geometry bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  x         F  -200.  200.  ! X position of target
  2  y         F  -200.  200.  ! Y position of target
  3  z         F  -200.  200.  ! Z position of target
  4  radius    F  -200.  200.  ! radius of target
  5  lenght    F  -200.  200.  ! lenght of target
  6  material  I     0   100.  ! material of target: 0=empty; 1=Hydrogen; 
!
 END TABLE
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TGPB    ! create write display delete ! Tagger hits matched start counter
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  pointer    I     0    100     ! pointer to TAGR  
  2  Time       F -1000. 1000.     ! starttime_TAG at interaction point(ns)
  3  Energy     F     0.  100.     ! photon energy(GeV)
  4  dt     	F -1000. 1000.     ! starttime_ST - starttime_TAG (ns)
!				     if no starttime_ST, dt = -starttime_TAG
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TGS     ! create write display delete ! Photon Flux Scaler bank 
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1   RAWT1      I     0    9999999  ! 
  2   RAWT2      I     0    9999999  ! 
  3   RAWT3      I     0    9999999  ! 
  4   RAWT4      I     0    9999999  ! 
  5   RAWT5      I     0    9999999  ! 
  6   RAWT6      I     0    9999999  ! 
  7   RAWT7      I     0    9999999  ! 
  8   RAWT8      I     0    9999999  ! 
  9   RAWT9      I     0    9999999  ! 
  10  RAWT10     I     0    9999999  ! 
  11  RAWT11     I     0    9999999  ! 
  12  RAWT12     I     0    9999999  ! 
  13  RAWT13     I     0    9999999  ! 
  14  RAWT14     I     0    9999999  ! 
  15  RAWT15     I     0    9999999  ! 
  16  RAWT16     I     0    9999999  ! 
  17  RAWT17     I     0    9999999  ! 
  18  RAWT18     I     0    9999999  ! 
  19  RAWT19     I     0    9999999  ! 
  20  RAWT20     I     0    9999999  ! 
  21  RAWT21     I     0    9999999  ! 
  22  RAWT22     I     0    9999999  ! 
  23  RAWT23     I     0    9999999  ! 
  24  RAWT24     I     0    9999999  ! 
  25  RAWT25     I     0    9999999  ! 
  26  RAWT26     I     0    9999999  ! 
  27  RAWT27     I     0    9999999  ! 
  28  RAWT28     I     0    9999999  ! 
  29  RAWT29     I     0    9999999  ! 
  30  RAWT30     I     0    9999999  ! 
  31  RAWT31     I     0    9999999  ! 
  32  RAWT32     I     0    9999999  ! 
  33  RAWT33     I     0    9999999  ! 
  34  RAWT34     I     0    9999999  ! 
  35  RAWT35     I     0    9999999  ! 
  36  RAWT36     I     0    9999999  ! 
  37  RAWT37     I     0    9999999  ! 
  38  RAWT38     I     0    9999999  ! 
  39  RAWT39     I     0    9999999  ! 
  40  RAWT40     I     0    9999999  ! 
  41  RAWT41     I     0    9999999  ! 
  42  RAWT42     I     0    9999999  ! 
  43  RAWT43     I     0    9999999  ! 
  44  RAWT44     I     0    9999999  ! 
  45  RAWT45     I     0    9999999  ! 
  46  RAWT46     I     0    9999999  ! 
  47  RAWT47     I     0    9999999  ! 
  48  RAWT48     I     0    9999999  ! 
  49  RAWT49     I     0    9999999  ! 
  50  RAWT50     I     0    9999999  ! 
  51  RAWT51     I     0    9999999  ! 
  52  RAWT52     I     0    9999999  ! 
  53  RAWT53     I     0    9999999  ! 
  54  RAWT54     I     0    9999999  ! 
  55  RAWT55     I     0    9999999  ! 
  56  RAWT56     I     0    9999999  ! 
  57  RAWT57     I     0    9999999  ! 
  58  RAWT58     I     0    9999999  ! 
  59  RAWT59     I     0    9999999  ! 
  60  RAWT60     I     0    9999999  ! 
  61  RAWT61     I     0    9999999  ! 
  62  notused62  I     0    9999999  ! 
  63  notused63  I     0    9999999  ! 
  64  notused64  I     0    9999999  ! 
  65  BNK1T1     I     0    9999999  !
  66  BNK1T2     I     0    9999999  !
  67  BNK1T3     I     0    9999999  !
  68  BNK1T4     I     0    9999999  !
  69  BNK1T5     I     0    9999999  !
  70  BNK1T6     I     0    9999999  !
  71  BNK1T7     I     0    9999999  !
  72  BNK1T8     I     0    9999999  !
  73  BNK1T9     I     0    9999999  !
  74  BNK1T10    I     0    9999999  !
  75  BNK1T11    I     0    9999999  !
  76  BNK1T12    I     0    9999999  !
  77  BNK1T13    I     0    9999999  !
  78  BNK1T14    I     0    9999999  !
  79  BNK1T15    I     0    9999999  !
  80  BNK1T16    I     0    9999999  !
  81  BNK1T17    I     0    9999999  !
  82  BNK1T18    I     0    9999999  !
  83  BNK1T19    I     0    9999999  !
  84  BNK1T20    I     0    9999999  !
  85  BNK1T21    I     0    9999999  !
  86  BNK1T22    I     0    9999999  !
  87  BNK1T23    I     0    9999999  !
  88  BNK1T24    I     0    9999999  !
  89  BNK1T25    I     0    9999999  !
  90  BNK1T26    I     0    9999999  !
  91  BNK1T27    I     0    9999999  !
  92  BNK1T28    I     0    9999999  !
  93  BNK1T29    I     0    9999999  !
  94  BNK1T30    I     0    9999999  !
  95  BNK1T31    I     0    9999999  !
  96  BNK1T32    I     0    9999999  !
  97  BNK1T33    I     0    9999999  !
  98  BNK1T34    I     0    9999999  !
  99  BNK1T35    I     0    9999999  !
 100  BNK1T36    I     0    9999999  !
 101  BNK1T37    I     0    9999999  !
 102  BNK1T38    I     0    9999999  !
 103  BNK1T39    I     0    9999999  !
 104  BNK1T40    I     0    9999999  !
 105  BNK1T41    I     0    9999999  !
 106  BNK1T42    I     0    9999999  !
 107  BNK1T43    I     0    9999999  !
 108  BNK1T44    I     0    9999999  !
 109  BNK1T45    I     0    9999999  !
 110  BNK1T46    I     0    9999999  !
 111  BNK1T47    I     0    9999999  !
 112  BNK1T48    I     0    9999999  !
 113  BNK1T49    I     0    9999999  !
 114  BNK1T50    I     0    9999999  !
 115  BNK1T51    I     0    9999999  !
 116  BNK1T52    I     0    9999999  !
 117  BNK1T53    I     0    9999999  !
 118  BNK1T54    I     0    9999999  !
 119  BNK1T55    I     0    9999999  !
 120  BNK1T56    I     0    9999999  !
 121  BNK1T57    I     0    9999999  !
 122  BNK1T58    I     0    9999999  !
 123  BNK1T59    I     0    9999999  !
 124  BNK1T60    I     0    9999999  !
 125  BNK1T61    I     0    9999999  !
 126  notused126 I     0    9999999  !
 127  notused127 I     0    9999999  !
 128  notused128 I     0    9999999  !
 129  BNK2T1     I     0    9999999  !
 130  BNK2T2     I     0    9999999  !
 131  BNK2T3     I     0    9999999  !
 132  BNK2T4     I     0    9999999  !
 133  BNK2T5     I     0    9999999  !
 134  BNK2T6     I     0    9999999  !
 135  BNK2T7     I     0    9999999  !
 136  BNK2T8     I     0    9999999  !
 137  BNK2T9     I     0    9999999  !
 138  BNK2T10    I     0    9999999  !
 139  BNK2T11    I     0    9999999  !
 140  BNK2T12    I     0    9999999  !
 141  BNK2T13    I     0    9999999  !
 142  BNK2T14    I     0    9999999  !
 143  BNK2T15    I     0    9999999  !
 144  BNK2T16    I     0    9999999  !
 145  BNK2T17    I     0    9999999  !
 146  BNK2T18    I     0    9999999  !
 147  BNK2T19    I     0    9999999  !
 148  BNK2T20    I     0    9999999  !
 149  BNK2T21    I     0    9999999  !
 150  BNK2T22    I     0    9999999  !
 151  BNK2T23    I     0    9999999  !
 152  BNK2T24    I     0    9999999  !
 153  BNK2T25    I     0    9999999  !
 154  BNK2T26    I     0    9999999  !
 155  BNK2T27    I     0    9999999  !
 156  BNK2T28    I     0    9999999  !
 157  BNK2T29    I     0    9999999  !
 158  BNK2T30    I     0    9999999  !
 159  BNK2T31    I     0    9999999  !
 160  BNK2T32    I     0    9999999  !
 161  BNK2T33    I     0    9999999  !
 162  BNK2T34    I     0    9999999  !
 163  BNK2T35    I     0    9999999  !
 164  BNK2T36    I     0    9999999  !
 165  BNK2T37    I     0    9999999  !
 166  BNK2T38    I     0    9999999  !
 167  BNK2T39    I     0    9999999  !
 168  BNK2T40    I     0    9999999  !
 169  BNK2T41    I     0    9999999  !
 170  BNK2T42    I     0    9999999  !
 171  BNK2T43    I     0    9999999  !
 172  BNK2T44    I     0    9999999  !
 173  BNK2T45    I     0    9999999  !
 174  BNK2T46    I     0    9999999  !
 175  BNK2T47    I     0    9999999  !
 176  BNK2T48    I     0    9999999  !
 177  BNK2T49    I     0    9999999  !
 178  BNK2T50    I     0    9999999  !
 179  BNK2T51    I     0    9999999  !
 180  BNK2T52    I     0    9999999  !
 181  BNK2T53    I     0    9999999  !
 182  BNK2T54    I     0    9999999  !
 183  BNK2T55    I     0    9999999  !
 184  BNK2T56    I     0    9999999  !
 185  BNK2T57    I     0    9999999  !
 186  BNK2T58    I     0    9999999  !
 187  BNK2T59    I     0    9999999  !
 188  BNK2T60    I     0    9999999  !
 189  BNK2T61    I     0    9999999  !
 190  notused190 I     0    9999999  !
 191  notused191 I     0    9999999  !
 192  notused192 I     0    9999999  !
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TLV1     ! create write display delete ! Level1 trigger bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID       I   257    3000  ! id = Detector * 256 + channel
                               ! Detector = 1 for the CC   (16 input channels)
                               ! Detector = 2 for the TOF  (32 input channels doubles and triples)
                               ! Detector = 3 for the EC   (16 input channels)
                               ! Detector = 4 for the LAC  (16 input channels)
							   ! Detector = 5 for TS inputs
							   ! channel 1 Async 1
                               ! channel 2 Async 2
                               ! channel 3 MOR 1
                               ! channel 4 Start Counter 1
                               ! channels 5 -- 16 not used
                               ! channel 17 Prescale 1
                               ! channel 18 Prescale 2
                               ! channel 19 Prescale 3
                               ! channel 20 Prescale 4
                               ! channel 21 Prescale 5
                               ! channel 22 Prescale 6
                               ! channel 23 Prescale 7
                               ! channel 24 Prescale 8
                               ! channel 25 Prescale 9
                               ! channel 26 Prescale 10
                               ! channel 27 Prescale 11
                               ! channel 28 Prescale 12
                               ! channel 29 Start Counter 2
                               ! channel 30 MOR 2
                               ! channel 31 Helicity bit
                               ! channel 32 Helicity bit
   2  time     F     0   65536 ! time(ns) 
!
 END TABLE
!




!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TRCF    ! create write display delete! Trigger configuration
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  trigcfg   A     0    9999999  ! Trigger config file
!
 END TABLE
!




!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TRGS    ! create write display delete! Scaler bank 
!
!   ATTributes:
!   -----------
!COL ATT-name FMT  Min     Max     ! Comments
!
  1  CLOCK_UG     I     0    9999999  ! Clock ungated
  2  FCUP_UG      I     0    9999999  ! FCUP ungated
  3  Microsec     I     0    9999999  ! Microsecond clock (will overflow during normal run)
  4  random_ug    I     0    9999999  ! Random pulser ungated
  5  MOR_ST       I     0    9999999  ! MOR.ST
  6  MOR_PC       I     0    9999999  ! MOR.PC
  7  MOR_PS       I     0    9999999  ! MOR.PS
  8  MOR_TAC      I     0    9999999  ! MOR.TAC
  9  MOR          I     0    9999999  ! Master OR
 10  PC           I     0    9999999  ! Pair Counter
 11  PS           I     0    9999999  ! Pair Spectrometer
 12  TAC          I     0    9999999  ! Total Absorption Counter
 13  ST           I     0    9999999  ! ST
 14  hel_sync     I     0    9999999  ! Helicity sync signal
 15  clock_ug_2   I     0    9999999  ! Duplicate of channel 1
 16  fcup_ug_2    I     0    9999999  ! Duplicate of channel 2
 17  CLOCK_G1     I     0    9999999  ! Clock with run gatei
 18  FCUP_G1      I     0    9999999  ! FCUP with Run Gate
 19  NotUsed19    I     0    9999999  ! Currently not used
 20  random_g1    I     0    9999999  ! Random pulser run gated
 21  MOR_ST_rg    I     0    9999999  ! MOR.ST  with run gate
 22  MOR_PC_rg    I     0    9999999  ! MOR.PC with run gate
 23  MOR_PS_rg    I     0    9999999  ! MOR.PS with run gate
 24  MOR_TAC_rg   I     0    9999999  ! MOR.TAC with run gate
 25  MOR_rg       I     0    9999999  ! MASTER_OR with run gate
 26  PC_rg        I     0    9999999  ! PC with run gate
 27  PS_rg        I     0    9999999  ! PS with run gate
 28  TAC_rg       I     0    9999999  ! TAC with run gate
 29  ST_rg        I     0    9999999  ! ST  with run gate
 30  random_g1_2  I     0    9999999  ! duplicate of channel 20
 31  clock_g1_2   I     0    9999999  ! duplicate of channel 17
 32  fcup_g1_2    I     0    9999999  ! duplicate of channel 18
 33  CLOCK_G2     I     0    9999999  ! CLOCK live gated
 34  FCUP_G2      I     0    9999999  ! FCUP live gated
 35  trig_or_g2   I     0    9999999  ! Trigger OR of 1-12 live gated
 36  random_g2    I     0    9999999  ! Random pulser live gated
 37  NotUsed37    I     0    9999999  ! Currently not used
 38  NotUsed38    I     0    9999999  ! Currently not used
 39  NotUsed39    I     0    9999999  ! Currently not used
 40  NotUsed40    I     0    9999999  ! Currently not used
 41  MOR_lg       I     0    9999999  ! MASTER_OR live gated
 42  NotUsed42    I     0    9999999  ! Currently not used
 43  NotUsed43    I     0    9999999  ! Currently not used
 44  NotUsed44    I     0    9999999  ! Currently not used
 45  NotUsed45    I     0    9999999  ! Currently not used
 46  random_g2_2  I     0    9999999  ! duplicate of channel 36
 47  clock_g2_2   I     0    9999999  ! duplicate of channel 33
 48  fcup_g2_2    I     0    9999999  ! duplicate of channel 34

 49  trig1_ug     I     0    9999999  ! Trigger 1 ungated, prescaled
 50  trig2_ug     I     0    9999999  ! Trigger 2 ungated, prescaled
 51  trig3_ug     I     0    9999999  ! Trigger 3 ungated, prescaled
 52  trig4_ug     I     0    9999999  ! Trigger 4 ungated, prescaled
 53  trig5_ug     I     0    9999999  ! Trigger 5 ungated, prescaled
 54  trig6_ug     I     0    9999999  ! Trigger 6 ungated, prescaled
 55  trig7_ug     I     0    9999999  ! Trigger 7 ungated, prescaled
 56  trig8_ug     I     0    9999999  ! Trigger 8 ungated, prescaled
 57  trig9_ug     I     0    9999999  ! Trigger 9 ungated, prescaled
 58  trig10_ug    I     0    9999999  ! Trigger 10 ungated, prescaled
 59  trig11_ug    I     0    9999999  ! Trigger 11 ungated, prescaled
 60  trig12_ug    I     0    9999999  ! Trigger 12 ungated, prescaled
 61  trig_or_ug   I     0    9999999  ! Trigger OR of 1-12 ungated,
 62  l1accept     I     0    9999999  ! Level 1 accept
 63  notused63    I     0    9999999  ! Currently not used
 64  notused64    I     0    9999999  ! Currently not used
 65  l2fail       I     0    9999999  ! Level2 fail
 66  l2pass       I     0    9999999  ! Level2 pass
 67  l2start      I     0    9999999  ! Level2 start
 68  l2clear      I     0    9999999  ! Level2 clear
 69  l2accept     I     0    9999999  ! Level2 accept
 70  l3accept     I     0    9999999  ! Level3 accept
 71  notused71    I     0    9999999  ! Currently not used
 72  notused72    I     0    9999999  ! Currently not used
 73  l2sec1_g  	  I     0    9999999  ! Level2 sec1 gated
 74  l2sec2_g  	  I     0    9999999  ! Level2 sec2 gated
 75  l2sec3_g  	  I     0    9999999  ! Level2 sec3 gated
 76  l2sec4_g  	  I     0    9999999  ! Level2 sec4 gated
 77  l2sec5_g  	  I     0    9999999  ! Level2 sec5 gated
 78  l2sec6_g  	  I     0    9999999  ! Level2 sec6 gated
 79  l2_or_g      I     0    9999999  ! OR level2 gated
 80  l2_ok_g      I     0    9999999  ! Level 2 OK gated
 81  trig1_lg     I     0    9999999  ! Trigger 1 live gated
 82  trig2_lg     I     0    9999999  ! Trigger 2 live gated
 83  trig3_lg  	  I     0    9999999  ! Trigger 3 live gated
 84  trig4_lg  	  I     0    9999999  ! Trigger 4 live gated
 85  trig5_lg  	  I     0    9999999  ! Trigger 5 live gated
 86  trig6_lg  	  I     0    9999999  ! Trigger 6 live gated
 87  trig7_lg  	  I     0    9999999  ! Trigger 7 live gated
 88  trig8_lg  	  I     0    9999999  ! Trigger 8 live gated
 89  trig9_lg     I     0    9999999  ! Trigger 9 live gated
 90  trig10_lg    I     0    9999999  ! Trigger 10 live gated
 91  trig11_lg    I     0    9999999  ! Trigger 11 live gated
 92  trig12_lg    I     0    9999999  ! Trigger 12 live gated
 93  notused93	  I     0    9999999  ! not used
 94  notused94    I     0    9999999  ! not used
 95  ignore95     I     0    9999999  ! ignore
 96  ignore96  	  I     0    9999999  ! ignore
!
 END TABLE
!




!       BANKname BANKtype      ! Comments
 TABLE  TRKS  ! create write display delete ! TRacK Status Bank 
!
! THIS IS THE INTERPACKAGE MATCHING BANK
! modified: F.Klein 12/3/97: return id of CC,SC,EC that match to the track
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  trk_lev    I    100      420    ! Tracknr + analysis_level*100
  2  beta       F      0.       1.   ! Beta of the track in unit of c
  3  st_time    F  -1000.    1000.   ! Start time of the event (track: photon trigg)
  4  cc_time    F      0.    1000.   ! CC calibrated time for this track (ns)
  5  sc_time    F      0.    1000.   ! SC calibrated time for this track (ns)
  6  ec_time    F      0.    1000.   ! EC calibrated time for this track (ns)
  7  st_id      I      0       48    ! ST hit id (ptr. to STx: photon trigg)
                                     !   or SC paddle# (el. trigg)
  8  cc_id      I      0       10    ! CC hit id (ptr. to CCRC)
  9  sc_id      I      0       10    ! SC hit id (ptr. to SCRW)
 10  ec_id      I      0       10    ! EC Cluster-id (ptr. to ECHB)
!                                    ! hit_whole*10000+hit_inner*100+hit_outer
 END TABLE
!!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  TRL1  ! create write display delete ! Tracking Results for Layer 1
!        (additional tracking bank for EG1 that should be added to *BER)
!  record_no=0
!    (additional record_no=1 only after time based tracking: 
!              for all hit based tracks that didn't make it through 
!              time based tracking (after skipping 'pseudo' tracks))
!
!  Track# = row#  (cf. HBTR / TBTR banks)
!       fit_flags:  flags which were used in the fitting  (bit pattern)
!               bit0: transverse vertex constraint (width via dpar_SigmaBeam)
!               bit1: longitudinal vx. constraint (length via dpar_TargetLen)
!               bit2: propagation to variable vertex (otherwise: beamline)
!               bit8:  fit in DC layer 1 to 12 
!               bit9:  fit in DC layer 13 to 24 
!               bit10: fit in DC layer 25 to 36 
!               bit11: fit in DC axial superlayers only
!
!       hits_hbt, hits_tbt:  #hits per superlayer used in fit (decimal coded)
!                          nhits_per_superlayer *10^(superlayer-1)
!                             plus  10^6 for all 6 superlayers in fit
!       hits_hbt:  DC hits used in HBT fit  (usually all existing DC hits
!                                                           on the track)
!       hits_tbt:  DC hits used in TBT fit  (i.e. those which passed 
!                                                  left-right-ambiguity fits)
!       
!  	coordinates/directions for first DC layer in CLAS coordinate system
!       (first DC layer = layer 1, except for special fit flags: bits5-7)
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  fit_flags  I      0    65536   ! flags used in track fitting
  2  hits_hbt   I      0  1666666   ! hits used in HBT fit
  3  hits_tbt   I      0  1666666   ! hits used in TBT fit
  4  x          F   -300.    300.   ! x
  5  y          F   -300.    300.   ! y   position in first DC layer 
  6  z          F   -300.    300.   ! z
  7  cx         F     -1.      1.   ! cx
  8  cy         F     -1.      1.   ! cy  dir.cosine at first DC layer
  9  cz         F     -1.      1.   ! cz
 10  tlen       F      0.    300.   ! track length to this layer (starting
!                                      from 'vertex' in HBTR / TBTR
!
!RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  TRPB  ! create write display delete ! Tracking Results for Layer 1
!        (additional tracking bank for EG1 that should be added to *BER)
!  record_no=0
!       hits_hbt, hits_tbt:  #hits per superlayer used in fit (decimal coded)
!                          nhits_per_superlayer *10^(superlayer-1)
!                             plus  10^6 for all 6 superlayers in fit
!       hits_tbt:  DC hits used in TBT fit  (i.e. those which passed 
!                                                  left-right-ambiguity fits)
!       
!  	coordinates/directions for first DC layer in CLAS coordinate system
!       (first DC layer = layer 1, except for special fit flags: bits5-7)
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max   ! Comments
!
  1  hits_tbt   I      0  1666666   ! hits used in TBT fit
  2  x          F   -300.    300.   ! x
  3  y          F   -300.    300.   ! y   position in first DC layer 
  4  z          F   -300.    300.   ! z
  5  cx         F     -1.      1.   ! cx
  6  cy         F     -1.      1.   ! cy  dir.cosine at first DC layer
  7  cz         F     -1.      1.   ! cz
  8  tlen       F      0.    300.   ! track length to this layer (starting
!                                      from 'vertex' in HBTR / TBTR
!
!RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TSPR   B32 ! create write display delete ! Trigger supervisor program memory
!
!   ATTributes:
!   -----------
!COL ATT-name 	   	    FMT  Min     Max     ! Comments
!     	      
  1 mem                      I    0    9999999  ! memory value
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  TSRG  B32  ! create write display delete ! Trigger supervisor registers
!
!   ATTributes:
!   -----------
!COL ATT-name 	   	    FMT  Min     Max     ! Comments
!     	      
  1 reg                     I    0    9999999  ! register value
!
 END TABLE
!
!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   UNUS      ! create write display delete ! Unused hits/tracks bank
!
!COL ATT-name FMT Min    Max   !Comments
   1 NDCUN    I    0     20    ! Number of unused DC tracks
   2 IDCUN    I    0     20    ! Trek ID  
   1 NSCUN    I    0     20    ! Number of unused SC hits
   2 ISCUN    I    0     20    ! SC hit ID
   1 NCCUN    I    0     20    ! Number of unused CC hits
   2 ICCUN    I    0     20    ! CC hit ID
   1 NECUN    I    0     20    ! Number of unused EC hits
   2 IECUN    I    0     20    ! EC hit ID
!
 END TABLE
!
!********************************************************************
!       BANKname BANKtype      ! Comments
 TABLE  VERT  ! create write display delete ! vertex Result bank
!
! THE COORDINATE SYSTEM USED IN THIS BANK IS THE DETECTOR COORDINATE SYSTEM.
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max    ! Comments
!
  1  vertex     I   -1000    1000   ! vertex id
  2  trk1       I       0      10   ! track #1
  3  trk2       I       0      10   ! track #2
  4  x          F  -1000.   1000.   ! x vector3_t vert{x,y,z}
  5  y          F  -1000.   1000.   ! y 
  6  z          F  -1000.   1000.   ! z
  7  sepd       F   -100.    100.   ! seperation distance
!
! 
!     
!
 END TABLE


END$
