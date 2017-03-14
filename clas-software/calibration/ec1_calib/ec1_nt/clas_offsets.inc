
C    Offsets for the BMPR bank follow 

          integer BMPR_Q_TOT               !  Total Charge (Coulomb)
          parameter ( BMPR_Q_TOT = 1 ) 
          integer BMPR_Q_TOT_LV               !  Total charge in Live Time (Coulomb) 
          parameter ( BMPR_Q_TOT_LV = 2 ) 
          integer BMPR_TL_TU               !  ( Time Live ) / ( Time Ungated )
          parameter ( BMPR_TL_TU = 3 ) 
          integer BMPR_CURRENT               !  Current in Ampers
          parameter ( BMPR_CURRENT = 4 ) 

C    Offsets for the CALL bank follow 

          integer CALL_ID               !  catch-all element(RF,laser diode,etc)
          parameter ( CALL_ID = 1 ) 
          integer CALL_TDC               !  tdc information (channels)
          parameter ( CALL_TDC = 2 ) 
          integer CALL_ADC               !  adc information (channels)
          parameter ( CALL_ADC = 3 ) 

C    Offsets for the CC bank follow 

          integer CC_ID               !  the address of the hit detector element
          parameter ( CC_ID = 1 ) 
          integer CC_TDC               !  tdc information (channels)
          parameter ( CC_TDC = 2 ) 
          integer CC_ADC               !  adc information (channels)
          parameter ( CC_ADC = 3 ) 

C    Offsets for the CC01 bank follow 

          integer CC01_ID               !  the address of the hit detector element
          parameter ( CC01_ID = 1 ) 
          integer CC01_TIME               !  time(ns) 
          parameter ( CC01_TIME = 2 ) 
          integer CC01_N_PE               !  number of photoelectrons
          parameter ( CC01_N_PE = 3 ) 

C    Offsets for the CC1 bank follow 

          integer CC1_ID               !  the address of the hit detector element
          parameter ( CC1_ID = 1 ) 
          integer CC1_TDC               !  tdc information (channels)
          parameter ( CC1_TDC = 2 ) 
          integer CC1_ADC               !  adc information (channels)
          parameter ( CC1_ADC = 3 ) 

C    Offsets for the CCH bank follow 

          integer CCH_XIN               !  x pos track entry into cerenkov
          parameter ( CCH_XIN = 1 ) 
          integer CCH_YIN               !  y pos
          parameter ( CCH_YIN = 2 ) 
          integer CCH_ZIN               !  z pos
          parameter ( CCH_ZIN = 3 ) 
          integer CCH_XOUT               !  x pos track exit from cerenkov
          parameter ( CCH_XOUT = 4 ) 
          integer CCH_YOUT               !  y pos
          parameter ( CCH_YOUT = 5 ) 
          integer CCH_ZOUT               !  z pos
          parameter ( CCH_ZOUT = 6 ) 
          integer CCH_PMOM               !  track momentum 
          parameter ( CCH_PMOM = 7 ) 
          integer CCH_ID               !  track PDG id
          parameter ( CCH_ID = 8 ) 
          integer CCH_TKNUM               !  track number, 1000*istak+itra
          parameter ( CCH_TKNUM = 9 ) 
          integer CCH_SEGMENT               !  cc segment number of hit
          parameter ( CCH_SEGMENT = 10 ) 
          integer CCH_TDC               !  tdc for hit, 1 count = 50 psec
          parameter ( CCH_TDC = 11 ) 
          integer CCH_HIT               !  1 if cc hit made, 0 for no cc hit
          parameter ( CCH_HIT = 12 ) 

C    Offsets for the CCH1 bank follow 

          integer CCH1_TKNUM               !  track number, 1000*istak+itra
          parameter ( CCH1_TKNUM = 1 ) 
          integer CCH1_ID               !  track PDG id
          parameter ( CCH1_ID = 2 ) 
          integer CCH1_NHITS               !  number of CC hits per track
          parameter ( CCH1_NHITS = 3 ) 
          integer CCH1_SECTOR               !  sector number of track
          parameter ( CCH1_SECTOR = 4 ) 
          integer CCH1_SEGMENT               !  cc segment number of track
          parameter ( CCH1_SEGMENT = 5 ) 
          integer CCH1_PMOM               !  track momentum 
          parameter ( CCH1_PMOM = 6 ) 
          integer CCH1_XIN               !  x pos track entry into cerenkov
          parameter ( CCH1_XIN = 7 ) 
          integer CCH1_YIN               !  y pos
          parameter ( CCH1_YIN = 8 ) 
          integer CCH1_ZIN               !  z pos
          parameter ( CCH1_ZIN = 9 ) 
          integer CCH1_XOUT               !  x pos track exit from cerenkov
          parameter ( CCH1_XOUT = 10 ) 
          integer CCH1_YOUT               !  y pos
          parameter ( CCH1_YOUT = 11 ) 
          integer CCH1_ZOUT               !  z pos
          parameter ( CCH1_ZOUT = 12 ) 

C    Offsets for the CCH2 bank follow 

          integer CCH2_ITRNUM               !  track # for the hit
          parameter ( CCH2_ITRNUM = 1 ) 
          integer CCH2_SECTOR               !  sector number of hit
          parameter ( CCH2_SECTOR = 2 ) 
          integer CCH2_SEGMENT               !  cc segment # of hit
          parameter ( CCH2_SEGMENT = 3 ) 
          integer CCH2_ITDC               !  TOF of hit in channels (50 nsec/ch)
          parameter ( CCH2_ITDC = 4 ) 

C    Offsets for the CCPB bank follow 

          integer CCPB_IHIT               !  Bank row number 
          parameter ( CCPB_IHIT = 1 ) 
          integer CCPB_NPHE               !  Number of photo-electrons
          parameter ( CCPB_NPHE = 2 ) 
          integer CCPB_TIME               !  Flight time relative to the evnt start time
          parameter ( CCPB_TIME = 3 ) 
          integer CCPB_PATH               !  Path lenght from target
          parameter ( CCPB_PATH = 4 ) 
          integer CCPB_SECTOR               !  CC hit sector 
          parameter ( CCPB_SECTOR = 5 ) 
          integer CCPB_PARTID               !  Particle row number for CC hit 
          parameter ( CCPB_PARTID = 6 ) 
          integer CCPB_ICCHIT               !  CC hit ID 
          parameter ( CCPB_ICCHIT = 7 ) 
          integer CCPB_STATUS               !  Status word
          parameter ( CCPB_STATUS = 8 ) 

C    Offsets for the CCRC bank follow 

          integer CCRC_NRSECT               !  Sector #
          parameter ( CCRC_NRSECT = 1 ) 
          integer CCRC_NRSEGM               !  10 * Mean segment #
          parameter ( CCRC_NRSEGM = 2 ) 
          integer CCRC_NRSEGM_P               !  10 * Max segment # in the cluster
          parameter ( CCRC_NRSEGM_P = 3 ) 
          integer CCRC_NRSEGM_M               !  10 * Min segment # in the cluster
          parameter ( CCRC_NRSEGM_M = 4 ) 
          integer CCRC_NRPHE               !  Number of photoelectrons obtained
          parameter ( CCRC_NRPHE = 5 ) 
          integer CCRC_NRTIME               !  TOF in channels (50psec/channel)
          parameter ( CCRC_NRTIME = 6 ) 
          integer CCRC_NRTHET               !  Estimated angle Theta
          parameter ( CCRC_NRTHET = 7 ) 
          integer CCRC_NRDTHET               !  Estimated error of angle Theta
          parameter ( CCRC_NRDTHET = 8 ) 
          integer CCRC_NRPHY               !  Phy index 
          parameter ( CCRC_NRPHY = 9 ) 
          integer CCRC_NRIEC               !  Estimated i-coordinate in EC 
          parameter ( CCRC_NRIEC = 10 ) 
          integer CCRC_NRDIEC               !  Estimated error of i-coord. in EC
          parameter ( CCRC_NRDIEC = 11 ) 
          integer CCRC_NRSTAT               !  Status  word (yet unclear) 
          parameter ( CCRC_NRSTAT = 12 ) 

C    Offsets for the DC0 bank follow 

          integer DC0_ID               !  the address of the hit detector element
          parameter ( DC0_ID = 1 ) 
          integer DC0_TDC               !  tdc information (channels)
          parameter ( DC0_TDC = 2 ) 

C    Offsets for the DC1 bank follow 

          integer DC1_ID               !  the address of the hit detector element
          parameter ( DC1_ID = 1 ) 
          integer DC1_TIME               !  time(ns) 
          parameter ( DC1_TIME = 2 ) 

C    Offsets for the DCDW bank follow 

          integer DCDW_ID               !  ID_wire 
          parameter ( DCDW_ID = 1 ) 
          integer DCDW_TIDLY               !  TIme DeLaY (ns) 		 
          parameter ( DCDW_TIDLY = 2 ) 
          integer DCDW_STAT               !  wire status word 
          parameter ( DCDW_STAT = 3 ) 

C    Offsets for the DCGM bank follow 

          integer DCGM_X_CURVE               !  x center of curvature (cm)
          parameter ( DCGM_X_CURVE = 1 ) 
          integer DCGM_Y_CURVE               !  y center of curvature (cm)
          parameter ( DCGM_Y_CURVE = 2 ) 
          integer DCGM_Z_CURVE               !  z center of curvature (cm)
          parameter ( DCGM_Z_CURVE = 3 ) 
          integer DCGM_R_CURVE               !  radius of curvature (cm)
          parameter ( DCGM_R_CURVE = 4 ) 
          integer DCGM_THETA_START               !  angle of first logical wire WRT the center of curvature (rad)
          parameter ( DCGM_THETA_START = 5 ) 
          integer DCGM_D_THETA               !  delta theta between wires WRT center of curvature
          parameter ( DCGM_D_THETA = 6 ) 
          integer DCGM_X_NMID               !  x normal vector to the midplane
          parameter ( DCGM_X_NMID = 7 ) 
          integer DCGM_Y_NMID               !  y normal vector to the midplane
          parameter ( DCGM_Y_NMID = 8 ) 
          integer DCGM_Z_NMID               !  z normal vector to the midplane
          parameter ( DCGM_Z_NMID = 9 ) 
          integer DCGM_THETA_MIN               !  theta of first physical wire (rad)
          parameter ( DCGM_THETA_MIN = 10 ) 
          integer DCGM_THETA_MAX               !  theta of last physical wire (rad)
          parameter ( DCGM_THETA_MAX = 11 ) 
          integer DCGM_MIN_WIRE               !  minimum physical wire number
          parameter ( DCGM_MIN_WIRE = 12 ) 
          integer DCGM_MAX_WIRE               !  maximum physical wire number
          parameter ( DCGM_MAX_WIRE = 13 ) 
          integer DCGM_STEREO               !  approximate stereo angle
          parameter ( DCGM_STEREO = 14 ) 
          integer DCGM_CELL_SIZE               !  cell size (cm)
          parameter ( DCGM_CELL_SIZE = 15 ) 
          integer DCGM_X_NORM               !  x normal vector to the plane(region 1)
          parameter ( DCGM_X_NORM = 16 ) 
          integer DCGM_Y_NORM               !  y normal vector to the plane(region 1)
          parameter ( DCGM_Y_NORM = 17 ) 
          integer DCGM_Z_NORM               !  z normal vector to the plane(region 1)
          parameter ( DCGM_Z_NORM = 18 ) 
          integer DCGM_P_DIST               !  distance of plane to origin(cm) (region 1)
          parameter ( DCGM_P_DIST = 19 ) 
          integer DCGM_P_SEP               !  planar separation(cm) (region 1)
          parameter ( DCGM_P_SEP = 20 ) 
          integer DCGM_MAX_CYLW               !  maximum cylindrical wire
          parameter ( DCGM_MAX_CYLW = 21 ) 

C    Offsets for the DCGW bank follow 

          integer DCGW_X_MID               !  x at midplane of wire(cm)
          parameter ( DCGW_X_MID = 1 ) 
          integer DCGW_Y_MID               !  y at midplane of wire(cm)
          parameter ( DCGW_Y_MID = 2 ) 
          integer DCGW_Z_MID               !  z at midplane of wire(cm)
          parameter ( DCGW_Z_MID = 3 ) 
          integer DCGW_X_DIR               !  x direction cosine along wire (cm)
          parameter ( DCGW_X_DIR = 4 ) 
          integer DCGW_Y_DIR               !  y direction cosine along wire (cm)
          parameter ( DCGW_Y_DIR = 5 ) 
          integer DCGW_Z_DIR               !  z direction cosine along wire (cm)
          parameter ( DCGW_Z_DIR = 6 ) 
          integer DCGW_W_LEN               !  wire length from midplane to amplifier (-y)(cm)
          parameter ( DCGW_W_LEN = 7 ) 
          integer DCGW_W_LEN_HV               !  wire length from midplane to HV (+y)(cm)
          parameter ( DCGW_W_LEN_HV = 8 ) 

C    Offsets for the DCH bank follow 

          integer DCH_X               !  x at layer center
          parameter ( DCH_X = 1 ) 
          integer DCH_Y               !  y at layer center
          parameter ( DCH_Y = 2 ) 
          integer DCH_Z               !  z at layer center
          parameter ( DCH_Z = 3 ) 
          integer DCH_STEP               !  step size through layer
          parameter ( DCH_STEP = 4 ) 
          integer DCH_DEDX               !  energy deposit in layer
          parameter ( DCH_DEDX = 5 ) 
          integer DCH_PMOM               !  track momentum at layer center 
          parameter ( DCH_PMOM = 6 ) 
          integer DCH_TIME               !  time of hit at layer center
          parameter ( DCH_TIME = 7 ) 
          integer DCH_CX               !  track x dir cosine at layer center
          parameter ( DCH_CX = 8 ) 
          integer DCH_CY               !  track y dir cosine at layer center
          parameter ( DCH_CY = 9 ) 
          integer DCH_CZ               !  track z dir cosine at layer center
          parameter ( DCH_CZ = 10 ) 
          integer DCH_TRACK               !  track number
          parameter ( DCH_TRACK = 11 ) 
          integer DCH_ID               !  track PDG id
          parameter ( DCH_ID = 12 ) 
          integer DCH_LAYER               !  layer number
          parameter ( DCH_LAYER = 13 ) 

C    Offsets for the DCPB bank follow 

          integer DCPB_ITRK               !  Number of rows and Bank row number 
          parameter ( DCPB_ITRK = 1 ) 
          integer DCPB_PTRK               !  Reconstructed track momentum
          parameter ( DCPB_PTRK = 2 ) 
          integer DCPB_MTRK               !  Reconstructed mass
          parameter ( DCPB_MTRK = 3 ) 
          integer DCPB_CHARGE               !  charge
          parameter ( DCPB_CHARGE = 4 ) 
          integer DCPB_CX               !  x dir cosine at track origin
          parameter ( DCPB_CX = 5 ) 
          integer DCPB_CY               !  y dir cosine at track origin
          parameter ( DCPB_CY = 6 ) 
          integer DCPB_CZ               !  z dir cosine at track origin
          parameter ( DCPB_CZ = 7 ) 
          integer DCPB_X               !  X coordinate of vertex (cm)
          parameter ( DCPB_X = 8 ) 
          integer DCPB_Y               !  Y coordinate of vertex (cm)
          parameter ( DCPB_Y = 9 ) 
          integer DCPB_Z               !  Z coordinate of vertex (cm)
          parameter ( DCPB_Z = 10 ) 
          integer DCPB_CHI2               !  Chisquare of track fitting
          parameter ( DCPB_CHI2 = 11 ) 
          integer DCPB_SECTOR               !  DC track sector 
          parameter ( DCPB_SECTOR = 12 ) 
          integer DCPB_PARTID               !  Particle row number for DC track 
          parameter ( DCPB_PARTID = 13 ) 
          integer DCPB_IDCTRK               !  DC track ID
          parameter ( DCPB_IDCTRK = 14 ) 
          integer DCPB_STATUS               !  Status word
          parameter ( DCPB_STATUS = 15 ) 

C    Offsets for the DCV1 bank follow 

          integer DCV1_TSR1               !  Tsmear 
          parameter ( DCV1_TSR1 = 1 ) 
          integer DCV1_V0R1               !  drift velocity (slope)
          parameter ( DCV1_V0R1 = 2 ) 
          integer DCV1_TMR1               !  Maximum drift time (Tmax)
          parameter ( DCV1_TMR1 = 3 ) 
          integer DCV1_SP1R1               !  spare  
          parameter ( DCV1_SP1R1 = 4 ) 
          integer DCV1_SP2R1               !  spare	 
          parameter ( DCV1_SP2R1 = 5 ) 

C    Offsets for the DCV2 bank follow 

          integer DCV2_TS1R2               !  Tsmear 
          parameter ( DCV2_TS1R2 = 1 ) 
          integer DCV2_V01R2               !  drift velocity (slope)
          parameter ( DCV2_V01R2 = 2 ) 
          integer DCV2_VA1R2               !  drift velocity function parameter
          parameter ( DCV2_VA1R2 = 3 ) 
          integer DCV2_VB1R2               !  drift velocity function parameter  
          parameter ( DCV2_VB1R2 = 4 ) 
          integer DCV2_TM1R2               !  Maximum drift time (Tmax) 		 
          parameter ( DCV2_TM1R2 = 5 ) 
          integer DCV2_TA1R2               !  Tmax function parameter
          parameter ( DCV2_TA1R2 = 6 ) 
          integer DCV2_TB1R2               !  Tmax function parameter
          parameter ( DCV2_TB1R2 = 7 ) 
          integer DCV2_TS2R2               !  Tsmear 
          parameter ( DCV2_TS2R2 = 8 ) 
          integer DCV2_V02R2               !  drift velocity (slope)
          parameter ( DCV2_V02R2 = 9 ) 
          integer DCV2_VA2R2               !  drift velocity function parameter
          parameter ( DCV2_VA2R2 = 10 ) 
          integer DCV2_VB2R2               !  drift velocity function parameter  
          parameter ( DCV2_VB2R2 = 11 ) 
          integer DCV2_TM2R2               !  Maximum drift time (Tmax) 		 
          parameter ( DCV2_TM2R2 = 12 ) 
          integer DCV2_TA2R2               !  Tmax function parameter
          parameter ( DCV2_TA2R2 = 13 ) 
          integer DCV2_TB2R2               !  Tmax function parameter
          parameter ( DCV2_TB2R2 = 14 ) 

C    Offsets for the DCV3 bank follow 

          integer DCV3_TSR3               !  Tsmear 
          parameter ( DCV3_TSR3 = 1 ) 
          integer DCV3_V0R3               !  drift velocity (slope)
          parameter ( DCV3_V0R3 = 2 ) 
          integer DCV3_TMR3               !  Maximum drift time (Tmax)
          parameter ( DCV3_TMR3 = 3 ) 
          integer DCV3_SP1R3               !  spare  
          parameter ( DCV3_SP1R3 = 4 ) 
          integer DCV3_SP2R3               !  spare	 
          parameter ( DCV3_SP2R3 = 5 ) 

C    Offsets for the DDLY bank follow 

          integer DDLY_ID               !  ID_wire 
          parameter ( DDLY_ID = 1 ) 
          integer DDLY_TIDLY               !  TIme DeLaY (ns) 		 
          parameter ( DDLY_TIDLY = 2 ) 
          integer DDLY_STAT               !  wire STATus = 100*C + 10*A + B
          parameter ( DDLY_STAT = 3 ) 

C    Offsets for the DGEO bank follow 

          integer DGEO_ID_SEC               !  ID_sector 
          parameter ( DGEO_ID_SEC = 1 ) 
          integer DGEO_ID_REG               !  ID_region 
          parameter ( DGEO_ID_REG = 2 ) 
          integer DGEO_XPOS               !  x misalignment 		 
          parameter ( DGEO_XPOS = 3 ) 
          integer DGEO_YPOS               !  y misalignment 		 
          parameter ( DGEO_YPOS = 4 ) 
          integer DGEO_ZPOS               !  z misalignment 		 
          parameter ( DGEO_ZPOS = 5 ) 
          integer DGEO_SXPOS               !  sx sine of little x angle 		 
          parameter ( DGEO_SXPOS = 6 ) 
          integer DGEO_SYPOS               !  sy sine of little y angle 
          parameter ( DGEO_SYPOS = 7 ) 
          integer DGEO_SZPOS               !  sz sine of little z angle 
          parameter ( DGEO_SZPOS = 8 ) 

C    Offsets for the DTRK bank follow 

          integer DTRK_X               ! 
          parameter ( DTRK_X = 1 ) 
          integer DTRK_Y               ! 
          parameter ( DTRK_Y = 2 ) 
          integer DTRK_Z               ! 
          parameter ( DTRK_Z = 3 ) 

C    Offsets for the EC bank follow 

          integer EC_ID               !  the address of the hit detector element
          parameter ( EC_ID = 1 ) 
          integer EC_TDC               !  tdc information (channels)
          parameter ( EC_TDC = 2 ) 
          integer EC_ADC               !  adc information (channels)
          parameter ( EC_ADC = 3 ) 

C    Offsets for the EC01 bank follow 

          integer EC01_ID               !  the address of the hit detector element
          parameter ( EC01_ID = 1 ) 
          integer EC01_TIME               !  time for left paddle(ns) 
          parameter ( EC01_TIME = 2 ) 
          integer EC01_ENERGY               !  energy in left paddle(MeV) 
          parameter ( EC01_ENERGY = 3 ) 

C    Offsets for the EC1 bank follow 

          integer EC1_ID               !  the address of the hit detector element
          parameter ( EC1_ID = 1 ) 
          integer EC1_TDCL               !  tdc information (channels)
          parameter ( EC1_TDCL = 2 ) 
          integer EC1_ADCL               !  adc information (channels)
          parameter ( EC1_ADCL = 3 ) 
          integer EC1_TDCR               !  tdc information (channels)
          parameter ( EC1_TDCR = 4 ) 
          integer EC1_ADCR               !  adc information (channels)
          parameter ( EC1_ADCR = 5 ) 

C    Offsets for the EC1P bank follow 

          integer EC1P_N1X               !  x component of outward normal to plane
          parameter ( EC1P_N1X = 1 ) 
          integer EC1P_N1Y               !  y component of outward normal to plane
          parameter ( EC1P_N1Y = 2 ) 
          integer EC1P_N1Z               !  z component of outward normal to plane
          parameter ( EC1P_N1Z = 3 ) 
          integer EC1P_R1N               !  distanse in cm from origin to plane
          parameter ( EC1P_R1N = 4 ) 

C    Offsets for the EC1R bank follow 

          integer EC1R_E_TOT               !  cluster energy (sum of inner+outer)
          parameter ( EC1R_E_TOT = 1 ) 
          integer EC1R_DE_TOT               !   error on the cluster energy
          parameter ( EC1R_DE_TOT = 2 ) 
          integer EC1R_T_TOT               !  time
          parameter ( EC1R_T_TOT = 3 ) 
          integer EC1R_DT_TOT               !  error on the time
          parameter ( EC1R_DT_TOT = 4 ) 
          integer EC1R_X_M               !  x-in CLAS frame
          parameter ( EC1R_X_M = 5 ) 
          integer EC1R_Y_M               !  y-in CLAS frame
          parameter ( EC1R_Y_M = 6 ) 
          integer EC1R_Z_M               !  z-in CLAS frame
          parameter ( EC1R_Z_M = 7 ) 
          integer EC1R_DX_M               !  error on x
          parameter ( EC1R_DX_M = 8 ) 
          integer EC1R_DY_M               !  error on y
          parameter ( EC1R_DY_M = 9 ) 
          integer EC1R_DZ_M               !  error on z
          parameter ( EC1R_DZ_M = 10 ) 
          integer EC1R_E_IN               !   cluster energy in inner layer
          parameter ( EC1R_E_IN = 11 ) 
          integer EC1R_T_IN               !  time from inner layer
          parameter ( EC1R_T_IN = 12 ) 
          integer EC1R_X_IN               !  lab coordinate , inner layer
          parameter ( EC1R_X_IN = 13 ) 
          integer EC1R_Y_IN               !  lab coordinate , inner layer
          parameter ( EC1R_Y_IN = 14 ) 
          integer EC1R_X_OUT               !  lab coordinate , outer layer
          parameter ( EC1R_X_OUT = 15 ) 
          integer EC1R_Y_OUT               !  lab coordinate , outer layer
          parameter ( EC1R_Y_OUT = 16 ) 
          integer EC1R_X2_IN_L               !  second moment of x inner left
          parameter ( EC1R_X2_IN_L = 17 ) 
          integer EC1R_X2_IN_R               !  second moment of x inner right 
          parameter ( EC1R_X2_IN_R = 18 ) 
          integer EC1R_Y2_IN_L               !  second moment of y inner left
          parameter ( EC1R_Y2_IN_L = 19 ) 
          integer EC1R_Y2_IN_R               !  second moment of y inner right
          parameter ( EC1R_Y2_IN_R = 20 ) 
          integer EC1R_X2_OUT_L               !  second moment of x outer hit left
          parameter ( EC1R_X2_OUT_L = 21 ) 
          integer EC1R_X2_OUT_R               !  second moment of x outer hit right
          parameter ( EC1R_X2_OUT_R = 22 ) 
          integer EC1R_Y2_OUT_L               !  second moment of y outer hit left
          parameter ( EC1R_Y2_OUT_L = 23 ) 
          integer EC1R_Y2_OUT_R               !  second moment of y outer hit right
          parameter ( EC1R_Y2_OUT_R = 24 ) 
          integer EC1R_I_IN               !  cluster center in X inner short layer
          parameter ( EC1R_I_IN = 25 ) 
          integer EC1R_J_IN               !  cluster center in Y inner long layer
          parameter ( EC1R_J_IN = 26 ) 
          integer EC1R_I_OUT               !  cluster center in X outer layers
          parameter ( EC1R_I_OUT = 27 ) 
          integer EC1R_J_OUT               !  cluster center in Y outer layer
          parameter ( EC1R_J_OUT = 28 ) 
          integer EC1R_A_IN_XL               !  energy sum in inner short left pmts
          parameter ( EC1R_A_IN_XL = 29 ) 
          integer EC1R_A_IN_XR               !  energy sum in inner short right pmts
          parameter ( EC1R_A_IN_XR = 30 ) 
          integer EC1R_A_IN_YL               !  energy sum in inner long  left pmts
          parameter ( EC1R_A_IN_YL = 31 ) 
          integer EC1R_A_IN_YR               !  energy sum in inner long  right pmts
          parameter ( EC1R_A_IN_YR = 32 ) 
          integer EC1R_A_OUT_XL               !  energy sum in outer short left pmts
          parameter ( EC1R_A_OUT_XL = 33 ) 
          integer EC1R_A_OUT_XR               !  energy sum in outer short right pmts
          parameter ( EC1R_A_OUT_XR = 34 ) 
          integer EC1R_A_OUT_YL               !  energy sum in outer long left pmts
          parameter ( EC1R_A_OUT_YL = 35 ) 
          integer EC1R_A_OUT_YR               !  energy sum in outer long right pmts
          parameter ( EC1R_A_OUT_YR = 36 ) 
          integer EC1R_T_IN_XS               !  tdc sum in inner short righ+left  for cluster center
          parameter ( EC1R_T_IN_XS = 37 ) 
          integer EC1R_T_IN_XD               !  tdc dif in inner short right-left 
          parameter ( EC1R_T_IN_XD = 38 ) 
          integer EC1R_T_IN_YS               !  tdc sum in inner long  righ+left
          parameter ( EC1R_T_IN_YS = 39 ) 
          integer EC1R_T_IN_YD               !  tdc dif in inner long  right-left
          parameter ( EC1R_T_IN_YD = 40 ) 
          integer EC1R_T_OUT_XS               !  tdc sum in outer short righ+left 
          parameter ( EC1R_T_OUT_XS = 41 ) 
          integer EC1R_T_OUT_XD               !  tdc dif in outer short right-left 
          parameter ( EC1R_T_OUT_XD = 42 ) 
          integer EC1R_T_OUT_YS               !  tdc sum in outer long righ+left
          parameter ( EC1R_T_OUT_YS = 43 ) 
          integer EC1R_T_OUT_YD               !  tdc dif in outer long right-left 
          parameter ( EC1R_T_OUT_YD = 44 ) 
          integer EC1R_IBL               !  LAC  block number
          parameter ( EC1R_IBL = 45 ) 
          integer EC1R_NCLUSTER               !  1000xNclust4+100xNclust3+10xNclust2+Nclust1 
          parameter ( EC1R_NCLUSTER = 46 ) 
          integer EC1R_PMTFIRED               !  Number of fired pmt (more than threshold) 
          parameter ( EC1R_PMTFIRED = 47 ) 
          integer EC1R_Z_IN               !  Z in CLAS frame
          parameter ( EC1R_Z_IN = 48 ) 
          integer EC1R_Z_OUT               !  Z out in CLAS frame
          parameter ( EC1R_Z_OUT = 49 ) 
          integer EC1R_ISTAT               !  status word
          parameter ( EC1R_ISTAT = 50 ) 

C    Offsets for the ECCA bank follow 

          integer ECCA_ID               !  PMT ID
          parameter ( ECCA_ID = 1 ) 
          integer ECCA_APED               !  ADC pedestals (channels)
          parameter ( ECCA_APED = 2 ) 
          integer ECCA_ASIG               !  ADC pedestal variance (channels)
          parameter ( ECCA_ASIG = 3 ) 
          integer ECCA_AMIP               !  ADC calibration from MIP (N.I.M.P/ch.)
          parameter ( ECCA_AMIP = 4 ) 
          integer ECCA_AMIPU               !  aMIP Error
          parameter ( ECCA_AMIPU = 5 ) 
          integer ECCA_ASHR               !  ADC calibration from showers (GeV/ch.)
          parameter ( ECCA_ASHR = 6 ) 
          integer ECCA_ASHRU               !  aSHR Error
          parameter ( ECCA_ASHRU = 7 ) 
          integer ECCA_STAT               !  4 byte status word    
          parameter ( ECCA_STAT = 8 ) 

C    Offsets for the ECCL bank follow 

          integer ECCL_ID               !  PMT ID
          parameter ( ECCL_ID = 1 ) 
          integer ECCL_LDB               !  Stack atten length (database)(cm)
          parameter ( ECCL_LDB = 2 ) 
          integer ECCL_LDBU               !  lDB Error (cm)
          parameter ( ECCL_LDBU = 3 ) 
          integer ECCL_LMIP               !  Stack atten length (MinIonPart)(cm) 
          parameter ( ECCL_LMIP = 4 ) 
          integer ECCL_LMIPU               !  lMIP Error (cm)
          parameter ( ECCL_LMIPU = 5 ) 
          integer ECCL_LSHR               !  Stack atten length (showers)(cm)  
          parameter ( ECCL_LSHR = 6 ) 
          integer ECCL_LSHRU               !  lSHR Error (cm) 
          parameter ( ECCL_LSHRU = 7 ) 
          integer ECCL_STAT               !  4 byte status word    
          parameter ( ECCL_STAT = 8 ) 

C    Offsets for the ECCT bank follow 

          integer ECCT_ID               !  PMT ID
          parameter ( ECCT_ID = 1 ) 
          integer ECCT_TOFF               !  TDC offset (channels)
          parameter ( ECCT_TOFF = 2 ) 
          integer ECCT_TOFFU               !  tOFF Error
          parameter ( ECCT_TOFFU = 3 ) 
          integer ECCT_TGAIN               !  TDC conversion gain (nS/channel)
          parameter ( ECCT_TGAIN = 4 ) 
          integer ECCT_TGAINU               !  tGAIN Error
          parameter ( ECCT_TGAINU = 5 ) 
          integer ECCT_TW0               !  Time walk constant (channels)
          parameter ( ECCT_TW0 = 6 ) 
          integer ECCT_TW0U               !  tWOu Error
          parameter ( ECCT_TW0U = 7 ) 
          integer ECCT_TW1               !  Time walk correction parameter 
          parameter ( ECCT_TW1 = 8 ) 
          integer ECCT_TW1U               !  tW1u Error
          parameter ( ECCT_TW1U = 9 ) 
          integer ECCT_TVEF               !  Effective velocity of light (cm/ns)
          parameter ( ECCT_TVEF = 10 ) 
          integer ECCT_TVEFU               !  tVEFu Error
          parameter ( ECCT_TVEFU = 11 ) 
          integer ECCT_STAT               !  4 byte status word    
          parameter ( ECCT_STAT = 12 ) 

C    Offsets for the ECG bank follow 

          integer ECG_L0               !  distance from the target 
          parameter ( ECG_L0 = 1 ) 
          integer ECG_THE0               !  angle between the beam and perpendicular
          parameter ( ECG_THE0 = 2 ) 
          integer ECG_YLOW               !  
          parameter ( ECG_YLOW = 3 ) 
          integer ECG_YHI               !  
          parameter ( ECG_YHI = 4 ) 
          integer ECG_DYLOW               !  
          parameter ( ECG_DYLOW = 5 ) 
          integer ECG_DYHI               !  
          parameter ( ECG_DYHI = 6 ) 
          integer ECG_LRTH               !  thickness of the single layer
          parameter ( ECG_LRTH = 6 ) 
          integer ECG_TANGR               !  
          parameter ( ECG_TANGR = 7 ) 
          integer ECG_SECTOR               !  
          parameter ( ECG_SECTOR = 8 ) 
          integer ECG_PHISEC               !  
          parameter ( ECG_PHISEC = 9 ) 
          integer ECG_X0OFF               !  
          parameter ( ECG_X0OFF = 10 ) 
          integer ECG_Y0OFF               !  
          parameter ( ECG_Y0OFF = 11 ) 
          integer ECG_Z0OFF               !  
          parameter ( ECG_Z0OFF = 12 ) 
          integer ECG_ROTM11               !  
          parameter ( ECG_ROTM11 = 13 ) 
          integer ECG_ROTM12               !  
          parameter ( ECG_ROTM12 = 14 ) 
          integer ECG_ROTM13               !  
          parameter ( ECG_ROTM13 = 15 ) 
          integer ECG_ROTM21               !  
          parameter ( ECG_ROTM21 = 16 ) 
          integer ECG_ROTM22               !  
          parameter ( ECG_ROTM22 = 17 ) 
          integer ECG_ROTM23               !  
          parameter ( ECG_ROTM23 = 18 ) 
          integer ECG_ROTM31               !  
          parameter ( ECG_ROTM31 = 19 ) 
          integer ECG_ROTM32               !  
          parameter ( ECG_ROTM32 = 20 ) 
          integer ECG_ROTM33               !  
          parameter ( ECG_ROTM33 = 21 ) 

C    Offsets for the ECH bank follow 

          integer ECH_X               !  x of hit
          parameter ( ECH_X = 1 ) 
          integer ECH_Y               !  y of hit
          parameter ( ECH_Y = 2 ) 
          integer ECH_Z               !  z of hit
          parameter ( ECH_Z = 3 ) 
          integer ECH_CX               !  track x dir cosine
          parameter ( ECH_CX = 4 ) 
          integer ECH_CY               !  track y dir cosine
          parameter ( ECH_CY = 5 ) 
          integer ECH_CZ               !  track z dir cosine
          parameter ( ECH_CZ = 6 ) 
          integer ECH_PMOM               !  track momentum
          parameter ( ECH_PMOM = 7 ) 
          integer ECH_ID               !  track PDG id
          parameter ( ECH_ID = 8 ) 
          integer ECH_TOF               !  time of flight
          parameter ( ECH_TOF = 9 ) 
          integer ECH_EDEPIN               !  deposit energy (inner part)
          parameter ( ECH_EDEPIN = 10 ) 
          integer ECH_EDEPOUT               !  deposit energy (outer part)
          parameter ( ECH_EDEPOUT = 11 ) 

C    Offsets for the ECHB bank follow 

          integer ECHB_SECT               !  Sector number & Layer number
          parameter ( ECHB_SECT = 1 ) 
          integer ECHB_E__HIT               !  energy found 
          parameter ( ECHB_E__HIT = 2 ) 
          integer ECHB_DE_HIT               !  error on the energy found 
          parameter ( ECHB_DE_HIT = 3 ) 
          integer ECHB_T_HIT               !  time found 
          parameter ( ECHB_T_HIT = 4 ) 
          integer ECHB_DT_HI               !  error time found 
          parameter ( ECHB_DT_HI = 5 ) 
          integer ECHB_I_HIT               !  sector rectangular coordinate 
          parameter ( ECHB_I_HIT = 6 ) 
          integer ECHB_J_HIT               !  sector rectangular coordinate 
          parameter ( ECHB_J_HIT = 7 ) 
          integer ECHB_DI_HIT               !  sector rectangular coordinate error,
          parameter ( ECHB_DI_HIT = 8 ) 
          integer ECHB_DJ_HIT               !  sector rectangular coordinate error,
          parameter ( ECHB_DJ_HIT = 9 ) 
          integer ECHB_X_HIT               !  lab coordinate,
          parameter ( ECHB_X_HIT = 10 ) 
          integer ECHB_Y_HIT               !  lab coordinate,
          parameter ( ECHB_Y_HIT = 11 ) 
          integer ECHB_Z_HIT               !  lab coordinate,
          parameter ( ECHB_Z_HIT = 12 ) 
          integer ECHB_DX_HIT               !  lab coordinate error,
          parameter ( ECHB_DX_HIT = 13 ) 
          integer ECHB_DY_HIT               !  lab coordinate error, 
          parameter ( ECHB_DY_HIT = 14 ) 
          integer ECHB_DZ_HIT               !  lab coordinate error,
          parameter ( ECHB_DZ_HIT = 15 ) 
          integer ECHB_U2_HIT               !  second moment of u  _hit pattern
          parameter ( ECHB_U2_HIT = 16 ) 
          integer ECHB_V2_HIT               !  second moment of v  _hit pattern
          parameter ( ECHB_V2_HIT = 17 ) 
          integer ECHB_W2_HIT               !  second moment of w  _hit pattern
          parameter ( ECHB_W2_HIT = 18 ) 
          integer ECHB_U3_HIT               !  third moment of u  _hit pattern 
          parameter ( ECHB_U3_HIT = 19 ) 
          integer ECHB_V3_HIT               !  third moment of v  _hit pattern 
          parameter ( ECHB_V3_HIT = 20 ) 
          integer ECHB_W3_HIT               !  third moment of w  _hit pattern 
          parameter ( ECHB_W3_HIT = 21 ) 
          integer ECHB_U4_HIT               !  forth moment of u  _hit pattern 
          parameter ( ECHB_U4_HIT = 22 ) 
          integer ECHB_V4_HIT               !  forth moment of v  _hit pattern 
          parameter ( ECHB_V4_HIT = 23 ) 
          integer ECHB_W4_HIT               !  forth moment of w  _hit pattern 
          parameter ( ECHB_W4_HIT = 24 ) 
          integer ECHB_CENTR_U               !  peak position on U axis 
          parameter ( ECHB_CENTR_U = 25 ) 
          integer ECHB_CENTR_V               !  peak position on V axis 
          parameter ( ECHB_CENTR_V = 26 ) 
          integer ECHB_CENTR_W               !  peak position on W axis 
          parameter ( ECHB_CENTR_W = 27 ) 
          integer ECHB_PATH_U               !  path length from hit position to U axis 
          parameter ( ECHB_PATH_U = 28 ) 
          integer ECHB_PATH_V               !  path length from hit position to V axis 
          parameter ( ECHB_PATH_V = 29 ) 
          integer ECHB_PATH_W               !  path length from hit position to W axis 
          parameter ( ECHB_PATH_W = 30 ) 
          integer ECHB_NSTRP_U               !  Number of U strips in the hit
          parameter ( ECHB_NSTRP_U = 31 ) 
          integer ECHB_NSTRP_V               !  Number of V strips in the hit
          parameter ( ECHB_NSTRP_V = 32 ) 
          integer ECHB_NSTRP_W               !  Number of W strips in the hit
          parameter ( ECHB_NSTRP_W = 33 ) 
          integer ECHB_MATCHID1               !  Id of matched hit in the layer1
          parameter ( ECHB_MATCHID1 = 34 ) 
          integer ECHB_CH21               !  Quality measure of matching with layer1
          parameter ( ECHB_CH21 = 35 ) 
          integer ECHB_MATCHID2               !  Id of matched hit in the layer2
          parameter ( ECHB_MATCHID2 = 36 ) 
          integer ECHB_CH22               !  Quality measure of matching with layer2
          parameter ( ECHB_CH22 = 37 ) 
          integer ECHB_ISTAT               !  Number of hits & hit ID
          parameter ( ECHB_ISTAT = 38 ) 

C    Offsets for the ECP bank follow 

          integer ECP_N1X               !  x component of outward normal to plane
          parameter ( ECP_N1X = 1 ) 
          integer ECP_N1Y               !  y component of outward normal to plane
          parameter ( ECP_N1Y = 2 ) 
          integer ECP_N1Z               !  z component of outward normal to plane
          parameter ( ECP_N1Z = 3 ) 
          integer ECP_R1N               !  distanse in cm from origin to plane
          parameter ( ECP_R1N = 4 ) 
          integer ECP_PLW               !  whole plane depth
          parameter ( ECP_PLW = 5 ) 
          integer ECP_PLI               !  inner plane depth
          parameter ( ECP_PLI = 6 ) 
          integer ECP_PLO               !  outer plane depth
          parameter ( ECP_PLO = 7 ) 

C    Offsets for the ECPB bank follow 

          integer ECPB_IHIT               !  Bank row number 
          parameter ( ECPB_IHIT = 1 ) 
          integer ECPB_ID               !  Particle Data Group id
          parameter ( ECPB_ID = 2 ) 
          integer ECPB_ETOT               !  Reconstructed total energy
          parameter ( ECPB_ETOT = 3 ) 
          integer ECPB_EIN               !  Inner energy
          parameter ( ECPB_EIN = 4 ) 
          integer ECPB_EOUT               !  Outer energy 
          parameter ( ECPB_EOUT = 5 ) 
          integer ECPB_CHARGE               !  Charge
          parameter ( ECPB_CHARGE = 6 ) 
          integer ECPB_TIME               !  Flight time relative to the evnt start time
          parameter ( ECPB_TIME = 7 ) 
          integer ECPB_PATH               !  Path lenght from target
          parameter ( ECPB_PATH = 8 ) 
          integer ECPB_CX               !  x dir cosine at track origin
          parameter ( ECPB_CX = 9 ) 
          integer ECPB_CY               !  y dir cosine at track origin
          parameter ( ECPB_CY = 10 ) 
          integer ECPB_CZ               !  z dir cosine at track origin
          parameter ( ECPB_CZ = 11 ) 
          integer ECPB_SECTOR               !  EC hit sector 
          parameter ( ECPB_SECTOR = 12 ) 
          integer ECPB_PARTID               !  Particle row number for EC hit 
          parameter ( ECPB_PARTID = 13 ) 
          integer ECPB_IWHOLE               !  EC hit ID for the layer WHOLE 
          parameter ( ECPB_IWHOLE = 14 ) 
          integer ECPB_IINNER               !  EC hit ID for the layer INNER   
          parameter ( ECPB_IINNER = 15 ) 
          integer ECPB_IOUTER               !  EC hit ID for the layer OUTER  
          parameter ( ECPB_IOUTER = 16 ) 
          integer ECPB_STATUS               !  Status word
          parameter ( ECPB_STATUS = 17 ) 

C    Offsets for the ECPI bank follow 

          integer ECPI_ID               !  sector number
          parameter ( ECPI_ID = 1 ) 
          integer ECPI_LAYER               !  layer number, note 1-inner, 2-outer, 3-whole
          parameter ( ECPI_LAYER = 2 ) 
          integer ECPI_HITID               !  number of hits (first 16 bits) and hit ID (last 16 bits)
          parameter ( ECPI_HITID = 3 ) 
          integer ECPI_ILOC               !  position of the hit in the local coordinate system
          parameter ( ECPI_ILOC = 4 ) 
          integer ECPI_JLOC               !  position of the hit in the local coordinate system
          parameter ( ECPI_JLOC = 5 ) 
          integer ECPI_DILOC               !  i width of the hit
          parameter ( ECPI_DILOC = 6 ) 
          integer ECPI_DJLOC               !  j width of the hit 
          parameter ( ECPI_DJLOC = 7 ) 
          integer ECPI_R               !  radius of the shower
          parameter ( ECPI_R = 8 ) 
          integer ECPI_E               !  energy
          parameter ( ECPI_E = 9 ) 

C    Offsets for the ECPO bank follow 

          integer ECPO_ID               !  the address of the hit detector element
          parameter ( ECPO_ID = 1 ) 
          integer ECPO_UID               !  id of U view
          parameter ( ECPO_UID = 2 ) 
          integer ECPO_VID               !        V
          parameter ( ECPO_VID = 3 ) 
          integer ECPO_WID               !        W
          parameter ( ECPO_WID = 4 ) 
          integer ECPO_E               !  energy of pixels
          parameter ( ECPO_E = 5 ) 
          integer ECPO_T               !  
          parameter ( ECPO_T = 6 ) 
          integer ECPO_X               !  
          parameter ( ECPO_X = 7 ) 
          integer ECPO_Y               !  
          parameter ( ECPO_Y = 8 ) 
          integer ECPO_Z               !  
          parameter ( ECPO_Z = 9 ) 

C    Offsets for the ECRB bank follow 

          integer ECRB_E_IN               !  energy found for the inner layer
          parameter ( ECRB_E_IN = 1 ) 
          integer ECRB_E_OUT               !  energy found for the outer layer
          parameter ( ECRB_E_OUT = 2 ) 
          integer ECRB_DE_IN               !  error on the energy found for the inner layer
          parameter ( ECRB_DE_IN = 3 ) 
          integer ECRB_DE_OUT               !  error on the energy found for the outer layer
          parameter ( ECRB_DE_OUT = 4 ) 
          integer ECRB_T_IN               !  time found for the inner layer
          parameter ( ECRB_T_IN = 5 ) 
          integer ECRB_T_OUT               !  time found for the outer layer
          parameter ( ECRB_T_OUT = 6 ) 
          integer ECRB_DT_IN               !  error on the time found for the inner layer
          parameter ( ECRB_DT_IN = 7 ) 
          integer ECRB_DT_OUT               !  error on the time found for the outer layer
          parameter ( ECRB_DT_OUT = 8 ) 
          integer ECRB_I_IN               !  sector rectangular coordinate for the inner layer
          parameter ( ECRB_I_IN = 9 ) 
          integer ECRB_J_IN               !  sector rectangular coordinate for the inner layer
          parameter ( ECRB_J_IN = 10 ) 
          integer ECRB_I_OUT               !  sector rectangular coordinate for the outer layer
          parameter ( ECRB_I_OUT = 11 ) 
          integer ECRB_J_OUT               !  sector rectangular coordinate for the outer layer
          parameter ( ECRB_J_OUT = 12 ) 
          integer ECRB_DI_IN               !  sector rectangular coordinate error, inner layer
          parameter ( ECRB_DI_IN = 13 ) 
          integer ECRB_DJ_IN               !  sector rectangular coordinate error, inner layer
          parameter ( ECRB_DJ_IN = 14 ) 
          integer ECRB_DI_OUT               !  sector rectangular coordinate error, outer layer
          parameter ( ECRB_DI_OUT = 15 ) 
          integer ECRB_DJ_OUT               !  sector rectangular coordinate error, outer layer
          parameter ( ECRB_DJ_OUT = 16 ) 
          integer ECRB_X_IN               !  lab coordinate, inner layer
          parameter ( ECRB_X_IN = 17 ) 
          integer ECRB_Y_IN               !  lab coordinate, inner layer
          parameter ( ECRB_Y_IN = 18 ) 
          integer ECRB_Z_IN               !  lab coordinate, inner layer
          parameter ( ECRB_Z_IN = 19 ) 
          integer ECRB_X_OUT               !  lab coordinate, outer layer
          parameter ( ECRB_X_OUT = 20 ) 
          integer ECRB_Y_OUT               !  lab coordinate, outer layer
          parameter ( ECRB_Y_OUT = 21 ) 
          integer ECRB_Z_OUT               !  lab coordinate, outer layer
          parameter ( ECRB_Z_OUT = 22 ) 
          integer ECRB_DX_IN               !  lab coordinate error, inner layer
          parameter ( ECRB_DX_IN = 23 ) 
          integer ECRB_DY_IN               !  lab coordinate error, inner layer
          parameter ( ECRB_DY_IN = 24 ) 
          integer ECRB_DZ_IN               !  lab coordinate error, inner layer
          parameter ( ECRB_DZ_IN = 25 ) 
          integer ECRB_DX_OUT               !  lab coordinate error, outer layer
          parameter ( ECRB_DX_OUT = 26 ) 
          integer ECRB_DY_OUT               !  lab coordinate error, outer layer
          parameter ( ECRB_DY_OUT = 27 ) 
          integer ECRB_DZ_OUT               !  lab coordinate error, outer layer
          parameter ( ECRB_DZ_OUT = 28 ) 
          integer ECRB_U2_IN               !  second moment of u inner hit pattern
          parameter ( ECRB_U2_IN = 29 ) 
          integer ECRB_V2_IN               !  second moment of v inner hit pattern
          parameter ( ECRB_V2_IN = 30 ) 
          integer ECRB_W2_IN               !  second moment of w inner hit pattern
          parameter ( ECRB_W2_IN = 31 ) 
          integer ECRB_U2_OUT               !  second moment of u outer hit pattern
          parameter ( ECRB_U2_OUT = 32 ) 
          integer ECRB_V2_OUT               !  second moment of v outer hit pattern
          parameter ( ECRB_V2_OUT = 33 ) 
          integer ECRB_W2_OUT               !  second moment of w outer hit pattern
          parameter ( ECRB_W2_OUT = 34 ) 
          integer ECRB_U3_IN               !  third moment of u inner hit pattern 
          parameter ( ECRB_U3_IN = 35 ) 
          integer ECRB_V3_IN               !  third moment of v inner hit pattern 
          parameter ( ECRB_V3_IN = 36 ) 
          integer ECRB_W3_IN               !  third moment of w inner hit pattern 
          parameter ( ECRB_W3_IN = 37 ) 
          integer ECRB_U3_OUT               !  third moment of u outer hit pattern 
          parameter ( ECRB_U3_OUT = 38 ) 
          integer ECRB_V3_OUT               !  third moment of v outer hit pattern 
          parameter ( ECRB_V3_OUT = 39 ) 
          integer ECRB_W3_OUT               !  third moment of w outer hit pattern 
          parameter ( ECRB_W3_OUT = 40 ) 
          integer ECRB_I2               !  second moment of overall shower, sector coordinates
          parameter ( ECRB_I2 = 41 ) 
          integer ECRB_J2               !  second moment of overall shower, sector coordinates
          parameter ( ECRB_J2 = 42 ) 
          integer ECRB_I3               !  third moment of overall shower, sector coordinates
          parameter ( ECRB_I3 = 43 ) 
          integer ECRB_J3               !  third moment of overall shower, sector coordinates
          parameter ( ECRB_J3 = 44 ) 
          integer ECRB_SPARE1               !  spare
          parameter ( ECRB_SPARE1 = 45 ) 
          integer ECRB_SPARE2               !  spare
          parameter ( ECRB_SPARE2 = 46 ) 
          integer ECRB_SPARE3               !  spare
          parameter ( ECRB_SPARE3 = 47 ) 
          integer ECRB_SPARE4               !  spare
          parameter ( ECRB_SPARE4 = 48 ) 
          integer ECRB_ISTAT               !  status word
          parameter ( ECRB_ISTAT = 49 ) 

C    Offsets for the EID0 bank follow 

          integer EID0_JEID0SEC               !   Sector # for electron candidate
          parameter ( EID0_JEID0SEC = 1 ) 
          integer EID0_JEID0CC               !   Pointer to the hit in CCRC bank
          parameter ( EID0_JEID0CC = 2 ) 
          integer EID0_JEID0EC               !   Pointer to the hit in ECHB bank
          parameter ( EID0_JEID0EC = 3 ) 
          integer EID0_JEID0SC               !   Pointer to the hit in SCRX bank
          parameter ( EID0_JEID0SC = 4 ) 

C    Offsets for the EVNT bank follow 

          integer EVNT_IPART               !  Bank row number 
          parameter ( EVNT_IPART = 1 ) 
          integer EVNT_ID               !  Particle Data Group id
          parameter ( EVNT_ID = 2 ) 
          integer EVNT_PMOM               !  momentum
          parameter ( EVNT_PMOM = 3 ) 
          integer EVNT_MASS               !  mass
          parameter ( EVNT_MASS = 4 ) 
          integer EVNT_CHARGE               !  charge
          parameter ( EVNT_CHARGE = 5 ) 
          integer EVNT_BETTA               !  Particle velocity in the units of c
          parameter ( EVNT_BETTA = 6 ) 
          integer EVNT_CX               !  x dir cosine at track origin
          parameter ( EVNT_CX = 7 ) 
          integer EVNT_CY               !  y dir cosine at track origin
          parameter ( EVNT_CY = 8 ) 
          integer EVNT_CZ               !  z dir cosine at track origin
          parameter ( EVNT_CZ = 9 ) 
          integer EVNT_X               !  X coordinate of vertex (cm)
          parameter ( EVNT_X = 10 ) 
          integer EVNT_Y               !  Y coordinate of vertex (cm)
          parameter ( EVNT_Y = 11 ) 
          integer EVNT_Z               !  Z coordinate of vertex (cm)
          parameter ( EVNT_Z = 12 ) 
          integer EVNT_DCSTAT               !  Status of DC (=0 if DC is not involved) 
          parameter ( EVNT_DCSTAT = 13 ) 
          integer EVNT_CCSTAT               !  Status of CC (=0 if CC is not involved) 
          parameter ( EVNT_CCSTAT = 14 ) 
          integer EVNT_SCSTAT               !  Status of SC (=0 if SC is not involved)  
          parameter ( EVNT_SCSTAT = 15 ) 
          integer EVNT_ECSTAT               !  Status of EC (=0 if EC is not involved) 
          parameter ( EVNT_ECSTAT = 16 ) 
          integer EVNT_CHI2DC               !  Chisquare of track fitting
          parameter ( EVNT_CHI2DC = 17 ) 
          integer EVNT_CHI2CC               !  Chisquare of geometrical matching
          parameter ( EVNT_CHI2CC = 18 ) 
          integer EVNT_CHI2SC               !  Chisquare of geometrical matching
          parameter ( EVNT_CHI2SC = 19 ) 
          integer EVNT_CHI2EC               !  Chisquare of geometrical matching
          parameter ( EVNT_CHI2EC = 20 ) 
          integer EVNT_STATUS               !  Status word (=0 out of time particle)
          parameter ( EVNT_STATUS = 21 ) 

C    Offsets for the HBER bank follow 

          integer HBER_INV_P               !  1/p
          parameter ( HBER_INV_P = 1 ) 
          integer HBER_LAMBDA               !  dip angle 
          parameter ( HBER_LAMBDA = 2 ) 
          integer HBER_PHI               !  phi
          parameter ( HBER_PHI = 3 ) 
          integer HBER_D0               !  min.distance from origin in {x,y} [cm]
          parameter ( HBER_D0 = 4 ) 
          integer HBER_Z0               !  min.distance from beamline {x}    [cm]
          parameter ( HBER_Z0 = 5 ) 
          integer HBER_C11               !  element C{1,1}
          parameter ( HBER_C11 = 6 ) 
          integer HBER_C12               !  element C{1,2}
          parameter ( HBER_C12 = 7 ) 
          integer HBER_C13               !  element C{1,3}
          parameter ( HBER_C13 = 8 ) 
          integer HBER_C14               !  element C{1,4}
          parameter ( HBER_C14 = 9 ) 
          integer HBER_C15               !  element C{1,5}
          parameter ( HBER_C15 = 10 ) 
          integer HBER_C22               !  element C{2,2}
          parameter ( HBER_C22 = 11 ) 
          integer HBER_C23               !  element C{2,3}
          parameter ( HBER_C23 = 12 ) 
          integer HBER_C24               !  element C{2,4}
          parameter ( HBER_C24 = 13 ) 
          integer HBER_C25               !  element C{2,5}
          parameter ( HBER_C25 = 14 ) 
          integer HBER_C33               !  element C{3,3}
          parameter ( HBER_C33 = 15 ) 
          integer HBER_C34               !  element C{3,4}
          parameter ( HBER_C34 = 16 ) 
          integer HBER_C35               !  element C{3,5}
          parameter ( HBER_C35 = 17 ) 
          integer HBER_C44               !  element C{4,4}
          parameter ( HBER_C44 = 18 ) 
          integer HBER_C45               !  element C{4,5}
          parameter ( HBER_C45 = 19 ) 
          integer HBER_C55               !  element C{5,5}
          parameter ( HBER_C55 = 20 ) 

C    Offsets for the HBID bank follow 

          integer HBID_TRACK               !  Track Candidate (ptr to HBTR)
          parameter ( HBID_TRACK = 1 ) 
          integer HBID_SEC               !  Sector track is in
          parameter ( HBID_SEC = 2 ) 
          integer HBID_SC_STAT               !  SC status word (see sc.h)
          parameter ( HBID_SC_STAT = 3 ) 
          integer HBID_SC_ID               !  ID of scintillator hit
          parameter ( HBID_SC_ID = 4 ) 
          integer HBID_SC_TIME               !  SC calibrated time for this track (ns)
          parameter ( HBID_SC_TIME = 5 ) 
          integer HBID_SC_QUAL               !  quality of match for SC
          parameter ( HBID_SC_QUAL = 6 ) 
          integer HBID_SC_VTIME               !  time at vertex for SC(ns)
          parameter ( HBID_SC_VTIME = 7 ) 
          integer HBID_SC_BETA               !  Beta calculated from TOF from SC
          parameter ( HBID_SC_BETA = 8 ) 
          integer HBID_CC_STAT               !  CC status word (see cc.h)
          parameter ( HBID_CC_STAT = 9 ) 
          integer HBID_CC_ID               !  ID of CC counter hit (ptr to CC01)
          parameter ( HBID_CC_ID = 10 ) 
          integer HBID_CC_TIME               !  CC calibrated time for this track (ns)
          parameter ( HBID_CC_TIME = 11 ) 
          integer HBID_CC_QUAL               !  quality of match for CC
          parameter ( HBID_CC_QUAL = 12 ) 
          integer HBID_CC_VTIME               !  time at vertex for CC(ns)
          parameter ( HBID_CC_VTIME = 13 ) 
          integer HBID_CC_BETA               !  Beta as calculated by the CC
          parameter ( HBID_CC_BETA = 14 ) 
          integer HBID_EC_STAT               !  EC status word (see ec.h)
          parameter ( HBID_EC_STAT = 15 ) 
          integer HBID_EC_ID               !  ID of EC cluster hit
          parameter ( HBID_EC_ID = 16 ) 
          integer HBID_EC_TIME               !  EC calibrated time for this track (ns)
          parameter ( HBID_EC_TIME = 17 ) 
          integer HBID_EC_QUAL               !  EC quality factor
          parameter ( HBID_EC_QUAL = 18 ) 
          integer HBID_EC_VTIME               !  time at vertex for EC(ns)
          parameter ( HBID_EC_VTIME = 19 ) 
          integer HBID_EC_BETA               !  Beta as calculated by the EC
          parameter ( HBID_EC_BETA = 20 ) 
          integer HBID_BETA               !  Beta of the track in units of c
          parameter ( HBID_BETA = 21 ) 

C    Offsets for the HBLA bank follow 

          integer HBLA_TRK_PLN               !  (track_number) *100 + Trk_plane_number
          parameter ( HBLA_TRK_PLN = 1 ) 
          integer HBLA_X               !  z coord [cm]  for track in this plane
          parameter ( HBLA_X = 2 ) 
          integer HBLA_Y               !  y coord [cm]  for track in this plane
          parameter ( HBLA_Y = 3 ) 
          integer HBLA_Z               !  z coord [cm]  for track in this plane
          parameter ( HBLA_Z = 4 ) 
          integer HBLA_CX               !  direction cosine (cx) at coord.{x,y,z}
          parameter ( HBLA_CX = 5 ) 
          integer HBLA_CY               !  direction cosine (cy) at coord.{x,y,z}
          parameter ( HBLA_CY = 6 ) 
          integer HBLA_CZ               !  direction cosine (cz) at coord.{x,y,z}
          parameter ( HBLA_CZ = 7 ) 
          integer HBLA_TLEN               !  track length [cm] from origin to this plane
          parameter ( HBLA_TLEN = 8 ) 
          integer HBLA_DC1               !  Pointer to DC1 bank
          parameter ( HBLA_DC1 = 9 ) 
          integer HBLA_STAT               !  Status of the hit
          parameter ( HBLA_STAT = 10 ) 
          integer HBLA_WIRE               !  Wire number 
          parameter ( HBLA_WIRE = 11 ) 
          integer HBLA_DTIME               !  drift time [ns]  (not corrected for start ime)
          parameter ( HBLA_DTIME = 12 ) 
          integer HBLA_ALPHA               !  track angle (relative to R of SL) [deg]
          parameter ( HBLA_ALPHA = 13 ) 
          integer HBLA_WLEN               !  Wire length (hit pos. to preamp)  [cm]
          parameter ( HBLA_WLEN = 14 ) 
          integer HBLA_SGDOCA               !  sigma DOCA
          parameter ( HBLA_SGDOCA = 15 ) 
          integer HBLA_FITDOCA               !  Fitted DOCA
          parameter ( HBLA_FITDOCA = 16 ) 

C    Offsets for the HBTB bank follow 

          integer HBTB_TRK               !  Track Candidate 
          parameter ( HBTB_TRK = 1 ) 
          integer HBTB_SLY               !  Superlayer
          parameter ( HBTB_SLY = 2 ) 
          integer HBTB_ICL               !  Cluster# that matched to the track
          parameter ( HBTB_ICL = 3 ) 
          integer HBTB_ISG               !  Segment# in this cluster
          parameter ( HBTB_ISG = 4 ) 
          integer HBTB_ID1               !  Ptr. to DC1 entry  for 1.Layer in SL
          parameter ( HBTB_ID1 = 5 ) 
          integer HBTB_ID2               !  Ptr. to DC1 entry  for 2.Layer in SL
          parameter ( HBTB_ID2 = 6 ) 
          integer HBTB_ID3               !  Ptr. to DC1 entry  for 3.Layer in SL
          parameter ( HBTB_ID3 = 7 ) 
          integer HBTB_ID4               !  Ptr. to DC1 entry  for 4.Layer in SL
          parameter ( HBTB_ID4 = 8 ) 
          integer HBTB_ID5               !  Ptr. to DC1 entry  for 5.Layer in SL
          parameter ( HBTB_ID5 = 9 ) 
          integer HBTB_ID6               !  Ptr. to DC1 entry  for 6.Layer in SL
          parameter ( HBTB_ID6 = 10 ) 

C    Offsets for the HBTR bank follow 

          integer HBTR_X               !  x
          parameter ( HBTR_X = 1 ) 
          integer HBTR_Y               !  y 'Vertex' position {x,y,z}
          parameter ( HBTR_Y = 2 ) 
          integer HBTR_Z               !  z
          parameter ( HBTR_Z = 3 ) 
          integer HBTR_PX               !  Px
          parameter ( HBTR_PX = 4 ) 
          integer HBTR_PY               !  Py  momentum at 'vertex' {Px,Py,Pz}
          parameter ( HBTR_PY = 5 ) 
          integer HBTR_PZ               !  Pz
          parameter ( HBTR_PZ = 6 ) 
          integer HBTR_Q               !  charge   (straight tracks: set q=0.)
          parameter ( HBTR_Q = 7 ) 
          integer HBTR_CHI2               !  Chisquare for this Track
          parameter ( HBTR_CHI2 = 8 ) 
          integer HBTR_ITR_SEC               !  Trackno_in_Sector + Sector*100
          parameter ( HBTR_ITR_SEC = 9 ) 

C    Offsets for the HCAL bank follow 

          integer HCAL_VERSION               !  Version number
          parameter ( HCAL_VERSION = 1 ) 
          integer HCAL_RUN_LOW               !  LOW RUN number		 
          parameter ( HCAL_RUN_LOW = 2 ) 
          integer HCAL_RUN_HIGH               !  HIGH RUN number
          parameter ( HCAL_RUN_HIGH = 3 ) 
          integer HCAL_CATIME               !  CAlibration TIME (unix time = 0 1/1/1970)		
          parameter ( HCAL_CATIME = 4 ) 
          integer HCAL_ROCCA               !  32 bit ReadOut Controller CAlibration status
          parameter ( HCAL_ROCCA = 5 ) 

C    Offsets for the HDPL bank follow 

          integer HDPL_TRK_PLN               !  (track_number) *100 + plane_number
          parameter ( HDPL_TRK_PLN = 1 ) 
          integer HDPL_X               !  z coord [cm]  for track in this plane
          parameter ( HDPL_X = 2 ) 
          integer HDPL_Y               !  y coord [cm]  for track in this plane
          parameter ( HDPL_Y = 3 ) 
          integer HDPL_Z               !  z coord [cm]  for track in this plane
          parameter ( HDPL_Z = 4 ) 
          integer HDPL_CX               !  direction cosine (cx) at coord.{x,y,z}
          parameter ( HDPL_CX = 5 ) 
          integer HDPL_CY               !  direction cosine (cy) at coord.{x,y,z}
          parameter ( HDPL_CY = 6 ) 
          integer HDPL_CZ               !  direction cosine (cz) at coord.{x,y,z}
          parameter ( HDPL_CZ = 7 ) 
          integer HDPL_TLEN               !  track length [cm] from origin to this plane
          parameter ( HDPL_TLEN = 8 ) 

C    Offsets for the HEAD bank follow 

          integer HEAD_VERSION               !  Version Number
          parameter ( HEAD_VERSION = 1 ) 
          integer HEAD_NRUN               !  Run Number (monotonically increasing)
          parameter ( HEAD_NRUN = 2 ) 
          integer HEAD_NEVENT               !  Event Number (starting with 1 at run begin,
          parameter ( HEAD_NEVENT = 3 ) 
          integer HEAD_TIME               !  Event Time (UNIX time = seconds as of January 1,1970)
          parameter ( HEAD_TIME = 4 ) 
          integer HEAD_TYPE               !  Event Type (Defined by on-line system or MC run:
          parameter ( HEAD_TYPE = 5 ) 
          integer HEAD_ROC               !  32 bit readout controller status
          parameter ( HEAD_ROC = 6 ) 
          integer HEAD_EVTCLASS               !  Event Classification from DAQ:
          parameter ( HEAD_EVTCLASS = 7 ) 
          integer HEAD_TRIGBITS               !  Trigger Latch Bits
          parameter ( HEAD_TRIGBITS = 8 ) 

C    Offsets for the HEVT bank follow 

          integer HEVT_ESTATUS               !  Event Statuse after reconstruction
          parameter ( HEVT_ESTATUS = 1 ) 
          integer HEVT_NPART               !  Number of final reconstructed particles
          parameter ( HEVT_NPART = 2 ) 
          integer HEVT_GPART               !  Number of geometrically reconstructed particles
          parameter ( HEVT_GPART = 3 ) 
          integer HEVT_WEIGHT               !  Prescale factor for this Event Class
          parameter ( HEVT_WEIGHT = 4 ) 
          integer HEVT_TRIGGER               !  Trigger Type
          parameter ( HEVT_TRIGGER = 5 ) 
          integer HEVT_FC               !  Faraday Cup (nA)
          parameter ( HEVT_FC = 6 ) 
          integer HEVT_PTIME               !  Event Processing Time (UNIX time = seconds)
          parameter ( HEVT_PTIME = 7 ) 

C    Offsets for the MCEV bank follow 

          integer MCEV_I1               !  first geant random number seed for event
          parameter ( MCEV_I1 = 1 ) 
          integer MCEV_I2               !  second seed
          parameter ( MCEV_I2 = 2 ) 

C    Offsets for the MCTK bank follow 

          integer MCTK_CX               !  x dir cosine at track origin
          parameter ( MCTK_CX = 1 ) 
          integer MCTK_CY               !  y dir cosine
          parameter ( MCTK_CY = 2 ) 
          integer MCTK_CZ               !  z dir cosine
          parameter ( MCTK_CZ = 3 ) 
          integer MCTK_PMOM               !  momentum
          parameter ( MCTK_PMOM = 4 ) 
          integer MCTK_MASS               !  mass
          parameter ( MCTK_MASS = 5 ) 
          integer MCTK_CHARGE               !  charge
          parameter ( MCTK_CHARGE = 6 ) 
          integer MCTK_ID               !  track Particle Data Group id
          parameter ( MCTK_ID = 7 ) 
          integer MCTK_FLAG               !  track flag
          parameter ( MCTK_FLAG = 8 ) 
          integer MCTK_BEG_VTX               !  beginning vertex number 
          parameter ( MCTK_BEG_VTX = 9 ) 
          integer MCTK_END_VTX               !  ending vertex number
          parameter ( MCTK_END_VTX = 10 ) 
          integer MCTK_PARENT               !  parent track
          parameter ( MCTK_PARENT = 11 ) 

C    Offsets for the MCVX bank follow 

          integer MCVX_X               !  x of vertex
          parameter ( MCVX_X = 1 ) 
          integer MCVX_Y               !  y
          parameter ( MCVX_Y = 2 ) 
          integer MCVX_Z               !  z
          parameter ( MCVX_Z = 3 ) 
          integer MCVX_TOF               !  secs of flight
          parameter ( MCVX_TOF = 4 ) 
          integer MCVX_FLAG               !  vertex flag
          parameter ( MCVX_FLAG = 5 ) 

C    Offsets for the PART bank follow 

          integer PART_PID               !  particle id (GEANT)
          parameter ( PART_PID = 1 ) 
          integer PART_X               !  x, Vertex position {x,y,z} 
          parameter ( PART_X = 2 ) 
          integer PART_Y               !  y
          parameter ( PART_Y = 3 ) 
          integer PART_Z               !  z
          parameter ( PART_Z = 4 ) 
          integer PART_E               !  Energy
          parameter ( PART_E = 5 ) 
          integer PART_PX               !  momentum {x,y,z}
          parameter ( PART_PX = 6 ) 
          integer PART_PY               !   py
          parameter ( PART_PY = 7 ) 
          integer PART_PZ               !  pz
          parameter ( PART_PZ = 8 ) 
          integer PART_Q               !  charge
          parameter ( PART_Q = 9 ) 
          integer PART_TRKID               !  track pointer
          parameter ( PART_TRKID = 10 ) 
          integer PART_QPID               !  quality factor for the pid
          parameter ( PART_QPID = 11 ) 
          integer PART_QTRK               !  quality factor for the trk
          parameter ( PART_QTRK = 12 ) 
          integer PART_FLAGS               !  set of flags defining track (ie, BEAM)
          parameter ( PART_FLAGS = 13 ) 

C    Offsets for the SC bank follow 

          integer SC_ID               !  the address of the hit detector element
          parameter ( SC_ID = 1 ) 
          integer SC_TDCL               !  tdc information (channels)
          parameter ( SC_TDCL = 2 ) 
          integer SC_ADCL               !  adc information (channels)
          parameter ( SC_ADCL = 3 ) 
          integer SC_TDCR               !  tdc information (channels)
          parameter ( SC_TDCR = 4 ) 
          integer SC_ADCR               !  adc information (channels)
          parameter ( SC_ADCR = 5 ) 

C    Offsets for the SC1 bank follow 

          integer SC1_ID               !  the address of the hit detector element
          parameter ( SC1_ID = 1 ) 
          integer SC1_TIME_L               !  time for left paddle(ns) 
          parameter ( SC1_TIME_L = 2 ) 
          integer SC1_ENERGY_L               !  energy in left paddle(MeV) 
          parameter ( SC1_ENERGY_L = 3 ) 
          integer SC1_TIME_R               !  time for right paddle(ns) 
          parameter ( SC1_TIME_R = 4 ) 
          integer SC1_ENERGY_R               !  energy in right paddle(MeV) 
          parameter ( SC1_ENERGY_R = 5 ) 

C    Offsets for the SCC bank follow 

          integer SCC_ID               !  paddle id# 
          parameter ( SCC_ID = 1 ) 
          integer SCC_DATE               !  UNIX univiersal time of calibraton (32 bits) 
          parameter ( SCC_DATE = 2 ) 
          integer SCC_VERSION               !  sequential version# of calibration 
          parameter ( SCC_VERSION = 3 ) 
          integer SCC_STATUS               !  4 byte status word (see details) 
          parameter ( SCC_STATUS = 4 ) 
          integer SCC_TD0L               !  (Left) gives 0 time at center for tube (ns) 
          parameter ( SCC_TD0L = 5 ) 
          integer SCC_TD0LU               !  (Left) uncertainty (ns) 
          parameter ( SCC_TD0LU = 6 ) 
          integer SCC_TD0R               !  (Right) gives 0 time at center for tube (ns) 
          parameter ( SCC_TD0R = 7 ) 
          integer SCC_TD0RU               !  (Right) uncertainty (ns) 
          parameter ( SCC_TD0RU = 8 ) 
          integer SCC_TD1L               !  (Left) TDC**1 coefficient (ns/ch) 
          parameter ( SCC_TD1L = 9 ) 
          integer SCC_TD1LU               !  (Left) uncertainty (ns/ch) 
          parameter ( SCC_TD1LU = 10 ) 
          integer SCC_TD1R               !  (Right) TDC**1 coefficient (ns/ch) 
          parameter ( SCC_TD1R = 11 ) 
          integer SCC_TD1RU               !  (Right) uncertainty (ns/ch) 
          parameter ( SCC_TD1RU = 12 ) 
          integer SCC_TD2L               !  (Left) TDC**2 coefficient 
          parameter ( SCC_TD2L = 13 ) 
          integer SCC_TD2LU               !  (Left) uncertainty (ns/ch) 
          parameter ( SCC_TD2LU = 14 ) 
          integer SCC_TD2R               !  (Right)TDC**2 coefficient 
          parameter ( SCC_TD2R = 15 ) 
          integer SCC_TD2RU               !  (Right) uncertainty (ns/ch) 
          parameter ( SCC_TD2RU = 16 ) 
          integer SCC_TW0L               !  (Left) time walk constant (ns) 
          parameter ( SCC_TW0L = 17 ) 
          integer SCC_TW0LU               !  (Left) uncertainty (ns) 
          parameter ( SCC_TW0LU = 18 ) 
          integer SCC_TW0R               !  (Right) time walk constant (ns) 
          parameter ( SCC_TW0R = 19 ) 
          integer SCC_TW0RU               !  (Right) uncertainty (ns) 
          parameter ( SCC_TW0RU = 20 ) 
          integer SCC_TW1L               !  (Left) time walk 1st factor 
          parameter ( SCC_TW1L = 21 ) 
          integer SCC_TW1LU               !  (Left) uncertainty 
          parameter ( SCC_TW1LU = 22 ) 
          integer SCC_TW1R               !  (Right) time walk 1st factor 
          parameter ( SCC_TW1R = 23 ) 
          integer SCC_TW1RU               !  (Right) uncertainty 
          parameter ( SCC_TW1RU = 24 ) 
          integer SCC_TW2L               !  (Left) time walk 2nd factor 
          parameter ( SCC_TW2L = 25 ) 
          integer SCC_TW2LU               !  (Left) uncertainty 
          parameter ( SCC_TW2LU = 26 ) 
          integer SCC_TW2R               !  (Right) time walk 2nd factor 
          parameter ( SCC_TW2R = 27 ) 
          integer SCC_TW2RU               !  (Right) uncertainty 
          parameter ( SCC_TW2RU = 28 ) 
          integer SCC_ADPL               !  (Left) ADC pedestal (ch) 
          parameter ( SCC_ADPL = 29 ) 
          integer SCC_ADPLU               !  (Left) uncertainty 
          parameter ( SCC_ADPLU = 30 ) 
          integer SCC_ADPR               !  (Right) ADC pedestal (ch) 
          parameter ( SCC_ADPR = 31 ) 
          integer SCC_ADPRU               !  (Right) uncertainty 
          parameter ( SCC_ADPRU = 32 ) 
          integer SCC_M0L               !  (Left) nmip adc channel 
          parameter ( SCC_M0L = 33 ) 
          integer SCC_M0LU               !  (Left) uncertainty (ch) 
          parameter ( SCC_M0LU = 34 ) 
          integer SCC_M0R               !  (Right)nmip adc channel 
          parameter ( SCC_M0R = 35 ) 
          integer SCC_M0RU               !  (Right) uncertainty(ch) 
          parameter ( SCC_M0RU = 36 ) 
          integer SCC_VEFL               !  (Left) effective velocity of light (cm/ns) 
          parameter ( SCC_VEFL = 37 ) 
          integer SCC_VEFLU               !  (Left) uncertainty (cm/ns) 
          parameter ( SCC_VEFLU = 38 ) 
          integer SCC_VEFR               !  (Right) effective velocity of light (cm/ns) 
          parameter ( SCC_VEFR = 39 ) 
          integer SCC_VEFRU               !  (Right) uncertainty (cm/ns) 
          parameter ( SCC_VEFRU = 40 ) 
          integer SCC_ATNL               !  (Left) attenuation length (cm) 
          parameter ( SCC_ATNL = 41 ) 
          integer SCC_ATNLU               !  (Left) uncertainty (cm) 
          parameter ( SCC_ATNLU = 42 ) 
          integer SCC_ATNR               !  (Right) attenuation length (cm) 
          parameter ( SCC_ATNR = 43 ) 
          integer SCC_ATNRU               !  (Right) uncertainty (cm) 
          parameter ( SCC_ATNRU = 44 ) 
          integer SCC_TSIG0L               !  (Left) 1st parameter of measured resolution (ns) 
          parameter ( SCC_TSIG0L = 45 ) 
          integer SCC_TSIG0R               !  (Right) 1st parameter of measured resolution (ns) 
          parameter ( SCC_TSIG0R = 46 ) 
          integer SCC_TSIG1L               !  (Left) 2nd parameter of measured resolution 
          parameter ( SCC_TSIG1L = 47 ) 
          integer SCC_TSIG1R               !  (Right) 2nd parameter of measured resolution 
          parameter ( SCC_TSIG1R = 48 ) 

C    Offsets for the SCG bank follow 

          integer SCG_ID               !   paddle id#
          parameter ( SCG_ID = 1 ) 
          integer SCG_PANEL               !  panel number - to which plane (panel) the paddle belongs
          parameter ( SCG_PANEL = 2 ) 
          integer SCG_XCCW               !  X center of CCW end of paddle in CLAS system
          parameter ( SCG_XCCW = 3 ) 
          integer SCG_YCCW               !  Y center of CCW end 
          parameter ( SCG_YCCW = 4 ) 
          integer SCG_ZCCW               !  - Z center of CCW end
          parameter ( SCG_ZCCW = 5 ) 
          integer SCG_XCW               !  X center of CW end 
          parameter ( SCG_XCW = 6 ) 
          integer SCG_YCW               !  Y center of CW end
          parameter ( SCG_YCW = 7 ) 
          integer SCG_ZCW               ! - Z center of CW end 
          parameter ( SCG_ZCW = 8 ) 
          integer SCG_WIDTH               !  width (cm) (~|| CLAS theta) 
          parameter ( SCG_WIDTH = 9 ) 
          integer SCG_THICK               !  thickness (cm) (~|| CLAS r) 
          parameter ( SCG_THICK = 10 ) 
          integer SCG_DELTA               !  the difference in length of the higher-theta slab minus the lower-theta slab divided by 4 (cm) 
          parameter ( SCG_DELTA = 11 ) 

C    Offsets for the SCH bank follow 

          integer SCH_X               !  x of hit
          parameter ( SCH_X = 1 ) 
          integer SCH_Y               !  y of hit
          parameter ( SCH_Y = 2 ) 
          integer SCH_Z               !  z of hit
          parameter ( SCH_Z = 3 ) 
          integer SCH_CX               !  track x dir cosine
          parameter ( SCH_CX = 4 ) 
          integer SCH_CY               !  track y dir cosine
          parameter ( SCH_CY = 5 ) 
          integer SCH_CZ               !  track z dir cosine
          parameter ( SCH_CZ = 6 ) 
          integer SCH_PMOM               !  track momentum
          parameter ( SCH_PMOM = 7 ) 
          integer SCH_TRACK               !  track number
          parameter ( SCH_TRACK = 8 ) 
          integer SCH_ID               !  track PDG id
          parameter ( SCH_ID = 9 ) 

C    Offsets for the SCI bank follow 

          integer SCI_ID               !  paddle id# in least significant 2 bytes, sector# in most significant 2 bytes 
          parameter ( SCI_ID = 1 ) 
          integer SCI_STATUS               !  general status information (see details) 
          parameter ( SCI_STATUS = 2 ) 
          integer SCI_TL               !  (Left) calibration  - corrected time, such that all TOF counters are timed relative to each other (nS) 
          parameter ( SCI_TL = 3 ) 
          integer SCI_AL               !  (Left) calibration corrected and normalized to normal incidence minimum ionizing energy deposit 
          parameter ( SCI_AL = 4 ) 
          integer SCI_TR               !  (Right) calibration  -corrected time, such that all TOF counters are timed relative to each other (nS) 
          parameter ( SCI_TR = 5 ) 
          integer SCI_AR               !  (Right) calibration corrected and normalized to normal incidence minimum ionizing energy deposit 
          parameter ( SCI_AR = 6 ) 
          integer SCI_POST               !  position from center along paddle as determined from the time (cm); schematically POST=(TL  F  -99999.9 999999.9    
          parameter ( SCI_POST = 7 ) 
          integer SCI_POSA               !  position from center along paddle as determined from the energy (cm); schematically POSA=0.5*ATNavg*ln(AL/AR) 
          parameter ( SCI_POSA = 8 ) 

C    Offsets for the ScIf bank follow 

          integer SCIF_ID               !  paddle id# in least significant 2 bytes, sector# in most  significant 2 bytes
          parameter ( SCIF_ID = 1 ) 
          integer SCIF_STATUS               !  general status information (see details)
          parameter ( SCIF_STATUS = 2 ) 
          integer SCIF_XO               !  CLAS X of center of paddle
          parameter ( SCIF_XO = 3 ) 
          integer SCIF_YO               !  CLAS Y of center of paddle
          parameter ( SCIF_YO = 4 ) 
          integer SCIF_ZO               !  CLAS Z of center of paddle
          parameter ( SCIF_ZO = 5 ) 
          integer SCIF_IX               !  CLAS X component of I unit vector
          parameter ( SCIF_IX = 6 ) 
          integer SCIF_IY               !  CLAS Y component of I unit vector
          parameter ( SCIF_IY = 7 ) 
          integer SCIF_IZ               !  CLAS Z component of I unit vector
          parameter ( SCIF_IZ = 8 ) 
          integer SCIF_JX               !  CLAS X component of J unit vector (major axis)
          parameter ( SCIF_JX = 9 ) 
          integer SCIF_JY               !  CLAS Y component of J unit vector (major axis)
          parameter ( SCIF_JY = 10 ) 
          integer SCIF_JZ               !  CLAS Z component of J unit vector (major axis)
          parameter ( SCIF_JZ = 11 ) 
          integer SCIF_KX               !  CLAS X component of K unit vector (minor axis)
          parameter ( SCIF_KX = 12 ) 
          integer SCIF_KY               !  CLAS Y component of K unit vector (minor axis)
          parameter ( SCIF_KY = 13 ) 
          integer SCIF_KZ               !  CLAS Z component of K unit vector (minor axis)
          parameter ( SCIF_KZ = 14 ) 
          integer SCIF_DELW               !  distance from center to surface of paddle || I (width/2) (cm)
          parameter ( SCIF_DELW = 15 ) 
          integer SCIF_DELL               !  distance from center to surface of paddle || J (length/2) (cm)
          parameter ( SCIF_DELL = 16 ) 
          integer SCIF_DELT               !  distance from center to surface of paddle || K (thickness/2) (cm)
          parameter ( SCIF_DELT = 17 ) 
          integer SCIF_NMIP               !  energy deposit for a normal minimum ionizing particle (GeV)
          parameter ( SCIF_NMIP = 18 ) 
          integer SCIF_VELAVG               !  average effective speed of light in paddle (cm/nS)
          parameter ( SCIF_VELAVG = 19 ) 
          integer SCIF_ATNAVG               !  average attenuation length in paddle (cm)
          parameter ( SCIF_ATNAVG = 20 ) 
          integer SCIF_VELL               !  (Left) effective speed of light in paddle (cm/nS)
          parameter ( SCIF_VELL = 21 ) 
          integer SCIF_VELR               !  (Right) effective speed of light in paddle (cm/nS)
          parameter ( SCIF_VELR = 22 ) 
          integer SCIF_ATNL               !  (Left) attenuation length in paddle (cm)
          parameter ( SCIF_ATNL = 23 ) 
          integer SCIF_ATNR               !  (Right) attenuation length in paddle (cm)
          parameter ( SCIF_ATNR = 24 ) 

C    Offsets for the SCMD bank follow 

          integer SCMD_ID               !  paddle # 
          parameter ( SCMD_ID = 1 ) 
          integer SCMD_STATUS               !  general status information 
          parameter ( SCMD_STATUS = 2 ) 
          integer SCMD_X_NORM               !  X of unit vector normal to paddle
          parameter ( SCMD_X_NORM = 3 ) 
          integer SCMD_Y_NORM               !  Y of unit vector normal to paddle
          parameter ( SCMD_Y_NORM = 4 ) 
          integer SCMD_D_NORM               !  normal distance to paddle plane
          parameter ( SCMD_D_NORM = 5 ) 
          integer SCMD_X_PANEL               !  X of unit vector along the panel
          parameter ( SCMD_X_PANEL = 6 ) 
          integer SCMD_Y_PANEL               !  Y of unit vector along the panel
          parameter ( SCMD_Y_PANEL = 7 ) 
          integer SCMD_B_PAD               !  Begin of paddle along the panel
          parameter ( SCMD_B_PAD = 8 ) 
          integer SCMD_E_PAD               !  End of paddle along the panel
          parameter ( SCMD_E_PAD = 9 ) 
          integer SCMD_TOF               !  time of flight (nS)
          parameter ( SCMD_TOF = 10 ) 
          integer SCMD_EDP               !  energy deposited (GeV) 
          parameter ( SCMD_EDP = 11 ) 
          integer SCMD_P_TIME               !  position i.r.t. mid.plane from time
          parameter ( SCMD_P_TIME = 12 ) 
          integer SCMD_P_ATLN               !  position i.r.t. mid.plane from atten.
          parameter ( SCMD_P_ATLN = 13 ) 

C    Offsets for the SCP bank follow 

          integer SCP_N1X               !  x component of outward normal to panel 1
          parameter ( SCP_N1X = 1 ) 
          integer SCP_N1Y               !  y component of outward normal to panel 1
          parameter ( SCP_N1Y = 2 ) 
          integer SCP_N1Z               !  z component of outward normal to panel 1
          parameter ( SCP_N1Z = 3 ) 
          integer SCP_R1N               !  min.dist.inner(cm)from origin to panel 1
          parameter ( SCP_R1N = 4 ) 
          integer SCP_N2X               !  x component of outward normal to panel 2
          parameter ( SCP_N2X = 5 ) 
          integer SCP_N2Y               !  y component of outward normal to panel 2
          parameter ( SCP_N2Y = 6 ) 
          integer SCP_N2Z               !  z component of outward normal to panel 2
          parameter ( SCP_N2Z = 7 ) 
          integer SCP_R2N               !  min.dist.inner(cm)from origin to panel 2
          parameter ( SCP_R2N = 8 ) 
          integer SCP_N3X               !  x component of outward normal to panel 3
          parameter ( SCP_N3X = 9 ) 
          integer SCP_N3Y               !  y component of outward normal to panel 3
          parameter ( SCP_N3Y = 10 ) 
          integer SCP_N3Z               !  z component of outward normal to panel 3
          parameter ( SCP_N3Z = 11 ) 
          integer SCP_R3N               !  min.dist.inner(cm)from origin to panel 3
          parameter ( SCP_R3N = 12 ) 
          integer SCP_N4X               !  x component of outward normal to panel 4
          parameter ( SCP_N4X = 13 ) 
          integer SCP_N4Y               !  y component of outward normal to panel 4
          parameter ( SCP_N4Y = 14 ) 
          integer SCP_N4Z               !  z component of outward normal to panel 4
          parameter ( SCP_N4Z = 15 ) 
          integer SCP_R4N               !  min.dist.inner(cm)from origin to panel 4
          parameter ( SCP_R4N = 16 ) 

C    Offsets for the SCPB bank follow 

          integer SCPB_IHIT               !  Bank row number 
          parameter ( SCPB_IHIT = 1 ) 
          integer SCPB_ID               !  Particle Data Group id
          parameter ( SCPB_ID = 2 ) 
          integer SCPB_EDEP               !  Deposited energy (dE/dX)
          parameter ( SCPB_EDEP = 3 ) 
          integer SCPB_TIME               !  Flight time relative to the evnt start time
          parameter ( SCPB_TIME = 4 ) 
          integer SCPB_PATH               !  Path lenght from target
          parameter ( SCPB_PATH = 5 ) 
          integer SCPB_SECTOR               !  SC hit sector 
          parameter ( SCPB_SECTOR = 6 ) 
          integer SCPB_PARTID               !  Particle wor number for SC hit 
          parameter ( SCPB_PARTID = 7 ) 
          integer SCPB_ISCHIT               !  SC hit ID 
          parameter ( SCPB_ISCHIT = 8 ) 
          integer SCPB_STATUS               !  Status word
          parameter ( SCPB_STATUS = 9 ) 

C    Offsets for the SCR bank follow 

          integer SCR_ID               !  paddle id  
          parameter ( SCR_ID = 1 ) 
          integer SCR_ENERGY               !  Energy (MeV) 
          parameter ( SCR_ENERGY = 2 ) 
          integer SCR_TIME               !  time(ns) 
          parameter ( SCR_TIME = 3 ) 
          integer SCR_X               !  x position in sector coodinate system 
          parameter ( SCR_X = 4 ) 
          integer SCR_Y               !  y position in sector coodinate system 
          parameter ( SCR_Y = 5 ) 
          integer SCR_Z               !  z position in sector coodinate system 
          parameter ( SCR_Z = 6 ) 
          integer SCR_DX               !  x error in sector coodinate system 
          parameter ( SCR_DX = 7 ) 
          integer SCR_DY               !  y error in sector coodinate system 
          parameter ( SCR_DY = 8 ) 
          integer SCR_DZ               !  z error in sector coodinate system 
          parameter ( SCR_DZ = 9 ) 
          integer SCR_STATUS               !  status word defined in sc.h
          parameter ( SCR_STATUS = 10 ) 

C    Offsets for the SCRB bank follow 

          integer SCRB_ID               !  paddle id# in least significant 2 bytes, sector# in most significant 2 bytes 
          parameter ( SCRB_ID = 1 ) 
          integer SCRB_STATUS               !  general status information (see details) 
          parameter ( SCRB_STATUS = 2 ) 
          integer SCRB_HIT               !  HIT gives status information on the hit; PSST is the processing status (see details) 
          parameter ( SCRB_HIT = 3 ) 
          integer SCRB_TOF               !  time of flight (nS) (if no tracking is available, it is the time relative to the trigger rather than the flight time) 
          parameter ( SCRB_TOF = 4 ) 
          integer SCRB_DTOF               !  uncertainty in TOF (nS) 
          parameter ( SCRB_DTOF = 5 ) 
          integer SCRB_EDP               !  energy deposited (GeV) 
          parameter ( SCRB_EDP = 6 ) 
          integer SCRB_DEDP               !  uncertainty in EDP (GeV) 
          parameter ( SCRB_DEDP = 7 ) 
          integer SCRB_POST               !  position derived from the time measurements (cm) 
          parameter ( SCRB_POST = 8 ) 
          integer SCRB_DPOST               !  uncertainty in POST(cm) 
          parameter ( SCRB_DPOST = 9 ) 
          integer SCRB_POSA               !  position derived from the amplitude measurements (cm) 
          parameter ( SCRB_POSA = 10 ) 
          integer SCRB_DPOSA               !  uncertainty in POSA (cm) 
          parameter ( SCRB_DPOSA = 11 ) 

C    Offsets for the SCRC bank follow 

          integer SCRC_ID               !  cluster id   
          parameter ( SCRC_ID = 1 ) 
          integer SCRC_ENERGY               !  cluster Energy (MeV) 
          parameter ( SCRC_ENERGY = 2 ) 
          integer SCRC_DENERGY               !  error in cluster energy (ns)
          parameter ( SCRC_DENERGY = 3 ) 
          integer SCRC_TIME               !  cluster (energy-weighted) time(ns) 
          parameter ( SCRC_TIME = 4 ) 
          integer SCRC_DTIME               !  error in cluster time (ns)
          parameter ( SCRC_DTIME = 5 ) 
          integer SCRC_X               !  x position in sector coordinate system 
          parameter ( SCRC_X = 6 ) 
          integer SCRC_Y               !  y position in sector coordinate system 
          parameter ( SCRC_Y = 7 ) 
          integer SCRC_Z               !  z position in sector coordinate system 
          parameter ( SCRC_Z = 8 ) 
          integer SCRC_DX               !  x error in sector coordinate system 
          parameter ( SCRC_DX = 9 ) 
          integer SCRC_DY               !  y error in sector coordinate system 
          parameter ( SCRC_DY = 10 ) 
          integer SCRC_DZ               !  z error in sector coordinate system 
          parameter ( SCRC_DZ = 11 ) 
          integer SCRC_STATUS               !  status word defined in sc.h
          parameter ( SCRC_STATUS = 12 ) 

C    Offsets for the SCRW bank follow 

          integer SCRW_ID               !  paddle id# in least significant 2 bytes, sector# in most significant 2 bytes 
          parameter ( SCRW_ID = 1 ) 
          integer SCRW_STATUS               !  general status information (see details) 
          parameter ( SCRW_STATUS = 2 ) 
          integer SCRW_HIT               !  HIT gives status information on the hit; PSST is the processing status (see details) 
          parameter ( SCRW_HIT = 3 ) 
          integer SCRW_THIT               !  time w.r.t. trigger start (nS) 
          parameter ( SCRW_THIT = 4 ) 
          integer SCRW_DTHIT               !  uncertainty in Thit (nS) 
          parameter ( SCRW_DTHIT = 5 ) 
          integer SCRW_EHIT               !  energy deposit (GeV) (corrected for attenuation) 
          parameter ( SCRW_EHIT = 6 ) 
          integer SCRW_DEHIT               !  uncertainty in Ehit (GeV) 
          parameter ( SCRW_DEHIT = 7 ) 
          integer SCRW_QHIT               !  estimated quality of hit (1.00=ideal) 
          parameter ( SCRW_QHIT = 8 ) 
          integer SCRW_XHIT               !  CLAS X position derived from the time measurements (cm) 
          parameter ( SCRW_XHIT = 9 ) 
          integer SCRW_YHIT               !  CLAS Y 
          parameter ( SCRW_YHIT = 10 ) 
          integer SCRW_ZHIT               !  CLAS Z 
          parameter ( SCRW_ZHIT = 11 ) 
          integer SCRW_DIX               !  CLAS X component of dI (paddle width/2) (cm) 
          parameter ( SCRW_DIX = 12 ) 
          integer SCRW_DIY               !  CLAS Y 
          parameter ( SCRW_DIY = 13 ) 
          integer SCRW_DIZ               !  CLAS Z 
          parameter ( SCRW_DIZ = 14 ) 
          integer SCRW_DJX               !  CLAS X component of dJ (hit width/2) (cm) 
          parameter ( SCRW_DJX = 15 ) 
          integer SCRW_DJY               !  CLAS Y 
          parameter ( SCRW_DJY = 16 ) 
          integer SCRW_DJZ               !  CLAS Z 
          parameter ( SCRW_DJZ = 17 ) 
          integer SCRW_DKX               !  CLAS X component of dK (paddle thickness/2) (cm) 
          parameter ( SCRW_DKX = 18 ) 
          integer SCRW_DKY               !  CLAS Y 
          parameter ( SCRW_DKY = 19 ) 
          integer SCRW_DKZ               !  CLAS Z 
          parameter ( SCRW_DKZ = 20 ) 

C    Offsets for the SCRX bank follow 

          integer SCRX_SECTOR               !  sector of CLAS 
          parameter ( SCRX_SECTOR = 1 ) 
          integer SCRX_IDA               !  average paddle ID of cluster hit 
          parameter ( SCRX_IDA = 2 ) 
          integer SCRX_STATUS               !  bit pattern to be specified later   
          parameter ( SCRX_STATUS = 3 ) 
          integer SCRX_TCLUSTER               !  time w.r.t. trigger start (nS)    
          parameter ( SCRX_TCLUSTER = 4 ) 
          integer SCRX_DTCLUSTER               !  uncertainty in Tcluster (nS)  
          parameter ( SCRX_DTCLUSTER = 5 ) 
          integer SCRX_ECLUSTER               !  energy deposit (GeV) (attn. corrected) 
          parameter ( SCRX_ECLUSTER = 6 ) 
          integer SCRX_DECLUSTER               !  uncertainty in Ecluster (GeV)
          parameter ( SCRX_DECLUSTER = 7 ) 
          integer SCRX_QCLUSTER               !  estimated quality of cluster (ideal=1.0)
          parameter ( SCRX_QCLUSTER = 8 ) 
          integer SCRX_XSECTOR               !  x in sector based system (cm); midplane 
          parameter ( SCRX_XSECTOR = 9 ) 
          integer SCRX_YSECTOR               !  y in sector based system (cm); along SC
          parameter ( SCRX_YSECTOR = 10 ) 
          integer SCRX_ZSECTOR               !  z in sector based system (cm); ZCLAS 
          parameter ( SCRX_ZSECTOR = 11 ) 
          integer SCRX_DXSECTOR               !  uncertainty in Xsector (cm)
          parameter ( SCRX_DXSECTOR = 12 ) 
          integer SCRX_DYSECTOR               !  uncertainty in Ysector (cm)
          parameter ( SCRX_DYSECTOR = 13 ) 
          integer SCRX_DZSECTOR               !  uncertainty in Zsector (cm)
          parameter ( SCRX_DZSECTOR = 14 ) 
          integer SCRX_YSECTORI               !  Ysector of first paddle of cluster (cm) 
          parameter ( SCRX_YSECTORI = 15 ) 
          integer SCRX_YSECTORJ               !  Ysector of second paddle,if none then 0
          parameter ( SCRX_YSECTORJ = 16 ) 
          integer SCRX_DYSECTORI               !  uncertainty in YsectorI (cm)
          parameter ( SCRX_DYSECTORI = 17 ) 
          integer SCRX_DYSECTORJ               !  uncertainty in YsectorJ,if none then 0  
          parameter ( SCRX_DYSECTORJ = 18 ) 

C    Offsets for the ST0 bank follow 

          integer ST0_ID               !  the address of the hit detector element
          parameter ( ST0_ID = 1 ) 
          integer ST0_TDC               !  tdc information (channels)
          parameter ( ST0_TDC = 2 ) 
          integer ST0_ADC               !  adc information (channels)
          parameter ( ST0_ADC = 3 ) 

C    Offsets for the ST1 bank follow 

          integer ST1_ID               !  the address of the hit detector element
          parameter ( ST1_ID = 1 ) 
          integer ST1_TIME               !  Time (ns)
          parameter ( ST1_TIME = 2 ) 
          integer ST1_ADCPS               !  Pedestal-subtracted ADC value (ch)
          parameter ( ST1_ADCPS = 3 ) 

C    Offsets for the TAGE bank follow 

          integer TAGE_ID               !  the address of the hit detector element
          parameter ( TAGE_ID = 1 ) 
          integer TAGE_TDC               !  tdc information (channels)
          parameter ( TAGE_TDC = 2 ) 

C    Offsets for the TAGR bank follow 

          integer TAGR_ERG               !  Energy of the photon  in GeV
          parameter ( TAGR_ERG = 1 ) 
          integer TAGR_DERG               !  dE of the photon in GeV
          parameter ( TAGR_DERG = 2 ) 
          integer TAGR_TPHO               !  Time of the photon (as of 9/29/97 TDC value * .05nsec/channel)
          parameter ( TAGR_TPHO = 3 ) 
          integer TAGR_STAT               !  Error status of the thing 0 is good hit 1-10 to be defined
          parameter ( TAGR_STAT = 4 ) 

C    Offsets for the TAGT bank follow 

          integer TAGT_ID               !  the address of the hit detector element
          parameter ( TAGT_ID = 1 ) 
          integer TAGT_TDCL               !  tdc information (Left counters channels)
          parameter ( TAGT_TDCL = 2 ) 
          integer TAGT_TDCR               !  tdc information (Right counters channels)
          parameter ( TAGT_TDCR = 3 ) 

C    Offsets for the TBER bank follow 

          integer TBER_INV_P               !  1/p
          parameter ( TBER_INV_P = 1 ) 
          integer TBER_LAMBDA               !  dip angle 
          parameter ( TBER_LAMBDA = 2 ) 
          integer TBER_PHI               !  phi
          parameter ( TBER_PHI = 3 ) 
          integer TBER_D0               !  min.distance from origin in {x,y} [cm]
          parameter ( TBER_D0 = 4 ) 
          integer TBER_Z0               !  min.distance from beamline {x}    [cm]
          parameter ( TBER_Z0 = 5 ) 
          integer TBER_C11               !  element C{1,1}
          parameter ( TBER_C11 = 6 ) 
          integer TBER_C12               !  element C{1,2}
          parameter ( TBER_C12 = 7 ) 
          integer TBER_C13               !  element C{1,3}
          parameter ( TBER_C13 = 8 ) 
          integer TBER_C14               !  element C{1,4}
          parameter ( TBER_C14 = 9 ) 
          integer TBER_C15               !  element C{1,5}
          parameter ( TBER_C15 = 10 ) 
          integer TBER_C22               !  element C{2,2}
          parameter ( TBER_C22 = 11 ) 
          integer TBER_C23               !  element C{2,3}
          parameter ( TBER_C23 = 12 ) 
          integer TBER_C24               !  element C{2,4}
          parameter ( TBER_C24 = 13 ) 
          integer TBER_C25               !  element C{2,5}
          parameter ( TBER_C25 = 14 ) 
          integer TBER_C33               !  element C{3,3}
          parameter ( TBER_C33 = 15 ) 
          integer TBER_C34               !  element C{3,4}
          parameter ( TBER_C34 = 16 ) 
          integer TBER_C35               !  element C{3,5}
          parameter ( TBER_C35 = 17 ) 
          integer TBER_C44               !  element C{4,4}
          parameter ( TBER_C44 = 18 ) 
          integer TBER_C45               !  element C{4,5}
          parameter ( TBER_C45 = 19 ) 
          integer TBER_C55               !  element C{5,5}
          parameter ( TBER_C55 = 20 ) 

C    Offsets for the TBLA bank follow 

          integer TBLA_TRK_PLN               !  (track_number) *100 + Trk_plane_number
          parameter ( TBLA_TRK_PLN = 1 ) 
          integer TBLA_X               !  z coord [cm]  for track in this plane
          parameter ( TBLA_X = 2 ) 
          integer TBLA_Y               !  y coord [cm]  for track in this plane
          parameter ( TBLA_Y = 3 ) 
          integer TBLA_Z               !  z coord [cm]  for track in this plane
          parameter ( TBLA_Z = 4 ) 
          integer TBLA_CX               !  direction cosine (cx) at coord.{x,y,z}
          parameter ( TBLA_CX = 5 ) 
          integer TBLA_CY               !  direction cosine (cy) at coord.{x,y,z}
          parameter ( TBLA_CY = 6 ) 
          integer TBLA_CZ               !  direction cosine (cz) at coord.{x,y,z}
          parameter ( TBLA_CZ = 7 ) 
          integer TBLA_TLEN               !  track length [cm] from origin to this plane
          parameter ( TBLA_TLEN = 8 ) 
          integer TBLA_DC1               !  Pointer to DC1 bank
          parameter ( TBLA_DC1 = 9 ) 
          integer TBLA_STAT               !  Status of the hit
          parameter ( TBLA_STAT = 10 ) 
          integer TBLA_WIRE               !  Wire number 
          parameter ( TBLA_WIRE = 11 ) 
          integer TBLA_DTIME               !  drift time  [ns]
          parameter ( TBLA_DTIME = 12 ) 
          integer TBLA_ALPHA               !  track angle (relative to R of SL) [deg]
          parameter ( TBLA_ALPHA = 13 ) 
          integer TBLA_WLEN               !  Wire length (hit pos. to preamp)  [cm]
          parameter ( TBLA_WLEN = 14 ) 
          integer TBLA_SGDOCA               !  sigma DOCA  [cm]
          parameter ( TBLA_SGDOCA = 15 ) 
          integer TBLA_FITDOCA               !  Fitted DOCA [cm]
          parameter ( TBLA_FITDOCA = 16 ) 
          integer TBLA_CALCDOCA               !  calculated DOCA (via dtime)  [cm]
          parameter ( TBLA_CALCDOCA = 17 ) 

C    Offsets for the TBTR bank follow 

          integer TBTR_X               !  x
          parameter ( TBTR_X = 1 ) 
          integer TBTR_Y               !  y 'Vertex' position {x,y,z}
          parameter ( TBTR_Y = 2 ) 
          integer TBTR_Z               !  z
          parameter ( TBTR_Z = 3 ) 
          integer TBTR_PX               !  Px
          parameter ( TBTR_PX = 4 ) 
          integer TBTR_PY               !  Py  momentum at 'vertex' {Px,Py,Pz}
          parameter ( TBTR_PY = 5 ) 
          integer TBTR_PZ               !  Pz
          parameter ( TBTR_PZ = 6 ) 
          integer TBTR_Q               !  charge   (straight tracks: set q=0.) 
          parameter ( TBTR_Q = 7 ) 
          integer TBTR_CHI2               !  Chisquare for this Track
          parameter ( TBTR_CHI2 = 8 ) 
          integer TBTR_ITR_SEC               !  Trackno_in_Sector + Sector*100
          parameter ( TBTR_ITR_SEC = 9 ) 
          integer TBTR_ITR_HBT               !  Trackno in HBTR for this track
          parameter ( TBTR_ITR_HBT = 10 ) 

C    Offsets for the TCSB bank follow 

          integer TCSB_XPOS               !  x misalignment of TCS in HCS 		 
          parameter ( TCSB_XPOS = 1 ) 
          integer TCSB_YPOS               !  y misalignment of TCS in HCS	 
          parameter ( TCSB_YPOS = 2 ) 
          integer TCSB_ZPOS               !  z misalignment of TCS in HCS	 
          parameter ( TCSB_ZPOS = 3 ) 
          integer TCSB_SXPOS               !  sx sine of x-axis misorientation of TCS vs HCS		 
          parameter ( TCSB_SXPOS = 4 ) 
          integer TCSB_SYPOS               !  sy sine of y-axis misorientation of TCS vs HCS
          parameter ( TCSB_SYPOS = 5 ) 
          integer TCSB_SZPOS               !  sz sine of z-axis misorientation of TCS vs HCS
          parameter ( TCSB_SZPOS = 6 ) 

C    Offsets for the TDPL bank follow 

          integer TDPL_TRK_PLN               !  (track_number) *100 + plane_number
          parameter ( TDPL_TRK_PLN = 1 ) 
          integer TDPL_X               !  z coord [cm]  for track in this plane
          parameter ( TDPL_X = 2 ) 
          integer TDPL_Y               !  y coord [cm]  for track in this plane
          parameter ( TDPL_Y = 3 ) 
          integer TDPL_Z               !  z coord [cm]  for track in this plane
          parameter ( TDPL_Z = 4 ) 
          integer TDPL_CX               !  direction cosine (cx) at coord.{x,y,z}
          parameter ( TDPL_CX = 5 ) 
          integer TDPL_CY               !  direction cosine (cy) at coord.{x,y,z}
          parameter ( TDPL_CY = 6 ) 
          integer TDPL_CZ               !  direction cosine (cz) at coord.{x,y,z}
          parameter ( TDPL_CZ = 7 ) 
          integer TDPL_TLEN               !  track length [cm] from origin to this plane
          parameter ( TDPL_TLEN = 8 ) 

C    Offsets for the TRGS bank follow 

          integer TRGS_CLOCK_UG               !  Ungated Clock
          parameter ( TRGS_CLOCK_UG = 1 ) 
          integer TRGS_FCUP_UG               !  FCUP UNGATED
          parameter ( TRGS_FCUP_UG = 2 ) 
          integer TRGS_NOTUSED3               !  Currently not used
          parameter ( TRGS_NOTUSED3 = 3 ) 
          integer TRGS_NOTUSED4               !  Currently not used  
          parameter ( TRGS_NOTUSED4 = 4 ) 
          integer TRGS_NOTUSED5               !  Currently not used  
          parameter ( TRGS_NOTUSED5 = 5 ) 
          integer TRGS_NOTUSED6               !  Currently not used  
          parameter ( TRGS_NOTUSED6 = 6 ) 
          integer TRGS_NOTUSED7               !  Currently not used  
          parameter ( TRGS_NOTUSED7 = 7 ) 
          integer TRGS_NOTUSED8               !  Currently not used 
          parameter ( TRGS_NOTUSED8 = 8 ) 
          integer TRGS_MASTER_OR               !  Master OR
          parameter ( TRGS_MASTER_OR = 9 ) 
          integer TRGS_PC               !  Pair Counter
          parameter ( TRGS_PC = 10 ) 
          integer TRGS_PS               !  Pair Spectrometer
          parameter ( TRGS_PS = 11 ) 
          integer TRGS_TAC               !  Total Absorption Counter
          parameter ( TRGS_TAC = 12 ) 
          integer TRGS_NOTUSED13               !  Currently not used    
          parameter ( TRGS_NOTUSED13 = 13 ) 
          integer TRGS_NOTUSED14               !  Currently not used    
          parameter ( TRGS_NOTUSED14 = 14 ) 
          integer TRGS_CLOCK_UG_2               !  Currently not used    
          parameter ( TRGS_CLOCK_UG_2 = 15 ) 
          integer TRGS_FCUP_UG_2               !  Currently not used    
          parameter ( TRGS_FCUP_UG_2 = 16 ) 
          integer TRGS_CLOCK_G1               !  Clock with run gate
          parameter ( TRGS_CLOCK_G1 = 17 ) 
          integer TRGS_FCUP_G1               !  FCUP with Run Gate
          parameter ( TRGS_FCUP_G1 = 18 ) 
          integer TRGS_NOTUSED19               !  Currently not used  
          parameter ( TRGS_NOTUSED19 = 19 ) 
          integer TRGS_NOTUSED20               !  Currently not used  
          parameter ( TRGS_NOTUSED20 = 20 ) 
          integer TRGS_NOTUSED21               !  Currently not used  
          parameter ( TRGS_NOTUSED21 = 21 ) 
          integer TRGS_NOTUSED22               !  Currently not used  
          parameter ( TRGS_NOTUSED22 = 22 ) 
          integer TRGS_NOTUSED23               !  Currently not used  
          parameter ( TRGS_NOTUSED23 = 23 ) 
          integer TRGS_NOTUSED24               !  Currently not used  
          parameter ( TRGS_NOTUSED24 = 24 ) 
          integer TRGS_NOTUSED25               !  Currently not used  
          parameter ( TRGS_NOTUSED25 = 25 ) 
          integer TRGS_NOTUSED26               !  Currently not used  
          parameter ( TRGS_NOTUSED26 = 26 ) 
          integer TRGS_NOTUSED27               !  Currently not used  
          parameter ( TRGS_NOTUSED27 = 27 ) 
          integer TRGS_NOTUSED28               !  Currently not used  
          parameter ( TRGS_NOTUSED28 = 28 ) 
          integer TRGS_NOTUSED29               !  Currently not used  
          parameter ( TRGS_NOTUSED29 = 29 ) 
          integer TRGS_NOTUSED30               !  Currently not used  
          parameter ( TRGS_NOTUSED30 = 30 ) 
          integer TRGS_CLOCK_G1_2               !  Currently not used  
          parameter ( TRGS_CLOCK_G1_2 = 31 ) 
          integer TRGS_FCUP_G1_2               !  Currently not used  
          parameter ( TRGS_FCUP_G1_2 = 32 ) 
          integer TRGS_CLOCK_G2               !  CLOCK with Live gate
          parameter ( TRGS_CLOCK_G2 = 33 ) 
          integer TRGS_FCUP_G2               !  FCUP with Live gate
          parameter ( TRGS_FCUP_G2 = 34 ) 
          integer TRGS_NOTUSED35               !  Currently not used  
          parameter ( TRGS_NOTUSED35 = 35 ) 
          integer TRGS_NOTUSED36               !  Currently not used  
          parameter ( TRGS_NOTUSED36 = 36 ) 
          integer TRGS_NOTUSED37               !  Currently not used  
          parameter ( TRGS_NOTUSED37 = 37 ) 
          integer TRGS_NOTUSED38               !  Currently not used  
          parameter ( TRGS_NOTUSED38 = 38 ) 
          integer TRGS_NOTUSED39               !  Currently not used  
          parameter ( TRGS_NOTUSED39 = 39 ) 
          integer TRGS_NOTUSED40               !  Currently not used  
          parameter ( TRGS_NOTUSED40 = 40 ) 
          integer TRGS_NOTUSED41               !  Currently not used  
          parameter ( TRGS_NOTUSED41 = 41 ) 
          integer TRGS_NOTUSED42               !  Currently not used  
          parameter ( TRGS_NOTUSED42 = 42 ) 
          integer TRGS_NOTUSED43               !  Currently not used  
          parameter ( TRGS_NOTUSED43 = 43 ) 
          integer TRGS_NOTUSED44               !  Currently not used  
          parameter ( TRGS_NOTUSED44 = 44 ) 
          integer TRGS_NOTUSED45               !  Currently not used  
          parameter ( TRGS_NOTUSED45 = 45 ) 
          integer TRGS_NOTUSED46               !  Currently not used  
          parameter ( TRGS_NOTUSED46 = 46 ) 
          integer TRGS_CLOCK_G2_2               !  Currently not used  
          parameter ( TRGS_CLOCK_G2_2 = 47 ) 
          integer TRGS_FCUP_G2_2               !  Currently not used   
          parameter ( TRGS_FCUP_G2_2 = 48 ) 
          integer TRGS_TRIG1_UG               !  Trigger 1 ungated, prescaled
          parameter ( TRGS_TRIG1_UG = 49 ) 
          integer TRGS_TRIG2_UG               !  Trigger 2 ungated, prescaled
          parameter ( TRGS_TRIG2_UG = 50 ) 
          integer TRGS_TRIG3_UG               !  Trigger 3 ungated, prescaled
          parameter ( TRGS_TRIG3_UG = 51 ) 
          integer TRGS_TRIG4_UG               !  Trigger 4 ungated, prescaled
          parameter ( TRGS_TRIG4_UG = 52 ) 
          integer TRGS_TRIG5_UG               !  Trigger 5 ungated, prescaled
          parameter ( TRGS_TRIG5_UG = 53 ) 
          integer TRGS_TRIG6_UG               !  Trigger 6 ungated, prescaled
          parameter ( TRGS_TRIG6_UG = 54 ) 
          integer TRGS_TRIG7_UG               !  Trigger 7 ungated, prescaled
          parameter ( TRGS_TRIG7_UG = 55 ) 
          integer TRGS_TRIG8_UG               !  Trigger 8 ungated, prescaled
          parameter ( TRGS_TRIG8_UG = 56 ) 
          integer TRGS_TRIG9_UG               !  Trigger 9 ungated, prescaled
          parameter ( TRGS_TRIG9_UG = 57 ) 
          integer TRGS_TRIG10_UG               !  Trigger 10 ungated, prescaled
          parameter ( TRGS_TRIG10_UG = 58 ) 
          integer TRGS_TRIG11_UG               !  Trigger 11 ungated, prescaled
          parameter ( TRGS_TRIG11_UG = 59 ) 
          integer TRGS_TRIG12_UG               !  Trigger 12 ungated, prescaled
          parameter ( TRGS_TRIG12_UG = 60 ) 
          integer TRGS_TRIG_OR_UG               !  Trigger OR of 1-12 ungated, prescaled
          parameter ( TRGS_TRIG_OR_UG = 61 ) 
          integer TRGS_NOTUSED62               !  Currently not used  
          parameter ( TRGS_NOTUSED62 = 62 ) 
          integer TRGS_NOTUSED63               !  Currently not used  
          parameter ( TRGS_NOTUSED63 = 63 ) 
          integer TRGS_NOTUSED64               !  Currently not used  
          parameter ( TRGS_NOTUSED64 = 64 ) 

C    Offsets for the TRKS bank follow 

          integer TRKS_TRK_LEV               !  Tracknr + analysis_level*100
          parameter ( TRKS_TRK_LEV = 1 ) 
          integer TRKS_BETA               !  Beta of the track in unit of c
          parameter ( TRKS_BETA = 2 ) 
          integer TRKS_ST_TIME               !  Start time of the event (track: photon trigg)
          parameter ( TRKS_ST_TIME = 3 ) 
          integer TRKS_CC_TIME               !  CC calibrated time for this track (ns)
          parameter ( TRKS_CC_TIME = 4 ) 
          integer TRKS_SC_TIME               !  SC calibrated time for this track (ns)
          parameter ( TRKS_SC_TIME = 5 ) 
          integer TRKS_EC_TIME               !  EC calibrated time for this track (ns)
          parameter ( TRKS_EC_TIME = 6 ) 
          integer TRKS_ST_ID               !  ST hit id (ptr. to STx: photon trigg)
          parameter ( TRKS_ST_ID = 7 ) 
          integer TRKS_CC_ID               !  CC hit id (ptr. to CCRC)
          parameter ( TRKS_CC_ID = 8 ) 
          integer TRKS_SC_ID               !  SC hit id (ptr. to SCRW)
          parameter ( TRKS_SC_ID = 9 ) 
          integer TRKS_EC_ID               !  EC Cluster-id (ptr. to ECHB)
          parameter ( TRKS_EC_ID = 10 ) 

C    Offsets for the UNUS bank follow 

          integer UNUS_NDCUN               !  Number of unused DC tracks
          parameter ( UNUS_NDCUN = 1 ) 
          integer UNUS_IDCUN               !  Trek ID  
          parameter ( UNUS_IDCUN = 2 ) 
          integer UNUS_NSCUN               !  Number of unused SC hits
          parameter ( UNUS_NSCUN = 1 ) 
          integer UNUS_ISCUN               !  SC hit ID
          parameter ( UNUS_ISCUN = 2 ) 
          integer UNUS_NCCUN               !  Number of unused CC hits
          parameter ( UNUS_NCCUN = 1 ) 
          integer UNUS_ICCUN               !  CC hit ID
          parameter ( UNUS_ICCUN = 2 ) 
          integer UNUS_NECUN               !  Number of unused EC hits
          parameter ( UNUS_NECUN = 1 ) 
          integer UNUS_IECUN               !  EC hit ID
          parameter ( UNUS_IECUN = 2 ) 

C    Offsets for the VERT bank follow 

          integer VERT_VERTEX               !  vertex id
          parameter ( VERT_VERTEX = 1 ) 
          integer VERT_TRK1               !  track #1
          parameter ( VERT_TRK1 = 2 ) 
          integer VERT_TRK2               !  track #2
          parameter ( VERT_TRK2 = 3 ) 
          integer VERT_X               !  x
          parameter ( VERT_X = 4 ) 
          integer VERT_Y               !  y Vertex position {x,y,z}
          parameter ( VERT_Y = 5 ) 
          integer VERT_Z               !  z
          parameter ( VERT_Z = 6 ) 
          integer VERT_SEPD               !  seperation distance
          parameter ( VERT_SEPD = 7 ) 
          integer VERT_SIGX               !  sigma in x
          parameter ( VERT_SIGX = 8 ) 
          integer VERT_SIGY               !  sigma in y
          parameter ( VERT_SIGY = 9 ) 
          integer VERT_SIGZ               !  sigma in z
          parameter ( VERT_SIGZ = 10 ) 
          integer VERT_SIGSEPD               !  sigma in sepd   
          parameter ( VERT_SIGSEPD = 11 ) 
