
          integer N_DDL_BANKS 
          parameter ( N_DDL_BANKS = 83 ) 

          integer INDEX_TO_BANKS( N_DDL_BANKS ), INDEX16_TO_BANKS( N_DDL_BANKS ) 

          integer WHICH_ARRAY( N_DDL_BANKS ) 

          character*4 ddl_bank_names( N_DDL_BANKS ) 

        integer  BMPR_ind, CALL_ind, CC_ind, CC01_ind
        integer  CC1_ind, CCH_ind, CCH1_ind, CCH2_ind
        integer  CCPB_ind, CCRC_ind, DC0_ind, DC1_ind
        integer  DCDW_ind, DCGM_ind, DCGW_ind, DCH_ind
        integer  DCPB_ind, DCV1_ind, DCV2_ind, DCV3_ind
        integer  DDLY_ind, DGEO_ind, DTRK_ind, EC_ind
        integer  EC01_ind, EC1_ind, EC1P_ind, EC1R_ind
        integer  ECCA_ind, ECCL_ind, ECCT_ind, ECG_ind
        integer  ECH_ind, ECHB_ind, ECP_ind, ECPB_ind
        integer  ECPI_ind, ECPO_ind, ECRB_ind, EID0_ind
        integer  EVNT_ind, HBER_ind, HBID_ind, HBLA_ind
        integer  HBTB_ind, HBTR_ind, HCAL_ind, HDPL_ind
        integer  HEAD_ind, HEVT_ind, MCEV_ind, MCTK_ind
        integer  MCVX_ind, PART_ind, SC_ind, SC1_ind
        integer  SCC_ind, SCG_ind, SCH_ind, SCI_ind
        integer  ScIf_ind, SCMD_ind, SCP_ind, SCPB_ind
        integer  SCR_ind, SCRB_ind, SCRC_ind, SCRW_ind
        integer  SCRX_ind, ST0_ind, ST1_ind, TAGE_ind
        integer  TAGR_ind, TAGT_ind, TBER_ind, TBLA_ind
        integer  TBTR_ind, TCSB_ind, TDPL_ind, TRGS_ind
        integer  TRKS_ind, UNUS_ind, VERT_ind
        integer  BMPR_ind16, CALL_ind16, CC_ind16, CC01_ind16
        integer  CC1_ind16, CCH_ind16, CCH1_ind16, CCH2_ind16
        integer  CCPB_ind16, CCRC_ind16, DC0_ind16, DC1_ind16
        integer  DCDW_ind16, DCGM_ind16, DCGW_ind16, DCH_ind16
        integer  DCPB_ind16, DCV1_ind16, DCV2_ind16, DCV3_ind16
        integer  DDLY_ind16, DGEO_ind16, DTRK_ind16, EC_ind16
        integer  EC01_ind16, EC1_ind16, EC1P_ind16, EC1R_ind16
        integer  ECCA_ind16, ECCL_ind16, ECCT_ind16, ECG_ind16
        integer  ECH_ind16, ECHB_ind16, ECP_ind16, ECPB_ind16
        integer  ECPI_ind16, ECPO_ind16, ECRB_ind16, EID0_ind16
        integer  EVNT_ind16, HBER_ind16, HBID_ind16, HBLA_ind16
        integer  HBTB_ind16, HBTR_ind16, HCAL_ind16, HDPL_ind16
        integer  HEAD_ind16, HEVT_ind16, MCEV_ind16, MCTK_ind16
        integer  MCVX_ind16, PART_ind16, SC_ind16, SC1_ind16
        integer  SCC_ind16, SCG_ind16, SCH_ind16, SCI_ind16
        integer  ScIf_ind16, SCMD_ind16, SCP_ind16, SCPB_ind16
        integer  SCR_ind16, SCRB_ind16, SCRC_ind16, SCRW_ind16
        integer  SCRX_ind16, ST0_ind16, ST1_ind16, TAGE_ind16
        integer  TAGR_ind16, TAGT_ind16, TBER_ind16, TBLA_ind16
        integer  TBTR_ind16, TCSB_ind16, TDPL_ind16, TRGS_ind16
        integer  TRKS_ind16, UNUS_ind16, VERT_ind16
        integer  BMPR_array, CALL_array, CC_array, CC01_array
        integer  CC1_array, CCH_array, CCH1_array, CCH2_array
        integer  CCPB_array, CCRC_array, DC0_array, DC1_array
        integer  DCDW_array, DCGM_array, DCGW_array, DCH_array
        integer  DCPB_array, DCV1_array, DCV2_array, DCV3_array
        integer  DDLY_array, DGEO_array, DTRK_array, EC_array
        integer  EC01_array, EC1_array, EC1P_array, EC1R_array
        integer  ECCA_array, ECCL_array, ECCT_array, ECG_array
        integer  ECH_array, ECHB_array, ECP_array, ECPB_array
        integer  ECPI_array, ECPO_array, ECRB_array, EID0_array
        integer  EVNT_array, HBER_array, HBID_array, HBLA_array
        integer  HBTB_array, HBTR_array, HCAL_array, HDPL_array
        integer  HEAD_array, HEVT_array, MCEV_array, MCTK_array
        integer  MCVX_array, PART_array, SC_array, SC1_array
        integer  SCC_array, SCG_array, SCH_array, SCI_array
        integer  ScIf_array, SCMD_array, SCP_array, SCPB_array
        integer  SCR_array, SCRB_array, SCRC_array, SCRW_array
        integer  SCRX_array, ST0_array, ST1_array, TAGE_array
        integer  TAGR_array, TAGT_array, TBER_array, TBLA_array
        integer  TBTR_array, TCSB_array, TDPL_array, TRGS_array
        integer  TRKS_array, UNUS_array, VERT_array

C define the COMMON BLOCK 


          common /clas_index/
     +        index_to_banks, index16_to_banks, which_array 

C  Now equivalance the array to the individual names
        EQUIVALENCE (INDEX_TO_BANKS(1) , BMPR_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(2) , CALL_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(3) , CC_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(4) , CC01_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(5) , CC1_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(6) , CCH_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(7) , CCH1_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(8) , CCH2_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(9) , CCPB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(10) , CCRC_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(11) , DC0_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(12) , DC1_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(13) , DCDW_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(14) , DCGM_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(15) , DCGW_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(16) , DCH_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(17) , DCPB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(18) , DCV1_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(19) , DCV2_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(20) , DCV3_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(21) , DDLY_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(22) , DGEO_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(23) , DTRK_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(24) , EC_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(25) , EC01_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(26) , EC1_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(27) , EC1P_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(28) , EC1R_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(29) , ECCA_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(30) , ECCL_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(31) , ECCT_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(32) , ECG_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(33) , ECH_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(34) , ECHB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(35) , ECP_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(36) , ECPB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(37) , ECPI_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(38) , ECPO_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(39) , ECRB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(40) , EID0_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(41) , EVNT_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(42) , HBER_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(43) , HBID_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(44) , HBLA_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(45) , HBTB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(46) , HBTR_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(47) , HCAL_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(48) , HDPL_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(49) , HEAD_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(50) , HEVT_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(51) , MCEV_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(52) , MCTK_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(53) , MCVX_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(54) , PART_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(55) , SC_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(56) , SC1_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(57) , SCC_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(58) , SCG_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(59) , SCH_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(60) , SCI_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(61) , ScIf_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(62) , SCMD_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(63) , SCP_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(64) , SCPB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(65) , SCR_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(66) , SCRB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(67) , SCRC_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(68) , SCRW_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(69) , SCRX_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(70) , ST0_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(71) , ST1_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(72) , TAGE_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(73) , TAGR_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(74) , TAGT_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(75) , TBER_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(76) , TBLA_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(77) , TBTR_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(78) , TCSB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(79) , TDPL_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(80) , TRGS_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(81) , TRKS_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(82) , UNUS_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(83) , VERT_ind) 

C  Now equivalance the array to the individual names for the 16 bit banks
        EQUIVALENCE (INDEX16_TO_BANKS(1) , BMPR_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(2) , CALL_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(3) , CC_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(4) , CC01_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(5) , CC1_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(6) , CCH_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(7) , CCH1_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(8) , CCH2_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(9) , CCPB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(10) , CCRC_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(11) , DC0_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(12) , DC1_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(13) , DCDW_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(14) , DCGM_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(15) , DCGW_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(16) , DCH_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(17) , DCPB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(18) , DCV1_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(19) , DCV2_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(20) , DCV3_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(21) , DDLY_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(22) , DGEO_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(23) , DTRK_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(24) , EC_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(25) , EC01_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(26) , EC1_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(27) , EC1P_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(28) , EC1R_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(29) , ECCA_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(30) , ECCL_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(31) , ECCT_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(32) , ECG_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(33) , ECH_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(34) , ECHB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(35) , ECP_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(36) , ECPB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(37) , ECPI_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(38) , ECPO_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(39) , ECRB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(40) , EID0_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(41) , EVNT_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(42) , HBER_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(43) , HBID_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(44) , HBLA_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(45) , HBTB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(46) , HBTR_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(47) , HCAL_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(48) , HDPL_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(49) , HEAD_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(50) , HEVT_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(51) , MCEV_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(52) , MCTK_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(53) , MCVX_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(54) , PART_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(55) , SC_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(56) , SC1_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(57) , SCC_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(58) , SCG_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(59) , SCH_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(60) , SCI_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(61) , ScIf_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(62) , SCMD_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(63) , SCP_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(64) , SCPB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(65) , SCR_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(66) , SCRB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(67) , SCRC_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(68) , SCRW_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(69) , SCRX_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(70) , ST0_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(71) , ST1_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(72) , TAGE_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(73) , TAGR_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(74) , TAGT_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(75) , TBER_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(76) , TBLA_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(77) , TBTR_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(78) , TCSB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(79) , TDPL_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(80) , TRGS_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(81) , TRKS_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(82) , UNUS_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(83) , VERT_ind16) 

C  Now equivalance the array which_bank which is a flag to which
     
C  IW or JW array 
        EQUIVALENCE (WHICH_ARRAY(1) , BMPR_array) 
        EQUIVALENCE (WHICH_ARRAY(2) , CALL_array) 
        EQUIVALENCE (WHICH_ARRAY(3) , CC_array) 
        EQUIVALENCE (WHICH_ARRAY(4) , CC01_array) 
        EQUIVALENCE (WHICH_ARRAY(5) , CC1_array) 
        EQUIVALENCE (WHICH_ARRAY(6) , CCH_array) 
        EQUIVALENCE (WHICH_ARRAY(7) , CCH1_array) 
        EQUIVALENCE (WHICH_ARRAY(8) , CCH2_array) 
        EQUIVALENCE (WHICH_ARRAY(9) , CCPB_array) 
        EQUIVALENCE (WHICH_ARRAY(10) , CCRC_array) 
        EQUIVALENCE (WHICH_ARRAY(11) , DC0_array) 
        EQUIVALENCE (WHICH_ARRAY(12) , DC1_array) 
        EQUIVALENCE (WHICH_ARRAY(13) , DCDW_array) 
        EQUIVALENCE (WHICH_ARRAY(14) , DCGM_array) 
        EQUIVALENCE (WHICH_ARRAY(15) , DCGW_array) 
        EQUIVALENCE (WHICH_ARRAY(16) , DCH_array) 
        EQUIVALENCE (WHICH_ARRAY(17) , DCPB_array) 
        EQUIVALENCE (WHICH_ARRAY(18) , DCV1_array) 
        EQUIVALENCE (WHICH_ARRAY(19) , DCV2_array) 
        EQUIVALENCE (WHICH_ARRAY(20) , DCV3_array) 
        EQUIVALENCE (WHICH_ARRAY(21) , DDLY_array) 
        EQUIVALENCE (WHICH_ARRAY(22) , DGEO_array) 
        EQUIVALENCE (WHICH_ARRAY(23) , DTRK_array) 
        EQUIVALENCE (WHICH_ARRAY(24) , EC_array) 
        EQUIVALENCE (WHICH_ARRAY(25) , EC01_array) 
        EQUIVALENCE (WHICH_ARRAY(26) , EC1_array) 
        EQUIVALENCE (WHICH_ARRAY(27) , EC1P_array) 
        EQUIVALENCE (WHICH_ARRAY(28) , EC1R_array) 
        EQUIVALENCE (WHICH_ARRAY(29) , ECCA_array) 
        EQUIVALENCE (WHICH_ARRAY(30) , ECCL_array) 
        EQUIVALENCE (WHICH_ARRAY(31) , ECCT_array) 
        EQUIVALENCE (WHICH_ARRAY(32) , ECG_array) 
        EQUIVALENCE (WHICH_ARRAY(33) , ECH_array) 
        EQUIVALENCE (WHICH_ARRAY(34) , ECHB_array) 
        EQUIVALENCE (WHICH_ARRAY(35) , ECP_array) 
        EQUIVALENCE (WHICH_ARRAY(36) , ECPB_array) 
        EQUIVALENCE (WHICH_ARRAY(37) , ECPI_array) 
        EQUIVALENCE (WHICH_ARRAY(38) , ECPO_array) 
        EQUIVALENCE (WHICH_ARRAY(39) , ECRB_array) 
        EQUIVALENCE (WHICH_ARRAY(40) , EID0_array) 
        EQUIVALENCE (WHICH_ARRAY(41) , EVNT_array) 
        EQUIVALENCE (WHICH_ARRAY(42) , HBER_array) 
        EQUIVALENCE (WHICH_ARRAY(43) , HBID_array) 
        EQUIVALENCE (WHICH_ARRAY(44) , HBLA_array) 
        EQUIVALENCE (WHICH_ARRAY(45) , HBTB_array) 
        EQUIVALENCE (WHICH_ARRAY(46) , HBTR_array) 
        EQUIVALENCE (WHICH_ARRAY(47) , HCAL_array) 
        EQUIVALENCE (WHICH_ARRAY(48) , HDPL_array) 
        EQUIVALENCE (WHICH_ARRAY(49) , HEAD_array) 
        EQUIVALENCE (WHICH_ARRAY(50) , HEVT_array) 
        EQUIVALENCE (WHICH_ARRAY(51) , MCEV_array) 
        EQUIVALENCE (WHICH_ARRAY(52) , MCTK_array) 
        EQUIVALENCE (WHICH_ARRAY(53) , MCVX_array) 
        EQUIVALENCE (WHICH_ARRAY(54) , PART_array) 
        EQUIVALENCE (WHICH_ARRAY(55) , SC_array) 
        EQUIVALENCE (WHICH_ARRAY(56) , SC1_array) 
        EQUIVALENCE (WHICH_ARRAY(57) , SCC_array) 
        EQUIVALENCE (WHICH_ARRAY(58) , SCG_array) 
        EQUIVALENCE (WHICH_ARRAY(59) , SCH_array) 
        EQUIVALENCE (WHICH_ARRAY(60) , SCI_array) 
        EQUIVALENCE (WHICH_ARRAY(61) , ScIf_array) 
        EQUIVALENCE (WHICH_ARRAY(62) , SCMD_array) 
        EQUIVALENCE (WHICH_ARRAY(63) , SCP_array) 
        EQUIVALENCE (WHICH_ARRAY(64) , SCPB_array) 
        EQUIVALENCE (WHICH_ARRAY(65) , SCR_array) 
        EQUIVALENCE (WHICH_ARRAY(66) , SCRB_array) 
        EQUIVALENCE (WHICH_ARRAY(67) , SCRC_array) 
        EQUIVALENCE (WHICH_ARRAY(68) , SCRW_array) 
        EQUIVALENCE (WHICH_ARRAY(69) , SCRX_array) 
        EQUIVALENCE (WHICH_ARRAY(70) , ST0_array) 
        EQUIVALENCE (WHICH_ARRAY(71) , ST1_array) 
        EQUIVALENCE (WHICH_ARRAY(72) , TAGE_array) 
        EQUIVALENCE (WHICH_ARRAY(73) , TAGR_array) 
        EQUIVALENCE (WHICH_ARRAY(74) , TAGT_array) 
        EQUIVALENCE (WHICH_ARRAY(75) , TBER_array) 
        EQUIVALENCE (WHICH_ARRAY(76) , TBLA_array) 
        EQUIVALENCE (WHICH_ARRAY(77) , TBTR_array) 
        EQUIVALENCE (WHICH_ARRAY(78) , TCSB_array) 
        EQUIVALENCE (WHICH_ARRAY(79) , TDPL_array) 
        EQUIVALENCE (WHICH_ARRAY(80) , TRGS_array) 
        EQUIVALENCE (WHICH_ARRAY(81) , TRKS_array) 
        EQUIVALENCE (WHICH_ARRAY(82) , UNUS_array) 
        EQUIVALENCE (WHICH_ARRAY(83) , VERT_array) 

C  Define the DDL bank names 
         data  DDL_BANK_NAMES(1)/'BMPR'/ 
         data  DDL_BANK_NAMES(2)/'CALL'/ 
         data  DDL_BANK_NAMES(3)/'CC  '/ 
         data  DDL_BANK_NAMES(4)/'CC01'/ 
         data  DDL_BANK_NAMES(5)/'CC1 '/ 
         data  DDL_BANK_NAMES(6)/'CCH '/ 
         data  DDL_BANK_NAMES(7)/'CCH1'/ 
         data  DDL_BANK_NAMES(8)/'CCH2'/ 
         data  DDL_BANK_NAMES(9)/'CCPB'/ 
         data  DDL_BANK_NAMES(10)/'CCRC'/ 
         data  DDL_BANK_NAMES(11)/'DC0 '/ 
         data  DDL_BANK_NAMES(12)/'DC1 '/ 
         data  DDL_BANK_NAMES(13)/'DCDW'/ 
         data  DDL_BANK_NAMES(14)/'DCGM'/ 
         data  DDL_BANK_NAMES(15)/'DCGW'/ 
         data  DDL_BANK_NAMES(16)/'DCH '/ 
         data  DDL_BANK_NAMES(17)/'DCPB'/ 
         data  DDL_BANK_NAMES(18)/'DCV1'/ 
         data  DDL_BANK_NAMES(19)/'DCV2'/ 
         data  DDL_BANK_NAMES(20)/'DCV3'/ 
         data  DDL_BANK_NAMES(21)/'DDLY'/ 
         data  DDL_BANK_NAMES(22)/'DGEO'/ 
         data  DDL_BANK_NAMES(23)/'DTRK'/ 
         data  DDL_BANK_NAMES(24)/'EC  '/ 
         data  DDL_BANK_NAMES(25)/'EC01'/ 
         data  DDL_BANK_NAMES(26)/'EC1 '/ 
         data  DDL_BANK_NAMES(27)/'EC1P'/ 
         data  DDL_BANK_NAMES(28)/'EC1R'/ 
         data  DDL_BANK_NAMES(29)/'ECCA'/ 
         data  DDL_BANK_NAMES(30)/'ECCL'/ 
         data  DDL_BANK_NAMES(31)/'ECCT'/ 
         data  DDL_BANK_NAMES(32)/'ECG '/ 
         data  DDL_BANK_NAMES(33)/'ECH '/ 
         data  DDL_BANK_NAMES(34)/'ECHB'/ 
         data  DDL_BANK_NAMES(35)/'ECP '/ 
         data  DDL_BANK_NAMES(36)/'ECPB'/ 
         data  DDL_BANK_NAMES(37)/'ECPI'/ 
         data  DDL_BANK_NAMES(38)/'ECPO'/ 
         data  DDL_BANK_NAMES(39)/'ECRB'/ 
         data  DDL_BANK_NAMES(40)/'EID0'/ 
         data  DDL_BANK_NAMES(41)/'EVNT'/ 
         data  DDL_BANK_NAMES(42)/'HBER'/ 
         data  DDL_BANK_NAMES(43)/'HBID'/ 
         data  DDL_BANK_NAMES(44)/'HBLA'/ 
         data  DDL_BANK_NAMES(45)/'HBTB'/ 
         data  DDL_BANK_NAMES(46)/'HBTR'/ 
         data  DDL_BANK_NAMES(47)/'HCAL'/ 
         data  DDL_BANK_NAMES(48)/'HDPL'/ 
         data  DDL_BANK_NAMES(49)/'HEAD'/ 
         data  DDL_BANK_NAMES(50)/'HEVT'/ 
         data  DDL_BANK_NAMES(51)/'MCEV'/ 
         data  DDL_BANK_NAMES(52)/'MCTK'/ 
         data  DDL_BANK_NAMES(53)/'MCVX'/ 
         data  DDL_BANK_NAMES(54)/'PART'/ 
         data  DDL_BANK_NAMES(55)/'SC  '/ 
         data  DDL_BANK_NAMES(56)/'SC1 '/ 
         data  DDL_BANK_NAMES(57)/'SCC '/ 
         data  DDL_BANK_NAMES(58)/'SCG '/ 
         data  DDL_BANK_NAMES(59)/'SCH '/ 
         data  DDL_BANK_NAMES(60)/'SCI '/ 
         data  DDL_BANK_NAMES(61)/'SCIF'/ 
         data  DDL_BANK_NAMES(62)/'SCMD'/ 
         data  DDL_BANK_NAMES(63)/'SCP '/ 
         data  DDL_BANK_NAMES(64)/'SCPB'/ 
         data  DDL_BANK_NAMES(65)/'SCR '/ 
         data  DDL_BANK_NAMES(66)/'SCRB'/ 
         data  DDL_BANK_NAMES(67)/'SCRC'/ 
         data  DDL_BANK_NAMES(68)/'SCRW'/ 
         data  DDL_BANK_NAMES(69)/'SCRX'/ 
         data  DDL_BANK_NAMES(70)/'ST0 '/ 
         data  DDL_BANK_NAMES(71)/'ST1 '/ 
         data  DDL_BANK_NAMES(72)/'TAGE'/ 
         data  DDL_BANK_NAMES(73)/'TAGR'/ 
         data  DDL_BANK_NAMES(74)/'TAGT'/ 
         data  DDL_BANK_NAMES(75)/'TBER'/ 
         data  DDL_BANK_NAMES(76)/'TBLA'/ 
         data  DDL_BANK_NAMES(77)/'TBTR'/ 
         data  DDL_BANK_NAMES(78)/'TCSB'/ 
         data  DDL_BANK_NAMES(79)/'TDPL'/ 
         data  DDL_BANK_NAMES(80)/'TRGS'/ 
         data  DDL_BANK_NAMES(81)/'TRKS'/ 
         data  DDL_BANK_NAMES(82)/'UNUS'/ 
         data  DDL_BANK_NAMES(83)/'VERT'/ 
