
          integer N_DDL_BANKS 
          parameter ( N_DDL_BANKS = 183 ) 

          integer INDEX_TO_BANKS( N_DDL_BANKS ), INDEX16_TO_BANKS( N_DDL_BANKS ) 

          integer WHICH_ARRAY( N_DDL_BANKS ) 

          character*4 ddl_bank_names( N_DDL_BANKS ) 

        integer  BEAM_ind, BMPR_ind, BREP_ind, CALB_ind
        integer  CALL_ind, CC01_ind, CC1_ind, CC_ind
        integer  CCDI_ind, CCH_ind, CCMT_ind, CCPB_ind
        integer  CCPE_ind, CCRC_ind, CCS_ind, CHI2_ind
        integer  CL01_ind, CLST_ind, CPED_ind, CSQL_ind
        integer  DC0_ind, DC1_ind, DCDW_ind, DCGM_ind
        integer  DCGW_ind, DCH_ind, DCMN_ind, DCPB_ind
        integer  DCV1_ind, DCV2_ind, DCV3_ind, DDLY_ind
        integer  DGEO_ind, DHCL_ind, DITM_ind, DOCA_ind
        integer  DPCP_ind, DPSP_ind, DSPC_ind, DSPS_ind
        integer  DSTC_ind, DTCP_ind, DTRK_ind, EC01_ind
        integer  EC1_ind, EC1P_ind, EC1R_ind, ECCA_ind
        integer  ECCL_ind, ECCT_ind, EC_ind, ECDI_ind
        integer  ECG_ind, ECHB_ind, ECH_ind, ECMT_ind
        integer  ECP1_ind, ECPB_ind, ECPC_ind, ECP_ind
        integer  ECPE_ind, ECPI_ind, ECPO_ind, ECRB_ind
        integer  ECS_ind, EID0_ind, EPIC_ind, EVNT_ind
        integer  FBPM_ind, G1SL_ind, G2SL_ind, G3SL_ind
        integer  G4SL_ind, GPAR_ind, GPID_ind, GP_X_ind
        integer  GP_Y_ind, HBER_ind, HBID_ind, HBLA_ind
        integer  HBTB_ind, HBTR_ind, HCAL_ind, HDPL_ind
        integer  HEAD_ind, HEVT_ind, HLS_ind, IC_ind
        integer  ICHB_ind, KFIT_ind, L1PG_ind, L2H_ind
        integer  L2S_ind, LASR_ind, LCDI_ind, LCPB_ind
        integer  LOWQ_ind, MCEV_ind, MCHD_ind, MCTK_ind
        integer  MCVX_ind, MTRK_ind, MVRT_ind, PART_ind
        integer  PCO_ind, PHTM_ind, PID1_ind, PID2_ind
        integer  PID3_ind, PID4_ind, PID5_ind, PID6_ind
        integer  PIDT_ind, PRTM_ind, PSO_ind, PTDB_ind
        integer  RCST_ind, RF_ind, RGLK_ind, RNLG_ind
        integer  RNPE_ind, RTSL_ind, RUNC_ind, S1ST_ind
        integer  SC1_ind, SCC_ind, SC_ind, SCDI_ind
        integer  SCGD_ind, SCG_ind, SCH_ind, SCMD_ind
        integer  SCMT_ind, SCMW_ind, SCPB_ind, SCP_ind
        integer  SCPE_ind, SCPS_ind, SCRC_ind, SCR_ind
        integer  SCS_ind, SGMP_ind, SPAR_ind, SPIN_ind
        integer  ST1_ind, ST_ind, STG_ind, STH_ind
        integer  STN0_ind, STN1_ind, STPB_ind, STPE_ind
        integer  STR_ind, STS_ind, STSN_ind, SYNC_ind
        integer  TACO_ind, TAGE_ind, TAGI_ind, TAGM_ind
        integer  TAGR_ind, TAGT_ind, TBER_ind, TBID_ind
        integer  TBLA_ind, TBTR_ind, TCSB_ind, TDPL_ind
        integer  TESC_ind, TGBI_ind, TGEO_ind, TGPB_ind
        integer  TGS_ind, TLV1_ind, TRCF_ind, TRGS_ind
        integer  TRKS_ind, TRL1_ind, TRPB_ind, TSPR_ind
        integer  TSRG_ind, UNUS_ind, VERT_ind
        integer  BEAM_ind16, BMPR_ind16, BREP_ind16, CALB_ind16
        integer  CALL_ind16, CC01_ind16, CC1_ind16, CC_ind16
        integer  CCDI_ind16, CCH_ind16, CCMT_ind16, CCPB_ind16
        integer  CCPE_ind16, CCRC_ind16, CCS_ind16, CHI2_ind16
        integer  CL01_ind16, CLST_ind16, CPED_ind16, CSQL_ind16
        integer  DC0_ind16, DC1_ind16, DCDW_ind16, DCGM_ind16
        integer  DCGW_ind16, DCH_ind16, DCMN_ind16, DCPB_ind16
        integer  DCV1_ind16, DCV2_ind16, DCV3_ind16, DDLY_ind16
        integer  DGEO_ind16, DHCL_ind16, DITM_ind16, DOCA_ind16
        integer  DPCP_ind16, DPSP_ind16, DSPC_ind16, DSPS_ind16
        integer  DSTC_ind16, DTCP_ind16, DTRK_ind16, EC01_ind16
        integer  EC1_ind16, EC1P_ind16, EC1R_ind16, ECCA_ind16
        integer  ECCL_ind16, ECCT_ind16, EC_ind16, ECDI_ind16
        integer  ECG_ind16, ECHB_ind16, ECH_ind16, ECMT_ind16
        integer  ECP1_ind16, ECPB_ind16, ECPC_ind16, ECP_ind16
        integer  ECPE_ind16, ECPI_ind16, ECPO_ind16, ECRB_ind16
        integer  ECS_ind16, EID0_ind16, EPIC_ind16, EVNT_ind16
        integer  FBPM_ind16, G1SL_ind16, G2SL_ind16, G3SL_ind16
        integer  G4SL_ind16, GPAR_ind16, GPID_ind16, GP_X_ind16
        integer  GP_Y_ind16, HBER_ind16, HBID_ind16, HBLA_ind16
        integer  HBTB_ind16, HBTR_ind16, HCAL_ind16, HDPL_ind16
        integer  HEAD_ind16, HEVT_ind16, HLS_ind16, IC_ind16
        integer  ICHB_ind16, KFIT_ind16, L1PG_ind16, L2H_ind16
        integer  L2S_ind16, LASR_ind16, LCDI_ind16, LCPB_ind16
        integer  LOWQ_ind16, MCEV_ind16, MCHD_ind16, MCTK_ind16
        integer  MCVX_ind16, MTRK_ind16, MVRT_ind16, PART_ind16
        integer  PCO_ind16, PHTM_ind16, PID1_ind16, PID2_ind16
        integer  PID3_ind16, PID4_ind16, PID5_ind16, PID6_ind16
        integer  PIDT_ind16, PRTM_ind16, PSO_ind16, PTDB_ind16
        integer  RCST_ind16, RF_ind16, RGLK_ind16, RNLG_ind16
        integer  RNPE_ind16, RTSL_ind16, RUNC_ind16, S1ST_ind16
        integer  SC1_ind16, SCC_ind16, SC_ind16, SCDI_ind16
        integer  SCGD_ind16, SCG_ind16, SCH_ind16, SCMD_ind16
        integer  SCMT_ind16, SCMW_ind16, SCPB_ind16, SCP_ind16
        integer  SCPE_ind16, SCPS_ind16, SCRC_ind16, SCR_ind16
        integer  SCS_ind16, SGMP_ind16, SPAR_ind16, SPIN_ind16
        integer  ST1_ind16, ST_ind16, STG_ind16, STH_ind16
        integer  STN0_ind16, STN1_ind16, STPB_ind16, STPE_ind16
        integer  STR_ind16, STS_ind16, STSN_ind16, SYNC_ind16
        integer  TACO_ind16, TAGE_ind16, TAGI_ind16, TAGM_ind16
        integer  TAGR_ind16, TAGT_ind16, TBER_ind16, TBID_ind16
        integer  TBLA_ind16, TBTR_ind16, TCSB_ind16, TDPL_ind16
        integer  TESC_ind16, TGBI_ind16, TGEO_ind16, TGPB_ind16
        integer  TGS_ind16, TLV1_ind16, TRCF_ind16, TRGS_ind16
        integer  TRKS_ind16, TRL1_ind16, TRPB_ind16, TSPR_ind16
        integer  TSRG_ind16, UNUS_ind16, VERT_ind16
        integer  BEAM_array, BMPR_array, BREP_array, CALB_array
        integer  CALL_array, CC01_array, CC1_array, CC_array
        integer  CCDI_array, CCH_array, CCMT_array, CCPB_array
        integer  CCPE_array, CCRC_array, CCS_array, CHI2_array
        integer  CL01_array, CLST_array, CPED_array, CSQL_array
        integer  DC0_array, DC1_array, DCDW_array, DCGM_array
        integer  DCGW_array, DCH_array, DCMN_array, DCPB_array
        integer  DCV1_array, DCV2_array, DCV3_array, DDLY_array
        integer  DGEO_array, DHCL_array, DITM_array, DOCA_array
        integer  DPCP_array, DPSP_array, DSPC_array, DSPS_array
        integer  DSTC_array, DTCP_array, DTRK_array, EC01_array
        integer  EC1_array, EC1P_array, EC1R_array, ECCA_array
        integer  ECCL_array, ECCT_array, EC_array, ECDI_array
        integer  ECG_array, ECHB_array, ECH_array, ECMT_array
        integer  ECP1_array, ECPB_array, ECPC_array, ECP_array
        integer  ECPE_array, ECPI_array, ECPO_array, ECRB_array
        integer  ECS_array, EID0_array, EPIC_array, EVNT_array
        integer  FBPM_array, G1SL_array, G2SL_array, G3SL_array
        integer  G4SL_array, GPAR_array, GPID_array, GP_X_array
        integer  GP_Y_array, HBER_array, HBID_array, HBLA_array
        integer  HBTB_array, HBTR_array, HCAL_array, HDPL_array
        integer  HEAD_array, HEVT_array, HLS_array, IC_array
        integer  ICHB_array, KFIT_array, L1PG_array, L2H_array
        integer  L2S_array, LASR_array, LCDI_array, LCPB_array
        integer  LOWQ_array, MCEV_array, MCHD_array, MCTK_array
        integer  MCVX_array, MTRK_array, MVRT_array, PART_array
        integer  PCO_array, PHTM_array, PID1_array, PID2_array
        integer  PID3_array, PID4_array, PID5_array, PID6_array
        integer  PIDT_array, PRTM_array, PSO_array, PTDB_array
        integer  RCST_array, RF_array, RGLK_array, RNLG_array
        integer  RNPE_array, RTSL_array, RUNC_array, S1ST_array
        integer  SC1_array, SCC_array, SC_array, SCDI_array
        integer  SCGD_array, SCG_array, SCH_array, SCMD_array
        integer  SCMT_array, SCMW_array, SCPB_array, SCP_array
        integer  SCPE_array, SCPS_array, SCRC_array, SCR_array
        integer  SCS_array, SGMP_array, SPAR_array, SPIN_array
        integer  ST1_array, ST_array, STG_array, STH_array
        integer  STN0_array, STN1_array, STPB_array, STPE_array
        integer  STR_array, STS_array, STSN_array, SYNC_array
        integer  TACO_array, TAGE_array, TAGI_array, TAGM_array
        integer  TAGR_array, TAGT_array, TBER_array, TBID_array
        integer  TBLA_array, TBTR_array, TCSB_array, TDPL_array
        integer  TESC_array, TGBI_array, TGEO_array, TGPB_array
        integer  TGS_array, TLV1_array, TRCF_array, TRGS_array
        integer  TRKS_array, TRL1_array, TRPB_array, TSPR_array
        integer  TSRG_array, UNUS_array, VERT_array

C define the COMMON BLOCK 


          common /clas_index/
     +        index_to_banks, index16_to_banks, which_array 

C  Now equivalance the array to the individual names
        EQUIVALENCE (INDEX_TO_BANKS(1) , BEAM_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(2) , BMPR_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(3) , BREP_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(4) , CALB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(5) , CALL_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(6) , CC01_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(7) , CC1_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(8) , CC_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(9) , CCDI_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(10) , CCH_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(11) , CCMT_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(12) , CCPB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(13) , CCPE_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(14) , CCRC_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(15) , CCS_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(16) , CHI2_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(17) , CL01_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(18) , CLST_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(19) , CPED_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(20) , CSQL_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(21) , DC0_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(22) , DC1_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(23) , DCDW_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(24) , DCGM_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(25) , DCGW_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(26) , DCH_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(27) , DCMN_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(28) , DCPB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(29) , DCV1_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(30) , DCV2_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(31) , DCV3_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(32) , DDLY_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(33) , DGEO_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(34) , DHCL_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(35) , DITM_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(36) , DOCA_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(37) , DPCP_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(38) , DPSP_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(39) , DSPC_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(40) , DSPS_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(41) , DSTC_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(42) , DTCP_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(43) , DTRK_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(44) , EC01_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(45) , EC1_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(46) , EC1P_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(47) , EC1R_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(48) , ECCA_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(49) , ECCL_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(50) , ECCT_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(51) , EC_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(52) , ECDI_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(53) , ECG_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(54) , ECHB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(55) , ECH_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(56) , ECMT_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(57) , ECP1_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(58) , ECPB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(59) , ECPC_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(60) , ECP_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(61) , ECPE_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(62) , ECPI_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(63) , ECPO_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(64) , ECRB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(65) , ECS_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(66) , EID0_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(67) , EPIC_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(68) , EVNT_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(69) , FBPM_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(70) , G1SL_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(71) , G2SL_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(72) , G3SL_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(73) , G4SL_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(74) , GPAR_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(75) , GPID_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(76) , GP_X_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(77) , GP_Y_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(78) , HBER_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(79) , HBID_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(80) , HBLA_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(81) , HBTB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(82) , HBTR_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(83) , HCAL_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(84) , HDPL_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(85) , HEAD_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(86) , HEVT_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(87) , HLS_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(88) , IC_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(89) , ICHB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(90) , KFIT_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(91) , L1PG_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(92) , L2H_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(93) , L2S_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(94) , LASR_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(95) , LCDI_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(96) , LCPB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(97) , LOWQ_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(98) , MCEV_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(99) , MCHD_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(100) , MCTK_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(101) , MCVX_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(102) , MTRK_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(103) , MVRT_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(104) , PART_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(105) , PCO_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(106) , PHTM_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(107) , PID1_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(108) , PID2_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(109) , PID3_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(110) , PID4_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(111) , PID5_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(112) , PID6_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(113) , PIDT_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(114) , PRTM_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(115) , PSO_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(116) , PTDB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(117) , RCST_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(118) , RF_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(119) , RGLK_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(120) , RNLG_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(121) , RNPE_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(122) , RTSL_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(123) , RUNC_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(124) , S1ST_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(125) , SC1_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(126) , SCC_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(127) , SC_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(128) , SCDI_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(129) , SCGD_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(130) , SCG_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(131) , SCH_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(132) , SCMD_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(133) , SCMT_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(134) , SCMW_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(135) , SCPB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(136) , SCP_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(137) , SCPE_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(138) , SCPS_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(139) , SCRC_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(140) , SCR_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(141) , SCS_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(142) , SGMP_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(143) , SPAR_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(144) , SPIN_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(145) , ST1_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(146) , ST_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(147) , STG_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(148) , STH_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(149) , STN0_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(150) , STN1_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(151) , STPB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(152) , STPE_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(153) , STR_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(154) , STS_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(155) , STSN_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(156) , SYNC_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(157) , TACO_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(158) , TAGE_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(159) , TAGI_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(160) , TAGM_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(161) , TAGR_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(162) , TAGT_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(163) , TBER_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(164) , TBID_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(165) , TBLA_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(166) , TBTR_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(167) , TCSB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(168) , TDPL_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(169) , TESC_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(170) , TGBI_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(171) , TGEO_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(172) , TGPB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(173) , TGS_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(174) , TLV1_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(175) , TRCF_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(176) , TRGS_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(177) , TRKS_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(178) , TRL1_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(179) , TRPB_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(180) , TSPR_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(181) , TSRG_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(182) , UNUS_ind) 
        EQUIVALENCE (INDEX_TO_BANKS(183) , VERT_ind) 

C  Now equivalance the array to the individual names for the 16 bit banks
        EQUIVALENCE (INDEX16_TO_BANKS(1) , BEAM_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(2) , BMPR_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(3) , BREP_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(4) , CALB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(5) , CALL_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(6) , CC01_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(7) , CC1_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(8) , CC_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(9) , CCDI_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(10) , CCH_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(11) , CCMT_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(12) , CCPB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(13) , CCPE_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(14) , CCRC_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(15) , CCS_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(16) , CHI2_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(17) , CL01_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(18) , CLST_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(19) , CPED_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(20) , CSQL_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(21) , DC0_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(22) , DC1_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(23) , DCDW_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(24) , DCGM_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(25) , DCGW_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(26) , DCH_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(27) , DCMN_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(28) , DCPB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(29) , DCV1_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(30) , DCV2_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(31) , DCV3_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(32) , DDLY_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(33) , DGEO_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(34) , DHCL_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(35) , DITM_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(36) , DOCA_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(37) , DPCP_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(38) , DPSP_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(39) , DSPC_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(40) , DSPS_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(41) , DSTC_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(42) , DTCP_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(43) , DTRK_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(44) , EC01_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(45) , EC1_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(46) , EC1P_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(47) , EC1R_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(48) , ECCA_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(49) , ECCL_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(50) , ECCT_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(51) , EC_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(52) , ECDI_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(53) , ECG_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(54) , ECHB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(55) , ECH_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(56) , ECMT_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(57) , ECP1_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(58) , ECPB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(59) , ECPC_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(60) , ECP_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(61) , ECPE_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(62) , ECPI_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(63) , ECPO_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(64) , ECRB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(65) , ECS_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(66) , EID0_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(67) , EPIC_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(68) , EVNT_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(69) , FBPM_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(70) , G1SL_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(71) , G2SL_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(72) , G3SL_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(73) , G4SL_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(74) , GPAR_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(75) , GPID_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(76) , GP_X_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(77) , GP_Y_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(78) , HBER_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(79) , HBID_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(80) , HBLA_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(81) , HBTB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(82) , HBTR_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(83) , HCAL_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(84) , HDPL_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(85) , HEAD_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(86) , HEVT_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(87) , HLS_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(88) , IC_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(89) , ICHB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(90) , KFIT_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(91) , L1PG_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(92) , L2H_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(93) , L2S_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(94) , LASR_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(95) , LCDI_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(96) , LCPB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(97) , LOWQ_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(98) , MCEV_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(99) , MCHD_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(100) , MCTK_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(101) , MCVX_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(102) , MTRK_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(103) , MVRT_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(104) , PART_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(105) , PCO_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(106) , PHTM_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(107) , PID1_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(108) , PID2_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(109) , PID3_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(110) , PID4_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(111) , PID5_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(112) , PID6_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(113) , PIDT_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(114) , PRTM_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(115) , PSO_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(116) , PTDB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(117) , RCST_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(118) , RF_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(119) , RGLK_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(120) , RNLG_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(121) , RNPE_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(122) , RTSL_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(123) , RUNC_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(124) , S1ST_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(125) , SC1_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(126) , SCC_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(127) , SC_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(128) , SCDI_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(129) , SCGD_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(130) , SCG_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(131) , SCH_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(132) , SCMD_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(133) , SCMT_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(134) , SCMW_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(135) , SCPB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(136) , SCP_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(137) , SCPE_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(138) , SCPS_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(139) , SCRC_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(140) , SCR_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(141) , SCS_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(142) , SGMP_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(143) , SPAR_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(144) , SPIN_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(145) , ST1_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(146) , ST_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(147) , STG_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(148) , STH_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(149) , STN0_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(150) , STN1_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(151) , STPB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(152) , STPE_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(153) , STR_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(154) , STS_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(155) , STSN_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(156) , SYNC_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(157) , TACO_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(158) , TAGE_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(159) , TAGI_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(160) , TAGM_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(161) , TAGR_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(162) , TAGT_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(163) , TBER_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(164) , TBID_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(165) , TBLA_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(166) , TBTR_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(167) , TCSB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(168) , TDPL_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(169) , TESC_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(170) , TGBI_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(171) , TGEO_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(172) , TGPB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(173) , TGS_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(174) , TLV1_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(175) , TRCF_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(176) , TRGS_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(177) , TRKS_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(178) , TRL1_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(179) , TRPB_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(180) , TSPR_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(181) , TSRG_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(182) , UNUS_ind16) 
        EQUIVALENCE (INDEX16_TO_BANKS(183) , VERT_ind16) 

C  Now equivalance the array which_bank which is a flag to which
     
C  IW or JW array 
        EQUIVALENCE (WHICH_ARRAY(1) , BEAM_array) 
        EQUIVALENCE (WHICH_ARRAY(2) , BMPR_array) 
        EQUIVALENCE (WHICH_ARRAY(3) , BREP_array) 
        EQUIVALENCE (WHICH_ARRAY(4) , CALB_array) 
        EQUIVALENCE (WHICH_ARRAY(5) , CALL_array) 
        EQUIVALENCE (WHICH_ARRAY(6) , CC01_array) 
        EQUIVALENCE (WHICH_ARRAY(7) , CC1_array) 
        EQUIVALENCE (WHICH_ARRAY(8) , CC_array) 
        EQUIVALENCE (WHICH_ARRAY(9) , CCDI_array) 
        EQUIVALENCE (WHICH_ARRAY(10) , CCH_array) 
        EQUIVALENCE (WHICH_ARRAY(11) , CCMT_array) 
        EQUIVALENCE (WHICH_ARRAY(12) , CCPB_array) 
        EQUIVALENCE (WHICH_ARRAY(13) , CCPE_array) 
        EQUIVALENCE (WHICH_ARRAY(14) , CCRC_array) 
        EQUIVALENCE (WHICH_ARRAY(15) , CCS_array) 
        EQUIVALENCE (WHICH_ARRAY(16) , CHI2_array) 
        EQUIVALENCE (WHICH_ARRAY(17) , CL01_array) 
        EQUIVALENCE (WHICH_ARRAY(18) , CLST_array) 
        EQUIVALENCE (WHICH_ARRAY(19) , CPED_array) 
        EQUIVALENCE (WHICH_ARRAY(20) , CSQL_array) 
        EQUIVALENCE (WHICH_ARRAY(21) , DC0_array) 
        EQUIVALENCE (WHICH_ARRAY(22) , DC1_array) 
        EQUIVALENCE (WHICH_ARRAY(23) , DCDW_array) 
        EQUIVALENCE (WHICH_ARRAY(24) , DCGM_array) 
        EQUIVALENCE (WHICH_ARRAY(25) , DCGW_array) 
        EQUIVALENCE (WHICH_ARRAY(26) , DCH_array) 
        EQUIVALENCE (WHICH_ARRAY(27) , DCMN_array) 
        EQUIVALENCE (WHICH_ARRAY(28) , DCPB_array) 
        EQUIVALENCE (WHICH_ARRAY(29) , DCV1_array) 
        EQUIVALENCE (WHICH_ARRAY(30) , DCV2_array) 
        EQUIVALENCE (WHICH_ARRAY(31) , DCV3_array) 
        EQUIVALENCE (WHICH_ARRAY(32) , DDLY_array) 
        EQUIVALENCE (WHICH_ARRAY(33) , DGEO_array) 
        EQUIVALENCE (WHICH_ARRAY(34) , DHCL_array) 
        EQUIVALENCE (WHICH_ARRAY(35) , DITM_array) 
        EQUIVALENCE (WHICH_ARRAY(36) , DOCA_array) 
        EQUIVALENCE (WHICH_ARRAY(37) , DPCP_array) 
        EQUIVALENCE (WHICH_ARRAY(38) , DPSP_array) 
        EQUIVALENCE (WHICH_ARRAY(39) , DSPC_array) 
        EQUIVALENCE (WHICH_ARRAY(40) , DSPS_array) 
        EQUIVALENCE (WHICH_ARRAY(41) , DSTC_array) 
        EQUIVALENCE (WHICH_ARRAY(42) , DTCP_array) 
        EQUIVALENCE (WHICH_ARRAY(43) , DTRK_array) 
        EQUIVALENCE (WHICH_ARRAY(44) , EC01_array) 
        EQUIVALENCE (WHICH_ARRAY(45) , EC1_array) 
        EQUIVALENCE (WHICH_ARRAY(46) , EC1P_array) 
        EQUIVALENCE (WHICH_ARRAY(47) , EC1R_array) 
        EQUIVALENCE (WHICH_ARRAY(48) , ECCA_array) 
        EQUIVALENCE (WHICH_ARRAY(49) , ECCL_array) 
        EQUIVALENCE (WHICH_ARRAY(50) , ECCT_array) 
        EQUIVALENCE (WHICH_ARRAY(51) , EC_array) 
        EQUIVALENCE (WHICH_ARRAY(52) , ECDI_array) 
        EQUIVALENCE (WHICH_ARRAY(53) , ECG_array) 
        EQUIVALENCE (WHICH_ARRAY(54) , ECHB_array) 
        EQUIVALENCE (WHICH_ARRAY(55) , ECH_array) 
        EQUIVALENCE (WHICH_ARRAY(56) , ECMT_array) 
        EQUIVALENCE (WHICH_ARRAY(57) , ECP1_array) 
        EQUIVALENCE (WHICH_ARRAY(58) , ECPB_array) 
        EQUIVALENCE (WHICH_ARRAY(59) , ECPC_array) 
        EQUIVALENCE (WHICH_ARRAY(60) , ECP_array) 
        EQUIVALENCE (WHICH_ARRAY(61) , ECPE_array) 
        EQUIVALENCE (WHICH_ARRAY(62) , ECPI_array) 
        EQUIVALENCE (WHICH_ARRAY(63) , ECPO_array) 
        EQUIVALENCE (WHICH_ARRAY(64) , ECRB_array) 
        EQUIVALENCE (WHICH_ARRAY(65) , ECS_array) 
        EQUIVALENCE (WHICH_ARRAY(66) , EID0_array) 
        EQUIVALENCE (WHICH_ARRAY(67) , EPIC_array) 
        EQUIVALENCE (WHICH_ARRAY(68) , EVNT_array) 
        EQUIVALENCE (WHICH_ARRAY(69) , FBPM_array) 
        EQUIVALENCE (WHICH_ARRAY(70) , G1SL_array) 
        EQUIVALENCE (WHICH_ARRAY(71) , G2SL_array) 
        EQUIVALENCE (WHICH_ARRAY(72) , G3SL_array) 
        EQUIVALENCE (WHICH_ARRAY(73) , G4SL_array) 
        EQUIVALENCE (WHICH_ARRAY(74) , GPAR_array) 
        EQUIVALENCE (WHICH_ARRAY(75) , GPID_array) 
        EQUIVALENCE (WHICH_ARRAY(76) , GP_X_array) 
        EQUIVALENCE (WHICH_ARRAY(77) , GP_Y_array) 
        EQUIVALENCE (WHICH_ARRAY(78) , HBER_array) 
        EQUIVALENCE (WHICH_ARRAY(79) , HBID_array) 
        EQUIVALENCE (WHICH_ARRAY(80) , HBLA_array) 
        EQUIVALENCE (WHICH_ARRAY(81) , HBTB_array) 
        EQUIVALENCE (WHICH_ARRAY(82) , HBTR_array) 
        EQUIVALENCE (WHICH_ARRAY(83) , HCAL_array) 
        EQUIVALENCE (WHICH_ARRAY(84) , HDPL_array) 
        EQUIVALENCE (WHICH_ARRAY(85) , HEAD_array) 
        EQUIVALENCE (WHICH_ARRAY(86) , HEVT_array) 
        EQUIVALENCE (WHICH_ARRAY(87) , HLS_array) 
        EQUIVALENCE (WHICH_ARRAY(88) , IC_array) 
        EQUIVALENCE (WHICH_ARRAY(89) , ICHB_array) 
        EQUIVALENCE (WHICH_ARRAY(90) , KFIT_array) 
        EQUIVALENCE (WHICH_ARRAY(91) , L1PG_array) 
        EQUIVALENCE (WHICH_ARRAY(92) , L2H_array) 
        EQUIVALENCE (WHICH_ARRAY(93) , L2S_array) 
        EQUIVALENCE (WHICH_ARRAY(94) , LASR_array) 
        EQUIVALENCE (WHICH_ARRAY(95) , LCDI_array) 
        EQUIVALENCE (WHICH_ARRAY(96) , LCPB_array) 
        EQUIVALENCE (WHICH_ARRAY(97) , LOWQ_array) 
        EQUIVALENCE (WHICH_ARRAY(98) , MCEV_array) 
        EQUIVALENCE (WHICH_ARRAY(99) , MCHD_array) 
        EQUIVALENCE (WHICH_ARRAY(100) , MCTK_array) 
        EQUIVALENCE (WHICH_ARRAY(101) , MCVX_array) 
        EQUIVALENCE (WHICH_ARRAY(102) , MTRK_array) 
        EQUIVALENCE (WHICH_ARRAY(103) , MVRT_array) 
        EQUIVALENCE (WHICH_ARRAY(104) , PART_array) 
        EQUIVALENCE (WHICH_ARRAY(105) , PCO_array) 
        EQUIVALENCE (WHICH_ARRAY(106) , PHTM_array) 
        EQUIVALENCE (WHICH_ARRAY(107) , PID1_array) 
        EQUIVALENCE (WHICH_ARRAY(108) , PID2_array) 
        EQUIVALENCE (WHICH_ARRAY(109) , PID3_array) 
        EQUIVALENCE (WHICH_ARRAY(110) , PID4_array) 
        EQUIVALENCE (WHICH_ARRAY(111) , PID5_array) 
        EQUIVALENCE (WHICH_ARRAY(112) , PID6_array) 
        EQUIVALENCE (WHICH_ARRAY(113) , PIDT_array) 
        EQUIVALENCE (WHICH_ARRAY(114) , PRTM_array) 
        EQUIVALENCE (WHICH_ARRAY(115) , PSO_array) 
        EQUIVALENCE (WHICH_ARRAY(116) , PTDB_array) 
        EQUIVALENCE (WHICH_ARRAY(117) , RCST_array) 
        EQUIVALENCE (WHICH_ARRAY(118) , RF_array) 
        EQUIVALENCE (WHICH_ARRAY(119) , RGLK_array) 
        EQUIVALENCE (WHICH_ARRAY(120) , RNLG_array) 
        EQUIVALENCE (WHICH_ARRAY(121) , RNPE_array) 
        EQUIVALENCE (WHICH_ARRAY(122) , RTSL_array) 
        EQUIVALENCE (WHICH_ARRAY(123) , RUNC_array) 
        EQUIVALENCE (WHICH_ARRAY(124) , S1ST_array) 
        EQUIVALENCE (WHICH_ARRAY(125) , SC1_array) 
        EQUIVALENCE (WHICH_ARRAY(126) , SCC_array) 
        EQUIVALENCE (WHICH_ARRAY(127) , SC_array) 
        EQUIVALENCE (WHICH_ARRAY(128) , SCDI_array) 
        EQUIVALENCE (WHICH_ARRAY(129) , SCGD_array) 
        EQUIVALENCE (WHICH_ARRAY(130) , SCG_array) 
        EQUIVALENCE (WHICH_ARRAY(131) , SCH_array) 
        EQUIVALENCE (WHICH_ARRAY(132) , SCMD_array) 
        EQUIVALENCE (WHICH_ARRAY(133) , SCMT_array) 
        EQUIVALENCE (WHICH_ARRAY(134) , SCMW_array) 
        EQUIVALENCE (WHICH_ARRAY(135) , SCPB_array) 
        EQUIVALENCE (WHICH_ARRAY(136) , SCP_array) 
        EQUIVALENCE (WHICH_ARRAY(137) , SCPE_array) 
        EQUIVALENCE (WHICH_ARRAY(138) , SCPS_array) 
        EQUIVALENCE (WHICH_ARRAY(139) , SCRC_array) 
        EQUIVALENCE (WHICH_ARRAY(140) , SCR_array) 
        EQUIVALENCE (WHICH_ARRAY(141) , SCS_array) 
        EQUIVALENCE (WHICH_ARRAY(142) , SGMP_array) 
        EQUIVALENCE (WHICH_ARRAY(143) , SPAR_array) 
        EQUIVALENCE (WHICH_ARRAY(144) , SPIN_array) 
        EQUIVALENCE (WHICH_ARRAY(145) , ST1_array) 
        EQUIVALENCE (WHICH_ARRAY(146) , ST_array) 
        EQUIVALENCE (WHICH_ARRAY(147) , STG_array) 
        EQUIVALENCE (WHICH_ARRAY(148) , STH_array) 
        EQUIVALENCE (WHICH_ARRAY(149) , STN0_array) 
        EQUIVALENCE (WHICH_ARRAY(150) , STN1_array) 
        EQUIVALENCE (WHICH_ARRAY(151) , STPB_array) 
        EQUIVALENCE (WHICH_ARRAY(152) , STPE_array) 
        EQUIVALENCE (WHICH_ARRAY(153) , STR_array) 
        EQUIVALENCE (WHICH_ARRAY(154) , STS_array) 
        EQUIVALENCE (WHICH_ARRAY(155) , STSN_array) 
        EQUIVALENCE (WHICH_ARRAY(156) , SYNC_array) 
        EQUIVALENCE (WHICH_ARRAY(157) , TACO_array) 
        EQUIVALENCE (WHICH_ARRAY(158) , TAGE_array) 
        EQUIVALENCE (WHICH_ARRAY(159) , TAGI_array) 
        EQUIVALENCE (WHICH_ARRAY(160) , TAGM_array) 
        EQUIVALENCE (WHICH_ARRAY(161) , TAGR_array) 
        EQUIVALENCE (WHICH_ARRAY(162) , TAGT_array) 
        EQUIVALENCE (WHICH_ARRAY(163) , TBER_array) 
        EQUIVALENCE (WHICH_ARRAY(164) , TBID_array) 
        EQUIVALENCE (WHICH_ARRAY(165) , TBLA_array) 
        EQUIVALENCE (WHICH_ARRAY(166) , TBTR_array) 
        EQUIVALENCE (WHICH_ARRAY(167) , TCSB_array) 
        EQUIVALENCE (WHICH_ARRAY(168) , TDPL_array) 
        EQUIVALENCE (WHICH_ARRAY(169) , TESC_array) 
        EQUIVALENCE (WHICH_ARRAY(170) , TGBI_array) 
        EQUIVALENCE (WHICH_ARRAY(171) , TGEO_array) 
        EQUIVALENCE (WHICH_ARRAY(172) , TGPB_array) 
        EQUIVALENCE (WHICH_ARRAY(173) , TGS_array) 
        EQUIVALENCE (WHICH_ARRAY(174) , TLV1_array) 
        EQUIVALENCE (WHICH_ARRAY(175) , TRCF_array) 
        EQUIVALENCE (WHICH_ARRAY(176) , TRGS_array) 
        EQUIVALENCE (WHICH_ARRAY(177) , TRKS_array) 
        EQUIVALENCE (WHICH_ARRAY(178) , TRL1_array) 
        EQUIVALENCE (WHICH_ARRAY(179) , TRPB_array) 
        EQUIVALENCE (WHICH_ARRAY(180) , TSPR_array) 
        EQUIVALENCE (WHICH_ARRAY(181) , TSRG_array) 
        EQUIVALENCE (WHICH_ARRAY(182) , UNUS_array) 
        EQUIVALENCE (WHICH_ARRAY(183) , VERT_array) 

C  Define the DDL bank names 
         data  DDL_BANK_NAMES(1)/'BEAM'/ 
         data  DDL_BANK_NAMES(2)/'BMPR'/ 
         data  DDL_BANK_NAMES(3)/'BREP'/ 
         data  DDL_BANK_NAMES(4)/'CALB'/ 
         data  DDL_BANK_NAMES(5)/'CALL'/ 
         data  DDL_BANK_NAMES(6)/'CC01'/ 
         data  DDL_BANK_NAMES(7)/'CC1 '/ 
         data  DDL_BANK_NAMES(8)/'CC  '/ 
         data  DDL_BANK_NAMES(9)/'CCDI'/ 
         data  DDL_BANK_NAMES(10)/'CCH '/ 
         data  DDL_BANK_NAMES(11)/'CCMT'/ 
         data  DDL_BANK_NAMES(12)/'CCPB'/ 
         data  DDL_BANK_NAMES(13)/'CCPE'/ 
         data  DDL_BANK_NAMES(14)/'CCRC'/ 
         data  DDL_BANK_NAMES(15)/'CCS '/ 
         data  DDL_BANK_NAMES(16)/'CHI2'/ 
         data  DDL_BANK_NAMES(17)/'CL01'/ 
         data  DDL_BANK_NAMES(18)/'CLST'/ 
         data  DDL_BANK_NAMES(19)/'CPED'/ 
         data  DDL_BANK_NAMES(20)/'CSQL'/ 
         data  DDL_BANK_NAMES(21)/'DC0 '/ 
         data  DDL_BANK_NAMES(22)/'DC1 '/ 
         data  DDL_BANK_NAMES(23)/'DCDW'/ 
         data  DDL_BANK_NAMES(24)/'DCGM'/ 
         data  DDL_BANK_NAMES(25)/'DCGW'/ 
         data  DDL_BANK_NAMES(26)/'DCH '/ 
         data  DDL_BANK_NAMES(27)/'DCMN'/ 
         data  DDL_BANK_NAMES(28)/'DCPB'/ 
         data  DDL_BANK_NAMES(29)/'DCV1'/ 
         data  DDL_BANK_NAMES(30)/'DCV2'/ 
         data  DDL_BANK_NAMES(31)/'DCV3'/ 
         data  DDL_BANK_NAMES(32)/'DDLY'/ 
         data  DDL_BANK_NAMES(33)/'DGEO'/ 
         data  DDL_BANK_NAMES(34)/'DHCL'/ 
         data  DDL_BANK_NAMES(35)/'DITM'/ 
         data  DDL_BANK_NAMES(36)/'DOCA'/ 
         data  DDL_BANK_NAMES(37)/'DPCP'/ 
         data  DDL_BANK_NAMES(38)/'DPSP'/ 
         data  DDL_BANK_NAMES(39)/'DSPC'/ 
         data  DDL_BANK_NAMES(40)/'DSPS'/ 
         data  DDL_BANK_NAMES(41)/'DSTC'/ 
         data  DDL_BANK_NAMES(42)/'DTCP'/ 
         data  DDL_BANK_NAMES(43)/'DTRK'/ 
         data  DDL_BANK_NAMES(44)/'EC01'/ 
         data  DDL_BANK_NAMES(45)/'EC1 '/ 
         data  DDL_BANK_NAMES(46)/'EC1P'/ 
         data  DDL_BANK_NAMES(47)/'EC1R'/ 
         data  DDL_BANK_NAMES(48)/'ECCA'/ 
         data  DDL_BANK_NAMES(49)/'ECCL'/ 
         data  DDL_BANK_NAMES(50)/'ECCT'/ 
         data  DDL_BANK_NAMES(51)/'EC  '/ 
         data  DDL_BANK_NAMES(52)/'ECDI'/ 
         data  DDL_BANK_NAMES(53)/'ECG '/ 
         data  DDL_BANK_NAMES(54)/'ECHB'/ 
         data  DDL_BANK_NAMES(55)/'ECH '/ 
         data  DDL_BANK_NAMES(56)/'ECMT'/ 
         data  DDL_BANK_NAMES(57)/'ECP1'/ 
         data  DDL_BANK_NAMES(58)/'ECPB'/ 
         data  DDL_BANK_NAMES(59)/'ECPC'/ 
         data  DDL_BANK_NAMES(60)/'ECP '/ 
         data  DDL_BANK_NAMES(61)/'ECPE'/ 
         data  DDL_BANK_NAMES(62)/'ECPI'/ 
         data  DDL_BANK_NAMES(63)/'ECPO'/ 
         data  DDL_BANK_NAMES(64)/'ECRB'/ 
         data  DDL_BANK_NAMES(65)/'ECS '/ 
         data  DDL_BANK_NAMES(66)/'EID0'/ 
         data  DDL_BANK_NAMES(67)/'EPIC'/ 
         data  DDL_BANK_NAMES(68)/'EVNT'/ 
         data  DDL_BANK_NAMES(69)/'FBPM'/ 
         data  DDL_BANK_NAMES(70)/'G1SL'/ 
         data  DDL_BANK_NAMES(71)/'G2SL'/ 
         data  DDL_BANK_NAMES(72)/'G3SL'/ 
         data  DDL_BANK_NAMES(73)/'G4SL'/ 
         data  DDL_BANK_NAMES(74)/'GPAR'/ 
         data  DDL_BANK_NAMES(75)/'GPID'/ 
         data  DDL_BANK_NAMES(76)/'GP_X'/ 
         data  DDL_BANK_NAMES(77)/'GP_Y'/ 
         data  DDL_BANK_NAMES(78)/'HBER'/ 
         data  DDL_BANK_NAMES(79)/'HBID'/ 
         data  DDL_BANK_NAMES(80)/'HBLA'/ 
         data  DDL_BANK_NAMES(81)/'HBTB'/ 
         data  DDL_BANK_NAMES(82)/'HBTR'/ 
         data  DDL_BANK_NAMES(83)/'HCAL'/ 
         data  DDL_BANK_NAMES(84)/'HDPL'/ 
         data  DDL_BANK_NAMES(85)/'HEAD'/ 
         data  DDL_BANK_NAMES(86)/'HEVT'/ 
         data  DDL_BANK_NAMES(87)/'HLS '/ 
         data  DDL_BANK_NAMES(88)/'IC  '/ 
         data  DDL_BANK_NAMES(89)/'ICHB'/ 
         data  DDL_BANK_NAMES(90)/'KFIT'/ 
         data  DDL_BANK_NAMES(91)/'L1PG'/ 
         data  DDL_BANK_NAMES(92)/'L2H '/ 
         data  DDL_BANK_NAMES(93)/'L2S '/ 
         data  DDL_BANK_NAMES(94)/'LASR'/ 
         data  DDL_BANK_NAMES(95)/'LCDI'/ 
         data  DDL_BANK_NAMES(96)/'LCPB'/ 
         data  DDL_BANK_NAMES(97)/'LOWQ'/ 
         data  DDL_BANK_NAMES(98)/'MCEV'/ 
         data  DDL_BANK_NAMES(99)/'MCHD'/ 
         data  DDL_BANK_NAMES(100)/'MCTK'/ 
         data  DDL_BANK_NAMES(101)/'MCVX'/ 
         data  DDL_BANK_NAMES(102)/'MTRK'/ 
         data  DDL_BANK_NAMES(103)/'MVRT'/ 
         data  DDL_BANK_NAMES(104)/'PART'/ 
         data  DDL_BANK_NAMES(105)/'PCO '/ 
         data  DDL_BANK_NAMES(106)/'PHTM'/ 
         data  DDL_BANK_NAMES(107)/'PID1'/ 
         data  DDL_BANK_NAMES(108)/'PID2'/ 
         data  DDL_BANK_NAMES(109)/'PID3'/ 
         data  DDL_BANK_NAMES(110)/'PID4'/ 
         data  DDL_BANK_NAMES(111)/'PID5'/ 
         data  DDL_BANK_NAMES(112)/'PID6'/ 
         data  DDL_BANK_NAMES(113)/'PIDT'/ 
         data  DDL_BANK_NAMES(114)/'PRTM'/ 
         data  DDL_BANK_NAMES(115)/'PSO '/ 
         data  DDL_BANK_NAMES(116)/'PTDB'/ 
         data  DDL_BANK_NAMES(117)/'RCST'/ 
         data  DDL_BANK_NAMES(118)/'RF  '/ 
         data  DDL_BANK_NAMES(119)/'RGLK'/ 
         data  DDL_BANK_NAMES(120)/'RNLG'/ 
         data  DDL_BANK_NAMES(121)/'RNPE'/ 
         data  DDL_BANK_NAMES(122)/'RTSL'/ 
         data  DDL_BANK_NAMES(123)/'RUNC'/ 
         data  DDL_BANK_NAMES(124)/'S1ST'/ 
         data  DDL_BANK_NAMES(125)/'SC1 '/ 
         data  DDL_BANK_NAMES(126)/'SCC '/ 
         data  DDL_BANK_NAMES(127)/'SC  '/ 
         data  DDL_BANK_NAMES(128)/'SCDI'/ 
         data  DDL_BANK_NAMES(129)/'SCGD'/ 
         data  DDL_BANK_NAMES(130)/'SCG '/ 
         data  DDL_BANK_NAMES(131)/'SCH '/ 
         data  DDL_BANK_NAMES(132)/'SCMD'/ 
         data  DDL_BANK_NAMES(133)/'SCMT'/ 
         data  DDL_BANK_NAMES(134)/'SCMW'/ 
         data  DDL_BANK_NAMES(135)/'SCPB'/ 
         data  DDL_BANK_NAMES(136)/'SCP '/ 
         data  DDL_BANK_NAMES(137)/'SCPE'/ 
         data  DDL_BANK_NAMES(138)/'SCPS'/ 
         data  DDL_BANK_NAMES(139)/'SCRC'/ 
         data  DDL_BANK_NAMES(140)/'SCR '/ 
         data  DDL_BANK_NAMES(141)/'SCS '/ 
         data  DDL_BANK_NAMES(142)/'SGMP'/ 
         data  DDL_BANK_NAMES(143)/'SPAR'/ 
         data  DDL_BANK_NAMES(144)/'SPIN'/ 
         data  DDL_BANK_NAMES(145)/'ST1 '/ 
         data  DDL_BANK_NAMES(146)/'ST  '/ 
         data  DDL_BANK_NAMES(147)/'STG '/ 
         data  DDL_BANK_NAMES(148)/'STH '/ 
         data  DDL_BANK_NAMES(149)/'STN0'/ 
         data  DDL_BANK_NAMES(150)/'STN1'/ 
         data  DDL_BANK_NAMES(151)/'STPB'/ 
         data  DDL_BANK_NAMES(152)/'STPE'/ 
         data  DDL_BANK_NAMES(153)/'STR '/ 
         data  DDL_BANK_NAMES(154)/'STS '/ 
         data  DDL_BANK_NAMES(155)/'STSN'/ 
         data  DDL_BANK_NAMES(156)/'SYNC'/ 
         data  DDL_BANK_NAMES(157)/'TACO'/ 
         data  DDL_BANK_NAMES(158)/'TAGE'/ 
         data  DDL_BANK_NAMES(159)/'TAGI'/ 
         data  DDL_BANK_NAMES(160)/'TAGM'/ 
         data  DDL_BANK_NAMES(161)/'TAGR'/ 
         data  DDL_BANK_NAMES(162)/'TAGT'/ 
         data  DDL_BANK_NAMES(163)/'TBER'/ 
         data  DDL_BANK_NAMES(164)/'TBID'/ 
         data  DDL_BANK_NAMES(165)/'TBLA'/ 
         data  DDL_BANK_NAMES(166)/'TBTR'/ 
         data  DDL_BANK_NAMES(167)/'TCSB'/ 
         data  DDL_BANK_NAMES(168)/'TDPL'/ 
         data  DDL_BANK_NAMES(169)/'TESC'/ 
         data  DDL_BANK_NAMES(170)/'TGBI'/ 
         data  DDL_BANK_NAMES(171)/'TGEO'/ 
         data  DDL_BANK_NAMES(172)/'TGPB'/ 
         data  DDL_BANK_NAMES(173)/'TGS '/ 
         data  DDL_BANK_NAMES(174)/'TLV1'/ 
         data  DDL_BANK_NAMES(175)/'TRCF'/ 
         data  DDL_BANK_NAMES(176)/'TRGS'/ 
         data  DDL_BANK_NAMES(177)/'TRKS'/ 
         data  DDL_BANK_NAMES(178)/'TRL1'/ 
         data  DDL_BANK_NAMES(179)/'TRPB'/ 
         data  DDL_BANK_NAMES(180)/'TSPR'/ 
         data  DDL_BANK_NAMES(181)/'TSRG'/ 
         data  DDL_BANK_NAMES(182)/'UNUS'/ 
         data  DDL_BANK_NAMES(183)/'VERT'/ 
