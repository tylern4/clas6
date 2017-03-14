#ifndef __CALDB_TABLE_H__
#define __CALDB_TABLE_H__

/// defining caldb items as constants ================================
const int CALL_CALIB_RFoffset_rf2rf1Offset = 0;
const int call_RFoffset_rf2rf1Offset = 0;
const int CALL_CALIB_T0_unc = 1;
const int call_T0_unc = 1;
const int CALL_CALIB_T0_value = 2;
const int call_T0_value = 2;
const int CALL_CALIB_T1_unc = 3;
const int call_T1_unc = 3;
const int CALL_CALIB_T1_value = 4;
const int call_T1_value = 4;
const int CALL_CALIB_T2_unc = 5;
const int call_T2_unc = 5;
const int CALL_CALIB_T2_value = 6;
const int call_T2_value = 6;
const int CALL_CALIB_pedestals_unc = 7;
const int call_pedestals_unc = 7;
const int CALL_CALIB_pedestals_value = 8;
const int call_pedestals_value = 8;
const int CC_CALIB_HV_value = 9;
const int cc_HV_value = 9;
const int CC_CALIB_TDC_T0 = 10;
const int cc_TDC_T0 = 10;
const int CC_CALIB_TDC_T1 = 11;
const int cc_TDC_T1 = 11;
const int CC_CALIB_pedestals_mean = 12;
const int cc_pedestals_mean = 12;
const int CC_CALIB_pedestals_sigma = 13;
const int cc_pedestals_sigma = 13;
const int CC_CALIB_photoE_amplitude = 14;
const int cc_photoE_amplitude = 14;
const int DC_DOCA_RMS_Region1 = 15;
const int dcd_RMS_Region1 = 15;
const int DC_DOCA_RMS_Region2 = 16;
const int dcd_RMS_Region2 = 16;
const int DC_DOCA_RMS_Region3 = 17;
const int dcd_RMS_Region3 = 17;
const int DC_DOCA_Sigma_Region1 = 18;
const int dcd_Sigma_Region1 = 18;
const int DC_DOCA_Sigma_Region2 = 19;
const int dcd_Sigma_Region2 = 19;
const int DC_DOCA_Sigma_Region3 = 20;
const int dcd_Sigma_Region3 = 20;
const int DC_DOCA_Timewalk_Region1 = 21;
const int dcd_Timewalk_Region1 = 21;
const int DC_DOCA_Timewalk_Region2 = 22;
const int dcd_Timewalk_Region2 = 22;
const int DC_DOCA_Timewalk_Region3 = 23;
const int dcd_Timewalk_Region3 = 23;
const int DC_DOCA_Timewalk_betaslope = 24;
const int dcd_Timewalk_betaslope = 24;
const int DC_DOCA_Timewalk_comment = 25;
const int dcd_Timewalk_comment = 25;
const int DC_DOCA_Timewalk_factor = 26;
const int dcd_Timewalk_factor = 26;
const int DC_DOCA_Timewalk_functiontype = 27;
const int dcd_Timewalk_functiontype = 27;
const int DC_DOCA_Timewalk_tau = 28;
const int dcd_Timewalk_tau = 28;
const int DC_DOCA_t_max_Sector1 = 29;
const int dcd_t_max_Sector1 = 29;
const int DC_DOCA_t_max_Sector2 = 30;
const int dcd_t_max_Sector2 = 30;
const int DC_DOCA_t_max_Sector3 = 31;
const int dcd_t_max_Sector3 = 31;
const int DC_DOCA_t_max_Sector4 = 32;
const int dcd_t_max_Sector4 = 32;
const int DC_DOCA_t_max_Sector5 = 33;
const int dcd_t_max_Sector5 = 33;
const int DC_DOCA_t_max_Sector6 = 34;
const int dcd_t_max_Sector6 = 34;
const int DC_DOCA_t_max_comment = 35;
const int dcd_t_max_comment = 35;
const int DC_DOCA_xvst_par_Sect2_SL1 = 36;
const int dcd_xvst_par_Sect2_SL1 = 36;
const int DC_DOCA_xvst_par_Sect2_SL2 = 37;
const int dcd_xvst_par_Sect2_SL2 = 37;
const int DC_DOCA_xvst_par_Sect2_SL3 = 38;
const int dcd_xvst_par_Sect2_SL3 = 38;
const int DC_DOCA_xvst_par_Sect2_SL4 = 39;
const int dcd_xvst_par_Sect2_SL4 = 39;
const int DC_DOCA_xvst_par_Sect2_SL5 = 40;
const int dcd_xvst_par_Sect2_SL5 = 40;
const int DC_DOCA_xvst_par_Sect2_SL6 = 41;
const int dcd_xvst_par_Sect2_SL6 = 41;
const int DC_DOCA_xvst_par_Sect2_comment = 42;
const int dcd_xvst_par_Sect2_comment = 42;
const int DC_DOCA_xvst_par_Sect3_SL1 = 43;
const int dcd_xvst_par_Sect3_SL1 = 43;
const int DC_DOCA_xvst_par_Sect3_SL2 = 44;
const int dcd_xvst_par_Sect3_SL2 = 44;
const int DC_DOCA_xvst_par_Sect3_SL3 = 45;
const int dcd_xvst_par_Sect3_SL3 = 45;
const int DC_DOCA_xvst_par_Sect3_SL4 = 46;
const int dcd_xvst_par_Sect3_SL4 = 46;
const int DC_DOCA_xvst_par_Sect3_SL5 = 47;
const int dcd_xvst_par_Sect3_SL5 = 47;
const int DC_DOCA_xvst_par_Sect3_SL6 = 48;
const int dcd_xvst_par_Sect3_SL6 = 48;
const int DC_DOCA_xvst_par_Sect3_comment = 49;
const int dcd_xvst_par_Sect3_comment = 49;
const int DC_DOCA_xvst_par_Sect4_SL1 = 50;
const int dcd_xvst_par_Sect4_SL1 = 50;
const int DC_DOCA_xvst_par_Sect4_SL2 = 51;
const int dcd_xvst_par_Sect4_SL2 = 51;
const int DC_DOCA_xvst_par_Sect4_SL3 = 52;
const int dcd_xvst_par_Sect4_SL3 = 52;
const int DC_DOCA_xvst_par_Sect4_SL4 = 53;
const int dcd_xvst_par_Sect4_SL4 = 53;
const int DC_DOCA_xvst_par_Sect4_SL5 = 54;
const int dcd_xvst_par_Sect4_SL5 = 54;
const int DC_DOCA_xvst_par_Sect4_SL6 = 55;
const int dcd_xvst_par_Sect4_SL6 = 55;
const int DC_DOCA_xvst_par_Sect4_comment = 56;
const int dcd_xvst_par_Sect4_comment = 56;
const int DC_DOCA_xvst_par_Sect5_SL1 = 57;
const int dcd_xvst_par_Sect5_SL1 = 57;
const int DC_DOCA_xvst_par_Sect5_SL2 = 58;
const int dcd_xvst_par_Sect5_SL2 = 58;
const int DC_DOCA_xvst_par_Sect5_SL3 = 59;
const int dcd_xvst_par_Sect5_SL3 = 59;
const int DC_DOCA_xvst_par_Sect5_SL4 = 60;
const int dcd_xvst_par_Sect5_SL4 = 60;
const int DC_DOCA_xvst_par_Sect5_SL5 = 61;
const int dcd_xvst_par_Sect5_SL5 = 61;
const int DC_DOCA_xvst_par_Sect5_SL6 = 62;
const int dcd_xvst_par_Sect5_SL6 = 62;
const int DC_DOCA_xvst_par_Sect5_comment = 63;
const int dcd_xvst_par_Sect5_comment = 63;
const int DC_DOCA_xvst_par_Sect6_SL1 = 64;
const int dcd_xvst_par_Sect6_SL1 = 64;
const int DC_DOCA_xvst_par_Sect6_SL2 = 65;
const int dcd_xvst_par_Sect6_SL2 = 65;
const int DC_DOCA_xvst_par_Sect6_SL3 = 66;
const int dcd_xvst_par_Sect6_SL3 = 66;
const int DC_DOCA_xvst_par_Sect6_SL4 = 67;
const int dcd_xvst_par_Sect6_SL4 = 67;
const int DC_DOCA_xvst_par_Sect6_SL5 = 68;
const int dcd_xvst_par_Sect6_SL5 = 68;
const int DC_DOCA_xvst_par_Sect6_SL6 = 69;
const int dcd_xvst_par_Sect6_SL6 = 69;
const int DC_DOCA_xvst_par_Sect6_comment = 70;
const int dcd_xvst_par_Sect6_comment = 70;
const int DC_DOCA_xvst_params_SL1 = 71;
const int dcd_xvst_params_SL1 = 71;
const int DC_DOCA_xvst_params_SL2 = 72;
const int dcd_xvst_params_SL2 = 72;
const int DC_DOCA_xvst_params_SL3 = 73;
const int dcd_xvst_params_SL3 = 73;
const int DC_DOCA_xvst_params_SL4 = 74;
const int dcd_xvst_params_SL4 = 74;
const int DC_DOCA_xvst_params_SL5 = 75;
const int dcd_xvst_params_SL5 = 75;
const int DC_DOCA_xvst_params_SL6 = 76;
const int dcd_xvst_params_SL6 = 76;
const int DC_DOCA_xvst_params_comment = 77;
const int dcd_xvst_params_comment = 77;
const int RF_OFFSETS_correction_value = 78;
const int rf_correction_value = 78;
const int beam_fcup_scale = 79;
const int TAG_CALIB_tag_t_dt_multihit = 80;
const int tag_tag_t_dt_multihit = 80;
const int TAG_CALIB_tag_e_e2t = 81;
const int tag_tag_e_e2t = 81;
const int LAC_TIMING_sector1_d_l = 82;
const int lact_sector1_d_l = 82;
const int LAC_TIMING_sector1_d_r = 83;
const int lact_sector1_d_r = 83;
const int GPP_DC_Reg1_mean = 84;
const int gpp_DC_Reg1_mean = 84;
const int GPP_DC_Reg2_mean = 85;
const int gpp_DC_Reg2_mean = 85;
const int GPP_DC_Reg3_mean = 86;
const int gpp_DC_Reg3_mean = 86;
const int DC_GEOM_dc_xalign = 87;
const int dcg_dc_xalign = 87;
const int DC_GEOM_dc_xrot = 88;
const int dcg_dc_xrot = 88;
const int DC_GEOM_dc_yalign = 89;
const int dcg_dc_yalign = 89;
const int DC_GEOM_dc_yrot = 90;
const int dcg_dc_yrot = 90;
const int DC_GEOM_dc_zalign = 91;
const int dcg_dc_zalign = 91;
const int DC_GEOM_dc_zrot = 92;
const int dcg_dc_zrot = 92;
const int DC_GEOM_toroid_align = 93;
const int dcg_toroid_align = 93;
const int DC_GEOM_toroid_rot = 94;
const int dcg_toroid_rot = 94;
const int DC_STATUS_sector1_status = 95;
const int dcst_sector1_status = 95;
const int DC_STATUS_sector2_status = 96;
const int dcst_sector2_status = 96;
const int DC_STATUS_sector3_status = 97;
const int dcst_sector3_status = 97;
const int DC_STATUS_sector4_status = 98;
const int dcst_sector4_status = 98;
const int DC_STATUS_sector5_status = 99;
const int dcst_sector5_status = 99;
const int DC_STATUS_sector6_status = 100;
const int dcst_sector6_status = 100;
const int DC_SWAP_swap_ids_sector1 = 101;
const int dcsw_swap_ids_sector1 = 101;
const int DC_SWAP_swap_ids_sector2 = 102;
const int dcsw_swap_ids_sector2 = 102;
const int DC_SWAP_swap_ids_sector3 = 103;
const int dcsw_swap_ids_sector3 = 103;
const int DC_SWAP_swap_ids_sector4 = 104;
const int dcsw_swap_ids_sector4 = 104;
const int DC_SWAP_swap_ids_sector5 = 105;
const int dcsw_swap_ids_sector5 = 105;
const int DC_SWAP_swap_ids_sector6 = 106;
const int dcsw_swap_ids_sector6 = 106;
const int DC_SWAP_swap_info_bits = 107;
const int dcsw_swap_info_bits = 107;
const int DC_TDLY_Delays_DC2TOF = 108;
const int dcty_Delays_DC2TOF = 108;
const int DC_TDLY_Delays_Region4Pulser = 109;
const int dcty_Delays_Region4Pulser = 109;
const int DC_TDLY_sector1_T0 = 110;
const int dcty_sector1_T0 = 110;
const int DC_TDLY_sector2_T0 = 111;
const int dcty_sector2_T0 = 111;
const int DC_TDLY_sector3_T0 = 112;
const int dcty_sector3_T0 = 112;
const int DC_TDLY_sector4_T0 = 113;
const int dcty_sector4_T0 = 113;
const int DC_TDLY_sector5_T0 = 114;
const int dcty_sector5_T0 = 114;
const int DC_TDLY_sector6_T0 = 115;
const int dcty_sector6_T0 = 115;
const int DC_TIMEDIST_SL1_cuts = 116;
const int dctm_SL1_cuts = 116;
const int DC_TIMEDIST_SL1_dist = 117;
const int dctm_SL1_dist = 117;
const int DC_TIMEDIST_SL1_time = 118;
const int dctm_SL1_time = 118;
const int DC_TIMEDIST_SL2_cuts = 119;
const int dctm_SL2_cuts = 119;
const int DC_TIMEDIST_SL2_dist = 120;
const int dctm_SL2_dist = 120;
const int DC_TIMEDIST_SL2_time = 121;
const int dctm_SL2_time = 121;
const int DC_TIMEDIST_SL3_cuts = 122;
const int dctm_SL3_cuts = 122;
const int DC_TIMEDIST_SL3_dist = 123;
const int dctm_SL3_dist = 123;
const int DC_TIMEDIST_SL3_time = 124;
const int dctm_SL3_time = 124;
const int DC_TIMEDIST_SL4_cuts = 125;
const int dctm_SL4_cuts = 125;
const int DC_TIMEDIST_SL4_dist = 126;
const int dctm_SL4_dist = 126;
const int DC_TIMEDIST_SL4_time = 127;
const int dctm_SL4_time = 127;
const int DC_TIMEDIST_SL5_cuts = 128;
const int dctm_SL5_cuts = 128;
const int DC_TIMEDIST_SL5_dist = 129;
const int dctm_SL5_dist = 129;
const int DC_TIMEDIST_SL5_time = 130;
const int dctm_SL5_time = 130;
const int DC_TIMEDIST_SL6_cuts = 131;
const int dctm_SL6_cuts = 131;
const int DC_TIMEDIST_SL6_dist = 132;
const int dctm_SL6_dist = 132;
const int DC_TIMEDIST_SL6_time = 133;
const int dctm_SL6_time = 133;
const int DC_TIMEDIST_region1_x_vs_t_pars = 134;
const int dctm_region1_x_vs_t_pars = 134;
const int DC_TIMEDIST_region2_x_vs_t_pars = 135;
const int dctm_region2_x_vs_t_pars = 135;
const int DC_TIMEDIST_region3_x_vs_t_pars = 136;
const int dctm_region3_x_vs_t_pars = 136;
const int DC_TRANS_trans_sector1 = 137;
const int dctr_trans_sector1 = 137;
const int DC_TRANS_trans_sector2 = 138;
const int dctr_trans_sector2 = 138;
const int DC_TRANS_trans_sector3 = 139;
const int dctr_trans_sector3 = 139;
const int DC_TRANS_trans_sector4 = 140;
const int dctr_trans_sector4 = 140;
const int DC_TRANS_trans_sector5 = 141;
const int dctr_trans_sector5 = 141;
const int DC_TRANS_trans_sector6 = 142;
const int dctr_trans_sector6 = 142;
const int EC_CALIB_EC_ATTEN_InnerU = 143;
const int ec_EC_ATTEN_InnerU = 143;
const int EC_CALIB_EC_ATTEN_InnerV = 144;
const int ec_EC_ATTEN_InnerV = 144;
const int EC_CALIB_EC_ATTEN_InnerW = 145;
const int ec_EC_ATTEN_InnerW = 145;
const int EC_CALIB_EC_ATTEN_OuterU = 146;
const int ec_EC_ATTEN_OuterU = 146;
const int EC_CALIB_EC_ATTEN_OuterV = 147;
const int ec_EC_ATTEN_OuterV = 147;
const int EC_CALIB_EC_ATTEN_OuterW = 148;
const int ec_EC_ATTEN_OuterW = 148;
const int EC_CALIB_EC_ATTEN_WholeU = 149;
const int ec_EC_ATTEN_WholeU = 149;
const int EC_CALIB_EC_ATTEN_WholeV = 150;
const int ec_EC_ATTEN_WholeV = 150;
const int EC_CALIB_EC_ATTEN_WholeW = 151;
const int ec_EC_ATTEN_WholeW = 151;
const int EC_CALIB_EC_GAIN_InnerU = 152;
const int ec_EC_GAIN_InnerU = 152;
const int EC_CALIB_EC_GAIN_InnerV = 153;
const int ec_EC_GAIN_InnerV = 153;
const int EC_CALIB_EC_GAIN_InnerW = 154;
const int ec_EC_GAIN_InnerW = 154;
const int EC_CALIB_EC_GAIN_OuterU = 155;
const int ec_EC_GAIN_OuterU = 155;
const int EC_CALIB_EC_GAIN_OuterV = 156;
const int ec_EC_GAIN_OuterV = 156;
const int EC_CALIB_EC_GAIN_OuterW = 157;
const int ec_EC_GAIN_OuterW = 157;
const int EC_CALIB_EC_GAIN_WholeU = 158;
const int ec_EC_GAIN_WholeU = 158;
const int EC_CALIB_EC_GAIN_WholeV = 159;
const int ec_EC_GAIN_WholeV = 159;
const int EC_CALIB_EC_GAIN_WholeW = 160;
const int ec_EC_GAIN_WholeW = 160;
const int EC_CALIB_EC_PEDESTALS_InnerU = 161;
const int ec_EC_PEDESTALS_InnerU = 161;
const int EC_CALIB_EC_PEDESTALS_InnerV = 162;
const int ec_EC_PEDESTALS_InnerV = 162;
const int EC_CALIB_EC_PEDESTALS_InnerW = 163;
const int ec_EC_PEDESTALS_InnerW = 163;
const int EC_CALIB_EC_PEDESTALS_OuterU = 164;
const int ec_EC_PEDESTALS_OuterU = 164;
const int EC_CALIB_EC_PEDESTALS_OuterV = 165;
const int ec_EC_PEDESTALS_OuterV = 165;
const int EC_CALIB_EC_PEDESTALS_OuterW = 166;
const int ec_EC_PEDESTALS_OuterW = 166;
const int EC_CALIB_EC_PEDESTALS_WholeU = 167;
const int ec_EC_PEDESTALS_WholeU = 167;
const int EC_CALIB_EC_PEDESTALS_WholeV = 168;
const int ec_EC_PEDESTALS_WholeV = 168;
const int EC_CALIB_EC_PEDESTALS_WholeW = 169;
const int ec_EC_PEDESTALS_WholeW = 169;
const int EC_CALIB_EC_Tadc_InnerU = 170;
const int ec_EC_Tadc_InnerU = 170;
const int EC_CALIB_EC_Tadc_InnerV = 171;
const int ec_EC_Tadc_InnerV = 171;
const int EC_CALIB_EC_Tadc_InnerW = 172;
const int ec_EC_Tadc_InnerW = 172;
const int EC_CALIB_EC_Tadc_OuterU = 173;
const int ec_EC_Tadc_OuterU = 173;
const int EC_CALIB_EC_Tadc_OuterV = 174;
const int ec_EC_Tadc_OuterV = 174;
const int EC_CALIB_EC_Tadc_OuterW = 175;
const int ec_EC_Tadc_OuterW = 175;
const int EC_CALIB_EC_Tch_InnerU = 176;
const int ec_EC_Tch_InnerU = 176;
const int EC_CALIB_EC_Tch_InnerV = 177;
const int ec_EC_Tch_InnerV = 177;
const int EC_CALIB_EC_Tch_InnerW = 178;
const int ec_EC_Tch_InnerW = 178;
const int EC_CALIB_EC_Tch_OuterU = 179;
const int ec_EC_Tch_OuterU = 179;
const int EC_CALIB_EC_Tch_OuterV = 180;
const int ec_EC_Tch_OuterV = 180;
const int EC_CALIB_EC_Tch_OuterW = 181;
const int ec_EC_Tch_OuterW = 181;
const int EC_CALIB_EC_To_InnerU = 182;
const int ec_EC_To_InnerU = 182;
const int EC_CALIB_EC_To_InnerV = 183;
const int ec_EC_To_InnerV = 183;
const int EC_CALIB_EC_To_InnerW = 184;
const int ec_EC_To_InnerW = 184;
const int EC_CALIB_EC_To_OuterU = 185;
const int ec_EC_To_OuterU = 185;
const int EC_CALIB_EC_To_OuterV = 186;
const int ec_EC_To_OuterV = 186;
const int EC_CALIB_EC_To_OuterW = 187;
const int ec_EC_To_OuterW = 187;
const int EC_CALIB_EC_Trms_InnerU = 188;
const int ec_EC_Trms_InnerU = 188;
const int EC_CALIB_EC_Trms_InnerV = 189;
const int ec_EC_Trms_InnerV = 189;
const int EC_CALIB_EC_Trms_InnerW = 190;
const int ec_EC_Trms_InnerW = 190;
const int EC_CALIB_EC_Trms_OuterU = 191;
const int ec_EC_Trms_OuterU = 191;
const int EC_CALIB_EC_Trms_OuterV = 192;
const int ec_EC_Trms_OuterV = 192;
const int EC_CALIB_EC_Trms_OuterW = 193;
const int ec_EC_Trms_OuterW = 193;
const int EC_CALIB_EC_dT1_InnerU = 194;
const int ec_EC_dT1_InnerU = 194;
const int EC_CALIB_EC_dT1_InnerV = 195;
const int ec_EC_dT1_InnerV = 195;
const int EC_CALIB_EC_dT1_InnerW = 196;
const int ec_EC_dT1_InnerW = 196;
const int EC_CALIB_EC_dT1_OuterU = 197;
const int ec_EC_dT1_OuterU = 197;
const int EC_CALIB_EC_dT1_OuterV = 198;
const int ec_EC_dT1_OuterV = 198;
const int EC_CALIB_EC_dT1_OuterW = 199;
const int ec_EC_dT1_OuterW = 199;
const int EC_CALIB_EC_dT2_InnerU = 200;
const int ec_EC_dT2_InnerU = 200;
const int EC_CALIB_EC_dT2_InnerV = 201;
const int ec_EC_dT2_InnerV = 201;
const int EC_CALIB_EC_dT2_InnerW = 202;
const int ec_EC_dT2_InnerW = 202;
const int EC_CALIB_EC_dT2_OuterU = 203;
const int ec_EC_dT2_OuterU = 203;
const int EC_CALIB_EC_dT2_OuterV = 204;
const int ec_EC_dT2_OuterV = 204;
const int EC_CALIB_EC_dT2_OuterW = 205;
const int ec_EC_dT2_OuterW = 205;
const int E_T_MATRIX_matrix_contents = 206;
const int e_t_matrix_contents = 206;
const int GEOMETRY_beam_position = 207;
const int geo_beam_position = 207;
const int GEOMETRY_fwd_carriage_position = 208;
const int geo_fwd_carriage_position = 208;
const int GEOMETRY_fwd_carriage_rotation = 209;
const int geo_fwd_carriage_rotation = 209;
const int GEOMETRY_north_clam_position = 210;
const int geo_north_clam_position = 210;
const int GEOMETRY_north_clam_rotation = 211;
const int geo_north_clam_rotation = 211;
const int GEOMETRY_south_clam_position = 212;
const int geo_south_clam_position = 212;
const int GEOMETRY_south_clam_rotation = 213;
const int geo_south_clam_rotation = 213;
const int GEOMETRY_space_frame_1_position = 214;
const int geo_space_frame_1_position = 214;
const int GEOMETRY_space_frame_1_rotation = 215;
const int geo_space_frame_1_rotation = 215;
const int GEOMETRY_space_frame_2_position = 216;
const int geo_space_frame_2_position = 216;
const int GEOMETRY_space_frame_2_rotation = 217;
const int geo_space_frame_2_rotation = 217;
const int GEOMETRY_space_frame_3_position = 218;
const int geo_space_frame_3_position = 218;
const int GEOMETRY_space_frame_3_rotation = 219;
const int geo_space_frame_3_rotation = 219;
const int GEOMETRY_space_frame_4_position = 220;
const int geo_space_frame_4_position = 220;
const int GEOMETRY_space_frame_4_rotation = 221;
const int geo_space_frame_4_rotation = 221;
const int GEOMETRY_space_frame_5_position = 222;
const int geo_space_frame_5_position = 222;
const int GEOMETRY_space_frame_5_rotation = 223;
const int geo_space_frame_5_rotation = 223;
const int GEOMETRY_space_frame_6_position = 224;
const int geo_space_frame_6_position = 224;
const int GEOMETRY_space_frame_6_rotation = 225;
const int geo_space_frame_6_rotation = 225;
const int GPP_DC_Reg1 = 226;
const int gpp_DC_Reg1 = 226;
const int GPP_DC_Reg2 = 227;
const int gpp_DC_Reg2 = 227;
const int GPP_DC_Reg3 = 228;
const int gpp_DC_Reg3 = 228;
const int GPP_SC_Length = 229;
const int gpp_SC_Length = 229;
const int GPP_SC_Sigma = 230;
const int gpp_SC_Sigma = 230;
const int LAC_CALIB_sector1_c1l = 231;
const int lac_sector1_c1l = 231;
const int LAC_CALIB_sector1_c1r = 232;
const int lac_sector1_c1r = 232;
const int LAC_CALIB_sector1_c2l = 233;
const int lac_sector1_c2l = 233;
const int LAC_CALIB_sector1_c2r = 234;
const int lac_sector1_c2r = 234;
const int LAC_CALIB_sector2_c1l = 235;
const int lac_sector2_c1l = 235;
const int LAC_CALIB_sector2_c1r = 236;
const int lac_sector2_c1r = 236;
const int LAC_CALIB_sector2_c2l = 237;
const int lac_sector2_c2l = 237;
const int LAC_CALIB_sector2_c2r = 238;
const int lac_sector2_c2r = 238;
const int LAC_PEDESTALS_sector1_left = 239;
const int lacp_sector1_left = 239;
const int LAC_PEDESTALS_sector1_right = 240;
const int lacp_sector1_right = 240;
const int LAC_PEDESTALS_sector2_left = 241;
const int lacp_sector2_left = 241;
const int LAC_PEDESTALS_sector2_right = 242;
const int lacp_sector2_right = 242;
const int LAC_T0_sector1_t1l = 243;
const int lac0_sector1_t1l = 243;
const int LAC_T0_sector1_t1r = 244;
const int lac0_sector1_t1r = 244;
const int LAC_T0_sector1_t2l = 245;
const int lac0_sector1_t2l = 245;
const int LAC_T0_sector1_t2r = 246;
const int lac0_sector1_t2r = 246;
const int LAC_T0_sector2_t1l = 247;
const int lac0_sector2_t1l = 247;
const int LAC_T0_sector2_t1r = 248;
const int lac0_sector2_t1r = 248;
const int LAC_T0_sector2_t2l = 249;
const int lac0_sector2_t2l = 249;
const int LAC_T0_sector2_t2r = 250;
const int lac0_sector2_t2r = 250;
const int NEW_RUN_DATES_runinfo_date = 251;
const int nrun_runinfo_date = 251;
const int NEW_RUN_DATES_runinfo_period = 252;
const int nrun_runinfo_period = 252;
const int NORM_PS_scalers_delay = 253;
const int norm_PS_scalers_delay = 253;
const int NORM_PS_scalers_norm = 254;
const int norm_PS_scalers_norm = 254;
const int NORM_PS_scalers_prod = 255;
const int norm_PS_scalers_prod = 255;
const int NORM_PS_scalers_ratio = 256;
const int norm_PS_scalers_ratio = 256;
const int NORM_PS_scalers_translation = 257;
const int norm_PS_scalers_translation = 257;
const int NORM_PS_scalers_width_corr = 258;
const int norm_PS_scalers_width_corr = 258;
const int NORM_PS_scalers_width_ratio = 259;
const int norm_PS_scalers_width_ratio = 259;
const int NORM_normalization_F_norm = 260;
const int norm_normalization_F_norm = 260;
const int NORM_normalization_F_norm_u = 261;
const int norm_normalization_F_norm_u = 261;
const int NORM_normalization_livetime = 262;
const int norm_normalization_livetime = 262;
const int NORM_normalization_ngamma = 263;
const int norm_normalization_ngamma = 263;
const int NORM_normalization_prescale = 264;
const int norm_normalization_prescale = 264;
const int NORM_production_livetime = 265;
const int norm_production_livetime = 265;
const int NORM_production_pflux_ps = 266;
const int norm_production_pflux_ps = 266;
const int NORM_production_pflux_tscal = 267;
const int norm_production_pflux_tscal = 267;
const int NORM_production_pflux_tscal_u = 268;
const int norm_production_pflux_tscal_u = 268;
const int NORM_production_prescale = 269;
const int norm_production_prescale = 269;
const int PRESCALE_CORR_corr1_value = 270;
const int pres_corr1_value = 270;
const int PRESCALE_CORR_corr2_value = 271;
const int pres_corr2_value = 271;
const int PRESCALE_CORR_phase_value = 272;
const int pres_phase_value = 272;
const int PRESCALE_CORR_prescale_value = 273;
const int pres_prescale_value = 273;
const int RF_COR_F_F1_p0 = 274;
const int rfcr_F1_p0 = 274;
const int RF_COR_F_F1_p1 = 275;
const int rfcr_F1_p1 = 275;
const int RF_COR_F_F1_p2 = 276;
const int rfcr_F1_p2 = 276;
const int RF_COR_F_F2_p0 = 277;
const int rfcr_F2_p0 = 277;
const int RF_COR_F_F2_p1 = 278;
const int rfcr_F2_p1 = 278;
const int RF_COR_F_F2_p2 = 279;
const int rfcr_F2_p2 = 279;
const int RF_OFFSETS_F1_high = 280;
const int rf_F1_high = 280;
const int RF_OFFSETS_F1_low = 281;
const int rf_F1_low = 281;
const int RF_OFFSETS_F1_p0 = 282;
const int rf_F1_p0 = 282;
const int RF_OFFSETS_F1_p1 = 283;
const int rf_F1_p1 = 283;
const int RF_OFFSETS_F1_p2 = 284;
const int rf_F1_p2 = 284;
const int RF_OFFSETS_F1_p3 = 285;
const int rf_F1_p3 = 285;
const int RF_OFFSETS_F2_high = 286;
const int rf_F2_high = 286;
const int RF_OFFSETS_F2_low = 287;
const int rf_F2_low = 287;
const int RF_OFFSETS_F2_p0 = 288;
const int rf_F2_p0 = 288;
const int RF_OFFSETS_F2_p1 = 289;
const int rf_F2_p1 = 289;
const int RF_OFFSETS_F2_p2 = 290;
const int rf_F2_p2 = 290;
const int RF_OFFSETS_F2_p3 = 291;
const int rf_F2_p3 = 291;
const int RF_OFFSETS_F3_high = 292;
const int rf_F3_high = 292;
const int RF_OFFSETS_F3_low = 293;
const int rf_F3_low = 293;
const int RF_OFFSETS_F3_p0 = 294;
const int rf_F3_p0 = 294;
const int RF_OFFSETS_F3_p1 = 295;
const int rf_F3_p1 = 295;
const int RF_OFFSETS_F3_p2 = 296;
const int rf_F3_p2 = 296;
const int RF_OFFSETS_F3_p3 = 297;
const int rf_F3_p3 = 297;
const int RF_OFFSETS_F4_high = 298;
const int rf_F4_high = 298;
const int RF_OFFSETS_F4_low = 299;
const int rf_F4_low = 299;
const int RF_OFFSETS_F4_p0 = 300;
const int rf_F4_p0 = 300;
const int RF_OFFSETS_F4_p1 = 301;
const int rf_F4_p1 = 301;
const int RF_OFFSETS_F4_p2 = 302;
const int rf_F4_p2 = 302;
const int RF_OFFSETS_F4_p3 = 303;
const int rf_F4_p3 = 303;
const int RF_OFFSETS_dead_range_rf1_dead_high = 304;
const int rf_dead_range_rf1_dead_high = 304;
const int RF_OFFSETS_dead_range_rf1_dead_low = 305;
const int rf_dead_range_rf1_dead_low = 305;
const int RF_OFFSETS_dead_range_rf2_dead_high = 306;
const int rf_dead_range_rf2_dead_high = 306;
const int RF_OFFSETS_dead_range_rf2_dead_low = 307;
const int rf_dead_range_rf2_dead_low = 307;
const int RF_OFFSETS_offset_delta = 308;
const int rf_offset_delta = 308;
const int RF_OFFSETS_offset_value = 309;
const int rf_offset_value = 309;
const int RF_OFFSETS_rf_diff_rf1_gt_rf2 = 310;
const int rf_rf_diff_rf1_gt_rf2 = 310;
const int RF_OFFSETS_rf_diff_rf1_lt_rf2 = 311;
const int rf_rf_diff_rf1_lt_rf2 = 311;
const int RF_OFFSETS_rf_range_high = 312;
const int rf_rf_range_high = 312;
const int RF_OFFSETS_rf_range_high2 = 313;
const int rf_rf_range_high2 = 313;
const int RF_OFFSETS_rf_range_low = 314;
const int rf_rf_range_low = 314;
const int RF_OFFSETS_rf_range_low2 = 315;
const int rf_rf_range_low2 = 315;
const int RF_OFFSETS_rf_range_rf1_high1 = 316;
const int rf_rf_range_rf1_high1 = 316;
const int RF_OFFSETS_rf_range_rf1_high2 = 317;
const int rf_rf_range_rf1_high2 = 317;
const int RF_OFFSETS_rf_range_rf1_low1 = 318;
const int rf_rf_range_rf1_low1 = 318;
const int RF_OFFSETS_rf_range_rf1_low2 = 319;
const int rf_rf_range_rf1_low2 = 319;
const int RF_OFFSETS_rf_range_rf2_high1 = 320;
const int rf_rf_range_rf2_high1 = 320;
const int RF_OFFSETS_rf_range_rf2_high2 = 321;
const int rf_rf_range_rf2_high2 = 321;
const int RF_OFFSETS_rf_range_rf2_low1 = 322;
const int rf_rf_range_rf2_low1 = 322;
const int RF_OFFSETS_rf_range_rf2_low2 = 323;
const int rf_rf_range_rf2_low2 = 323;
const int RF_OFFSETS_status_value = 324;
const int rf_status_value = 324;
const int RUN_CONTROL_Faraday_cup_Q_active = 325;
const int runc_Faraday_cup_Q_active = 325;
const int RUN_CONTROL_Faraday_cup_Q_all = 326;
const int runc_Faraday_cup_Q_all = 326;
const int RUN_CONTROL_Faraday_cup_Q_live = 327;
const int runc_Faraday_cup_Q_live = 327;
const int RUN_CONTROL_beam_energy = 328;
const int runc_beam_energy = 328;
const int RUN_CONTROL_beam_type = 329;
const int runc_beam_type = 329;
const int RUN_CONTROL_beampol_staterr = 330;
const int runc_beampol_staterr = 330;
const int RUN_CONTROL_beampol_syserr = 331;
const int runc_beampol_syserr = 331;
const int RUN_CONTROL_beampol_value = 332;
const int runc_beampol_value = 332;
const int RUN_CONTROL_currents_minitorus = 333;
const int runc_currents_minitorus = 333;
const int RUN_CONTROL_currents_pair_spec = 334;
const int runc_currents_pair_spec = 334;
const int RUN_CONTROL_currents_ptm = 335;
const int runc_currents_ptm = 335;
const int RUN_CONTROL_currents_tagger = 336;
const int runc_currents_tagger = 336;
const int RUN_CONTROL_currents_torus = 337;
const int runc_currents_torus = 337;
const int RUN_CONTROL_events_raw = 338;
const int runc_events_raw = 338;
const int RUN_CONTROL_roads_link = 339;
const int runc_roads_link = 339;
const int RUN_CONTROL_target_mass = 340;
const int runc_target_mass = 340;
const int RUN_CONTROL_target_type = 341;
const int runc_target_type = 341;
const int RUN_DATES_date_value = 342;
const int rund_date_value = 342;
const int S1ST_S1ST_event_count = 343;
const int s1st_S1ST_event_count = 343;
const int S1ST_S1ST_latch_bit1 = 344;
const int s1st_S1ST_latch_bit1 = 344;
const int S1ST_S1ST_latch_bit10 = 345;
const int s1st_S1ST_latch_bit10 = 345;
const int S1ST_S1ST_latch_bit11 = 346;
const int s1st_S1ST_latch_bit11 = 346;
const int S1ST_S1ST_latch_bit12 = 347;
const int s1st_S1ST_latch_bit12 = 347;
const int S1ST_S1ST_latch_bit2 = 348;
const int s1st_S1ST_latch_bit2 = 348;
const int S1ST_S1ST_latch_bit3 = 349;
const int s1st_S1ST_latch_bit3 = 349;
const int S1ST_S1ST_latch_bit4 = 350;
const int s1st_S1ST_latch_bit4 = 350;
const int S1ST_S1ST_latch_bit5 = 351;
const int s1st_S1ST_latch_bit5 = 351;
const int S1ST_S1ST_latch_bit6 = 352;
const int s1st_S1ST_latch_bit6 = 352;
const int S1ST_S1ST_latch_bit7 = 353;
const int s1st_S1ST_latch_bit7 = 353;
const int S1ST_S1ST_latch_bit8 = 354;
const int s1st_S1ST_latch_bit8 = 354;
const int S1ST_S1ST_latch_bit9 = 355;
const int s1st_S1ST_latch_bit9 = 355;
const int S1ST_S1ST_latch_empty = 356;
const int s1st_S1ST_latch_empty = 356;
const int S1ST_S1ST_latch_not_empty = 357;
const int s1st_S1ST_latch_not_empty = 357;
const int S1ST_S1ST_latch_zero = 358;
const int s1st_S1ST_latch_zero = 358;
const int SCALERS_TGS_values = 359;
const int scal_TGS_values = 359;
const int SCALERS_TRGS_values = 360;
const int scal_TRGS_values = 360;
const int SC_CALIBRATIONS_NMIP_ADC_left = 361;
const int sc_NMIP_ADC_left = 361;
const int SC_CALIBRATIONS_NMIP_ADC_right = 362;
const int sc_NMIP_ADC_right = 362;
const int SC_CALIBRATIONS_NMIP_ADCu_left = 363;
const int sc_NMIP_ADCu_left = 363;
const int SC_CALIBRATIONS_NMIP_ADCu_right = 364;
const int sc_NMIP_ADCu_right = 364;
const int SC_CALIBRATIONS_T0_TDC_left = 365;
const int sc_T0_TDC_left = 365;
const int SC_CALIBRATIONS_T0_TDC_right = 366;
const int sc_T0_TDC_right = 366;
const int SC_CALIBRATIONS_T0_TDCu_left = 367;
const int sc_T0_TDCu_left = 367;
const int SC_CALIBRATIONS_T0_TDCu_right = 368;
const int sc_T0_TDCu_right = 368;
const int SC_CALIBRATIONS_T1_left = 369;
const int sc_T1_left = 369;
const int SC_CALIBRATIONS_T1_right = 370;
const int sc_T1_right = 370;
const int SC_CALIBRATIONS_T1u_left = 371;
const int sc_T1u_left = 371;
const int SC_CALIBRATIONS_T1u_right = 372;
const int sc_T1u_right = 372;
const int SC_CALIBRATIONS_T2_left = 373;
const int sc_T2_left = 373;
const int SC_CALIBRATIONS_T2_right = 374;
const int sc_T2_right = 374;
const int SC_CALIBRATIONS_T2u_left = 375;
const int sc_T2u_left = 375;
const int SC_CALIBRATIONS_T2u_right = 376;
const int sc_T2u_right = 376;
const int SC_CALIBRATIONS_T_sigma_first_param = 377;
const int sc_T_sigma_first_param = 377;
const int SC_CALIBRATIONS_T_sigma_second_param = 378;
const int sc_T_sigma_second_param = 378;
const int SC_CALIBRATIONS_WALK0u_left = 379;
const int sc_WALK0u_left = 379;
const int SC_CALIBRATIONS_WALK0u_right = 380;
const int sc_WALK0u_right = 380;
const int SC_CALIBRATIONS_WALK1_left = 381;
const int sc_WALK1_left = 381;
const int SC_CALIBRATIONS_WALK1_right = 382;
const int sc_WALK1_right = 382;
const int SC_CALIBRATIONS_WALK1u_left = 383;
const int sc_WALK1u_left = 383;
const int SC_CALIBRATIONS_WALK1u_right = 384;
const int sc_WALK1u_right = 384;
const int SC_CALIBRATIONS_WALK2_left = 385;
const int sc_WALK2_left = 385;
const int SC_CALIBRATIONS_WALK2_right = 386;
const int sc_WALK2_right = 386;
const int SC_CALIBRATIONS_WALK2u_left = 387;
const int sc_WALK2u_left = 387;
const int SC_CALIBRATIONS_WALK2u_right = 388;
const int sc_WALK2u_right = 388;
const int SC_CALIBRATIONS_WALK_A0_left = 389;
const int sc_WALK_A0_left = 389;
const int SC_CALIBRATIONS_WALK_A0_right = 390;
const int sc_WALK_A0_right = 390;
const int SC_CALIBRATIONS_Yoffset_value = 391;
const int sc_Yoffset_value = 391;
const int SC_CALIBRATIONS_atten_length_left = 392;
const int sc_atten_length_left = 392;
const int SC_CALIBRATIONS_atten_length_right = 393;
const int sc_atten_length_right = 393;
const int SC_CALIBRATIONS_atten_u_left = 394;
const int sc_atten_u_left = 394;
const int SC_CALIBRATIONS_atten_u_right = 395;
const int sc_atten_u_right = 395;
const int SC_CALIBRATIONS_delta_T_coupled_paddle1 = 396;
const int sc_delta_T_coupled_paddle1 = 396;
const int SC_CALIBRATIONS_delta_T_coupled_paddle2 = 397;
const int sc_delta_T_coupled_paddle2 = 397;
const int SC_CALIBRATIONS_delta_T_dc2sc = 398;
const int sc_delta_T_dc2sc = 398;
const int SC_CALIBRATIONS_delta_T_left_right = 399;
const int sc_delta_T_left_right = 399;
const int SC_CALIBRATIONS_delta_T_paddle2paddle = 400;
const int sc_delta_T_paddle2paddle = 400;
const int SC_CALIBRATIONS_delta_T_panel2panel = 401;
const int sc_delta_T_panel2panel = 401;
const int SC_CALIBRATIONS_delta_Tu_left_right = 402;
const int sc_delta_Tu_left_right = 402;
const int SC_CALIBRATIONS_delta_Tu_paddle2paddle = 403;
const int sc_delta_Tu_paddle2paddle = 403;
const int SC_CALIBRATIONS_delta_Tu_panel2panel = 404;
const int sc_delta_Tu_panel2panel = 404;
const int SC_CALIBRATIONS_pedestals_left = 405;
const int sc_pedestals_left = 405;
const int SC_CALIBRATIONS_pedestals_right = 406;
const int sc_pedestals_right = 406;
const int SC_CALIBRATIONS_pedu_left = 407;
const int sc_pedu_left = 407;
const int SC_CALIBRATIONS_pedu_right = 408;
const int sc_pedu_right = 408;
const int SC_CALIBRATIONS_pulser_normalisation = 409;
const int sc_pulser_normalisation = 409;
const int SC_CALIBRATIONS_status_left = 410;
const int sc_status_left = 410;
const int SC_CALIBRATIONS_status_right = 411;
const int sc_status_right = 411;
const int SC_CALIBRATIONS_time_walk_ref_adc = 412;
const int sc_time_walk_ref_adc = 412;
const int SC_CALIBRATIONS_veff_left = 413;
const int sc_veff_left = 413;
const int SC_CALIBRATIONS_veff_right = 414;
const int sc_veff_right = 414;
const int SC_CALIBRATIONS_veffu_left = 415;
const int sc_veffu_left = 415;
const int SC_CALIBRATIONS_veffu_right = 416;
const int sc_veffu_right = 416;
const int ST_CALIB_LAMBDA_unc = 417;
const int st_LAMBDA_unc = 417;
const int ST_CALIB_LAMBDA_value = 418;
const int st_LAMBDA_value = 418;
const int ST_CALIB_M0_unc = 419;
const int st_M0_unc = 419;
const int ST_CALIB_M0_value = 420;
const int st_M0_value = 420;
const int ST_CALIB_T0_unc = 421;
const int st_T0_unc = 421;
const int ST_CALIB_T0_value = 422;
const int st_T0_value = 422;
const int ST_CALIB_T1_unc = 423;
const int st_T1_unc = 423;
const int ST_CALIB_T1_value = 424;
const int st_T1_value = 424;
const int ST_CALIB_T2_unc = 425;
const int st_T2_unc = 425;
const int ST_CALIB_T2_value = 426;
const int st_T2_value = 426;
const int ST_CALIB_W0_unc = 427;
const int st_W0_unc = 427;
const int ST_CALIB_W0_value = 428;
const int st_W0_value = 428;
const int ST_CALIB_W1_unc = 429;
const int st_W1_unc = 429;
const int ST_CALIB_W1_value = 430;
const int st_W1_value = 430;
const int ST_CALIB_W2_unc = 431;
const int st_W2_unc = 431;
const int ST_CALIB_W2_value = 432;
const int st_W2_value = 432;
const int ST_CALIB_delta_T_pair2pair = 433;
const int st_delta_T_pair2pair = 433;
const int ST_CALIB_delta_T_side2side = 434;
const int st_delta_T_side2side = 434;
const int ST_CALIB_delta_T_unc = 435;
const int st_delta_T_unc = 435;
const int ST_CALIB_delta_T_value = 436;
const int st_delta_T_value = 436;
const int ST_CALIB_delta_Tu_side2side = 437;
const int st_delta_Tu_side2side = 437;
const int ST_CALIB_pedestals_unc = 438;
const int st_pedestals_unc = 438;
const int ST_CALIB_pedestals_value = 439;
const int st_pedestals_value = 439;
const int ST_CALIB_st2tof_unc = 440;
const int st_st2tof_unc = 440;
const int ST_CALIB_st2tof_value = 441;
const int st_st2tof_value = 441;
const int ST_CALIB_veff_leg_far = 442;
const int st_veff_leg_far = 442;
const int ST_CALIB_veff_leg_near = 443;
const int st_veff_leg_near = 443;
const int ST_CALIB_veff_leg_unc = 444;
const int st_veff_leg_unc = 444;
const int ST_CALIB_veff_leg_value = 445;
const int st_veff_leg_value = 445;
const int ST_CALIB_veff_nose_far = 446;
const int st_veff_nose_far = 446;
const int ST_CALIB_veff_nose_near = 447;
const int st_veff_nose_near = 447;
const int ST_CALIB_veff_nose_unc = 448;
const int st_veff_nose_unc = 448;
const int ST_CALIB_veff_nose_value = 449;
const int st_veff_nose_value = 449;
const int TAG_CALIB_pc_peak = 450;
const int tag_pc_peak = 450;
const int TAG_CALIB_pc_ped = 451;
const int tag_pc_ped = 451;
const int TAG_CALIB_pc_tdc_w = 452;
const int tag_pc_tdc_w = 452;
const int TAG_CALIB_ps_Ethresh = 453;
const int tag_ps_Ethresh = 453;
const int TAG_CALIB_ps_peak = 454;
const int tag_ps_peak = 454;
const int TAG_CALIB_ps_ped = 455;
const int tag_ps_ped = 455;
const int TAG_CALIB_ps_tdc_w = 456;
const int tag_ps_tdc_w = 456;
const int TAG_CALIB_ps_walk = 457;
const int tag_ps_walk = 457;
const int TAG_CALIB_status_tage = 458;
const int tag_status_tage = 458;
const int TAG_CALIB_status_tagt_left = 459;
const int tag_status_tagt_left = 459;
const int TAG_CALIB_status_tagt_right = 460;
const int tag_status_tagt_right = 460;
const int TAG_CALIB_tac_Escale = 461;
const int tag_tac_Escale = 461;
const int TAG_CALIB_tac_peak = 462;
const int tag_tac_peak = 462;
const int TAG_CALIB_tac_ped = 463;
const int tag_tac_ped = 463;
const int TAG_CALIB_tac_tdc_w = 464;
const int tag_tac_tdc_w = 464;
const int TAG_CALIB_tag2tof_value = 465;
const int tag_tag2tof_value = 465;
const int TAG_CALIB_tag_e_dt = 466;
const int tag_tag_e_dt = 466;
const int TAG_CALIB_tag_e_slope = 467;
const int tag_tag_e_slope = 467;
const int TAG_CALIB_tag_t_ci = 468;
const int tag_tag_t_ci = 468;
const int TAG_CALIB_tag_t_do_translation = 469;
const int tag_tag_t_do_translation = 469;
const int TAG_CALIB_tag_t_dt_left = 470;
const int tag_tag_t_dt_left = 470;
const int TAG_CALIB_tag_t_dt_right = 471;
const int tag_tag_t_dt_right = 471;
const int TAG_CALIB_tag_t_slope_left = 472;
const int tag_tag_t_slope_left = 472;
const int TAG_CALIB_tag_t_slope_right = 473;
const int tag_tag_t_slope_right = 473;
const int TAG_CALIB_tag_t_transl_to_table = 474;
const int tag_tag_t_transl_to_table = 474;
const int TAG_SCALER_G1SL_value = 475;
const int tags_G1SL_value = 475;
const int TAG_SCALER_G2SL_value = 476;
const int tags_G2SL_value = 476;
const int TAG_SCALER_G3SL_value = 477;
const int tags_G3SL_value = 477;
const int TAG_SCALER_G4SL_value = 478;
const int tags_G4SL_value = 478;
const int TAG_SCALER_PCEff_value = 479;
const int tags_PCEff_value = 479;
const int TAG_SCALER_PSEff_value = 480;
const int tags_PSEff_value = 480;
const int TAG_SCALER_RTSL_value = 481;
const int tags_RTSL_value = 481;
const int TAG_SCALER_TagEff_value = 482;
const int tags_TagEff_value = 482;
const int TAG_SCALER_gate_value = 483;
const int tags_gate_value = 483;
const int g6normalisation_GENERAL_PSAccCorr = 484;
const int g6n_GENERAL_PSAccCorr = 484;
const int g6normalisation_GENERAL_T_for_E = 485;
const int g6n_GENERAL_T_for_E = 485;
const int g6normalisation_GENERAL_width_ratio = 486;
const int g6n_GENERAL_width_ratio = 486;
const int g6normalisation_NORMALISATION_NGamma = 487;
const int g6n_NORMALISATION_NGamma = 487;
const int g6normalisation_NORMALISATION_NGammaT = 488;
const int g6n_NORMALISATION_NGammaT = 488;
const int g6normalisation_NORMALISATION_PC = 489;
const int g6n_NORMALISATION_PC = 489;
const int g6normalisation_NORMALISATION_PS = 490;
const int g6n_NORMALISATION_PS = 490;
const int g6normalisation_NORMALISATION_RAWT = 491;
const int g6n_NORMALISATION_RAWT = 491;
const int g6normalisation_NORMALISATION_TAC = 492;
const int g6n_NORMALISATION_TAC = 492;
const int g6normalisation_NORMALISATION_livetime = 493;
const int g6n_NORMALISATION_livetime = 493;
const int g6normalisation_NORMALISATION_prescale = 494;
const int g6n_NORMALISATION_prescale = 494;
const int g6normalisation_PRODUCTION_G1SL = 495;
const int g6n_PRODUCTION_G1SL = 495;
const int g6normalisation_PRODUCTION_G2SL = 496;
const int g6n_PRODUCTION_G2SL = 496;
const int g6normalisation_PRODUCTION_G3SL = 497;
const int g6n_PRODUCTION_G3SL = 497;
const int g6normalisation_PRODUCTION_G4SL = 498;
const int g6n_PRODUCTION_G4SL = 498;
const int g6normalisation_PRODUCTION_NEvent = 499;
const int g6n_PRODUCTION_NEvent = 499;
const int g6normalisation_PRODUCTION_NGammaE = 500;
const int g6n_PRODUCTION_NGammaE = 500;
const int g6normalisation_PRODUCTION_NGammaE_2 = 501;
const int g6n_PRODUCTION_NGammaE_2 = 501;
const int g6normalisation_PRODUCTION_NGammaT = 502;
const int g6n_PRODUCTION_NGammaT = 502;
const int g6normalisation_PRODUCTION_NGammaT_2 = 503;
const int g6n_PRODUCTION_NGammaT_2 = 503;
const int g6normalisation_PRODUCTION_RTSL = 504;
const int g6n_PRODUCTION_RTSL = 504;
const int g6normalisation_PRODUCTION_livetime = 505;
const int g6n_PRODUCTION_livetime = 505;
const int g6normalisation_PRODUCTION_prescale = 506;
const int g6n_PRODUCTION_prescale = 506;
const int g_post_cal_norm_et_1 = 507;
const int gpc_norm_et_1 = 507;
const int g_post_cal_norm_et_10 = 508;
const int gpc_norm_et_10 = 508;
const int g_post_cal_norm_et_11 = 509;
const int gpc_norm_et_11 = 509;
const int g_post_cal_norm_et_12 = 510;
const int gpc_norm_et_12 = 510;
const int g_post_cal_norm_et_13 = 511;
const int gpc_norm_et_13 = 511;
const int g_post_cal_norm_et_14 = 512;
const int gpc_norm_et_14 = 512;
const int g_post_cal_norm_et_15 = 513;
const int gpc_norm_et_15 = 513;
const int g_post_cal_norm_et_16 = 514;
const int gpc_norm_et_16 = 514;
const int g_post_cal_norm_et_17 = 515;
const int gpc_norm_et_17 = 515;
const int g_post_cal_norm_et_18 = 516;
const int gpc_norm_et_18 = 516;
const int g_post_cal_norm_et_19 = 517;
const int gpc_norm_et_19 = 517;
const int g_post_cal_norm_et_2 = 518;
const int gpc_norm_et_2 = 518;
const int g_post_cal_norm_et_20 = 519;
const int gpc_norm_et_20 = 519;
const int g_post_cal_norm_et_21 = 520;
const int gpc_norm_et_21 = 520;
const int g_post_cal_norm_et_22 = 521;
const int gpc_norm_et_22 = 521;
const int g_post_cal_norm_et_23 = 522;
const int gpc_norm_et_23 = 522;
const int g_post_cal_norm_et_24 = 523;
const int gpc_norm_et_24 = 523;
const int g_post_cal_norm_et_25 = 524;
const int gpc_norm_et_25 = 524;
const int g_post_cal_norm_et_26 = 525;
const int gpc_norm_et_26 = 525;
const int g_post_cal_norm_et_27 = 526;
const int gpc_norm_et_27 = 526;
const int g_post_cal_norm_et_28 = 527;
const int gpc_norm_et_28 = 527;
const int g_post_cal_norm_et_29 = 528;
const int gpc_norm_et_29 = 528;
const int g_post_cal_norm_et_3 = 529;
const int gpc_norm_et_3 = 529;
const int g_post_cal_norm_et_30 = 530;
const int gpc_norm_et_30 = 530;
const int g_post_cal_norm_et_31 = 531;
const int gpc_norm_et_31 = 531;
const int g_post_cal_norm_et_32 = 532;
const int gpc_norm_et_32 = 532;
const int g_post_cal_norm_et_33 = 533;
const int gpc_norm_et_33 = 533;
const int g_post_cal_norm_et_34 = 534;
const int gpc_norm_et_34 = 534;
const int g_post_cal_norm_et_35 = 535;
const int gpc_norm_et_35 = 535;
const int g_post_cal_norm_et_36 = 536;
const int gpc_norm_et_36 = 536;
const int g_post_cal_norm_et_37 = 537;
const int gpc_norm_et_37 = 537;
const int g_post_cal_norm_et_38 = 538;
const int gpc_norm_et_38 = 538;
const int g_post_cal_norm_et_39 = 539;
const int gpc_norm_et_39 = 539;
const int g_post_cal_norm_et_4 = 540;
const int gpc_norm_et_4 = 540;
const int g_post_cal_norm_et_40 = 541;
const int gpc_norm_et_40 = 541;
const int g_post_cal_norm_et_41 = 542;
const int gpc_norm_et_41 = 542;
const int g_post_cal_norm_et_42 = 543;
const int gpc_norm_et_42 = 543;
const int g_post_cal_norm_et_43 = 544;
const int gpc_norm_et_43 = 544;
const int g_post_cal_norm_et_44 = 545;
const int gpc_norm_et_44 = 545;
const int g_post_cal_norm_et_45 = 546;
const int gpc_norm_et_45 = 546;
const int g_post_cal_norm_et_46 = 547;
const int gpc_norm_et_46 = 547;
const int g_post_cal_norm_et_47 = 548;
const int gpc_norm_et_47 = 548;
const int g_post_cal_norm_et_48 = 549;
const int gpc_norm_et_48 = 549;
const int g_post_cal_norm_et_49 = 550;
const int gpc_norm_et_49 = 550;
const int g_post_cal_norm_et_5 = 551;
const int gpc_norm_et_5 = 551;
const int g_post_cal_norm_et_50 = 552;
const int gpc_norm_et_50 = 552;
const int g_post_cal_norm_et_51 = 553;
const int gpc_norm_et_51 = 553;
const int g_post_cal_norm_et_52 = 554;
const int gpc_norm_et_52 = 554;
const int g_post_cal_norm_et_53 = 555;
const int gpc_norm_et_53 = 555;
const int g_post_cal_norm_et_54 = 556;
const int gpc_norm_et_54 = 556;
const int g_post_cal_norm_et_55 = 557;
const int gpc_norm_et_55 = 557;
const int g_post_cal_norm_et_56 = 558;
const int gpc_norm_et_56 = 558;
const int g_post_cal_norm_et_57 = 559;
const int gpc_norm_et_57 = 559;
const int g_post_cal_norm_et_58 = 560;
const int gpc_norm_et_58 = 560;
const int g_post_cal_norm_et_59 = 561;
const int gpc_norm_et_59 = 561;
const int g_post_cal_norm_et_6 = 562;
const int gpc_norm_et_6 = 562;
const int g_post_cal_norm_et_60 = 563;
const int gpc_norm_et_60 = 563;
const int g_post_cal_norm_et_61 = 564;
const int gpc_norm_et_61 = 564;
const int g_post_cal_norm_et_7 = 565;
const int gpc_norm_et_7 = 565;
const int g_post_cal_norm_et_8 = 566;
const int gpc_norm_et_8 = 566;
const int g_post_cal_norm_et_9 = 567;
const int gpc_norm_et_9 = 567;
const int g_post_cal_norm_t_cal = 568;
const int gpc_norm_t_cal = 568;
const int g_post_cal_norm_t_kill = 569;
const int gpc_norm_t_kill = 569;
const int g_post_cal_prod_t_cal = 570;
const int gpc_prod_t_cal = 570;
const int g_post_cal_prod_t_kill = 571;
const int gpc_prod_t_kill = 571;
const int mom_corr_phi_func_sector1 = 572;
const int mom_phi_func_sector1 = 572;
const int mom_corr_phi_func_sector2 = 573;
const int mom_phi_func_sector2 = 573;
const int mom_corr_phi_func_sector3 = 574;
const int mom_phi_func_sector3 = 574;
const int mom_corr_phi_func_sector4 = 575;
const int mom_phi_func_sector4 = 575;
const int mom_corr_phi_func_sector5 = 576;
const int mom_phi_func_sector5 = 576;
const int mom_corr_phi_func_sector6 = 577;
const int mom_phi_func_sector6 = 577;
const int mom_corr_theta_func_parms = 578;
const int mom_theta_func_parms = 578;
const int mom_corr_theta_func_sector1 = 579;
const int mom_theta_func_sector1 = 579;
const int mom_corr_theta_func_sector2 = 580;
const int mom_theta_func_sector2 = 580;
const int mom_corr_theta_func_sector3 = 581;
const int mom_theta_func_sector3 = 581;
const int mom_corr_theta_func_sector4 = 582;
const int mom_theta_func_sector4 = 582;
const int mom_corr_theta_func_sector5 = 583;
const int mom_theta_func_sector5 = 583;
const int mom_corr_theta_func_sector6 = 584;
const int mom_theta_func_sector6 = 584;
const int LAC_TIMING_sector1_c_in_stack = 585;
const int lact_sector1_c_in_stack = 585;
const int LAC_TIMING_sector1_c_l = 586;
const int lact_sector1_c_l = 586;
const int LAC_TIMING_sector1_c_r = 587;
const int lact_sector1_c_r = 587;
const int LAC_TIMING_sector1_tw_al = 588;
const int lact_sector1_tw_al = 588;
const int LAC_TIMING_sector1_tw_ar = 589;
const int lact_sector1_tw_ar = 589;
const int LAC_TIMING_sector1_tw_bl = 590;
const int lact_sector1_tw_bl = 590;
const int LAC_TIMING_sector1_tw_br = 591;
const int lact_sector1_tw_br = 591;
const int LAC_TIMING_sector2_c_in_stack = 592;
const int lact_sector2_c_in_stack = 592;
const int LAC_TIMING_sector2_c_l = 593;
const int lact_sector2_c_l = 593;
const int LAC_TIMING_sector2_c_r = 594;
const int lact_sector2_c_r = 594;
const int LAC_TIMING_sector2_d_l = 595;
const int lact_sector2_d_l = 595;
const int LAC_TIMING_sector2_d_r = 596;
const int lact_sector2_d_r = 596;
const int LAC_TIMING_sector2_tw_al = 597;
const int lact_sector2_tw_al = 597;
const int LAC_TIMING_sector2_tw_ar = 598;
const int lact_sector2_tw_ar = 598;
const int LAC_TIMING_sector2_tw_bl = 599;
const int lact_sector2_tw_bl = 599;
const int LAC_TIMING_sector2_tw_br = 600;
const int lact_sector2_tw_br = 600;
const int GEOMETRY_target_position = 601;
const int geo_target_position = 601;
const int GEOMETRY_st_aperture = 602;
const int geo_st_aperture = 602;
const int GEOMETRY_st_dist2bl = 603;
const int geo_st_dist2bl = 603;
const int GEOMETRY_st_legVolume = 604;
const int geo_st_legVolume = 604;
const int GEOMETRY_st_noseAngle = 605;
const int geo_st_noseAngle = 605;
const int GEOMETRY_st_noseVolume = 606;
const int geo_st_noseVolume = 606;
const int GEOMETRY_st_offset = 607;
const int geo_st_offset = 607;
const int GEOMETRY_st_thickness = 608;
const int geo_st_thickness = 608;
const int NORM_photon_atten_value = 609;
const int norm_photon_atten_value = 609;
const int NORM_photon_atten_error = 610;
const int norm_photon_atten_error = 610;
const int NORM_gflux_begin_t_window = 611;
const int norm_gflux_begin_t_window = 611;
const int NORM_gflux_end_t_window = 612;
const int norm_gflux_end_t_window = 612;
const int NORM_gflux_ngamma_25mev = 613;
const int norm_gflux_ngamma_25mev = 613;
const int NORM_gflux_ngamma_25mev_u = 614;
const int norm_gflux_ngamma_25mev_u = 614;
const int NORM_gflux_ngamma_eb = 615;
const int norm_gflux_ngamma_eb = 615;
const int NORM_gflux_ngamma_eb_u = 616;
const int norm_gflux_ngamma_eb_u = 616;
const int NORM_gflux_ngamma_tc = 617;
const int norm_gflux_ngamma_tc = 617;
const int NORM_gflux_ngamma_tc_u = 618;
const int norm_gflux_ngamma_tc_u = 618;
const int NORM_gflux_tag_ratio = 619;
const int norm_gflux_tag_ratio = 619;
const int NORM_gflux_tag_ratio_u = 620;
const int norm_gflux_tag_ratio_u = 620;
const int NORM_gflux_norm_run = 621;
const int norm_gflux_norm_run = 621;
const int GPP_tagr_offset = 622;
const int gpp_tagr_offset = 622;
const int GPP_tagr_sigma = 623;
const int gpp_tagr_sigma = 623;
const int g6c_mom_corr_BEAM_value = 624;
const int g6m_BEAM_value = 624;
const int g6c_mom_corr_PIMINUS_value = 625;
const int g6m_PIMINUS_value = 625;
const int g6c_mom_corr_PIPLUS_value = 626;
const int g6m_PIPLUS_value = 626;
const int g6c_mom_corr_PROTON_value = 627;
const int g6m_PROTON_value = 627;
const int SC_CALIBRATIONS_swap_TDCi_left = 628;
const int sc_swap_TDCi_left = 628;
const int SC_CALIBRATIONS_swap_TDCi_right = 629;
const int sc_swap_TDCi_right = 629;
const int IC_status_status = 630;
const int ic_status_status = 630;
const int IC_Timing_Tch = 631;
const int ic_Timing_Tch = 631;
const int GPID_pion_low = 632;
const int gpid_pion_low = 632;
const int GPID_pion_high = 633;
const int gpid_pion_high = 633;
const int GPID_pion_dbeta = 634;
const int gpid_pion_dbeta = 634;
const int GPID_kaon_dbeta = 635;
const int gpid_kaon_dbeta = 635;
const int GPID_kaon_low = 636;
const int gpid_kaon_low = 636;
const int GPID_kaon_high = 637;
const int gpid_kaon_high = 637;
const int GPID_proton_high = 638;
const int gpid_proton_high = 638;
const int GPID_proton_low = 639;
const int gpid_proton_low = 639;
const int GPID_proton_dbeta = 640;
const int gpid_proton_dbeta = 640;
const int GPID_deuteron_dbeta = 641;
const int gpid_deuteron_dbeta = 641;
const int GPID_deuteron_high = 642;
const int gpid_deuteron_high = 642;
const int GPID_deuteron_low = 643;
const int gpid_deuteron_low = 643;
const int GPID_triton_low = 644;
const int gpid_triton_low = 644;
const int GPID_triton_high = 645;
const int gpid_triton_high = 645;
const int GPID_triton_dbeta = 646;
const int gpid_triton_dbeta = 646;
const int GPID_kpi_slope = 647;
const int gpid_kpi_slope = 647;
const int GPID_kpi_offset = 648;
const int gpid_kpi_offset = 648;
const int testSystem_testSubsystem_testItem = 649;
const int test_testSubsystem_testItem = 649;
const int IC_Energy_Gain0 = 650;
const int ic_Energy_Gain0 = 650;
const int IC_Energy_Pedestals = 651;
const int ic_Energy_Pedestals = 651;
const int IC_Timing_T0 = 652;
const int ic_Timing_T0 = 652;
const int IC_Timing_T1 = 653;
const int ic_Timing_T1 = 653;
const int GPP_DC_WIRE_sector1 = 654;
const int gpp_DC_WIRE_sector1 = 654;
const int GPP_DC_WIRE_sector2 = 655;
const int gpp_DC_WIRE_sector2 = 655;
const int GPP_DC_WIRE_sector3 = 656;
const int gpp_DC_WIRE_sector3 = 656;
const int GPP_DC_WIRE_sector4 = 657;
const int gpp_DC_WIRE_sector4 = 657;
const int GPP_DC_WIRE_sector5 = 658;
const int gpp_DC_WIRE_sector5 = 658;
const int GPP_DC_WIRE_sector6 = 659;
const int gpp_DC_WIRE_sector6 = 659;
const int IC_Geometry_nx = 660;
const int ic_Geometry_nx = 660;
const int IC_Geometry_ny = 661;
const int ic_Geometry_ny = 661;
const int IC_Geometry_stepx = 662;
const int ic_Geometry_stepx = 662;
const int IC_Geometry_stepy = 663;
const int ic_Geometry_stepy = 663;
const int IC_Geometry_x0 = 664;
const int ic_Geometry_x0 = 664;
const int IC_Geometry_y0 = 665;
const int ic_Geometry_y0 = 665;
const int IC_Geometry_z0 = 666;
const int ic_Geometry_z0 = 666;
const int IC_Energy_RMS = 667;
const int ic_Energy_RMS = 667;
const int COOKING_PARAMETERS_CURRENTS_torus = 668;
const int cook_CURRENTS_torus = 668;
const int COOKING_PARAMETERS_CURRENTS_mini = 669;
const int cook_CURRENTS_mini = 669;
const int COOKING_PARAMETERS_CURRENTS_poltarget = 670;
const int cook_CURRENTS_poltarget = 670;
const int COOKING_PARAMETERS_FILES_prlink = 671;
const int cook_FILES_prlink = 671;
const int COOKING_PARAMETERS_FILES_bgrid = 672;
const int cook_FILES_bgrid = 672;
const int COOKING_PARAMETERS_FILES_bin_path = 673;
const int cook_FILES_bin_path = 673;
const int COOKING_PARAMETERS_DB_runindex = 674;
const int cook_DB_runindex = 674;
const int COOKING_PARAMETERS_DB_timestamp = 675;
const int cook_DB_timestamp = 675;
const int GEOMETRY_targmagnet_position = 676;
const int geo_targmagnet_position = 676;
const int GEOMETRY_targmagnet_angles = 677;
const int geo_targmagnet_angles = 677;
const int GPID_kp_offset = 678;
const int gpid_kp_offset = 678;
const int GPID_kp_slope = 679;
const int gpid_kp_slope = 679;
const int GPID_st_st_min = 680;
const int gpid_st_st_min = 680;
const int testSystem_veff_leg = 681;
const int test_veff_leg = 681;
const int testSystem_M0_value = 682;
const int test_M0_value = 682;
const int testSystem_T0_value = 683;
const int test_T0_value = 683;
const int testSystem_T1_value = 684;
const int test_T1_value = 684;
const int testSystem_T2_value = 685;
const int test_T2_value = 685;
const int testSystem_W0_value = 686;
const int test_W0_value = 686;
const int testSystem_W1_value = 687;
const int test_W1_value = 687;
const int testSystem_W2_value = 688;
const int test_W2_value = 688;
const int testSystem_delta_T_p2p = 689;
const int test_delta_T_p2p = 689;
const int testSystem_delta_T_s2s = 690;
const int test_delta_T_s2s = 690;
const int testSystem_delta_T_st2tof = 691;
const int test_delta_T_st2tof = 691;
const int testSystem_pedestals_value = 692;
const int test_pedestals_value = 692;
const int testSystem_veff_nose = 693;
const int test_veff_nose = 693;
const int STN_CALIB_M0_value = 694;
const int stn_M0_value = 694;
const int STN_CALIB_T0_value = 695;
const int stn_T0_value = 695;
const int STN_CALIB_T1_value = 696;
const int stn_T1_value = 696;
const int STN_CALIB_T2_value = 697;
const int stn_T2_value = 697;
const int STN_CALIB_W0_value = 698;
const int stn_W0_value = 698;
const int STN_CALIB_W1_value = 699;
const int stn_W1_value = 699;
const int STN_CALIB_W2_value = 700;
const int stn_W2_value = 700;
const int NORM_gflux_stolen_window = 701;
const int norm_gflux_stolen_window = 701;
const int STN_CALIB_delta_T_s2s = 702;
const int stn_delta_T_s2s = 702;
const int STN_CALIB_delta_T_st2tof = 703;
const int stn_delta_T_st2tof = 703;
const int STN_CALIB_pedestals_value = 704;
const int stn_pedestals_value = 704;
const int STN_CALIB_veff_leg = 705;
const int stn_veff_leg = 705;
const int STN_CALIB_veff_nose = 706;
const int stn_veff_nose = 706;
const int STN_CALIB_delta_T_pd2pd = 707;
const int stn_delta_T_pd2pd = 707;
const int STN_CALIB_adc_max_value = 708;
const int stn_adc_max_value = 708;
const int STN_CALIB_veff_nose1 = 709;
const int stn_veff_nose1 = 709;
const int STN_CALIB_veff_nose2 = 710;
const int stn_veff_nose2 = 710;
const int STN_CALIB_TDC_min_value = 711;
const int stn_TDC_min_value = 711;
const int STN_CALIB_TDC_max_value = 712;
const int stn_TDC_max_value = 712;
const int TAG_CALIB_tag_t_use_tgtlr = 713;
const int tag_tag_t_use_tgtlr = 713;
const int CALL_CALIB_use_RF_id = 714;
const int call_use_RF_id = 714;
const int SC_CALIBRATIONS_T0_left = 715;
const int sc_T0_left = 715;
const int SC_CALIBRATIONS_T0_right = 716;
const int sc_T0_right = 716;
const int IC_Timing_sig = 717;
const int ic_Timing_sig = 717;
const int IC_Recon_ecentrmin = 718;
const int ic_Recon_ecentrmin = 718;
const int IC_Recon_ehitmin = 719;
const int ic_Recon_ehitmin = 719;
const int IC_Recon_deepZ = 720;
const int ic_Recon_deepZ = 720;
const int IC_Recon_w0 = 721;
const int ic_Recon_w0 = 721;
const int IC_Recon_eclustmin = 722;
const int ic_Recon_eclustmin = 722;
const int STN_CALIB_code_type_value = 723;
const int stn_code_type_value = 723;
const int IC_Timing_T2 = 724;
const int ic_Timing_T2 = 724;
const int STN_CALIB_adc2edep_reg1const = 725;
const int stn_adc2edep_reg1const = 725;
const int STN_CALIB_adc2edep_reg1scale = 726;
const int stn_adc2edep_reg1scale = 726;
const int STN_CALIB_adc2edep_reg1exp = 727;
const int stn_adc2edep_reg1exp = 727;
const int STN_CALIB_adc2edep_reg2par1 = 728;
const int stn_adc2edep_reg2par1 = 728;
const int STN_CALIB_adc2edep_reg2par2 = 729;
const int stn_adc2edep_reg2par2 = 729;
const int STN_CALIB_adc2edep_reg3par0 = 730;
const int stn_adc2edep_reg3par0 = 730;
const int STN_CALIB_adc2edep_reg3par1 = 731;
const int stn_adc2edep_reg3par1 = 731;
const int STN_CALIB_adc2edep_reg3par2 = 732;
const int stn_adc2edep_reg3par2 = 732;
const int STN_CALIB_adc2edep_reg1limit = 733;
const int stn_adc2edep_reg1limit = 733;
const int STN_CALIB_adc2edep_mip_energy = 734;
const int stn_adc2edep_mip_energy = 734;
const int GEOMETRY_st_bendRadius = 735;
const int geo_st_bendRadius = 735;
const int SC_CALIBRATIONS_version_flag = 736;
const int sc_version_flag = 736;
const int SC_CALIBRATIONS_V2_NMIP_ADC_left = 737;
const int scv2_NMIP_ADC_left = 737;
const int SC_CALIBRATIONS_V2_NMIP_ADC_right = 738;
const int scv2_NMIP_ADC_right = 738;
const int SC_CALIBRATIONS_V2_NMIP_ADCu_left = 739;
const int scv2_NMIP_ADCu_left = 739;
const int SC_CALIBRATIONS_V2_NMIP_ADCu_right = 740;
const int scv2_NMIP_ADCu_right = 740;
const int SC_CALIBRATIONS_V2_T0_TDC_left = 741;
const int scv2_T0_TDC_left = 741;
const int SC_CALIBRATIONS_V2_T0_TDC_right = 742;
const int scv2_T0_TDC_right = 742;
const int SC_CALIBRATIONS_V2_T0_TDCu_left = 743;
const int scv2_T0_TDCu_left = 743;
const int SC_CALIBRATIONS_V2_T0_TDCu_right = 744;
const int scv2_T0_TDCu_right = 744;
const int SC_CALIBRATIONS_V2_T1_left = 745;
const int scv2_T1_left = 745;
const int SC_CALIBRATIONS_V2_T1_right = 746;
const int scv2_T1_right = 746;
const int SC_CALIBRATIONS_V2_T1u_left = 747;
const int scv2_T1u_left = 747;
const int SC_CALIBRATIONS_V2_T1u_right = 748;
const int scv2_T1u_right = 748;
const int SC_CALIBRATIONS_V2_T2_left = 749;
const int scv2_T2_left = 749;
const int SC_CALIBRATIONS_V2_T2_right = 750;
const int scv2_T2_right = 750;
const int SC_CALIBRATIONS_V2_T2u_left = 751;
const int scv2_T2u_left = 751;
const int SC_CALIBRATIONS_V2_T2u_right = 752;
const int scv2_T2u_right = 752;
const int SC_CALIBRATIONS_V2_T_sigma_first_param = 753;
const int scv2_T_sigma_first_param = 753;
const int SC_CALIBRATIONS_V2_T_sigma_second_param = 754;
const int scv2_T_sigma_second_param = 754;
const int SC_CALIBRATIONS_V2_WALK0u_left = 755;
const int scv2_WALK0u_left = 755;
const int SC_CALIBRATIONS_V2_WALK0u_right = 756;
const int scv2_WALK0u_right = 756;
const int SC_CALIBRATIONS_V2_WALK1_left = 757;
const int scv2_WALK1_left = 757;
const int SC_CALIBRATIONS_V2_WALK1_right = 758;
const int scv2_WALK1_right = 758;
const int SC_CALIBRATIONS_V2_WALK1u_left = 759;
const int scv2_WALK1u_left = 759;
const int SC_CALIBRATIONS_V2_WALK1u_right = 760;
const int scv2_WALK1u_right = 760;
const int SC_CALIBRATIONS_V2_WALK2_left = 761;
const int scv2_WALK2_left = 761;
const int SC_CALIBRATIONS_V2_WALK2_right = 762;
const int scv2_WALK2_right = 762;
const int SC_CALIBRATIONS_V2_WALK2u_left = 763;
const int scv2_WALK2u_left = 763;
const int SC_CALIBRATIONS_V2_WALK2u_right = 764;
const int scv2_WALK2u_right = 764;
const int SC_CALIBRATIONS_V2_WALK_A0_left = 765;
const int scv2_WALK_A0_left = 765;
const int SC_CALIBRATIONS_V2_WALK_A0_right = 766;
const int scv2_WALK_A0_right = 766;
const int SC_CALIBRATIONS_V2_Yoffset_value = 767;
const int scv2_Yoffset_value = 767;
const int SC_CALIBRATIONS_V2_atten_length_left = 768;
const int scv2_atten_length_left = 768;
const int SC_CALIBRATIONS_V2_atten_length_right = 769;
const int scv2_atten_length_right = 769;
const int SC_CALIBRATIONS_V2_atten_u_left = 770;
const int scv2_atten_u_left = 770;
const int SC_CALIBRATIONS_V2_atten_u_right = 771;
const int scv2_atten_u_right = 771;
const int SC_CALIBRATIONS_V2_delta_T_coupled_paddle1 = 772;
const int scv2_delta_T_coupled_paddle1 = 772;
const int SC_CALIBRATIONS_V2_delta_T_coupled_paddle2 = 773;
const int scv2_delta_T_coupled_paddle2 = 773;
const int SC_CALIBRATIONS_V2_delta_T_dc2sc = 774;
const int scv2_delta_T_dc2sc = 774;
const int SC_CALIBRATIONS_V2_delta_T_left_right = 775;
const int scv2_delta_T_left_right = 775;
const int SC_CALIBRATIONS_V2_delta_T_paddle2paddle = 776;
const int scv2_delta_T_paddle2paddle = 776;
const int SC_CALIBRATIONS_V2_delta_T_panel2panel = 777;
const int scv2_delta_T_panel2panel = 777;
const int SC_CALIBRATIONS_V2_delta_Tu_left_right = 778;
const int scv2_delta_Tu_left_right = 778;
const int SC_CALIBRATIONS_V2_delta_Tu_paddle2paddle = 779;
const int scv2_delta_Tu_paddle2paddle = 779;
const int SC_CALIBRATIONS_V2_delta_Tu_panel2panel = 780;
const int scv2_delta_Tu_panel2panel = 780;
const int SC_CALIBRATIONS_V2_pedestals_left = 781;
const int scv2_pedestals_left = 781;
const int SC_CALIBRATIONS_V2_pedestals_right = 782;
const int scv2_pedestals_right = 782;
const int SC_CALIBRATIONS_V2_pedu_left = 783;
const int scv2_pedu_left = 783;
const int SC_CALIBRATIONS_V2_pedu_right = 784;
const int scv2_pedu_right = 784;
const int SC_CALIBRATIONS_V2_pulser_normalisation = 785;
const int scv2_pulser_normalisation = 785;
const int SC_CALIBRATIONS_V2_status_left = 786;
const int scv2_status_left = 786;
const int SC_CALIBRATIONS_V2_status_right = 787;
const int scv2_status_right = 787;
const int SC_CALIBRATIONS_V2_time_walk_ref_adc = 788;
const int scv2_time_walk_ref_adc = 788;
const int SC_CALIBRATIONS_V2_veff_left = 789;
const int scv2_veff_left = 789;
const int SC_CALIBRATIONS_V2_veff_right = 790;
const int scv2_veff_right = 790;
const int SC_CALIBRATIONS_V2_veffu_left = 791;
const int scv2_veffu_left = 791;
const int SC_CALIBRATIONS_V2_veffu_right = 792;
const int scv2_veffu_right = 792;
const int MAX_CALDB_TABLE = 793;
/// end of caldb items constants =====================================
enum const_type { ctype_unknown, ctype_float, ctype_int, ctype_char };

static struct caldb_table_t {
  int inx;
  int itemid;
  const char* system;
  const char* subsystem;
  const char* item;
  int length;
  const_type type;
} caldb_table_descr[MAX_CALDB_TABLE] = {
  {0, 1, "CALL_CALIB", "RFoffset", "rf2rf1Offset", 1, ctype_float },
  {1, 2, "CALL_CALIB", "T0", "unc", 10, ctype_float },
  {2, 3, "CALL_CALIB", "T0", "value", 10, ctype_float },
  {3, 4, "CALL_CALIB", "T1", "unc", 10, ctype_float },
  {4, 5, "CALL_CALIB", "T1", "value", 10, ctype_float },
  {5, 6, "CALL_CALIB", "T2", "unc", 10, ctype_float },
  {6, 7, "CALL_CALIB", "T2", "value", 10, ctype_float },
  {7, 8, "CALL_CALIB", "pedestals", "unc", 10, ctype_float },
  {8, 9, "CALL_CALIB", "pedestals", "value", 10, ctype_float },
  {9, 10, "CC_CALIB", "HV", "value", 216, ctype_float },
  {10, 11, "CC_CALIB", "TDC", "T0", 216, ctype_float },
  {11, 12, "CC_CALIB", "TDC", "T1", 216, ctype_float },
  {12, 13, "CC_CALIB", "pedestals", "mean", 216, ctype_float },
  {13, 14, "CC_CALIB", "pedestals", "sigma", 216, ctype_float },
  {14, 15, "CC_CALIB", "photoE", "amplitude", 216, ctype_float },
  {15, 29, "DC_DOCA", "RMS", "Region1", 50, ctype_float },
  {16, 30, "DC_DOCA", "RMS", "Region2", 50, ctype_float },
  {17, 31, "DC_DOCA", "RMS", "Region3", 50, ctype_float },
  {18, 32, "DC_DOCA", "Sigma", "Region1", 50, ctype_float },
  {19, 33, "DC_DOCA", "Sigma", "Region2", 50, ctype_float },
  {20, 34, "DC_DOCA", "Sigma", "Region3", 50, ctype_float },
  {21, 35, "DC_DOCA", "Timewalk", "Region1", 10, ctype_float },
  {22, 36, "DC_DOCA", "Timewalk", "Region2", 10, ctype_float },
  {23, 37, "DC_DOCA", "Timewalk", "Region3", 10, ctype_float },
  {24, 38, "DC_DOCA", "Timewalk", "betaslope", 6, ctype_float },
  {25, 39, "DC_DOCA", "Timewalk", "comment", 1024, ctype_char },
  {26, 40, "DC_DOCA", "Timewalk", "factor", 3, ctype_float },
  {27, 41, "DC_DOCA", "Timewalk", "functiontype", 1, ctype_int },
  {28, 42, "DC_DOCA", "Timewalk", "tau", 3, ctype_float },
  {29, 43, "DC_DOCA", "t_max", "Sector1", 36, ctype_float },
  {30, 44, "DC_DOCA", "t_max", "Sector2", 36, ctype_float },
  {31, 45, "DC_DOCA", "t_max", "Sector3", 36, ctype_float },
  {32, 46, "DC_DOCA", "t_max", "Sector4", 36, ctype_float },
  {33, 47, "DC_DOCA", "t_max", "Sector5", 36, ctype_float },
  {34, 48, "DC_DOCA", "t_max", "Sector6", 36, ctype_float },
  {35, 49, "DC_DOCA", "t_max", "comment", 1024, ctype_char },
  {36, 50, "DC_DOCA", "xvst_par_Sect2", "SL1", 24, ctype_float },
  {37, 51, "DC_DOCA", "xvst_par_Sect2", "SL2", 24, ctype_float },
  {38, 52, "DC_DOCA", "xvst_par_Sect2", "SL3", 24, ctype_float },
  {39, 53, "DC_DOCA", "xvst_par_Sect2", "SL4", 24, ctype_float },
  {40, 54, "DC_DOCA", "xvst_par_Sect2", "SL5", 24, ctype_float },
  {41, 55, "DC_DOCA", "xvst_par_Sect2", "SL6", 24, ctype_float },
  {42, 56, "DC_DOCA", "xvst_par_Sect2", "comment", 1024, ctype_char },
  {43, 57, "DC_DOCA", "xvst_par_Sect3", "SL1", 24, ctype_float },
  {44, 58, "DC_DOCA", "xvst_par_Sect3", "SL2", 24, ctype_float },
  {45, 59, "DC_DOCA", "xvst_par_Sect3", "SL3", 24, ctype_float },
  {46, 60, "DC_DOCA", "xvst_par_Sect3", "SL4", 24, ctype_float },
  {47, 61, "DC_DOCA", "xvst_par_Sect3", "SL5", 24, ctype_float },
  {48, 62, "DC_DOCA", "xvst_par_Sect3", "SL6", 24, ctype_float },
  {49, 63, "DC_DOCA", "xvst_par_Sect3", "comment", 1024, ctype_char },
  {50, 64, "DC_DOCA", "xvst_par_Sect4", "SL1", 24, ctype_float },
  {51, 65, "DC_DOCA", "xvst_par_Sect4", "SL2", 24, ctype_float },
  {52, 66, "DC_DOCA", "xvst_par_Sect4", "SL3", 24, ctype_float },
  {53, 67, "DC_DOCA", "xvst_par_Sect4", "SL4", 24, ctype_float },
  {54, 68, "DC_DOCA", "xvst_par_Sect4", "SL5", 24, ctype_float },
  {55, 69, "DC_DOCA", "xvst_par_Sect4", "SL6", 24, ctype_float },
  {56, 70, "DC_DOCA", "xvst_par_Sect4", "comment", 1024, ctype_char },
  {57, 71, "DC_DOCA", "xvst_par_Sect5", "SL1", 24, ctype_float },
  {58, 72, "DC_DOCA", "xvst_par_Sect5", "SL2", 24, ctype_float },
  {59, 73, "DC_DOCA", "xvst_par_Sect5", "SL3", 24, ctype_float },
  {60, 74, "DC_DOCA", "xvst_par_Sect5", "SL4", 24, ctype_float },
  {61, 75, "DC_DOCA", "xvst_par_Sect5", "SL5", 24, ctype_float },
  {62, 76, "DC_DOCA", "xvst_par_Sect5", "SL6", 24, ctype_float },
  {63, 77, "DC_DOCA", "xvst_par_Sect5", "comment", 1024, ctype_char },
  {64, 78, "DC_DOCA", "xvst_par_Sect6", "SL1", 24, ctype_float },
  {65, 79, "DC_DOCA", "xvst_par_Sect6", "SL2", 24, ctype_float },
  {66, 80, "DC_DOCA", "xvst_par_Sect6", "SL3", 24, ctype_float },
  {67, 81, "DC_DOCA", "xvst_par_Sect6", "SL4", 24, ctype_float },
  {68, 82, "DC_DOCA", "xvst_par_Sect6", "SL5", 24, ctype_float },
  {69, 83, "DC_DOCA", "xvst_par_Sect6", "SL6", 24, ctype_float },
  {70, 84, "DC_DOCA", "xvst_par_Sect6", "comment", 1024, ctype_char },
  {71, 85, "DC_DOCA", "xvst_params", "SL1", 24, ctype_float },
  {72, 86, "DC_DOCA", "xvst_params", "SL2", 24, ctype_float },
  {73, 87, "DC_DOCA", "xvst_params", "SL3", 24, ctype_float },
  {74, 88, "DC_DOCA", "xvst_params", "SL4", 24, ctype_float },
  {75, 89, "DC_DOCA", "xvst_params", "SL5", 24, ctype_float },
  {76, 90, "DC_DOCA", "xvst_params", "SL6", 24, ctype_float },
  {77, 91, "DC_DOCA", "xvst_params", "comment", 1024, ctype_char },
  {78, 903, "RF_OFFSETS", "correction", "value", 100, ctype_float },
  {79, 902, "beam", "fcup", "scale", 1, ctype_float },
  {80, 900, "TAG_CALIB", "tag_t", "dt_multihit", 61, ctype_float },
  {81, 901, "TAG_CALIB", "tag_e", "e2t", 1, ctype_float },
  {82, 870, "LAC_TIMING", "sector1", "d_l", 128, ctype_float },
  {83, 871, "LAC_TIMING", "sector1", "d_r", 128, ctype_float },
  {84, 867, "GPP", "DC", "Reg1_mean", 11, ctype_float },
  {85, 868, "GPP", "DC", "Reg2_mean", 11, ctype_float },
  {86, 869, "GPP", "DC", "Reg3_mean", 11, ctype_float },
  {87, 152, "DC_GEOM", "dc", "xalign", 18, ctype_float },
  {88, 153, "DC_GEOM", "dc", "xrot", 18, ctype_float },
  {89, 154, "DC_GEOM", "dc", "yalign", 18, ctype_float },
  {90, 155, "DC_GEOM", "dc", "yrot", 18, ctype_float },
  {91, 156, "DC_GEOM", "dc", "zalign", 18, ctype_float },
  {92, 157, "DC_GEOM", "dc", "zrot", 18, ctype_float },
  {93, 158, "DC_GEOM", "toroid", "align", 3, ctype_float },
  {94, 159, "DC_GEOM", "toroid", "rot", 3, ctype_float },
  {95, 160, "DC_STATUS", "sector1", "status", 6912, ctype_int },
  {96, 161, "DC_STATUS", "sector2", "status", 6912, ctype_int },
  {97, 162, "DC_STATUS", "sector3", "status", 6912, ctype_int },
  {98, 163, "DC_STATUS", "sector4", "status", 6912, ctype_int },
  {99, 164, "DC_STATUS", "sector5", "status", 6912, ctype_int },
  {100, 165, "DC_STATUS", "sector6", "status", 6912, ctype_int },
  {101, 166, "DC_SWAP", "swap_ids", "sector1", 32, ctype_int },
  {102, 167, "DC_SWAP", "swap_ids", "sector2", 32, ctype_int },
  {103, 168, "DC_SWAP", "swap_ids", "sector3", 32, ctype_int },
  {104, 169, "DC_SWAP", "swap_ids", "sector4", 32, ctype_int },
  {105, 170, "DC_SWAP", "swap_ids", "sector5", 32, ctype_int },
  {106, 171, "DC_SWAP", "swap_ids", "sector6", 32, ctype_int },
  {107, 172, "DC_SWAP", "swap_info", "bits", 6, ctype_int },
  {108, 173, "DC_TDLY", "Delays", "DC2TOF", 1, ctype_float },
  {109, 174, "DC_TDLY", "Delays", "Region4Pulser", 3, ctype_float },
  {110, 175, "DC_TDLY", "sector1", "T0", 6912, ctype_float },
  {111, 176, "DC_TDLY", "sector2", "T0", 6912, ctype_float },
  {112, 177, "DC_TDLY", "sector3", "T0", 6912, ctype_float },
  {113, 178, "DC_TDLY", "sector4", "T0", 6912, ctype_float },
  {114, 179, "DC_TDLY", "sector5", "T0", 6912, ctype_float },
  {115, 180, "DC_TDLY", "sector6", "T0", 6912, ctype_float },
  {116, 181, "DC_TIMEDIST", "SL1", "cuts", 3, ctype_float },
  {117, 182, "DC_TIMEDIST", "SL1", "dist", 40, ctype_float },
  {118, 183, "DC_TIMEDIST", "SL1", "time", 40, ctype_float },
  {119, 184, "DC_TIMEDIST", "SL2", "cuts", 3, ctype_float },
  {120, 185, "DC_TIMEDIST", "SL2", "dist", 40, ctype_float },
  {121, 186, "DC_TIMEDIST", "SL2", "time", 40, ctype_float },
  {122, 187, "DC_TIMEDIST", "SL3", "cuts", 3, ctype_float },
  {123, 188, "DC_TIMEDIST", "SL3", "dist", 40, ctype_float },
  {124, 189, "DC_TIMEDIST", "SL3", "time", 40, ctype_float },
  {125, 190, "DC_TIMEDIST", "SL4", "cuts", 3, ctype_float },
  {126, 191, "DC_TIMEDIST", "SL4", "dist", 40, ctype_float },
  {127, 192, "DC_TIMEDIST", "SL4", "time", 40, ctype_float },
  {128, 193, "DC_TIMEDIST", "SL5", "cuts", 3, ctype_float },
  {129, 194, "DC_TIMEDIST", "SL5", "dist", 40, ctype_float },
  {130, 195, "DC_TIMEDIST", "SL5", "time", 40, ctype_float },
  {131, 196, "DC_TIMEDIST", "SL6", "cuts", 3, ctype_float },
  {132, 197, "DC_TIMEDIST", "SL6", "dist", 40, ctype_float },
  {133, 198, "DC_TIMEDIST", "SL6", "time", 40, ctype_float },
  {134, 199, "DC_TIMEDIST", "region1", "x_vs_t_pars", 360, ctype_float },
  {135, 200, "DC_TIMEDIST", "region2", "x_vs_t_pars", 1008, ctype_float },
  {136, 201, "DC_TIMEDIST", "region3", "x_vs_t_pars", 360, ctype_float },
  {137, 202, "DC_TRANS", "trans", "sector1", 6912, ctype_int },
  {138, 203, "DC_TRANS", "trans", "sector2", 6912, ctype_int },
  {139, 204, "DC_TRANS", "trans", "sector3", 6912, ctype_int },
  {140, 205, "DC_TRANS", "trans", "sector4", 6912, ctype_int },
  {141, 206, "DC_TRANS", "trans", "sector5", 6912, ctype_int },
  {142, 207, "DC_TRANS", "trans", "sector6", 6912, ctype_int },
  {143, 217, "EC_CALIB", "EC_ATTEN", "InnerU", 216, ctype_float },
  {144, 218, "EC_CALIB", "EC_ATTEN", "InnerV", 216, ctype_float },
  {145, 219, "EC_CALIB", "EC_ATTEN", "InnerW", 216, ctype_float },
  {146, 220, "EC_CALIB", "EC_ATTEN", "OuterU", 216, ctype_float },
  {147, 221, "EC_CALIB", "EC_ATTEN", "OuterV", 216, ctype_float },
  {148, 222, "EC_CALIB", "EC_ATTEN", "OuterW", 216, ctype_float },
  {149, 223, "EC_CALIB", "EC_ATTEN", "WholeU", 216, ctype_float },
  {150, 224, "EC_CALIB", "EC_ATTEN", "WholeV", 216, ctype_float },
  {151, 225, "EC_CALIB", "EC_ATTEN", "WholeW", 216, ctype_float },
  {152, 226, "EC_CALIB", "EC_GAIN", "InnerU", 216, ctype_float },
  {153, 227, "EC_CALIB", "EC_GAIN", "InnerV", 216, ctype_float },
  {154, 228, "EC_CALIB", "EC_GAIN", "InnerW", 216, ctype_float },
  {155, 229, "EC_CALIB", "EC_GAIN", "OuterU", 216, ctype_float },
  {156, 230, "EC_CALIB", "EC_GAIN", "OuterV", 216, ctype_float },
  {157, 231, "EC_CALIB", "EC_GAIN", "OuterW", 216, ctype_float },
  {158, 232, "EC_CALIB", "EC_GAIN", "WholeU", 216, ctype_float },
  {159, 233, "EC_CALIB", "EC_GAIN", "WholeV", 216, ctype_float },
  {160, 234, "EC_CALIB", "EC_GAIN", "WholeW", 216, ctype_float },
  {161, 235, "EC_CALIB", "EC_PEDESTALS", "InnerU", 216, ctype_float },
  {162, 236, "EC_CALIB", "EC_PEDESTALS", "InnerV", 216, ctype_float },
  {163, 237, "EC_CALIB", "EC_PEDESTALS", "InnerW", 216, ctype_float },
  {164, 238, "EC_CALIB", "EC_PEDESTALS", "OuterU", 216, ctype_float },
  {165, 239, "EC_CALIB", "EC_PEDESTALS", "OuterV", 216, ctype_float },
  {166, 240, "EC_CALIB", "EC_PEDESTALS", "OuterW", 216, ctype_float },
  {167, 241, "EC_CALIB", "EC_PEDESTALS", "WholeU", 216, ctype_float },
  {168, 242, "EC_CALIB", "EC_PEDESTALS", "WholeV", 216, ctype_float },
  {169, 243, "EC_CALIB", "EC_PEDESTALS", "WholeW", 216, ctype_float },
  {170, 244, "EC_CALIB", "EC_Tadc", "InnerU", 216, ctype_float },
  {171, 245, "EC_CALIB", "EC_Tadc", "InnerV", 216, ctype_float },
  {172, 246, "EC_CALIB", "EC_Tadc", "InnerW", 216, ctype_float },
  {173, 247, "EC_CALIB", "EC_Tadc", "OuterU", 216, ctype_float },
  {174, 248, "EC_CALIB", "EC_Tadc", "OuterV", 216, ctype_float },
  {175, 249, "EC_CALIB", "EC_Tadc", "OuterW", 216, ctype_float },
  {176, 250, "EC_CALIB", "EC_Tch", "InnerU", 216, ctype_float },
  {177, 251, "EC_CALIB", "EC_Tch", "InnerV", 216, ctype_float },
  {178, 252, "EC_CALIB", "EC_Tch", "InnerW", 216, ctype_float },
  {179, 253, "EC_CALIB", "EC_Tch", "OuterU", 216, ctype_float },
  {180, 254, "EC_CALIB", "EC_Tch", "OuterV", 216, ctype_float },
  {181, 255, "EC_CALIB", "EC_Tch", "OuterW", 216, ctype_float },
  {182, 256, "EC_CALIB", "EC_To", "InnerU", 216, ctype_float },
  {183, 257, "EC_CALIB", "EC_To", "InnerV", 216, ctype_float },
  {184, 258, "EC_CALIB", "EC_To", "InnerW", 216, ctype_float },
  {185, 259, "EC_CALIB", "EC_To", "OuterU", 216, ctype_float },
  {186, 260, "EC_CALIB", "EC_To", "OuterV", 216, ctype_float },
  {187, 261, "EC_CALIB", "EC_To", "OuterW", 216, ctype_float },
  {188, 262, "EC_CALIB", "EC_Trms", "InnerU", 216, ctype_float },
  {189, 263, "EC_CALIB", "EC_Trms", "InnerV", 216, ctype_float },
  {190, 264, "EC_CALIB", "EC_Trms", "InnerW", 216, ctype_float },
  {191, 265, "EC_CALIB", "EC_Trms", "OuterU", 216, ctype_float },
  {192, 266, "EC_CALIB", "EC_Trms", "OuterV", 216, ctype_float },
  {193, 267, "EC_CALIB", "EC_Trms", "OuterW", 216, ctype_float },
  {194, 268, "EC_CALIB", "EC_dT1", "InnerU", 216, ctype_float },
  {195, 269, "EC_CALIB", "EC_dT1", "InnerV", 216, ctype_float },
  {196, 270, "EC_CALIB", "EC_dT1", "InnerW", 216, ctype_float },
  {197, 271, "EC_CALIB", "EC_dT1", "OuterU", 216, ctype_float },
  {198, 272, "EC_CALIB", "EC_dT1", "OuterV", 216, ctype_float },
  {199, 273, "EC_CALIB", "EC_dT1", "OuterW", 216, ctype_float },
  {200, 274, "EC_CALIB", "EC_dT2", "InnerU", 216, ctype_float },
  {201, 275, "EC_CALIB", "EC_dT2", "InnerV", 216, ctype_float },
  {202, 276, "EC_CALIB", "EC_dT2", "InnerW", 216, ctype_float },
  {203, 277, "EC_CALIB", "EC_dT2", "OuterU", 216, ctype_float },
  {204, 278, "EC_CALIB", "EC_dT2", "OuterV", 216, ctype_float },
  {205, 279, "EC_CALIB", "EC_dT2", "OuterW", 216, ctype_float },
  {206, 346, "E_T_MATRIX", "matrix", "contents", 2690, ctype_int },
  {207, 347, "GEOMETRY", "beam", "position", 3, ctype_float },
  {208, 348, "GEOMETRY", "fwd_carriage", "position", 3, ctype_float },
  {209, 349, "GEOMETRY", "fwd_carriage", "rotation", 3, ctype_float },
  {210, 350, "GEOMETRY", "north_clam", "position", 3, ctype_float },
  {211, 351, "GEOMETRY", "north_clam", "rotation", 3, ctype_float },
  {212, 352, "GEOMETRY", "south_clam", "position", 3, ctype_float },
  {213, 353, "GEOMETRY", "south_clam", "rotation", 3, ctype_float },
  {214, 354, "GEOMETRY", "space_frame_1", "position", 3, ctype_float },
  {215, 355, "GEOMETRY", "space_frame_1", "rotation", 3, ctype_float },
  {216, 356, "GEOMETRY", "space_frame_2", "position", 3, ctype_float },
  {217, 357, "GEOMETRY", "space_frame_2", "rotation", 3, ctype_float },
  {218, 358, "GEOMETRY", "space_frame_3", "position", 3, ctype_float },
  {219, 359, "GEOMETRY", "space_frame_3", "rotation", 3, ctype_float },
  {220, 360, "GEOMETRY", "space_frame_4", "position", 3, ctype_float },
  {221, 361, "GEOMETRY", "space_frame_4", "rotation", 3, ctype_float },
  {222, 362, "GEOMETRY", "space_frame_5", "position", 3, ctype_float },
  {223, 363, "GEOMETRY", "space_frame_5", "rotation", 3, ctype_float },
  {224, 364, "GEOMETRY", "space_frame_6", "position", 3, ctype_float },
  {225, 365, "GEOMETRY", "space_frame_6", "rotation", 3, ctype_float },
  {226, 366, "GPP", "DC", "Reg1", 11, ctype_float },
  {227, 367, "GPP", "DC", "Reg2", 11, ctype_float },
  {228, 368, "GPP", "DC", "Reg3", 11, ctype_float },
  {229, 369, "GPP", "SC", "Length", 48, ctype_float },
  {230, 370, "GPP", "SC", "Sigma", 48, ctype_float },
  {231, 371, "LAC_CALIB", "sector1", "c1l", 128, ctype_float },
  {232, 372, "LAC_CALIB", "sector1", "c1r", 128, ctype_float },
  {233, 373, "LAC_CALIB", "sector1", "c2l", 128, ctype_float },
  {234, 374, "LAC_CALIB", "sector1", "c2r", 128, ctype_float },
  {235, 375, "LAC_CALIB", "sector2", "c1l", 128, ctype_float },
  {236, 376, "LAC_CALIB", "sector2", "c1r", 128, ctype_float },
  {237, 377, "LAC_CALIB", "sector2", "c2l", 128, ctype_float },
  {238, 378, "LAC_CALIB", "sector2", "c2r", 128, ctype_float },
  {239, 379, "LAC_PEDESTALS", "sector1", "left", 128, ctype_float },
  {240, 380, "LAC_PEDESTALS", "sector1", "right", 128, ctype_float },
  {241, 381, "LAC_PEDESTALS", "sector2", "left", 128, ctype_float },
  {242, 382, "LAC_PEDESTALS", "sector2", "right", 128, ctype_float },
  {243, 383, "LAC_T0", "sector1", "t1l", 128, ctype_float },
  {244, 384, "LAC_T0", "sector1", "t1r", 128, ctype_float },
  {245, 385, "LAC_T0", "sector1", "t2l", 128, ctype_float },
  {246, 386, "LAC_T0", "sector1", "t2r", 128, ctype_float },
  {247, 387, "LAC_T0", "sector2", "t1l", 128, ctype_float },
  {248, 388, "LAC_T0", "sector2", "t1r", 128, ctype_float },
  {249, 389, "LAC_T0", "sector2", "t2l", 128, ctype_float },
  {250, 390, "LAC_T0", "sector2", "t2r", 128, ctype_float },
  {251, 391, "NEW_RUN_DATES", "runinfo", "date", 256, ctype_char },
  {252, 392, "NEW_RUN_DATES", "runinfo", "period", 256, ctype_char },
  {253, 393, "NORM", "PS_scalers", "delay", 64, ctype_float },
  {254, 394, "NORM", "PS_scalers", "norm", 64, ctype_float },
  {255, 395, "NORM", "PS_scalers", "prod", 64, ctype_float },
  {256, 396, "NORM", "PS_scalers", "ratio", 64, ctype_float },
  {257, 397, "NORM", "PS_scalers", "translation", 61, ctype_int },
  {258, 398, "NORM", "PS_scalers", "width_corr", 1, ctype_float },
  {259, 399, "NORM", "PS_scalers", "width_ratio", 64, ctype_float },
  {260, 400, "NORM", "normalization", "F_norm", 61, ctype_float },
  {261, 401, "NORM", "normalization", "F_norm_u", 61, ctype_float },
  {262, 402, "NORM", "normalization", "livetime", 1, ctype_float },
  {263, 403, "NORM", "normalization", "ngamma", 767, ctype_float },
  {264, 404, "NORM", "normalization", "prescale", 1, ctype_float },
  {265, 405, "NORM", "production", "livetime", 1, ctype_float },
  {266, 406, "NORM", "production", "pflux_ps", 767, ctype_float },
  {267, 407, "NORM", "production", "pflux_tscal", 767, ctype_float },
  {268, 408, "NORM", "production", "pflux_tscal_u", 767, ctype_float },
  {269, 409, "NORM", "production", "prescale", 1, ctype_float },
  {270, 410, "PRESCALE_CORR", "corr1", "value", 121, ctype_float },
  {271, 411, "PRESCALE_CORR", "corr2", "value", 121, ctype_float },
  {272, 412, "PRESCALE_CORR", "phase", "value", 61, ctype_float },
  {273, 413, "PRESCALE_CORR", "prescale", "value", 61, ctype_float },
  {274, 414, "RF_COR_F", "F1", "p0", 1, ctype_float },
  {275, 415, "RF_COR_F", "F1", "p1", 1, ctype_float },
  {276, 416, "RF_COR_F", "F1", "p2", 1, ctype_float },
  {277, 417, "RF_COR_F", "F2", "p0", 1, ctype_float },
  {278, 418, "RF_COR_F", "F2", "p1", 1, ctype_float },
  {279, 419, "RF_COR_F", "F2", "p2", 1, ctype_float },
  {280, 420, "RF_OFFSETS", "F1", "high", 1, ctype_float },
  {281, 421, "RF_OFFSETS", "F1", "low", 1, ctype_float },
  {282, 422, "RF_OFFSETS", "F1", "p0", 1, ctype_float },
  {283, 423, "RF_OFFSETS", "F1", "p1", 1, ctype_float },
  {284, 424, "RF_OFFSETS", "F1", "p2", 1, ctype_float },
  {285, 425, "RF_OFFSETS", "F1", "p3", 1, ctype_float },
  {286, 426, "RF_OFFSETS", "F2", "high", 1, ctype_float },
  {287, 427, "RF_OFFSETS", "F2", "low", 1, ctype_float },
  {288, 428, "RF_OFFSETS", "F2", "p0", 1, ctype_float },
  {289, 429, "RF_OFFSETS", "F2", "p1", 1, ctype_float },
  {290, 430, "RF_OFFSETS", "F2", "p2", 1, ctype_float },
  {291, 431, "RF_OFFSETS", "F2", "p3", 1, ctype_float },
  {292, 432, "RF_OFFSETS", "F3", "high", 1, ctype_float },
  {293, 433, "RF_OFFSETS", "F3", "low", 1, ctype_float },
  {294, 434, "RF_OFFSETS", "F3", "p0", 1, ctype_float },
  {295, 435, "RF_OFFSETS", "F3", "p1", 1, ctype_float },
  {296, 436, "RF_OFFSETS", "F3", "p2", 1, ctype_float },
  {297, 437, "RF_OFFSETS", "F3", "p3", 1, ctype_float },
  {298, 438, "RF_OFFSETS", "F4", "high", 1, ctype_float },
  {299, 439, "RF_OFFSETS", "F4", "low", 1, ctype_float },
  {300, 440, "RF_OFFSETS", "F4", "p0", 1, ctype_float },
  {301, 441, "RF_OFFSETS", "F4", "p1", 1, ctype_float },
  {302, 442, "RF_OFFSETS", "F4", "p2", 1, ctype_float },
  {303, 443, "RF_OFFSETS", "F4", "p3", 1, ctype_float },
  {304, 444, "RF_OFFSETS", "dead_range", "rf1_dead_high", 1, ctype_float },
  {305, 445, "RF_OFFSETS", "dead_range", "rf1_dead_low", 1, ctype_float },
  {306, 446, "RF_OFFSETS", "dead_range", "rf2_dead_high", 1, ctype_float },
  {307, 447, "RF_OFFSETS", "dead_range", "rf2_dead_low", 1, ctype_float },
  {308, 448, "RF_OFFSETS", "offset", "delta", 1, ctype_float },
  {309, 449, "RF_OFFSETS", "offset", "value", 1, ctype_float },
  {310, 450, "RF_OFFSETS", "rf_diff", "rf1_gt_rf2", 1, ctype_float },
  {311, 451, "RF_OFFSETS", "rf_diff", "rf1_lt_rf2", 1, ctype_float },
  {312, 452, "RF_OFFSETS", "rf_range", "high", 1, ctype_float },
  {313, 453, "RF_OFFSETS", "rf_range", "high2", 1, ctype_float },
  {314, 454, "RF_OFFSETS", "rf_range", "low", 1, ctype_float },
  {315, 455, "RF_OFFSETS", "rf_range", "low2", 1, ctype_float },
  {316, 456, "RF_OFFSETS", "rf_range", "rf1_high1", 1, ctype_float },
  {317, 457, "RF_OFFSETS", "rf_range", "rf1_high2", 1, ctype_float },
  {318, 458, "RF_OFFSETS", "rf_range", "rf1_low1", 1, ctype_float },
  {319, 459, "RF_OFFSETS", "rf_range", "rf1_low2", 1, ctype_float },
  {320, 460, "RF_OFFSETS", "rf_range", "rf2_high1", 1, ctype_float },
  {321, 461, "RF_OFFSETS", "rf_range", "rf2_high2", 1, ctype_float },
  {322, 462, "RF_OFFSETS", "rf_range", "rf2_low1", 1, ctype_float },
  {323, 463, "RF_OFFSETS", "rf_range", "rf2_low2", 1, ctype_float },
  {324, 464, "RF_OFFSETS", "status", "value", 1, ctype_int },
  {325, 465, "RUN_CONTROL", "Faraday_cup", "Q_active", 1, ctype_float },
  {326, 466, "RUN_CONTROL", "Faraday_cup", "Q_all", 1, ctype_float },
  {327, 467, "RUN_CONTROL", "Faraday_cup", "Q_live", 1, ctype_float },
  {328, 468, "RUN_CONTROL", "beam", "energy", 1, ctype_float },
  {329, 469, "RUN_CONTROL", "beam", "type", 1, ctype_int },
  {330, 470, "RUN_CONTROL", "beampol", "staterr", 1, ctype_float },
  {331, 471, "RUN_CONTROL", "beampol", "syserr", 1, ctype_float },
  {332, 472, "RUN_CONTROL", "beampol", "value", 1, ctype_float },
  {333, 473, "RUN_CONTROL", "currents", "minitorus", 1, ctype_float },
  {334, 474, "RUN_CONTROL", "currents", "pair_spec", 1, ctype_float },
  {335, 475, "RUN_CONTROL", "currents", "ptm", 1, ctype_float },
  {336, 476, "RUN_CONTROL", "currents", "tagger", 1, ctype_float },
  {337, 477, "RUN_CONTROL", "currents", "torus", 1, ctype_float },
  {338, 478, "RUN_CONTROL", "events", "raw", 1, ctype_int },
  {339, 479, "RUN_CONTROL", "roads", "link", 256, ctype_char },
  {340, 480, "RUN_CONTROL", "target", "mass", 1, ctype_float },
  {341, 481, "RUN_CONTROL", "target", "type", 1, ctype_int },
  {342, 482, "RUN_DATES", "date", "value", 256, ctype_char },
  {343, 483, "S1ST", "S1ST", "event_count", 1, ctype_int },
  {344, 484, "S1ST", "S1ST", "latch_bit1", 1, ctype_int },
  {345, 485, "S1ST", "S1ST", "latch_bit10", 1, ctype_int },
  {346, 486, "S1ST", "S1ST", "latch_bit11", 1, ctype_int },
  {347, 487, "S1ST", "S1ST", "latch_bit12", 1, ctype_int },
  {348, 488, "S1ST", "S1ST", "latch_bit2", 1, ctype_int },
  {349, 489, "S1ST", "S1ST", "latch_bit3", 1, ctype_int },
  {350, 490, "S1ST", "S1ST", "latch_bit4", 1, ctype_int },
  {351, 491, "S1ST", "S1ST", "latch_bit5", 1, ctype_int },
  {352, 492, "S1ST", "S1ST", "latch_bit6", 1, ctype_int },
  {353, 493, "S1ST", "S1ST", "latch_bit7", 1, ctype_int },
  {354, 494, "S1ST", "S1ST", "latch_bit8", 1, ctype_int },
  {355, 495, "S1ST", "S1ST", "latch_bit9", 1, ctype_int },
  {356, 496, "S1ST", "S1ST", "latch_empty", 1, ctype_int },
  {357, 497, "S1ST", "S1ST", "latch_not_empty", 1, ctype_int },
  {358, 498, "S1ST", "S1ST", "latch_zero", 1, ctype_int },
  {359, 499, "SCALERS", "TGS", "values", 192, ctype_int },
  {360, 500, "SCALERS", "TRGS", "values", 64, ctype_int },
  {361, 501, "SC_CALIBRATIONS", "NMIP_ADC", "left", 288, ctype_float },
  {362, 502, "SC_CALIBRATIONS", "NMIP_ADC", "right", 288, ctype_float },
  {363, 503, "SC_CALIBRATIONS", "NMIP_ADCu", "left", 288, ctype_float },
  {364, 504, "SC_CALIBRATIONS", "NMIP_ADCu", "right", 288, ctype_float },
  {365, 505, "SC_CALIBRATIONS", "T0_TDC", "left", 288, ctype_float },
  {366, 506, "SC_CALIBRATIONS", "T0_TDC", "right", 288, ctype_float },
  {367, 507, "SC_CALIBRATIONS", "T0_TDCu", "left", 288, ctype_float },
  {368, 508, "SC_CALIBRATIONS", "T0_TDCu", "right", 288, ctype_float },
  {369, 509, "SC_CALIBRATIONS", "T1", "left", 288, ctype_float },
  {370, 510, "SC_CALIBRATIONS", "T1", "right", 288, ctype_float },
  {371, 511, "SC_CALIBRATIONS", "T1u", "left", 288, ctype_float },
  {372, 512, "SC_CALIBRATIONS", "T1u", "right", 288, ctype_float },
  {373, 513, "SC_CALIBRATIONS", "T2", "left", 288, ctype_float },
  {374, 514, "SC_CALIBRATIONS", "T2", "right", 288, ctype_float },
  {375, 515, "SC_CALIBRATIONS", "T2u", "left", 288, ctype_float },
  {376, 516, "SC_CALIBRATIONS", "T2u", "right", 288, ctype_float },
  {377, 517, "SC_CALIBRATIONS", "T_sigma", "first_param", 288, ctype_float },
  {378, 518, "SC_CALIBRATIONS", "T_sigma", "second_param", 288, ctype_float },
  {379, 519, "SC_CALIBRATIONS", "WALK0u", "left", 288, ctype_float },
  {380, 520, "SC_CALIBRATIONS", "WALK0u", "right", 288, ctype_float },
  {381, 521, "SC_CALIBRATIONS", "WALK1", "left", 288, ctype_float },
  {382, 522, "SC_CALIBRATIONS", "WALK1", "right", 288, ctype_float },
  {383, 523, "SC_CALIBRATIONS", "WALK1u", "left", 288, ctype_float },
  {384, 524, "SC_CALIBRATIONS", "WALK1u", "right", 288, ctype_float },
  {385, 525, "SC_CALIBRATIONS", "WALK2", "left", 288, ctype_float },
  {386, 526, "SC_CALIBRATIONS", "WALK2", "right", 288, ctype_float },
  {387, 527, "SC_CALIBRATIONS", "WALK2u", "left", 288, ctype_float },
  {388, 528, "SC_CALIBRATIONS", "WALK2u", "right", 288, ctype_float },
  {389, 529, "SC_CALIBRATIONS", "WALK_A0", "left", 288, ctype_float },
  {390, 530, "SC_CALIBRATIONS", "WALK_A0", "right", 288, ctype_float },
  {391, 531, "SC_CALIBRATIONS", "Yoffset", "value", 288, ctype_float },
  {392, 532, "SC_CALIBRATIONS", "atten_length", "left", 288, ctype_float },
  {393, 533, "SC_CALIBRATIONS", "atten_length", "right", 288, ctype_float },
  {394, 534, "SC_CALIBRATIONS", "atten_u", "left", 288, ctype_float },
  {395, 535, "SC_CALIBRATIONS", "atten_u", "right", 288, ctype_float },
  {396, 536, "SC_CALIBRATIONS", "delta_T", "coupled_paddle1", 54, ctype_float },
  {397, 537, "SC_CALIBRATIONS", "delta_T", "coupled_paddle2", 54, ctype_float },
  {398, 538, "SC_CALIBRATIONS", "delta_T", "dc2sc", 24, ctype_float },
  {399, 539, "SC_CALIBRATIONS", "delta_T", "left_right", 288, ctype_float },
  {400, 540, "SC_CALIBRATIONS", "delta_T", "paddle2paddle", 288, ctype_float },
  {401, 541, "SC_CALIBRATIONS", "delta_T", "panel2panel", 24, ctype_float },
  {402, 542, "SC_CALIBRATIONS", "delta_Tu", "left_right", 288, ctype_float },
  {403, 543, "SC_CALIBRATIONS", "delta_Tu", "paddle2paddle", 288, ctype_float },
  {404, 544, "SC_CALIBRATIONS", "delta_Tu", "panel2panel", 24, ctype_float },
  {405, 545, "SC_CALIBRATIONS", "pedestals", "left", 288, ctype_float },
  {406, 546, "SC_CALIBRATIONS", "pedestals", "right", 288, ctype_float },
  {407, 547, "SC_CALIBRATIONS", "pedu", "left", 288, ctype_float },
  {408, 548, "SC_CALIBRATIONS", "pedu", "right", 288, ctype_float },
  {409, 549, "SC_CALIBRATIONS", "pulser", "normalisation", 1, ctype_float },
  {410, 550, "SC_CALIBRATIONS", "status", "left", 288, ctype_int },
  {411, 551, "SC_CALIBRATIONS", "status", "right", 288, ctype_int },
  {412, 552, "SC_CALIBRATIONS", "time_walk", "ref_adc", 1, ctype_float },
  {413, 553, "SC_CALIBRATIONS", "veff", "left", 288, ctype_float },
  {414, 554, "SC_CALIBRATIONS", "veff", "right", 288, ctype_float },
  {415, 555, "SC_CALIBRATIONS", "veffu", "left", 288, ctype_float },
  {416, 556, "SC_CALIBRATIONS", "veffu", "right", 288, ctype_float },
  {417, 665, "ST_CALIB", "LAMBDA", "unc", 6, ctype_float },
  {418, 666, "ST_CALIB", "LAMBDA", "value", 6, ctype_float },
  {419, 667, "ST_CALIB", "M0", "unc", 6, ctype_float },
  {420, 668, "ST_CALIB", "M0", "value", 6, ctype_float },
  {421, 669, "ST_CALIB", "T0", "unc", 6, ctype_float },
  {422, 670, "ST_CALIB", "T0", "value", 6, ctype_float },
  {423, 671, "ST_CALIB", "T1", "unc", 6, ctype_float },
  {424, 672, "ST_CALIB", "T1", "value", 6, ctype_float },
  {425, 673, "ST_CALIB", "T2", "unc", 6, ctype_float },
  {426, 674, "ST_CALIB", "T2", "value", 6, ctype_float },
  {427, 675, "ST_CALIB", "W0", "unc", 6, ctype_float },
  {428, 676, "ST_CALIB", "W0", "value", 6, ctype_float },
  {429, 677, "ST_CALIB", "W1", "unc", 6, ctype_float },
  {430, 678, "ST_CALIB", "W1", "value", 6, ctype_float },
  {431, 679, "ST_CALIB", "W2", "unc", 6, ctype_float },
  {432, 680, "ST_CALIB", "W2", "value", 6, ctype_float },
  {433, 681, "ST_CALIB", "delta_T", "pair2pair", 3, ctype_float },
  {434, 682, "ST_CALIB", "delta_T", "side2side", 3, ctype_float },
  {435, 683, "ST_CALIB", "delta_T", "unc", 6, ctype_float },
  {436, 684, "ST_CALIB", "delta_T", "value", 6, ctype_float },
  {437, 685, "ST_CALIB", "delta_Tu", "side2side", 3, ctype_float },
  {438, 686, "ST_CALIB", "pedestals", "unc", 6, ctype_float },
  {439, 687, "ST_CALIB", "pedestals", "value", 6, ctype_float },
  {440, 688, "ST_CALIB", "st2tof", "unc", 1, ctype_float },
  {441, 689, "ST_CALIB", "st2tof", "value", 1, ctype_float },
  {442, 690, "ST_CALIB", "veff_leg", "far", 6, ctype_float },
  {443, 691, "ST_CALIB", "veff_leg", "near", 6, ctype_float },
  {444, 692, "ST_CALIB", "veff_leg", "unc", 6, ctype_float },
  {445, 693, "ST_CALIB", "veff_leg", "value", 6, ctype_float },
  {446, 694, "ST_CALIB", "veff_nose", "far", 6, ctype_float },
  {447, 695, "ST_CALIB", "veff_nose", "near", 6, ctype_float },
  {448, 696, "ST_CALIB", "veff_nose", "unc", 6, ctype_float },
  {449, 697, "ST_CALIB", "veff_nose", "value", 6, ctype_float },
  {450, 698, "TAG_CALIB", "pc", "peak", 1, ctype_float },
  {451, 699, "TAG_CALIB", "pc", "ped", 6, ctype_float },
  {452, 700, "TAG_CALIB", "pc", "tdc_w", 1, ctype_float },
  {453, 701, "TAG_CALIB", "ps", "Ethresh", 1, ctype_float },
  {454, 702, "TAG_CALIB", "ps", "peak", 8, ctype_float },
  {455, 703, "TAG_CALIB", "ps", "ped", 8, ctype_float },
  {456, 704, "TAG_CALIB", "ps", "tdc_w", 8, ctype_float },
  {457, 705, "TAG_CALIB", "ps", "walk", 3, ctype_float },
  {458, 706, "TAG_CALIB", "status", "tage", 384, ctype_int },
  {459, 707, "TAG_CALIB", "status", "tagt_left", 61, ctype_int },
  {460, 708, "TAG_CALIB", "status", "tagt_right", 61, ctype_int },
  {461, 709, "TAG_CALIB", "tac", "Escale", 7, ctype_float },
  {462, 710, "TAG_CALIB", "tac", "peak", 1, ctype_float },
  {463, 711, "TAG_CALIB", "tac", "ped", 7, ctype_float },
  {464, 712, "TAG_CALIB", "tac", "tdc_w", 1, ctype_float },
  {465, 713, "TAG_CALIB", "tag2tof", "value", 1, ctype_float },
  {466, 714, "TAG_CALIB", "tag_e", "dt", 384, ctype_float },
  {467, 715, "TAG_CALIB", "tag_e", "slope", 1, ctype_float },
  {468, 716, "TAG_CALIB", "tag_t", "ci", 121, ctype_float },
  {469, 717, "TAG_CALIB", "tag_t", "do_translation", 1, ctype_int },
  {470, 718, "TAG_CALIB", "tag_t", "dt_left", 61, ctype_float },
  {471, 719, "TAG_CALIB", "tag_t", "dt_right", 61, ctype_float },
  {472, 720, "TAG_CALIB", "tag_t", "slope_left", 61, ctype_float },
  {473, 721, "TAG_CALIB", "tag_t", "slope_right", 61, ctype_float },
  {474, 722, "TAG_CALIB", "tag_t", "transl_to_table", 122, ctype_int },
  {475, 723, "TAG_SCALER", "G1SL", "value", 64, ctype_int },
  {476, 724, "TAG_SCALER", "G2SL", "value", 64, ctype_int },
  {477, 725, "TAG_SCALER", "G3SL", "value", 64, ctype_int },
  {478, 726, "TAG_SCALER", "G4SL", "value", 64, ctype_int },
  {479, 727, "TAG_SCALER", "PCEff", "value", 64, ctype_float },
  {480, 728, "TAG_SCALER", "PSEff", "value", 64, ctype_float },
  {481, 729, "TAG_SCALER", "RTSL", "value", 64, ctype_int },
  {482, 730, "TAG_SCALER", "TagEff", "value", 64, ctype_float },
  {483, 731, "TAG_SCALER", "gate", "value", 1, ctype_int },
  {484, 732, "g6normalisation", "GENERAL", "PSAccCorr", 61, ctype_float },
  {485, 733, "g6normalisation", "GENERAL", "T_for_E", 767, ctype_int },
  {486, 734, "g6normalisation", "GENERAL", "width_ratio", 61, ctype_float },
  {487, 735, "g6normalisation", "NORMALISATION", "NGamma", 767, ctype_float },
  {488, 736, "g6normalisation", "NORMALISATION", "NGammaT", 61, ctype_float },
  {489, 737, "g6normalisation", "NORMALISATION", "PC", 61, ctype_float },
  {490, 738, "g6normalisation", "NORMALISATION", "PS", 61, ctype_float },
  {491, 739, "g6normalisation", "NORMALISATION", "RAWT", 61, ctype_float },
  {492, 740, "g6normalisation", "NORMALISATION", "TAC", 61, ctype_float },
  {493, 741, "g6normalisation", "NORMALISATION", "livetime", 1, ctype_float },
  {494, 742, "g6normalisation", "NORMALISATION", "prescale", 1, ctype_int },
  {495, 743, "g6normalisation", "PRODUCTION", "G1SL", 61, ctype_float },
  {496, 744, "g6normalisation", "PRODUCTION", "G2SL", 61, ctype_float },
  {497, 745, "g6normalisation", "PRODUCTION", "G3SL", 61, ctype_float },
  {498, 746, "g6normalisation", "PRODUCTION", "G4SL", 61, ctype_float },
  {499, 747, "g6normalisation", "PRODUCTION", "NEvent", 1, ctype_int },
  {500, 748, "g6normalisation", "PRODUCTION", "NGammaE", 767, ctype_float },
  {501, 749, "g6normalisation", "PRODUCTION", "NGammaE_2", 767, ctype_float },
  {502, 750, "g6normalisation", "PRODUCTION", "NGammaT", 61, ctype_float },
  {503, 751, "g6normalisation", "PRODUCTION", "NGammaT_2", 61, ctype_float },
  {504, 752, "g6normalisation", "PRODUCTION", "RTSL", 61, ctype_float },
  {505, 753, "g6normalisation", "PRODUCTION", "livetime", 1, ctype_float },
  {506, 754, "g6normalisation", "PRODUCTION", "prescale", 1, ctype_int },
  {507, 755, "g_post_cal", "norm", "et_1", 767, ctype_float },
  {508, 756, "g_post_cal", "norm", "et_10", 767, ctype_float },
  {509, 757, "g_post_cal", "norm", "et_11", 767, ctype_float },
  {510, 758, "g_post_cal", "norm", "et_12", 767, ctype_float },
  {511, 759, "g_post_cal", "norm", "et_13", 767, ctype_float },
  {512, 760, "g_post_cal", "norm", "et_14", 767, ctype_float },
  {513, 761, "g_post_cal", "norm", "et_15", 767, ctype_float },
  {514, 762, "g_post_cal", "norm", "et_16", 767, ctype_float },
  {515, 763, "g_post_cal", "norm", "et_17", 767, ctype_float },
  {516, 764, "g_post_cal", "norm", "et_18", 767, ctype_float },
  {517, 765, "g_post_cal", "norm", "et_19", 767, ctype_float },
  {518, 766, "g_post_cal", "norm", "et_2", 767, ctype_float },
  {519, 767, "g_post_cal", "norm", "et_20", 767, ctype_float },
  {520, 768, "g_post_cal", "norm", "et_21", 767, ctype_float },
  {521, 769, "g_post_cal", "norm", "et_22", 767, ctype_float },
  {522, 770, "g_post_cal", "norm", "et_23", 767, ctype_float },
  {523, 771, "g_post_cal", "norm", "et_24", 767, ctype_float },
  {524, 772, "g_post_cal", "norm", "et_25", 767, ctype_float },
  {525, 773, "g_post_cal", "norm", "et_26", 767, ctype_float },
  {526, 774, "g_post_cal", "norm", "et_27", 767, ctype_float },
  {527, 775, "g_post_cal", "norm", "et_28", 767, ctype_float },
  {528, 776, "g_post_cal", "norm", "et_29", 767, ctype_float },
  {529, 777, "g_post_cal", "norm", "et_3", 767, ctype_float },
  {530, 778, "g_post_cal", "norm", "et_30", 767, ctype_float },
  {531, 779, "g_post_cal", "norm", "et_31", 767, ctype_float },
  {532, 780, "g_post_cal", "norm", "et_32", 767, ctype_float },
  {533, 781, "g_post_cal", "norm", "et_33", 767, ctype_float },
  {534, 782, "g_post_cal", "norm", "et_34", 767, ctype_float },
  {535, 783, "g_post_cal", "norm", "et_35", 767, ctype_float },
  {536, 784, "g_post_cal", "norm", "et_36", 767, ctype_float },
  {537, 785, "g_post_cal", "norm", "et_37", 767, ctype_float },
  {538, 786, "g_post_cal", "norm", "et_38", 767, ctype_float },
  {539, 787, "g_post_cal", "norm", "et_39", 767, ctype_float },
  {540, 788, "g_post_cal", "norm", "et_4", 767, ctype_float },
  {541, 789, "g_post_cal", "norm", "et_40", 767, ctype_float },
  {542, 790, "g_post_cal", "norm", "et_41", 767, ctype_float },
  {543, 791, "g_post_cal", "norm", "et_42", 767, ctype_float },
  {544, 792, "g_post_cal", "norm", "et_43", 767, ctype_float },
  {545, 793, "g_post_cal", "norm", "et_44", 767, ctype_float },
  {546, 794, "g_post_cal", "norm", "et_45", 767, ctype_float },
  {547, 795, "g_post_cal", "norm", "et_46", 767, ctype_float },
  {548, 796, "g_post_cal", "norm", "et_47", 767, ctype_float },
  {549, 797, "g_post_cal", "norm", "et_48", 767, ctype_float },
  {550, 798, "g_post_cal", "norm", "et_49", 767, ctype_float },
  {551, 799, "g_post_cal", "norm", "et_5", 767, ctype_float },
  {552, 800, "g_post_cal", "norm", "et_50", 767, ctype_float },
  {553, 801, "g_post_cal", "norm", "et_51", 767, ctype_float },
  {554, 802, "g_post_cal", "norm", "et_52", 767, ctype_float },
  {555, 803, "g_post_cal", "norm", "et_53", 767, ctype_float },
  {556, 804, "g_post_cal", "norm", "et_54", 767, ctype_float },
  {557, 805, "g_post_cal", "norm", "et_55", 767, ctype_float },
  {558, 806, "g_post_cal", "norm", "et_56", 767, ctype_float },
  {559, 807, "g_post_cal", "norm", "et_57", 767, ctype_float },
  {560, 808, "g_post_cal", "norm", "et_58", 767, ctype_float },
  {561, 809, "g_post_cal", "norm", "et_59", 767, ctype_float },
  {562, 810, "g_post_cal", "norm", "et_6", 767, ctype_float },
  {563, 811, "g_post_cal", "norm", "et_60", 767, ctype_float },
  {564, 812, "g_post_cal", "norm", "et_61", 767, ctype_float },
  {565, 813, "g_post_cal", "norm", "et_7", 767, ctype_float },
  {566, 814, "g_post_cal", "norm", "et_8", 767, ctype_float },
  {567, 815, "g_post_cal", "norm", "et_9", 767, ctype_float },
  {568, 816, "g_post_cal", "norm", "t_cal", 121, ctype_float },
  {569, 817, "g_post_cal", "norm", "t_kill", 121, ctype_int },
  {570, 818, "g_post_cal", "prod", "t_cal", 121, ctype_float },
  {571, 819, "g_post_cal", "prod", "t_kill", 121, ctype_int },
  {572, 820, "mom_corr", "phi_func", "sector1", 5, ctype_float },
  {573, 821, "mom_corr", "phi_func", "sector2", 5, ctype_float },
  {574, 822, "mom_corr", "phi_func", "sector3", 5, ctype_float },
  {575, 823, "mom_corr", "phi_func", "sector4", 5, ctype_float },
  {576, 824, "mom_corr", "phi_func", "sector5", 5, ctype_float },
  {577, 825, "mom_corr", "phi_func", "sector6", 5, ctype_float },
  {578, 826, "mom_corr", "theta_func", "parms", 3, ctype_float },
  {579, 827, "mom_corr", "theta_func", "sector1", 3, ctype_float },
  {580, 828, "mom_corr", "theta_func", "sector2", 3, ctype_float },
  {581, 829, "mom_corr", "theta_func", "sector3", 3, ctype_float },
  {582, 830, "mom_corr", "theta_func", "sector4", 3, ctype_float },
  {583, 831, "mom_corr", "theta_func", "sector5", 3, ctype_float },
  {584, 832, "mom_corr", "theta_func", "sector6", 3, ctype_float },
  {585, 833, "LAC_TIMING", "sector1", "c_in_stack", 128, ctype_float },
  {586, 834, "LAC_TIMING", "sector1", "c_l", 128, ctype_float },
  {587, 835, "LAC_TIMING", "sector1", "c_r", 128, ctype_float },
  {588, 836, "LAC_TIMING", "sector1", "tw_al", 128, ctype_float },
  {589, 837, "LAC_TIMING", "sector1", "tw_ar", 128, ctype_float },
  {590, 838, "LAC_TIMING", "sector1", "tw_bl", 128, ctype_float },
  {591, 839, "LAC_TIMING", "sector1", "tw_br", 128, ctype_float },
  {592, 840, "LAC_TIMING", "sector2", "c_in_stack", 128, ctype_float },
  {593, 841, "LAC_TIMING", "sector2", "c_l", 128, ctype_float },
  {594, 842, "LAC_TIMING", "sector2", "c_r", 128, ctype_float },
  {595, 843, "LAC_TIMING", "sector2", "d_l", 128, ctype_float },
  {596, 844, "LAC_TIMING", "sector2", "d_r", 128, ctype_float },
  {597, 845, "LAC_TIMING", "sector2", "tw_al", 128, ctype_float },
  {598, 846, "LAC_TIMING", "sector2", "tw_ar", 128, ctype_float },
  {599, 847, "LAC_TIMING", "sector2", "tw_bl", 128, ctype_float },
  {600, 848, "LAC_TIMING", "sector2", "tw_br", 128, ctype_float },
  {601, 849, "GEOMETRY", "target", "position", 3, ctype_float },
  {602, 850, "GEOMETRY", "st", "aperture", 6, ctype_float },
  {603, 851, "GEOMETRY", "st", "dist2bl", 6, ctype_float },
  {604, 852, "GEOMETRY", "st", "legVolume", 18, ctype_float },
  {605, 853, "GEOMETRY", "st", "noseAngle", 6, ctype_float },
  {606, 854, "GEOMETRY", "st", "noseVolume", 24, ctype_float },
  {607, 855, "GEOMETRY", "st", "offset", 18, ctype_float },
  {608, 856, "GEOMETRY", "st", "thickness", 6, ctype_float },
  {609, 904, "NORM", "photon_atten", "value", 1, ctype_float },
  {610, 905, "NORM", "photon_atten", "error", 1, ctype_float },
  {611, 906, "NORM", "gflux", "begin_t_window", 1, ctype_float },
  {612, 907, "NORM", "gflux", "end_t_window", 1, ctype_float },
  {613, 908, "NORM", "gflux", "ngamma_25mev", 100, ctype_float },
  {614, 909, "NORM", "gflux", "ngamma_25mev_u", 100, ctype_float },
  {615, 910, "NORM", "gflux", "ngamma_eb", 767, ctype_float },
  {616, 911, "NORM", "gflux", "ngamma_eb_u", 767, ctype_float },
  {617, 912, "NORM", "gflux", "ngamma_tc", 61, ctype_float },
  {618, 913, "NORM", "gflux", "ngamma_tc_u", 61, ctype_float },
  {619, 914, "NORM", "gflux", "tag_ratio", 61, ctype_float },
  {620, 915, "NORM", "gflux", "tag_ratio_u", 61, ctype_float },
  {621, 916, "NORM", "gflux", "norm_run", 1, ctype_int },
  {622, 917, "GPP", "tagr", "offset", 1, ctype_float },
  {623, 918, "GPP", "tagr", "sigma", 1, ctype_float },
  {624, 919, "g6c_mom_corr", "BEAM", "value", 1, ctype_float },
  {625, 920, "g6c_mom_corr", "PIMINUS", "value", 36, ctype_float },
  {626, 921, "g6c_mom_corr", "PIPLUS", "value", 36, ctype_float },
  {627, 922, "g6c_mom_corr", "PROTON", "value", 36, ctype_float },
  {628, 923, "SC_CALIBRATIONS", "swap_TDCi", "left", 288, ctype_int },
  {629, 924, "SC_CALIBRATIONS", "swap_TDCi", "right", 288, ctype_int },
  {630, 973, "IC", "status", "status", 529, ctype_int },
  {631, 972, "IC", "Timing", "Tch", 529, ctype_float },
  {632, 949, "GPID", "pion", "low", 50, ctype_float },
  {633, 950, "GPID", "pion", "high", 50, ctype_float },
  {634, 951, "GPID", "pion", "dbeta", 1, ctype_float },
  {635, 952, "GPID", "kaon", "dbeta", 1, ctype_float },
  {636, 953, "GPID", "kaon", "low", 50, ctype_float },
  {637, 954, "GPID", "kaon", "high", 50, ctype_float },
  {638, 955, "GPID", "proton", "high", 50, ctype_float },
  {639, 956, "GPID", "proton", "low", 50, ctype_float },
  {640, 957, "GPID", "proton", "dbeta", 1, ctype_float },
  {641, 958, "GPID", "deuteron", "dbeta", 1, ctype_float },
  {642, 959, "GPID", "deuteron", "high", 50, ctype_float },
  {643, 960, "GPID", "deuteron", "low", 50, ctype_float },
  {644, 961, "GPID", "triton", "low", 50, ctype_float },
  {645, 962, "GPID", "triton", "high", 50, ctype_float },
  {646, 963, "GPID", "triton", "dbeta", 1, ctype_float },
  {647, 964, "GPID", "kpi", "slope", 1, ctype_float },
  {648, 965, "GPID", "kpi", "offset", 1, ctype_float },
  {649, 966, "testSystem", "testSubsystem", "testItem", 10, ctype_float },
  {650, 968, "IC", "Energy", "Gain0", 529, ctype_float },
  {651, 969, "IC", "Energy", "Pedestals", 529, ctype_float },
  {652, 970, "IC", "Timing", "T0", 529, ctype_float },
  {653, 971, "IC", "Timing", "T1", 529, ctype_float },
  {654, 943, "GPP", "DC_WIRE", "sector1", 6912, ctype_float },
  {655, 944, "GPP", "DC_WIRE", "sector2", 6912, ctype_float },
  {656, 945, "GPP", "DC_WIRE", "sector3", 6912, ctype_float },
  {657, 946, "GPP", "DC_WIRE", "sector4", 6912, ctype_float },
  {658, 947, "GPP", "DC_WIRE", "sector5", 6912, ctype_float },
  {659, 948, "GPP", "DC_WIRE", "sector6", 6912, ctype_float },
  {660, 978, "IC", "Geometry", "nx", 1, ctype_int },
  {661, 979, "IC", "Geometry", "ny", 1, ctype_int },
  {662, 980, "IC", "Geometry", "stepx", 1, ctype_float },
  {663, 981, "IC", "Geometry", "stepy", 1, ctype_float },
  {664, 982, "IC", "Geometry", "x0", 1, ctype_float },
  {665, 983, "IC", "Geometry", "y0", 1, ctype_float },
  {666, 984, "IC", "Geometry", "z0", 1, ctype_float },
  {667, 985, "IC", "Energy", "RMS", 529, ctype_float },
  {668, 986, "COOKING_PARAMETERS", "CURRENTS", "torus", 1, ctype_float },
  {669, 987, "COOKING_PARAMETERS", "CURRENTS", "mini", 1, ctype_float },
  {670, 988, "COOKING_PARAMETERS", "CURRENTS", "poltarget", 1, ctype_float },
  {671, 989, "COOKING_PARAMETERS", "FILES", "prlink", 64, ctype_char },
  {672, 990, "COOKING_PARAMETERS", "FILES", "bgrid", 64, ctype_char },
  {673, 991, "COOKING_PARAMETERS", "FILES", "bin_path", 255, ctype_char },
  {674, 992, "COOKING_PARAMETERS", "DB", "runindex", 32, ctype_char },
  {675, 993, "COOKING_PARAMETERS", "DB", "timestamp", 255, ctype_char },
  {676, 994, "GEOMETRY", "targmagnet", "position", 3, ctype_float },
  {677, 995, "GEOMETRY", "targmagnet", "angles", 3, ctype_float },
  {678, 996, "GPID", "kp", "offset", 1, ctype_float },
  {679, 997, "GPID", "kp", "slope", 1, ctype_float },
  {680, 998, "GPID", "st", "st_min", 1, ctype_float },
  {681, 999, "testSystem", "veff", "leg", 24, ctype_float },
  {682, 1000, "testSystem", "M0", "value", 24, ctype_float },
  {683, 1001, "testSystem", "T0", "value", 24, ctype_float },
  {684, 1002, "testSystem", "T1", "value", 24, ctype_float },
  {685, 1003, "testSystem", "T2", "value", 24, ctype_float },
  {686, 1004, "testSystem", "W0", "value", 24, ctype_float },
  {687, 1005, "testSystem", "W1", "value", 24, ctype_float },
  {688, 1006, "testSystem", "W2", "value", 24, ctype_float },
  {689, 1007, "testSystem", "delta_T", "p2p", 4, ctype_float },
  {690, 1008, "testSystem", "delta_T", "s2s", 6, ctype_float },
  {691, 1009, "testSystem", "delta_T", "st2tof", 1, ctype_float },
  {692, 1010, "testSystem", "pedestals", "value", 24, ctype_float },
  {693, 1011, "testSystem", "veff", "nose", 24, ctype_float },
  {694, 1012, "STN_CALIB", "M0", "value", 24, ctype_float },
  {695, 1013, "STN_CALIB", "T0", "value", 24, ctype_float },
  {696, 1014, "STN_CALIB", "T1", "value", 24, ctype_float },
  {697, 1015, "STN_CALIB", "T2", "value", 24, ctype_float },
  {698, 1016, "STN_CALIB", "W0", "value", 24, ctype_float },
  {699, 1017, "STN_CALIB", "W1", "value", 24, ctype_float },
  {700, 1018, "STN_CALIB", "W2", "value", 24, ctype_float },
  {701, 1026, "NORM", "gflux", "stolen_window", 61, ctype_float },
  {702, 1020, "STN_CALIB", "delta_T", "s2s", 6, ctype_float },
  {703, 1021, "STN_CALIB", "delta_T", "st2tof", 1, ctype_float },
  {704, 1022, "STN_CALIB", "pedestals", "value", 24, ctype_float },
  {705, 1023, "STN_CALIB", "veff", "leg", 24, ctype_float },
  {706, 1024, "STN_CALIB", "veff", "nose", 24, ctype_float },
  {707, 1025, "STN_CALIB", "delta_T", "pd2pd", 24, ctype_float },
  {708, 1027, "STN_CALIB", "adc_max", "value", 24, ctype_float },
  {709, 1028, "STN_CALIB", "veff", "nose1", 24, ctype_float },
  {710, 1029, "STN_CALIB", "veff", "nose2", 24, ctype_float },
  {711, 1030, "STN_CALIB", "TDC_min", "value", 1, ctype_float },
  {712, 1031, "STN_CALIB", "TDC_max", "value", 1, ctype_float },
  {713, 1032, "TAG_CALIB", "tag_t", "use_tgtlr", 1, ctype_int },
  {714, 1033, "CALL_CALIB", "use_RF", "id", 1, ctype_int },
  {715, 1034, "SC_CALIBRATIONS", "T0", "left", 288, ctype_float },
  {716, 1035, "SC_CALIBRATIONS", "T0", "right", 288, ctype_float },
  {717, 1036, "IC", "Timing", "sig", 529, ctype_float },
  {718, 1045, "IC", "Recon", "ecentrmin", 1, ctype_float },
  {719, 1044, "IC", "Recon", "ehitmin", 1, ctype_float },
  {720, 1043, "IC", "Recon", "deepZ", 1, ctype_float },
  {721, 1042, "IC", "Recon", "w0", 1, ctype_float },
  {722, 1046, "IC", "Recon", "eclustmin", 1, ctype_float },
  {723, 1047, "STN_CALIB", "code_type", "value", 1, ctype_int },
  {724, 1048, "IC", "Timing", "T2", 529, ctype_float },
  {725, 1049, "STN_CALIB", "adc2edep", "reg1const", 24, ctype_float },
  {726, 1050, "STN_CALIB", "adc2edep", "reg1scale", 24, ctype_float },
  {727, 1051, "STN_CALIB", "adc2edep", "reg1exp", 24, ctype_float },
  {728, 1052, "STN_CALIB", "adc2edep", "reg2par1", 24, ctype_float },
  {729, 1053, "STN_CALIB", "adc2edep", "reg2par2", 24, ctype_float },
  {730, 1054, "STN_CALIB", "adc2edep", "reg3par0", 24, ctype_float },
  {731, 1055, "STN_CALIB", "adc2edep", "reg3par1", 24, ctype_float },
  {732, 1056, "STN_CALIB", "adc2edep", "reg3par2", 24, ctype_float },
  {733, 1057, "STN_CALIB", "adc2edep", "reg1limit", 24, ctype_float },
  {734, 1058, "STN_CALIB", "adc2edep", "mip_energy", 1, ctype_float },
  {735, 1059, "GEOMETRY", "st", "bendRadius", 6, ctype_float },
  {736, 1060, "SC_CALIBRATIONS", "version", "flag", 1, ctype_int },
  {737, 1061, "SC_CALIBRATIONS_V2", "NMIP_ADC", "left", 342, ctype_float },
  {738, 1062, "SC_CALIBRATIONS_V2", "NMIP_ADC", "right", 342, ctype_float },
  {739, 1063, "SC_CALIBRATIONS_V2", "NMIP_ADCu", "left", 342, ctype_float },
  {740, 1064, "SC_CALIBRATIONS_V2", "NMIP_ADCu", "right", 342, ctype_float },
  {741, 1065, "SC_CALIBRATIONS_V2", "T0_TDC", "left", 342, ctype_float },
  {742, 1066, "SC_CALIBRATIONS_V2", "T0_TDC", "right", 342, ctype_float },
  {743, 1067, "SC_CALIBRATIONS_V2", "T0_TDCu", "left", 342, ctype_float },
  {744, 1068, "SC_CALIBRATIONS_V2", "T0_TDCu", "right", 342, ctype_float },
  {745, 1069, "SC_CALIBRATIONS_V2", "T1", "left", 342, ctype_float },
  {746, 1070, "SC_CALIBRATIONS_V2", "T1", "right", 342, ctype_float },
  {747, 1071, "SC_CALIBRATIONS_V2", "T1u", "left", 342, ctype_float },
  {748, 1072, "SC_CALIBRATIONS_V2", "T1u", "right", 342, ctype_float },
  {749, 1073, "SC_CALIBRATIONS_V2", "T2", "left", 342, ctype_float },
  {750, 1074, "SC_CALIBRATIONS_V2", "T2", "right", 342, ctype_float },
  {751, 1075, "SC_CALIBRATIONS_V2", "T2u", "left", 342, ctype_float },
  {752, 1076, "SC_CALIBRATIONS_V2", "T2u", "right", 342, ctype_float },
  {753, 1077, "SC_CALIBRATIONS_V2", "T_sigma", "first_param", 342, ctype_float },
  {754, 1078, "SC_CALIBRATIONS_V2", "T_sigma", "second_param", 342, ctype_float },
  {755, 1079, "SC_CALIBRATIONS_V2", "WALK0u", "left", 342, ctype_float },
  {756, 1080, "SC_CALIBRATIONS_V2", "WALK0u", "right", 342, ctype_float },
  {757, 1081, "SC_CALIBRATIONS_V2", "WALK1", "left", 342, ctype_float },
  {758, 1082, "SC_CALIBRATIONS_V2", "WALK1", "right", 342, ctype_float },
  {759, 1083, "SC_CALIBRATIONS_V2", "WALK1u", "left", 342, ctype_float },
  {760, 1084, "SC_CALIBRATIONS_V2", "WALK1u", "right", 342, ctype_float },
  {761, 1085, "SC_CALIBRATIONS_V2", "WALK2", "left", 342, ctype_float },
  {762, 1086, "SC_CALIBRATIONS_V2", "WALK2", "right", 342, ctype_float },
  {763, 1087, "SC_CALIBRATIONS_V2", "WALK2u", "left", 342, ctype_float },
  {764, 1088, "SC_CALIBRATIONS_V2", "WALK2u", "right", 342, ctype_float },
  {765, 1089, "SC_CALIBRATIONS_V2", "WALK_A0", "left", 342, ctype_float },
  {766, 1090, "SC_CALIBRATIONS_V2", "WALK_A0", "right", 342, ctype_float },
  {767, 1091, "SC_CALIBRATIONS_V2", "Yoffset", "value", 342, ctype_float },
  {768, 1092, "SC_CALIBRATIONS_V2", "atten_length", "left", 342, ctype_float },
  {769, 1093, "SC_CALIBRATIONS_V2", "atten_length", "right", 342, ctype_float },
  {770, 1094, "SC_CALIBRATIONS_V2", "atten_u", "left", 342, ctype_float },
  {771, 1095, "SC_CALIBRATIONS_V2", "atten_u", "right", 342, ctype_float },
  {772, 1096, "SC_CALIBRATIONS_V2", "delta_T", "coupled_paddle1", 54, ctype_float },
  {773, 1097, "SC_CALIBRATIONS_V2", "delta_T", "coupled_paddle2", 54, ctype_float },
  {774, 1098, "SC_CALIBRATIONS_V2", "delta_T", "dc2sc", 24, ctype_float },
  {775, 1099, "SC_CALIBRATIONS_V2", "delta_T", "left_right", 342, ctype_float },
  {776, 1100, "SC_CALIBRATIONS_V2", "delta_T", "paddle2paddle", 342, ctype_float },
  {777, 1101, "SC_CALIBRATIONS_V2", "delta_T", "panel2panel", 24, ctype_float },
  {778, 1102, "SC_CALIBRATIONS_V2", "delta_Tu", "left_right", 342, ctype_float },
  {779, 1103, "SC_CALIBRATIONS_V2", "delta_Tu", "paddle2paddle", 342, ctype_float },
  {780, 1104, "SC_CALIBRATIONS_V2", "delta_Tu", "panel2panel", 24, ctype_float },
  {781, 1105, "SC_CALIBRATIONS_V2", "pedestals", "left", 342, ctype_float },
  {782, 1106, "SC_CALIBRATIONS_V2", "pedestals", "right", 342, ctype_float },
  {783, 1107, "SC_CALIBRATIONS_V2", "pedu", "left", 342, ctype_float },
  {784, 1108, "SC_CALIBRATIONS_V2", "pedu", "right", 342, ctype_float },
  {785, 1109, "SC_CALIBRATIONS_V2", "pulser", "normalisation", 1, ctype_float },
  {786, 1110, "SC_CALIBRATIONS_V2", "status", "left", 342, ctype_int },
  {787, 1111, "SC_CALIBRATIONS_V2", "status", "right", 342, ctype_int },
  {788, 1112, "SC_CALIBRATIONS_V2", "time_walk", "ref_adc", 1, ctype_float },
  {789, 1113, "SC_CALIBRATIONS_V2", "veff", "left", 342, ctype_float },
  {790, 1114, "SC_CALIBRATIONS_V2", "veff", "right", 342, ctype_float },
  {791, 1115, "SC_CALIBRATIONS_V2", "veffu", "left", 342, ctype_float },
  {792, 1116, "SC_CALIBRATIONS_V2", "veffu", "right", 342, ctype_float },
};

#endif
