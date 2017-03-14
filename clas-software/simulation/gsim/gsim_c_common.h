/* FORTRAN COMMONs for GSIM (non-GEANT commons.)*/

#define MAX_DETECTOR (17)

enum DETECTOR_ELEMENT  {ALL,CC,DC,EC,EC1,SC,SH,ST,TORU,MINI,FOIL,PTG,TG,TG2,IC,SOL,OTHER};

/* ------ Start of file gsimpar.inc */

/* Defines */ 

#define CLAS_DOC ("CLAS_DOC")
#define CLAS_PARMS ("CLAS_PARMS")
#define CLAS_ROOT ("CLAS_ROOT")
#define GSIM_BGRIDPTG_ENV ("GSIM_BGRIDPTG")
#define GSIM_BGRIDPTG_FLAG ("bptg")
#define GSIM_BGRIDPTG_NAME ("bgrid_ptg.fpk")
#define GSIM_BGRID_NAME ("bgrid.fpk")
#define GSIM_BOSOUT_ENV ("GSIM_BOSOUT")
#define GSIM_BOSOUT_FLAG ("bo")
#define GSIM_BOSOUT_NAME ("gsimout.A00")
#define GSIM_ECATTEN_ENV ("GSIM_ECATTEN")
#define GSIM_ECATTEN_FLAG ("ec")
#define GSIM_ECATTEN_NAME ("ec_atten.dat")
#define GSIM_FFREAD_ENV ("GSIM_FFREAD")
#define GSIM_FFREAD_FLAG ("ff")
#define GSIM_FFREAD_NAME ("ffread.txt")
#define GSIM_GEOM_ENV ("GSIM_GEOM")
#define GSIM_GEOM_FLAG ("ge")
#define GSIM_GEOM_NAME ("gsimgeom.bfb")
#define GSIM_HELP_FILENAME ("gsim.hlp")
#define GSIM_KINE_FLAG ("k")
#define GSIM_LIZIN_ENV ("GSIM_LIZIN")
#define GSIM_LIZIN_FLAG ("li")
#define GSIM_LIZIN_NAME ("lizin.txt")
#define GSIM_LUNDIN_ENV ("GSIM_LUNDIN")
#define GSIM_LUNDIN_FLAG ("lu")
#define GSIM_LUNDIN_NAME ("lundin.dat")
#define GSIM_MCIN_ENV ("GSIM_MCIN")
#define GSIM_MCIN_FLAG ("mc")
#define GSIM_MCIN_NAME ("mcin.evt")
#define GSIM_MGRID_ENV ("GSIM_MGRID")
#define GSIM_MGRID_FLAG ("mg")
#define GSIM_MGRID_NAME ("bgrid_m.fpk")
#define GSIM_NOBOSOUT_FLAG ("nob")
#define GSIM_NODATA_FLAG ("noda")
#define GSIM_NODIGI_FLAG ("nodi")
#define GSIM_NOFFREAD_FLAG ("noff")
#define GSIM_NOFIELD_FLAG ("nofi")
#define GSIM_NOGEOM_FLAG ("nog")
#define GSIM_NOHITS_FLAG ("noh")
#define GSIM_NOMATE_FLAG ("noma")
#define GSIM_NOMCDATA_FLAG ("nomc")
#define GSIM_NOSEC_FLAG ("nos")
#define GSIM_TGRID_ENV ("GSIM_TGRID")
#define GSIM_TGRID_FLAG ("tg")
#define GSIM_TGRID_NAME ("bgrid_t.fpk")
#define GSIM_TRIG_FLAG ("t")
#define GSIM_SKIP_FLAG ("sk")
#define GSIM_NOACC_FLAG ("noacc")
#define MAX_BGRID_KEY (6)
#define MAX_VTX (40)

/* End of Defines */ 

/* ------ End of file gsimpar.inc */

/* ------ Start of file bgrid.inc */

/* Defines */ 

#define MAX_GRID (653000)
#define MAX_GRID_PTG (2000000)

/* End of Defines */ 

typedef struct { /*  bgrid */ 
  int nx;
  int ny;
  int nz;
  float xmin;
  float ymin;
  float zmin;
  float dx;
  float dy;
  float dz;
  int c_max;
  int dtime;
  int ngr;
  float qfield[MAX_GRID];
}BGRID_t ; 

typedef struct { /*  bgrid_ptg */ 
  int nx_ptg;
  int ny_ptg;
  int nz_ptg;
  float xmin_ptg;
  float ymin_ptg;
  float zmin_ptg;
  float dx_ptg;
  float dy_ptg;
  float dz_ptg;
  int c_max_ptg;
  int dtime_ptg;
  int ngr_ptg;
  float qptg[MAX_GRID_PTG];
}BGRID_PTG_t ; 

/* ------ End of file bgrid.inc */

/* ------ Start of file cccom.inc */

/* Defines */ 

#define MBOS_CCH (5000)
#define MCCHIT (400)
#define MCCSEGM (36)
#define MGUIDL (199)
#define MHTRK (50)

/* End of Defines */ 

typedef struct { /*  KCGUID */ 
  char guid[MGUIDL][80];
}KCGUID_t ; 

typedef struct { /*  CCNUMB */ 
  int mat_cc_gas;
  int mat_mirr_cc;
  int mat_sh1;
  int mat_sh2;
  int med_cc_gas;
  int med_mirr_cc;
  int med_sh1;
  int med_sh2;
}CCNUMB_t ; 

typedef struct { /*  CCHIT1 */ 
  int jhtrk;
  int itrk_num[MHTRK];
  int idh_prt[MHTRK];
  int ihitrk[MHTRK];
  int itrsect[MHTRK];
  int   itrsegm[MHTRK];
  float ph_tot[MHTRK];
  float xenter[MHTRK][3];
  float xexit[MHTRK][3];
}CCHIT1_t ; 

typedef struct { /*  CCHIT2 */ 
  int icchit;
  int itrnum[MCCHIT];
  int iccsect[MCCHIT];
  int iccsegm[MCCHIT];
  int   icctdc[MCCHIT];
}CCHIT2_t ; 

typedef struct { /*  CCJHIT */ 
  int jhit_cc[MBOS_CCH];
}CCJHIT_t ; 

typedef struct { /*  CCCALB */ 
  int kcciden[MCCSEGM][6];
  float xcct2ch[MCCSEGM][6];
  float opheamp[MCCSEGM][6];
  float dpheamp[MCCSEGM][6];
  float ccpedes[MCCSEGM][6];
  float acctime[MCCSEGM][6];
  float dcctime[MCCSEGM][6];
  int kccpmtn[MCCSEGM][6];
  float rccsens[MCCSEGM][6];
  int   kccrunn[MCCSEGM][6];
}CCCALB_t ; 

/* ------ End of file cccom.inc */

/* ------ Start of file ccdig.inc */

/* Defines */ 

#define CM_TO_CH (0.667128190396)
#define MAX_N_HIT_CC (100)

/* End of Defines */ 

typedef struct { /*  CCdig */ 
  int n_hits_cc;
  int n_sect_cc[MAX_N_HIT_CC];
  int n_cc_hit[MAX_N_HIT_CC];
  int i_tdc_cc[MAX_N_HIT_CC];
  int i_adc_cc[MAX_N_HIT_CC];
  int i_track_n[MAX_N_HIT_CC];
}CCDIG_t ; 

/* ------ End of file ccdig.inc */

/* ------ Start of file ccfrmp.inc */

/* Defines */ 

#define MIRRN (99)
#define NWCON (25)

/* End of Defines */ 

typedef struct { /*  CCFRMP */ 
  float xell[18][MIRRN];
  float yell[18][MIRRN];
  float xhyp[18][MIRRN];
  float yhyp[18][MIRRN];
  float xw1[18][NWCON];
  float yw1[18][NWCON];
  float xw2[18][NWCON];
  float yw2[18][NWCON];
  float xcms[18][4][4];
  float ycms[18][4][4];
  float zcms[18][4][4];
}CCFRMP_t ; 

/* ------ End of file ccfrmp.inc */

/* ------ Start of file ccp.inc */

typedef struct { /*  CCP */ 
  float pe0[18][5];
  float ph0[18][5];
  float pw0[18][3];
  float wc0[18][3];
  float pmt0[18][3];
  float wcr[18];
  float wcer[18];
  float pmtr[18];
  float dplwc[18];
  float wcang[18];
  float p00[3];
  float p10[18][3];
  float p20[18][3];
  float p30[18][3];
  float p40[18][3];
  float p50[18][3];
  float p60[18][3];
  float p70[18][3];
  float p80[18][3];
  float p90[18][3];
  float pa0[18][3];
  float x11[18];
  float y11[18];
  float x12[18];
  float y12[18];
  float x21[18];
  float y21[18];
  float x22[18];
  float y22[18];
  float xp1[18];
  float yp1[18];
  float xp2[18];
  float yp2[18];
  float csa0[18];
  float sw0[18][3];
  float th[18];
  float thmin[19];
  float x_pl[18];
  float elt_pl[18][3];
  float hwcpmt[18];
  float pwc10[18][3];
  float pwc20[18][3];
  float hwde[18];
  float hwdh[18];
  float cmsx1[18][3];
  float cmsx2[18][3];
  float hmssz[18][3];
  float plms0[18][4][3];
  float pcms0[18][6][3];
  float plmsw[18][3];
  float scrnp1[18][3];
  float scrnp2[18][3];
}CCP_t ; 

typedef struct { /*  CCPW */ 
  float pe[5];
  float ph[5];
  float pw[3];
  float p1[3];
  float p2[3];
  float p3[3];
  float p4[3];
  float p5[3];
  float p6[3];
  float p7[3];
  float p8[3];
  float p9[3];
  float pa[3];
  float pe_pl[3];
  float wcc[3];
  float sw[3];
  float csa;
  float xe_pl;
  float pwc1[3];
  float pwc2[3];
  float plms[4][3];
  float pcms[4][4][3];
  float scrp1[3];
  float scrp2[3];
}CCPW_t ; 

/* ------ End of file ccp.inc */

/* ------ Start of file ccph.inc */

/* Defines */ 

#define MIG (200)
#define MJTR (50)
#define MREF (100)
#define MSTP (100)
#define STPMIN (5.0)

/* End of Defines */ 

typedef struct { /*  CCPH */ 
  int jtr;
  int ig;
  int jtro;
  int igo;
  int ihnc[MIG];
  int ihsc[MIG];
  int nref[MIG];
  int njtr[MIG];
  float tdc[MIG];
  int nstp[MJTR];
  int nvsc[MJTR];
  int nvch[MJTR];
  int nvtype[MJTR];
  float vphi[MJTR];
  float vteta[MJTR];
  float vlen[MJTR];
  int ncl[MIG][MREF];
  float rl[MIG][MREF][3];
  float rtr[MJTR][MSTP][3];
  float vpmom[MJTR][MSTP];
}CCPH_t ; 

typedef struct { /*  CCINPR */ 
  int in_part0;
  float v0teta;
  float v0phi;
  int in_part;
  int nsc_in;
  int ncc_in;
  float xtr_in[3];
  float amom_in[3];
}CCINPR_t ; 

/* ------ End of file ccph.inc */

/* ------ Start of file cctest.inc */

typedef struct { /*  CCTEST */ 
  int icctst[10];
}CCTEST_t ; 

/* ------ End of file cctest.inc */

/* ------ Start of file clmate.inc */

typedef struct { /*  clmate */ 
  int imate_liq_h2;
  int imate_liq_d2;
  int imate_liq_he3;
  int imate_liq_he4;
  int imate_liq_n2;
  int imate_liq_neon;
  int imate_helium;
  int imate_air;
  int imate_vacuum;
  int imate_lithium;
  int imate_beryllium;
  int imate_carbon;
  int imate_aluminum;
  int imate_iron;
  int imate_copper;
  int imate_tungsten;
  int imate_lead;
  int imate_uranium;
  int imate_stainless_steel;
  int imate_ch2;
  int imate_tio2;
  int imate_polystyrene;
  int imate_polyethylene;
  int imate_cerex;
  int imate_mylar;
  int imate_kapton;
  int imate_alumylar;       /* aluminized mylar (9:1)             */
  int imate_alumaseal;      /* mylar/aluminum/mylar/glue compound */
  int imate_comp_alumaseal; /* kapton/alumaseal/epoxy compound    */
  int imate_cerexalu1;      /* cerex/alu compound (27:35)         */
  int imate_cerexalu2;      /* cerex/alu compound (15:14)         */
  int imate_cerexmyl1;       /* cerex/alum.mylar compound (25:22)  */
  int imate_lowdfoam;       /* low density foam for scattering chamber */
  int imate_carb_fiber;      /*carbon fiber/epoxy */
  int imate_butanol;
  int imate_nh3;
  int imate_nd3;
  int imate_titanium;
  int imate_rigidgraphite;
  int imate_flexgraphite;  
}CLMATE_t ; 

/* ------ End of file clmate.inc */

/* ------ Start of file cltmed.inc */

typedef struct { /*  cltmed */ 
  int clas_med_air;
  int clas_med_helium;
  int clas_med_mylar;
  int clas_med_carbon;
  int clas_med_vac;
  int clas_med_beryl;
  int clas_med_alu;
  int clas_med_lead;
  int clas_med_copper;
  int clas_med_h2liq;
  int clas_med_iron;
  int clas_med_ch2;
  int clas_med_helium4;
  int clas_med_d2liq;
  int clas_med_alumylar;
  int clas_med_roha;
  int clas_med_kapton;
  int clas_med_cerexmyl1;
  int clas_med_lowdfoam;
  int clas_med_alumaseal;
  int clas_med_cfiber;
  int clas_med_cerex;
  int clas_med_titanium;
  int clas_med_rigidgraphite;
  int clas_med_flexgraphite;
  int clas_med_tungsten;  
  int clas_med_stainless_steel;
}CLTMED_t ; 

/* ------ End of file cltmed.inc */

/* ------ Start of file dcgeom.inc */

/* Defines */ 

#define NCH (36)
#define KCH (48)  /* =NCH+12 =dc-layers including guard wire planes*/
#define NDX (30)
#define NLAY (6)
#define NSUP (6)

/* End of Defines */ 

typedef struct { /*  dcgeom */ 
  float ch_r[KCH];
  float ch_phimin[KCH];
  float ch_dphi[KCH];
  float ch_nwire[KCH];
  float ch_ast[KCH];
  float ch_dmax[KCH];
  float ch_offset[KCH][3];
  float ch_middir[KCH][3];
  float ch_wpmid[KCH][194][3];
  float ch_wpend[KCH][194][6];
  float ch_wdir[KCH][194][3];
  float ch_plane[KCH];
  float ch_dlen[KCH];
  int ch_nwpln[KCH];
  float ch_d[KCH];
  float ch_plx[KCH];
  float ch_ortvec[KCH][3];
  float ch_spacz[3];
  float ch_nwmin[NCH];
  float ch_nwmax[NCH];
}DCGEOM_t ; 

/* ------ End of file dcgeom.inc */

/* ------ Start of file dcmate.inc */

typedef struct { /*  dcmate */ 
  int dc_mat_dcgas;
}DCMATE_t ; 

/* ------ End of file dcmate.inc */

/* ------ Start of file dcpar.inc */

/* Defines */ 

#define NDC_SL (6)
#define NDDC (4)
#define NHDC (12)
#define NVDC (3)

/* End of Defines */ 

/* ------ End of file dcpar.inc */

/* ------ Start of file dcsets.inc */

typedef struct { /*  dcsets */ 
  int idtype_dc;
  int set_dc;
  int det_dc[6];
  int det_dcst[2];
}DCSETS_t ; 

/* ------ End of file dcsets.inc */

/* ------ Start of file dctmed.inc */

typedef struct { /*  dctmed */ 
  int dc_med_dcgas;
  int dc_med_air;
  int dc_med_helium;
}DCTMED_t ; 

/* ------ End of file dctmed.inc */

/* ------ Start of file ec1dep.inc */

typedef struct { /*  EC1DEP */ 
  float esdsc;
  float esdpb;
  float esdfe;
  float esdai;
  float esdsc1;
  float esdsc2;
}EC1DEP_t ; 

/* ------ End of file ec1dep.inc */

/* ------ Start of file ec1geom.inc */

typedef struct { /*  LACGEOM */ 
  float hs1;
  float hs2;
  float hb1;
  float hb2;
  float pbsthi;
  float scsthi;
  float hwosec;
  float ds;
  float db;
  int npbl;
  float tex1;
  float tex2;
  float tey1;
  float tey2;
  float tez1;
  float tez2;
  float fix1;
  float fix2;
  float fiy1;
  float fiy2;
  float fiz1;
  float fiz2;
  float xc1;
  float xc2;
  float yc1;
  float yc2;
  float zc1;
  float zc2;
  float sts1;
  float sts2;
  float air1;
  float air2;
}LACGEOM_t ; 

typedef struct { /*  LACATTN */ 
  int lattlen[256];
  float laclumi[2][256];
}LACATTN_t ; 

/* ------ End of file ec1geom.inc */

/* ------ Start of file ec1mate.inc */

typedef struct { /*  ec1mate */ 
  int ec1_mat_scint;
  int ec1_mat_steel;
}EC1MATE_t ; 

/* ------ End of file ec1mate.inc */

/* ------ Start of file ec1sets.inc */

typedef struct { /*  ec1sets */ 
  int idtype_ec1;
  int set_ec1;
  int det_ec1;
}EC1SETS_t ; 

/* ------ End of file ec1sets.inc */

/* ------ Start of file ec1tmed.inc */

typedef struct { /*  ec1tmed */ 
  int ec1_med_air;
  int ec1_med_vac;
  int ec1_med_scint;
  int ec1_med_lead;
  int ec1_med_steel;
  int ec1_med_cvp_air;
}EC1TMED_t ; 

/* ------ End of file ec1tmed.inc */

/* ------ Start of file ecgeom.inc */

/* Defines */ 

#define CENTIMETER (1.00)
#define DEGREE (VALUE_OF_TT/180.*RADIAN)
#define NDEC (2)
#define NFRONT (5)
#define NHEC (7)
#define NLAYER (39)
#define NMOD (13)
#define NSTRIP (36)
#define NVEC (4)
#define NVIEWS (3)
#define RADIAN (1.00)
#define VALUE_OF_TT (3.141592653589793)

/* End of Defines */ 

typedef struct { /*  ecgeom */ 
  float ec_basic_r;
  float ec_basic_angle;
  float ec_basic_uin;
  float ec_basic_in_hght;
  float ec_basic_out_hght;
  float ec_basic_in_wdth;
  float ec_basic_out_wdth;
  float cov1th;
  float cov2th;
  float cov3th;
  float pbthi;
  float scthi;
  float att_length[NSTRIP][NLAYER][6];
}ECGEOM_t ; 

/* ------ End of file ecgeom.inc */

/* ------ Start of file ecmate.inc */

typedef struct { /*  ecmate */ 
  int ec_mat_helium;
  int ec_mat_scint;
  int ec_mat_steel;
  int ec_mat_foam;
}ECMATE_t ; 

/* ------ End of file ecmate.inc */

/* ------ Start of file ecsets.inc */

/* Defines */ 

#define NHITS_ECH (9)

/* End of Defines */ 

typedef struct { /*  ecsets */ 
  int idtype_ec;
  int set_ec;
  int det_ec[6];
  int idtype_ech;
  int set_ech;
  int det_ech;
}ECSETS_t ; 

/* ------ End of file ecsets.inc */

/* ------ Start of file ectmed.inc */

typedef struct { /*  ectmed */ 
  int ec_med_air;
  int ec_med_helium;
  int ec_med_vac;
  int ec_med_scint;
  int ec_med_lead;
  int ec_med_steel;
  int ec_med_cvp_air;
  int ec_med_foam;
}ECTMED_t ; 

/* ------ End of file ectmed.inc */

/* ------ Start of file ffpar.inc */

/* Defines */ 

#define MAX_FF (20)

/* End of Defines */ 

typedef struct { /*  ffpar */ 
  int nogeom_ff[MAX_FF];
  int nosec_ff[MAX_FF];
  int nohits_ff[MAX_FF];
  int nodigi_ff[MAX_FF];
  int nobosout_ff[MAX_FF];
  int iflgk_ff;
  int nstepmax_ff;
  float slengmax_ff;
  float zmin_ff;
  float zmax_ff;
  float rmax_ff;
  int nomate_ff[MAX_FF];
  int nodata_ff[MAX_FF];
  int nomcdata_ff[MAX_FF];
  int fast_code_ff[MAX_FF];
  int skip_ff;
  float noacc_ff[MAX_FF];
  float Beam_energy_ff[2];
  float Beam_offset_ff[2];
  float Beam_sigma_ff;
}FFPAR_t ; 

/* ------ End of file ffpar.inc */

/* ------ Start of file ffpar_bckgr.inc */

typedef struct { /*  FFPAR_BCKGR */ 
  int ntarget;
  float olumen;
  float zelem;
  float atom;
  float rho;
  float tleng;
  float timedc;
  int nincel;
}FFPAR_BCKGR_t ; 

/* ------ End of file ffpar_bckgr.inc */

/* ------ Start of file ffpar_cc.inc */

typedef struct { /*  ffpar_cc */ 
  float cc_cuts[5];
}FFPAR_CC_t ; 

/* ------ End of file ffpar_cc.inc */

/* ------ Start of file ffpar_dc.inc */

typedef struct { /*  ffpar_dc */ 
  float dc_dist_to_time;
  float dc_cuts[5];
  float dc_resolution[6];
}FFPAR_DC_t ; 

/* ------ End of file ffpar_dc.inc */

/* ------ Start of file ffpar_ec.inc */

typedef struct { /*  ffpar_ec */ 
  float ec_cuts[5];
  float atlen;
  float poiss;
  int ec_tdc_to_channel;
}FFPAR_EC_t ; 

/* ------ End of file ffpar_ec.inc */

/* ------ Start of file ffpar_hod.inc */

typedef struct { /*  ffpar_sh */ 
  float sh_pois;
  int sh_tdc_to_channel;
}FFPAR_SH_t ; 

/* ------ End of file ffpar_sh.inc */

/* ------ Start of file ffpar_ec1.inc */

typedef struct { /*  ffpar_ec1 */ 
  float ec1_cuts[5];
  int iatten;
  int ithreshold;
  float photoel;
  float delay;
  float reflect;
  float charge_to_adc;
}FFPAR_EC1_t ; 

/* ------ End of file ffpar_ec1.inc */

/* ------ Start of file ffpar_foil.inc */

typedef struct { /*  ffpar_foil */ 
  float foil_baffles[7];
  float upst_pos_ff[3];
  int chamber_type_ff;
}FFPAR_FOIL_t ; 

/* ------ End of file ffpar_foil.inc */

/* ------ Start of file ffpar_mg.inc */

typedef struct { /*  ffpar_mg */ 
  int magtype;
  float torus_field;
  float magscale[2];
  int init_ifield;
  float init_fieldm;
  float mg_pos_ff[3];
  float soln_pos;
  float mgzmax;
}FFPAR_MG_t ; 

/* ------ End of file ffpar_mg.inc */
/* ------ Start of file ffpar_ic.inc */
typedef struct { /* ffpar_ic */
  float ic_cuts[5];
  float ic_z0;
  float ic_pois;
  int ic_spar;
  int ic_mod;
  float ic_front[3];
}FFPAR_IC_t;

/* ------ End of file ffpar_ic.inc */

/* ------ Start of file ffpar_dvcs.inc */
typedef struct { /* ffpar_dvcs */
  float dvcs_pos[3];
}FFPAR_DVCS_t;

/* ------ End of file ffpar_ic.inc */


/* ------ Start of file ffpar_ptg.inc */

typedef struct { /*  ffpar_ptg */ 
  float ptg_angle[3];
  int ptg_ifield;
  float ptg_fieldm;
  float ptg_scale;
  float ptg_maxradius;
}FFPAR_PTG_t ; 

/* ------ End of file ffpar_ptg.inc */


/* ------ Start of file ffpar_sc.inc */

typedef struct { /*  ffpar_sc */ 
  float sc_cuts[5];
}FFPAR_SC_t ; 

/* ------ End of file ffpar_sc.inc */

/* ------ Start of file ffpar_st.inc */

typedef struct { /*  ffpar_st */ 
  float st_cuts[5];
  float st_zoff_ff;
  float st_type;
}FFPAR_ST_t ; 

/* ------ End of file ffpar_st.inc */

/* ------ Start of file ffpar_tg.inc */

typedef struct { /*  ffpar_tg */ 
  int tg_type_ff;
  int tg_mate_ff;
  int tg_cell_ff;
  int tg2_mate_ff;
  int ivx_e5;
  int ivx_eg6;
  float tg_pos_ff[3];
  float tg_ins_ff[6];
  float tg2_pos_ff[3];
  float teg2_pos_ff[3];
  float tg_user_ff[6];
  float tg_tmedcuts_ff[5];
  float tgpar_save[12];
  float tg_scatz_ff;
}FFPAR_TG_t ; 

/* ------ End of file ffpar_tg.inc */

/* ------ Start of file ffpar_tg_e2.inc */

typedef struct { /* tg  */ 
  int itg_type[3];
  int ivx_e2;
  int tg_e2;
  int ivx_eg2[1];
}FFPAR_TG_E2_t ; 

/* ------ End of file ffpar_tg_e2.inc */

/* ------ Start of file ffpar_tg_g7.inc */

typedef struct { /* g7 target  */ 
  int tg_g7;
}FFPAR_TG_G7_t ; 

/* ------ End of file ffpar_tg_g7.inc */


/* ------ Start of file gsim_trigger.inc */

typedef struct { /*  GSIM_TRIGGER */ 
  float ttrig;
}GSIM_TRIGGER_t ; 

/* ------ End of file gsim_trigger.inc */

/* ------ Start of file gsimcontrol.inc */

typedef struct { /*  gsimcontrol */ 
  int noffread;
  int nofield;
  int rawbanks_in;
  int nogeom[MAX_DETECTOR];
  int nomate[MAX_DETECTOR];
  int nosec[MAX_DETECTOR];
  int nohits[MAX_DETECTOR];
  int nodigi[MAX_DETECTOR];
  int nobosout[MAX_DETECTOR];
  int nodata[MAX_DETECTOR];
  int nomcdata[MAX_DETECTOR];
  int helium_only;
  int fast_code[MAX_DETECTOR];
}GSIMCONTROL_t ; 

/* ------ End of file gsimcontrol.inc */

/* ------ Start of file gsimout.inc */

typedef struct { /*  gsimout */ 
  int gsimout_unit;
  char gsimout_filename[120];
  char gsimout_daname[120];
}GSIMOUT_t ; 

/* ------ End of file gsimout.inc */

/* ------ Start of file inpart.inc */

typedef struct { /*  INPART */ 
  int jwrit;
  int nche;
  float amss;
  float vin[7];
}INPART_t ; 

/* ------ End of file inpart.inc */

/* ------ Start of file lcgeom.inc */

/* Defines */ 

#define NSECT (6)
#define RNSECT (1./RTMP)
#define RTMP (NSECT)

/* End of Defines */ 

typedef struct { /*  lcgeom */ 
  float sinsec[NSECT];
  float cossec[NSECT];
  float tnhase;
  float ptang;
  float hthmg;
}LCGEOM_t ; 

/* ------ End of file lcgeom.inc */

/* ------ Start of file lcmat.inc */

typedef struct { /*  materials */ 
  int mat_carbo;
  int mat_h2gas;
  int mat_hegas;
  int mat_mylar;
  int mat_areth;
  int mat_plast;
  int mat_leadg;
  int mat_freon;
  int mat_stain;
}MATERIALS_t ; 

/* ------ End of file lcmat.inc */

/* ------ Start of file lcmed.inc */

/* ------ End of file lcmed.inc */

/* ------ Start of file lcsc.inc */

/* Defines */ 

#define NDSC (4)
#define NHSC (6)
#define NVSC (2)

/* End of Defines */ 

/* ------ End of file lcsc.inc */

/* ------ Start of file lcsets.inc */

/* Defines */ 

#define MSLCH (6)

/* End of Defines */ 

typedef struct { /*  lcsets */ 
  int isetch[MSLCH];
  int isetsc;
  int isetec[3];
  int isetst[2];
  int isetstc[2];
  int idetch[MSLCH];
  int idetsc;
  int idetec[3];
  int idetst[2];
  int idetstc[2];
  char csetch[MSLCH][4];
  char csetsc[4];
  char csetec[3][4];
  char cdetch[MSLCH][4];
  char cdetsc[4];
  char cdetec[3][4];
  char csetst[2][4];
  char cdetst[2][4];
  char csetstc[2][4];
  char cdetstc[2][4];
}LCSETS_t ; 

/* ------ End of file lcsets.inc */

/* ------ Start of file lcstc.inc */

/* Defines */ 

#define NDSTC (4)
#define NHSTC (6)
#define NVSTC (2)

/* End of Defines */ 

/* ------ End of file lcstc.inc */

/* ------ Start of file mcin.inc */

typedef struct { /*  mcin */ 
  int mcin_unit;
  char mcin_filename[120];
  char mcin_daname[120];
}MCIN_t ; 

/* ------ End of file mcin.inc */

/* ------ Start of file mgtmed.inc */

typedef struct { /*  mgtmed */ 
  int torus_med_copper;
  int mini_med_copper;
  int mini_med_aluminum;
}MGTMED_t ; 

/* ------ End of file mgtmed.inc */

/* ------ Start of file ptgtmed.inc */

typedef struct { /*  ptgtmed */ 
  int ptg_med_helium;
  int ptg_med_copper;
  int ptg_med_iron;
  int ptg_med_alu;
  int ptg_med_air;
  int ptg_med_lead;
  int ptg_med_nh3;
  int ptg_med_nd3;
}PTGTMED_t ; 

/* ------ End of file ptgtmed.inc */

/* ------ Start of file sc_com.inc */

typedef struct { /*  sc_com */ 
  float pekin;
  float ptot;
  int ipart_num;
}SC_COM_t ; 

/* ------ End of file sc_com.inc */

/* ------ Start of file sccalib.inc */

/* Defines */ 

#define ADCETH (3.0)
#define ADCMAX (8192)
#define ADCOVR (16384)
#define APEDSUB (0.0)
#define DEDXMIP (0.001998)
#define DISCTH (30.0)
#define IADCMAX (8192)
#define NBOSCAL (48)
#define NDDICA (40)
#define NELCCH (48)
#define NPADDL (57)
#define NSECMX (6)
#define T0SC (450)
#define TDCMAX (4096)

/* End of Defines */ 

typedef struct { /*  sccalib */ 
  float sccal_values[NSECMX][NPADDL][2][12];
}SCCALIB_t ; 

/* ------ End of file sccalib.inc */

/* ------ Start of file scgeom.inc */

/* Defines */ 

#define IPMAX (4)
#define NDDIGE (40)
#define NGEOCH (48)
#define NSCIN (57)

/* End of Defines */ 

typedef struct { /*  scgeom */ 
  int ifirst[IPMAX];
  int ilast[IPMAX];
  float phiort[IPMAX];
  float alsc[NSCIN];
  float xm[NSCIN];
  float ym[NSCIN];
  float zm[NSCIN];
  float wdth[NSCIN];
  float thck[NSCIN];
}SCGEOM_t ; 

/* ------ End of file scgeom.inc */
/* ------ Start of file shgeom.inc */

/* Defines */ 

#define NPIX (56)


/* End of Defines */ 

typedef struct { /*  shgeom */ 

}SHGEOM_t ; 

/* ------ End of file shgeom.inc */

/* ------ Start of file scmate.inc */

typedef struct { /*  scmate */ 
  int sc_mat_helium;
  int sc_mat_scint;
  int sc_mat_steel;
  int sc_mat_foam;
}SCMATE_t ; 

/* ------ End of file scmate.inc */

/* ------ Start of file scsets.inc */

/* Defines */ 

#define NDIGI_SC (4)
#define NHITS_SC (5)
#define NHITS_SCH (9)
#define NVOL_SC (2)

/* End of Defines */ 

typedef struct { /*  scsets */ 
  int idtype_sc;
  int set_sc;
  int det_sc;
  int idtype_sch;
  int set_sch;
  int det_sch;
}SCSETS_t ; 

/* ------ End of file scsets.inc */

/* ------ Start of file shsets.inc */

/* Defines */ 

#define NDIGI_HD (2)
#define NHITS_HD (5)
#define NHITS_HDH (9)
#define NVOL_HD (1)

/* End of Defines */ 

typedef struct { /*  shsets */ 
  int idtype_hd;
  int set_hd;
  int det_hd;
  int idtype_hdh;
  int set_hdh;
  int det_hdh;
}SHSETS_t ; 

/* ------ End of file shsets.inc */

/* ------ Start of file sctmed.inc */

typedef struct { /*  sctmed */ 
  int sc_med_air;
  int sc_med_helium;
  int sc_med_scint;
  int sc_med_lead;
  int sc_med_steel;
  int sc_med_foam;
}SCTMED_t ; 

/* ------ End of file sctmed.inc */

/* ------ Start of file stgeom.inc */

typedef struct { /*  stgeom */ 
  float mother[16];
  float leg_parms[3];
  float nose_mother[4];
  float nose1_parms[11];
  float nose2_parms[4];
  float dist_to_bl;
  float zoffset;
}STGEOM_t ; 

/* ------ End of file stgeom.inc */

/* ------ Start of file stmate.inc */

typedef struct { /*  stmate */ 
  int st_mat_scint;
}STMATE_t ; 

/* ------ End of file stmate.inc */

/* ------ Start of file stsets.inc */

/* Defines */ 

#define NDIGI_ST (4)
#define NHITS_ST (6)
#define NHITS_STH (10)
#define NVOL_ST (2)

/* End of Defines */ 

typedef struct { /*  stsets */ 
  int idtype_st;
  int set_st;
  int det_st1;
  int det_st2;
  int det_sth1;
  int det_sth2;
  int det_stp11;
  int det_stp12;
  int det_stp21;
  int det_stp22;
  int det_stp23;
  int det_stp31;
  int det_stp32;
  int det_stp33;
  int det_stp41;
  int det_stp42;
  int det_stph11;
  int det_stph12;
  int det_stph21;
  int det_stph22;
  int det_stph23;
  int det_stph31;
  int det_stph32;
  int det_stph33;
  int det_stph41;
  int det_stph42;
}STSETS_t ; 

/* ------ End of file stsets.inc */

/* ------ Start of file sttmed.inc */

typedef struct { /*  sttmed */ 
  int st_med_scint;
}STTMED_t ; 

/* ------ End of file sttmed.inc */

