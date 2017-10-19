#ifndef SEB_HH
#define SEB_HH

#include <TTree.h>

extern "C" {

  //HEVT Bank
  const int Maxparticles = 40;

  struct hevt_nt_ {
    int npart;
    int evstat;
    int intt;
    int evntid;
    int evtype;
    int evclas;
    int evthel;
    int evntclas2;
    float q_l;
    float t_l;
    float tr_time;
    float rf_time1;
    float rf_time2;
    
  };
  extern struct hevt_nt_ hevt_nt_;

  //EVNT Bank
  struct evnt_nt_ {
    int gpart;
    int id[Maxparticles];
    int stat[Maxparticles];
    int dc[Maxparticles];
    int cc[Maxparticles];
    int sc[Maxparticles];
    int ec[Maxparticles];
    int lec[Maxparticles];
    int ccst[Maxparticles];
    float p[Maxparticles];
    float m[Maxparticles];
    int q[Maxparticles];
    float b[Maxparticles];
    float cx[Maxparticles];
    float cy[Maxparticles];
    float cz[Maxparticles];
    float vx[Maxparticles];
    float vy[Maxparticles];
    float vz[Maxparticles];
  };
  extern struct evnt_nt_ evnt_nt_;

  //ECPB Bank
  struct ecpb_nt_ {
    int ec_part;
    int ec_stat[Maxparticles];
    int ec_sect[Maxparticles];
    int ec_whol[Maxparticles];
    int ec_inst[Maxparticles];
    int ec_oust[Maxparticles];
    float etot[Maxparticles];
    float ec_ei[Maxparticles];
    float ec_eo[Maxparticles];
    float ec_t[Maxparticles];
    float ec_r[Maxparticles];
    float ech_x[Maxparticles];
    float ech_y[Maxparticles];
    float ech_z[Maxparticles];
    float ec_m2[Maxparticles];
    float ec_m3[Maxparticles];
    float ec_m4[Maxparticles];
    float ec_c2[Maxparticles];
  };
  extern struct ecpb_nt_ ecpb_nt_;

  //DCPB Bank
  struct dcpb_nt_ {
    int dc_part;
    int dc_sect[Maxparticles];
    int dc_trk[Maxparticles];
    int dc_stat[Maxparticles];
    float dc_vx[Maxparticles];
    float dc_vy[Maxparticles];
    float dc_vz[Maxparticles];
    float dc_vr[Maxparticles];
    float dc_xsc[Maxparticles];
    float dc_ysc[Maxparticles];
    float dc_zsc[Maxparticles];
    float dc_cxsc[Maxparticles];
    float dc_cysc[Maxparticles];
    float dc_czsc[Maxparticles];
    float dc_c2[Maxparticles];
  };
  extern struct dcpb_nt_ dcpb_nt_;

  //SCPART Bank
  struct scpart_nt_ {
    int sc_part;
    int sc_sect[Maxparticles];
    int sc_hit[Maxparticles];
    int sc_pd[Maxparticles];
    int sc_stat[Maxparticles];
    float edep[Maxparticles];
    float sc_t[Maxparticles];
    float sc_r[Maxparticles];
    float sc_c2[Maxparticles];
  };
  extern struct scpart_nt_ scpart_nt_;

  //CCPART Bank
  struct ccpart_nt_ {
    int cc_part;
    int cc_sect[Maxparticles];
    int cc_hit[Maxparticles];
    int cc_segm[Maxparticles];
    int nphe[Maxparticles];
    float cc_t[Maxparticles];
    float cc_r[Maxparticles];
    float cc_c2[Maxparticles];
  };
  extern struct ccpart_nt_ ccpart_nt_;

  //lecpart_nt Bank
  struct lecpart_nt_ {
    int lac_part;
    int lec_sect[Maxparticles];
    int lec_hit[Maxparticles];
    int lec_stat[Maxparticles];
    float lec_etot[Maxparticles];
    float lec_ein[Maxparticles];
    float lec_t[Maxparticles];
    float lec_r[Maxparticles];
    float lec_x[Maxparticles];
    float lec_y[Maxparticles];
    float lec_z[Maxparticles];
    float lec_c2[Maxparticles];
  };
  extern struct lecpart_nt_ lecpart_nt_;

  //STPART Bank
  const int MaxST = 100;

  struct stpart_nt_ {
    int st_part;
    int st_status[MaxST];
    float st_time[MaxST];
    float st_rtrk[MaxST];
    int st_sector[MaxST];
    int st_ihit[MaxST];
    int st_trkno[MaxST];
  };
  extern struct stpart_nt_ stpart_nt_;

  struct TAG_NT_ {
    int taggoodhit;
    int tag_ptr[MaxST];
    float vertex_time[MaxST];
    float tag_energy[MaxST];
    float dt_st_tag[MaxST];
  };
  extern struct TAG_NT_ TAG_NT_;

  //Fortran fill function
  extern void fill_seb_nt_();
}

void seb_branches(TTree* tree);

#endif
