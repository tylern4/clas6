#include "seb.hh"

//Struct objects:
struct hevt_nt_ hevt_nt_;
struct evnt_nt_ evnt_nt_;
struct ecpb_nt_ ecpb_nt_;
struct dcpb_nt_ dcpb_nt_;
struct scpart_nt_ scpart_nt_;
struct ccpart_nt_ ccpart_nt_;
struct lecpart_nt_ lecpart_nt_;
struct stpart_nt_ stpart_nt_;
struct TAG_NT_ TAG_NT_;

//Branch function
void seb_branches(TTree* tree) {
  //HEVT
  tree->Branch("npart",&hevt_nt_.npart,"npart/I");
  tree->Branch("evstat",&hevt_nt_.evstat,"evstat/I");
  tree->Branch("intt",&hevt_nt_.intt,"intt/I");
  tree->Branch("evntid",&hevt_nt_.evntid,"evntid/I");
  tree->Branch("evtype",&hevt_nt_.evtype,"evtype/I");
  tree->Branch("evntclas",&hevt_nt_.evclas,"evntclas/I");
  tree->Branch("evthel",&hevt_nt_.evthel,"evthel/I");
  tree->Branch("evntclas2",&hevt_nt_.evntclas2,"evntclas2/I");
  tree->Branch("q_l",&hevt_nt_.q_l,"q_l/F");
  tree->Branch("t_l",&hevt_nt_.t_l,"t_l/F");
  tree->Branch("tr_time",&hevt_nt_.tr_time,"tr_time/F");
  tree->Branch("rf_time1",&hevt_nt_.rf_time1,"rf_time1/F");
  tree->Branch("rf_time2",&hevt_nt_.rf_time2,"rf_time2/F");

  //EVNT
  tree->Branch("gpart",&evnt_nt_.gpart,"gpart/I");
  tree->Branch("id",&evnt_nt_.id,"id[gpart]/I");
  tree->Branch("stat",&evnt_nt_.stat,"stat[gpart]/I");
  tree->Branch("dc",&evnt_nt_.dc,"dc[gpart]/I");
  tree->Branch("cc",&evnt_nt_.cc,"cc[gpart]/I");
  tree->Branch("sc",&evnt_nt_.sc,"sc[gpart]/I");
  tree->Branch("ec",&evnt_nt_.ec,"ec[gpart]/I");
  tree->Branch("lec",&evnt_nt_.lec,"lec[gpart]/I");
  tree->Branch("ccst",&evnt_nt_.ccst,"ccst[gpart]/I");
  tree->Branch("p",&evnt_nt_.p,"p[gpart]/F");
  tree->Branch("m",&evnt_nt_.m,"m[gpart]/F");
  tree->Branch("q",&evnt_nt_.q,"q[gpart]/I");
  tree->Branch("b",&evnt_nt_.b,"b[gpart]/F");
  tree->Branch("cx",&evnt_nt_.cx,"cx[gpart]/F");
  tree->Branch("cy",&evnt_nt_.cy,"cy[gpart]/F");
  tree->Branch("cz",&evnt_nt_.cz,"cz[gpart]/F");
  tree->Branch("vx",&evnt_nt_.vx,"vx[gpart]/F");
  tree->Branch("vy",&evnt_nt_.vy,"vy[gpart]/F");
  tree->Branch("vz",&evnt_nt_.vz,"vz[gpart]/F");

  //DCPB
  tree->Branch("dc_part",&dcpb_nt_.dc_part,"dc_part/I");
  tree->Branch("dc_sect",&dcpb_nt_.dc_sect,"dc_sect[dc_part]/I");
  tree->Branch("dc_trk",&dcpb_nt_.dc_trk,"dc_trk[dc_part]/I");
  tree->Branch("dc_stat",&dcpb_nt_.dc_stat,"dc_stat[dc_part]/I");
  tree->Branch("dc_vx",&dcpb_nt_.dc_vx,"dc_vx[dc_part]/F");
  tree->Branch("dc_vy",&dcpb_nt_.dc_vy,"dc_vy[dc_part]/F");
  tree->Branch("dc_vz",&dcpb_nt_.dc_vz,"dc_vz[dc_part]/F");
  tree->Branch("dc_vr",&dcpb_nt_.dc_vr,"dc_vr[dc_part]/F");
  tree->Branch("dc_xsc",&dcpb_nt_.dc_xsc,"dc_xsc[dc_part]/F");
  tree->Branch("dc_ysc",&dcpb_nt_.dc_ysc,"dc_ysc[dc_part]/F");
  tree->Branch("dc_zsc",&dcpb_nt_.dc_zsc,"dc_zsc[dc_part]/F");
  tree->Branch("dc_cxsc",&dcpb_nt_.dc_cxsc,"dc_cxsc[dc_part]/F");
  tree->Branch("dc_cysc",&dcpb_nt_.dc_cysc,"dc_cysc[dc_part]/F");
  tree->Branch("dc_czsc",&dcpb_nt_.dc_czsc,"dc_czsc[dc_part]/F");
  tree->Branch("dc_c2",&dcpb_nt_.dc_c2,"dc_c2[dc_part]/F");

  //ECPB
  tree->Branch("ec_part",&ecpb_nt_.ec_part,"ec_part/I");
  tree->Branch("ec_stat",&ecpb_nt_.ec_stat,"ec_stat[ec_part]/I");
  tree->Branch("ec_sect",&ecpb_nt_.ec_sect,"ec_sect[ec_part]/I");
  tree->Branch("ec_whol",&ecpb_nt_.ec_whol,"ec_whol[ec_part]/I");
  tree->Branch("ec_inst",&ecpb_nt_.ec_inst,"ec_inst[ec_part]/I");
  tree->Branch("ec_oust",&ecpb_nt_.ec_oust,"ec_oust[ec_part]/I");
  tree->Branch("etot",&ecpb_nt_.etot,"etot[ec_part]/F");
  tree->Branch("ec_ei",&ecpb_nt_.ec_ei,"ec_ei[ec_part]/F");
  tree->Branch("ec_eo",&ecpb_nt_.ec_eo,"ec_eo[ec_part]/F");
  tree->Branch("ec_t",&ecpb_nt_.ec_t,"ec_t[ec_part]/F");
  tree->Branch("ec_r",&ecpb_nt_.ec_r,"ec_r[ec_part]/F");
  tree->Branch("ech_x",&ecpb_nt_.ech_x,"ech_x[ec_part]/F");
  tree->Branch("ech_y",&ecpb_nt_.ech_y,"ech_y[ec_part]/F");
  tree->Branch("ech_z",&ecpb_nt_.ech_z,"ech_z[ec_part]/F");
  tree->Branch("ec_m2",&ecpb_nt_.ec_m2,"ec_m2[ec_part]/F");
  tree->Branch("ec_m3",&ecpb_nt_.ec_m3,"ec_m3[ec_part]/F");
  tree->Branch("ec_m4",&ecpb_nt_.ec_m4,"ec_m4[ec_part]/F");
  tree->Branch("ec_c2",&ecpb_nt_.ec_c2,"ec_c2[ec_part]/F");

  //SCPB (scpart_nt_)
  tree->Branch("sc_part",&scpart_nt_.sc_part,"sc_part/I");
  tree->Branch("sc_sect",&scpart_nt_.sc_sect,"sc_sect[sc_part]/I");
  tree->Branch("sc_hit",&scpart_nt_.sc_hit,"sc_hit[sc_part]/I");
  tree->Branch("sc_pd",&scpart_nt_.sc_pd,"sc_pd[sc_part]/I");
  tree->Branch("sc_stat",&scpart_nt_.sc_stat,"sc_stat[sc_part]/I");
  tree->Branch("edep",&scpart_nt_.edep,"edep[sc_part]/F");
  tree->Branch("sc_t",&scpart_nt_.sc_t,"sc_t[sc_part]/F");
  tree->Branch("sc_r",&scpart_nt_.sc_r,"sc_r[sc_part]/F");
  tree->Branch("sc_c2",&scpart_nt_.sc_c2,"sc_c2[sc_part]/F");

  //CCPB (ccpart_nt_)
  tree->Branch("cc_part",&ccpart_nt_.cc_part,"cc_part/I");
  tree->Branch("cc_sect",&ccpart_nt_.cc_sect,"cc_sect[cc_part]/I");
  tree->Branch("cc_hit",&ccpart_nt_.cc_hit,"cc_hit[cc_part]/I");
  tree->Branch("cc_segm",&ccpart_nt_.cc_segm,"cc_segm[cc_part]/I");
  tree->Branch("nphe",&ccpart_nt_.nphe,"nphe[cc_part]/I");
  tree->Branch("cc_t",&ccpart_nt_.cc_t,"cc_t[cc_part]/F");
  tree->Branch("cc_r",&ccpart_nt_.cc_r,"cc_r[cc_part]/F");
  tree->Branch("cc_c2",&ccpart_nt_.cc_c2,"cc_c2[cc_part]/F");

  //LECPB
  tree->Branch("lac_part",&lecpart_nt_.lac_part,"lac_part/I");
  tree->Branch("lec_sect",&lecpart_nt_.lec_sect,"lec_sect[lac_part]/I");
  tree->Branch("lec_hit",&lecpart_nt_.lec_hit,"lec_hit[lac_part]/I");
  tree->Branch("lec_stat",&lecpart_nt_.lec_stat,"lec_stat[lac_part]/I");
  tree->Branch("lec_etot",&lecpart_nt_.lec_etot,"lec_etot[lac_part]/F");
  tree->Branch("lec_ein",&lecpart_nt_.lec_ein,"lec_ein[lac_part]/F");
  tree->Branch("lec_t",&lecpart_nt_.lec_t,"lec_t[lac_part]/F");
  tree->Branch("lec_r",&lecpart_nt_.lec_r,"lec_r[lac_part]/F");
  tree->Branch("lec_x",&lecpart_nt_.lec_x,"lec_x[lac_part]/F");
  tree->Branch("lec_y",&lecpart_nt_.lec_y,"lec_y[lac_part]/F");
  tree->Branch("lec_z",&lecpart_nt_.lec_z,"lec_z[lac_part]/F");
  tree->Branch("lec_c2",&lecpart_nt_.lec_c2,"lec_c2[lac_part]/F");

  //STPB (stpart_nt_)
  tree->Branch("st_part",&stpart_nt_.st_part,"st_part/I");
  tree->Branch("st_status",&stpart_nt_.st_status,"st_status[st_part]/I");
  tree->Branch("st_time",&stpart_nt_.st_time,"st_time[st_part]/F");
  tree->Branch("st_rtrk",&stpart_nt_.st_rtrk,"st_rtrk[st_part]/F");


}
