#include <stdlib.h>

class JBosBank {
  /// virtual class, all banks derive from this one
protected:
  bool found;
  bankHeader_t* head;
public:
  JBosBank () : found(false), head(NULL) {};
  bool Found()   { return found; }
  int  GetNrows() { return found ? head->nrow : 0 ; }
  int  GetNcols() { return found ? head->ncol : 0 ; }
  int  GetSector() { return found ? head->nr  : 0 ; }
  void CheckIndex(int i) {
    if (!found) throw -1;
    if (i < 0) throw -2;
    if (i >= GetNrows()) throw -3;
  }
};

///----------C++ wrapper around bosbanks--------------------
class Jbmpr : public JBosBank {
  clasBMPR_t* p;
public:
  Jbmpr (int i) : JBosBank() {
    p = (clasBMPR_t*) getGroup(&bcs_, "BMPR", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  bmpr_t& operator[] (int i) {
    CheckIndex(i);
    return p->bmpr[i];
  }
};

class Jbrep : public JBosBank {
  clasBREP_t* p;
public:
  Jbrep (int i) : JBosBank() {
    p = (clasBREP_t*) getGroup(&bcs_, "BREP", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  brep_t& operator[] (int i) {
    CheckIndex(i);
    return p->brep[i];
  }
};

class Jcall : public JBosBank {
  clasCALL_t* p;
public:
  Jcall () : JBosBank() {
    p = (clasCALL_t*) getBank(&bcs_, "CALL");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  call_t& operator[] (int i) {
    CheckIndex(i);
    return p->call[i];
  }
};

class Jcc0 : public JBosBank {
  clasCC0_t* p;
public:
  Jcc0 (int i) : JBosBank() {
    p = (clasCC0_t*) getGroup(&bcs_, "CC  ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  cc0_t& operator[] (int i) {
    CheckIndex(i);
    return p->cc0[i];
  }
};

class Jcc01 : public JBosBank {
  clasCC01_t* p;
public:
  Jcc01 (int i) : JBosBank() {
    p = (clasCC01_t*) getGroup(&bcs_, "CC01", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  cc01_t& operator[] (int i) {
    CheckIndex(i);
    return p->cc01[i];
  }
};

class Jcc1 : public JBosBank {
  clasCC1_t* p;
public:
  Jcc1 (int i) : JBosBank() {
    p = (clasCC1_t*) getGroup(&bcs_, "CC1 ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  cc1_t& operator[] (int i) {
    CheckIndex(i);
    return p->cc1[i];
  }
};

class Jccdi : public JBosBank {
  clasCCDI_t* p;
public:
  Jccdi (int i) : JBosBank() {
    p = (clasCCDI_t*) getGroup(&bcs_, "CCDI", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ccdi_t& operator[] (int i) {
    CheckIndex(i);
    return p->ccdi[i];
  }
};

class Jcch : public JBosBank {
  clasCCH_t* p;
public:
  Jcch (int i) : JBosBank() {
    p = (clasCCH_t*) getGroup(&bcs_, "CCH ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  cch_t& operator[] (int i) {
    CheckIndex(i);
    return p->cch[i];
  }
};

class Jcch1 : public JBosBank {
  clasCCH1_t* p;
public:
  Jcch1 (int i) : JBosBank() {
    p = (clasCCH1_t*) getGroup(&bcs_, "CCH1", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  cch1_t& operator[] (int i) {
    CheckIndex(i);
    return p->cch1[i];
  }
};

class Jcch2 : public JBosBank {
  clasCCH2_t* p;
public:
  Jcch2 (int i) : JBosBank() {
    p = (clasCCH2_t*) getGroup(&bcs_, "CCH2", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  cch2_t& operator[] (int i) {
    CheckIndex(i);
    return p->cch2[i];
  }
};

class Jccmt : public JBosBank {
  clasCCMT_t* p;
public:
  Jccmt (int i) : JBosBank() {
    p = (clasCCMT_t*) getGroup(&bcs_, "CCMT", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ccmt_t& operator[] (int i) {
    CheckIndex(i);
    return p->ccmt[i];
  }
};

class Jccpb : public JBosBank {
  clasCCPB_t* p;
public:
  Jccpb () : JBosBank() {
    p = (clasCCPB_t*) getBank(&bcs_, "CCPB");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ccpb_t& operator[] (int i) {
    CheckIndex(i);
    return p->ccpb[i];
  }
};

class Jccpe : public JBosBank {
  clasCCPE_t* p;
public:
  Jccpe (int i) : JBosBank() {
    p = (clasCCPE_t*) getGroup(&bcs_, "CCPE", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ccpe_t& operator[] (int i) {
    CheckIndex(i);
    return p->ccpe[i];
  }
};

class Jccrc : public JBosBank {
  clasCCRC_t* p;
public:
  Jccrc (int i) : JBosBank() {
    p = (clasCCRC_t*) getGroup(&bcs_, "CCRC", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ccrc_t& operator[] (int i) {
    CheckIndex(i);
    return p->ccrc[i];
  }
};

class Jccs : public JBosBank {
  clasCCS_t* p;
public:
  Jccs (int i) : JBosBank() {
    p = (clasCCS_t*) getGroup(&bcs_, "CCS ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ccs_t& operator[] (int i) {
    CheckIndex(i);
    return p->ccs[i];
  }
};

#ifndef OLDBOS
class Jcct : public JBosBank {
  clasCCT_t* p;
public:
  Jcct (int i) : JBosBank() {
    p = (clasCCT_t*) getGroup(&bcs_, "CCT ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  cct_t& operator[] (int i) {
    CheckIndex(i);
    return p->cct[i];
  }
};
#endif
class Jchi2 : public JBosBank {
  clasCHI2_t* p;
public:
  Jchi2 (int i) : JBosBank() {
    p = (clasCHI2_t*) getGroup(&bcs_, "CHI2", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  chi2_t& operator[] (int i) {
    CheckIndex(i);
    return p->chi2[i];
  }
};

class Jcped : public JBosBank {
  clasCPED_t* p;
public:
  Jcped (int i) : JBosBank() {
    p = (clasCPED_t*) getGroup(&bcs_, "CPED", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  cped_t& operator[] (int i) {
    CheckIndex(i);
    return p->cped[i];
  }
};

class Jecpc : public JBosBank {
  clasECPC_t* p;
public:
  Jecpc() : JBosBank() {
    p = (clasECPC_t*) getBank(&bcs_, "ECPC");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  void GetNext() {
    if (!found) return;
    p = (clasECPC_t*) getNextBank(&bcs_, head);
    if (p) {
      head = &(p->bank);
    }
    else {
      found = false;
      head  = NULL;
    }
  }
  ecpc_t& operator[] (int i) {
    CheckIndex(i);
    return p->ecpc[i];
  }
};

class Jecpe : public JBosBank {
  clasECPE_t* p;
public:
  Jecpe (int i) : JBosBank() {
    p = (clasECPE_t*) getGroup(&bcs_, "ECPE", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ecpe_t& operator[] (int i) {
    CheckIndex(i);
    return p->ecpe[i];
  }
};

#ifndef OLDBOS
class Ject : public JBosBank {
  clasECT_t* p;
public:
  Ject (int i) : JBosBank() {
    p = (clasECT_t*) getGroup(&bcs_, "ECT ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ect_t& operator[] (int i) {
    CheckIndex(i);
    return p->ect[i];
  }
};
#endif

class Jcl01 : public JBosBank {
  clasCL01_t* p;
public:
  Jcl01 () : JBosBank() {
    p = (clasCL01_t*) getBank(&bcs_, "CL01");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  cl01_t& operator[] (int i) {
    CheckIndex(i);
    return p->cl01[i];
  }
};

class Jdc0 : public JBosBank {
  clasDC0_t* p;
public:
  Jdc0 (int i) : JBosBank() {
    p = (clasDC0_t*) getGroup(&bcs_, "DC0 ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  dc0_t& operator[] (int i) {
    CheckIndex(i);
    return p->dc0[i];
  }
};

class Jdc1 : public JBosBank {
  clasDC1_t* p;
public:
  Jdc1 (int i) : JBosBank() {
    p = (clasDC1_t*) getGroup(&bcs_, "DC1 ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  dc1_t& operator[] (int i) {
    CheckIndex(i);
    return p->dc1[i];
  }
};

class Jdcdw : public JBosBank {
  clasDCDW_t* p;
public:
  Jdcdw (int i) : JBosBank() {
    p = (clasDCDW_t*) getGroup(&bcs_, "DCDW", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  dcdw_t& operator[] (int i) {
    CheckIndex(i);
    return p->dcdw[i];
  }
};

class Jdcgm : public JBosBank {
  clasDCGM_t* p;
public:
  Jdcgm (int i) : JBosBank() {
    p = (clasDCGM_t*) getGroup(&bcs_, "DCGM", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  dcgm_t& operator[] (int i) {
    CheckIndex(i);
    return p->dcgm[i];
  }
};

class Jdcgw : public JBosBank {
  clasDCGW_t* p;
public:
  Jdcgw (int i) : JBosBank() {
    p = (clasDCGW_t*) getGroup(&bcs_, "DCGW", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  dcgw_t& operator[] (int i) {
    CheckIndex(i);
    return p->dcgw[i];
  }
};

class Jdch : public JBosBank {
  clasDCH_t* p;
public:
  Jdch (int i) : JBosBank() {
    p = (clasDCH_t*) getGroup(&bcs_, "DCH ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  dch_t& operator[] (int i) {
    CheckIndex(i);
    return p->dch[i];
  }
};

class Jdcpb : public JBosBank {
  clasDCPB_t* p;
public:
  Jdcpb () : JBosBank() {
    p = (clasDCPB_t*) getBank(&bcs_, "DCPB");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  dcpb_t& operator[] (int i) {
    CheckIndex(i);
    return p->dcpb[i];
  }
};

class Jdcv1 : public JBosBank {
  clasDCV1_t* p;
public:
  Jdcv1 (int i) : JBosBank() {
    p = (clasDCV1_t*) getGroup(&bcs_, "DCV1", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  dcv1_t& operator[] (int i) {
    CheckIndex(i);
    return p->dcv1[i];
  }
};

class Jdcv2 : public JBosBank {
  clasDCV2_t* p;
public:
  Jdcv2 (int i) : JBosBank() {
    p = (clasDCV2_t*) getGroup(&bcs_, "DCV2", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  dcv2_t& operator[] (int i) {
    CheckIndex(i);
    return p->dcv2[i];
  }
};

class Jdcv3 : public JBosBank {
  clasDCV3_t* p;
public:
  Jdcv3 (int i) : JBosBank() {
    p = (clasDCV3_t*) getGroup(&bcs_, "DCV3", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  dcv3_t& operator[] (int i) {
    CheckIndex(i);
    return p->dcv3[i];
  }
};

class Jddly : public JBosBank {
  clasDDLY_t* p;
public:
  Jddly (int i) : JBosBank() {
    p = (clasDDLY_t*) getGroup(&bcs_, "DDLY", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ddly_t& operator[] (int i) {
    CheckIndex(i);
    return p->ddly[i];
  }
};

class Jdgeo : public JBosBank {
  clasDGEO_t* p;
public:
  Jdgeo (int i) : JBosBank() {
    p = (clasDGEO_t*) getGroup(&bcs_, "DGEO", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  dgeo_t& operator[] (int i) {
    CheckIndex(i);
    return p->dgeo[i];
  }
};

class Jdhcl : public JBosBank {
  clasDHCL_t* p;
public:
  Jdhcl (int i) : JBosBank() {
    p = (clasDHCL_t*) getGroup(&bcs_, "DHCL", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  dhcl_t& operator[] (int i) {
    CheckIndex(i);
    return p->dhcl[i];
  }
};

class Jdoca : public JBosBank {
  clasDOCA_t* p;
public:
  Jdoca (int i) : JBosBank() {
    p = (clasDOCA_t*) getGroup(&bcs_, "DOCA", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  doca_t& operator[] (int i) {
    CheckIndex(i);
    return p->doca[i];
  }
};

class Jdpsp : public JBosBank {
  clasDPSP_t* p;
public:
  Jdpsp (int i) : JBosBank() {
    p = (clasDPSP_t*) getGroup(&bcs_, "DPSP", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  dpsp_t& operator[] (int i) {
    CheckIndex(i);
    return p->dpsp[i];
  }
};

class Jdspc : public JBosBank {
  clasDSPC_t* p;
public:
  Jdspc (int i) : JBosBank() {
    p = (clasDSPC_t*) getGroup(&bcs_, "DSPC", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  dspc_t& operator[] (int i) {
    CheckIndex(i);
    return p->dspc[i];
  }
};

class Jdsps : public JBosBank {
  clasDSPS_t* p;
public:
  Jdsps (int i) : JBosBank() {
    p = (clasDSPS_t*) getGroup(&bcs_, "DSPS", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  dsps_t& operator[] (int i) {
    CheckIndex(i);
    return p->dsps[i];
  }
};

class Jdstc : public JBosBank {
  clasDSTC_t* p;
public:
  Jdstc (int i) : JBosBank() {
    p = (clasDSTC_t*) getGroup(&bcs_, "DSTC", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  dstc_t& operator[] (int i) {
    CheckIndex(i);
    return p->dstc[i];
  }
};

class Jdtrk : public JBosBank {
  clasDTRK_t* p;
public:
  Jdtrk (int i) : JBosBank() {
    p = (clasDTRK_t*) getGroup(&bcs_, "DTRK", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  dtrk_t& operator[] (int i) {
    CheckIndex(i);
    return p->dtrk[i];
  }
};

class Jec : public JBosBank {
  clasEC_t* p;
public:
  Jec (int i) : JBosBank() {
    p = (clasEC_t*) getGroup(&bcs_, "EC  ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ec_t& operator[] (int i) {
    CheckIndex(i);
    return p->ec[i];
  }
};

class Jec01 : public JBosBank {
  clasEC01_t* p;
public:
  Jec01 (int i) : JBosBank() {
    p = (clasEC01_t*) getGroup(&bcs_, "EC01", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ec01_t& operator[] (int i) {
    CheckIndex(i);
    return p->ec01[i];
  }
};

class Jec1 : public JBosBank {
  clasEC1_t* p;
public:
  Jec1 (int i) : JBosBank() {
    p = (clasEC1_t*) getGroup(&bcs_, "EC1 ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ec1_t& operator[] (int i) {
    CheckIndex(i);
    return p->ec1[i];
  }
};

class Jec1p : public JBosBank {
  clasEC1P_t* p;
public:
  Jec1p (int i) : JBosBank() {
    p = (clasEC1P_t*) getGroup(&bcs_, "EC1P", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ec1p_t& operator[] (int i) {
    CheckIndex(i);
    return p->ec1p[i];
  }
};

class Jec1r : public JBosBank {
  clasEC1R_t* p;
public:
  Jec1r (int i) : JBosBank() {
    p = (clasEC1R_t*) getGroup(&bcs_, "EC1R", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ec1r_t& operator[] (int i) {
    CheckIndex(i);
    return p->ec1r[i];
  }
};

class Jecca : public JBosBank {
  clasECCA_t* p;
public:
  Jecca (int i) : JBosBank() {
    p = (clasECCA_t*) getGroup(&bcs_, "ECCA", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ecca_t& operator[] (int i) {
    CheckIndex(i);
    return p->ecca[i];
  }
};

class Jeccl : public JBosBank {
  clasECCL_t* p;
public:
  Jeccl (int i) : JBosBank() {
    p = (clasECCL_t*) getGroup(&bcs_, "ECCL", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  eccl_t& operator[] (int i) {
    CheckIndex(i);
    return p->eccl[i];
  }
};

class Jecct : public JBosBank {
  clasECCT_t* p;
public:
  Jecct (int i) : JBosBank() {
    p = (clasECCT_t*) getGroup(&bcs_, "ECCT", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ecct_t& operator[] (int i) {
    CheckIndex(i);
    return p->ecct[i];
  }
};

class Jecdi : public JBosBank {
  clasECDI_t* p;
public:
  Jecdi (int i) : JBosBank() {
    p = (clasECDI_t*) getGroup(&bcs_, "ECDI", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ecdi_t& operator[] (int i) {
    CheckIndex(i);
    return p->ecdi[i];
  }
};

class Jecg : public JBosBank {
  clasECG_t* p;
public:
  Jecg (int i) : JBosBank() {
    p = (clasECG_t*) getGroup(&bcs_, "ECG ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ecg_t& operator[] (int i) {
    CheckIndex(i);
    return p->ecg[i];
  }
};

class Jech : public JBosBank {
  clasECH_t* p;
public:
  Jech (int i) : JBosBank() {
    p = (clasECH_t*) getGroup(&bcs_, "ECH ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ech_t& operator[] (int i) {
    CheckIndex(i);
    return p->ech[i];
  }
};

class Jechb : public JBosBank {
  clasECHB_t* p;
public:
  Jechb (int i) : JBosBank() {
    p = (clasECHB_t*) getGroup(&bcs_, "ECHB", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  echb_t& operator[] (int i) {
    CheckIndex(i);
    return p->echb[i];
  }
};

class Jecmt : public JBosBank {
  clasECMT_t* p;
public:
  Jecmt (int i) : JBosBank() {
    p = (clasECMT_t*) getGroup(&bcs_, "ECMT", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ecmt_t& operator[] (int i) {
    CheckIndex(i);
    return p->ecmt[i];
  }
};

class Jecp : public JBosBank {
  clasECP_t* p;
public:
  Jecp (int i) : JBosBank() {
    p = (clasECP_t*) getGroup(&bcs_, "ECP ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ecp_t& operator[] (int i) {
    CheckIndex(i);
    return p->ecp[i];
  }
};

class Jecp1 : public JBosBank {
  clasECP1_t* p;
public:
  Jecp1 (int i) : JBosBank() {
    p = (clasECP1_t*) getGroup(&bcs_, "ECP1", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ecp1_t& operator[] (int i) {
    CheckIndex(i);
    return p->ecp1[i];
  }
};

class Jecpb : public JBosBank {
  clasECPB_t* p;
public:
  Jecpb () : JBosBank() {
    p = (clasECPB_t*) getBank(&bcs_, "ECPB");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ecpb_t& operator[] (int i) {
    CheckIndex(i);
    return p->ecpb[i];
  }
};

class Jecpi : public JBosBank {
  clasECPI_t* p;
public:
  Jecpi (int i) : JBosBank() {
    p = (clasECPI_t*) getGroup(&bcs_, "ECPI", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ecpi_t& operator[] (int i) {
    CheckIndex(i);
    return p->ecpi[i];
  }
};

class Jecpo : public JBosBank {
  clasECPO_t* p;
public:
  Jecpo (int i) : JBosBank() {
    p = (clasECPO_t*) getGroup(&bcs_, "ECPO", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ecpo_t& operator[] (int i) {
    CheckIndex(i);
    return p->ecpo[i];
  }
};

class Jecrb : public JBosBank {
  clasECRB_t* p;
public:
  Jecrb (int i) : JBosBank() {
    p = (clasECRB_t*) getGroup(&bcs_, "ECRB", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ecrb_t& operator[] (int i) {
    CheckIndex(i);
    return p->ecrb[i];
  }
};

class Jecs : public JBosBank {
  clasECS_t* p;
public:
  Jecs (int i) : JBosBank() {
    p = (clasECS_t*) getGroup(&bcs_, "ECS ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ecs_t& operator[] (int i) {
    CheckIndex(i);
    return p->ecs[i];
  }
};

class Jeid0 : public JBosBank {
  clasEID0_t* p;
public:
  Jeid0 (int i) : JBosBank() {
    p = (clasEID0_t*) getGroup(&bcs_, "EID0", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  eid0_t& operator[] (int i) {
    CheckIndex(i);
    return p->eid0[i];
  }
};

class Jepic : public JBosBank {
  clasEPIC_t* p;
public:
  Jepic (int i) : JBosBank() {
    p = (clasEPIC_t*) getGroup(&bcs_, "EPIC", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  epic_t& operator[] (int i) {
    CheckIndex(i);
    return p->epic[i];
  }
};

class Jevnt : public JBosBank {
  clasEVNT_t* p;
public:
  Jevnt () : JBosBank() {
    p = (clasEVNT_t*) getBank(&bcs_, "EVNT");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  evnt_t& operator[] (int i) {
    CheckIndex(i);
    return p->evnt[i];
  }
};

class Jfbpm : public JBosBank {
  clasFBPM_t* p;
public:
  Jfbpm (int i) : JBosBank() {
    p = (clasFBPM_t*) getGroup(&bcs_, "FBPM", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  fbpm_t& operator[] (int i) {
    CheckIndex(i);
    return p->fbpm[i];
  }
};

class Jgpar : public JBosBank {
  clasGPAR_t* p;
public:
  Jgpar (int i) : JBosBank() {
    p = (clasGPAR_t*) getGroup(&bcs_, "GPAR", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  gpar_t& operator[] (int i) {
    CheckIndex(i);
    return p->gpar[i];
  }
};

class Jg1sl : public JBosBank {
  clasG1SL_t* p;
public:
  Jg1sl (int i) : JBosBank() {
    p = (clasG1SL_t*) getGroup(&bcs_, "G1SL", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  g1sl_t& operator[] (int i) {
    CheckIndex(i);
    return p->g1sl[i];
  }
};

class Jg2sl : public JBosBank {
  clasG2SL_t* p;
public:
  Jg2sl (int i) : JBosBank() {
    p = (clasG2SL_t*) getGroup(&bcs_, "G2SL", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  g2sl_t& operator[] (int i) {
    CheckIndex(i);
    return p->g2sl[i];
  }
};

class Jg3sl : public JBosBank {
  clasG3SL_t* p;
public:
  Jg3sl (int i) : JBosBank() {
    p = (clasG3SL_t*) getGroup(&bcs_, "G3SL", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  g3sl_t& operator[] (int i) {
    CheckIndex(i);
    return p->g3sl[i];
  }
};

class Jg4sl : public JBosBank {
  clasG4SL_t* p;
public:
  Jg4sl (int i) : JBosBank() {
    p = (clasG4SL_t*) getGroup(&bcs_, "G4SL", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  g4sl_t& operator[] (int i) {
    CheckIndex(i);
    return p->g4sl[i];
  }
};

class Jgpid : public JBosBank {
  clasGPID_t* p;
public:
  Jgpid () : JBosBank() {
    p = (clasGPID_t*) getBank(&bcs_, "GPID");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  gpid_t& operator[] (int i) {
    CheckIndex(i);
    return p->gpid[i];
  }
};

class Jhber : public JBosBank {
  clasHBER_t* p;
public:
  Jhber (int i) : JBosBank() {
    p = (clasHBER_t*) getGroup(&bcs_, "HBER", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  hber_t& operator[] (int i) {
    CheckIndex(i);
    return p->hber[i];
  }
};

class Jhbid : public JBosBank {
  clasHBID_t* p;
public:
  Jhbid (int i) : JBosBank() {
    p = (clasHBID_t*) getGroup(&bcs_, "HBID", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  hbid_t& operator[] (int i) {
    CheckIndex(i);
    return p->hbid[i];
  }
};

class Jhbla : public JBosBank {
  clasHBLA_t* p;
public:
  Jhbla (int i) : JBosBank() {
    p = (clasHBLA_t*) getGroup(&bcs_, "HBLA", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  hbla_t& operator[] (int i) {
    CheckIndex(i);
    return p->hbla[i];
  }
};

class Jhbtb : public JBosBank {
  clasHBTB_t* p;
public:
  Jhbtb (int i) : JBosBank() {
    p = (clasHBTB_t*) getGroup(&bcs_, "HBTB", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  hbtb_t& operator[] (int i) {
    CheckIndex(i);
    return p->hbtb[i];
  }
};

class Jhbtr : public JBosBank {
  clasHBTR_t* p;
public:
  Jhbtr () : JBosBank() {
    p = (clasHBTR_t*) getBank(&bcs_, "HBTR");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  hbtr_t& operator[] (int i) {
    CheckIndex(i);
    return p->hbtr[i];
  }
};

class Jhcal : public JBosBank {
  clasHCAL_t* p;
public:
  Jhcal (int i) : JBosBank() {
    p = (clasHCAL_t*) getGroup(&bcs_, "HCAL", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  hcal_t& operator[] (int i) {
    CheckIndex(i);
    return p->hcal[i];
  }
};

class Jhdpl : public JBosBank {
  clasHDPL_t* p;
public:
  Jhdpl (int i) : JBosBank() {
    p = (clasHDPL_t*) getGroup(&bcs_, "HDPL", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  hdpl_t& operator[] (int i) {
    CheckIndex(i);
    return p->hdpl[i];
  }
};

class Jhead : public JBosBank {
  clasHEAD_t* p;
public:
  Jhead () : JBosBank() {
    p = (clasHEAD_t*) getBank(&bcs_, "HEAD");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  head_t& operator[] (int i) {
    CheckIndex(i);
    return p->head[i];
  }
};

class Jhevt : public JBosBank {
  clasHEVT_t* p;
public:
  Jhevt () : JBosBank() {
    p = (clasHEVT_t*) getBank(&bcs_, "HEVT");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  hevt_t& operator[] (int i) {
    CheckIndex(i);
    return p->hevt[i];
  }
};

class Jhls : public JBosBank {
  clasHLS_t* p;
public:
  Jhls (int i) : JBosBank() {
    p = (clasHLS_t*) getGroup(&bcs_, "HLS ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  hls_t& operator[] (int i) {
    CheckIndex(i);
    return p->hls[i];
  }
};

class Jlcdi : public JBosBank {
  clasLCDI_t* p;
public:
  Jlcdi (int i) : JBosBank() {
    p = (clasLCDI_t*) getGroup(&bcs_, "LCDI", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  lcdi_t& operator[] (int i) {
    CheckIndex(i);
    return p->lcdi[i];
  }
};

class Jlasr : public JBosBank {
  clasLASR_t* p;
public:
  Jlasr (int i) : JBosBank() {
    p = (clasLASR_t*) getGroup(&bcs_, "LASR", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  lasr_t& operator[] (int i) {
    CheckIndex(i);
    return p->lasr[i];
  }
};

class Jlcpb : public JBosBank {
  clasLCPB_t* p;
public:
  Jlcpb () : JBosBank() {
    p = (clasLCPB_t*) getBank(&bcs_, "LCPB");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  lcpb_t& operator[] (int i) {
    CheckIndex(i);
    return p->lcpb[i];
  }
};

class Jlowq : public JBosBank {
  clasLOWQ_t* p;
public:
  Jlowq (int i) : JBosBank() {
    p = (clasLOWQ_t*) getGroup(&bcs_, "LOWQ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  lowq_t& operator[] (int i) {
    CheckIndex(i);
    return p->lowq[i];
  }
};

class Jkfit : public JBosBank {
  clasKFIT_t* p;
public:
  Jkfit (int i) : JBosBank() {
    p = (clasKFIT_t*) getGroup(&bcs_, "KFIT", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  kfit_t& operator[] (int i) {
    CheckIndex(i);
    return p->kfit[i];
  }
};

class Jmcev : public JBosBank {
  clasMCEV_t* p;
public:
  Jmcev (int i) : JBosBank() {
    p = (clasMCEV_t*) getGroup(&bcs_, "MCEV", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  mcev_t& operator[] (int i) {
    CheckIndex(i);
    return p->mcev[i];
  }
};

class Jmchd : public JBosBank {
  clasMCHD_t* p;
public:
  Jmchd (int i) : JBosBank() {
    p = (clasMCHD_t*) getGroup(&bcs_, "MCHD", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  mchd_t& operator[] (int i) {
    CheckIndex(i);
    return p->mchd[i];
  }
};

class Jmctk : public JBosBank {
  clasMCTK_t* p;
public:
  Jmctk () : JBosBank() {
    p = (clasMCTK_t*) getBank(&bcs_, "MCTK");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  mctk_t& operator[] (int i) {
    CheckIndex(i);
    return p->mctk[i];
  }
};

class Jmcvx : public JBosBank {
  clasMCVX_t* p;
public:
  Jmcvx (int i) : JBosBank() {
    p = (clasMCVX_t*) getGroup(&bcs_, "MCVX", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  mcvx_t& operator[] (int i) {
    CheckIndex(i);
    return p->mcvx[i];
  }
};

class Jpart : public JBosBank {
  clasPART_t* p;
public:
  Jpart (int i) : JBosBank() {
    p = (clasPART_t*) getGroup(&bcs_, "PART", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  part_t& operator[] (int i) {
    CheckIndex(i);
    return p->part[i];
  }
};

class Jpco : public JBosBank {
  clasPCO_t* p;
public:
  Jpco (int i) : JBosBank() {
    p = (clasPCO_t*) getGroup(&bcs_, "PCO ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  pco_t& operator[] (int i) {
    CheckIndex(i);
    return p->pco[i];
  }
};

class Jpso : public JBosBank {
  clasPSO_t* p;
public:
  Jpso (int i) : JBosBank() {
    p = (clasPSO_t*) getGroup(&bcs_, "PSO ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  pso_t& operator[] (int i) {
    CheckIndex(i);
    return p->pso[i];
  }
};

class Jptdb : public JBosBank {
  clasPTDB_t* p;
public:
  Jptdb (int i) : JBosBank() {
    p = (clasPTDB_t*) getGroup(&bcs_, "PTDB", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  ptdb_t& operator[] (int i) {
    CheckIndex(i);
    return p->ptdb[i];
  }
};

class Jrf : public JBosBank {
  clasRF_t* p;
public:
  Jrf (int i) : JBosBank() {
    p = (clasRF_t*) getGroup(&bcs_, "RF  ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  rf_t& operator[] (int i) {
    CheckIndex(i);
    return p->rf[i];
  }
};

#ifndef OLDBOS
class Jrft : public JBosBank {
  clasRFT_t* p;
public:
  Jrft () : JBosBank() {
    p = (clasRFT_t*) getBank(&bcs_, "RFT ");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  rft_t& operator[] (int i) {
    CheckIndex(i);
    return p->rft[i];
  }
};
#endif



class Jrglk : public JBosBank {
  clasRGLK_t* p;
public:
  Jrglk (int i) : JBosBank() {
    p = (clasRGLK_t*) getGroup(&bcs_, "RGLK", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  rglk_t& operator[] (int i) {
    CheckIndex(i);
    return p->rglk[i];
  }
};

class Jrunc : public JBosBank {
  clasRUNC_t* p;
public:
  Jrunc () : JBosBank() {
    p = (clasRUNC_t*) getBank(&wcs_, "RUNC");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  runc_t& operator[] (int i) {
    CheckIndex(i);
    return p->runc;
  }
};

class Jrtsl : public JBosBank {
  clasRTSL_t* p;
public:
  Jrtsl (int i) : JBosBank() {
    p = (clasRTSL_t*) getGroup(&bcs_, "RTSL", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  rtsl_t& operator[] (int i) {
    CheckIndex(i);
    return p->rtsl[i];
  }
};

class Jsc : public JBosBank {
  clasSC_t* p;
public:
  Jsc (int i) : JBosBank() {
    p = (clasSC_t*) getGroup(&bcs_, "SC  ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  sc_t& operator[] (int i) {
    CheckIndex(i);
    return p->sc[i];
  }
};

class Jsc1 : public JBosBank {
  clasSC1_t* p;
public:
  Jsc1 (int i) : JBosBank() {
    p = (clasSC1_t*) getGroup(&bcs_, "SC1 ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  sc1_t& operator[] (int i) {
    CheckIndex(i);
    return p->sc1[i];
  }
};

class Jscc : public JBosBank {
  clasSCC_t* p;
public:
  Jscc (int i) : JBosBank() {
    p = (clasSCC_t*) getGroup(&bcs_, "SCC ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  scc_t& operator[] (int i) {
    CheckIndex(i);
    return p->scc[i];
  }
};

class Jscg : public JBosBank {
  clasSCG_t* p;
public:
  Jscg (int i) : JBosBank() {
    p = (clasSCG_t*) getGroup(&wcs_, "SCG ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  scg_t& operator[] (int i) {
    CheckIndex(i);
    return p->scg[i];
  }
};

class Jsch : public JBosBank {
  clasSCH_t* p;
public:
  Jsch (int i) : JBosBank() {
    p = (clasSCH_t*) getGroup(&bcs_, "SCH ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  sch_t& operator[] (int i) {
    CheckIndex(i);
    return p->sch[i];
  }
};

class Jscmd : public JBosBank {
  clasSCMD_t* p;
public:
  Jscmd (int i) : JBosBank() {
    p = (clasSCMD_t*) getGroup(&bcs_, "SCMD", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  scmd_t& operator[] (int i) {
    CheckIndex(i);
    return p->scmd[i];
  }
};

class Jscp : public JBosBank {
  clasSCP_t* p;
public:
  Jscp (int i) : JBosBank() {
    p = (clasSCP_t*) getGroup(&bcs_, "SCP ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  scp_t& operator[] (int i) {
    CheckIndex(i);
    return p->scp[i];
  }
};

class Jscpb : public JBosBank {
  clasSCPB_t* p;
public:
  Jscpb () : JBosBank() {
    p = (clasSCPB_t*) getBank(&bcs_, "SCPB");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  scpb_t& operator[] (int i) {
    CheckIndex(i);
    return p->scpb[i];
  }
};

class Jscpe : public JBosBank {
  clasSCPE_t* p;
public:
  Jscpe (int i) : JBosBank() {
    p = (clasSCPE_t*) getGroup(&bcs_, "SCPE", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  scpe_t& operator[] (int i) {
    CheckIndex(i);
    return p->scpe[i];
  }
};

class Jscr : public JBosBank {
  clasSCR_t* p;
public:
  Jscr (int i) : JBosBank() {
    p = (clasSCR_t*) getGroup(&bcs_, "SCR ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  scr_t& operator[] (int i) {
    CheckIndex(i);
    return p->scr[i];
  }
};

class Jscs : public JBosBank {
  clasSCS_t* p;
public:
  Jscs (int i) : JBosBank() {
    p = (clasSCS_t*) getGroup(&bcs_, "SCS ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  scs_t& operator[] (int i) {
    CheckIndex(i);
    return p->scs[i];
  }
};

class Jscdi : public JBosBank {
  clasSCDI_t* p;
public:
  Jscdi (int i) : JBosBank() {
    p = (clasSCDI_t*) getGroup(&bcs_, "SCDI", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  scdi_t& operator[] (int i) {
    CheckIndex(i);
    return p->scdi[i];
  }
};

class Jscmt : public JBosBank {
  clasSCMT_t* p;
public:
  Jscmt (int i) : JBosBank() {
    p = (clasSCMT_t*) getGroup(&bcs_, "SCMT", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  scmt_t& operator[] (int i) {
    CheckIndex(i);
    return p->scmt[i];
  }
};

class Jscmw : public JBosBank {
  clasSCMW_t* p;
public:
  Jscmw (int i) : JBosBank() {
    p = (clasSCMW_t*) getGroup(&bcs_, "SCMW", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  scmw_t& operator[] (int i) {
    CheckIndex(i);
    return p->scmw[i];
  }
};

class Jscrc : public JBosBank {
  clasSCRC_t* p;
public:
  Jscrc (int i) : JBosBank() {
    p = (clasSCRC_t*) getGroup(&bcs_, "SCRC", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  scrc_t& operator[] (int i) {
    CheckIndex(i);
    return p->scrc[i];
  }
};

#ifndef OLDBOS
class Jsct : public JBosBank {
  clasSCT_t* p;
public:
  Jsct (int i) : JBosBank() {
    p = (clasSCT_t*) getGroup(&bcs_, "SCT ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  sct_t& operator[] (int i) {
    CheckIndex(i);
    return p->sct[i];
  }
};
#endif
class Jspar : public JBosBank {
  clasSPAR_t* p;
public:
  Jspar (int i) : JBosBank() {
    p = (clasSPAR_t*) getGroup(&bcs_, "SPAR", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  spar_t& operator[] (int i) {
    CheckIndex(i);
    return p->spar[i];
  }
};

class Jst : public JBosBank {
  clasST_t* p;
public:
  Jst (int i) : JBosBank() {
    p = (clasST_t*) getGroup(&bcs_, "ST  ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  st_t& operator[] (int i) {
    CheckIndex(i);
    return p->st[i];
  }
};

class Jst1 : public JBosBank {
  clasST1_t* p;
public:
  Jst1 () : JBosBank() {
    p = (clasST1_t*) getBank(&bcs_, "ST1 ");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  st1_t& operator[] (int i) {
    CheckIndex(i);
    return p->st1[i];
  }
};

class Jstn0 : public JBosBank {
  clasSTN0_t* p;
public:
  Jstn0 () : JBosBank() {
    p = (clasSTN0_t*) getBank(&bcs_, "STN0");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  stn0_t& operator[] (int i) {
    CheckIndex(i);
    return p->stn0[i];
  }
};

class Jstn1 : public JBosBank {
  clasSTN1_t* p;
public:
  Jstn1 () : JBosBank() {
    p = (clasSTN1_t*) getBank(&bcs_, "STN1");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  stn1_t& operator[] (int i) {
    CheckIndex(i);
    return p->stn1[i];
  }
};

class Jstg : public JBosBank {
  clasSTG_t* p;
public:
  Jstg (int i) : JBosBank() {
    p = (clasSTG_t*) getGroup(&bcs_, "STG ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  stg_t& operator[] (int i) {
    CheckIndex(i);
    return p->stg[i];
  }
};

class Jsth : public JBosBank {
  clasSTH_t* p;
public:
  Jsth (int i) : JBosBank() {
    p = (clasSTH_t*) getGroup(&bcs_, "STH ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  sth_t& operator[] (int i) {
    CheckIndex(i);
    return p->sth[i];
  }
};

class Jstpe : public JBosBank {
  clasSTPE_t* p;
public:
  Jstpe (int i) : JBosBank() {
    p = (clasSTPE_t*) getGroup(&bcs_, "STPE", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  stpe_t& operator[] (int i) {
    CheckIndex(i);
    return p->stpe[i];
  }
};

class Jstpb : public JBosBank {
  clasSTPB_t* p;
public:
  Jstpb () : JBosBank() {
    p = (clasSTPB_t*) getBank(&bcs_, "STPB");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  stpb_t& operator[] (int i) {
    CheckIndex(i);
    return p->stpb[i];
  }
};

class Jstr : public JBosBank {
  clasSTR_t* p;
public:
  Jstr () : JBosBank() {
    p = (clasSTR_t*) getBank(&bcs_, "STR ");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  str_t& operator[] (int i) {
    CheckIndex(i);
    return p->str[i];
  }
};

class Jsts : public JBosBank {
  clasSTS_t* p;
public:
  Jsts (int i) : JBosBank() {
    p = (clasSTS_t*) getGroup(&bcs_, "STS ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  sts_t& operator[] (int i) {
    CheckIndex(i);
    return p->sts[i];
  }
};

class Js1st : public JBosBank {
  clasS1ST_t* p;
public:
  Js1st (int i) : JBosBank() {
    p = (clasS1ST_t*) getGroup(&bcs_, "S1ST", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  s1st_t& operator[] (int i) {
    CheckIndex(i);
    return p->s1st[i];
  }
};

class Jsync : public JBosBank {
  clasSYNC_t* p;
public:
  Jsync (int i) : JBosBank() {
    p = (clasSYNC_t*) getGroup(&bcs_, "SYNC", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  sync_t& operator[] (int i) {
    CheckIndex(i);
    return p->sync[i];
  }
};

class Jtaco : public JBosBank {
  clasTACO_t* p;
public:
  Jtaco (int i) : JBosBank() {
    p = (clasTACO_t*) getGroup(&bcs_, "TACO", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  taco_t& operator[] (int i) {
    CheckIndex(i);
    return p->taco[i];
  }
};

class Jtage : public JBosBank {
  clasTAGE_t* p;
public:
  Jtage () : JBosBank() {
    p = (clasTAGE_t*) getBank(&bcs_, "TAGE");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  tage_t& operator[] (int i) {
    CheckIndex(i);
    return p->tage[i];
  }
};

class Jtagi : public JBosBank {
  clasTAGI_t* p;
public:
  Jtagi (int i) : JBosBank() {
    p = (clasTAGI_t*) getGroup(&bcs_, "TAGI", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  tagi_t& operator[] (int i) {
    CheckIndex(i);
    return p->tagi[i];
  }
};

class Jtagm : public JBosBank {
  clasTAGM_t* p;
public:
  Jtagm (int i) : JBosBank() {
    p = (clasTAGM_t*) getGroup(&bcs_, "TAGM", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  tagm_t& operator[] (int i) {
    CheckIndex(i);
    return p->tagm[i];
  }
};

class Jtagr : public JBosBank {
  clasTAGR_t* p;
public:
  Jtagr () : JBosBank() {
    p = (clasTAGR_t*) getBank(&bcs_, "TAGR");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  tagr_t& operator[] (int i) {
    CheckIndex(i);
    return p->tagr[i];
  }
};

class Jtagt : public JBosBank {
  clasTAGT_t* p;
public:
  Jtagt (int i) : JBosBank() {
    p = (clasTAGT_t*) getGroup(&bcs_, "TAGT", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  tagt_t& operator[] (int i) {
    CheckIndex(i);
    return p->tagt[i];
  }
};

class Jtgtl : public JBosBank {
  clasTGTL_t* p;
public:
  Jtgtl () : JBosBank() {
    p = (clasTGTL_t*) getBank(&bcs_, "TGTL");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  tgtl_t& operator[] (int i) {
    CheckIndex(i);
    return p->tgtl[i];
  }
};

class Jtgtr : public JBosBank {
  clasTGTR_t* p;
public:
  Jtgtr () : JBosBank() {
    p = (clasTGTR_t*) getBank(&bcs_, "TGTR");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  tgtr_t& operator[] (int i) {
    CheckIndex(i);
    return p->tgtr[i];
  }
};

class Jtber : public JBosBank {
  clasTBER_t* p;
public:
  Jtber (int i) : JBosBank() {
    p = (clasTBER_t*) getGroup(&bcs_, "TBER", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  tber_t& operator[] (int i) {
    CheckIndex(i);
    return p->tber[i];
  }
};

class Jtbid : public JBosBank {
  clasTBID_t* p;
public:
  Jtbid (int i) : JBosBank() {
    p = (clasTBID_t*) getGroup(&bcs_, "TBID", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  tbid_t& operator[] (int i) {
    CheckIndex(i);
    return p->tbid[i];
  }
};

class Jtbla : public JBosBank {
  clasTBLA_t* p;
public:
  Jtbla (int i) : JBosBank() {
    p = (clasTBLA_t*) getGroup(&bcs_, "TBLA", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  tbla_t& operator[] (int i) {
    CheckIndex(i);
    return p->tbla[i];
  }
};

class Jtbtr : public JBosBank {
  clasTBTR_t* p;
public:
  Jtbtr () : JBosBank() {
    p = (clasTBTR_t*) getBank(&bcs_, "TBTR");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  tbtr_t& operator[] (int i) {
    CheckIndex(i);
    return p->tbtr[i];
  }
};

class Jtcsb : public JBosBank {
  clasTCSB_t* p;
public:
  Jtcsb (int i) : JBosBank() {
    p = (clasTCSB_t*) getGroup(&bcs_, "TCSB", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  tcsb_t& operator[] (int i) {
    CheckIndex(i);
    return p->tcsb[i];
  }
};

class Jtdpl : public JBosBank {
  clasTDPL_t* p;
public:
  Jtdpl (int i) : JBosBank() {
    p = (clasTDPL_t*) getGroup(&bcs_, "TDPL", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  tdpl_t& operator[] (int i) {
    CheckIndex(i);
    return p->tdpl[i];
  }
};

class Jtesc : public JBosBank {
  clasTESC_t* p;
public:
  Jtesc (int i) : JBosBank() {
    p = (clasTESC_t*) getGroup(&bcs_, "TESC", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  tesc_t& operator[] (int i) {
    CheckIndex(i);
    return p->tesc[i];
  }
};

class Jtgbi : public JBosBank {
  clasTGBI_t* p;
public:
  Jtgbi (int i) : JBosBank() {
    p = (clasTGBI_t*) getGroup(&bcs_, "TGBI", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  tgbi_t& operator[] (int i) {
    CheckIndex(i);
    return p->tgbi[i];
  }
};

class Jtgpb : public JBosBank {
  clasTGPB_t* p;
public:
  Jtgpb (int i) : JBosBank() {
    p = (clasTGPB_t*) getGroup(&bcs_, "TGPB", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  tgpb_t& operator[] (int i) {
    CheckIndex(i);
    return p->tgpb[i];
  }
};

class Jtgeo : public JBosBank {
  clasTGEO_t* p;
public:
  Jtgeo () : JBosBank() {
    p = (clasTGEO_t*) getBank(&wcs_, "TGEO");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  tgeo_t& operator[] (int i) {
    CheckIndex(i);
    return p->tgeo[i];
  }
};

class Jtgs : public JBosBank {
  clasTGS_t* p;
public:
  Jtgs (int i) : JBosBank() {
    p = (clasTGS_t*) getGroup(&bcs_, "TGS ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  tgs_t& operator[] (int i) {
    CheckIndex(i);
    return p->tgs[i];
  }
};

class Jtrgs : public JBosBank {
  clasTRGS_t* p;
public:
  Jtrgs (int i) : JBosBank() {
    p = (clasTRGS_t*) getGroup(&bcs_, "TRGS", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  trgs_t& operator[] (int i) {
    CheckIndex(i);
    return p->trgs[i];
  }
};

class Jtrks : public JBosBank {
  clasTRKS_t* p;
public:
  Jtrks (int i) : JBosBank() {
    p = (clasTRKS_t*) getGroup(&bcs_, "TRKS", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  trks_t& operator[] (int i) {
    CheckIndex(i);
    return p->trks[i];
  }
};

class Jtrl1 : public JBosBank {
  clasTRL1_t* p;
public:
  Jtrl1 (int i) : JBosBank() {
    p = (clasTRL1_t*) getGroup(&bcs_, "TRL1", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  trl1_t& operator[] (int i) {
    CheckIndex(i);
    return p->trl1[i];
  }
};

class Jtrgt : public JBosBank {
  clasTRGT_t* p;
public:
  Jtrgt (int i) : JBosBank() {
    p = (clasTRGT_t*) getGroup(&bcs_, "TRGT", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  trgt_t& operator[] (int i) {
    CheckIndex(i);
    return p->trgt[i];
  }
};

class Junus : public JBosBank {
  clasUNUS_t* p;
public:
  Junus (int i) : JBosBank() {
    p = (clasUNUS_t*) getGroup(&bcs_, "UNUS", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  unus_t& operator[] (int i) {
    CheckIndex(i);
    return p->unus[i];
  }
};

class Jvert : public JBosBank {
  clasVERT_t* p;
public:
  Jvert (int i) : JBosBank() {
    p = (clasVERT_t*) getGroup(&bcs_, "VERT", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  vert_t& operator[] (int i) {
    CheckIndex(i);
    return p->vert[i];
  }
};

class Jmvrt : public JBosBank {
  clasMVRT_t* p;
public:
  Jmvrt (int i) : JBosBank() {
    p = (clasMVRT_t*) getGroup(&bcs_, "MVRT", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  mvrt_t& operator[] (int i) {
    CheckIndex(i);
    return p->mvrt[i];
  }
};

class Jmtrk : public JBosBank {
  clasMTRK_t* p;
public:
  Jmtrk (int i) : JBosBank() {
    p = (clasMTRK_t*) getGroup(&bcs_, "MTRK", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  mtrk_t& operator[] (int i) {
    CheckIndex(i);
    return p->mtrk[i];
  }
};

class Jsgmp : public JBosBank {
  clasSGMP_t* p;
public:
  Jsgmp (int i) : JBosBank() {
    p = (clasSGMP_t*) getGroup(&bcs_, "SGMP", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  sgmp_t& operator[] (int i) {
    CheckIndex(i);
    return p->sgmp[i];
  }
};

class Jclst : public JBosBank {
  clasCLST_t* p;
public:
  Jclst (int i) : JBosBank() {
    p = (clasCLST_t*) getGroup(&bcs_, "CLST", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  clst_t& operator[] (int i) {
    CheckIndex(i);
    return p->clst[i];
  }
};

class Jtlv1 : public JBosBank {
  clasTLV1_t* p;
public:
  Jtlv1 (int i) : JBosBank() {
    p = (clasTLV1_t*) getGroup(&bcs_, "TLV1", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  tlv1_t& operator[] (int i) {
    CheckIndex(i);
    return p->tlv1[i];
  }
};


///----------end C++ wrapper around bosbanks--------------------

///----------------- sample how to use -------------------------

  // === Sample: get run number from HEAD bank
  /*
  Jhead head;
  runNumber = head[0].nrun;
  */  

  // === Sample: list the particle-id from EVNT bank
  /*
  Jevnt evnt;
  if (evnt.Found()) {               
    for (int i=0; i<evnt.GetNrow(); i++) {
      std::cout << evnt[i].id << endl;
    }
  }
  */

  // === Sample: find the z position of the intersect between
  //             a given track (within a sector) with a plane
  /*
  int track;  // the track index (e.g. from dcpb) [1..n]
  int sector; // the given sector [1..6]
  int plane;  // the plane (intersect with ST, CC, SC or EC) [1..10];
  Jtbtr tbtr;
  Jtdpl tdpl(sector);                   // the sector number is the key to get the bank  
  if (tbtr.Found() && tdpl.Found()) {   // proceed if both banks are existing
    int indexTdpl =                     // get the index for the TDPL bank ...
      tbtr[track-1].itr_sec % 100 - 1;  // ... which are the last to digits from itr_sec
                                        // subtract 1 to use as C++ index
    double zHitPosition =               // get the z position ... 
      tdpl[indexTdpl*10+plane-1].pos.z; // ... according to the tdpl enconing scheme
  }
  */
