#include <stddef.h>
#include <string.h>
#include "clasbanks.h"
extern "C" {
#include "clas_common.h"
}

int get_bos_index(int array_key, char *bankname, int bank_index);


BEAM::BEAM(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "BEAM");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (BEAM_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

BMPR::BMPR(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "BMPR");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (BMPR_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

BREP::BREP(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "BREP");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (BREP_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

CALB::CALB(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "CALB");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (CALB_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

CALL::CALL(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "CALL");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (CALL_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

CC::CC(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "CC");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (CC_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

CC01::CC01(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "CC01");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (CC01_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

CC1::CC1(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "CC1");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (CC1_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

CCDI::CCDI(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "CCDI");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (CCDI_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

CCH::CCH(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "CCH");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (CCH_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

CCMT::CCMT(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "CCMT");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (CCMT_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

CCPB::CCPB(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "CCPB");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (CCPB_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

CCPE::CCPE(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "CCPE");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (CCPE_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

CCRC::CCRC(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "CCRC");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (CCRC_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

CCS::CCS(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "CCS");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (CCS_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

CCT::CCT(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "CCT");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (CCT_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

CHI2::CHI2(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "CHI2");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (CHI2_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

CL01::CL01(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "CL01");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (CL01_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

CLST::CLST(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "CLST");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (CLST_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

CPED::CPED(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "CPED");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (CPED_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

CSQL::CSQL(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "CSQL");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (CSQL_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DC0::DC0(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DC0");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (DC0_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DC1::DC1(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DC1");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (DC1_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DCDW::DCDW(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DCDW");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (DCDW_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DCGM::DCGM(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DCGM");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (DCGM_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DCGW::DCGW(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DCGW");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (DCGW_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DCH::DCH(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DCH");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (DCH_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DCMN::DCMN(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DCMN");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (DCMN_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DCPB::DCPB(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DCPB");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (DCPB_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DCV1::DCV1(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DCV1");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (DCV1_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DCV2::DCV2(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DCV2");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (DCV2_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DCV3::DCV3(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DCV3");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (DCV3_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DDLY::DDLY(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DDLY");
  bos_ind = get_bos_index(1, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = wcs_.jw[ind - 4];
    row_ptr = (DDLY_row *)(&wcs_.jw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DGEO::DGEO(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DGEO");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (DGEO_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DHCL::DHCL(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DHCL");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (DHCL_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DITM::DITM(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DITM");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (DITM_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DOCA::DOCA(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DOCA");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (DOCA_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DPCP::DPCP(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DPCP");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (DPCP_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DPSP::DPSP(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DPSP");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (DPSP_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DSPC::DSPC(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DSPC");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (DSPC_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DSPS::DSPS(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DSPS");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (DSPS_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DSTC::DSTC(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DSTC");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (DSTC_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DTCP::DTCP(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DTCP");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (DTCP_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

DTRK::DTRK(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "DTRK");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (DTRK_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

EC::EC(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "EC");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (EC_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

EC01::EC01(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "EC01");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (EC01_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

EC1::EC1(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "EC1");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (EC1_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

EC1P::EC1P(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "EC1P");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (EC1P_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

EC1R::EC1R(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "EC1R");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (EC1R_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

ECCA::ECCA(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "ECCA");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (ECCA_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

ECCL::ECCL(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "ECCL");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (ECCL_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

ECCT::ECCT(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "ECCT");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (ECCT_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

ECDI::ECDI(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "ECDI");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (ECDI_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

ECG::ECG(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "ECG");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (ECG_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

ECH::ECH(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "ECH");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (ECH_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

ECHB::ECHB(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "ECHB");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (ECHB_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

ECMT::ECMT(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "ECMT");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (ECMT_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

ECP::ECP(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "ECP");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (ECP_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

ECP1::ECP1(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "ECP1");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (ECP1_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

ECPB::ECPB(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "ECPB");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (ECPB_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

ECPC::ECPC(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "ECPC");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (ECPC_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

ECPE::ECPE(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "ECPE");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (ECPE_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

ECPI::ECPI(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "ECPI");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (ECPI_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

ECPO::ECPO(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "ECPO");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (ECPO_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

ECRB::ECRB(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "ECRB");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (ECRB_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

ECS::ECS(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "ECS");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (ECS_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

ECT::ECT(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "ECT");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (ECT_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

EID0::EID0(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "EID0");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (EID0_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

EPIC::EPIC(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "EPIC");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (EPIC_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

EVNT::EVNT(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "EVNT");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (EVNT_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

FBPM::FBPM(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "FBPM");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (FBPM_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

G1SL::G1SL(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "G1SL");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (G1SL_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

G2SL::G2SL(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "G2SL");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (G2SL_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

G3SL::G3SL(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "G3SL");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (G3SL_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

G4SL::G4SL(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "G4SL");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (G4SL_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

GCPB::GCPB(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "GCPB");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (GCPB_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

GGEO::GGEO(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "GGEO");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (GGEO_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

GP_X::GP_X(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "GP_X");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (GP_X_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

GP_Y::GP_Y(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "GP_Y");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (GP_Y_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

GPAR::GPAR(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "GPAR");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (GPAR_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

GPID::GPID(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "GPID");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (GPID_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

HBER::HBER(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "HBER");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (HBER_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

HBID::HBID(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "HBID");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (HBID_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

HBLA::HBLA(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "HBLA");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (HBLA_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

HBTB::HBTB(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "HBTB");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (HBTB_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

HBTR::HBTR(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "HBTR");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (HBTR_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

HCAL::HCAL(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "HCAL");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (HCAL_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

HDPL::HDPL(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "HDPL");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (HDPL_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

HEAD::HEAD(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "HEAD");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (HEAD_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

HEVT::HEVT(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "HEVT");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (HEVT_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

HLB::HLB(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "HLB");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (HLB_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

HLS::HLS(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "HLS");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (HLS_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

IC::IC(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "IC");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (IC_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

ICHB::ICHB(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "ICHB");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (ICHB_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

ICPB::ICPB(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "ICPB");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (ICPB_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

KFIT::KFIT(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "KFIT");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (KFIT_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

L1PG::L1PG(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "L1PG");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (L1PG_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

L2H::L2H(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "L2H");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (L2H_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

L2S::L2S(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "L2S");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (L2S_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

LASR::LASR(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "LASR");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (LASR_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

LCDI::LCDI(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "LCDI");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (LCDI_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

LCPB::LCPB(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "LCPB");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (LCPB_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

LOWQ::LOWQ(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "LOWQ");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (LOWQ_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

LSRT::LSRT(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "LSRT");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (LSRT_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

MCEV::MCEV(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "MCEV");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (MCEV_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

MCHD::MCHD(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "MCHD");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (MCHD_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

MCTK::MCTK(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "MCTK");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (MCTK_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

MCVX::MCVX(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "MCVX");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (MCVX_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

MS1::MS1(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "MS1");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (MS1_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

MTRK::MTRK(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "MTRK");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (MTRK_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

MVRT::MVRT(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "MVRT");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (MVRT_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

PART::PART(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "PART");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (PART_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

PCO::PCO(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "PCO");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (PCO_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

PHTM::PHTM(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "PHTM");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (PHTM_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

PID1::PID1(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "PID1");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (PID1_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

PID2::PID2(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "PID2");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (PID2_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

PID3::PID3(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "PID3");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (PID3_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

PID4::PID4(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "PID4");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (PID4_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

PID5::PID5(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "PID5");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (PID5_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

PID6::PID6(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "PID6");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (PID6_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

PIDT::PIDT(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "PIDT");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (PIDT_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

PRTM::PRTM(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "PRTM");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (PRTM_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

PSO::PSO(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "PSO");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (PSO_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

PTDB::PTDB(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "PTDB");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (PTDB_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

RC26::RC26(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "RC26");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (RC26_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

RCST::RCST(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "RCST");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (RCST_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

REF::REF(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "REF");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (REF_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

RF::RF(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "RF");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (RF_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

RFT::RFT(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "RFT");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (RFT_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

RGLK::RGLK(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "RGLK");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (RGLK_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

RNLG::RNLG(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "RNLG");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (RNLG_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

RNPE::RNPE(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "RNPE");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (RNPE_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

RTSL::RTSL(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "RTSL");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (RTSL_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

RUNC::RUNC(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "RUNC");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (RUNC_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

S1ST::S1ST(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "S1ST");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (S1ST_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SC::SC(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SC");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SC_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SC1::SC1(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SC1");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SC1_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SCC::SCC(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SCC");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SCC_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SCDI::SCDI(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SCDI");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SCDI_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SCG::SCG(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SCG");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SCG_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SCGD::SCGD(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SCGD");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SCGD_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SCH::SCH(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SCH");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SCH_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SCMD::SCMD(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SCMD");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SCMD_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SCMT::SCMT(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SCMT");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SCMT_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SCMW::SCMW(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SCMW");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SCMW_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SCP::SCP(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SCP");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SCP_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SCPB::SCPB(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SCPB");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SCPB_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SCPE::SCPE(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SCPE");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SCPE_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SCPS::SCPS(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SCPS");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SCPS_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SCR::SCR(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SCR");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SCR_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SCRC::SCRC(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SCRC");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SCRC_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SCS::SCS(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SCS");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SCS_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SCT::SCT(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SCT");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SCT_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SGMP::SGMP(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SGMP");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SGMP_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SH::SH(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SH");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SH_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SHHB::SHHB(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SHHB");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SHHB_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SHPB::SHPB(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SHPB");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SHPB_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SHT::SHT(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SHT");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SHT_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SPAR::SPAR(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SPAR");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SPAR_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SPIN::SPIN(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SPIN");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SPIN_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

ST::ST(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "ST");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (ST_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

ST1::ST1(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "ST1");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (ST1_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

STG::STG(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "STG");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (STG_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

STH::STH(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "STH");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (STH_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

STN0::STN0(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "STN0");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (STN0_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

STN1::STN1(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "STN1");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (STN1_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

STPB::STPB(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "STPB");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (STPB_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

STPE::STPE(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "STPE");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (STPE_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

STR::STR(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "STR");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (STR_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

STRE::STRE(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "STRE");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (STRE_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

STS::STS(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "STS");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (STS_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

STSN::STSN(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "STSN");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (STSN_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

SYNC::SYNC(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "SYNC");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (SYNC_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TACO::TACO(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TACO");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TACO_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TAGE::TAGE(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TAGE");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TAGE_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TAGI::TAGI(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TAGI");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TAGI_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TAGM::TAGM(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TAGM");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TAGM_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TAGR::TAGR(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TAGR");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TAGR_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TAGT::TAGT(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TAGT");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TAGT_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TATL::TATL(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TATL");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TATL_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TATR::TATR(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TATR");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TATR_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TBER::TBER(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TBER");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TBER_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TBID::TBID(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TBID");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TBID_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TBLA::TBLA(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TBLA");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TBLA_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TBTR::TBTR(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TBTR");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TBTR_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TCSB::TCSB(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TCSB");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TCSB_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TCT::TCT(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TCT");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TCT_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TDPL::TDPL(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TDPL");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TDPL_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TESC::TESC(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TESC");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TESC_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TGBI::TGBI(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TGBI");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TGBI_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TGEO::TGEO(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TGEO");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TGEO_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TGPB::TGPB(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TGPB");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TGPB_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TGS::TGS(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TGS");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TGS_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TGTL::TGTL(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TGTL");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TGTL_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TGTR::TGTR(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TGTR");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TGTR_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TLV1::TLV1(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TLV1");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TLV1_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TPC::TPC(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TPC");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TPC_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TPCH::TPCH(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TPCH");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TPCH_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TPE0::TPE0(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TPE0");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TPE0_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TPE1::TPE1(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TPE1");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TPE1_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TRCF::TRCF(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TRCF");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TRCF_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TRGS::TRGS(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TRGS");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TRGS_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TRKS::TRKS(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TRKS");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TRKS_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TRL1::TRL1(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TRL1");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TRL1_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TRPB::TRPB(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TRPB");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TRPB_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

TRUE::TRUE(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "TRUE");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (TRUE_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

UNUS::UNUS(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "UNUS");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (UNUS_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}

VERT::VERT(int bank_index) {
  int bos_ind, ind;
  char bankname[4];
  strcpy(bankname, "VERT");
  bos_ind = get_bos_index(0, bankname, bank_index);
  if (bos_ind) {
    ind = bos_ind - 1;
    nrows = bcs_.iw[ind - 4];
    row_ptr = (VERT_row *)(&bcs_.iw[ind + 1]);
  } else {
    nrows = 0;
    row_ptr = NULL;
  }
}
