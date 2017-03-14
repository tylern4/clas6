#include <string.h>
#include <stdio.h>
#include <time.h>
#include <ntypes.h>
#include <bostypes.h>
#include <trklink.h>
#include <kinematics.h>
#include <printBOS.h>

/*print the head of a bank*/
void pBankHead(FILE *fp,bankHeader_t *bank) 
{
  fprintf(fp,"Group:\t%-4.4s\tSector:\t%hd\tNhits:\t%hd\tNext ind:\t%hd\n", bank->name, bank->nr,bank->nrow,bank->nextIndex);
}
/*print the HEAD bank - note the difference from above*/ 
void pHEAD(FILE *fp,clasHEAD_t *header)
{
  char *dataType;

  if (header->head[0].type < 100) {

    switch (header->head[0].type) {
    case 1:
    case 3:
    case 4:
    case 5:
    case 6:
    case 7:
    case 8:
    case 9:
      dataType = " (physics data)";
      break;
    case -1:
      dataType = " (SDA monte carlo)";
      break;
    case -2:
      dataType = " (GSIM monte carlo)";
      break; 
    case -3:
      dataType = " (CLASsim monte carlo)";
      break;
    case 2:
      dataType = " (physics sync)";
      break;
     
    case 10:
      dataType = " (scaler)";
      break;

    default:
      dataType = " (unknown)";
      break;
    }

  }
  else
     dataType = " (database record)";
 
  fprintf(fp,"\nVersion:\t%d\nRun:\t\t%d\nEvent:\t\t%d\nType:\t\t%d %s\nROC:\t\t%d\nCLASS:\t\t%d\nTrgbit:\t\t0x%x\nTIME:\t\t%s\n",header->head[0].version,header->head[0].nrun,
          header->head[0].nevent,header->head[0].type,dataType,header->head[0].roc,
          header->head[0].evtclass,header->head[0].trigbits,ctime((const long *) 
&(header->head[0].time)));
}


/* Print bmpr bank -------------------------------------------------*/
void pBMPR(FILE *fp, bmpr_t *bmpr)
{
  fprintf(fp,"Q_TOT: %f  Q_TOT_LV: %f  TL_TU: %f  CURRENT: %f  \n", bmpr->q_tot, bmpr->q_tot_lv, bmpr->tl_tu, bmpr->current);
}

void printBMPRbank(FILE *fp, clasBMPR_t *bmpr) {
  int i;

  if(bmpr->bank.nrow) {
    pBankHead(fp,&bmpr->bank);
    for (i = 0; i < bmpr->bank.nrow; i++)
      pBMPR(fp,&bmpr->bmpr[i]);
    fprintf(fp,"\n");
  }
}

/* Print brep bank -------------------------------------------------*/
void pBREP(FILE *fp, brep_t *brep)
{
  fprintf(fp,"MBSY2C_energy: %5.3f  IGT0I00BIASET: %5.3f  IGL1I00DAC2: %5.3f  SMRPOSA: %5.3f  SMRPOSB: %5.3f  SMRPOSC: %5.3f  Harp: %5.3f  hallb_sf_xy560: %5.3f  MTSETI: %5.3f  MTIRBCK: %5.3f  MTVRBCK: %5.3f  TMSETI: %5.3f  TMIRBCK: %5.3f  TMVRBCK: %5.3f  Cryo_pressure: %5.3f  Cryo_temperature: %5.3f  Cryo_status: %5.3f  VCG2C24: %5.3f  VCG2H01: %5.3f  scalerS2o: %5.3f  scalerS3o: %5.3f  scalerS4o: %5.3f  scalerS5o: %5.3f  scalerS6o: %5.3f  scalerS7o: %5.3f  scalerS8o: %5.3f  scalerS9o: %5.3f  IPM2H01_XPOS: %5.3f  IPM2H01_YPOS: %5.3f  IPM2H01: %5.3f  IPM2C24A_XPOS: %5.3f  IPM2C24A_YPOS: %5.3f  IPM2C24A: %5.3f  IPM2C22A_XPOS: %5.3f  IPM2C22A_YPOS: %5.3f  IPM2C22A: %5.3f  \n", brep->mbsy2c_energy, brep->igt0i00biaset, brep->igl1i00dac2, brep->smrposa, brep->smrposb, brep->smrposc, brep->harp, brep->hallb_sf_xy560, brep->mtseti, brep->mtirbck, brep->mtvrbck, brep->tmseti, brep->tmirbck, brep->tmvrbck, brep->cryo_pressure, brep->cryo_temperature, brep->cryo_status, brep->vcg2c24, brep->vcg2h01, brep->scalers2o, brep->scalers3o, brep->scalers4o, brep->scalers5o, brep->scalers6o, brep->scalers7o, brep->scalers8o, brep->scalers9o, brep->ipm2h01_xpos, brep->ipm2h01_ypos, brep->ipm2h01, brep->ipm2c24a_xpos, brep->ipm2c24a_ypos, brep->ipm2c24a, brep->ipm2c22a_xpos, brep->ipm2c22a_ypos, brep->ipm2c22a);
}

void printBREPbank(FILE *fp, clasBREP_t *brep) {
  int i;

  if(brep->bank.nrow) {
    pBankHead(fp,&brep->bank);
    for (i = 0; i < brep->bank.nrow; i++)
      pBREP(fp,&brep->brep[i]);
    fprintf(fp,"\n");
  }
}

/* Print call bank -------------------------------------------------*/
void pCALL(FILE *fp, call_t *call)
{
  fprintf(fp,"ID: %3hd  TDC: %3hd  ADC: %3hd  \n", call->id, call->tdc, call->adc);
}

void printCALLbank(FILE *fp, clasCALL_t *call) {
  int i;

  if(call->bank.nrow) {
    pBankHead(fp,&call->bank);
    for (i = 0; i < call->bank.nrow; i++)
      pCALL(fp,&call->call[i]);
    fprintf(fp,"\n");
  }
}

/* Print cc0 bank -------------------------------------------------*/
void pCC0(FILE *fp, cc0_t *cc0)
{
  fprintf(fp,"ID: %3hd  TDC: %3hd  ADC: %3hd  \n", cc0->id, cc0->tdc, cc0->adc);
}

void printCC0bank(FILE *fp, clasCC0_t *cc0) {
  int i;

  if(cc0->bank.nrow) {
    pBankHead(fp,&cc0->bank);
    for (i = 0; i < cc0->bank.nrow; i++)
      pCC0(fp,&cc0->cc0[i]);
    fprintf(fp,"\n");
  }
}

/* Print cc01 bank -------------------------------------------------*/
void pCC01(FILE *fp, cc01_t *cc01)
{
  fprintf(fp,"id: %3hd  time: %5.3f  n_pe: %5.3f  \n", cc01->id, cc01->time, cc01->n_pe);
}

void printCC01bank(FILE *fp, clasCC01_t *cc01) {
  int i;

  if(cc01->bank.nrow) {
    pBankHead(fp,&cc01->bank);
    for (i = 0; i < cc01->bank.nrow; i++)
      pCC01(fp,&cc01->cc01[i]);
    fprintf(fp,"\n");
  }
}

/* Print cc1 bank -------------------------------------------------*/
void pCC1(FILE *fp, cc1_t *cc1)
{
  fprintf(fp,"ID: %3hd  TDC: %3hd  ADC: %3hd  \n", cc1->id, cc1->tdc, cc1->adc);
}

void printCC1bank(FILE *fp, clasCC1_t *cc1) {
  int i;

  if(cc1->bank.nrow) {
    pBankHead(fp,&cc1->bank);
    for (i = 0; i < cc1->bank.nrow; i++)
      pCC1(fp,&cc1->cc1[i]);
    fprintf(fp,"\n");
  }
}

/* Print ccdi bank -------------------------------------------------*/
void pCCDI(FILE *fp, ccdi_t *ccdi)
{
  fprintf(fp,"crate: %6d  slot: %6d  mask: %6d  threshold: %6d  width: %6d  \n", ccdi->crate, ccdi->slot, ccdi->mask, ccdi->threshold, ccdi->width);
}

void printCCDIbank(FILE *fp, clasCCDI_t *ccdi) {
  int i;

  if(ccdi->bank.nrow) {
    pBankHead(fp,&ccdi->bank);
    for (i = 0; i < ccdi->bank.nrow; i++)
      pCCDI(fp,&ccdi->ccdi[i]);
    fprintf(fp,"\n");
  }
}

/* Print cch bank -------------------------------------------------*/
void pCCH(FILE *fp, cch_t *cch)
{
  fprintf(fp,"xin: %5.3f  yin: %5.3f  zin: %5.3f  xout: %5.3f  yout: %5.3f  zout: %5.3f  pmom: %5.3f  id: %5d  tknum: %6d  segment:  %2d  tdc:  %5d  hit:  %2d\n", cch->xin, cch->yin, cch->zin, cch->xout, cch->yout, cch->zout, cch->pmom, cch->id, cch->tknum, cch->segment, cch->tdc, cch->hit);
}

void printCCHbank(FILE *fp, clasCCH_t *cch) {
  int i;

  if(cch->bank.nrow) {
    pBankHead(fp,&cch->bank);
    for (i = 0; i < cch->bank.nrow; i++)
      pCCH(fp,&cch->cch[i]);
    fprintf(fp,"\n");
  }
}

/* Print cch1 bank -------------------------------------------------*/
void pCCH1(FILE *fp, cch1_t *cch1)
{
  fprintf(fp,
	  "tknum:  %6d  id:  %5d  nhits:  %4d  sector:  %2d  segment: %2d  pmom:  %5.3f  xin: %5.3f  yin: %5.3f  zin: %5.3f  xout: %5.3f  yout: %5.3f  zout: %5.3f  \n",
	  cch1->tknum, cch1->id, cch1->nhits, cch1->sector, cch1->segment, cch1->pmom, cch1->xin, cch1->yin, cch1->zin, cch1->xout, cch1->yout, cch1->zout);
}

void printCCH1bank(FILE *fp, clasCCH1_t *cch1) {
  int i;

  if(cch1->bank.nrow) {
    pBankHead(fp,&cch1->bank);
    for (i = 0; i < cch1->bank.nrow; i++)
      pCCH1(fp,&cch1->cch1[i]);
    fprintf(fp,"\n");
  }
}

/* Print cch2 bank -------------------------------------------------*/
void pCCH2(FILE *fp, cch2_t *cch2)
{
  fprintf(fp,"itrnum: %5d  sector:  %2d  segment:  %5d  itdc:  %5d\n", cch2->itrnum, cch2->sector, cch2->segment, cch2->itdc);
}

void printCCH2bank(FILE *fp, clasCCH2_t *cch2) {
  int i;

  if(cch2->bank.nrow) {
    pBankHead(fp,&cch2->bank);
    for (i = 0; i < cch2->bank.nrow; i++)
      pCCH2(fp,&cch2->cch2[i]);
    fprintf(fp,"\n");
  }
}

/* Print ccmt bank -------------------------------------------------*/
void pCCMT(FILE *fp, ccmt_t *ccmt)
{
  fprintf(fp,"mean_high: %6d  mean_lo: %6d  \n", ccmt->mean_high, ccmt->mean_lo);
}

void printCCMTbank(FILE *fp, clasCCMT_t *ccmt) {
  int i;

  if(ccmt->bank.nrow) {
    pBankHead(fp,&ccmt->bank);
    for (i = 0; i < ccmt->bank.nrow; i++)
      pCCMT(fp,&ccmt->ccmt[i]);
    fprintf(fp,"\n");
  }
}

/* Print ccpb bank -------------------------------------------------*/
void pCCPB(FILE *fp, ccpb_t *ccpb)
{
  fprintf(fp,"ScSgHt: %3d  Nphe: %5.3f  Time: %5.3f  Path: %5.3f  Chi2CC: %5.3f  Status: %3hd  \n", ccpb->scsght, ccpb->nphe, ccpb->time, ccpb->path, ccpb->chi2cc, ccpb->status);
}

void printCCPBbank(FILE *fp, clasCCPB_t *ccpb) {
  int i;

  if(ccpb->bank.nrow) {
    pBankHead(fp,&ccpb->bank);
    for (i = 0; i < ccpb->bank.nrow; i++)
      pCCPB(fp,&ccpb->ccpb[i]);
    fprintf(fp,"\n");
  }
}

/* Print ccpe bank -------------------------------------------------*/
void pCCPE(FILE *fp, ccpe_t *ccpe)
{
  fprintf(fp,"ID: %6d  mean: %6d  sigma: %6d  \n", ccpe->id, ccpe->mean, ccpe->sigma);
}

void printCCPEbank(FILE *fp, clasCCPE_t *ccpe) {
  int i;

  if(ccpe->bank.nrow) {
    pBankHead(fp,&ccpe->bank);
    for (i = 0; i < ccpe->bank.nrow; i++)
      pCCPE(fp,&ccpe->ccpe[i]);
    fprintf(fp,"\n");
  }
}

/* Print ccrc bank -------------------------------------------------*/
void pCCRC(FILE *fp, ccrc_t *ccrc)
{
  fprintf(fp,"nrsect: %3hd  nrsegm: %3hd  nrsegm_p: %3hd  nrsegm_m: %3hd  nrphe: %3hd  nrtime: %3hd  nrthet: %3hd  nrdthet: %3hd  nrphy: %3hd  nriec: %3hd  nrdiec: %3hd  nrstat: %3hd  \n", ccrc->nrsect, ccrc->nrsegm, ccrc->nrsegm_p, ccrc->nrsegm_m, ccrc->nrphe, ccrc->nrtime, ccrc->nrthet, ccrc->nrdthet, ccrc->nrphy, ccrc->nriec, ccrc->nrdiec, ccrc->nrstat);
}

void printCCRCbank(FILE *fp, clasCCRC_t *ccrc) {
  int i;

  if(ccrc->bank.nrow) {
    pBankHead(fp,&ccrc->bank);
    for (i = 0; i < ccrc->bank.nrow; i++)
      pCCRC(fp,&ccrc->ccrc[i]);
    fprintf(fp,"\n");
  }
}

/* Print cct bank -------------------------------------------------*/
void pCCT(FILE *fp, cct_t *cct)
{
  fprintf(fp,"ID: %6d  TDC: %6d \n", cct->id, cct->tdc);
}

void printCCTbank(FILE *fp, clasCCT_t *cct) {
  int i;

  if(cct->bank.nrow) {
    pBankHead(fp,&cct->bank);
    for (i = 0; i < cct->bank.nrow; i++)
      pCCT(fp,&cct->cct[i]);
    fprintf(fp,"\n");
  }
}


/* Print cl01 bank -------------------------------------------------*/
void pCL01(FILE *fp, cl01_t *cl01)
{
  fprintf(fp,"ac_amp: %6d  fc_diode_amp: %6d  fc_diode_t: %5.3f  nc_diode_amp: %6d  nc_diode_t: %5.3f  sc_diode_amp: %6d  sc_diode_t: %5.3f  sf_diode_amp: %6d  sf_diode_t: %5.3f  rf1: %5.3f  rf2: %5.3f  rf: %5.3f  \n", cl01->ac_amp, cl01->fc_diode_amp, cl01->fc_diode_t, cl01->nc_diode_amp, cl01->nc_diode_t, cl01->sc_diode_amp, cl01->sc_diode_t, cl01->sf_diode_amp, cl01->sf_diode_t, cl01->rf1, cl01->rf2, cl01->rf);
}

void printCL01bank(FILE *fp, clasCL01_t *cl01) {
  int i;

  if(cl01->bank.nrow) {
    pBankHead(fp,&cl01->bank);
    for (i = 0; i < cl01->bank.nrow; i++)
      pCL01(fp,&cl01->cl01[i]);
    fprintf(fp,"\n");
  }
}

/* Print clst bank -------------------------------------------------*/
void pCLST(FILE *fp, clst_t *clst)
{
  fprintf(fp,"layer: %d clustern: %d csegment: %d cclust: %d clustp: %d\n", clst->layer, clst->clustern, clst->csegment, clst->cclust, clst->clustp);
}

void printCLSTbank(FILE *fp, clasCLST_t *clst) {
  int i;

  if(clst->bank.nrow) {
    pBankHead(fp,&clst->bank);
    for (i = 0; i < clst->bank.nrow; i++)
      pCLST(fp,&clst->clst[i]);
    fprintf(fp,"\n");
  }
}

/* Print cped bank -------------------------------------------------*/
void pCPED(FILE *fp, cped_t *cped)
{
  fprintf(fp,"slot: %6d  channel: %6d  mean: %6d  sigma: %5.3f  offset: %6d  \n", cped->slot, cped->channel, cped->mean, cped->sigma, cped->offset);
}

void printCPEDbank(FILE *fp, clasCPED_t *cped) {
  int i;

  if(cped->bank.nrow) {
    pBankHead(fp,&cped->bank);
    for (i = 0; i < cped->bank.nrow; i++)
      pCPED(fp,&cped->cped[i]);
    fprintf(fp,"\n");
  }
}

/* Print dc0 bank -------------------------------------------------*/
void pDC0(FILE *fp, dc0_t *dc0)
{
  fprintf(fp,"ID: %3hd  TDC: %3hd  \n", dc0->id, dc0->tdc);
}

void printDC0bank(FILE *fp, clasDC0_t *dc0) {
  int i;

  if(dc0->bank.nrow) {
    pBankHead(fp,&dc0->bank);
    for (i = 0; i < dc0->bank.nrow; i++)
      pDC0(fp,&dc0->dc0[i]);
    fprintf(fp,"\n");
  }
}

/* Print dc1 bank -------------------------------------------------*/
void pDC1(FILE *fp, dc1_t *dc1)
{
  fprintf(fp,"ID: %3hd  time: %5.3f  \n", dc1->id, dc1->time);
}

void printDC1bank(FILE *fp, clasDC1_t *dc1) {
  int i;

  if(dc1->bank.nrow) {
    pBankHead(fp,&dc1->bank);
    for (i = 0; i < dc1->bank.nrow; i++)
      pDC1(fp,&dc1->dc1[i]);
    fprintf(fp,"\n");
  }
}

/* Print dcdw bank -------------------------------------------------*/
void pDCDW(FILE *fp, dcdw_t *dcdw)
{
  fprintf(fp,"ID: %3hd  TIDLY: %5.3f  STAT: %3hd  \n", dcdw->id, dcdw->tidly, dcdw->stat);
}

void printDCDWbank(FILE *fp, clasDCDW_t *dcdw) {
  int i;

  if(dcdw->bank.nrow) {
    pBankHead(fp,&dcdw->bank);
    for (i = 0; i < dcdw->bank.nrow; i++)
      pDCDW(fp,&dcdw->dcdw[i]);
    fprintf(fp,"\n");
  }
}

/* Print dcgm bank -------------------------------------------------*/
void pDCGM(FILE *fp, dcgm_t *dcgm)
{
  fprintf(fp,"x_curve: %5.3f  y_curve: %5.3f  z_curve: %5.3f  r_curve: %5.3f  theta_start: %5.3f  d_theta: %5.3f  x_nmid: %5.3f  y_nmid: %5.3f  z_nmid: %5.3f  theta_min: %5.3f  theta_max: %5.3f  min_wire: %3hd  max_wire: %3hd  stereo: %5.3f  cell_size: %5.3f  x_norm: %5.3f  y_norm: %5.3f  z_norm: %5.3f  p_dist: %5.3f  p_sep: %5.3f  max_cylw: %3hd  \n", dcgm->x_curve, dcgm->y_curve, dcgm->z_curve, dcgm->r_curve, dcgm->theta_start, dcgm->d_theta, dcgm->x_nmid, dcgm->y_nmid, dcgm->z_nmid, dcgm->theta_min, dcgm->theta_max, dcgm->min_wire, dcgm->max_wire, dcgm->stereo, dcgm->cell_size, dcgm->x_norm, dcgm->y_norm, dcgm->z_norm, dcgm->p_dist, dcgm->p_sep, dcgm->max_cylw);
}

void printDCGMbank(FILE *fp, clasDCGM_t *dcgm) {
  int i;

  if(dcgm->bank.nrow) {
    pBankHead(fp,&dcgm->bank);
    for (i = 0; i < dcgm->bank.nrow; i++)
      pDCGM(fp,&dcgm->dcgm[i]);
    fprintf(fp,"\n");
  }
}

/* Print dcgw bank -------------------------------------------------*/
void pDCGW(FILE *fp, dcgw_t *dcgw)
{
  fprintf(fp,"x_mid: %5.3f  y_mid: %5.3f  z_mid: %5.3f  x_dir: %5.3f  y_dir: %5.3f  z_dir: %5.3f  w_len: %5.3f  w_len_hv: %5.3f  \n", dcgw->x_mid, dcgw->y_mid, dcgw->z_mid, dcgw->x_dir, dcgw->y_dir, dcgw->z_dir, dcgw->w_len, dcgw->w_len_hv);
}

void printDCGWbank(FILE *fp, clasDCGW_t *dcgw) {
  int i;

  if(dcgw->bank.nrow) {
    pBankHead(fp,&dcgw->bank);
    for (i = 0; i < dcgw->bank.nrow; i++)
      pDCGW(fp,&dcgw->dcgw[i]);
    fprintf(fp,"\n");
  }
}

/* Print dch bank -------------------------------------------------*/
void pDCH(FILE *fp, dch_t *dch)
{
  fprintf(fp,"x: %5.3f  y: %5.3f  z: %5.3f  step: %5.3f  dedx: %5.3f  pmom: %5.3f  time: %5.3f  cx: %5.3f  cy: %5.3f  cz: %5.3f  track: %3hd  id: %3hd  layer: %3hd  \n", dch->x, dch->y, dch->z, dch->step, dch->dedx, dch->pmom, dch->time, dch->cx, dch->cy, dch->cz, dch->track, dch->id, dch->layer);
}

void printDCHbank(FILE *fp, clasDCH_t *dch) {
  int i;

  if(dch->bank.nrow) {
    pBankHead(fp,&dch->bank);
    for (i = 0; i < dch->bank.nrow; i++)
      pDCH(fp,&dch->dch[i]);
    fprintf(fp,"\n");
  }
}

/* Print dcpb bank -------------------------------------------------*/
void pDCPB(FILE *fp, dcpb_t *dcpb)
{
  fprintf(fp,"ScTr: %3d  x_SC: %5.3f  y_SC: %5.3f  z_SC: %5.3f  CX_SC: %5.3f  CY_SC: %5.3f  CZ_SC: %5.3f  X_EC: %5.3f  Y_EC: %5.3f  Z_EC: %5.3f  Th_CC: %5.3f  Chi2: %5.3f  Status: %3d  \n", dcpb->sctr, dcpb->x_sc, dcpb->y_sc, dcpb->z_sc, dcpb->cx_sc, dcpb->cy_sc, dcpb->cz_sc, dcpb->x_ec, dcpb->y_ec, dcpb->z_ec, dcpb->th_cc, dcpb->chi2, dcpb->status);
}

void printDCPBbank(FILE *fp, clasDCPB_t *dcpb) {
  int i;

  if(dcpb->bank.nrow) {
    pBankHead(fp,&dcpb->bank);
    for (i = 0; i < dcpb->bank.nrow; i++)
      pDCPB(fp,&dcpb->dcpb[i]);
    fprintf(fp,"\n");
  }
}

/* Print dcv1 bank -------------------------------------------------*/
void pDCV1(FILE *fp, dcv1_t *dcv1)
{
  fprintf(fp,"TsR1: %5.3f  V0R1: %5.3f  TmR1: %5.3f  sp1R1: %5.3f  sp2R1: %5.3f  \n", dcv1->tsr1, dcv1->v0r1, dcv1->tmr1, dcv1->sp1r1, dcv1->sp2r1);
}

void printDCV1bank(FILE *fp, clasDCV1_t *dcv1) {
  int i;

  if(dcv1->bank.nrow) {
    pBankHead(fp,&dcv1->bank);
    for (i = 0; i < dcv1->bank.nrow; i++)
      pDCV1(fp,&dcv1->dcv1[i]);
    fprintf(fp,"\n");
  }
}

/* Print dcv2 bank -------------------------------------------------*/
void pDCV2(FILE *fp, dcv2_t *dcv2)
{
  fprintf(fp,"Ts1R2: %5.3f  V01R2: %5.3f  VA1R2: %5.3f  VB1R2: %5.3f  Tm1R2: %5.3f  TA1R2: %5.3f  TB1R2: %5.3f  Ts2R2: %5.3f  V02R2: %5.3f  VA2R2: %5.3f  VB2R2: %5.3f  Tm2R2: %5.3f  TA2R2: %5.3f  TB2R2: %5.3f  \n", dcv2->ts1r2, dcv2->v01r2, dcv2->va1r2, dcv2->vb1r2, dcv2->tm1r2, dcv2->ta1r2, dcv2->tb1r2, dcv2->ts2r2, dcv2->v02r2, dcv2->va2r2, dcv2->vb2r2, dcv2->tm2r2, dcv2->ta2r2, dcv2->tb2r2);
}

void printDCV2bank(FILE *fp, clasDCV2_t *dcv2) {
  int i;

  if(dcv2->bank.nrow) {
    pBankHead(fp,&dcv2->bank);
    for (i = 0; i < dcv2->bank.nrow; i++)
      pDCV2(fp,&dcv2->dcv2[i]);
    fprintf(fp,"\n");
  }
}

/* Print dcv3 bank -------------------------------------------------*/
void pDCV3(FILE *fp, dcv3_t *dcv3)
{
  fprintf(fp,"TsR3: %5.3f  V0R3: %5.3f  TmR3: %5.3f  sp1R3: %5.3f  sp2R3: %5.3f  \n", dcv3->tsr3, dcv3->v0r3, dcv3->tmr3, dcv3->sp1r3, dcv3->sp2r3);
}

void printDCV3bank(FILE *fp, clasDCV3_t *dcv3) {
  int i;

  if(dcv3->bank.nrow) {
    pBankHead(fp,&dcv3->bank);
    for (i = 0; i < dcv3->bank.nrow; i++)
      pDCV3(fp,&dcv3->dcv3[i]);
    fprintf(fp,"\n");
  }
}

/* Print ddly bank -------------------------------------------------*/
void pDDLY(FILE *fp, ddly_t *ddly)
{
  fprintf(fp,"ID: %3hd  TIDLY: %5.3f  STAT: %3hd  \n", ddly->id, ddly->tidly, ddly->stat);
}

void printDDLYbank(FILE *fp, clasDDLY_t *ddly) {
  int i;

  if(ddly->bank.nrow) {
    pBankHead(fp,&ddly->bank);
    for (i = 0; i < ddly->bank.nrow; i++)
      pDDLY(fp,&ddly->ddly[i]);
    fprintf(fp,"\n");
  }
}

/* Print dgeo bank -------------------------------------------------*/
void pDGEO(FILE *fp, dgeo_t *dgeo)
{
  fprintf(fp,"ID_sec: %3hd  ID_reg: %3hd  xpos: %5.3f  ypos: %5.3f  zpos: %5.3f  sxpos: %5.3f  sypos: %5.3f  szpos: %5.3f  \n", dgeo->id_sec, dgeo->id_reg, dgeo->xpos, dgeo->ypos, dgeo->zpos, dgeo->sxpos, dgeo->sypos, dgeo->szpos);
}

void printDGEObank(FILE *fp, clasDGEO_t *dgeo) {
  int i;

  if(dgeo->bank.nrow) {
    pBankHead(fp,&dgeo->bank);
    for (i = 0; i < dgeo->bank.nrow; i++)
      pDGEO(fp,&dgeo->dgeo[i]);
    fprintf(fp,"\n");
  }
}

/* Print dhcl bank -------------------------------------------------*/
void pDHCL(FILE *fp, dhcl_t *dhcl)
{
  fprintf(fp,"SLY: %6d  BTRK: %6d  TRKS1: %6d  TRKS2: %6d  WIRE1: %6d  BWIR1: %6d  WIRE2: %6d  BWIR2: %6d  WIRE3: %6d  BWIR3: %6d  WIRE4: %6d  BWIR4: %6d  WIRE5: %6d  BWIR5: %6d  WIRE6: %6d  BWIR6: %6d  \n", dhcl->sly, dhcl->btrk, dhcl->trks1, dhcl->trks2, dhcl->wire1, dhcl->bwir1, dhcl->wire2, dhcl->bwir2, dhcl->wire3, dhcl->bwir3, dhcl->wire4, dhcl->bwir4, dhcl->wire5, dhcl->bwir5, dhcl->wire6, dhcl->bwir6);
}

void printDHCLbank(FILE *fp, clasDHCL_t *dhcl) {
  int i;

  if(dhcl->bank.nrow) {
    pBankHead(fp,&dhcl->bank);
    for (i = 0; i < dhcl->bank.nrow; i++)
      pDHCL(fp,&dhcl->dhcl[i]);
    fprintf(fp,"\n");
  }
}

/* Print doca bank -------------------------------------------------*/
void pDOCA(FILE *fp, doca_t *doca)
{
  fprintf(fp,"ID: %3hd  DOCA: %3hd  \n", doca->id, doca->doca);
}

void printDOCAbank(FILE *fp, clasDOCA_t *doca) {
  int i;

  if(doca->bank.nrow) {
    pBankHead(fp,&doca->bank);
    for (i = 0; i < doca->bank.nrow; i++)
      pDOCA(fp,&doca->doca[i]);
    fprintf(fp,"\n");
  }
}

/* Print dpsp bank -------------------------------------------------*/
void pDPSP(FILE *fp, dpsp_t *dpsp)
{
  fprintf(fp,"ID: %6d  mean: %6d  sigma: %5.3f  \n", dpsp->id, dpsp->mean, dpsp->sigma);
}

void printDPSPbank(FILE *fp, clasDPSP_t *dpsp) {
  int i;

  if(dpsp->bank.nrow) {
    pBankHead(fp,&dpsp->bank);
    for (i = 0; i < dpsp->bank.nrow; i++)
      pDPSP(fp,&dpsp->dpsp[i]);
    fprintf(fp,"\n");
  }
}

/* Print dspc bank -------------------------------------------------*/
void pDSPC(FILE *fp, dspc_t *dspc)
{
  fprintf(fp,"PCID: %3hd  TDCPC: %3hd  ADCMN: %3hd  ADCLT: %3hd  ADCRB: %3hd  ADCLB: %3hd  ADCRT: %3hd  ADCVE: %3hd  \n", dspc->pcid, dspc->tdcpc, dspc->adcmn, dspc->adclt, dspc->adcrb, dspc->adclb, dspc->adcrt, dspc->adcve);
}

void printDSPCbank(FILE *fp, clasDSPC_t *dspc) {
  int i;

  if(dspc->bank.nrow) {
    pBankHead(fp,&dspc->bank);
    for (i = 0; i < dspc->bank.nrow; i++)
      pDSPC(fp,&dspc->dspc[i]);
    fprintf(fp,"\n");
  }
}

/* Print dsps bank -------------------------------------------------*/
void pDSPS(FILE *fp, dsps_t *dsps)
{
  fprintf(fp,"ID: %3hd  TDC: %3hd  ADC: %3hd  \n", dsps->id, dsps->tdc, dsps->adc);
}

void printDSPSbank(FILE *fp, clasDSPS_t *dsps) {
  int i;

  if(dsps->bank.nrow) {
    pBankHead(fp,&dsps->bank);
    for (i = 0; i < dsps->bank.nrow; i++)
      pDSPS(fp,&dsps->dsps[i]);
    fprintf(fp,"\n");
  }
}

/* Print dstc bank -------------------------------------------------*/
void pDSTC(FILE *fp, dstc_t *dstc)
{
  fprintf(fp,"TACID: %3hd  TDCTAC: %3hd  ADCLT: %3hd  ADCRT: %3hd  ADCLB: %3hd  ADCRB: %3hd  ADCSUM1: %3hd  ADCSUM2: %3hd  ADCSUM3: %3hd  \n", dstc->tacid, dstc->tdctac, dstc->adclt, dstc->adcrt, dstc->adclb, dstc->adcrb, dstc->adcsum1, dstc->adcsum2, dstc->adcsum3);
}

void printDSTCbank(FILE *fp, clasDSTC_t *dstc) {
  int i;

  if(dstc->bank.nrow) {
    pBankHead(fp,&dstc->bank);
    for (i = 0; i < dstc->bank.nrow; i++)
      pDSTC(fp,&dstc->dstc[i]);
    fprintf(fp,"\n");
  }
}

/* Print dtrk bank -------------------------------------------------*/
void pDTRK(FILE *fp, dtrk_t *dtrk)
{
  fprintf(fp,"X: %5.3f  Y: %5.3f  Z: %5.3f  \n", dtrk->x, dtrk->y, dtrk->z);
}

void printDTRKbank(FILE *fp, clasDTRK_t *dtrk) {
  int i;

  if(dtrk->bank.nrow) {
    pBankHead(fp,&dtrk->bank);
    for (i = 0; i < dtrk->bank.nrow; i++)
      pDTRK(fp,&dtrk->dtrk[i]);
    fprintf(fp,"\n");
  }
}

/* Print ec bank -------------------------------------------------*/
void pEC(FILE *fp, ec_t *ec)
{
  fprintf(fp,"ID: %3hd  TDC: %3hd  ADC: %3hd  \n", ec->id, ec->tdc, ec->adc);
}

void printECbank(FILE *fp, clasEC_t *ec) {
  int i;

  if(ec->bank.nrow) {
    pBankHead(fp,&ec->bank);
    for (i = 0; i < ec->bank.nrow; i++)
      pEC(fp,&ec->ec[i]);
    fprintf(fp,"\n");
  }
}

/* Print ec01 bank -------------------------------------------------*/
void pEC01(FILE *fp, ec01_t *ec01)
{
  fprintf(fp,"ID: %3hd  time: %5.3f  energy: %5.3f  \n", ec01->id, ec01->time, ec01->energy);
}

void printEC01bank(FILE *fp, clasEC01_t *ec01) {
  int i;

  if(ec01->bank.nrow) {
    pBankHead(fp,&ec01->bank);
    for (i = 0; i < ec01->bank.nrow; i++)
      pEC01(fp,&ec01->ec01[i]);
    fprintf(fp,"\n");
  }
}

/* Print ec1 bank -------------------------------------------------*/
void pEC1(FILE *fp, ec1_t *ec1)
{
  fprintf(fp,"ID: %3hd  TDCL: %3hd  ADCL: %3hd  TDCR: %3hd  ADCR: %3hd  \n", ec1->id, ec1->tdcl, ec1->adcl, ec1->tdcr, ec1->adcr);
}

void printEC1bank(FILE *fp, clasEC1_t *ec1) {
  int i;

  if(ec1->bank.nrow) {
    pBankHead(fp,&ec1->bank);
    for (i = 0; i < ec1->bank.nrow; i++)
      pEC1(fp,&ec1->ec1[i]);
    fprintf(fp,"\n");
  }
}

/* Print ec1p bank -------------------------------------------------*/
void pEC1P(FILE *fp, ec1p_t *ec1p)
{
  fprintf(fp,"N1x: %5.3f  N1y: %5.3f  N1z: %5.3f  R1n: %5.3f  \n", ec1p->n1x, ec1p->n1y, ec1p->n1z, ec1p->r1n);
}

void printEC1Pbank(FILE *fp, clasEC1P_t *ec1p) {
  int i;

  if(ec1p->bank.nrow) {
    pBankHead(fp,&ec1p->bank);
    for (i = 0; i < ec1p->bank.nrow; i++)
      pEC1P(fp,&ec1p->ec1p[i]);
    fprintf(fp,"\n");
  }
}

/* Print ec1r bank -------------------------------------------------*/
void pEC1R(FILE *fp, ec1r_t *ec1r)
{
  fprintf(fp,"E_tot: %5.3f  dE_tot: %5.3f  t_tot: %5.3f  dt_tot: %5.3f  x_m: %5.3f  y_m: %5.3f  z_m: %5.3f  dx_m: %5.3f  dy_m: %5.3f  dz_m: %5.3f  E_in: %5.3f  t_in: %5.3f  x_in: %5.3f  y_in: %5.3f  x_out: %5.3f  y_out: %5.3f  x2_in_l: %5.3f  x2_in_r: %5.3f  y2_in_l: %5.3f  y2_in_r: %5.3f  x2_out_l: %5.3f  x2_out_r: %5.3f  y2_out_l: %5.3f  y2_out_r: %5.3f  i_in: %6d  j_in: %6d  i_out: %6d  j_out: %6d  a_in_xl: %5.3f  a_in_xr: %5.3f  a_in_yl: %5.3f  a_in_yr: %5.3f  a_out_xl: %5.3f  a_out_xr: %5.3f  a_out_yl: %5.3f  a_out_yr: %5.3f  t_in_xs: %5.3f  t_in_xd: %5.3f  t_in_ys: %5.3f  t_in_yd: %5.3f  t_out_xs: %5.3f  t_out_xd: %5.3f  t_out_ys: %5.3f  t_out_yd: %5.3f  ibl: %6d  ncluster: %6d  pmtfired: %6d  z_in: %5.3f  z_out: %5.3f  istat: %6d  \n", ec1r->e_tot, ec1r->de_tot, ec1r->t_tot, ec1r->dt_tot, ec1r->pos.x, ec1r->pos.y, ec1r->pos.z, ec1r->dpos.x, ec1r->dpos.y, ec1r->dpos.z, ec1r->e_in, ec1r->t_in, ec1r->x_in, ec1r->y_in, ec1r->x_out, ec1r->y_out, ec1r->x2_in_l, ec1r->x2_in_r, ec1r->y2_in_l, ec1r->y2_in_r, ec1r->x2_out_l, ec1r->x2_out_r, ec1r->y2_out_l, ec1r->y2_out_r, ec1r->i_in, ec1r->j_in, ec1r->i_out, ec1r->j_out, ec1r->a_in_xl, ec1r->a_in_xr, ec1r->a_in_yl, ec1r->a_in_yr, ec1r->a_out_xl, ec1r->a_out_xr, ec1r->a_out_yl, ec1r->a_out_yr, ec1r->t_in_xs, ec1r->t_in_xd, ec1r->t_in_ys, ec1r->t_in_yd, ec1r->t_out_xs, ec1r->t_out_xd, ec1r->t_out_ys, ec1r->t_out_yd, ec1r->ibl, ec1r->ncluster, ec1r->pmtfired, ec1r->z_in, ec1r->z_out, ec1r->istat);
}


void printEC1Rbank(FILE *fp, clasEC1R_t *ec1r) {
  int i;

  if(ec1r->bank.nrow) {
    pBankHead(fp,&ec1r->bank);
    for (i = 0; i < ec1r->bank.nrow; i++)
      pEC1R(fp,&ec1r->ec1r[i]);
    fprintf(fp,"\n");
  }
}

/* Print ecca bank -------------------------------------------------*/
void pECCA(FILE *fp, ecca_t *ecca)
{
  fprintf(fp,"id: %3hd  aPED: %5.3f  aSIG: %5.3f  aMIP: %5.3f  aMIPu: %5.3f  aSHR: %5.3f  aSHRu: %5.3f  stat: %3hd  \n", ecca->id, ecca->aped, ecca->asig, ecca->amip, ecca->amipu, ecca->ashr, ecca->ashru, ecca->stat);
}

void printECCAbank(FILE *fp, clasECCA_t *ecca) {
  int i;

  if(ecca->bank.nrow) {
    pBankHead(fp,&ecca->bank);
    for (i = 0; i < ecca->bank.nrow; i++)
      pECCA(fp,&ecca->ecca[i]);
    fprintf(fp,"\n");
  }
}

/* Print eccl bank -------------------------------------------------*/
void pECCL(FILE *fp, eccl_t *eccl)
{
  fprintf(fp,"id: %3hd  lDB: %5.3f  lDBu: %5.3f  lMIP: %5.3f  lMIPu: %5.3f  lSHR: %5.3f  lSHRu: %5.3f  stat: %3hd  \n", eccl->id, eccl->ldb, eccl->ldbu, eccl->lmip, eccl->lmipu, eccl->lshr, eccl->lshru, eccl->stat);
}

void printECCLbank(FILE *fp, clasECCL_t *eccl) {
  int i;

  if(eccl->bank.nrow) {
    pBankHead(fp,&eccl->bank);
    for (i = 0; i < eccl->bank.nrow; i++)
      pECCL(fp,&eccl->eccl[i]);
    fprintf(fp,"\n");
  }
}

/* Print ecct bank -------------------------------------------------*/
void pECCT(FILE *fp, ecct_t *ecct)
{
  fprintf(fp,"id: %3hd  tOFF: %5.3f  tOFFu: %5.3f  tGAIN: %5.3f  tGAINu: %5.3f  tW0: %5.3f  tW0u: %5.3f  tW1: %5.3f  tW1u: %5.3f  tVEF: %5.3f  tVEFu: %5.3f  stat: %3hd  \n", ecct->id, ecct->toff, ecct->toffu, ecct->tgain, ecct->tgainu, ecct->tw0, ecct->tw0u, ecct->tw1, ecct->tw1u, ecct->tvef, ecct->tvefu, ecct->stat);
}

void printECCTbank(FILE *fp, clasECCT_t *ecct) {
  int i;

  if(ecct->bank.nrow) {
    pBankHead(fp,&ecct->bank);
    for (i = 0; i < ecct->bank.nrow; i++)
      pECCT(fp,&ecct->ecct[i]);
    fprintf(fp,"\n");
  }
}

/* Print ecdi bank -------------------------------------------------*/
void pECDI(FILE *fp, ecdi_t *ecdi)
{
  fprintf(fp,"crate: %6d  slot: %6d  mask: %6d  threshold: %6d  width: %6d  \n", ecdi->crate, ecdi->slot, ecdi->mask, ecdi->threshold, ecdi->width);
}

void printECDIbank(FILE *fp, clasECDI_t *ecdi) {
  int i;

  if(ecdi->bank.nrow) {
    pBankHead(fp,&ecdi->bank);
    for (i = 0; i < ecdi->bank.nrow; i++)
      pECDI(fp,&ecdi->ecdi[i]);
    fprintf(fp,"\n");
  }
}

/* Print ecg bank -------------------------------------------------*/
void pECG(FILE *fp, ecg_t *ecg)
{
  fprintf(fp,"L0: %5.3f  THE0: %5.3f  YLOW: %5.3f  YHI: %5.3f  DYLOW: %5.3f  DYHI: %5.3f  LRTH: %5.3f  TANGR: %5.3f  SECTOR: %3hd  PHISEC: %5.3f  X0OFF: %5.3f  Y0OFF: %5.3f  Z0OFF: %5.3f  ROTM11: %5.3f  ROTM12: %5.3f  ROTM13: %5.3f  ROTM21: %5.3f  ROTM22: %5.3f  ROTM23: %5.3f  ROTM31: %5.3f  ROTM32: %5.3f  ROTM33: %5.3f  \n", ecg->l0, ecg->the0, ecg->ylow, ecg->yhi, ecg->dylow, ecg->dyhi, ecg->lrth, ecg->tangr, ecg->sector, ecg->phisec, ecg->x0off, ecg->y0off, ecg->z0off, ecg->rotm11, ecg->rotm12, ecg->rotm13, ecg->rotm21, ecg->rotm22, ecg->rotm23, ecg->rotm31, ecg->rotm32, ecg->rotm33);
}

void printECGbank(FILE *fp, clasECG_t *ecg) {
  int i;

  if(ecg->bank.nrow) {
    pBankHead(fp,&ecg->bank);
    for (i = 0; i < ecg->bank.nrow; i++)
      pECG(fp,&ecg->ecg[i]);
    fprintf(fp,"\n");
  }
}

/* Print ech bank -------------------------------------------------*/
void pECH(FILE *fp, ech_t *ech)
{
  fprintf(fp,"x: %5.3f  y: %5.3f  z: %5.3f  cx: %5.3f  cy: %5.3f  cz: %5.3f  pmom: %5.3f  id: %3hd  tof: %5.3f  edepin: %5.3f  edepout: %5.3f  \n", ech->x, ech->y, ech->z, ech->cx, ech->cy, ech->cz, ech->pmom, ech->id, ech->tof, ech->edepin, ech->edepout);
}

void printECHbank(FILE *fp, clasECH_t *ech) {
  int i;

  if(ech->bank.nrow) {
    pBankHead(fp,&ech->bank);
    for (i = 0; i < ech->bank.nrow; i++)
      pECH(fp,&ech->ech[i]);
    fprintf(fp,"\n");
  }
}

/* Print echb bank -------------------------------------------------*/
void pECHB(FILE *fp, echb_t *echb)
{
  fprintf(fp,"Sect: %3d  E__hit: %5.3f  dE_hit: %5.3f  t_hit: %5.3f  dt_hi: %5.3f  i_hit: %5.3f  j_hit: %5.3f  di_hit: %5.3f  dj_hit: %5.3f  x_hit: %5.3f  y_hit: %5.3f  z_hit: %5.3f  dx_hit: %5.3f  dy_hit: %5.3f  dz_hit: %5.3f  u2_hit: %5.3f  v2_hit: %5.3f  w2_hit: %5.3f  u3_hit: %5.3f  v3_hit: %5.3f  w3_hit: %5.3f  u4_hit: %5.3f  v4_hit: %5.3f  w4_hit: %5.3f  centr_U: %5.3f  centr_V: %5.3f  centr_W: %5.3f  path_U: %5.3f  path_V: %5.3f  path_W: %5.3f  Nstrp_U: %3hd  Nstrp_V: %3hd  Nstrp_W: %3hd  MatchID1: %3hd  CH21: %5.3f  MatchID2: %3hd  CH22: %5.3f  istat: %3hd  \n\n", echb->sect, echb->e__hit, echb->de_hit, echb->t_hit, echb->dt_hi, echb->i_hit, echb->j_hit, echb->di_hit, echb->dj_hit, echb->x_hit, echb->y_hit, echb->z_hit, echb->dx_hit, echb->dy_hit, echb->dz_hit, echb->u2_hit, echb->v2_hit, echb->w2_hit, echb->u3_hit, echb->v3_hit, echb->w3_hit, echb->u4_hit, echb->v4_hit, echb->w4_hit, echb->centr_u, echb->centr_v, echb->centr_w, echb->path_u, echb->path_v, echb->path_w, echb->nstrp_u, echb->nstrp_v, echb->nstrp_w, echb->matchid1, echb->ch21, echb->matchid2, echb->ch22, echb->istat);
}

void printECHBbank(FILE *fp, clasECHB_t *echb) {
  int i;

  if(echb->bank.nrow) {
    pBankHead(fp,&echb->bank);
    for (i = 0; i < echb->bank.nrow; i++)
      pECHB(fp,&echb->echb[i]);
    fprintf(fp,"\n");
  }
}

/* Print ecmt bank -------------------------------------------------*/
void pECMT(FILE *fp, ecmt_t *ecmt)
{
  fprintf(fp,"in_high: %6d  out_high: %6d  total_high: %6d  in_lo: %6d  out_lo: %6d  total_lo: %6d  \n", ecmt->in_high, ecmt->out_high, ecmt->total_high, ecmt->in_lo, ecmt->out_lo, ecmt->total_lo);
}

void printECMTbank(FILE *fp, clasECMT_t *ecmt) {
  int i;

  if(ecmt->bank.nrow) {
    pBankHead(fp,&ecmt->bank);
    for (i = 0; i < ecmt->bank.nrow; i++)
      pECMT(fp,&ecmt->ecmt[i]);
    fprintf(fp,"\n");
  }
}

/* Print ecp bank -------------------------------------------------*/
void pECP(FILE *fp, ecp_t *ecp)
{
  fprintf(fp,"N1x: %5.3f  N1y: %5.3f  N1z: %5.3f  R1n: %5.3f  PlW: %5.3f  PlI: %5.3f  PlO: %5.3f  \n", ecp->n1x, ecp->n1y, ecp->n1z, ecp->r1n, ecp->plw, ecp->pli, ecp->plo);
}

void printECPbank(FILE *fp, clasECP_t *ecp) {
  int i;

  if(ecp->bank.nrow) {
    pBankHead(fp,&ecp->bank);
    for (i = 0; i < ecp->bank.nrow; i++)
      pECP(fp,&ecp->ecp[i]);
    fprintf(fp,"\n");
  }
}

/* Print ecp1 bank -------------------------------------------------*/
void pECP1(FILE *fp, ecp1_t *ecp1)
{
  fprintf(fp,"ID: %6d  mean_left: %6d  sigma_left: %5.3f  mean_right: %6d  sigma_right: %5.3f  \n", ecp1->id, ecp1->mean_left, ecp1->sigma_left, ecp1->mean_right, ecp1->sigma_right);
}

void printECP1bank(FILE *fp, clasECP1_t *ecp1) {
  int i;

  if(ecp1->bank.nrow) {
    pBankHead(fp,&ecp1->bank);
    for (i = 0; i < ecp1->bank.nrow; i++)
      pECP1(fp,&ecp1->ecp1[i]);
    fprintf(fp,"\n");
  }
}

/* Print ecpb bank -------------------------------------------------*/
void pECPB(FILE *fp, ecpb_t *ecpb)
{
  fprintf(fp,"ScHt: %3d  Etot: %5.3f  Ein: %5.3f  Eout: %5.3f  Time: %5.3f  Path: %5.3f  X: %5.3f  Y: %5.3f  Z: %5.3f  M2_hit: %5.3f  M3_hit: %5.3f  M4_hit: %5.3f  InnStr: %3d  OutStr: %3d  Chi2EC: %5.3f  Status: %3hd  \n", ecpb->scht, ecpb->etot, ecpb->ein, ecpb->eout, ecpb->time, ecpb->path, ecpb->x, ecpb->y, ecpb->z, ecpb->m2_hit, ecpb->m3_hit, ecpb->m4_hit, ecpb->innstr, ecpb->outstr, ecpb->chi2ec, ecpb->status);
}

void printECPBbank(FILE *fp, clasECPB_t *ecpb) {
  int i;

  if(ecpb->bank.nrow) {
    pBankHead(fp,&ecpb->bank);
    for (i = 0; i < ecpb->bank.nrow; i++)
      pECPB(fp,&ecpb->ecpb[i]);
    fprintf(fp,"\n");
  }
}

/* Print ecpe bank -------------------------------------------------*/
void pECPE(FILE *fp, ecpe_t *ecpe)
{
  fprintf(fp,"ID: %6d  mean: %6d  sigma: %5.3f  \n", ecpe->id, ecpe->mean, ecpe->sigma);
}

void printECPEbank(FILE *fp, clasECPE_t *ecpe) {
  int i;

  if(ecpe->bank.nrow) {
    pBankHead(fp,&ecpe->bank);
    for (i = 0; i < ecpe->bank.nrow; i++)
      pECPE(fp,&ecpe->ecpe[i]);
    fprintf(fp,"\n");
  }
}


/* Print ecpi bank -------------------------------------------------*/
void pECPI(FILE *fp, ecpi_t *ecpi)
{
  fprintf(fp,"ID: %3hd  Layer: %3hd  HITID: %3hd  iloc: %5.3f  jloc: %5.3f  diloc: %5.3f  djloc: %5.3f  R: %5.3f  E: %5.3f  \n", ecpi->id, ecpi->layer, ecpi->hitid, ecpi->iloc, ecpi->jloc, ecpi->diloc, ecpi->djloc, ecpi->r, ecpi->e);
}

void printECPIbank(FILE *fp, clasECPI_t *ecpi) {
  int i;

  if(ecpi->bank.nrow) {
    pBankHead(fp,&ecpi->bank);
    for (i = 0; i < ecpi->bank.nrow; i++)
      pECPI(fp,&ecpi->ecpi[i]);
    fprintf(fp,"\n");
  }
}

/* Print ecpo bank -------------------------------------------------*/
void pECPO(FILE *fp, ecpo_t *ecpo)
{
  fprintf(fp,"ID: %3hd  UID: %3hd  VID: %3hd  WID: %3hd  E: %5.3f  T: %5.3f  X: %5.3f  Y: %5.3f  Z: %5.3f  \n", ecpo->id, ecpo->uid, ecpo->vid, ecpo->wid, ecpo->e, ecpo->t, ecpo->x, ecpo->y, ecpo->z);
}

void printECPObank(FILE *fp, clasECPO_t *ecpo) {
  int i;

  if(ecpo->bank.nrow) {
    pBankHead(fp,&ecpo->bank);
    for (i = 0; i < ecpo->bank.nrow; i++)
      pECPO(fp,&ecpo->ecpo[i]);
    fprintf(fp,"\n");
  }
}

/* Print ecrb bank -------------------------------------------------*/
void pECRB(FILE *fp, ecrb_t *ecrb)
{
  fprintf(fp,"E_in: %5.3f  E_out: %5.3f  dE_in: %5.3f  dE_out: %5.3f  t_in: %5.3f  t_out: %5.3f  dt_in: %5.3f  dt_out: %5.3f  i_in: %5.3f  j_in: %5.3f  i_out: %5.3f  j_out: %5.3f  di_in: %5.3f  dj_in: %5.3f  di_out: %5.3f  dj_out: %5.3f  x_in: %5.3f  y_in: %5.3f  z_in: %5.3f  x_out: %5.3f  y_out: %5.3f  z_out: %5.3f  dx_in: %5.3f  dy_in: %5.3f  dz_in: %5.3f  dx_out: %5.3f  dy_out: %5.3f  dz_out: %5.3f  u2_in: %5.3f  v2_in: %5.3f  w2_in: %5.3f  u2_out: %5.3f  v2_out: %5.3f  w2_out: %5.3f  u3_in: %5.3f  v3_in: %5.3f  w3_in: %5.3f  u3_out: %5.3f  v3_out: %5.3f  w3_out: %5.3f  i2: %5.3f  j2: %5.3f  i3: %5.3f  j3: %5.3f  spare1: %5.3f  spare2: %5.3f  spare3: %5.3f  spare4: %5.3f  istat: %3hd  \n", ecrb->e_in, ecrb->e_out, ecrb->de_in, ecrb->de_out, ecrb->t_in, ecrb->t_out, ecrb->dt_in, ecrb->dt_out, ecrb->i_in, ecrb->j_in, ecrb->i_out, ecrb->j_out, ecrb->di_in, ecrb->dj_in, ecrb->di_out, ecrb->dj_out, ecrb->x_in, ecrb->y_in, ecrb->z_in, ecrb->x_out, ecrb->y_out, ecrb->z_out, ecrb->dx_in, ecrb->dy_in, ecrb->dz_in, ecrb->dx_out, ecrb->dy_out, ecrb->dz_out, ecrb->u2_in, ecrb->v2_in, ecrb->w2_in, ecrb->u2_out, ecrb->v2_out, ecrb->w2_out, ecrb->u3_in, ecrb->v3_in, ecrb->w3_in, ecrb->u3_out, ecrb->v3_out, ecrb->w3_out, ecrb->i2, ecrb->j2, ecrb->i3, ecrb->j3, ecrb->spare1, ecrb->spare2, ecrb->spare3, ecrb->spare4, ecrb->istat);
}

void printECRBbank(FILE *fp, clasECRB_t *ecrb) {
  int i;

  if(ecrb->bank.nrow) {
    pBankHead(fp,&ecrb->bank);
    for (i = 0; i < ecrb->bank.nrow; i++)
      pECRB(fp,&ecrb->ecrb[i]);
    fprintf(fp,"\n");
  }
}

/* Print eid0 bank -------------------------------------------------*/
void pEID0(FILE *fp, eid0_t *eid0)
{
  fprintf(fp,"jeid0sec: %3hd  jeid0cc: %3hd  jeid0ec: %3hd  jeid0sc: %3hd  \n", eid0->jeid0sec, eid0->jeid0cc, eid0->jeid0ec, eid0->jeid0sc);
}

void printEID0bank(FILE *fp, clasEID0_t *eid0) {
  int i;

  if(eid0->bank.nrow) {
    pBankHead(fp,&eid0->bank);
    for (i = 0; i < eid0->bank.nrow; i++)
      pEID0(fp,&eid0->eid0[i]);
    fprintf(fp,"\n");
  }
}

/* Print epic bank -------------------------------------------------*/
void pEPIC(FILE *fp, epic_t *epic)
{
  fprintf(fp,"value: %5.3f  name: %c%c%c%c%c%c%c%c\n", epic->value, epic->char1, epic->char2, epic->char3, epic->char4, epic->char5, epic->char6, epic->char7, epic->char8);
}

void printEPICbank(FILE *fp, clasEPIC_t *epic) {
  int i;

  if(epic->bank.nrow) {
    pBankHead(fp,&epic->bank);
    for (i = 0; i < epic->bank.nrow; i++)
      pEPIC(fp,&epic->epic[i]);
    fprintf(fp,"\n");
  }
}

/* Print evnt bank -------------------------------------------------*/
void pEVNT(FILE *fp, evnt_t *evnt)
{
  fprintf(fp,"ID: %3hd  Pmom: %5.3f  Mass: %5.3f  Charge: %3hd  Betta: %5.3f  Cx: %5.3f  cy: %5.3f  cz: %5.3f  X: %5.3f  Y: %5.3f  Z: %5.3f  DCstat: %3hd  CCstat: %3hd  SCstat: %3hd  ECstat: %3hd  LCstat: %3hd  STstat: %3hd  Status: %3hd  \n", evnt->id, evnt->pmom, evnt->mass, evnt->charge, evnt->betta, evnt->dir_cos.x, evnt->dir_cos.y, evnt->dir_cos.z, evnt->vert.x, evnt->vert.y, evnt->vert.z, evnt->dcstat, evnt->ccstat, evnt->scstat, evnt->ecstat, evnt->lcstat, evnt->ststat, evnt->status);
}

void printEVNTbank(FILE *fp, clasEVNT_t *evnt) {
  int i;

  if(evnt->bank.nrow) {
    pBankHead(fp,&evnt->bank);
    for (i = 0; i < evnt->bank.nrow; i++)
      pEVNT(fp,&evnt->evnt[i]);
    fprintf(fp,"\n");
  }
}

/* Print fbpm bank -------------------------------------------------*/
void pFBPM(FILE *fp, fbpm_t *fbpm)
{
  fprintf(fp,"ID: %6d  TDC: %6d  ADC: %6d  \n", fbpm->id, fbpm->tdc, fbpm->adc);
}

void printFBPMbank(FILE *fp, clasFBPM_t *fbpm) {
  int i;

  if(fbpm->bank.nrow) {
    pBankHead(fp,&fbpm->bank);
    for (i = 0; i < fbpm->bank.nrow; i++)
      pFBPM(fp,&fbpm->fbpm[i]);
    fprintf(fp,"\n");
  }
}

/* Print g1sl bank -------------------------------------------------*/
void pG1SL(FILE *fp, g1sl_t *g1sl)
{
  fprintf(fp,"G1T1: %d  G1T2: %d  G1T3: %d  G1T4: %d  G1T5: %d  G1T6: %d  G1T7: %d  G1T8: %d  G1T9: %d  G1T10: %d  G1T11: %d  G1T12: %d  G1T13: %d  G1T14: %d  G1T15: %d  G1T16: %d  G1T17: %d  G1T18: %d  G1T19: %d  G1T20: %d  G1T21: %d  G1T22: %d  G1T23: %d  G1T24: %d  G1T25: %d  G1T26: %d  G1T27: %d  G1T28: %d  G1T29: %d  G1T30: %d  G1T31: %d  G1T32: %d  G1T33: %d  G1T34: %d  G1T35: %d  G1T36: %d  G1T37: %d  G1T38: %d  G1T39: %d  G1T40: %d  G1T41: %d  G1T42: %d  G1T43: %d  G1T44: %d  G1T45: %d  G1T46: %d  G1T47: %d  G1T48: %d  G1T49: %d  G1T50: %d  G1T51: %d  G1T52: %d  G1T53: %d  G1T54: %d  G1T55: %d  G1T56: %d  G1T57: %d  G1T58: %d  G1T59: %d  G1T60: %d  G1T61: %d  G1T62: %d  G1T63: %d  G1T64: %d  \n", g1sl->g1t1, g1sl->g1t2, g1sl->g1t3, g1sl->g1t4, g1sl->g1t5, g1sl->g1t6, g1sl->g1t7, g1sl->g1t8, g1sl->g1t9, g1sl->g1t10, g1sl->g1t11, g1sl->g1t12, g1sl->g1t13, g1sl->g1t14, g1sl->g1t15, g1sl->g1t16, g1sl->g1t17, g1sl->g1t18, g1sl->g1t19, g1sl->g1t20, g1sl->g1t21, g1sl->g1t22, g1sl->g1t23, g1sl->g1t24, g1sl->g1t25, g1sl->g1t26, g1sl->g1t27, g1sl->g1t28, g1sl->g1t29, g1sl->g1t30, g1sl->g1t31, g1sl->g1t32, g1sl->g1t33, g1sl->g1t34, g1sl->g1t35, g1sl->g1t36, g1sl->g1t37, g1sl->g1t38, g1sl->g1t39, g1sl->g1t40, g1sl->g1t41, g1sl->g1t42, g1sl->g1t43, g1sl->g1t44, g1sl->g1t45, g1sl->g1t46, g1sl->g1t47, g1sl->g1t48, g1sl->g1t49, g1sl->g1t50, g1sl->g1t51, g1sl->g1t52, g1sl->g1t53, g1sl->g1t54, g1sl->g1t55, g1sl->g1t56, g1sl->g1t57, g1sl->g1t58, g1sl->g1t59, g1sl->g1t60, g1sl->g1t61, g1sl->g1t62, g1sl->g1t63, g1sl->g1t64);
}

void printG1SLbank(FILE *fp, clasG1SL_t *g1sl) {
  int i;

  if(g1sl->bank.nrow) {
    pBankHead(fp,&g1sl->bank);
    for (i = 0; i < g1sl->bank.nrow; i++)
      pG1SL(fp,&g1sl->g1sl[i]);
    fprintf(fp,"\n");
  }
}

/* Print g2sl bank -------------------------------------------------*/
void pG2SL(FILE *fp, g2sl_t *g2sl)
{
  fprintf(fp,"G2T1: %d  G2T2: %d  G2T3: %d  G2T4: %d  G2T5: %d  G2T6: %d  G2T7: %d  G2T8: %d  G2T9: %d  G2T10: %d  G2T11: %d  G2T12: %d  G2T13: %d  G2T14: %d  G2T15: %d  G2T16: %d  G2T17: %d  G2T18: %d  G2T19: %d  G2T20: %d  G2T21: %d  G2T22: %d  G2T23: %d  G2T24: %d  G2T25: %d  G2T26: %d  G2T27: %d  G2T28: %d  G2T29: %d  G2T30: %d  G2T31: %d  G2T32: %d  G2T33: %d  G2T34: %d  G2T35: %d  G2T36: %d  G2T37: %d  G2T38: %d  G2T39: %d  G2T40: %d  G2T41: %d  G2T42: %d  G2T43: %d  G2T44: %d  G2T45: %d  G2T46: %d  G2T47: %d  G2T48: %d  G2T49: %d  G2T50: %d  G2T51: %d  G2T52: %d  G2T53: %d  G2T54: %d  G2T55: %d  G2T56: %d  G2T57: %d  G2T58: %d  G2T59: %d  G2T60: %d  G2T61: %d  G2T62: %d  G2T63: %d  G2T64: %d  \n", g2sl->g2t1, g2sl->g2t2, g2sl->g2t3, g2sl->g2t4, g2sl->g2t5, g2sl->g2t6, g2sl->g2t7, g2sl->g2t8, g2sl->g2t9, g2sl->g2t10, g2sl->g2t11, g2sl->g2t12, g2sl->g2t13, g2sl->g2t14, g2sl->g2t15, g2sl->g2t16, g2sl->g2t17, g2sl->g2t18, g2sl->g2t19, g2sl->g2t20, g2sl->g2t21, g2sl->g2t22, g2sl->g2t23, g2sl->g2t24, g2sl->g2t25, g2sl->g2t26, g2sl->g2t27, g2sl->g2t28, g2sl->g2t29, g2sl->g2t30, g2sl->g2t31, g2sl->g2t32, g2sl->g2t33, g2sl->g2t34, g2sl->g2t35, g2sl->g2t36, g2sl->g2t37, g2sl->g2t38, g2sl->g2t39, g2sl->g2t40, g2sl->g2t41, g2sl->g2t42, g2sl->g2t43, g2sl->g2t44, g2sl->g2t45, g2sl->g2t46, g2sl->g2t47, g2sl->g2t48, g2sl->g2t49, g2sl->g2t50, g2sl->g2t51, g2sl->g2t52, g2sl->g2t53, g2sl->g2t54, g2sl->g2t55, g2sl->g2t56, g2sl->g2t57, g2sl->g2t58, g2sl->g2t59, g2sl->g2t60, g2sl->g2t61, g2sl->g2t62, g2sl->g2t63, g2sl->g2t64);
}

void printG2SLbank(FILE *fp, clasG2SL_t *g2sl) {
  int i;

  if(g2sl->bank.nrow) {
    pBankHead(fp,&g2sl->bank);
    for (i = 0; i < g2sl->bank.nrow; i++)
      pG2SL(fp,&g2sl->g2sl[i]);
    fprintf(fp,"\n");
  }
}

/* Print g3sl bank -------------------------------------------------*/
void pG3SL(FILE *fp, g3sl_t *g3sl)
{
  fprintf(fp,"G3T1: %d  G3T2: %d  G3T3: %d  G3T4: %d  G3T5: %d  G3T6: %d  G3T7: %d  G3T8: %d  G3T9: %d  G3T10: %d  G3T11: %d  G3T12: %d  G3T13: %d  G3T14: %d  G3T15: %d  G3T16: %d  G3T17: %d  G3T18: %d  G3T19: %d  G3T20: %d  G3T21: %d  G3T22: %d  G3T23: %d  G3T24: %d  G3T25: %d  G3T26: %d  G3T27: %d  G3T28: %d  G3T29: %d  G3T30: %d  G3T31: %d  G3T32: %d  G3T33: %d  G3T34: %d  G3T35: %d  G3T36: %d  G3T37: %d  G3T38: %d  G3T39: %d  G3T40: %d  G3T41: %d  G3T42: %d  G3T43: %d  G3T44: %d  G3T45: %d  G3T46: %d  G3T47: %d  G3T48: %d  G3T49: %d  G3T50: %d  G3T51: %d  G3T52: %d  G3T53: %d  G3T54: %d  G3T55: %d  G3T56: %d  G3T57: %d  G3T58: %d  G3T59: %d  G3T60: %d  G3T61: %d  G3T62: %d  G3T63: %d  G3T64: %d  \n", g3sl->g3t1, g3sl->g3t2, g3sl->g3t3, g3sl->g3t4, g3sl->g3t5, g3sl->g3t6, g3sl->g3t7, g3sl->g3t8, g3sl->g3t9, g3sl->g3t10, g3sl->g3t11, g3sl->g3t12, g3sl->g3t13, g3sl->g3t14, g3sl->g3t15, g3sl->g3t16, g3sl->g3t17, g3sl->g3t18, g3sl->g3t19, g3sl->g3t20, g3sl->g3t21, g3sl->g3t22, g3sl->g3t23, g3sl->g3t24, g3sl->g3t25, g3sl->g3t26, g3sl->g3t27, g3sl->g3t28, g3sl->g3t29, g3sl->g3t30, g3sl->g3t31, g3sl->g3t32, g3sl->g3t33, g3sl->g3t34, g3sl->g3t35, g3sl->g3t36, g3sl->g3t37, g3sl->g3t38, g3sl->g3t39, g3sl->g3t40, g3sl->g3t41, g3sl->g3t42, g3sl->g3t43, g3sl->g3t44, g3sl->g3t45, g3sl->g3t46, g3sl->g3t47, g3sl->g3t48, g3sl->g3t49, g3sl->g3t50, g3sl->g3t51, g3sl->g3t52, g3sl->g3t53, g3sl->g3t54, g3sl->g3t55, g3sl->g3t56, g3sl->g3t57, g3sl->g3t58, g3sl->g3t59, g3sl->g3t60, g3sl->g3t61, g3sl->g3t62, g3sl->g3t63, g3sl->g3t64);
}

void printG3SLbank(FILE *fp, clasG3SL_t *g3sl) {
  int i;

  if(g3sl->bank.nrow) {
    pBankHead(fp,&g3sl->bank);
    for (i = 0; i < g3sl->bank.nrow; i++)
      pG3SL(fp,&g3sl->g3sl[i]);
    fprintf(fp,"\n");
  }
}

/* Print g4sl bank -------------------------------------------------*/
void pG4SL(FILE *fp, g4sl_t *g4sl)
{
  fprintf(fp,"G4T1: %d  G4T2: %d  G4T3: %d  G4T4: %d  G4T5: %d  G4T6: %d  G4T7: %d  G4T8: %d  G4T9: %d  G4T10: %d  G4T11: %d  G4T12: %d  G4T13: %d  G4T14: %d  G4T15: %d  G4T16: %d  G4T17: %d  G4T18: %d  G4T19: %d  G4T20: %d  G4T21: %d  G4T22: %d  G4T23: %d  G4T24: %d  G4T25: %d  G4T26: %d  G4T27: %d  G4T28: %d  G4T29: %d  G4T30: %d  G4T31: %d  G4T32: %d  G4T33: %d  G4T34: %d  G4T35: %d  G4T36: %d  G4T37: %d  G4T38: %d  G4T39: %d  G4T40: %d  G4T41: %d  G4T42: %d  G4T43: %d  G4T44: %d  G4T45: %d  G4T46: %d  G4T47: %d  G4T48: %d  G4T49: %d  G4T50: %d  G4T51: %d  G4T52: %d  G4T53: %d  G4T54: %d  G4T55: %d  G4T56: %d  G4T57: %d  G4T58: %d  G4T59: %d  G4T60: %d  G4T61: %d  G4T62: %d  G4T63: %d  G4T64: %d  \n", g4sl->g4t1, g4sl->g4t2, g4sl->g4t3, g4sl->g4t4, g4sl->g4t5, g4sl->g4t6, g4sl->g4t7, g4sl->g4t8, g4sl->g4t9, g4sl->g4t10, g4sl->g4t11, g4sl->g4t12, g4sl->g4t13, g4sl->g4t14, g4sl->g4t15, g4sl->g4t16, g4sl->g4t17, g4sl->g4t18, g4sl->g4t19, g4sl->g4t20, g4sl->g4t21, g4sl->g4t22, g4sl->g4t23, g4sl->g4t24, g4sl->g4t25, g4sl->g4t26, g4sl->g4t27, g4sl->g4t28, g4sl->g4t29, g4sl->g4t30, g4sl->g4t31, g4sl->g4t32, g4sl->g4t33, g4sl->g4t34, g4sl->g4t35, g4sl->g4t36, g4sl->g4t37, g4sl->g4t38, g4sl->g4t39, g4sl->g4t40, g4sl->g4t41, g4sl->g4t42, g4sl->g4t43, g4sl->g4t44, g4sl->g4t45, g4sl->g4t46, g4sl->g4t47, g4sl->g4t48, g4sl->g4t49, g4sl->g4t50, g4sl->g4t51, g4sl->g4t52, g4sl->g4t53, g4sl->g4t54, g4sl->g4t55, g4sl->g4t56, g4sl->g4t57, g4sl->g4t58, g4sl->g4t59, g4sl->g4t60, g4sl->g4t61, g4sl->g4t62, g4sl->g4t63, g4sl->g4t64);
}

void printG4SLbank(FILE *fp, clasG4SL_t *g4sl) {
  int i;

  if(g4sl->bank.nrow) {
    pBankHead(fp,&g4sl->bank);
    for (i = 0; i < g4sl->bank.nrow; i++)
      pG4SL(fp,&g4sl->g4sl[i]);
    fprintf(fp,"\n");
  }
}

/* Print gcpb bank -------------------------------------------------*/
void pGCPB(FILE *fp, gcpb_t *gcpb)
{
  fprintf(fp,"Id: %3d  vert->{x: %5.3f  y: %5.3f  z: %5.3f}  dE/dX: %5.3f  p->{x: %5.3f  y: %5.3f  z: %5.3f}  p_tot: %5.3f   x2: %5.3f   theta: %6.2f   q: %8.3f   dca: %5.3f   index: %4d   phi: %6.2f   vtl: %5.3f   sdist: %5.3f   edist: %5.3f   npts: %2d    r_0 : %5.3f\n",
	  gcpb->pid, gcpb->vert.x, gcpb->vert.y, gcpb->vert.z, gcpb->dedx,
	  gcpb->p.x, gcpb->p.y, gcpb->p.z,  
	  gcpb->p_tot, gcpb->x2, gcpb->theta, gcpb->q, gcpb->dca, 
	  gcpb->index, gcpb->phi, gcpb->vtl, gcpb->sdist, gcpb->edist, gcpb->npts, gcpb->r_0);
}

void printGCPBbank(FILE *fp, clasGCPB_t *gcpb) {
  int i;

  if(gcpb->bank.nrow) {
    pBankHead(fp,&gcpb->bank);
    for (i = 0; i < gcpb->bank.nrow; i++)
      pGCPB(fp,&gcpb->gcpb[i]);
    fprintf(fp,"\n");
  }
}



/* Print gpid bank -------------------------------------------------*/

void pGPID(FILE *fp, gpid_t *gpid)

{
  fprintf(fp,"pid: %6d\n"
	  "   vert->\tx: %5.3f\ty: %5.3f\tz: %5.3f  \n"
	  "   p->\t\tE: %5.3f\tpx: %5.3f\tpy: %5.3f\tpz: %5.3f  \n"
	  "   q: %3d\ttrkid: %3d\tsec: %3d\tpaddle: %3d\n"
	  "   dedx: %5.3f\tbeta: %5.3f  \n"
	  "   sc_stat: %3d\tsc_time: %6.3f \tsc_len: %6.3f  \n"
	  "   st_stat: %3d\tst_time: %6.3f \tst_len: %6.3f  \n"
	  "   mass: %5.3f\tmass_ref: %3d\tbetam: %5.3f  \n"
	  "   epho: %5.3f\ttpho: %5.3f\ttagrid: %3d\tngrf: %3d\tppid: %3d\n", 
	  gpid->pid, gpid->vert.x, gpid->vert.y, gpid->vert.z, gpid->p.t, gpid->p.space.x, gpid->p.space.y, gpid->p.space.z, gpid->q, gpid->trkid, gpid->sec, gpid->paddle, gpid->dedx, gpid->beta, gpid->sc_stat, gpid->sc_time, gpid->sc_len, gpid->st_stat, gpid->st_time, gpid->st_len, gpid->mass, gpid->mass_ref, gpid->betam, gpid->epho, gpid->tpho, gpid->tagrid, gpid->ngrf,gpid->ppid);
}


void printGPIDbank(FILE *fp, clasGPID_t *gpid) {
  int i;

  if(gpid->bank.nrow) {
    pBankHead(fp,&gpid->bank);
    for (i = 0; i < gpid->bank.nrow; i++)
      pGPID(fp,&gpid->gpid[i]);
    fprintf(fp,"\n");
  }
}








/* Print hber bank -------------------------------------------------*/
void pHBER(FILE *fp, hber_t *hber)
{
  fprintf(fp,"q_over_p: %6.4f  lambda: %6.4f  phi: %6.4f  d0: %6.4f  z0: %6.4f  c11: %g  c12: %g  c13: %g  c14: %g  c15: %g  c22: %g  c23: %g  c24: %g  c25: %g  c33: %g  c34: %g  c35: %g  c44: %g  c45: %g  c55: %g  chi2: %6.4f  layinfo1: %d  layinfo2: %d  \n", hber->q_over_p, hber->lambda, hber->phi, hber->d0, hber->z0, hber->c11, hber->c12, hber->c13, hber->c14, hber->c15, hber->c22, hber->c23, hber->c24, hber->c25, hber->c33, hber->c34, hber->c35, hber->c44, hber->c45, hber->c55, hber->chi2, hber->layinfo1, hber->layinfo2);
}

void printHBERbank(FILE *fp, clasHBER_t *hber) {
  int i;

  if(hber->bank.nrow) {
    pBankHead(fp,&hber->bank);
    for (i = 0; i < hber->bank.nrow; i++)
      pHBER(fp,&hber->hber[i]);
    fprintf(fp,"\n");
  }
}

/* Print hbid bank -------------------------------------------------*/
void pHBID(FILE *fp, hbid_t *hbid)
{
  fprintf(fp,"track: %6d  sec: %6d  beta: %5.3f  vtime: %5.3f  sc_stat: %6d  sc_id: %6d  sc_time: %5.3f  sc_qual: %5.3f  sc_vtime: %5.3f  sc_beta: %5.3f  cc_stat: %6d  cc_id: %6d  cc_time: %5.3f  cc_qual: %5.3f  cc_vtime: %5.3f  cc_beta: %5.3f  ec_stat: %6d  ec_id: %6d  ec_time: %5.3f  ec_qual: %5.3f  ec_vtime: %5.3f  ec_beta: %5.3f  st_stat: %6d  st_id: %6d  st_time: %5.3f  st_qual: %5.3f  st_vtime: %5.3f  st_beta: %5.3f  lac_stat: %6d  lac_id: %6d  lac_time: %5.3f  lac_qual: %5.3f  lac_vtime: %5.3f  lac_beta: %5.3f  \n", hbid->track, hbid->sec, hbid->beta, hbid->vtime, hbid->sc_stat, hbid->sc_id, hbid->sc_time, hbid->sc_qual, hbid->sc_vtime, hbid->sc_beta, hbid->cc_stat, hbid->cc_id, hbid->cc_time, hbid->cc_qual, hbid->cc_vtime, hbid->cc_beta, hbid->ec_stat, hbid->ec_id, hbid->ec_time, hbid->ec_qual, hbid->ec_vtime, hbid->ec_beta, hbid->st_stat, hbid->st_id, hbid->st_time, hbid->st_qual, hbid->st_vtime, hbid->st_beta, hbid->lac_stat, hbid->lac_id, hbid->lac_time, hbid->lac_qual, hbid->lac_vtime, hbid->lac_beta);
}

void printHBIDbank(FILE *fp, clasHBID_t *hbid) {
  int i;

  if(hbid->bank.nrow) {
    pBankHead(fp,&hbid->bank);
    for (i = 0; i < hbid->bank.nrow; i++)
      pHBID(fp,&hbid->hbid[i]);
    fprintf(fp,"\n");
  }
}



/* Print hbla bank -------------------------------------------------*/
void pHBLA(FILE *fp, hbla_t *hbla)
{
  fprintf(fp,"trk_pln: %3hd  pos:(x: %5.3f  y: %5.3f  z: %5.3f)  dir:(x: %5.3f  y: %5.3f  z: %5.3f)  tlen: %5.3f  dc1: %3hd  stat: %3hd  wire: %3hd  dtime: %5.3f  alpha: %5.3f  wlen: %5.3f  sgdoca: %5.3f  fitdoca: %5.3f  \n", hbla->trk_pln, hbla->pos.x, hbla->pos.y, hbla->pos.z, hbla->dir.x, hbla->dir.y, hbla->dir.z, hbla->tlen, hbla->dc1, hbla->stat, hbla->wire, hbla->dtime, hbla->alpha, hbla->wlen, hbla->sgdoca, hbla->fitdoca);
}

void printHBLAbank(FILE *fp, clasHBLA_t *hbla) {
  int i;

  if(hbla->bank.nrow) {
    pBankHead(fp,&hbla->bank);
    for (i = 0; i < hbla->bank.nrow; i++)
      pHBLA(fp,&hbla->hbla[i]);
    fprintf(fp,"\n");
  }
}

/* Print hbtb bank -------------------------------------------------*/
void pHBTB(FILE *fp, hbtb_t *hbtb)
{
  fprintf(fp,"trk: %3hd  sly: %3hd  icl: %3hd  isg: %3hd  id1: %3hd  id2: %3hd  id3: %3hd  id4: %3hd  id5: %3hd  id6: %3hd  \n", hbtb->trk, hbtb->sly, hbtb->icl, hbtb->isg, hbtb->id1, hbtb->id2, hbtb->id3, hbtb->id4, hbtb->id5, hbtb->id6);
}

void printHBTBbank(FILE *fp, clasHBTB_t *hbtb) {
  int i;

  if(hbtb->bank.nrow) {
    pBankHead(fp,&hbtb->bank);
    for (i = 0; i < hbtb->bank.nrow; i++)
      pHBTB(fp,&hbtb->hbtb[i]);
    fprintf(fp,"\n");
  }
}

/* Print hbtr bank -------------------------------------------------*/
void pHBTR(FILE *fp, hbtr_t *hbtr)
{
  fprintf(fp,"vert > x: %5.3f  y: %5.3f  z: %5.3f  p > px: %5.3f  py: %5.3f  pz: %5.3f  q: %5.3f  chi2: %5.3f  itr_sec: %3hd  \n", hbtr->vert.x, hbtr->vert.y, hbtr->vert.z, hbtr->p.x, hbtr->p.y, hbtr->p.z, hbtr->q, hbtr->chi2, hbtr->itr_sec);
}

void printHBTRbank(FILE *fp, clasHBTR_t *hbtr) {
  int i;

  if(hbtr->bank.nrow) {
    pBankHead(fp,&hbtr->bank);
    for (i = 0; i < hbtr->bank.nrow; i++)
      pHBTR(fp,&hbtr->hbtr[i]);
    fprintf(fp,"\n");
  }
}

/* Print hcal bank -------------------------------------------------*/
void pHCAL(FILE *fp, hcal_t *hcal)
{
  fprintf(fp,"VERSION: %3hd  RUN_LOW: %3hd  RUN_HIGH: %3hd  CATIME: %3hd  ROCCA: %3hd  \n", hcal->version, hcal->run_low, hcal->run_high, hcal->catime, hcal->rocca);
}

void printHCALbank(FILE *fp, clasHCAL_t *hcal) {
  int i;

  if(hcal->bank.nrow) {
    pBankHead(fp,&hcal->bank);
    for (i = 0; i < hcal->bank.nrow; i++)
      pHCAL(fp,&hcal->hcal[i]);
    fprintf(fp,"\n");
  }
}

/* Print hdpl bank -------------------------------------------------*/
void pHDPL(FILE *fp, hdpl_t *hdpl)
{
  fprintf(fp,"trk_pln: %3hd\n pos: ", hdpl->trk_pln);
  v3print(fp, hdpl->pos);
  fprintf(fp, " dir: ");
  v3print(fp, hdpl->dir);
  fprintf(fp, " tlen: %f\n", hdpl->tlen);
}

void printHDPLbank(FILE *fp, clasHDPL_t *hdpl) {
  int i;

  if(hdpl->bank.nrow) {
    pBankHead(fp,&hdpl->bank);
    for (i = 0; i < hdpl->bank.nrow; i++)
      pHDPL(fp,&hdpl->hdpl[i]);
    fprintf(fp,"\n");
  }
}

/* Print hevt bank -------------------------------------------------*/
void pHEVT(FILE *fp, hevt_t *hevt)
{
  fprintf(fp,"ESTATUS: %6d  NRUN: %6d  NEVENT: %6d  TYPE: %6d  NPGP: %6d  TRGPRS: %6d  FC: %5.3f  FCG: %5.3f  TG: %5.3f  STT: %5.3f  RF1: %5.3f  RF2: %5.3f  CON1: %5.3f  CON2: %5.3f  CON3: %5.3f  PTIME: %6d  \n", hevt->estatus, hevt->nrun, hevt->nevent, hevt->type, hevt->npgp, hevt->trgprs, hevt->fc, hevt->fcg, hevt->tg, hevt->stt, hevt->rf1, hevt->rf2, hevt->con1, hevt->con2, hevt->con3, hevt->ptime);
}

void printHEVTbank(FILE *fp, clasHEVT_t *hevt) {
  int i;

  if(hevt->bank.nrow) {
    pBankHead(fp,&hevt->bank);
    for (i = 0; i < hevt->bank.nrow; i++)
      pHEVT(fp,&hevt->hevt[i]);
    fprintf(fp,"\n");
  }
}

/* Print hlb bank -------------------------------------------------*/
void pHLB(FILE *fp, hlb_t *hlb)
{
  fprintf(fp,"C22AI: %d  C22AX: %d  C22AY: %d  C24AI: %d  C24AX: %d  C24AY: %d  CH01I: %d  CH01X: %d  CH01Y: %d \n", hlb->b1, hlb->b2, hlb->b3, hlb->b5, hlb->b6, hlb->b7, hlb->b9,hlb->b10, hlb->b11);
}


void printHLBbank(FILE *fp, clasHLB_t *hlb) {
  int i;

  if(hlb->bank.nrow) {
    pBankHead(fp,&hlb->bank);
    for (i = 0; i < hlb->bank.nrow; i++)
      pHLB(fp,&hlb->hlb[i]);
    fprintf(fp,"\n");
  }
}


/* Print hls bank -------------------------------------------------*/
void pHLS(FILE *fp, hls_t *hls)
{
   fprintf(fp,"10MHz clock: %d  OTR-1: %d  OTR-2: %d  SLM: %d  lvl-1 trigger rate: %d  L.R Moller coincidences: %d  L.R Moller accidentals: %d  F-CUP: %d  pmt-1: %d pmt-2: %d  pmt-3: %d  pmt-4: %d  reserve: %d  reserve: %d  Helicity states counter: %d HLS banks counter: %d\n", hls->s1, hls->s2, hls->s3, hls->s4, hls->s5, hls->s6, hls->s7, hls->s8, hls->s9, hls->s10, hls->s11, hls->s12, hls->s13, hls->s14, hls->s15, hls->s16);
}

void printHLSbank(FILE *fp, clasHLS_t *hls) {
  int i;

  if(hls->bank.nrow) {
    pBankHead(fp,&hls->bank);
    for (i = 0; i < hls->bank.nrow; i++)
      pHLS(fp,&hls->hls[i]);
    fprintf(fp,"\n");
  }
}

/* Print ic bank -------------------------------------------------*/
void pIC(FILE *fp, ic_t *ic)
{
fprintf(fp,"ID: %3hd  TDC: %3hd  ADC: %3hd  \n", ic->id, ic->tdc,ic->adc);
}

void printICbank(FILE *fp, clasIC_t *ic) {
  int i;
  if(ic->bank.nrow) {
     pBankHead(fp,&ic->bank);
    for (i = 0; i < ic->bank.nrow; i++)pIC(fp,&ic->ic[i]);
    fprintf(fp,"\n");
  }
}

/* Print ic0 bank -------------------------------------------------*/
void pIC0(FILE *fp, ic0_t *ic0)
{
fprintf(fp,"ID: %3hd  TDC: %3hd  \n", ic0->id, ic0->tdc);
} 

void printIC0bank(FILE *fp, clasIC0_t *ic0) {
  int i;
  if(ic0->bank.nrow) {
     pBankHead(fp,&ic0->bank);
    for (i = 0; i < ic0->bank.nrow; i++)pIC0(fp,&ic0->ic0[i]);
    fprintf(fp,"\n");
  }
}

/* Print ichb bank -------------------------------------------------*/
void pICHB(FILE *fp, ichb_t *ichb)
{
fprintf(fp,"Eclust: %3f  Tclust: %3f  Xclust: %3f  Yclust: %3f  Zclust: %3f\n", ichb->eclust, ichb->tclust,ichb->xclust,ichb->yclust,ichb->zclust);
fprintf(fp,"Eclmax: %3f  T_next: %3f  Xclmax: %3f  Yclmax: %3f  Zclmax: %3f\n", ichb->eclmax, ichb->t_next,ichb->xclmax,ichb->yclmax,ichb->zclmax);
fprintf(fp,"Ncryst: %d   Xerr  : %f   Yerr  : %f   Zerr  : %f \n",ichb->ncryst,ichb->res1,ichb->res2,ichb->res3);
}

void printICHBbank(FILE *fp, clasICHB_t *ichb) {
  int i;
  if(ichb->bank.nrow) {
          pBankHead(fp,&ichb->bank);
          for (i = 0; i < ichb->bank.nrow; i++)pICHB(fp,&ichb->ichb[i]);
          fprintf(fp,"\n");
  }
}

/* Print icpb bank -------------------------------------------------*/
void pICPB(FILE *fp, icpb_t *icpb)
{
fprintf(fp,"Eclust: %3f  Tclust: %3f T_next: %3f  Xclust: %3f  Yclust: %3f  Zclust: %3f\n", icpb->etot, icpb->time,icpb->t_next,icpb->x,icpb->y,icpb->z);
fprintf(fp,"M2  : %f   M3  : %f   status  : %d \n",icpb->m2_hit,icpb->m3_hit,icpb->status);
} 

void printICPBbank(FILE *fp, clasICPB_t *icpb) {
  int i;
  if(icpb->bank.nrow) {
          pBankHead(fp,&icpb->bank);
          for (i = 0; i < icpb->bank.nrow; i++)pICPB(fp,&icpb->icpb[i]);
          fprintf(fp,"\n");
  }
}


/* Print lasr bank -------------------------------------------------*/
void pLASR(FILE *fp, lasr_t *lasr)
{
  fprintf(fp,"ID: %6d  stat1: %6d  stat2: %6d  stat3: %6d  \n", lasr->id, lasr->stat1, lasr->stat2, lasr->stat3);
}

void printLASRbank(FILE *fp, clasLASR_t *lasr) {
  int i;

  if(lasr->bank.nrow) {
    pBankHead(fp,&lasr->bank);
    for (i = 0; i < lasr->bank.nrow; i++)
      pLASR(fp,&lasr->lasr[i]);
    fprintf(fp,"\n");
  }
}

/* Print lcdi bank -------------------------------------------------*/
void pLCDI(FILE *fp, lcdi_t *lcdi)
{
  fprintf(fp,"crate: %6d  slot: %6d  mask: %6d  threshold: %6d  \n", lcdi->crate, lcdi->slot, lcdi->mask, lcdi->threshold);
}

void printLCDIbank(FILE *fp, clasLCDI_t *lcdi) {
  int i;

  if(lcdi->bank.nrow) {
    pBankHead(fp,&lcdi->bank);
    for (i = 0; i < lcdi->bank.nrow; i++)
      pLCDI(fp,&lcdi->lcdi[i]);
    fprintf(fp,"\n");
  }
}

/* Print lcpb bank -------------------------------------------------*/
void pLCPB(FILE *fp, lcpb_t *lcpb)
{
  fprintf(fp,"ScHt: %6d  Etot: %5.3f  Time: %5.3f  Path: %5.3f  X: %5.3f  Y: %5.3f  Z: %5.3f  Chi2LC: %5.3f  Status: %6d  Ein: %5.3f  \n", lcpb->scht, lcpb->etot, lcpb->time, lcpb->path, lcpb->x, lcpb->y, lcpb->z, lcpb->chi2lc, lcpb->status, lcpb->ein); }

void printLCPBbank(FILE *fp, clasLCPB_t *lcpb) {
  int i;

  if(lcpb->bank.nrow) {
    pBankHead(fp,&lcpb->bank);
    for (i = 0; i < lcpb->bank.nrow; i++)
      pLCPB(fp,&lcpb->lcpb[i]);
    fprintf(fp,"\n");
  }
}


/* Print lowq bank -------------------------------------------------*/
void pLOWQ(FILE *fp, lowq_t *lowq)
{
  fprintf(fp,"pid: %3hd\n", lowq->pid);
  fprintf(fp,"   vert-> x: %5.3f  y: %5.3f  z: %5.3f\n", lowq->vert.x, lowq->vert.y, lowq->vert.z);
  fprintf(fp,"   p-> E: %5.3f  px: %5.3f  py: %5.3f  pz: %5.3f\n", lowq->p.t, lowq->p.space.x, lowq->p.space.y, lowq->p.space.z);
  fprintf(fp,"   qpid: %5.3f  qtrk: %5.3f  flags: %3hd  \n", lowq->qpid, lowq->qtrk, lowq->flags);
}

void printLOWQbank(FILE *fp, clasLOWQ_t *lowq) {
  int i;

  if(lowq->bank.nrow) {
    pBankHead(fp,&lowq->bank);
    for (i = 0; i < lowq->bank.nrow; i++)
      pLOWQ(fp,&lowq->lowq[i]);
    fprintf(fp,"\n");
  }
}


/* Print lsrt bank -------------------------------------------------*/
void pLSRT(FILE *fp, lsrt_t *lsrt)
{
  fprintf(fp,"ID: %6d  TDC: %6d \n", lsrt->id, lsrt->tdc);
}

void printLSRTbank(FILE *fp, clasLSRT_t *lsrt) {
  int i;

  if(lsrt->bank.nrow) {
    pBankHead(fp,&lsrt->bank);
    for (i = 0; i < lsrt->bank.nrow; i++)
      pLSRT(fp,&lsrt->lsrt[i]);
    fprintf(fp,"\n");
  }
}


/* Print mcev bank -------------------------------------------------*/
void pMCEV(FILE *fp, mcev_t *mcev)
{
  fprintf(fp,"I1: %3hd  i2: %3hd  \n", mcev->i1, mcev->i2);
}

void printMCEVbank(FILE *fp, clasMCEV_t *mcev) {
  int i;

  if(mcev->bank.nrow) {
    pBankHead(fp,&mcev->bank);
    for (i = 0; i < mcev->bank.nrow; i++)
      pMCEV(fp,&mcev->mcev[i]);
    fprintf(fp,"\n");
  }
}

/* Print mchd bank -------------------------------------------------*/
void pMCHD(FILE *fp, mchd_t *mchd)
{
  fprintf(fp,
	  "\nRun:\t\t%d\nEvent:\t\t%d\nType:\t\t%d\nReacType:\t%d\nWeight:\t\t%e\nW:\t\t%f\nQ2:\t\t%f\n"
	  "Photon:\t\t( %8g, ( %8g, %8g, %8g ) )\nTarget:\t\t( %8g, ( %8g, %8g, %8g ) )\n"
	  "TIME:\t\t%s\n",mchd->nrun,
          mchd->nevent,mchd->type,mchd->reactype,
	  mchd->weight,mchd->w,mchd->q2,
	  mchd->e_phot,mchd->px_phot,mchd->py_phot,mchd->pz_phot,
	  mchd->e_targ,mchd->px_targ,mchd->py_targ,mchd->pz_targ,
	  ctime((const long *) &(mchd->time)));
}

void printMCHDbank(FILE *fp, clasMCHD_t *mchd) {
  int i;

  if(mchd->bank.nrow) {
    pBankHead(fp,&mchd->bank);
    for (i = 0; i < mchd->bank.nrow; i++)
      pMCHD(fp,&mchd->mchd[i]);
    fprintf(fp,"\n");
  }
}

/* Print mctk bank -------------------------------------------------*/
void pMCTK(FILE *fp, mctk_t *mctk)
{
  fprintf(fp,"cx: %5.3f  cy: %5.3f  cz: %5.3f  pmom: %5.3f  mass: %5.3f  charge: %5.3f  id: %3hd  flag: %3hd  beg_vtx: %3hd  end_vtx: %3hd  parent: %3hd  \n", mctk->cx, mctk->cy, mctk->cz, mctk->pmom, mctk->mass, mctk->charge, mctk->id, mctk->flag, mctk->beg_vtx, mctk->end_vtx, mctk->parent);
}

void printMCTKbank(FILE *fp, clasMCTK_t *mctk) {
  int i;

  if(mctk->bank.nrow) {
    pBankHead(fp,&mctk->bank);
    for (i = 0; i < mctk->bank.nrow; i++)
      pMCTK(fp,&mctk->mctk[i]);
    fprintf(fp,"\n");
  }
}

/* Print mcvx bank -------------------------------------------------*/
void pMCVX(FILE *fp, mcvx_t *mcvx)
{
  fprintf(fp,"x: %5.3f  y: %5.3f  z: %5.3f  tof: %5.3f  flag: %3hd  \n", mcvx->x, mcvx->y, mcvx->z, mcvx->tof, mcvx->flag);
}

void printMCVXbank(FILE *fp, clasMCVX_t *mcvx) {
  int i;

  if(mcvx->bank.nrow) {
    pBankHead(fp,&mcvx->bank);
    for (i = 0; i < mcvx->bank.nrow; i++)
      pMCVX(fp,&mcvx->mcvx[i]);
    fprintf(fp,"\n");
  }
}

/* special Print mtrk bank -------------------------------------------------*/
void plnk_t(FILE *fp, link_t *lnk){
  fprintf(fp, "segm: %d, clust: %d; ", lnk->segm, lnk->clust);
} 

void pMTRK(FILE *fp, mtrk_t *mtrk){
  int i;
  fprintf(fp, "sect: %d > ", mtrk->sect);
  for(i=0; i<6; i++) plnk_t(fp, &(mtrk->data[i]));
  fprintf(fp, "\n");
}

void printMTRKbank(FILE *fp, clasMTRK_t *mtrk) {
  int i;

  if(mtrk->bank.nrow) {
    pBankHead(fp,&mtrk->bank);
    for (i = 0; i < mtrk->bank.nrow; i++)
      pMTRK(fp,&mtrk->mtrk[i]);
    fprintf(fp,"\n");
  }
}

/* Print part bank -------------------------------------------------*/
void pPART(FILE *fp, part_t *part)
{
  fprintf(fp,"pid: %3hd\n", part->pid);
  fprintf(fp,"   vert-> x: %5.3f  y: %5.3f  z: %5.3f\n", part->vert.x, part->vert.y, part->vert.z);
  fprintf(fp,"   p-> E: %5.3f  px: %5.3f  py: %5.3f  pz: %5.3f\n", part->p.t, part->p.space.x, part->p.space.y, part->p.space.z);
  fprintf(fp,"   q: %5.3f  trkid: %3hd  qpid: %5.3f  qtrk: %5.3g  flags: %3hd  \n", part->q, part->trkid, part->qpid, part->qtrk, part->flags);
}

void printPARTbank(FILE *fp, clasPART_t *part) {
  int i;

  if(part->bank.nrow) {
    pBankHead(fp,&part->bank);
    for (i = 0; i < part->bank.nrow; i++)
      pPART(fp,&part->part[i]);
    fprintf(fp,"\n");
  }
}

/* Print pco bank -------------------------------------------------*/
void pPCO(FILE *fp, pco_t *pco)
{
  fprintf(fp,"TIME: %5.3f  ELT: %5.3f  ERB: %5.3f  ELB: %5.3f  ERT: %5.3f  EMAIN: %5.3f  EVETO: %5.3f  TID: %3hd  \n", pco->time, pco->elt, pco->erb, pco->elb, pco->ert, pco->emain, pco->eveto, pco->tid);
}

void printPCObank(FILE *fp, clasPCO_t *pco) {
  int i;

  if(pco->bank.nrow) {
    pBankHead(fp,&pco->bank);
    for (i = 0; i < pco->bank.nrow; i++)
      pPCO(fp,&pco->pco[i]);
    fprintf(fp,"\n");
  }
}

/* Print pso bank -------------------------------------------------*/
void pPSO(FILE *fp, pso_t *pso)
{
  fprintf(fp,"ID: %3hd  TIME: %5.3f  ENER: %5.3f  TID: %3hd  \n", pso->id, pso->time, pso->ener, pso->tid);
}

void printPSObank(FILE *fp, clasPSO_t *pso) {
  int i;

  if(pso->bank.nrow) {
    pBankHead(fp,&pso->bank);
    for (i = 0; i < pso->bank.nrow; i++)
      pPSO(fp,&pso->pso[i]);
    fprintf(fp,"\n");
  }
}

/* Print ptdb bank -------------------------------------------------*/
void pPTDB(FILE *fp, ptdb_t *ptdb)
{
  fprintf(fp,"PBeam: %6d  PB_DATE: %6d  TTYPE: %6d  B_TARG: %6d  PTarg: %6d  PT_time: %6d  HE_Level: %6d  EIOF: %6d  TTempH: %6d  TTempC: %6d  AnealT: %6d  \n", ptdb->pbeam, ptdb->pb_date, ptdb->ttype, ptdb->b_targ, ptdb->ptarg, ptdb->pt_time, ptdb->he_level, ptdb->eiof, ptdb->ttemph, ptdb->ttempc, ptdb->anealt);
}

void printPTDBbank(FILE *fp, clasPTDB_t *ptdb) {
  int i;

  if(ptdb->bank.nrow) {
    pBankHead(fp,&ptdb->bank);
    for (i = 0; i < ptdb->bank.nrow; i++)
      pPTDB(fp,&ptdb->ptdb[i]);
    fprintf(fp,"\n");
  }
}


/* Print rc26 bank -------------------------------------------------*/
void pRC26(FILE *fp, rc26_t *rc26)
{
  fprintf(fp,"Trigger Pattern: %x  Gated Scaler: %d  Ungated Scaler: %d  \n", rc26->trigger_pattern, rc26->gated_scaler, rc26->ungated_scaler);
}

void printRC26bank(FILE *fp, clasRC26_t *rc26) {
  int i;

  if(rc26->bank.nrow) {
    pBankHead(fp,&rc26->bank);
    for (i = 0; i < rc26->bank.nrow; i++)
      pRC26(fp,&rc26->rc26[i]);
    fprintf(fp,"\n");
  }
}


/* Print rf bank -------------------------------------------------*/
void pRF(FILE *fp, rf_t *rf)
{
  fprintf(fp,"RF: %5.3f  RF1: %5.3f  RF2: %5.3f  \n", rf->rf, rf->rf1, rf->rf2);
}

void printRFbank(FILE *fp, clasRF_t *rf) {
  int i;

  if(rf->bank.nrow) {
    pBankHead(fp,&rf->bank);
    for (i = 0; i < rf->bank.nrow; i++)
      pRF(fp,&rf->rf[i]);
    fprintf(fp,"\n");
  }
}


/* Print rft bank -------------------------------------------------*/
void pRFT(FILE *fp, rft_t *rft)
{
  fprintf(fp,"ID: %6d  TDC: %6d \n", rft->id, rft->tdc);
}

void printRFTbank(FILE *fp, clasRFT_t *rft) {
  int i;

  if(rft->bank.nrow) {
    pBankHead(fp,&rft->bank);
    for (i = 0; i < rft->bank.nrow; i++)
      pRFT(fp,&rft->rft[i]);
    fprintf(fp,"\n");
  }
}


/* Print rglk bank -------------------------------------------------*/
void pRGLK(FILE *fp, rglk_t *rglk)
{
  fprintf(fp,"IREGION: %6d  X: %5.3f  Y: %5.3f  Z: %5.3f  THETA0: %5.3f  PHI0: %5.3f  RTHETA: %5.3f  RPHI: %5.3f  CHI2: %5.3f  STATUS: %6d  \n", rglk->iregion, rglk->x, rglk->y, rglk->z, rglk->theta0, rglk->phi0, rglk->rtheta, rglk->rphi, rglk->chi2, rglk->status);
}

void printRGLKbank(FILE *fp, clasRGLK_t *rglk) {
  int i;

  if(rglk->bank.nrow) {
    pBankHead(fp,&rglk->bank);
    for (i = 0; i < rglk->bank.nrow; i++)
      pRGLK(fp,&rglk->rglk[i]);
    fprintf(fp,"\n");
  }
}

/* Print rtsl bank -------------------------------------------------*/
void pRTSL(FILE *fp, rtsl_t *rtsl)
{
  fprintf(fp,"RAWT1: %d  RAWT2: %d  RAWT3: %d  RAWT4: %d  RAWT5: %d  RAWT6: %d  RAWT7: %d  RAWT8: %d  RAWT9: %d  RAWT10: %d  RAWT11: %d  RAWT12: %d  RAWT13: %d  RAWT14: %d  RAWT15: %d  RAWT16: %d  RAWT17: %d  RAWT18: %d  RAWT19: %d  RAWT20: %d  RAWT21: %d  RAWT22: %d  RAWT23: %d  RAWT24: %d  RAWT25: %d  RAWT26: %d  RAWT27: %d  RAWT28: %d  RAWT29: %d  RAWT30: %d  RAWT31: %d  RAWT32: %d  RAWT33: %d  RAWT34: %d  RAWT35: %d  RAWT36: %d  RAWT37: %d  RAWT38: %d  RAWT39: %d  RAWT40: %d  RAWT41: %d  RAWT42: %d  RAWT43: %d  RAWT44: %d  RAWT45: %d  RAWT46: %d  RAWT47: %d  RAWT48: %d  RAWT49: %d  RAWT50: %d  RAWT51: %d  RAWT52: %d  RAWT53: %d  RAWT54: %d  RAWT55: %d  RAWT56: %d  RAWT57: %d  RAWT58: %d  RAWT59: %d  RAWT60: %d  RAWT61: %d  RAWT62: %d  RAWT63: %d  RAWT64: %d  \n", rtsl->rawt1, rtsl->rawt2, rtsl->rawt3, rtsl->rawt4, rtsl->rawt5, rtsl->rawt6, rtsl->rawt7, rtsl->rawt8, rtsl->rawt9, rtsl->rawt10, rtsl->rawt11, rtsl->rawt12, rtsl->rawt13, rtsl->rawt14, rtsl->rawt15, rtsl->rawt16, rtsl->rawt17, rtsl->rawt18, rtsl->rawt19, rtsl->rawt20, rtsl->rawt21, rtsl->rawt22, rtsl->rawt23, rtsl->rawt24, rtsl->rawt25, rtsl->rawt26, rtsl->rawt27, rtsl->rawt28, rtsl->rawt29, rtsl->rawt30, rtsl->rawt31, rtsl->rawt32, rtsl->rawt33, rtsl->rawt34, rtsl->rawt35, rtsl->rawt36, rtsl->rawt37, rtsl->rawt38, rtsl->rawt39, rtsl->rawt40, rtsl->rawt41, rtsl->rawt42, rtsl->rawt43, rtsl->rawt44, rtsl->rawt45, rtsl->rawt46, rtsl->rawt47, rtsl->rawt48, rtsl->rawt49, rtsl->rawt50, rtsl->rawt51, rtsl->rawt52, rtsl->rawt53, rtsl->rawt54, rtsl->rawt55, rtsl->rawt56, rtsl->rawt57, rtsl->rawt58, rtsl->rawt59, rtsl->rawt60, rtsl->rawt61, rtsl->rawt62, rtsl->rawt63, rtsl->rawt64);
}

void printRTSLbank(FILE *fp, clasRTSL_t *rtsl) {
  int i;

  if(rtsl->bank.nrow) {
    pBankHead(fp,&rtsl->bank);
    for (i = 0; i < rtsl->bank.nrow; i++)
      pRTSL(fp,&rtsl->rtsl[i]);
    fprintf(fp,"\n");
  }
}

/* print runc bank -------------------------------------------------*/
void pRUNC(FILE *fp, runc_t *runc){
  switch(runc->beam.type.val.i[0]){
  case 0:
    fprintf(fp, "electron beam\n");
    break;
  case 1:
    fprintf(fp, "photon beam\n");
    break;
  default:
    fprintf(fp, "unknown beam type\n");
    break;
  }
  fprintf(fp, "beam:\n\ttype: %i\t4-vector: %1.3f %1.3f %1.3f %1.3f\n", 
	  runc->beam.type.val.i[0], 
	  runc->beam.p.t, runc->beam.p.space.x, runc->beam.p.space.y, runc->beam.p.space.z);
  fprintf(fp, "magnets:\n\ttorus: %4.1f A\tminitorus: %4.1f A\ttagger: %4.1f A\n",
	  runc->torus.val.f[0], runc->minitorus.val.f[0], runc->tagger.val.f[0]);
  fprintf(fp, "target: \n\t4-vector: %1.3f %1.3f %1.3f %1.3f\n", 
	  runc->target.t, runc->target.space.x, runc->target.space.y, runc->target.space.z);
}

/* Print sc bank -------------------------------------------------*/
void pSC(FILE *fp, sc_t *sc) 
{ 
  fprintf(fp,"ID: %3hd  TDCL: %3hd  ADCL: %3hd  TDCR: %3hd  ADCR: %3hd  \n", sc->id, sc->tdcl, 
	  sc->adcl, sc->tdcr, sc->adcr); 
}

void printSCbank(FILE *fp, clasSC_t *sc) { 
  int i; 

  if(sc->bank.nrow) { 
    pBankHead(fp,&sc->bank); 
    for (i = 0; i < sc->bank.nrow; i++) 
      pSC(fp,&sc->sc[i]); 
    fprintf(fp,"\n"); 
  }
}

/* Print sc1 bank -------------------------------------------------*/
void pSC1(FILE *fp, sc1_t *sc1)
{
  fprintf(fp,"ID: %6d  time_l: %5.3f  energy_l: %5.3f  time_r: %5.3f  energy_r: %5.3f  dtime_l: %5.3f  denergy_l: %5.3f  dtime_r: %5.3f  denergy_r: %5.3f  \n", sc1->id, sc1->time_l, sc1->energy_l, sc1->time_r, sc1->energy_r, sc1->dtime_l, sc1->denergy_l, sc1->dtime_r, sc1->denergy_r);
}

void printSC1bank(FILE *fp, clasSC1_t *sc1) {
  int i;

  if(sc1->bank.nrow) {
    pBankHead(fp,&sc1->bank);
    for (i = 0; i < sc1->bank.nrow; i++)
      pSC1(fp,&sc1->sc1[i]);
    fprintf(fp,"\n");
  }
}

/* Print scc bank -------------------------------------------------*/
void pSCC(FILE *fp, scc_t *scc)
{
  fprintf(fp,"id: %3hd  date: %3hd  version: %3hd  status: %3hd  TD0L: %5.3f  TD0Lu: %5.3f  TD0R: %5.3f  TD0Ru: %5.3f  TD1L: %5.3f  TD1Lu: %5.3f  TD1R: %5.3f  TD1Ru: %5.3f  TD2L: %5.3f  TD2Lu: %5.3f  TD2R: %5.3f  TD2Ru: %5.3f  TW0L: %5.3f  TW0Lu: %5.3f  TW0R: %5.3f  TW0Ru: %5.3f  TW1L: %5.3f  TW1Lu: %5.3f  TW1R: %5.3f  TW1Ru: %5.3f  TW2L: %5.3f  TW2Lu: %5.3f  TW2R: %5.3f  TW2Ru: %5.3f  ADPL: %5.3f  ADPLu: %5.3f  ADPR: %5.3f  ADPRu: %5.3f  M0L: %5.3f  M0Lu: %5.3f  M0R: %5.3f  M0Ru: %5.3f  VEFL: %5.3f  VEFLu: %5.3f  VEFR: %5.3f  VEFRu: %5.3f  ATNL: %5.3f  ATNLu: %5.3f  ATNR: %5.3f  ATNRu: %5.3f  TSIG0L: %5.3f  TSIG0R: %5.3f  TSIG1L: %5.3f  TSIG1R: %5.3f  \n", scc->id, scc->date, scc->version, scc->status, scc->td0l, scc->td0lu, scc->td0r, scc->td0ru, scc->td1l, scc->td1lu, scc->td1r, scc->td1ru, scc->td2l, scc->td2lu, scc->td2r, scc->td2ru, scc->tw0l, scc->tw0lu, scc->tw0r, scc->tw0ru, scc->tw1l, scc->tw1lu, scc->tw1r, scc->tw1ru, scc->tw2l, scc->tw2lu, scc->tw2r, scc->tw2ru, scc->adpl, scc->adplu, scc->adpr, scc->adpru, scc->m0l, scc->m0lu, scc->m0r, scc->m0ru, scc->vefl, scc->veflu, scc->vefr, scc->vefru, scc->atnl, scc->atnlu, scc->atnr, scc->atnru, scc->tsig0l, scc->tsig0r, scc->tsig1l, scc->tsig1r);
}

void printSCCbank(FILE *fp, clasSCC_t *scc) {
  int i;

  if(scc->bank.nrow) {
    pBankHead(fp,&scc->bank);
    for (i = 0; i < scc->bank.nrow; i++)
      pSCC(fp,&scc->scc[i]);
    fprintf(fp,"\n");
  }
}

/* Print scg bank -------------------------------------------------*/
void pSCG(FILE *fp, scg_t *scg)
{
  fprintf(fp,"id: %3hd  panel: %3hd  Xccw: %5.3f  Yccw: %5.3f  Zccw: %5.3f  Xcw: %5.3f  Ycw: %5.3f  Zcw: %5.3f  WIDTH: %5.3f  THICK: %5.3f  DELTA: %5.3f  \n", scg->id, scg->panel, scg->xccw, scg->yccw, scg->zccw, scg->xcw, scg->ycw, scg->zcw, scg->width, scg->thick, scg->delta);
}

void printSCGbank(FILE *fp, clasSCG_t *scg) {
  int i;

  if(scg->bank.nrow) {
    pBankHead(fp,&scg->bank);
    for (i = 0; i < scg->bank.nrow; i++)
      pSCG(fp,&scg->scg[i]);
    fprintf(fp,"\n");
  }
}

/* Print sch bank -------------------------------------------------*/
void pSCH(FILE *fp, sch_t *sch)
{
  fprintf(fp,"x: %5.3f  y: %5.3f  z: %5.3f  cx: %5.3f  cy: %5.3f  cz: %5.3f  pmom: %5.3f  track: %3hd  id: %3hd  \n", sch->x, sch->y, sch->z, sch->cx, sch->cy, sch->cz, sch->pmom, sch->track, sch->id);
}

void printSCHbank(FILE *fp, clasSCH_t *sch) {
  int i;

  if(sch->bank.nrow) {
    pBankHead(fp,&sch->bank);
    for (i = 0; i < sch->bank.nrow; i++)
      pSCH(fp,&sch->sch[i]);
    fprintf(fp,"\n");
  }
}

/* Print scmd bank -------------------------------------------------*/
void pSCMD(FILE *fp, scmd_t *scmd)
{
  fprintf(fp,"ID: %3hd  STATUS: %3hd  X_NORM: %5.3f  Y_NORM: %5.3f  D_NORM: %5.3f  X_PANEL: %5.3f  Y_PANEL: %5.3f  B_PAD: %5.3f  E_PAD: %5.3f  TOF: %5.3f  EDP: %5.3f  P_TIME: %5.3f  P_ATLN: %5.3f  \n", scmd->id, scmd->status, scmd->x_norm, scmd->y_norm, scmd->d_norm, scmd->x_panel, scmd->y_panel, scmd->b_pad, scmd->e_pad, scmd->tof, scmd->edp, scmd->p_time, scmd->p_atln);
}

void printSCMDbank(FILE *fp, clasSCMD_t *scmd) {
  int i;

  if(scmd->bank.nrow) {
    pBankHead(fp,&scmd->bank);
    for (i = 0; i < scmd->bank.nrow; i++)
      pSCMD(fp,&scmd->scmd[i]);
    fprintf(fp,"\n");
  }
}

/* Print scmt bank -------------------------------------------------*/
void pSCMT(FILE *fp, scmt_t *scmt)
{
  fprintf(fp,"mean_thr: %6d  \n", scmt->mean_thr);
}

void printSCMTbank(FILE *fp, clasSCMT_t *scmt) {
  int i;

  if(scmt->bank.nrow) {
    pBankHead(fp,&scmt->bank);
    for (i = 0; i < scmt->bank.nrow; i++)
      pSCMT(fp,&scmt->scmt[i]);
    fprintf(fp,"\n");
  }
}

/* Print scmw bank -------------------------------------------------*/
void pSCMW(FILE *fp, scmw_t *scmw)
{
  fprintf(fp,"mean_width: %6d  \n", scmw->mean_width);
}

void printSCMWbank(FILE *fp, clasSCMW_t *scmw) {
  int i;

  if(scmw->bank.nrow) {
    pBankHead(fp,&scmw->bank);
    for (i = 0; i < scmw->bank.nrow; i++)
      pSCMW(fp,&scmw->scmw[i]);
    fprintf(fp,"\n");
  }
}

/* Print scdi bank -------------------------------------------------*/
void pSCDI(FILE *fp, scdi_t *scdi)
{
  fprintf(fp,"crate: %6d  slot: %6d  mask: %6d  threshold: %6d  width: %6d  \n", scdi->crate, scdi->slot, scdi->mask, scdi->threshold, scdi->width);
}

void printSCDIbank(FILE *fp, clasSCDI_t *scdi) {
  int i;

  if(scdi->bank.nrow) {
    pBankHead(fp,&scdi->bank);
    for (i = 0; i < scdi->bank.nrow; i++)
      pSCDI(fp,&scdi->scdi[i]);
    fprintf(fp,"\n");
  }
}

/* Print scp bank -------------------------------------------------*/
void pSCP(FILE *fp, scp_t *scp)
{
  fprintf(fp,"N1x: %5.3f  N1y: %5.3f  N1z: %5.3f  R1n: %5.3f  N2x: %5.3f  N2y: %5.3f  N2z: %5.3f  R2n: %5.3f  N3x: %5.3f  N3y: %5.3f  N3z: %5.3f  R3n: %5.3f  N4x: %5.3f  N4y: %5.3f  N4z: %5.3f  R4n: %5.3f  \n", scp->n1x, scp->n1y, scp->n1z, scp->r1n, scp->n2x, scp->n2y, scp->n2z, scp->r2n, scp->n3x, scp->n3y, scp->n3z, scp->r3n, scp->n4x, scp->n4y, scp->n4z, scp->r4n);
}

void printSCPbank(FILE *fp, clasSCP_t *scp) {
  int i;

  if(scp->bank.nrow) {
    pBankHead(fp,&scp->bank);
    for (i = 0; i < scp->bank.nrow; i++)
      pSCP(fp,&scp->scp[i]);
    fprintf(fp,"\n");
  }
}

/* Print scpb bank -------------------------------------------------*/
void pSCPB(FILE *fp, scpb_t *scpb)
{
  fprintf(fp,"ScPdHt: %3d  Edep: %5.3f  Time: %5.3f  Path: %5.3f  Chi2SC: %5.3f  Status: %3d  \n", scpb->scpdht, scpb->edep, scpb->time, scpb->path, scpb->chi2sc, scpb->status);
}

void printSCPBbank(FILE *fp, clasSCPB_t *scpb) {
  int i;

  if(scpb->bank.nrow) {
    pBankHead(fp,&scpb->bank);
    for (i = 0; i < scpb->bank.nrow; i++)
      pSCPB(fp,&scpb->scpb[i]);
    fprintf(fp,"\n");
  }
}

/* Print scpe bank -------------------------------------------------*/
void pSCPE(FILE *fp, scpe_t *scpe)
{
  fprintf(fp,"ID: %6d  mean_left: %6d  sigma_left: %5.3f  mean_right: %6d  sigma_right: %5.3f  \n", scpe->id, scpe->mean_left, scpe->sigma_left, scpe->mean_right, scpe->sigma_right);
}

void printSCPEbank(FILE *fp, clasSCPE_t *scpe) {
  int i;

  if(scpe->bank.nrow) {
    pBankHead(fp,&scpe->bank);
    for (i = 0; i < scpe->bank.nrow; i++)
      pSCPE(fp,&scpe->scpe[i]);
    fprintf(fp,"\n");
  }
}

/* Print scr bank -------------------------------------------------*/
void pSCR(FILE *fp, scr_t *scr)
{
  fprintf(fp,"id: %6d  energy: %5.3f  time: %5.3f pos >", scr->id, scr->energy, scr->time);
  v3print(fp, scr->pos);
  fprintf(fp, " err > ");
  v3print(fp, scr->err);
  fprintf(fp, " status: %6d  denergy: %5.3f  dtime: %5.3f\n", scr->status, scr->denergy, scr->dtime);
}

void printSCRbank(FILE *fp, clasSCR_t *scr) {
  int i;

  if(scr->bank.nrow) {
    pBankHead(fp,&scr->bank);
    for (i = 0; i < scr->bank.nrow; i++)
      pSCR(fp,&scr->scr[i]);
    fprintf(fp,"\n");
  }
}

/* Print scrc bank -------------------------------------------------*/
void pSCRC(FILE *fp, scrc_t *scrc)
{
  fprintf(fp,"id: %6d  energy: %5.3f  time: %5.3f pos >", scrc->id, scrc->energy, scrc->time);
  v3print(fp, scrc->pos);
  fprintf(fp, " err > ");
  v3print(fp, scrc->err);
  fprintf(fp, " status: %6d  denergy: %5.3f  dtime: %5.3f\n", scrc->status, scrc->denergy, scrc->dtime);
}

void printSCRCbank(FILE *fp, clasSCRC_t *scrc) {
  int i;

  if(scrc->bank.nrow) {
    pBankHead(fp,&scrc->bank);
    for (i = 0; i < scrc->bank.nrow; i++)
      pSCRC(fp,&scrc->scrc[i]);
    fprintf(fp,"\n");
  }
}

/* Print sct bank -------------------------------------------------*/
void pSCT(FILE *fp, sct_t *sct)
{
  fprintf(fp,"ID: %6d  TDC: %6d \n", sct->id, sct->tdc);
}

void printSCTbank(FILE *fp, clasSCT_t *sct) {
  int i;

  if(sct->bank.nrow) {
    pBankHead(fp,&sct->bank);
    for (i = 0; i < sct->bank.nrow; i++)
      pSCT(fp,&sct->sct[i]);
    fprintf(fp,"\n");
  }
}

/* Print sgmp bank -------------------------------------------------*/
void pwire_t(FILE *fp, wire_t *wire){
  fprintf(fp, "wireid: %d, time: %d; ", wire->wireid, wire->time);
}

void pSGMP(FILE *fp, sgmp_t *sgm)
{
  int i;
  fprintf(fp,"is: %d > ", sgm->is);
  for(i=0; i<6; i++) pwire_t(fp, &(sgm->data[i]));
  fprintf(fp,"\n");
}

void printSGMPbank(FILE *fp, clasSGMP_t *sgmp) {
  int i;

  if(sgmp->bank.nrow) {
    pBankHead(fp,&sgmp->bank);
    for (i = 0; i < sgmp->bank.nrow; i++)
      pSGMP(fp,&sgmp->sgmp[i]);
    fprintf(fp,"\n");
  }
}

/* Print spar bank -------------------------------------------------*/
void pSPAR(FILE *fp, spar_t *spar)
{
  fprintf(fp,"slot: %6d  channel: %6d  spar: %6d  pedmean: %6d  \n", spar->slot, spar->channel, spar->spar, spar->pedmean);
}

void printSPARbank(FILE *fp, clasSPAR_t *spar) {
  int i;

  if(spar->bank.nrow) {
    pBankHead(fp,&spar->bank);
    for (i = 0; i < spar->bank.nrow; i++)
      pSPAR(fp,&spar->spar[i]);
    fprintf(fp,"\n");
  }
}

/* Print st bank -------------------------------------------------*/
void pST(FILE *fp, st_t *st)
{
  fprintf(fp,"ID: %6d  TDC: %6d  ADC: %6d  \n", st->id, st->tdc, st->adc);
}

void printSTbank(FILE *fp, clasST_t *st) {
  int i;

  if(st->bank.nrow) {
    pBankHead(fp,&st->bank);
    for (i = 0; i < st->bank.nrow; i++)
      pST(fp,&st->st[i]);
    fprintf(fp,"\n");
  }
}

/* Print stn0 bank -------------------------------------------------*/
void pSTN0(FILE *fp, stn0_t *stn0)
{
  fprintf(fp,"ID: %6d  TDC: %6d \n", stn0->id, stn0->tdc);
}

void printSTN0bank(FILE *fp, clasSTN0_t *stn0) {
  int i;

  if(stn0->bank.nrow) {
    pBankHead(fp,&stn0->bank);
    for (i = 0; i < stn0->bank.nrow; i++)
      pSTN0(fp,&stn0->stn0[i]);
    fprintf(fp,"\n");
  }
}

/* Print stn1 bank -------------------------------------------------*/
void pSTN1(FILE *fp, stn1_t *stn1)
{
  fprintf(fp,"ID: %6d  ADC: %6d  \n", stn1->id, stn1->adc);
}

void printSTN1bank(FILE *fp, clasSTN1_t *stn1) {
  int i;

  if(stn1->bank.nrow) {
    pBankHead(fp,&stn1->bank);
    for (i = 0; i < stn1->bank.nrow; i++)
      pSTN1(fp,&stn1->stn1[i]);
    fprintf(fp,"\n");
  }
}

/* Print st1 bank -------------------------------------------------*/
void pST1(FILE *fp, st1_t *st1)
{
  fprintf(fp,"ID: %6d  status: %6d  TIME_1: %5.3f  ADC_1: %5.3f  TIME_2: %5.3f  ADC_2: %5.3f  \n", st1->id, st1->status, st1->time_1, st1->adc_1, st1->time_2, st1->adc_2);
}

void printST1bank(FILE *fp, clasST1_t *st1) {
  int i;

  if(st1->bank.nrow) {
    pBankHead(fp,&st1->bank);
    for (i = 0; i < st1->bank.nrow; i++)
      pST1(fp,&st1->st1[i]);
    fprintf(fp,"\n");
  }
}

/* Print stpe bank -------------------------------------------------*/
void pSTPE(FILE *fp, stpe_t *stpe)
{
  fprintf(fp,"ID: %6d  mean: %6d  sigma: %5.3f  \n", stpe->id, stpe->mean, stpe->sigma);
}

void printSTPEbank(FILE *fp, clasSTPE_t *stpe) {
  int i;

  if(stpe->bank.nrow) {
    pBankHead(fp,&stpe->bank);
    for (i = 0; i < stpe->bank.nrow; i++)
      pSTPE(fp,&stpe->stpe[i]);
    fprintf(fp,"\n");
  }
}

/* Print sth bank -------------------------------------------------*/
void pSTH(FILE *fp, sth_t *sth)
{
  fprintf(fp,"pos:(x: %5.3f  y: %5.3f  z: %5.3f)  dir:(x: %5.3f  y: %5.3f  z: %5.3f)  pmom: %5.3f  track: %3hd  id: %3hd  tof: %5.3f  \n", sth->pos.x, sth->pos.y, sth->pos.z, sth->dir.x, sth->dir.y, sth->dir.z, sth->pmom, sth->track, sth->id, sth->tof);
}

void printSTHbank(FILE *fp, clasSTH_t *sth) {
  int i;

  if(sth->bank.nrow) {
    pBankHead(fp,&sth->bank);
    for (i = 0; i < sth->bank.nrow; i++)
      pSTH(fp,&sth->sth[i]);
    fprintf(fp,"\n");
  }
}

/* Print stpb bank -------------------------------------------------*/
void pSTPB(FILE *fp, stpb_t *stpb)
{
  fprintf(fp,"SThid: %3d  Time: %5.3f  Path: %5.3f  charge: %3d  Status: %3d  \n", stpb->sthid, stpb->time, stpb->path, stpb->charge, stpb->status);
}

void printSTPBbank(FILE *fp, clasSTPB_t *stpb) {
  int i;

  if(stpb->bank.nrow) {
    pBankHead(fp,&stpb->bank);
    for (i = 0; i < stpb->bank.nrow; i++)
      pSTPB(fp,&stpb->stpb[i]);
    fprintf(fp,"\n");
  }
}
/* Print str bank -------------------------------------------------*/
void pSTR(FILE *fp, str_t *str)
{
  fprintf(fp,"ID: %6d  Trk_no: %6d  ST_TIME: %5.3f  ST_L: %5.3f  st_pos: %5.3f  status: %6d  \n", str->id, str->trk_no, str->st_time, str->st_l, str->st_pos, str->status);
}

void printSTRbank(FILE *fp, clasSTR_t *str) {
  int i;

  if(str->bank.nrow) {
    pBankHead(fp,&str->bank);
    for (i = 0; i < str->bank.nrow; i++)
      pSTR(fp,&str->str[i]);
    fprintf(fp,"\n");
  }
}

/* Print stre bank -------------------------------------------------*/
void pSTRE(FILE *fp, stre_t *stre)
{
  fprintf(fp,"ID: %6d  Trk_no: %6d  ST_TIME: %5.3f  ST_L: %5.3f  st_pos: %5.3f  st_edep: %8.3f  st_corr: %8.3f  status: %6d  \n", stre->id, stre->trk_no, stre->st_time, stre->st_l, stre->st_pos, stre->st_edep, stre->st_corr, stre->status);
}

void printSTREbank(FILE *fp, clasSTRE_t *stre) {
  int i;

  if(stre->bank.nrow) {
    pBankHead(fp,&stre->bank);
    for (i = 0; i < stre->bank.nrow; i++)
      pSTRE(fp,&stre->stre[i]);
    fprintf(fp,"\n");
  }
}

/* Print s1st bank -------------------------------------------------*/
void pS1ST(FILE *fp, s1st_t *s1st)
{
  fprintf(fp,"latch_bit1_count: %6d  latch_bit2_count: %6d  latch_bit3_count: %6d  latch_bit4_count: %6d  latch_bit5_count: %6d  latch_bit6_count: %6d  latch_bit7_count: %6d  latch_bit8_count: %6d  latch_bit9_count: %6d  latch_bit10_count: %6d  latch_bit11_count: %6d  latch_bit12_count: %6d  event_count: %6d  latch_zero_count: %6d  latch_empty_count: %6d  latch_not_empty_count: %6d  \n", s1st->latch_bit1_count, s1st->latch_bit2_count, s1st->latch_bit3_count, s1st->latch_bit4_count, s1st->latch_bit5_count, s1st->latch_bit6_count, s1st->latch_bit7_count, s1st->latch_bit8_count, s1st->latch_bit9_count, s1st->latch_bit10_count, s1st->latch_bit11_count, s1st->latch_bit12_count, s1st->event_count, s1st->latch_zero_count, s1st->latch_empty_count, s1st->latch_not_empty_count);
}

void printS1STbank(FILE *fp, clasS1ST_t *s1st) {
  int i;

  if(s1st->bank.nrow) {
    pBankHead(fp,&s1st->bank);
    for (i = 0; i < s1st->bank.nrow; i++)
      pS1ST(fp,&s1st->s1st[i]);
    fprintf(fp,"\n");
  }
}

/* Print sync bank -------------------------------------------------*/
void pSYNC(FILE *fp, sync_t *sync)
{
  fprintf(fp,"ID: %6d  TDCL: %6d  ADCL: %6d  \n", sync->id, sync->tdcl, sync->adcl);
}

void printSYNCbank(FILE *fp, clasSYNC_t *sync) {
  int i;

  if(sync->bank.nrow) {
    pBankHead(fp,&sync->bank);
    for (i = 0; i < sync->bank.nrow; i++)
      pSYNC(fp,&sync->sync[i]);
    fprintf(fp,"\n");
  }
}

/* Print taco bank -------------------------------------------------*/
void pTACO(FILE *fp, taco_t *taco)
{
  fprintf(fp,"ID: %3hd  TIME: %5.3f  ELT: %5.3f  ERT: %5.3f  ELB: %5.3f  ERB: %5.3f  ESUM: %5.3f  ESUM2: %5.3f  ESUM3: %5.3f  TID: %3hd  \n", taco->id, taco->time, taco->elt, taco->ert, taco->elb, taco->erb, taco->esum, taco->esum2, taco->esum3, taco->tid);
}

void printTACObank(FILE *fp, clasTACO_t *taco) {
  int i;

  if(taco->bank.nrow) {
    pBankHead(fp,&taco->bank);
    for (i = 0; i < taco->bank.nrow; i++)
      pTACO(fp,&taco->taco[i]);
    fprintf(fp,"\n");
  }
}

/* Print tage bank -------------------------------------------------*/
void pTAGE(FILE *fp, tage_t *tage)
{
  fprintf(fp,"ID: %3hd  TDC: %3hd  \n", tage->id, tage->tdc);
}

void printTAGEbank(FILE *fp, clasTAGE_t *tage) {
  int i;

  if(tage->bank.nrow) {
    pBankHead(fp,&tage->bank);
    for (i = 0; i < tage->bank.nrow; i++)
      pTAGE(fp,&tage->tage[i]);
    fprintf(fp,"\n");
  }
}

/* Print tagi bank -------------------------------------------------*/
void pTAGI(FILE *fp, tagi_t *tagi)
{
  fprintf(fp,"IDT: %3hd  TIMEL: %5.3f  TIMER: %5.3f  IDE: %3hd  TIMEE: %5.3f  TIMEMEAN: %5.3f  TRF: %5.3f  \n", tagi->idt, tagi->timel, tagi->timer, tagi->ide, tagi->timee, tagi->timemean, tagi->trf);
}

void printTAGIbank(FILE *fp, clasTAGI_t *tagi) {
  int i;

  if(tagi->bank.nrow) {
    pBankHead(fp,&tagi->bank);
    for (i = 0; i < tagi->bank.nrow; i++)
      pTAGI(fp,&tagi->tagi[i]);
    fprintf(fp,"\n");
  }
}
/* Print tagm bank -------------------------------------------------*/
void pTAGM(FILE *fp, tagm_t *tagm)
{
  fprintf(fp,"ENERGY: %5.3f  T: %5.3f  E_T: %5.3f  STATUS: %6d  Tid: %6d  Eid: %6d  \n", tagm->energy,tagm->t, tagm->e_t, tagm->status, tagm->tid, tagm->eid);
}

void printTAGMbank(FILE *fp, clasTAGM_t *tagm) {
  int i;

  if(tagm->bank.nrow) {
    pBankHead(fp,&tagm->bank);
    for (i = 0; i < tagm->bank.nrow; i++)
      pTAGM(fp,&tagm->tagm[i]);
    fprintf(fp,"\n");
  }
}

/* Print tagr bank -------------------------------------------------*/
void pTAGR(FILE *fp, tagr_t *tagr)
{
  fprintf(fp,"ERG: %5.3f  TTAG: %5.3f  TPHO: %5.3f  STAT: %6d  T_id: %6d  E_id: %6d  \n", tagr->erg, tagr->ttag, tagr->tpho, tagr->stat, tagr->t_id, tagr->e_id);
}

void printTAGRbank(FILE *fp, clasTAGR_t *tagr) {
  int i;

  if(tagr->bank.nrow) {
    pBankHead(fp,&tagr->bank);
    for (i = 0; i < tagr->bank.nrow; i++)
      pTAGR(fp,&tagr->tagr[i]);
    fprintf(fp,"\n");
  }
}

/* Print tagt bank -------------------------------------------------*/
void pTAGT(FILE *fp, tagt_t *tagt)
{
  fprintf(fp,"ID: %3hd  TDCL: %3hd  TDCR: %3hd  \n", tagt->id, tagt->tdcl, tagt->tdcr);
}

void printTAGTbank(FILE *fp, clasTAGT_t *tagt) {
  int i;

  if(tagt->bank.nrow) {
    pBankHead(fp,&tagt->bank);
    for (i = 0; i < tagt->bank.nrow; i++)
      pTAGT(fp,&tagt->tagt[i]);
    fprintf(fp,"\n");
  }
}

/* Print tber bank -------------------------------------------------*/
void pTBER(FILE *fp, tber_t *tber)
{
  fprintf(fp,"q_over_p: %6.4f  lambda: %6.4f  phi: %6.4f  d0: %6.4f  z0: %6.4f  c11: %g  c12: %g  c13: %g  c14: %g  c15: %g  c22: %g  c23: %g  c24: %g  c25: %g  c33: %g  c34: %g  c35: %g  c44: %g  c45: %g  c55: %g  chi2: %6.4f  layinfo1: %d  layinfo2: %d  \n", tber->q_over_p, tber->lambda, tber->phi, tber->d0, tber->z0, tber->c11, tber->c12, tber->c13, tber->c14, tber->c15, tber->c22, tber->c23, tber->c24, tber->c25, tber->c33, tber->c34, tber->c35, tber->c44, tber->c45, tber->c55, tber->chi2, tber->layinfo1, tber->layinfo2);
}

void printTBERbank(FILE *fp, clasTBER_t *tber) {
  int i;

  if(tber->bank.nrow) {
    pBankHead(fp,&tber->bank);
    for (i = 0; i < tber->bank.nrow; i++)
      pTBER(fp,&tber->tber[i]);
    fprintf(fp,"\n");
  }
}

/* Print tbid bank -------------------------------------------------*/
void pTBID(FILE *fp, tbid_t *tbid)
{
  fprintf(fp,"track: %6d  sec: %6d  beta: %5.3f  vtime: %5.3f  \n"
	  " SC:  stat: %2d\tid: %3d\ttime: %8.3f\tqual: %6.3f\tvtime: %6.3f\tbeta: %5.3f  \n"
	  " CC:  stat: %2d\tid: %3d\ttime: %8.3f\tqual: %6.3f\tvtime: %6.3f\tbeta: %5.3f  \n"
	  " EC:  stat: %2d\tid: %3d\ttime: %8.3f\tqual: %6.3f\tvtime: %6.3f\tbeta: %5.3f  \n"
	  " ST:  stat: %2d\tid: %3d\ttime: %8.3f\tqual: %6.3f\tvtime: %6.3f\tbeta: %5.3f  \n"
	  " LAC: stat: %2d\tid: %3d\ttime: %8.3f\tqual: %6.3f\tvtime: %6.3f\tbeta: %5.3f  \n\n", tbid->track, tbid->sec, tbid->beta, tbid->vtime, tbid->sc_stat, tbid->sc_id, tbid->sc_time, tbid->sc_qual, tbid->sc_vtime, tbid->sc_beta, tbid->cc_stat, tbid->cc_id, tbid->cc_time, tbid->cc_qual, tbid->cc_vtime, tbid->cc_beta, tbid->ec_stat, tbid->ec_id, tbid->ec_time, tbid->ec_qual, tbid->ec_vtime, tbid->ec_beta, tbid->st_stat, tbid->st_id, tbid->st_time, tbid->st_qual, tbid->st_vtime, tbid->st_beta, tbid->lac_stat, tbid->lac_id, tbid->lac_time, tbid->lac_qual, tbid->lac_vtime, tbid->lac_beta);
}

void printTBIDbank(FILE *fp, clasTBID_t *tbid) {
  int i;

  if(tbid->bank.nrow) {
    pBankHead(fp,&tbid->bank);
    for (i = 0; i < tbid->bank.nrow; i++)
      pTBID(fp,&tbid->tbid[i]);
    fprintf(fp,"\n");
  }
}



/* Print tbla bank -------------------------------------------------*/
void pTBLA(FILE *fp, tbla_t *tbla)
{
  fprintf(fp,"trk_pln: %3hd  pos:(x: %5.3f  y: %5.3f  z: %5.3f)  dir:(x: %5.3f  y: %5.3f  z: %5.3f)  tlen: %5.3f  dc1: %3hd  stat: %3hd  wire: %3hd  dtime: %5.3f  alpha: %5.3f  wlen: %5.3f  sgdoca: %5.3f  fitdoca: %5.3f  calcdoca: %5.3f  \n", tbla->trk_pln, tbla->pos.x, tbla->pos.y, tbla->pos.z, tbla->dir.x, tbla->dir.y, tbla->dir.z, tbla->tlen, tbla->dc1, tbla->stat, tbla->wire, tbla->dtime, tbla->alpha, tbla->wlen, tbla->sgdoca, tbla->fitdoca, tbla->calcdoca);
}

void printTBLAbank(FILE *fp, clasTBLA_t *tbla) {
  int i;

  if(tbla->bank.nrow) {
    pBankHead(fp,&tbla->bank);
    for (i = 0; i < tbla->bank.nrow; i++)
      pTBLA(fp,&tbla->tbla[i]);
    fprintf(fp,"\n");
  }
}

/* Print tbtr bank -------------------------------------------------*/
void pTBTR(FILE *fp, tbtr_t *tbtr)
{
  fprintf(fp,"vert > x: %5.3f  y: %5.3f  z: %5.3f  p > px: %5.3f  py: %5.3f  pz: %5.3f  q: %5.3f  chi2: %5.3f  itr_sec: %3hd  itr_hbt: %3hd  \n", tbtr->vert.x, tbtr->vert.y, tbtr->vert.z, tbtr->p.x, tbtr->p.y, tbtr->p.z, tbtr->q, tbtr->chi2, tbtr->itr_sec, tbtr->itr_hbt);
}

void printTBTRbank(FILE *fp, clasTBTR_t *tbtr) {
  int i;

  if(tbtr->bank.nrow) {
    pBankHead(fp,&tbtr->bank);
    for (i = 0; i < tbtr->bank.nrow; i++)
      pTBTR(fp,&tbtr->tbtr[i]);
    fprintf(fp,"\n");
  }
}

/* Print tcsb bank -------------------------------------------------*/
void pTCSB(FILE *fp, tcsb_t *tcsb)
{
  fprintf(fp,"xpos: %5.3f  ypos: %5.3f  zpos: %5.3f  sxpos: %5.3f  sypos: %5.3f  szpos: %5.3f  \n", tcsb->xpos, tcsb->ypos, tcsb->zpos, tcsb->sxpos, tcsb->sypos, tcsb->szpos);
}

void printTCSBbank(FILE *fp, clasTCSB_t *tcsb) {
  int i;

  if(tcsb->bank.nrow) {
    pBankHead(fp,&tcsb->bank);
    for (i = 0; i < tcsb->bank.nrow; i++)
      pTCSB(fp,&tcsb->tcsb[i]);
    fprintf(fp,"\n");
  }
}

/* Print tdpl bank -------------------------------------------------*/
void pTDPL(FILE *fp, tdpl_t *tdpl)
{
  fprintf(fp,"trk_pln: %3d pos: ",tdpl->trk_pln);
  v3print(fp, tdpl->pos);
  fprintf(fp, " dir: ");
  v3print(fp, tdpl->dir);
  fprintf(fp, " tlen: %f\n", tdpl->tlen);
}


void printTDPLbank(FILE *fp, clasTDPL_t *tdpl) {
  int i;

  if(tdpl->bank.nrow) {
    pBankHead(fp,&tdpl->bank);
    for (i = 0; i < tdpl->bank.nrow; i++)
      pTDPL(fp,&tdpl->tdpl[i]);
    fprintf(fp,"\n");
  }
}

/* Print tesc bank -------------------------------------------------*/
void pTESC(FILE *fp, tesc_t *tesc)
{
  fprintf(fp,"value: %5.3f  \n", tesc->value);
}

void printTESCbank(FILE *fp, clasTESC_t *tesc) {
  int i;

  if(tesc->bank.nrow) {
    pBankHead(fp,&tesc->bank);
    for (i = 0; i < tesc->bank.nrow; i++)
      pTESC(fp,&tesc->tesc[i]);
    fprintf(fp,"\n");
  }
}




/* Print tgbi bank -------------------------------------------------*/
void pTGBI(FILE *fp, tgbi_t *tgbi)
{
  fprintf(fp,"latch1: %x  helicity_scaler: %d  interrupt_time: %d  latch2: %x \n", tgbi->latch1, tgbi->helicity_scaler, tgbi->interrupt_time,tgbi->latch2);
}

void printTGBIbank(FILE *fp, clasTGBI_t *tgbi) {
  int i;

  if(tgbi->bank.nrow) {
    pBankHead(fp,&tgbi->bank);
    for (i = 0; i < tgbi->bank.nrow; i++)
      pTGBI(fp,&tgbi->tgbi[i]);
    fprintf(fp,"\n");
  }
}

/* Print tgpb bank -------------------------------------------------*/
void pTGPB(FILE *fp, tgpb_t *tgpb)
{
  fprintf(fp,"pointer: %6d  Time: %5.3f  Energy: %5.3f  dt: %5.3f  \n", tgpb->pointer, tgpb->time, tgpb->energy, tgpb->dt);
}

void printTGPBbank(FILE *fp, clasTGPB_t *tgpb) {
  int i;

  if(tgpb->bank.nrow) {
    pBankHead(fp,&tgpb->bank);
    for (i = 0; i < tgpb->bank.nrow; i++)
      pTGPB(fp,&tgpb->tgpb[i]);
    fprintf(fp,"\n");
  }
}



/* Print tgs bank -------------------------------------------------*/
void pTGS(FILE *fp, tgs_t *tgs)
{
  fprintf(fp,"RAWT1: %d  RAWT2: %d  RAWT3: %d  RAWT4: %d  RAWT5: %d  RAWT6: %d  RAWT7: %d  RAWT8: %d  RAWT9: %d  RAWT10: %d  RAWT11: %d  RAWT12: %d  RAWT13: %d  RAWT14: %d  RAWT15: %d  RAWT16: %d  RAWT17: %d  RAWT18: %d  RAWT19: %d  RAWT20: %d  RAWT21: %d  RAWT22: %d  RAWT23: %d  RAWT24: %d  RAWT25: %d  RAWT26: %d  RAWT27: %d  RAWT28: %d  RAWT29: %d  RAWT30: %d  RAWT31: %d  RAWT32: %d  RAWT33: %d  RAWT34: %d  RAWT35: %d  RAWT36: %d  RAWT37: %d  RAWT38: %d  RAWT39: %d  RAWT40: %d  RAWT41: %d  RAWT42: %d  RAWT43: %d  RAWT44: %d  RAWT45: %d  RAWT46: %d  RAWT47: %d  RAWT48: %d  RAWT49: %d  RAWT50: %d  RAWT51: %d  RAWT52: %d  RAWT53: %d  RAWT54: %d  RAWT55: %d  RAWT56: %d  RAWT57: %d  RAWT58: %d  RAWT59: %d  RAWT60: %d  RAWT61: %d  notused62: %d  notused63: %d  notused64: %d  BNK1T1: %d  BNK1T2: %d  BNK1T3: %d  BNK1T4: %d  BNK1T5: %d  BNK1T6: %d  BNK1T7: %d  BNK1T8: %d  BNK1T9: %d  BNK1T10: %d  BNK1T11: %d  BNK1T12: %d  BNK1T13: %d  BNK1T14: %d  BNK1T15: %d  BNK1T16: %d  BNK1T17: %d  BNK1T18: %d  BNK1T19: %d  BNK1T20: %d  BNK1T21: %d  BNK1T22: %d  BNK1T23: %d  BNK1T24: %d  BNK1T25: %d  BNK1T26: %d  BNK1T27: %d  BNK1T28: %d  BNK1T29: %d  BNK1T30: %d  BNK1T31: %d  BNK1T32: %d  BNK1T33: %d  BNK1T34: %d  BNK1T35: %d  BNK1T36: %d  BNK1T37: %d  BNK1T38: %d  BNK1T39: %d  BNK1T40: %d  BNK1T41: %d  BNK1T42: %d  BNK1T43: %d  BNK1T44: %d  BNK1T45: %d  BNK1T46: %d  BNK1T47: %d  BNK1T48: %d  BNK1T49: %d  BNK1T50: %d  BNK1T51: %d  BNK1T52: %d  BNK1T53: %d  BNK1T54: %d  BNK1T55: %d  BNK1T56: %d  BNK1T57: %d  BNK1T58: %d  BNK1T59: %d  BNK1T60: %d  BNK1T61: %d  notused126: %d  notused127: %d  notused128: %d  BNK2T1: %d  BNK2T2: %d  BNK2T3: %d  BNK2T4: %d  BNK2T5: %d  BNK2T6: %d  BNK2T7: %d  BNK2T8: %d  BNK2T9: %d  BNK2T10: %d  BNK2T11: %d  BNK2T12: %d  BNK2T13: %d  BNK2T14: %d  BNK2T15: %d  BNK2T16: %d  BNK2T17: %d  BNK2T18: %d  BNK2T19: %d  BNK2T20: %d  BNK2T21: %d  BNK2T22: %d  BNK2T23: %d  BNK2T24: %d  BNK2T25: %d  BNK2T26: %d  BNK2T27: %d  BNK2T28: %d  BNK2T29: %d  BNK2T30: %d  BNK2T31: %d  BNK2T32: %d  BNK2T33: %d  BNK2T34: %d  BNK2T35: %d  BNK2T36: %d  BNK2T37: %d  BNK2T38: %d  BNK2T39: %d  BNK2T40: %d  BNK2T41: %d  BNK2T42: %d  BNK2T43: %d  BNK2T44: %d  BNK2T45: %d  BNK2T46: %d  BNK2T47: %d  BNK2T48: %d  BNK2T49: %d  BNK2T50: %d  BNK2T51: %d  BNK2T52: %d  BNK2T53: %d  BNK2T54: %d  BNK2T55: %d  BNK2T56: %d  BNK2T57: %d  BNK2T58: %d  BNK2T59: %d  BNK2T60: %d  BNK2T61: %d  notused190: %d  notused191: %d  notused192: %d  \n", tgs->rawt1, tgs->rawt2, tgs->rawt3, tgs->rawt4, tgs->rawt5, tgs->rawt6, tgs->rawt7, tgs->rawt8, tgs->rawt9, tgs->rawt10, tgs->rawt11, tgs->rawt12, tgs->rawt13, tgs->rawt14, tgs->rawt15, tgs->rawt16, tgs->rawt17, tgs->rawt18, tgs->rawt19, tgs->rawt20, tgs->rawt21, tgs->rawt22, tgs->rawt23, tgs->rawt24, tgs->rawt25, tgs->rawt26, tgs->rawt27, tgs->rawt28, tgs->rawt29, tgs->rawt30, tgs->rawt31, tgs->rawt32, tgs->rawt33, tgs->rawt34, tgs->rawt35, tgs->rawt36, tgs->rawt37, tgs->rawt38, tgs->rawt39, tgs->rawt40, tgs->rawt41, tgs->rawt42, tgs->rawt43, tgs->rawt44, tgs->rawt45, tgs->rawt46, tgs->rawt47, tgs->rawt48, tgs->rawt49, tgs->rawt50, tgs->rawt51, tgs->rawt52, tgs->rawt53, tgs->rawt54, tgs->rawt55, tgs->rawt56, tgs->rawt57, tgs->rawt58, tgs->rawt59, tgs->rawt60, tgs->rawt61, tgs->notused62, tgs->notused63, tgs->notused64, tgs->bnk1t1, tgs->bnk1t2, tgs->bnk1t3, tgs->bnk1t4, tgs->bnk1t5, tgs->bnk1t6, tgs->bnk1t7, tgs->bnk1t8, tgs->bnk1t9, tgs->bnk1t10, tgs->bnk1t11, tgs->bnk1t12, tgs->bnk1t13, tgs->bnk1t14, tgs->bnk1t15, tgs->bnk1t16, tgs->bnk1t17, tgs->bnk1t18, tgs->bnk1t19, tgs->bnk1t20, tgs->bnk1t21, tgs->bnk1t22, tgs->bnk1t23, tgs->bnk1t24, tgs->bnk1t25, tgs->bnk1t26, tgs->bnk1t27, tgs->bnk1t28, tgs->bnk1t29, tgs->bnk1t30, tgs->bnk1t31, tgs->bnk1t32, tgs->bnk1t33, tgs->bnk1t34, tgs->bnk1t35, tgs->bnk1t36, tgs->bnk1t37, tgs->bnk1t38, tgs->bnk1t39, tgs->bnk1t40, tgs->bnk1t41, tgs->bnk1t42, tgs->bnk1t43, tgs->bnk1t44, tgs->bnk1t45, tgs->bnk1t46, tgs->bnk1t47, tgs->bnk1t48, tgs->bnk1t49, tgs->bnk1t50, tgs->bnk1t51, tgs->bnk1t52, tgs->bnk1t53, tgs->bnk1t54, tgs->bnk1t55, tgs->bnk1t56, tgs->bnk1t57, tgs->bnk1t58, tgs->bnk1t59, tgs->bnk1t60, tgs->bnk1t61, tgs->notused126, tgs->notused127, tgs->notused128, tgs->bnk2t1, tgs->bnk2t2, tgs->bnk2t3, tgs->bnk2t4, tgs->bnk2t5, tgs->bnk2t6, tgs->bnk2t7, tgs->bnk2t8, tgs->bnk2t9, tgs->bnk2t10, tgs->bnk2t11, tgs->bnk2t12, tgs->bnk2t13, tgs->bnk2t14, tgs->bnk2t15, tgs->bnk2t16, tgs->bnk2t17, tgs->bnk2t18, tgs->bnk2t19, tgs->bnk2t20, tgs->bnk2t21, tgs->bnk2t22, tgs->bnk2t23, tgs->bnk2t24, tgs->bnk2t25, tgs->bnk2t26, tgs->bnk2t27, tgs->bnk2t28, tgs->bnk2t29, tgs->bnk2t30, tgs->bnk2t31, tgs->bnk2t32, tgs->bnk2t33, tgs->bnk2t34, tgs->bnk2t35, tgs->bnk2t36, tgs->bnk2t37, tgs->bnk2t38, tgs->bnk2t39, tgs->bnk2t40, tgs->bnk2t41, tgs->bnk2t42, tgs->bnk2t43, tgs->bnk2t44, tgs->bnk2t45, tgs->bnk2t46, tgs->bnk2t47, tgs->bnk2t48, tgs->bnk2t49, tgs->bnk2t50, tgs->bnk2t51, tgs->bnk2t52, tgs->bnk2t53, tgs->bnk2t54, tgs->bnk2t55, tgs->bnk2t56, tgs->bnk2t57, tgs->bnk2t58, tgs->bnk2t59, tgs->bnk2t60, tgs->bnk2t61, tgs->notused190, tgs->notused191, tgs->notused192);
}

void printTGSbank(FILE *fp, clasTGS_t *tgs) {
  int i;

  if(tgs->bank.nrow) {
    pBankHead(fp,&tgs->bank);
    for (i = 0; i < tgs->bank.nrow; i++)
      pTGS(fp,&tgs->tgs[i]);
    fprintf(fp,"\n");
  }
}
/* Print tgtl bank -------------------------------------------------*/
void pTGTL(FILE *fp, tgtl_t *tgtl)
{
  fprintf(fp,"ID: %3hd  TDC: %3hd  \n", tgtl->id, tgtl->tdc);
}

void printTGTLbank(FILE *fp, clasTGTL_t *tgtl) {
  int i;

  if(tgtl->bank.nrow) {
    pBankHead(fp,&tgtl->bank);
    for (i = 0; i < tgtl->bank.nrow; i++)
      pTGTL(fp,&tgtl->tgtl[i]);
    fprintf(fp,"\n");
  }
}
/* Print tgtl bank -------------------------------------------------*/
void pTGTR(FILE *fp, tgtr_t *tgtr)
{
  fprintf(fp,"ID: %3hd  TDC: %3hd  \n", tgtr->id, tgtr->tdc);
}

void printTGTRbank(FILE *fp, clasTGTR_t *tgtr) {
  int i;

  if(tgtr->bank.nrow) {
    pBankHead(fp,&tgtr->bank);
    for (i = 0; i < tgtr->bank.nrow; i++)
      pTGTR(fp,&tgtr->tgtr[i]);
    fprintf(fp,"\n");
  }
}

/* Print trgs bank -------------------------------------------------*/
void pTRGS(FILE *fp, trgs_t *trgs)
{
  fprintf(fp,"CLOCK_UG: %6d  FCUP_UG: %6d  Microsec: %6d  random_ug: %6d  MOR_ST: %6d  MOR_PC: %6d  MOR_PS: %6d  MOR_TAC: %6d  MOR: %6d  PC: %6d  PS: %6d  TAC: %6d  ST: %6d  hel_sync: %6d  clock_ug_2: %6d  fcup_ug_2: %6d  CLOCK_G1: %6d  FCUP_G1: %6d  NotUsed19: %6d  random_g1: %6d  MOR_ST_rg: %6d  MOR_PC_rg: %6d  MOR_PS_rg: %6d  MOR_TAC_rg: %6d  MOR_rg: %6d  PC_rg: %6d  PS_rg: %6d  TAC_rg: %6d  ST_rg: %6d  random_g1_2: %6d  clock_g1_2: %6d  fcup_g1_2: %6d  CLOCK_G2: %6d  FCUP_G2: %6d  trig_or_g2: %6d  random_g2: %6d  NotUsed37: %6d  NotUsed38: %6d  NotUsed39: %6d  NotUsed40: %6d  MOR_lg: %6d  NotUsed42: %6d  NotUsed43: %6d  NotUsed44: %6d  NotUsed45: %6d  random_g2_2: %6d  clock_g2_2: %6d  fcup_g2_2: %6d  trig1_ug: %6d  trig2_ug: %6d  trig3_ug: %6d  trig4_ug: %6d  trig5_ug: %6d  trig6_ug: %6d  trig7_ug: %6d  trig8_ug: %6d  trig9_ug: %6d  trig10_ug: %6d  trig11_ug: %6d  trig12_ug: %6d  trig_or_ug: %6d  l1accept: %6d  notused63: %6d  notused64: %6d  l2fail: %6d  l2pass: %6d  l2start: %6d  l2clear: %6d  l2accept: %6d  l3accept: %6d  notused71: %6d  notused72: %6d  l2sec1_g: %6d  l2sec2_g: %6d  l2sec3_g: %6d  l2sec4_g: %6d  l2sec5_g: %6d  l2sec6_g: %6d  l2_or_g: %6d  l2_ok_g: %6d  \n", trgs->clock_ug, trgs->fcup_ug, trgs->microsec, trgs->random_ug, trgs->mor_st, trgs->mor_pc, trgs->mor_ps, trgs->mor_tac, trgs->mor, trgs->pc, trgs->ps, trgs->tac, trgs->st, trgs->hel_sync, trgs->clock_ug_2, trgs->fcup_ug_2, trgs->clock_g1, trgs->fcup_g1, trgs->notused19, trgs->random_g1, trgs->mor_st_rg, trgs->mor_pc_rg, trgs->mor_ps_rg, trgs->mor_tac_rg, trgs->mor_rg, trgs->pc_rg, trgs->ps_rg, trgs->tac_rg, trgs->st_rg, trgs->random_g1_2, trgs->clock_g1_2, trgs->fcup_g1_2, trgs->clock_g2, trgs->fcup_g2, trgs->trig_or_g2, trgs->random_g2, trgs->notused37, trgs->notused38, trgs->notused39, trgs->notused40, trgs->mor_lg, trgs->notused42, trgs->notused43, trgs->notused44, trgs->notused45, trgs->random_g2_2, trgs->clock_g2_2, trgs->fcup_g2_2, trgs->trig1_ug, trgs->trig2_ug, trgs->trig3_ug, trgs->trig4_ug, trgs->trig5_ug, trgs->trig6_ug, trgs->trig7_ug, trgs->trig8_ug, trgs->trig9_ug, trgs->trig10_ug, trgs->trig11_ug, trgs->trig12_ug, trgs->trig_or_ug, trgs->l1accept, trgs->notused63, trgs->notused64, trgs->l2fail, trgs->l2pass, trgs->l2start, trgs->l2clear, trgs->l2accept, trgs->l3accept, trgs->notused71, trgs->notused72, trgs->l2sec1_g, trgs->l2sec2_g, trgs->l2sec3_g, trgs->l2sec4_g, trgs->l2sec5_g, trgs->l2sec6_g, trgs->l2_or_g, trgs->l2_ok_g);
}



void printTRGSbank(FILE *fp, clasTRGS_t *trgs) {
  int i;

  if(trgs->bank.nrow) {
    pBankHead(fp,&trgs->bank);
    for (i = 0; i < trgs->bank.nrow; i++)
      pTRGS(fp,&trgs->trgs[i]);
    fprintf(fp,"\n");
  }
}
 
/* Print trgt bank -------------------------------------------------*/
void pTRGT(FILE *fp, trgt_t *trgt)
{
  fprintf(fp,"ID: %6d  TDC: %6d  \n", trgt->id, trgt->tdc);
}
 
void printTRGTbank(FILE *fp, clasTRGT_t *trgt) {
  int i;
 
  if(trgt->bank.nrow) {
    pBankHead(fp,&trgt->bank);
    for (i = 0; i < trgt->bank.nrow; i++)
      pTRGT(fp,&trgt->trgt[i]);
    fprintf(fp,"\n");
  }
}

/* Print trks bank -------------------------------------------------*/
void pTRKS(FILE *fp, trks_t *trks)
{
  fprintf(fp,"trk_lev: %d  beta: %5.3f  st_time: %5.3f  cc_time: %5.3f  sc_time: %5.3f  ec_time: %5.3f  st_id: %d  cc_id: %d  sc_id: %3hd  ec_id: %3hd  \n", trks->trk_lev, trks->beta, trks->st_time, trks->cc_time, trks->sc_time, trks->ec_time, trks->st_id, trks->cc_id, trks->sc_id, trks->ec_id);
}

void printTRKSbank(FILE *fp, clasTRKS_t *trks) {
  int i;

  if(trks->bank.nrow) {
    pBankHead(fp,&trks->bank);
    for (i = 0; i < trks->bank.nrow; i++)
      pTRKS(fp,&trks->trks[i]);
    fprintf(fp,"\n");
  }
}




/* Print unus bank -------------------------------------------------*/
void pUNUS(FILE *fp, unus_t *unus)
{
  fprintf(fp,"NDCUN: %3hd  IDCUN: %3hd  NSCUN: %3hd  ISCUN: %3hd  NCCUN: %3hd  ICCUN: %3hd  NECUN: %3hd  IECUN: %3hd  \n", unus->ndcun, unus->idcun, unus->nscun, unus->iscun, unus->nccun, unus->iccun, unus->necun, unus->iecun);
}

void printUNUSbank(FILE *fp, clasUNUS_t *unus) {
  int i;

  if(unus->bank.nrow) {
    pBankHead(fp,&unus->bank);
    for (i = 0; i < unus->bank.nrow; i++)
      pUNUS(fp,&unus->unus[i]);
    fprintf(fp,"\n");
  }
}

/* Print vert bank -------------------------------------------------*/
void pVERT(FILE *fp, vert_t *vert)
{
  fprintf(fp,"vertex: %3hd  trk1: %3hd  trk2: %3hd  x: %5.3f  y: %5.3f  z: %5.3f sepd: %5.3f\n", 
	  vert->vertex, vert->trk1, vert->trk2, vert->vert.x, vert->vert.y, vert->vert.z, 
	  vert->sepd);
}

void printVERTbank(FILE *fp, clasVERT_t *vert) {
  int i;

  if(vert->bank.nrow) {
    pBankHead(fp,&vert->bank);
    for (i = 0; i < vert->bank.nrow; i++)
      pVERT(fp,&vert->vert[i]);
    fprintf(fp,"\n");
  }
}

/* Print chi2 bank -------------------------------------------------*/
void pCHI2(FILE *fp, chi2_t *chi2)
{
  fprintf(fp,"chi2: %5.3f  cl: %7.5f  ndf: %d  iterations: %4d  \n", chi2->chi2, chi2->cl, chi2->ndf, chi2->iteration);
}

void printCHI2bank(FILE *fp, clasCHI2_t *chi2) {
  int i;

  if(chi2->bank.nrow) {
    pBankHead(fp,&chi2->bank);
    for (i = 0; i < chi2->bank.nrow; i++)
      pCHI2(fp,&chi2->chi2[i]);
    fprintf(fp,"\n");
  }
}

/* Print ecpc bank -------------------------------------------------*/
void pECPC(FILE *fp, ecpc_t *ecpc)
{
  fprintf(fp,"ecpc: 100*layer+stripID: %d  tdc: %5.3f adc-pedestal:  %5.3f \n", ecpc->id, ecpc->tdc, ecpc->adc);
}

void printECPCbank(FILE *fp, clasECPC_t *ecpc) {
  int i;

  if(ecpc->bank.nrow) {
    pBankHead(fp,&ecpc->bank);
    for (i = 0; i < ecpc->bank.nrow; i++)
      pECPC(fp,&ecpc->ecpc[i]);
    fprintf(fp,"\n");
  }
}

/* Print ect bank -------------------------------------------------*/
void pECT(FILE *fp, ect_t *ect)
{
  fprintf(fp,"ID: %6d  TDC: %6d \n", ect->id, ect->tdc);
}

void printECTbank(FILE *fp, clasECT_t *ect) {
  int i;

  if(ect->bank.nrow) {
    pBankHead(fp,&ect->bank);
    for (i = 0; i < ect->bank.nrow; i++)
      pECT(fp,&ect->ect[i]);
    fprintf(fp,"\n");
  }
}

/* Print kfit bank -------------------------------------------------*/
void pKFIT(FILE *fp, kfit_t *kfit)
{

  fprintf(fp,"chi2 contribution:   %10.7f\n",kfit->chi2piece);
  fprintf(fp,"momentum_f: %5.3f  lambda_f: %5.3f  phi_f: %5.3f  d0_f: %5.3f  z0_f: %5.3f  \n\n", kfit->momenta_f, kfit->lambda_f, kfit->phi_f, kfit->d0_f, kfit->z0_f);
  fprintf(fp,"sigma11: %.4e\n",kfit->sigma11);
  fprintf(fp,"sigma21: %.4e ",kfit->sigma12);
  fprintf(fp,"sigma22: %.4e\n",kfit->sigma22);
  fprintf(fp,"sigma31: %.4e ",kfit->sigma13);
  fprintf(fp,"sigma32: %.4e ",kfit->sigma23);
  fprintf(fp,"sigma33: %.4e\n",kfit->sigma33);
  fprintf(fp,"sigma41: %.4e ",kfit->sigma14);
  fprintf(fp,"sigma42: %.4e ",kfit->sigma24);
  fprintf(fp,"sigma43: %.4e ",kfit->sigma34);
  fprintf(fp,"sigma44: %.4e\n",kfit->sigma44);
  fprintf(fp,"sigma51: %.4e ",kfit->sigma15);
  fprintf(fp,"sigma52: %.4e ",kfit->sigma25);
  fprintf(fp,"sigma53: %.4e ",kfit->sigma35);
  fprintf(fp,"sigma54: %.4e ",kfit->sigma45);
  fprintf(fp,"sigma55: %.4e\n",kfit->sigma55);
  fprintf(fp,"\n");

  fprintf(fp,"cov11: %.4e ",kfit->cov11);
  fprintf(fp,"cov11: %.4e ",kfit->cov12);
  fprintf(fp,"cov13: %.4e ",kfit->cov13);
  fprintf(fp,"cov14: %.4e ",kfit->cov14);
  fprintf(fp,"cov15: %.4e\n",kfit->cov15);
  fprintf(fp,"cov21: %.4e ",kfit->cov21);
  fprintf(fp,"cov22: %.4e ",kfit->cov22);
  fprintf(fp,"cov23: %.4e ",kfit->cov23);
  fprintf(fp,"cov24: %.4e ",kfit->cov24);
  fprintf(fp,"cov25: %.4e\n",kfit->cov25);
  fprintf(fp,"cov31: %.4e ",kfit->cov31);
  fprintf(fp,"cov32: %.4e ",kfit->cov32);
  fprintf(fp,"cov33: %.4e ",kfit->cov33);
  fprintf(fp,"cov34: %.4e ",kfit->cov34);
  fprintf(fp,"cov35: %.4e\n",kfit->cov35);
  fprintf(fp,"cov41: %.4e ",kfit->cov41);
  fprintf(fp,"cov42: %.4e ",kfit->cov42);
  fprintf(fp,"cov43: %.4e ",kfit->cov43);
  fprintf(fp,"cov44: %.4e ",kfit->cov44);
  fprintf(fp,"cov45: %.4e\n",kfit->cov45);
  fprintf(fp,"cov51: %.4e ",kfit->cov51);
  fprintf(fp,"cov52: %.4e ",kfit->cov52);
  fprintf(fp,"cov53: %.4e ",kfit->cov53);
  fprintf(fp,"cov54: %.4e ",kfit->cov54);
  fprintf(fp,"cov55: %.4e\n",kfit->cov55);
  fprintf(fp,"\n\n");
}

void printKFITbank(FILE *fp, clasKFIT_t *kfit) {
  int i;

  if(kfit->bank.nrow) {
    pBankHead(fp,&kfit->bank);
    for (i = 0; i < kfit->bank.nrow; i++)
      pKFIT(fp,&kfit->kfit[i]);
    fprintf(fp,"\n");
  }
  fprintf(fp,"-----------------------------------------\n");
}

/* Print scs bank -------------------------------------------------*/
void pSCS(FILE *fp, scs_t *scs)
{
  fprintf(fp,"SCS1: %6d  SCS2: %6d  SCS3: %6d  SCS4: %6d  SCS5: %6d  SCS6: %6d  SCS7: %6d  SCS8: %6d  SCS9: %6d  SCS10: %6d  SCS11: %6d  SCS12: %6d  SCS13: %6d  SCS14: %6d  SCS15: %6d  SCS16: %6d  SCS17: %6d  SCS18: %6d  SCS19: %6d  SCS20: %6d  SCS21: %6d  SCS22: %6d  SCS23: %6d  SCS24: %6d  SCS25: %6d  SCS26: %6d  SCS27: %6d  SCS28: %6d  SCS29: %6d  SCS30: %6d  SCS31: %6d  SCS32: %6d  SCS33: %6d  SCS34: %6d  SCS35: %6d  SCS36: %6d  SCS37: %6d  SCS38: %6d  SCS39: %6d  SCS40: %6d  SCS41: %6d  SCS42: %6d  SCS43: %6d  SCS44: %6d  SCS45: %6d  SCS46: %6d  SCS47: %6d  SCS48: %6d  SCS49: %6d  SCS50: %6d  SCS51: %6d  SCS52: %6d  SCS53: %6d  SCS54: %6d  SCS55: %6d  SCS56: %6d  SCS57: %6d  SCS58: %6d  SCS59: %6d  SCS60: %6d  SCS61: %6d  SCS62: %6d  SCS63: %6d  SCS64: %6d  SCS65: %6d  SCS66: %6d  SCS67: %6d  SCS68: %6d  SCS69: %6d  SCS70: %6d  SCS71: %6d  SCS72: %6d  SCS73: %6d  SCS74: %6d  SCS75: %6d  SCS76: %6d  SCS77: %6d  SCS78: %6d  SCS79: %6d  SCS80: %6d  SCS81: %6d  SCS82: %6d  SCS83: %6d  SCS84: %6d  SCS85: %6d  SCS86: %6d  SCS87: %6d  SCS88: %6d  SCS89: %6d  SCS90: %6d  SCS91: %6d  SCS92: %6d  SCS93: %6d  SCS94: %6d  SCS95: %6d  SCS96: %6d  SCS97: %6d  SCS98: %6d  SCS99: %6d  SCS100: %6d  SCS101: %6d  SCS102: %6d  SCS103: %6d  SCS104: %6d  SCS105: %6d  SCS106: %6d  SCS107: %6d  SCS108: %6d  SCS109: %6d  SCS110: %6d  SCS111: %6d  SCS112: %6d  SCS113: %6d  SCS114: %6d  SCS115: %6d  SCS116: %6d  SCS117: %6d  SCS118: %6d  SCS119: %6d  SCS120: %6d  SCS121: %6d  SCS122: %6d  SCS123: %6d  SCS124: %6d  SCS125: %6d  SCS126: %6d  SCS127: %6d  SCS128: %6d  SCS129: %6d  SCS130: %6d  SCS131: %6d  SCS132: %6d  SCS133: %6d  SCS134: %6d  SCS135: %6d  SCS136: %6d  SCS137: %6d  SCS138: %6d  SCS139: %6d  SCS140: %6d  SCS141: %6d  SCS142: %6d  SCS143: %6d  SCS144: %6d  SCS145: %6d  SCS146: %6d  SCS147: %6d  SCS148: %6d  SCS149: %6d  SCS150: %6d  SCS151: %6d  SCS152: %6d  SCS153: %6d  SCS154: %6d  SCS155: %6d  SCS156: %6d  SCS157: %6d  SCS158: %6d  SCS159: %6d  SCS160: %6d  SCS161: %6d  SCS162: %6d  SCS163: %6d  SCS164: %6d  SCS165: %6d  SCS166: %6d  SCS167: %6d  SCS168: %6d  SCS169: %6d  SCS170: %6d  SCS171: %6d  SCS172: %6d  SCS173: %6d  SCS174: %6d  SCS175: %6d  SCS176: %6d  SCS177: %6d  SCS178: %6d  SCS179: %6d  SCS180: %6d  SCS181: %6d  SCS182: %6d  SCS183: %6d  SCS184: %6d  SCS185: %6d  SCS186: %6d  SCS187: %6d  SCS188: %6d  SCS189: %6d  SCS190: %6d  SCS191: %6d  SCS192: %6d  \n", scs->scs1, scs->scs2, scs->scs3, scs->scs4, scs->scs5, scs->scs6, scs->scs7, scs->scs8, scs->scs9, scs->scs10, scs->scs11, scs->scs12, scs->scs13, scs->scs14, scs->scs15, scs->scs16, scs->scs17, scs->scs18, scs->scs19, scs->scs20, scs->scs21, scs->scs22, scs->scs23, scs->scs24, scs->scs25, scs->scs26, scs->scs27, scs->scs28, scs->scs29, scs->scs30, scs->scs31, scs->scs32, scs->scs33, scs->scs34, scs->scs35, scs->scs36, scs->scs37, scs->scs38, scs->scs39, scs->scs40, scs->scs41, scs->scs42, scs->scs43, scs->scs44, scs->scs45, scs->scs46, scs->scs47, scs->scs48, scs->scs49, scs->scs50, scs->scs51, scs->scs52, scs->scs53, scs->scs54, scs->scs55, scs->scs56, scs->scs57, scs->scs58, scs->scs59, scs->scs60, scs->scs61, scs->scs62, scs->scs63, scs->scs64, scs->scs65, scs->scs66, scs->scs67, scs->scs68, scs->scs69, scs->scs70, scs->scs71, scs->scs72, scs->scs73, scs->scs74, scs->scs75, scs->scs76, scs->scs77, scs->scs78, scs->scs79, scs->scs80, scs->scs81, scs->scs82, scs->scs83, scs->scs84, scs->scs85, scs->scs86, scs->scs87, scs->scs88, scs->scs89, scs->scs90, scs->scs91, scs->scs92, scs->scs93, scs->scs94, scs->scs95, scs->scs96, scs->scs97, scs->scs98, scs->scs99, scs->scs100, scs->scs101, scs->scs102, scs->scs103, scs->scs104, scs->scs105, scs->scs106, scs->scs107, scs->scs108, scs->scs109, scs->scs110, scs->scs111, scs->scs112, scs->scs113, scs->scs114, scs->scs115, scs->scs116, scs->scs117, scs->scs118, scs->scs119, scs->scs120, scs->scs121, scs->scs122, scs->scs123, scs->scs124, scs->scs125, scs->scs126, scs->scs127, scs->scs128, scs->scs129, scs->scs130, scs->scs131, scs->scs132, scs->scs133, scs->scs134, scs->scs135, scs->scs136, scs->scs137, scs->scs138, scs->scs139, scs->scs140, scs->scs141, scs->scs142, scs->scs143, scs->scs144, scs->scs145, scs->scs146, scs->scs147, scs->scs148, scs->scs149, scs->scs150, scs->scs151, scs->scs152, scs->scs153, scs->scs154, scs->scs155, scs->scs156, scs->scs157, scs->scs158, scs->scs159, scs->scs160, scs->scs161, scs->scs162, scs->scs163, scs->scs164, scs->scs165, scs->scs166, scs->scs167, scs->scs168, scs->scs169, scs->scs170, scs->scs171, scs->scs172, scs->scs173, scs->scs174, scs->scs175, scs->scs176, scs->scs177, scs->scs178, scs->scs179, scs->scs180, scs->scs181, scs->scs182, scs->scs183, scs->scs184, scs->scs185, scs->scs186, scs->scs187, scs->scs188, scs->scs189, scs->scs190, scs->scs191, scs->scs192);
}

void printSCSbank(FILE *fp, clasSCS_t *scs) {
  int i;

  if(scs->bank.nrow) {
    pBankHead(fp,&scs->bank);
    for (i = 0; i < scs->bank.nrow; i++)
      pSCS(fp,&scs->scs[i]);
    fprintf(fp,"\n");
  }
}

/* Print ecs bank -------------------------------------------------*/
void pECS(FILE *fp, ecs_t *ecs)
{
  fprintf(fp,"ECS1: %d  ECS2: %d  ECS3: %d  ECS4: %d  ECS5: %d  ECS6: %d  ECS7: %d  ECS8: %d  ECS9: %d  ECS10: %d  ECS11: %d  ECS12: %d  ECS13: %d  ECS14: %d  ECS15: %d  ECS16: %d  ECS17: %d  ECS18: %d  ECS19: %d  ECS20: %d  ECS21: %d  ECS22: %d  ECS23: %d  ECS24: %d  ECS25: %d  ECS26: %d  ECS27: %d  ECS28: %d  ECS29: %d  ECS30: %d  ECS31: %d  ECS32: %d  ECS33: %d  ECS34: %d  ECS35: %d  ECS36: %d  ECS37: %d  ECS38: %d  ECS39: %d  ECS40: %d  ECS41: %d  ECS42: %d  ECS43: %d  ECS44: %d  ECS45: %d  ECS46: %d  ECS47: %d  ECS48: %d  ECS49: %d  ECS50: %d  ECS51: %d  ECS52: %d  ECS53: %d  ECS54: %d  ECS55: %d  ECS56: %d  ECS57: %d  ECS58: %d  ECS59: %d  ECS60: %d  ECS61: %d  ECS62: %d  ECS63: %d  ECS64: %d  ECS65: %d  ECS66: %d  ECS67: %d  ECS68: %d  ECS69: %d  ECS70: %d  ECS71: %d  ECS72: %d  ECS73: %d  ECS74: %d  ECS75: %d  ECS76: %d  ECS77: %d  ECS78: %d  ECS79: %d  ECS80: %d  ECS81: %d  ECS82: %d  ECS83: %d  ECS84: %d  ECS85: %d  ECS86: %d  ECS87: %d  ECS88: %d  ECS89: %d  ECS90: %d  ECS91: %d  ECS92: %d  ECS93: %d  ECS94: %d  ECS95: %d  ECS96: %d  \n", ecs->ecs1, ecs->ecs2, ecs->ecs3, ecs->ecs4, ecs->ecs5, ecs->ecs6, ecs->ecs7, ecs->ecs8, ecs->ecs9, ecs->ecs10, ecs->ecs11, ecs->ecs12, ecs->ecs13, ecs->ecs14, ecs->ecs15, ecs->ecs16, ecs->ecs17, ecs->ecs18, ecs->ecs19, ecs->ecs20, ecs->ecs21, ecs->ecs22, ecs->ecs23, ecs->ecs24, ecs->ecs25, ecs->ecs26, ecs->ecs27, ecs->ecs28, ecs->ecs29, ecs->ecs30, ecs->ecs31, ecs->ecs32, ecs->ecs33, ecs->ecs34, ecs->ecs35, ecs->ecs36, ecs->ecs37, ecs->ecs38, ecs->ecs39, ecs->ecs40, ecs->ecs41, ecs->ecs42, ecs->ecs43, ecs->ecs44, ecs->ecs45, ecs->ecs46, ecs->ecs47, ecs->ecs48, ecs->ecs49, ecs->ecs50, ecs->ecs51, ecs->ecs52, ecs->ecs53, ecs->ecs54, ecs->ecs55, ecs->ecs56, ecs->ecs57, ecs->ecs58, ecs->ecs59, ecs->ecs60, ecs->ecs61, ecs->ecs62, ecs->ecs63, ecs->ecs64, ecs->ecs65, ecs->ecs66, ecs->ecs67, ecs->ecs68, ecs->ecs69, ecs->ecs70, ecs->ecs71, ecs->ecs72, ecs->ecs73, ecs->ecs74, ecs->ecs75, ecs->ecs76, ecs->ecs77, ecs->ecs78, ecs->ecs79, ecs->ecs80, ecs->ecs81, ecs->ecs82, ecs->ecs83, ecs->ecs84, ecs->ecs85, ecs->ecs86, ecs->ecs87, ecs->ecs88, ecs->ecs89, ecs->ecs90, ecs->ecs91, ecs->ecs92, ecs->ecs93, ecs->ecs94, ecs->ecs95, ecs->ecs96);
}

void printECSbank(FILE *fp, clasECS_t *ecs) {
  int i;

  if(ecs->bank.nrow) {
    pBankHead(fp,&ecs->bank);
    for (i = 0; i < ecs->bank.nrow; i++)
      pECS(fp,&ecs->ecs[i]);
    fprintf(fp,"\n");
  }
}

/* Print ccs bank -------------------------------------------------*/
void pCCS(FILE *fp, ccs_t *ccs)
{
  fprintf(fp,"CCS1: %d  CCS2: %d  CCS3: %d  CCS4: %d  CCS5: %d  CCS6: %d  CCS7: %d  CCS8: %d  CCS9: %d  CCS10: %d  CCS11: %d  CCS12: %d  CCS13: %d  CCS14: %d  CCS15: %d  CCS16: %d  CCS17: %d  CCS18: %d  CCS19: %d  CCS20: %d  CCS21: %d  CCS22: %d  CCS23: %d  CCS24: %d  CCS25: %d  CCS26: %d  CCS27: %d  CCS28: %d  CCS29: %d  CCS30: %d  CCS31: %d  CCS32: %d  CCS33: %d  CCS34: %d  CCS35: %d  CCS36: %d  CCS37: %d  CCS38: %d  CCS39: %d  CCS40: %d  CCS41: %d  CCS42: %d  CCS43: %d  CCS44: %d  CCS45: %d  CCS46: %d  CCS47: %d  CCS48: %d  CCS49: %d  CCS50: %d  CCS51: %d  CCS52: %d  CCS53: %d  CCS54: %d  CCS55: %d  CCS56: %d  CCS57: %d  CCS58: %d  CCS59: %d  CCS60: %d  CCS61: %d  CCS62: %d  CCS63: %d  CCS64: %d  CCS65: %d  CCS66: %d  CCS67: %d  CCS68: %d  CCS69: %d  CCS70: %d  CCS71: %d  CCS72: %d  CCS73: %d  CCS74: %d  CCS75: %d  CCS76: %d  CCS77: %d  CCS78: %d  CCS79: %d  CCS80: %d  CCS81: %d  CCS82: %d  CCS83: %d  CCS84: %d  CCS85: %d  CCS86: %d  CCS87: %d  CCS88: %d  CCS89: %d  CCS90: %d  CCS91: %d  CCS92: %d  CCS93: %d  CCS94: %d  CCS95: %d  CCS96: %d  \n", ccs->ccs1, ccs->ccs2, ccs->ccs3, ccs->ccs4, ccs->ccs5, ccs->ccs6, ccs->ccs7, ccs->ccs8, ccs->ccs9, ccs->ccs10, ccs->ccs11, ccs->ccs12, ccs->ccs13, ccs->ccs14, ccs->ccs15, ccs->ccs16, ccs->ccs17, ccs->ccs18, ccs->ccs19, ccs->ccs20, ccs->ccs21, ccs->ccs22, ccs->ccs23, ccs->ccs24, ccs->ccs25, ccs->ccs26, ccs->ccs27, ccs->ccs28, ccs->ccs29, ccs->ccs30, ccs->ccs31, ccs->ccs32, ccs->ccs33, ccs->ccs34, ccs->ccs35, ccs->ccs36, ccs->ccs37, ccs->ccs38, ccs->ccs39, ccs->ccs40, ccs->ccs41, ccs->ccs42, ccs->ccs43, ccs->ccs44, ccs->ccs45, ccs->ccs46, ccs->ccs47, ccs->ccs48, ccs->ccs49, ccs->ccs50, ccs->ccs51, ccs->ccs52, ccs->ccs53, ccs->ccs54, ccs->ccs55, ccs->ccs56, ccs->ccs57, ccs->ccs58, ccs->ccs59, ccs->ccs60, ccs->ccs61, ccs->ccs62, ccs->ccs63, ccs->ccs64, ccs->ccs65, ccs->ccs66, ccs->ccs67, ccs->ccs68, ccs->ccs69, ccs->ccs70, ccs->ccs71, ccs->ccs72, ccs->ccs73, ccs->ccs74, ccs->ccs75, ccs->ccs76, ccs->ccs77, ccs->ccs78, ccs->ccs79, ccs->ccs80, ccs->ccs81, ccs->ccs82, ccs->ccs83, ccs->ccs84, ccs->ccs85, ccs->ccs86, ccs->ccs87, ccs->ccs88, ccs->ccs89, ccs->ccs90, ccs->ccs91, ccs->ccs92, ccs->ccs93, ccs->ccs94, ccs->ccs95, ccs->ccs96);
}

void printCCSbank(FILE *fp, clasCCS_t *ccs) {
  int i;

  if(ccs->bank.nrow) {
    pBankHead(fp,&ccs->bank);
    for (i = 0; i < ccs->bank.nrow; i++)
      pCCS(fp,&ccs->ccs[i]);
    fprintf(fp,"\n");
  }
}

/* Print sts bank -------------------------------------------------*/
void pSTS(FILE *fp, sts_t *sts)
{
  fprintf(fp,"STS1: %d  STS2: %d  STS3: %d  STS4: %d  STS5: %d  STS6: %d  STS7: %d  STS8: %d  STS9: %d  STS10: %d  STS11: %d  STS12: %d  STS13: %d  STS14: %d  STS15: %d  STS16: %d  \n", sts->sts1, sts->sts2, sts->sts3, sts->sts4, sts->sts5, sts->sts6, sts->sts7, sts->sts8, sts->sts9, sts->sts10, sts->sts11, sts->sts12, sts->sts13, sts->sts14, sts->sts15, sts->sts16);
}

void printSTSbank(FILE *fp, clasSTS_t *sts) {
  int i;

  if(sts->bank.nrow) {
    pBankHead(fp,&sts->bank);
    for (i = 0; i < sts->bank.nrow; i++)
      pSTS(fp,&sts->sts[i]);
    fprintf(fp,"\n");
  }
}

/* Print stsn bank -------------------------------------------------*/
void pSTSN(FILE *fp, stsn_t *stsn)
{
  fprintf(fp,"STSN1: %d  STSN2: %d  STSN3: %d  STSN4: %d  STSN5: %d  STSN6: %d  STSN7: %d  STSN8: %d  STSN9: %d  STSN10: %d  STSN11: %d  STSN12: %d  STSN13: %d  STSN14: %d  STSN15: %d  STSN16: %d  STSN17: %d  STSN18: %d  STSN19: %d  STSN20: %d  STSN21: %d  STSN22: %d  STSN23: %d  STSN24: %d  STOR: %d  STMULT: %d   STANDMOR: %d   MULTANDMOR: %d\n", stsn->stsn1, stsn->stsn2, stsn->stsn3, stsn->stsn4, stsn->stsn5, stsn->stsn6, stsn->stsn7, stsn->stsn8, stsn->stsn9, stsn->stsn10, stsn->stsn11, stsn->stsn12, stsn->stsn13, stsn->stsn14, stsn->stsn15, stsn->stsn16,stsn->stsn17, stsn->stsn18, stsn->stsn19, stsn->stsn20, stsn->stsn21, stsn->stsn22, stsn->stsn23, stsn->stsn24, stsn->stor, stsn->stmult, stsn->standmor, stsn->multandmor );
}

void printSTSNbank(FILE *fp, clasSTSN_t *stsn) {
  int i;

  if(stsn->bank.nrow) {
    pBankHead(fp,&stsn->bank);
    for (i = 0; i < stsn->bank.nrow; i++)
      pSTSN(fp,&stsn->stsn[i]);
    fprintf(fp,"\n");
  }
}


/* Print ms1 bank -------------------------------------------------*/
void pMS1(FILE *fp, ms1_t *ms1)
{
  fprintf(fp,"ID: %6d  ADC: %6d  \n", ms1->id, ms1->adc);
}

void printMS1bank(FILE *fp, clasMS1_t *ms1) {
  int i;

  if(ms1->bank.nrow) {
    pBankHead(fp,&ms1->bank);
    for (i = 0; i < ms1->bank.nrow; i++)
      pMS1(fp,&ms1->ms1[i]);
    fprintf(fp,"\n");
  }
}







/* Print mvrt bank -------------------------------------------------*/
void pMVRT(FILE *fp, mvrt_t *mvrt)
{
  fprintf(fp,"v_id: %6d  ntrk: %6d  x: %5.3f  y: %5.3f  z: %5.3f  chi2: %5.3f  cxx: %5.3f  cxy: %5.3f  cxz: %5.3f  cyy: %5.3f  cyz: %5.3f  czz: %5.3f  stat: %6d  \n", mvrt->v_id, mvrt->ntrk, mvrt->vert.x, mvrt->vert.y, mvrt->vert.z, mvrt->chi2, mvrt->cxx, mvrt->cxy, mvrt->cxz, mvrt->cyy, mvrt->cyz, mvrt->czz, mvrt->stat);
}

void printMVRTbank(FILE *fp, clasMVRT_t *mvrt) {
  int i;

  if(mvrt->bank.nrow) {
    pBankHead(fp,&mvrt->bank);
    for (i = 0; i < mvrt->bank.nrow; i++)
      pMVRT(fp,&mvrt->mvrt[i]);
    fprintf(fp,"\n");
  }
}
/* Print tlv1  bank -------------------------------------------------*/
void pTLV1(FILE *fp, tlv1_t *tlv1)
{
  fprintf(fp,"id: %6x  time:%6d \n", tlv1->id,tlv1->time );
}

void printTLV1bank(FILE *fp, clasTLV1_t *tlv1) {
  int i;

  if(tlv1->bank.nrow) {
    pBankHead(fp,&tlv1->bank);
    for (i = 0; i < tlv1->bank.nrow; i++)
      pTLV1(fp,&tlv1->tlv1[i]);
    fprintf(fp,"\n");
  }
}
/* Print TPC bank -------------------------------------------------*/
void pTPC(FILE *fp, tpc_t *tpc) 
{ 
  fprintf(fp,"ID: %3hd  TDC: %3hd  ADC: %3hd \n", tpc->id, tpc->tdc, 
	  tpc->adc); 
}

void printTPCbank(FILE *fp, clasTPC_t *tpc) { 
  int i; 

  if(tpc->bank.nrow) { 
    pBankHead(fp,&tpc->bank); 
    for (i = 0; i < tpc->bank.nrow; i++) 
      pTPC(fp,&tpc->tpc[i]); 
    fprintf(fp,"\n"); 
  }
}
