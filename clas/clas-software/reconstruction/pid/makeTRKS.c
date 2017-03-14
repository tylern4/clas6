/* -=======================================================- *
$Id: makeTRKS.c,v 1.3 1998/09/23 22:06:19 staylor Exp $
$Author: staylor $
$Revision: 1.3 $
$Date: 1998/09/23 22:06:19 $
* -=======================================================- */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <math.h>
#include <bostypes.h>
#include <kinematics.h>
#include <sc.h>
#include <pid.h>
#include <ec.h>
#include <utility.h>
#include <makebanks.h>
#include <call.h>
#include <pdgutil.h>
#include <particleType.h>

/*---------------------------------------------------------------------------*/
int make_trks_banks_() {
  int sec;
  for(sec = 1; sec <= 6; sec++)
    make_TRKS_bank(sec, getBank(&bcs_, "HBID"));
}

/*---------------------------------------------------------------------------*/
clasTRKS_t *make_TRKS_bank(int sec, clasBID_t *BID) {
  /**
   * input: and sector of TRKS bank, HBID bank
   * output: TRKS bank
   **/
  trks_t *trks;
  clasTRKS_t *TRKS=NULL;
  int nTRKS = 0;
  int i, ret = 0;

  if (BID) {
    trks = (trks_t *) malloc(sizeof(trks_t) * BID->bank.nrow);
    memset(trks, 0, sizeof(trks_t) * BID->bank.nrow);
    /* only make the TRKS bank if there is a chance of making it to TBT */
    if (BID->bid[0].vtime > BAD_VERTEX_TIME) {
      for (i=0 ; i< BID->bank.nrow; i++) {
        if ((BID->bid[i].sec == sec) && BID->bid[i].track) {
          trks[nTRKS].trk_lev = BID->bid[i].track + 200;
          trks[nTRKS].beta = BID->bid[i].beta;
          trks[nTRKS].st_time = BID->bid[i].vtime;

          //printf("%f %f %f (makeTRKS.c HBID bid[%i] vtime, st.time, sc.time)\n", BID->bid[i].vtime, BID->bid[i].st.time, BID->bid[i].sc.time, i);

          trks[nTRKS].cc_time = BID->bid[i].cc.time;
          trks[nTRKS].sc_time = BID->bid[i].sc.time;
          trks[nTRKS].ec_time = BID->bid[i].ec.time;
          trks[nTRKS].st_id = BID->bid[i].st.id;
          trks[nTRKS].cc_id = BID->bid[i].cc.id;
          trks[nTRKS].sc_id = BID->bid[i].sc.id;
          trks[nTRKS].ec_id = BID->bid[i].ec.id;
          nTRKS++;
        }
      }
      if (nTRKS) {
        if (TRKS = makeBank(&bcs_, "TRKS", sec, sizeof(trks_t)/4, nTRKS)) {
          for (i=0; i < nTRKS; i++)
            TRKS->trks[i] = trks[i];
        }
      }
    }
    free(trks);
  }
  return TRKS;
}

/*---------------------------------------------------------------------------*/
clasTRKS_t *make_TRKS_bank_plus(int sec, clasBID_t *BID){
  /* input: TBID bank, and sector of TRKS bank
     output: TRKS bank */
  trks_t *trks;
  clasTRKS_t *TRKS=NULL;
  clasHBTR_t *HBTR = getBank(&bcs_, "HBTR");
  clasTBTR_t *TBTR = getBank(&bcs_, "TBTR");
  int nTRKS = 0;
  int i, hbtr_ind = 0;

  if (BID && TBTR && HBTR){
    trks = (trks_t *) malloc(sizeof(trks_t) * HBTR->bank.nrow);
    memset(trks, 0, sizeof(trks_t) * HBTR->bank.nrow);
    /* only make the TRKS bank if there is a chance of making it to TBT*/
    if(BID->bid[0].vtime > BAD_VERTEX_TIME){
      for(i=0 ; i< BID->bank.nrow; i++){
    if((BID->bid[i].sec == sec) && BID->bid[i].track && BID->bid[i].sc.stat){
      trks[nTRKS].trk_lev = TBTR->tbtr[BID->bid[i].track-1].itr_hbt + 200;
      trks[nTRKS].beta = BID->bid[i].beta;
      trks[nTRKS].st_time = BID->bid[i].vtime;
      trks[nTRKS].cc_time = BID->bid[i].cc.time;
      trks[nTRKS].sc_time = BID->bid[i].sc.time;
      trks[nTRKS].ec_time = BID->bid[i].ec.time;
      trks[nTRKS].st_id = BID->bid[i].st.id;
      trks[nTRKS].cc_id = BID->bid[i].cc.id;
      trks[nTRKS].sc_id = BID->bid[i].sc.id;
      trks[nTRKS].ec_id = BID->bid[i].ec.id;
      nTRKS++;
    }
      }
      for(hbtr_ind=0; hbtr_ind < HBTR->bank.nrow; hbtr_ind++){
    int hbtr_sec = HBTR->hbtr[hbtr_ind].itr_sec / 100;
    if ((HBTR->hbtr[hbtr_ind].q < 0) && (hbtr_sec == sec)){
      if (!match_hbtr2tbid(hbtr_ind+1,BID,TBTR)){
        trks[nTRKS++]=piminus2trks(&(HBTR->hbtr[hbtr_ind]), hbtr_ind+1, BID->bid[0].vtime);
      }
    }
      }

      if (nTRKS){
    if (TRKS = makeBank(&bcs_, "TRKS", sec, sizeof(trks_t)/4, nTRKS)){
      for(i=0; i < nTRKS; i++) TRKS->trks[i] = trks[i];
    }
      }
    }
    free(trks);
  }
  return TRKS;
}

/*---------------------------------------------------------------------------*/
trks_t *get_hbid_trks(hbid_t *hbid){
  clasTRKS_t *TRKS=NULL;
  int i;

  if(TRKS = getGroup(&bcs_, "TRKS", hbid->sec)){
    for(i=0; i<TRKS->bank.nrow; i++){
      if( hbid->track == (TRKS->trks[i].trk_lev)%100) return(&(TRKS->trks[i]));
    }
  }
  return(NULL);
}

/*---------------------------------------------------------------------------*/
trks_t piminus2trks(hbtr_t *hbtr, int trkno, float vtime){
  static int numpiminus=0;
  int hbt_ind;
  float p,beta;
  hdpl_t *hdpl=NULL;
  trks_t pi_trks;

  memset(&pi_trks,0,sizeof(trks_t));
  if (hbtr){
    p=v3mag(hbtr->p);
    beta=p/sqrt(p*p+pow(gmass(PiPlus),2));
    pi_trks.trk_lev = 200+trkno;
    pi_trks.beta = beta;
    pi_trks.st_time = vtime;
  }
  return pi_trks;
}

/*---------------------------------------------------------------------------*/
bid_t *match_hbtr2tbid(int trkno,clasBID_t *BID,clasTBTR_t *TBTR){
  int i, match_ptr;

  for(i=0; i<BID->bank.nrow; i++){
    int tbtr_ind=BID->bid[i].track;
    if (tbtr_ind){
      if (trkno == TBTR->tbtr[tbtr_ind-1].itr_hbt ){
    return(&(BID->bid[i]));
      }
    }
  }
  return NULL;
}
