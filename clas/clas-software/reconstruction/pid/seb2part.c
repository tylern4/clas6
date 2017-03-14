/* -=======================================================- *
$Id: seb2part.c,v 1.9 2003/02/27 08:02:28 pasyuk Exp $
$Author: pasyuk $
$Revision: 1.9 $
$Date: 2003/02/27 08:02:28 $
/* -=======================================================-*/
#define USE(var) static void * use_##var = (void *) &var
  static char crcsid[] = "$Id: seb2part.c,v 1.3"
                         " 1997/04/07 15:27:44 manak Exp $";
USE(crcsid);   /* make sure it is not optimized away */
  static char crname[] = "seb2part.c";
  static char crauth[] = "Joe Manak";
/*----------------------------------------------------------*/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ntypes.h>
#include <bostypes.h>
#include <math.h>
#include <bosddl.h>
#include <kinematics.h>
#include <ec.h>
#include <utility.h>
#include <pdgutil.h>
#include <pid.h>
#include <PartUtil.h>


int makePART_from_EVNT(clasEVNT_t *EVNT, int partno){
  clasPART_t *PART = NULL;
  int i;

  if (PART = makeBank(&bcs_, "PART", partno, sizeof(part_t)/4, EVNT->bank.nrow )){
    for(i=0; i < EVNT->bank.nrow; i++){
      PART->part[i] = evnt2part( &(EVNT->evnt[i]) );
    }
  }
}

/* minimal part bank info */
part_t evnt2part(evnt_t *evnt){
  part_t part;
  vector3_t p = v3mult(evnt->pmom, evnt->dir_cos);
  
  part.vert = evnt->vert;
  part.q = evnt->charge;
  part.p = v3_to_v4(&p , pmass_(&(evnt->id)) );
  part.pid = pdg_to_geant_(&(evnt->id));
    if(evnt->id == 45) part.pid = 45;
  part.trkid = 0; /* no pointer to tbid yet*/
  part.qpid = part.qtrk = 0;
  return(part);

}

void seb2pid( int partno){
  /* for each entry in the evnt bank make a part bank and
     a tbid bank  - sort of seb particle id with bid matching - 
     an attept with a minimal amount of code to convert seb
     pid into a pid bank format*/
  clasEVNT_t *EVNT=getBank(&bcs_, "EVNT");
  clasDCPB_t *DCPB=NULL;
  clasECPB_t *ECPB=NULL;
  clasSCPB_t *SCPB=NULL;
  clasTGPB_t *TGPB=NULL;
  clasBID_t *TBID=NULL;
  clasPART_t *PART=NULL;
  part_t *part=NULL;
  bid_t *tbid=NULL;
  int i;
  int trk_sec[6]={0,0,0,0,0,0}; 


  if (EVNT){
    part = (part_t *)malloc(sizeof(part_t)*EVNT->bank.nrow);
    memset((void *)part,0, sizeof(part_t)*EVNT->bank.nrow); /*set to zero*/
    tbid = (bid_t *)malloc(sizeof(bid_t)*EVNT->bank.nrow);
    memset((void *)tbid,0, sizeof(bid_t)*EVNT->bank.nrow); /*set to zero*/

    if (TGPB=getBank(&bcs_,"TGPB")){ /* For photon running */
      int j;
      float best_diff=ST_TAG_COINCIDENCE_WINDOW;
      for (j=0;j<TGPB->bank.nrow;j++){
	if (fabs(TGPB->tgpb[j].dt)<best_diff){
	  best_diff=fabs(TGPB->tgpb[j].dt);
	  tbid[0].vtime=TGPB->tgpb[j].time;	      
	}
      }
    }
    for (i=0; i < EVNT->bank.nrow; i++){
      part[i] = evnt2part( &(EVNT->evnt[i]) );
      part[i].trkid = i+1;
      tbid[i].beta = EVNT->evnt[i].betta;
      tbid[i].vtime=tbid[0].vtime;
      if (EVNT->evnt[i].dcstat){
	int dcpb_ind = EVNT->evnt[i].dcstat - 1 ;
	clasDCPB_t *DCPB = getBank(&bcs_, "DCPB");
	clasTBTR_t *TBTR = getBank(&bcs_, "TBTR");
	if (DCPB && TBTR) {
	  int tbtr_ind = (DCPB->dcpb[dcpb_ind].sctr)%100 -1;
	  tbtr_t *tbtr = &(TBTR->tbtr[tbtr_ind]);
	  part[i].qtrk = TBTR->tbtr[tbtr_ind].chi2;
	  tbid[i].sec = (DCPB->dcpb[dcpb_ind].sctr)/100;
	  tbid[i].track = (DCPB->dcpb[dcpb_ind].sctr)%100;
	  trk_sec[tbid[i].sec-1]++;
	  if (EVNT->evnt[i].ccstat){
	    clasCC01_t *CC01=getGroup(&bcs_, "CC01", tbid[i].sec);
	    if (CC01) match_to_CC01((hdpl_t*)tbtr2tdpl(tbtr), CC01, &(tbid[i].cc));
	  }
	  if (EVNT->evnt[i].scstat) {
	    clasSCRC_t *SCRC=getGroup(&bcs_,"SCRC", tbid[i].sec);
	    if (SCRC) match_to_tof((hdpl_t*)tbtr2tdpl(tbtr), SCRC,&(tbid[i].sc));
	  }
	  if (EVNT->evnt[i].ststat){
	    clasSTR_t *STR = getBank(&bcs_,"STR ");
	    if (STR) match_to_st((hdpl_t*)tbtr2tdpl(tbtr), STR,&(tbid[i]), trk_sec[tbid[i].sec-1]);
	  }
	  if (EVNT->evnt[i].ecstat){
	    clasECHB_t *ECHB = getBank(&bcs_, "ECHB");
	    if (ECHB) match_to_ec((hdpl_t*)tbtr2tdpl(tbtr), tbid[i].sec, ECHB, &(tbid[i].ec)); 
	  }
	}
      } else {
	/* gamma or neutron */
	if (EVNT->evnt[i].ecstat){
	  clasECPB_t *ECPB = getBank(&bcs_, "ECPB");
	  int ecpb_ind = EVNT->evnt[i].ecstat -1;
	  if (ECPB) {
	    tbid[i].ec = ecpb2match(ECPB->ecpb[ecpb_ind], &(tbid[i].sec));
	  }
	}
      }
    }
    if (TBID = makeBank(&bcs_, "TBID", partno, sizeof(bid_t)/4, EVNT->bank.nrow )){
      for (i=0; i < EVNT->bank.nrow; i++) TBID->bid[i] = tbid[i];
    }
    if (PART =  makeBank(&bcs_, "PART", partno, sizeof(part_t)/4, EVNT->bank.nrow )){
      for (i=0; i < EVNT->bank.nrow; i++) PART->part[i] = part[i];
    }
    free(part);
    free(tbid);
  }

}

match_t ecpb2match(ecpb_t ecpb, int *sec){
  match_t match = {0, 0, 0.0, 0.0, 0.0, 0.0};
  clasECHB_t *ECHB = getBank(&bcs_, "ECHB");

  match.stat = GOOD_MATCH;
  match.id = ecpb.scht%100;
  if (ECHB) {
    int echb_ind = match.id - 1;
    match.time = ECHB->echb[echb_ind].t_hit;
    match.qual = echb_close2tbtr(&ECHB->echb[echb_ind]);
      *sec = echb2sector(&ECHB->echb[echb_ind]);
  }

  return match;
}

/* failed attempt at doing it the right way... seb particle
 id with seb matching*/

match_t ccpb2match(ccpb_t ccpb){
  match_t match;


  match.stat = GOOD_MATCH;
  match.id = ccrc_ind2cc01_ind(ccpb_id2ccrc_id(ccpb.scsght));
  match.time = ccpb.time;
  match.qual = ccpb.chi2cc;

  return(match);
}

int ccrc_ind2cc01_ind(int index) {
}

int ccpb_id2ccrc_id(int scsght){
  return ( (scsght%1000)%100);
}
