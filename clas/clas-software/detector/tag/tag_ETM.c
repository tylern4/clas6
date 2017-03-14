//  RCS ID string.  Don't edit next line! CVS takes care of it.
//  $Id: tag_ETM.c,v 1.10 2008/06/23 01:31:00 fklein Exp $
//
//_begin_inc
//  include files :
//  ---------------------
// System headers first 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
// ---------------------- 
// other headers 
#include <ntypes.h>
#include <bostypes.h>
#include <call.h>
#include "tagtcl.h"
#include <tagcal_cmn.h>
#include <tag_param.h>
#include <tagruncontrol.h>
#include "tag_event.h"
#include <tagtnorm.h>
#include <printBOS.h>
// ---------------------- 
//_end_inc 
//
//  Local pre-defined variables:
//  ---------------------------  
//  RCS information: Do not edit next block! CVS takes care of this variable.
//

//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#define USE(var) static void * use_##var = (void *) &var
// keep this all on one line so CVS can recognize this 
static char crcsid[] =
   "$Id: tag_ETM.c,v 1.10 2008/06/23 01:31:00 fklein Exp $";
USE(crcsid);  // make sure it is not optimized away 

typedef int QSORT_PROC_t(const void *, const void *); 

typedef struct {
  int id;
  float t_mean[2];
  float tl[2];
  float tr[2];
  int status;
} TAGT_bin_t;

TAGT_bin_t Tchan_hit[MAXTC];

typedef struct {
  int id;
  float t[2];
  int status;
} TAGE_bin_t;

TAGE_bin_t Echan_hit[MAXEC];

typedef struct {
  int tid;
  int eid; 
  int Thit;
  int Ehit;
  int status;
} ET_hit_t;

ET_hit_t ET_coincidence[MAXECTC];

// prototypes

int tag_ETM();
int ECh_sort(TAGE_bin_t *hit1, TAGE_bin_t *hit2);
int TCh_sort(TAGT_bin_t *hit1, TAGT_bin_t *hit2);
int ET_sort(ET_hit_t *hit1, ET_hit_t *hit2);

// fortran wraper
int tag_etm_(int *ok){
  *ok = tag_ETM();
  return;
}

int tag_ETM() {
  
  int i = 0;
  int j = 0;  
  int k = 0;
  int usedE[MAXEC];
  int usedT[MAXTC];
  
  int n_tch_hits = 0;
  int n_ech_hits = 0;
  int n_et_coin_hits = 0;
  
  float tmean1;
  float tmean2;
  
  int TCH_min;
  int TCH_max;
  
  int eid;
  int tmin2tchmin;
  
  clasTAGI_t *TAGI = NULL;
  clasTAGR_t *TAGR = NULL;
  
  //bin E-counters first
  //reset everything before we go any further
  
  for (i=0; i<MAXEC; i++) {
    Echan_hit[i].id    = 0;
    Echan_hit[i].t[0]  = 0.;
    Echan_hit[i].t[1]  = 0.;
    Echan_hit[i].status = 0;
    usedE[i] = 0;
  }
  
  //loop over and find overlaps of adjacent counters
  
  k=0;
  if(tagecfiltered_.NBgoodE){
    for (i=0; i<tagecfiltered_.NBgoodE; i++) {
      if (usedE[i]) continue; //make sure we use each hit once
      for (j=i+1; j<tagecfiltered_.NBgoodE; j++) {
	if (usedE[j] | usedE[i]) continue; //make sure we use each hit once
	if(tagecfiltered_.Good_E_id[j] == tagecfiltered_.Good_E_id[i]+1) {
	  if( fabs(tagecfiltered_.Good_E_val[j]-tagecfiltered_.Good_E_val[i]) <= tagparam_.adj_E_coinc/2) {
	    if( k < MAXEC) {
	      Echan_hit[k].id = 2*tagecfiltered_.Good_E_id[i]; //overlap id, even number
	      Echan_hit[k].t[0] =  tagecfiltered_.Good_E_val[i];
	      Echan_hit[k].t[1] =  tagecfiltered_.Good_E_val[j];
	      Echan_hit[k].status |= E_PRESENT; 	    
	      k++; // count them
	      usedE[i]++; // mark both hits as used
	      usedE[j]++;
	    }
	    else {
	      fprintf(stderr, "TAG -W- Too many E-hits!!\n");
	      break;
	    }
	  }
	}
      }
    }
  }

  // now doo another loop to copy non-overlap hits
  if(tagecfiltered_.NBgoodE){
    for (i=0; i<tagecfiltered_.NBgoodE; i++) {
      if (usedE[i]) continue; //make sure we use each hit once
      if( k < MAXEC) {
	Echan_hit[k].id = 2*tagecfiltered_.Good_E_id[i]-1; //non-overlap id, odd number
	Echan_hit[k].t[0] =  tagecfiltered_.Good_E_val[i];
	Echan_hit[k].status |= E_PRESENT;	    
	k++; // count them
	usedE[i]++; // mark both hits as used
      }
      else {
	fprintf(stderr, "TAG -W- Too many E-hits!!\n");
	break;
      }
    }
  }
  
  n_ech_hits = k; // Thish is total number of hits, overlap and non-overlap
  
  // now we will order hits by id an time
  
  if(n_ech_hits>1) qsort(Echan_hit, n_ech_hits, sizeof(TAGE_bin_t), (QSORT_PROC_t*)ECh_sort); 
  
  // done with E binninig
  
  //bin T-counters now
  //reset everything before we go any further
  
  for (i=0; i<MAXTC; i++) {
    Tchan_hit[i].id        = 0;
    Tchan_hit[i].t_mean[0] = 0;
    Tchan_hit[i].t_mean[1] = 0;
    Tchan_hit[i].tl[0]     = 0.;
    Tchan_hit[i].tl[1]     = 0.;
    Tchan_hit[i].tr[0]     = 0.;
    Tchan_hit[i].tr[1]     = 0.;
    Tchan_hit[i].status    = 0;
    usedT[i] = 0;

    tagtnorm_.Tn_ID[i] = 0;
    tagtnorm_.Tn_L_val[0][i] = 0.;
    tagtnorm_.Tn_L_val[1][i] = 0.;
    tagtnorm_.Tn_R_val[0][i] = 0.;
    tagtnorm_.Tn_R_val[1][i] = 0.;
    tagtnorm_.Mean_Tn_val[0][i] = 0.;
    tagtnorm_.Mean_Tn_val[1][i] = 0.;
    tagtnorm_.Diff_Tn_val[0][i] = 0.;
    tagtnorm_.Diff_Tn_val[1][i] = 0.;
    tagtnorm_.Tn_time[0][i] = 0.;
    tagtnorm_.Tn_time[1][i] = 0.;
  }
  tagtnorm_.Nb_Tn = 0;

  //loop over and find overlaps of adjacent counters
  k=0;
  if(tagtcfiltered_.NBgoodT){
    for (i=0; i<tagtcfiltered_.NBgoodT; i++) {
      if (usedT[i]) continue; //make sure we use each hit once
      tmean1 = (tagtcfiltered_.Good_T_L_val[i] + tagtcfiltered_.Good_T_R_val[i])/2;
      for (j=i+1; j<tagtcfiltered_.NBgoodT; j++) {
	if (usedT[j] | usedT[i]) continue; //make sure we use each hit once
	if(tagtcfiltered_.Good_T_id[j] == tagtcfiltered_.Good_T_id[i]+1) {
	  tmean2 = (tagtcfiltered_.Good_T_L_val[j] + tagtcfiltered_.Good_T_R_val[j])/2;
	  if( fabs(tmean1-tmean2) < tagparam_.adj_T_coinc/2) {
	    if ( k<MAXTC) {
	      Tchan_hit[k].id        = 2*tagtcfiltered_.Good_T_id[i]; //overlap id, even number	    
	      Tchan_hit[k].t_mean[0] = tmean1;
	      Tchan_hit[k].t_mean[1] = tmean2;
	      Tchan_hit[k].tl[0]     =  tagtcfiltered_.Good_T_L_val[i];
	      Tchan_hit[k].tl[1]     =  tagtcfiltered_.Good_T_L_val[j];
	      Tchan_hit[k].tr[0]     =  tagtcfiltered_.Good_T_R_val[i];
	      Tchan_hit[k].tr[1]     =  tagtcfiltered_.Good_T_R_val[j];
	      Tchan_hit[k].status   |= TLR_PRESENT;      
	      
	      k++; // count them
	      usedT[i]++; // mark both hits as used
	      usedT[j]++;
	    }
	    else {
	      fprintf (stderr, "\n Number of T-hits exceeds MAXTC=%d \n", MAXTC);
	      break; //exit from the loop
	    }
	  }
	}
      }
    }
  }
  
  // now doo another loop to copy non-overlap hits
  if(tagtcfiltered_.NBgoodT){
    for (i=0; i<tagtcfiltered_.NBgoodT; i++) {
      if (usedT[i]) continue; //make sure we use each hit once
      if ( k<MAXTC) {
	Tchan_hit[k].id = 2*tagtcfiltered_.Good_T_id[i]-1; //non-overlap id, odd number
	tmean1 = (tagtcfiltered_.Good_T_L_val[i] + tagtcfiltered_.Good_T_R_val[i])/2;
	
	Tchan_hit[k].t_mean[0] = tmean1;
	Tchan_hit[k].tl[0]     =  tagtcfiltered_.Good_T_L_val[i];
	Tchan_hit[k].tr[0]     =  tagtcfiltered_.Good_T_R_val[i];
	Tchan_hit[k].status   |= TLR_PRESENT;      
	
	k++; // count them
	usedT[i]++; // mark hit as used
      }
      else {
	fprintf (stderr, "\n Number of T-hits exceeds MAXTC=%d \n", MAXTC);
	break; //exit from the loop
      }
    }
  }
  
  n_tch_hits = k; // Thish is total number of hits, overlap and non-overlap
  
  // now we will order hits by id an time
  
  if(n_tch_hits>1) qsort(Tchan_hit,n_tch_hits,sizeof(TAGT_bin_t),(QSORT_PROC_t*)TCh_sort); 
  
  // now do RF correction and fill common/TagTcorr/
  
  for (i=0; i<n_tch_hits; i++) {
    tagtcorr_.Mean_T_val[0][i] = Tchan_hit[i].t_mean[0] - tagcival_.TagTCi[Tchan_hit[i].id-1];
    tagtcorr_.Diff_T_val[0][i] = (Tchan_hit[i].tr[0] - Tchan_hit[i].tl[0])/2;
    tagtcorr_.T_time[0][i]     =  rf_corr_time_counter(Tchan_hit[i].id,tagtcorr_.Mean_T_val[0][i]);
      //      tagtcorr_.T_time[0][i]     =  rf_corr_time(tagtcorr_.Mean_T_val[0][i]);
    if( Tchan_hit[i].id%2 == 0) { //overlap
      tagtcorr_.Mean_T_val[1][i] = Tchan_hit[i].t_mean[1] - tagcival_.TagTCi[Tchan_hit[i].id-1];
      tagtcorr_.Diff_T_val[1][i] = (Tchan_hit[i].tr[1] - Tchan_hit[i].tl[1])/2;
      tagtcorr_.T_time[1][i]     =  rf_corr_time_counter(Tchan_hit[i].id,tagtcorr_.Mean_T_val[1][i]);
      //      tagtcorr_.T_time[1][i]     =  rf_corr_time(tagtcorr_.Mean_T_val[1][i]);
    }
  }

  //  for (i=0; i< n_tch_hits; i++) fprintf(stderr, "tid=%d tmean1=%f, \n",
  //				Tchan_hit[i].id,tagtcorr_.Mean_T_val[0][i]);
  
  // search for triple T hits
  
  //  for (i=0; i<n_tch_hits; i++) {
  //    for (j=i+1; j<n_tch_hits; j++) {
  //      if ( (Tchan_hit[j].id%2 == 0) && (Tchan_hit[j].id == Tchan_hit[i].id+1) ) {
  //	if( fabs(Tchan_hit[i].t_mean[0]-Tchan_hit[j].t_mean[0]) < tagparam_.adj_T_coinc/2) {
  //	  Tchan_hit[j].status |= TRIPPLE_T; // mark the third hit to be ignored
  //	}
  //    }
  //  }
  // }
  
  // done with T binninig
  
  // deal with T18-T20
  // if we have triple coincidencd T18-T19-T20, we ignore T20,
  // if there is T19-T20, we ignore T20 and reassugne id ftom 38 to 37
  // if we have T18-T20 (tch 35 adn 39), ignore T20

  for (i=0; i<n_tch_hits; i++) {
    if (Tchan_hit[i].id <35) continue;
    if (Tchan_hit[i].id >39) break;
    if (Tchan_hit[i].id == 35 || Tchan_hit[i].id == 36) {
      for (j=i+1; j<n_tch_hits; j++) {
	if (Tchan_hit[j].id >39) break;
	if (Tchan_hit[j].id == 39 && 
	    fabs(Tchan_hit[i].t_mean[0]-Tchan_hit[j].t_mean[0]) 
	    < tagparam_.adj_T_coinc/2) Tchan_hit[j].status = 0; // reset status for T20 
      }
    }
    else if (Tchan_hit[i].id == 38) Tchan_hit[i].id == 37;
  }

  // copy hits to tagtnorm common block for gflux stolen hits
  tagtnorm_.Nb_Tn = n_tch_hits;
  for (i=0; i<n_tch_hits; i++) {
    tagtnorm_.Tn_L_val[0][i] = Tchan_hit[i].tl[0];
    tagtnorm_.Tn_L_val[1][i] = Tchan_hit[i].tl[1];
    tagtnorm_.Tn_R_val[0][i] = Tchan_hit[i].tr[0];
    tagtnorm_.Tn_R_val[1][i] = Tchan_hit[i].tr[1];
    tagtnorm_.Mean_Tn_val[0][i] = tagtcorr_.Mean_T_val[0][i];
    tagtnorm_.Mean_Tn_val[1][i] = tagtcorr_.Mean_T_val[1][i];
    tagtnorm_.Diff_Tn_val[0][i] = tagtcorr_.Diff_T_val[0][i];
    tagtnorm_.Diff_Tn_val[1][i] = tagtcorr_.Diff_T_val[1][i];
    tagtnorm_.Tn_time[0][i] = tagtcorr_.T_time[0][i]; 
    tagtnorm_.Tn_time[1][i] = tagtcorr_.T_time[1][i]; 
  }

  // now we can do ET matching
  
  // reset arrays
  
  for (i=0; i<MAXTC; i++) usedT[i] = 0;
  for (i=0; i<MAXEC; i++) usedE[i] = 0;
  
  for (i=0; i<MAXECTC; i++) {
    ET_coincidence[i].tid    = 0;
    ET_coincidence[i].eid    = 0;
    ET_coincidence[i].Thit   = -1;
    ET_coincidence[i].Ehit   = -1;
    ET_coincidence[i].status = 0;
  }
  
  k = 0;

  //  fprintf(stderr," ET_window %f \n",tagparam_.ET_window);

  for (i=0; i<n_ech_hits; i++) {
    
    eid = (Echan_hit[i].id + 1)/2; // we need to unbin them back eid is eithe first of the overlap or single
    
    if(tagtcl_.Tagger_ETmatch == 1 ) { // do ET geometry matching
      // make sure that window given by tchmin isn't more narrow than window given by tmin 
      tmin2tchmin = (ectcmaps_.tmin[eid-1] -1) * 2 + 1;
      
      //      TCH_min = (tmin2tchmin < ectcmaps_.tchmin[eid-1]) ? tmin2tchmin : ectcmaps_.tchmin[eid-1];
      TCH_min = ectcmaps_.tchmin[eid-1];

      if(Echan_hit[i].id%2) 
	TCH_max=ectcmaps_.tchmax[eid-1]; // single E
      else 
	TCH_max=ectcmaps_.tchmax[eid-1+1]; // overlap of two Es
    }
    else { //do not do ET geometry matching
      TCH_min = 1;
      TCH_max = 121;
    }
    
    for (j=0; j<n_tch_hits; j++) {
      // check geometry first
      if(Tchan_hit[j].id > TCH_max) break;     //get out of the loop
      if(Tchan_hit[j].id < TCH_min) continue;  //not there yet
      
      // geometry matched, check timing

      if(fabs(Echan_hit[i].t[0] - Tchan_hit[j].t_mean[0]) < tagparam_.ET_window/2) {
	if ( k<MAXECTC ) {
	ET_coincidence[k].tid     = Tchan_hit[j].id;
	ET_coincidence[k].eid     = Echan_hit[i].id;
	ET_coincidence[k].Thit    = j;
	ET_coincidence[k].Ehit    = i;
	ET_coincidence[k].status |= Tchan_hit[j].status;
	ET_coincidence[k].status |= Echan_hit[i].status;

	usedT[j]++;
	usedE[i]++;
	k++;
	}
	else {	  
	  fprintf(stderr, "Too many ET coincidences\n");
	  break;
	}
      }
    }
  }
  
  // do another loop search for unmatched T-hits
  
/*   for (i=0; i<n_tch_hits; i++) { */
/*     if (usedT[i] == 0) { */
/*       ET_coincidence[k].tid     = Tchan_hit[i].id; */
/*       ET_coincidence[k].Thit    = i; */
/*       ET_coincidence[k].status |= Tchan_hit[i].status; */
/*       k++; */
/*     } */
/*   } */
  
  n_et_coin_hits = k;
  
  //loop over found coincidences to check how many times ech hit was used and update status accordingly
  
  for (i=0; i<n_et_coin_hits; i++) {
    if (n_et_coin_hits > 1) ET_coincidence[i].status |= MULTIPLE_HITS;
    if (usedE[ET_coincidence[i].Ehit] != 1)  ET_coincidence[i].status |= MULTIPLE_T;
    if (usedT[ET_coincidence[i].Thit] != 1)  ET_coincidence[i].status |= MULTIPLE_E;
  }

  // for backward compatibility copy hits over to common/tagetbin/  
  
  dropAllBanks(&bcs_,"TAGI");
  dropAllBanks(&bcs_,"TAGR");
  
  // fill TAGI bank
  if  (n_et_coin_hits) {
    if (TAGI = makeBank(&bcs_, "TAGI", 1, sizeof(tagi_t)/4, n_et_coin_hits) ){
      for (i=0; i<n_et_coin_hits; i++) {
	TAGI->tagi[i].idt      = ET_coincidence[i].tid;
	TAGI->tagi[i].ide      = ET_coincidence[i].eid;
	TAGI->tagi[i].timel    = Tchan_hit[ET_coincidence[i].Thit].tl[0];
	TAGI->tagi[i].timer    = Tchan_hit[ET_coincidence[i].Thit].tr[0]; 	
	TAGI->tagi[i].timemean = tagtcorr_.Mean_T_val[0][ET_coincidence[i].Thit];
	TAGI->tagi[i].trf      = tagtcorr_.T_time[0][ET_coincidence[i].Thit] - 
	  tagtcorr_.Mean_T_val[0][ET_coincidence[i].Thit];

	if (ET_coincidence[i].tid %2) TAGI->tagi[i].nexttime = 0.;
	else TAGI->tagi[i].nexttime = tagtcorr_.Mean_T_val[0][ET_coincidence[i].Thit] - 
	       tagtcorr_.Mean_T_val[1][ET_coincidence[i].Thit];

	if (ET_coincidence[i].eid) TAGI->tagi[i].timee = Echan_hit[ET_coincidence[i].Ehit].t[0]; 
	else TAGI->tagi[i].timee = 0.;
      }
    }
    // fill TAGR bank
    if (TAGR = makeBank(&bcs_, "TAGR", 1, sizeof(tagr_t)/4, n_et_coin_hits)){
      for (i=0; i<n_et_coin_hits; i++) {
	TAGR->tagr[i].t_id = ET_coincidence[i].tid;
	TAGR->tagr[i].e_id = ET_coincidence[i].eid;
	TAGR->tagr[i].ttag = tagtcorr_.Mean_T_val[0][ET_coincidence[i].Thit] + tagtotof_.tag2tof;
	TAGR->tagr[i].tpho = tagtcorr_.T_time[0][ET_coincidence[i].Thit] + tagtotof_.tag2tof;
	TAGR->tagr[i].stat = ET_coincidence[i].status;
	TAGR->tagr[i].erg  = tagruncontrol_.Beam_energy*tagenergyboundaries_.Eaverage[ET_coincidence[i].eid-1]/1000;
	if(tagtcl_.Tagger_energy_correct) TAGR->tagr[i].erg *= ecor_table_.ecor[TAGR->tagr[i].e_id-1];
      }
    }
  }						    
}

//////////////////////////
// sorting functions    //
//////////////////////////

int ECh_sort(TAGE_bin_t *hit1, TAGE_bin_t *hit2){
  clasTAGE_t *TAGE = NULL;
  // sort by id firts
  if (hit1->id != hit2->id) return hit1->id - hit2->id;
  // then by time
  if (hit1->t[0] != hit2->t[0]) return hit1->t[0] - hit2->t[0];
  // error, identical hits
  fprintf(stderr, "Ech_sort -E- identiacal hits, wrong !!!!\n");
  return 0;
}

int TCh_sort(TAGT_bin_t *hit1, TAGT_bin_t *hit2){
  // sort by id firts
  if (hit1->id != hit2->id) return hit1->id - hit2->id;
  // then by time
  if (hit1->t_mean[0] != hit2->t_mean[0]) return hit1->t_mean[0] - hit2->t_mean[0];
  // error, identical hits
  fprintf(stderr, "Tch_sort -E- identiacal hits, wrong !!!!\n");
    return 0;
}

int ET_sort(ET_hit_t *hit1, ET_hit_t *hit2){

  // sort by Tid first
  if (hit1->tid != hit2->tid) return hit1->tid - hit2->tid;
  // then by time
  if (Tchan_hit[hit1->Thit].t_mean[0] != Tchan_hit[hit2->Thit].t_mean[0]) 
    return Tchan_hit[hit1->Thit].t_mean[0] - Tchan_hit[hit2->Thit].t_mean[0];
  //then by Eid 
  if (hit1->eid != hit2->eid) return hit1->eid - hit2->eid;   
  // error, identical hits
  fprintf(stderr, "ET_sort -E- identiacal hits, wrong !!!!\n");
    return 0;
}
