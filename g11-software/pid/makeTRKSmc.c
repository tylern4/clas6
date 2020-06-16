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



int makeTRKSmc(){
  /* input: HBTR bank
     output: TRKS bank */
   trks_t *trks;
   int sec;
   hbtr_t *hbtr;
  clasTRKS_t *TRKS=NULL;
  clasHBTR_t *HBTR = (clasHBTR_t *)getBank(&bcs_,"HBTR");
  int nTRKS = 0;
  int i, ret = 0;

  if (HBTR){
    trks = (trks_t *) malloc(sizeof(trks_t) * HBTR->bank.nrow);
    for (sec = 1; sec < 7; ++sec) {
      nTRKS = 0;
      for(i=0 ; i< HBTR->bank.nrow; i++){
	hbtr = &HBTR->hbtr[i];
	if(hbtr->itr_sec/100 == sec){
	  trks[nTRKS].trk_lev = hbtr->itr_sec % 100 + 200;
	  trks[nTRKS].beta = 1.0;
	  trks[nTRKS].st_time = 0.0;
	  trks[nTRKS].cc_time = 0.0;
	  trks[nTRKS].sc_time = 0.0;
	  trks[nTRKS].ec_time = 0.0;
	  trks[nTRKS].st_id = 1; 
	  trks[nTRKS].cc_id = 1;
	  trks[nTRKS].sc_id = 1;
	  trks[nTRKS].ec_id = 1;
	  nTRKS++;
	}  
      }
    
      if (nTRKS){
	if (TRKS = makeBank(&bcs_, "TRKS", sec, sizeof(trks_t)/4, nTRKS)){
	  for(i=0; i < nTRKS; i++) TRKS->trks[i] = trks[i];
	  ret++;

	}
      }
    }
 
  free(trks);
  }
  return ret;
}
