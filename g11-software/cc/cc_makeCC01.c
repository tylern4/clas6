#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ntypes.h>
#include <bostypes.h>
#include <errno.h>
#include <math.h>
#include <bosddl.h>
#include <makebanks.h>



float cc_peds[NSEC][CC_STRIPS];
float cc_A_to_phe[NSEC][CC_STRIPS];
float cc_t0[NSEC][CC_STRIPS];



float get_CC01_time(int tdc, float t0)
{ 
  float uncorrected_time = tdc; 
  float corrected_time = tdc*CC_TDC_TO_NS + t0; 
  
  if (tdc <=0) return(CC_TIME_UNDERFLOW); 
  else if (tdc >= CC_TDC_MAX)  return(CC_TIME_OVERFLOW); 
  else return(corrected_time);                                  
} 
 
float get_CC01_n_phe(int adc, float pedestal, float counts_per_photoelectron)
{
  if ((adc - pedestal) <= 0) {
    return(0);
  }
  return((adc-pedestal)/counts_per_photoelectron); 
}


int valid_CC_hit(cc0_t  cc)
{
  if(cc.id >= 1 && cc.id <= CC_STRIPS && cc.tdc > 0 && cc.tdc < CC_TDC_MAX ){
    return 1;
  }
  return(0);
}

/*convert paddle id to angle in theta - should be checked*/
float CC2theta(int id){
  return(((id-1)/2)*2.05 +7.0);
}

int make_cc01_banks_(){
  int i;
  for (i=1; i <= 6; i++) make_CC01_bank(i);
}

int make_CC01_bank(int sector)
{
  
  cc0_t *cc=NULL;
  clasCC01_t *CC01=NULL;
  cc01_t cc01arr[CC_STRIPS];
  int nCC01 = 0;
  clasCC0_t *CC;
  int id;
  
  if (CC = getGroup(&bcs_, "CC  ", sector)) {
    int cc0_index;
    for(cc0_index = 0; cc0_index < CC->bank.nrow; cc0_index++) {
      /*check paddle id and tdc values - make sure they are valid*/
      if (valid_CC_hit(CC->cc0[cc0_index])){

	id = CC->cc0[cc0_index].id;
	
	cc01arr[nCC01].id = id;
	
	cc01arr[nCC01].time = get_CC01_time(CC->cc0[cc0_index].tdc, cc_t0[sector-1][id-1]);

	cc01arr[nCC01].n_pe = get_CC01_n_phe(CC->cc0[cc0_index].adc,
					     cc_peds[sector-1][id-1],
					     cc_A_to_phe[sector-1][id-1]);
	nCC01++;
      }
    }

    if (nCC01){
      /*sort*/
      /*qsort((void*)ec01arr, nEC01, sizeof(ec01_t), ec01arr.time);*/
      
      if(CC01 = makeBank(&bcs_, "CC01 ",sector,3,nCC01)){
	int i;
	for(i=0; i < nCC01; i++) 
	  CC01->cc01[i] = cc01arr[i];
      }
    }
  }
}

int cc_begin_run(int runno){
  clasHEAD_t *HEAD = getBank(&bcs_, "HEAD");

  if (HEAD){
    if (HEAD->head[0].type >= 0) {
      /* data */
      cc_init(runno);
    } else {
      /*monte carlo - do something else */
      cc_init(1);
    }
  }
}

int cc_init(int runno)
{
  char def_map[100],*dir;
  int first; 
  char *ccmap = "CC_CALIB";

  if(dir = getenv("CLAS_PARMS") ){
    sprintf(def_map,"%s/Maps/%s.map",dir, ccmap);
    map_get_float(def_map,"pedestals","mean",216,cc_peds,runno,&first);
    /* should put error checking here if *firsttime returns -1 or -2 */
    map_get_float(def_map,"photoE","amplitude",216,cc_A_to_phe,runno,&first);
    map_get_float(def_map,"TDC","T0",216,cc_t0,runno,&first);
  }
}

int cc01_init_(int *runno){
  cc_init(*runno);
}

cc0_t *cc01_2_cc0(cc01_t *cc01, int sec){
  clasCC0_t *CC0 = getGroup(&bcs_, "CC  ", sec);
  int i;
  for(i=0; i < CC0->bank.nrow; i++){
    if (cc01->id == CC0->cc0[i].id) return &(CC0->cc0[i]);
  }
  return NULL;
}
