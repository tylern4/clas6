/* make_mctagr.c */
/* Author: John McNabb and Robert Feuerbach*/
/* Purpose: make tagr bank for monte carlo data
/* Inputs: 
   E_electron : maximum electron energy   
   E_gamma    : photon energy
   t_offset   : time offset so that start time at vertex is 0
   use_energy : 0=calculate energy from ebin, 1=use E_gamma 
*/   
/* Outputs:  
   the TAGR bank
*/

/*_begin_doc
 *  RCS ID string
 *  $Id: make_mctagr.c,v 1.7 2001/08/24 16:42:08 mcnabb Exp $*/
#define USE(var) static void * use_##var = (void *) &var
static char crcsid[] = 
   "$Id: make_mctagr.c,v 1.7 2001/08/24 16:42:08 mcnabb Exp $";
USE(crcsid);   /* make sure it is not optimized away */
/* END of RCS information*/

#include <ntypes.h>
#include <bostypes.h>
#include <stdio.h>
#include <vertex.h>
#include <math.h>
#include <kinematics.h>
#include <string.h>
#include <scalers.h>

/* NUM_E_BINS defined in scalers.h*/

int make_mctagr_(float *E_electron, float *E_gamma, float *t_offset, int *use_energy){
  make_mctagr(&bcs_,*E_electron, *E_gamma, *t_offset, *use_energy);
}

clasTAGR_t *make_mctagr(BOSbank *bos,float E_electron, float E_gamma, float t_offset, int use_energy){
  int col = (sizeof(tagr_t)/sizeof(float));
  clasTAGR_t *TAGR=NULL;
  tagr_t *tagr;
  int i;
  float E_perc;
  float Ebin_mean=0.0;
  int Ebin=1,Tbin=1;
  extern float ebin_min_map[NUM_E_BINS+1];
  extern float ebin_max_map[NUM_E_BINS+1];
  extern float ebin_mean_map[NUM_E_BINS+1];
  extern float tbin_min_map[NUM_T_BINS+1];
  extern float tbin_max_map[NUM_T_BINS+1];

  E_perc = E_gamma/E_electron; 
  if (E_perc <= ebin_min_map[1] && E_perc >= ebin_max_map[NUM_E_BINS]){
    if(TAGR=(clasTAGR_t *)makeBank(bos,"TAGR",0,col, 1)){
      tagr = TAGR->tagr;
      for(i=1;i <= NUM_E_BINS; i++){
	if (E_perc>=ebin_min_map[i] && E_perc<ebin_min_map[i-1]){
	  Ebin = i;
	  Ebin_mean = ebin_mean_map[i];
	  i=NUM_E_BINS+1;	
	}
      }

      /*
      Tbin = T_binmax(Ebin);
      */

      /* The previous method for getting the Tbins results in
       * the odd bins being empty.  Using tbin_min_map & 
       * tbin_max_map fixes this (see fill_tbin_map() in 
       * E_T_util.c).  NB: Tbin 37 corresponding to T-19 by
       * itself is always empty since T-19 is always overlapped.
       * sp 09/00.
       */

             
      for(i=1;i <= NUM_T_BINS; i++){	
	if (E_perc>=tbin_min_map[i] && E_perc<tbin_max_map[i]){
	  Tbin = i;
	  i=NUM_T_BINS+1;	
	}
      }
      
      if(0==use_energy){
	tagr->erg = Ebin_mean*E_electron;
      }else{
	tagr->erg = E_gamma;
      }
      tagr->ttag = t_offset;
      tagr->tpho = t_offset;
      tagr->stat = 15;
      tagr->t_id = Tbin;
      tagr->e_id = Ebin;      
    }
  }

  return TAGR;
}




