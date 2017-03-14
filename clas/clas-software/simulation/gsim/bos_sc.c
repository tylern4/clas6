/*
 * $Id: bos_sc.c,v 1.3 2008/03/16 04:33:17 fklein Exp $
 *
 * Documentation for subroutine bos_sc.c
 *
 * Purpose:
 * --------
 *  Put the hits from digi_sc into the BOS banks.
 *
 *  Re-write of the original bos_sc.F, which became obsolete
 *  when moving to full geometry, and which had problems in
 *  the large angle section of hte code.
 *  This code should probably be absorbed into a rewrite of
 *  digi_sc.F to digi_sc.c
 *
 * Author: Maurik Holtrop     Created: Mon April 6 13:45:00 EST 
 *
 *_end_doc
 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "ntypes.h"
#include "bostypes.h"
#include "kinematics.h"
#include "sc.h"
#include "gsim_c_common.h"
#include "geant_commons.h"
#include "sc_gsim.h"

static char cfile[] =  "$RCSfile: bos_sc.c,v $";
static char crevis[] = "$Revision: 1.3 $";
static char cstate[] = "$State: Exp $";
static char cdate[] =  "$Date: 2008/03/16 04:33:17 $";
static char cautho[] = "$Author: fklein $";
/*   Module information: */
static char crname[] = "bos_sc.c";
static char crauth[] = "Maurik Holtrop";

/* Fortran COMMON blocks we haven't gotten rid of yet. */

extern GSIMCONTROL_t gsimcontrol_;
int geant_to_pdg_(int *);
void   check_sc_bank_(void);

extern int FIRST_SPLIT_PADDLE; /* First paddle that has ganged strips: set in geom_sc.c */
/* From sets_sc we find: */

enum SCH_HITS {HITX,HITY,HITZ,HITCX,HITCY,HITCZ,PTOT,STAK,PART};
enum SC_DIGI {ADCL,TDCL,ADCR,TDCR};

#define MTRUE  1
#define MFALSE 0

/* Stub for calling from FORTRAN. */

void bos_sc(void);
void bos_sc_(void);


void bos_sc_(void){
  bos_sc();
}

void bos_sc(void){

  int zero=0;
  int one =1;

  /* For the MC hits code. */

  int nvdimh=1;
  int numvsh;
  int nhdimh=9;
  int nhmaxh=500;
  int itracksh[500];
  int numbvh[500];
  float hitsh[500][NHITS_SCH];  /* from gsim_c_common: NHITS_SCH = 9 */

  /* For the SC digi to BOS code. */

#define MAX_ADC_VAL 8192
#define MAX_TRACK_DIGI 1000  /* Max number of tracks contributing to each hit.*/
#define MAX_DIGI   100       /* Max number of digitizations we'll allow. */
/* #define NVOL_SC 2  from gsim_c_common.h  # of volume descriptions: SC, SCIN */
/* #define NDIGI_SC 4 from gsim_c_common.h  # of digi pars: ADCL,TDCL,ADCR,TDCR */

  int numvs[2];   /* Volume descriptor index: SC,SCIN */
  int max_track_digi=MAX_TRACK_DIGI;
  int max_digi = MAX_DIGI;
  int nvol_sc=NVOL_SC;
  int ndigi_sc=NDIGI_SC;
  int num_vol[NVOL_SC];      /* Contains the # of the vol's to check. */
  int itracks[MAX_DIGI][MAX_TRACK_DIGI]; /* List of tracks that contrib to this digi*/
  int ntrack[MAX_DIGI];      /* Number of tracks that contrib to this digi. */
  int num_vol_digi[MAX_DIGI][NVOL_SC]; /* For each digi, which volume */
  int kdigits[MAX_DIGI][NDIGI_SC];     /* The digitizations for each digi. */
  int ndigi;                           /* # of digi for this vol list. */
  int idigi;                           /* Digi counter index. */
  int ipaddle,ipaddle2;  /* Converted num_vol_digi. */
  int i1,i2;
  char do_not_process[MAX_DIGI];        /* Flag the digi's that were already done. */
  int num_to_bos;                      /* Number of digi's to go to bos < ndigi */
  /* Shared. */

  int nhits;
  int isec,ihit;
  int id;
  int ii;

  clasSCH_t *SCHbank;
  clasSC_t  *SCbank;

#ifdef DEBUG
  printf(" NEW BOS_SC code. \n");
  gphits_("SC  ","SCIN",4,4); /* Debug: print the hits to console. */
  gpdigi_("SC  ","SCIN",4,4); /* Debug: print the digitizations to console. */
#endif

  if(!gsimcontrol_.nomcdata[SC]){  /* Start SCHbank bank output. */

    for(isec = 1;isec<=6; isec++){
      numvsh = isec;

      gfhits_("SCH ","SC  ",&nvdimh,&nhdimh,&nhmaxh,&zero,
	      &numvsh,itracksh,numbvh,hitsh,&nhits,4,4);
      if(nhits>0){

/*	printf("Nhits: %d in sector %d\n",nhits,isec);	 */
	
	if( (SCHbank = (clasSCH_t *)makeBank(&bcs_,"SCH ",isec,sizeof(sch_t)/sizeof(int),nhits))
	    == NULL){
	  fprintf(stderr," ERROR-- Could not create new SCH bank on output.\n");
	}else{
	  for(ihit=0;ihit<nhits;ihit++){
	    id = (int)hitsh[ihit][PART];
#ifdef DEBUG
	    printf("Hit: %d  Pid: %f %d  PTOT: %e TRAK: %d Numbvh: %d\n",ihit,
 		   hitsh[ihit][PART],id,hitsh[ihit][PTOT],itracksh[ihit],numbvh[ihit]);
#endif 
	    SCHbank->sch[ihit].x    = hitsh[ihit][HITX];
	    SCHbank->sch[ihit].y    = hitsh[ihit][HITY];
	    SCHbank->sch[ihit].z    = hitsh[ihit][HITZ];
	    SCHbank->sch[ihit].cx   = hitsh[ihit][HITCX];
	    SCHbank->sch[ihit].cy   = hitsh[ihit][HITCY];
	    SCHbank->sch[ihit].cz   = hitsh[ihit][HITCZ];
	    SCHbank->sch[ihit].pmom = hitsh[ihit][PTOT];
	    SCHbank->sch[ihit].track= itracksh[ihit];
	    SCHbank->sch[ihit].id   = geant_to_pdg_(&id);

	  }
	}
      }
    }
  }       /* End SCH bank output . */

  /* Start filling the SC bank with the info from the GEANT Digi banks. 
   * The Geant banks have info for 57 paddles, 40 - 57 are ganged together so
   * the info is to be combined into 40-48.
   */
  
  if(!gsimcontrol_.nodata[SC]){
    
    num_vol[1] = 0;  /* For each gfdigi call return all SCIN with digi info.  */
    for(isec=1;isec<=6;isec++){

      memset(do_not_process,MFALSE,sizeof(do_not_process));  /* Set do_not_process to zero */
      num_to_bos = 0;

      num_vol[0] = isec;  /* For each gfdigi, return only SC in sector isec. */
      
      gfdigi_("SC  ","SCIN",&max_track_digi,&nvol_sc,&ndigi_sc,&max_digi,num_vol,
	      itracks,ntrack,num_vol_digi,kdigits,&ndigi,4,4);

      if(ndigi >0 ){  /* This sector had digitizations. */

#ifdef DEBUG
	fprintf(stderr,"Number of digitizations: %d\n",ndigi);
#endif	
	for(idigi=0;idigi<ndigi;idigi++){

	  if(do_not_process[idigi]==MTRUE){

#ifdef DEBUG
	    fprintf(stderr,"Processing of hit %d bypassed. \n",idigi);
#endif
	    continue;
	  }

	  num_to_bos++;  
	  ipaddle = num_vol_digi[idigi][1] - (isec-1)*57; /* 57*6 total SCIN id's ... */

	  if(ipaddle < 0 || isec != num_vol_digi[idigi][0]){
	    fprintf(stderr,"Serious problem in SC_DIGI: isec: %d ipaddle: %d num_vol_digi[%d]=%d,%d \n",
		    isec,ipaddle,num_vol_digi[idigi][0],num_vol_digi[idigi][1]);
	    continue;
	  }
	  
#ifdef DEBUG
	  fprintf(stderr,"digi[%2d]: isec: %d ipad: %d, ntrack: %d \n",idigi,isec,ipaddle,ntrack[idigi]);
	  fprintf(stderr,"         : ADCL: %d TDCL: %d  ADCR: %d  TDCR: %d \n",
		  kdigits[idigi][ADCL],kdigits[idigi][TDCL],kdigits[idigi][ADCR],kdigits[idigi][TDCR]);
#endif

/* We need to check whether this hit was part of a "ganged" set, if so, deal with it. 
 * Note that paddle '40' is combined with paddle '41' , '42' with '43' etc.
 * Thus paddle_id/2 tells you if the two paddles are in the same pair or not. 
 *
 * NOTICE: FIRST_SPLIT_PADDLE=39, because it's an INDEX, when starting to count
 * from ZERO. (We're using C remember.)
 */
	  if(ipaddle > FIRST_SPLIT_PADDLE){ /* Yes, it might have a partner. */
	    for(ii=idigi+1; ii<ndigi; ii++){ /* Loop over the digi's still to process only. */
	      ipaddle2 = (num_vol_digi[ii][1] - (isec-1)*57);
	      i1 = (int)ipaddle/2;
	      i2 = (int)ipaddle2/2;
	      i1 = (i1==i2);
	      
	      if(do_not_process[ii] == MFALSE && 
		 (ipaddle2 = (num_vol_digi[ii][1] - (isec-1)*57))> FIRST_SPLIT_PADDLE &&
		 (int)(ipaddle/2) == (int)(ipaddle2/2)){  /* ipaddle2 is adjacent to ipaddle ! */
		 
		do_not_process[ii]=MTRUE;    /* Do not process this one again. */
		
/* We need to sum the ADC pulses and take the first one of the TDC pulses. */ 		

#define ADC_OVERFLOW_CHK(x) (x>MAX_ADC_VAL?MAX_ADC_VAL:x)
#define TDC_FIRST_CHK(x1,x2) (x1<x2?x1:x2)
		kdigits[idigi][ADCL] = ADC_OVERFLOW_CHK(kdigits[idigi][ADCL]+kdigits[ii][ADCL]);
		kdigits[idigi][ADCR] = ADC_OVERFLOW_CHK(kdigits[idigi][ADCR]+kdigits[ii][ADCR]);
		kdigits[idigi][TDCL] = TDC_FIRST_CHK(kdigits[idigi][TDCL],kdigits[ii][TDCL]);
		kdigits[idigi][TDCR] = TDC_FIRST_CHK(kdigits[idigi][TDCR],kdigits[ii][TDCR]);
	      }
	    }
	  }    /* Done processing split paddles. */
	}      /* Digi loop. */

/* Now store the digi in the SC bosbank. */
	
	if((SCbank = (clasSC_t *)makeBank(&bcs_,"SC  ",isec,5,num_to_bos))==NULL){
	  fprintf(stderr," ERROR-- Could not create new SC bank on output.\n");
	}else{


/* Go throught the digi loop again, this time skipping the "flagged" digi's
 * that are added to the "partner" paddle. 
 */
	  ii = 0;  /* Count the banks, not the digi's */

	  for(idigi=0;idigi<ndigi;idigi++){
	    if(do_not_process[idigi]==MTRUE)continue;    /* Skip this one. */
	    
	    ipaddle = num_vol_digi[idigi][1] - (isec-1)*57; /* 57*6 total SCIN id's ... */

/* For paddles in the last two panels we need to recalculatte the id, taking into account the
 * ganged paddles.
 */

	    if(ipaddle > FIRST_SPLIT_PADDLE){
	      id = (int)(ipaddle - FIRST_SPLIT_PADDLE-1)/2 + FIRST_SPLIT_PADDLE+1;
	    }else{
	      id = ipaddle;
	    }
	    SCbank->sc[ii].id = id;
	    SCbank->sc[ii].tdcl = kdigits[idigi][TDCL];
	    SCbank->sc[ii].adcl = kdigits[idigi][ADCL];
	    SCbank->sc[ii].tdcr = kdigits[idigi][TDCR];
	    SCbank->sc[ii].adcr = kdigits[idigi][ADCR];
	    ii++;
	  }
	}
      }
    }      
  }

#ifdef DEBUG
  check_sc_bank_();
#endif

}

#ifdef DEBUG
void check_sc_bank_(void){

  int i,j;

  clasSC_t *SCb;
  
  for(j=1;j<=6;j++){
    

    SCb = (clasSC_t *)getGroup(&bcs_,"SC  ",j);
    if(SCb!=NULL){
      for(i=0;i<SCb->bank.nrow;i++){
	printf("Sector %2d  Paddle: %3d  tdcl: %5d  tdcr: %5d adcl: %5d adcr: %5d\n",
	       j,SCb->sc[i].id,SCb->sc[i].tdcl,SCb->sc[i].tdcr,
	       SCb->sc[i].adcl,SCb->sc[i].adcr);
      }
    }
  }
  return;
  
}
#endif
