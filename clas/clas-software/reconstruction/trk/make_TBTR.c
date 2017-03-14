#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <math.h>
#include <bostypes.h>
#include <makebanks.h>

/* -=======================================================- *
$Id: make_TBTR.c,v 1.2 1998/04/19 18:33:26 mcnabb Exp $
$Author: mcnabb $
$Revision: 1.2 $
$Date: 1998/04/19 18:33:26 $
* -=======================================================- */

int make_tbtr_(){
  return(make_TBTR());
}

int make_TBTR(){         /*makes TBTR bank from TBER bank*/
  int i,j,count,ntrk;    
  clasTBTR_t *TBTR = NULL;       /*structures from bostypes.h*/
  clasTBER_t *TBER = NULL;      /*      |        */ 
  tbtr_t *tbtr;                 /*      |        */
  tber_t *tber;                 /*      V        */
  int tbtrcol=10;                /* number of columns in the tbtr bank*/

  /*first get the first tber bank */
  if(TBER=(clasTBER_t *)getBank(&bcs_,"TBER")){ 
    ntrk=(TBER->bank.nrow);              /* number of tracks in the event*/
    tber = TBER->tber;          /*bos bank structures(saves pointer junk)*/
    if(TBTR=(clasTBTR_t *)makeBank(&bcs_,"TBTR",0,tbtrcol, ntrk)){
       tbtr = TBTR->tbtr;  
       for(i=0;i<ntrk;i++){                 /*loop over all tracks*/
	 tber2tbtr((tber+i),(tbtr+i),i+1); 
       }                         /*end for i = loop over all tracks*/
       return(1);                 /*succesfully made TBTR bank*/
    }                            /*end if made TBTR*/
  }                              /*end if TBER exists*/
  printf("failed to make TBTR");
  return(0);                      /*make TBTR bank failed*/
}











