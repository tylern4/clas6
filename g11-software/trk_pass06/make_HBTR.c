#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <math.h>
#include <bostypes.h>
#include <makebanks.h>

/* -=======================================================- *
$Id: make_HBTR.c,v 1.2 1998/04/19 18:33:26 mcnabb Exp $
$Author: mcnabb $
$Revision: 1.2 $
$Date: 1998/04/19 18:33:26 $
* -=======================================================- */

int make_hbtr_(){
  return(make_HBTR());
}

int make_HBTR(){         /*makes HBTR bank from HBER bank*/
  int i,j,count,ntrk;    
  clasHBTR_t *HBTR = NULL;       /*structures from bostypes.h*/
  clasHBER_t *HBER = NULL;      /*      |        */ 
  hbtr_t *hbtr;                 /*      |        */
  hber_t *hber;                 /*      V        */
  int hbtrcol=9;                /* number of columns in the hbtr bank*/

  /*first get the first hber bank */
  if(HBER=(clasHBER_t *)getBank(&bcs_,"HBER")){ 
    ntrk=(HBER->bank.nrow);              /* number of tracks in the event*/
    hber = HBER->hber;          /*bos bank structures(saves pointer junk)*/
    if(HBTR=(clasHBTR_t *)makeBank(&bcs_,"HBTR",0,hbtrcol, ntrk)){
       hbtr = HBTR->hbtr;  
       for(i=0;i<ntrk;i++){                 /*loop over all tracks*/
	 hber2hbtr((hber+i),(hbtr+i)); 
       }                         /*end for i = loop over all tracks*/
       return(1);                 /*succesfully made HBTR bank*/
    }                            /*end if made HBTR*/
  }                              /*end if HBER exists*/
  printf("failed to make HBTR");
  return(0);                      /*make HBTR bank failed*/
}




