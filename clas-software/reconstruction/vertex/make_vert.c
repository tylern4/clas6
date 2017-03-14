/* make_vert.c */
/* Author: John McNabb */
/* Purpose: makes vertex banks from TBTR or HBTR bank */
/* Inputs: TBTR or HBTR bank */
/* Outputs: VERT bank */
/*_begin_doc
 *  RCS ID string
 *  $Id: make_vert.c,v 1.1.1.1 1998/12/01 17:07:31 mcnabb Exp $*/
#define USE(var) static void * use_##var = (void *) &var
static char crcsid[] = 
   "$Id: make_vert.c,v 1.1.1.1 1998/12/01 17:07:31 mcnabb Exp $";
USE(crcsid);   /* make sure it is not optimized away */
/* END of RCS information*/

#include <ntypes.h>
#include <bostypes.h>
#include <stdio.h>
#include <vertex.h>
#include <math.h>
#include <kinematics.h>

void make_vert();
void make_vert_();

void make_vert_(){
  make_vert();
}

void make_vert(){
  clasTBTR_t *TBTR = NULL;       /*structures from bostypes.h*/
  clasHBTR_t *HBTR = NULL;       /*      ditto        */

  if(TBTR=(clasTBTR_t *)getBank(&bcs_,"TBTR")){    
    vertex_tbtr_();
  }	                  /*end if(TBTR bank exists)           */
  else if(HBTR=(clasHBTR_t *)getBank(&bcs_,"HBTR")){    
    vertex_hbtr_();
  }	                  /*end if(HBTR bank exists)           */
  /*  else printf("\nerror making vert bank: no BTR banks found");*/

  return;
}                               /*end vertex_tbtr_*/









