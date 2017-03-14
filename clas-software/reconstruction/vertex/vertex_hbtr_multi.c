/* vertex_hbtr_multi_*/
/* Author: John McNabb */
/* Purpose: find vertices between 2 tracks */
/* Inputs: HBTR bank */
/* Outputs: VERT bank #2*/
/*_begin_doc
 *  RCS ID string
 *  $Id: vertex_hbtr_multi.c,v 1.1.1.1 1998/12/01 17:07:31 mcnabb Exp $*/
#define USE(var) static void * use_##var = (void *) &var
static char crcsid[] = 
   "$Id: vertex_hbtr_multi.c,v 1.1.1.1 1998/12/01 17:07:31 mcnabb Exp $";
USE(crcsid);   /* make sure it is not optimized away */
/* END of RCS information*/

#include <ntypes.h>
#include <bostypes.h>
#include <stdio.h>
#include <vertex.h>
#include <math.h>
#include <kinematics.h>

void vertex_hbtr_multi_();

void vertex_hbtr_multi_(){
  int i,j,count,ntrk;    
  clasHBTR_t *HBTR = NULL;       /*structures from bostypes.h*/
  clasVERT_t *VERT = NULL;      /*      |        */ 
  hbtr_t *hbtr;                 /*      |        */
  vert_t *vert;                 /*      V        */
  line_t line1={0,0,0,0,1,0}; /*line structures defined in vertex.h*/
  line_t line2={0,0,0,0,1,0}; /* note that the direction is a unit vector*/
  float sepd,R;                 /*various distances and the dot product*/
  int vcol=11;                   /* number of columns in VERT bank */

  /*first get the first hbtr bank */
  if(HBTR=(clasHBTR_t *)getBank(&bcs_,"HBTR")){ 
    ntrk=(HBTR->bank.nrow);              /* number of tracks in the event*/
    if(ntrk<2)                           /* only for multiple tracks events */
          return;
    hbtr = HBTR->hbtr;          /*bos bank structures(saves pointer junk)*/
    vcol = sizeof(vert_t)/4;
    if(VERT=(clasVERT_t *)makeBank(&bcs_,"VERT",2,vcol, (ntrk*ntrk-ntrk)/2)){
       vert = VERT->vert;  
       count = 0;
       for(i=0;i<ntrk-1;i++){                 /*loop over all tracks       */
	  line1.point = (hbtr+i)->vert;       /*set line1 to the ith track */
          line1.dir = v3norm((hbtr+i)->p);
	  for(j=i+1;j<ntrk;j++){           /* loop over all remaining tracks */
             line2.point = (hbtr+j)->vert;    /*set line2 to the jth track */
	     line2.dir = v3norm((hbtr+j)->p);

/*get info of closest approach between line1 and line2 */
	     vertex_doca(&line1,&line2,&sepd,&R); 
             (vert+count)->trk1=i+1;           /*write info to vert bos bank 1*/
             (vert+count)->trk2=j+1;
             (vert+count)->vert=v3mult(.5,v3add(line1.point,line2.point));
             (vert+count)->sepd=sepd;
	     (vert+count)->vertex=0;
             count++;
          }
       } 
      }                     /*end if(make vert bank)              */
     else printf("error writing the vert 2 bank\n");
  }	                  /*end if(HBTR bank exists)           */
  return;
}                               /*end vertex_hbtr_multi_*/










