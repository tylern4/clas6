/* vertex_hbtr_ */
/* Author: John McNabb */
/* Purpose: make corrections to the hbtr vertex location */
/* Inputs: HBTR bank */
/* Outputs: VERT bank */
/*_begin_doc
 *  RCS ID string
 *  $Id: vertex_hbtr.c,v 1.1.1.1 1998/12/01 17:07:31 mcnabb Exp $*/
#define USE(var) static void * use_##var = (void *) &var
static char crcsid[] = 
   "$Id: vertex_hbtr.c,v 1.1.1.1 1998/12/01 17:07:31 mcnabb Exp $";
USE(crcsid);   /* make sure it is not optimized away */
/* END of RCS information*/

#include <ntypes.h>
#include <bostypes.h>
#include <stdio.h>
#include <vertex.h>
#include <math.h>
#include <kinematics.h>

void vertex_hbtr_();

void vertex_hbtr_(){
  int i,j,k,ntrk,found;    
  clasHBTR_t *HBTR = NULL;       /*structures from bostypes.h*/
  clasVERT_t *VERT = NULL;      /*      |        */ 
  hbtr_t *hbtr;                 /*      |        */
  vert_t *vert;                 /*      V        */
  line_t line1={0,0,0,0,1,0}; /*line structures defined in vertex.h*/
                                /* note that the direction is a unit vector*/
  line_t zbeam={0,0,0,0,0,1}; /*line defining the beam location*/   
  float d=0,R=0;                    /*various distances and the dot product*/
  int vcol=7;                   /* number of columns in VERT bank */
  extern vector3_t beam_position;
  
  /*first get the first hbtr bank */
  if(HBTR=(clasHBTR_t *)getBank(&bcs_,"HBTR")){ 
    ntrk=(HBTR->bank.nrow);              /* number of tracks in the event*/
    hbtr = HBTR->hbtr;          /*bos bank structures(saves pointer junk)*/
    vcol = sizeof(vert_t)/4;
    if(VERT = (clasVERT_t *)makeBank(&bcs_, "VERT",1, vcol, ntrk)){
       vert = VERT->vert;  
       for(i=0;i<ntrk;i++){                 /*loop over all tracks       */
	  line1.point = (hbtr+i)->vert;     /*set line1 to the ith track */
	  line1.dir = v3norm((hbtr+i)->p);
	  zbeam.point=beam_position;        /*get beam_position*/
	  /*beam still assumed to be traveling in +Z direction*/
          R=v3dot(line1.dir,zbeam.dir);
	  vertex_doca(&line1,&zbeam,&d,&R); /*get info of closest approach
                                               between line1 and zbeam */
         
          (vert+i)->trk1=0;                  /*write info to vert bos bank 1*/
          (vert+i)->trk2=i+1;
	  (vert+i)->vert=v3mult(.5,v3add(line1.point,zbeam.point));
          (vert+i)->sepd=d;
          (vert+i)->vertex=0;
       } 
       if(ntrk>1)
         vertex_hbtr_multi_();
      }                     /*end if(make vert bank)              */
     else printf("error writing the vert bank\n");
  }	                  /*end if(HBTR bank exists)           */
  return;
}                               /*end vertex_hbtr_*/









