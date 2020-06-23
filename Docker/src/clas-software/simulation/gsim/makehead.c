#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <math.h>
#include <sys/types.h>
#include <time.h>

#include <bostypes.h>
#include <kinematics.h>
#include <sc.h>
#include <utility.h>
#include <makebanks.h>
#include <geant_commons.h>

extern GCFLAG_t  gcflag_; /* For access to runnumber */

void makehead_(int *class,int *run,int *event)
{

#define h HEAD->head[0]
#define h2 HEAD2->head[0]

  clasHEAD_t *HEAD,*HEAD2;
  time_t secs;


  /* if the HEAD bank exists, do not overwrite it */
  HEAD = (clasHEAD_t *) getBank(&wcs_,"HEAD");
  /*  fprintf(stderr,"creating head bank\n"); */


  time(&secs);

    /* make the HEAD bank */
    if (HEAD2 = (clasHEAD_t *) makeBank(&bcs_,"HEAD",0,sizeof(head_t)/sizeof(int),1)) {
      h2.version = HEAD ? h.version : 0;

      if (gcflag_.idrun != 1){
	h2.nrun = gcflag_.idrun; /* Override the runnumber if RUNG is set. */
      }else{
      h2.nrun = HEAD ? h.nrun : *run;
      }
      h2.nevent = HEAD ? h.nevent : *event;
      h2.time = HEAD ? h.time : secs;
      h2.type = HEAD ? h.type : -2; /* set type to -2, see head.ddl*/
      h2.roc = HEAD ? h.roc : 0;
      h2.evtclass = HEAD ? h.evtclass : *class;
 

      /*      bankList(&bcs_,"E+","HEAD"); */
    }
    else
      fprintf(stderr,"makehead: Unable to create HEAD bank\n");

    maketagr();


}
  
  
  
