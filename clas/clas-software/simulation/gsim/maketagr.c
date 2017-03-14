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

void maketagr()
{
  clasTAGR_t *TAGR,*TAGR2;

  int i;
  /* if the TAGR bank exists, write it verbatim into bcs */
  TAGR = (clasTAGR_t *) getBank(&wcs_,"TAGR");

  if (TAGR) {

    /* make the new TAGR bank */
    if (TAGR2 = (clasTAGR_t *) makeBank(&bcs_,"TAGR",0,TAGR->bank.nrow * sizeof(tagr_t)/sizeof(int),1)) {
      for (i = 0; i < TAGR->bank.nrow; ++i) {
	tagr_t *t1 = &TAGR->tagr[i];
	tagr_t *t2 = &TAGR2->tagr[i];

	t2->erg = t1->erg;
	t2->ttag = t1->ttag;
	t2->tpho = t1->tpho;
	t2->stat = t1->stat;
	t2->t_id = t1->t_id;
	t2->e_id = t1->e_id;

      }
 
    }
    else
      fprintf(stderr,"maketagr: Unable to create TAGR bank\n");

  }


}
  
  
  
