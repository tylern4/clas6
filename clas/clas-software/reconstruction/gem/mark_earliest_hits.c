/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include "rtpc.h"

/* stuff for calling fortran routines and linking cernlib */
#define  f2cFortran

int mark_earliest_hits(int chainnum)
{

  /* Take the hits on the chain 'chainnum' and, for each pad used
     on the chain, mark the hit on that pad that came at the earliest
     TDC time. We're going to try track fitting using only those hits. */


 int jj,kk,current_pad, this_time, early_time, early_hit;

 for (jj=0; jj< num_hits_this_chain[chainnum]; jj++) 
   {
     /* Start by marking all hits on the chain as unexamined by setting the TEMP bit ON*/
     hh_hitlist[chain_hits[chainnum][jj]].status |= HITTEMP;
     /*Explicitly set the 'earliest' flag off*/	  
     hh_hitlist[chain_hits[chainnum][jj]].status &= ~HEARLST;
   }

 for (jj=0; jj< num_hits_this_chain[chainnum]; jj++) 
   {
     if( (hh_hitlist[chain_hits[chainnum][jj]].status & HISUSED) &&
	 (hh_hitlist[chain_hits[chainnum][jj]].status & HITTEMP) )
       {
	 /* mark this hit as already examined by setting the TEMP bit OFF*/
	 hh_hitlist[chain_hits[chainnum][jj]].status &= ~HITTEMP;

	 current_pad= hh_hitlist[chain_hits[chainnum][jj]].pad;
	 early_time = hh_hitlist[chain_hits[chainnum][jj]].t;
	 early_hit= jj;
	 /* Check remaining hits on this chain to see if they are also on this pad*/
	 for (kk=jj+1; kk< num_hits_this_chain[chainnum]; kk++)
	   {
	     if (hh_hitlist[chain_hits[chainnum][kk]].pad == current_pad)
	       /*Same pad as master. See if it is earlier */
	       {
		 if( (hh_hitlist[chain_hits[chainnum][kk]].status & HISUSED) &&
		     (hh_hitlist[chain_hits[chainnum][kk]].status & HITTEMP) )
		   {
		     /* mark this hit as already examined by setting the TEMP bit OFF */
		     hh_hitlist[chain_hits[chainnum][kk]].status &= ~HITTEMP;
		     this_time= hh_hitlist[chain_hits[chainnum][kk]].t;
		     if(this_time < early_time)
		       {
			 /* It IS earlier. Remember it. */
			 early_time= this_time;
			 early_hit= kk;
		       }
		   }
	       }
	   }
	 /* We should now know which hit on this pad is the earliest one of the chain */
	 /* Mark it as such */
	 hh_hitlist[chain_hits[chainnum][early_hit]].status |= HEARLST;
       }
     /* Proceed with the next hit */
   }
 return(0);
}
  
	  
	  
 
