/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include "rtpc.h"

/* stuff for calling fortran routines and linking cernlib */
#define  f2cFortran
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
int event_id(float vtl, float charge, float edist, float sdist, int chainnum)
{
  #define BOUND_CUT 3.0
  int ii,jj;
  float dQdx;
  int outbadS, outbadE, inbadS, inbadE, bothbad;
  float hitphi;
  int left, right;
  bothbad = FALSE;
  outbadS = FALSE;
  outbadE = FALSE;
  inbadS = FALSE;
  inbadE = FALSE;
  left = FALSE;
  right = FALSE;
  

  dQdx = charge/vtl; 
  if (!quiet) printf("vtl, charge, dQdx, edist, sdist: %f, %f, %f, %f, %f\n",
		     vtl, charge, dQdx, edist, sdist);

  if ( (dQdx > MIN_PROT_DQDT_CUT) && 
       (dQdx < MAX_PROT_DQDT_CUT)
     ) 
    {
      for (jj=0; jj< num_hits_this_chain[chainnum]; jj++)
	{
	  if(hh_hitlist[chain_hits[chainnum][jj]].status & HISUSED)
	    {
	      hitphi = hh_hitlist[chain_hits[chainnum][jj]].phi;
	      if(hitphi < 3*PI/2 && hitphi > PI/2) right = TRUE;
	      else left = TRUE;
	    }
	}
      if(left) 
	{
	  leftCand++;
	}
      if(right) 
	{
	  rightCand++;
	}
      if(left && right)
	{
	  leftCand--;
	  rightCand--;
	  //printf("chain is on both sides of chamber\n");
	  //quiet = FALSE;
	  return(26);
	}
	    
      if ( edist < -BOUND_CUT ) 
	{
	  outbadE = TRUE;
	} //track ended outside drift region, far from chamber boundary

      if ( edist > BOUND_CUT ) 
	{
	  inbadE = TRUE;
	} //track ended inside drift region, far from chamber boundary
      if ( sdist < -BOUND_CUT ) 
	{
	  outbadS = TRUE;
	} //track started outside drift region, far from chamber boundary

      if ( sdist > BOUND_CUT ) 
	{
	  inbadS = TRUE;
	} //track started inside drift region, far from chamber boundary

      if(inbadE && inbadS) 
	{
	  siei++;
	  bothbad = TRUE;
	}
      if(outbadE && outbadS) 
	{
	  soeo++;
	  bothbad = TRUE;
	}
      if(inbadE && outbadS) 
	{
	  soei++;
	  bothbad = TRUE;
	}
      if(outbadE && inbadS)
	{
	  sieo++;
	  bothbad = TRUE;
	}
      if(!bothbad && outbadE) end_aftr_bound++;
      if(!bothbad && inbadE) end_bfore_bound++;
      if(!bothbad && outbadS) strt_bfore_bound++;
      if(!bothbad && inbadS) strt_aftr_bound++;
      
      if(bothbad) return(3);//bad start and end of track
      if(outbadE || inbadE) return(2);//bad track end
      if(outbadS || inbadS) return(1);//bad track start
      return(5);//good time proton
      
    }
  return(26); //bad dQdx
}
