#include "rtpc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
/* stuff for calling fortran routines and linking cernlib */
#define  f2cFortran


int countPads(int tracknum, int chainnum)
{
  /////////////////////////////////////////////////////////////////////////
  //this is a simple routine to count how on many channels the track induced a
  //signal
  //
  ////////////////////////////////////////////////////////////////////////

  #define NOPAD -1000
  
  int padList[NUM_PADS];
  int numPads, foundPad, thisPad;
  int jj, ii, kk, n;

  numPads = 0;
  foundPad = FALSE;
  

  if(chainnum>MAX_NUM_CHAINS) printf("chainnum is to big in countPads()!\n");

 
  if(GEMDEBUG) printf("starting countPads() with chainnum = %d, hh_num_hits = %d, num_hits_this_chain = %d\n",
		    chainnum, hh_num_hits, num_hits_this_chain[chainnum]);

  if(GEMDEBUG)
    {
      for(ii = 0; ii<num_hits_this_chain[chainnum]; ii++)
	{
	  printf("chain %d point %d = %d\n", chainnum, ii, chain_hits[chainnum][ii]);
	}
    }
  //initialize the pad list
 
  for (jj=0; jj< num_hits_this_chain[chainnum]; jj++)
    {
      //quiet = FALSE;
      if(hh_hitlist[chain_hits[chainnum][jj]].status & HISUSED)
	{
	  thisPad = hh_hitlist[chain_hits[chainnum][jj]].pad;
	  if(GEMDEBUG) printf("this pad = %d\n", thisPad);
	  foundPad = FALSE;
	  if(!numPads)
	    {
	      padList[numPads] = thisPad;
	      if(GEMDEBUG) printf("setting padList[%d]= to %d\n",numPads,thisPad);
	      numPads++;
	      foundPad = TRUE;
	    }
	  else
	    {
	      kk=0;
	      while(kk<numPads && !foundPad)
		{
		  if(GEMDEBUG) printf("checking padList[%d]=%d vs thisPad=%d\n",kk,padList[kk],thisPad);
		  if(padList[kk] == thisPad)
		    {
		      foundPad = TRUE;
		      if(GEMDEBUG) printf("this Pad %d is on the list\n",thisPad);
		    }
		  else kk++;
		}
	      if(!foundPad) 
		{
		  padList[numPads] = thisPad;
		  numPads++;
		  if(GEMDEBUG) printf("did not find the pad on the list, added %d and increment numPads\n", thisPad);
		}
	    }

	}//end of status bit check

    }//end of loop over hits in the chain
  
  for(ii=0;ii<numPads;ii++)
    //HFILL(704, padList[ii],0.0, 1.0); 

  if(GEMDEBUG) printf("numPads = %d\n",numPads);
  return (numPads);
}
