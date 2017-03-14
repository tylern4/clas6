#include "rtpc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

/* stuff for calling fortran routines and linking cernlib */
#define  f2cFortran

void find_dt(int tracknum, int *tmin, int *tmax)

{
  int ii,jj,chainnum;
  *tmin=  9999;
  *tmax= -9999;
  chainnum= track_chain[tracknum];
  
  for (jj=0; jj< num_hits_this_chain[chainnum]; jj++)
    {
      if(hh_hitlist[chain_hits[chainnum][jj]].status & HISUSED)
	{
	    if (hh_hitlist[chain_hits[chainnum][jj]].t < *tmin)
	      *tmin= hh_hitlist[chain_hits[chainnum][jj]].t;
	    if (hh_hitlist[chain_hits[chainnum][jj]].t > *tmax)
	      *tmax= hh_hitlist[chain_hits[chainnum][jj]].t;
	}
    }
}
