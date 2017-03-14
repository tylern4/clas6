#include "rtpc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

/* stuff for calling fortran routines and linking cernlib */
#define  f2cFortran
 
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
float sumtrackq(int tracknum, float *smin, float *smax)

     /* returns  function  value as sum of charges in  hits  assigned  to tracknum.
      smin/smax  are returned set to  path  length from  helix  starting point to
      nearest and  farthest measured points,  respectively */
{

  int jj;
  int npt, chainnum;
  cylindrical closest_point;
  float sigma,sumq;

  chainnum= track_chain[tracknum];

  npt=0;
  sumq= 0.0;
  *smin= 9999999.9;
  *smax=-9999999.9;
  
  for (jj=0; jj< num_hits_this_chain[chainnum]; jj++)
    {
      if(hh_hitlist[chain_hits[chainnum][jj]].status & HISUSED)
	{
	  sumq+= hh_hitlist[chain_hits[chainnum][jj]].q;
	  dca_to_helix(r_0[tracknum],  x_0[tracknum], y_0[tracknum], dzds[tracknum],
		       x_close[tracknum], y_close[tracknum], z_close[tracknum],
		       hh_hitlist[chain_hits[chainnum][jj]], &closest_point, &sigma);
	  if(sigma < *smin)
	    { 
	      *smin= sigma;
	      tmax  = hh_hitlist[chain_hits[chainnum][jj]].t;
	    }
	  if(sigma > *smax)
	    {
	      *smax= sigma;
	      tmin  = hh_hitlist[chain_hits[chainnum][jj]].t;
	    }
	  npt++;
	}
    }
  return(sumq);
}
  
