#include "rtpc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>


float calcChi(int tracknum, int chainnum)
{
  /////////////////////////////////////////////////////////////////////////
  //this routine calculates the residual of each point from a straight line
  //fit in the xy plane for calibration purposes.  It then returns the
  //residual sum/number of points used
  //
  ////////////////////////////////////////////////////////////////////////

  float xx, yy;
  float yfit;
  float residual, resSum;
  int jj, ii, n;
  resSum = 0.0;

  if(chainnum>MAX_NUM_CHAINS) printf("chainnum is too big in calcChi!\n");

 
  if(GEMDEBUG) printf("starting calcChi with chainnum = %d, hh_num_hits = %d, num_hits_this_chain = %d\n",
		    chainnum, hh_num_hits, num_hits_this_chain[chainnum]);

  if(GEMDEBUG)
    {
      for(ii = 0; ii<num_hits_this_chain[chainnum]; ii++)
	{
	  printf("chain %d point %d = %d\n", chainnum, ii, chain_hits[chainnum][ii]);
	}
    }
  for (jj=0; jj< num_hits_this_chain[chainnum]; jj++)
    {
      //quiet = FALSE;
      if(hh_hitlist[chain_hits[chainnum][jj]].status ==1)
	{
	  xx = hh_hitlist[chain_hits[chainnum][jj]].r*cos(hh_hitlist[chain_hits[chainnum][jj]].phi);
	  yy = hh_hitlist[chain_hits[chainnum][jj]].r*sin(hh_hitlist[chain_hits[chainnum][jj]].phi);
	  yfit = m_y[tracknum]*xx + b_y[tracknum];
	  residual = abs(yy - yfit);
	  resSum +=residual;
	  n++;
	  if(GEMDEBUG) printf("hit num %d on chain %d has residual %f\n",jj, chainnum, residual);
	}
    }
  return (resSum/(float)n);
}
