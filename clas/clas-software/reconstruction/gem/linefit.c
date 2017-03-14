#include "rtpc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

//this function will perform two line fits, with z as the independent variable - i will call my line parameters m_x, b_x, m_y, and b_y, there is a distinct possibility that the slope will be infinite (theta = 90deg) and I have to think about what to do in that case.

void linefit(int tracknum, int chainnum) //this fit will use z as my ind. variable - does charge weighting

{
  
  float xsum, ysum, yxsum, xsqsum; 
  float xdenom, wsum, xx, yy, charge;
  xsum=0.0; 
  ysum=0.0;
  yxsum=0.0; 
  xsqsum=0.0;
  xdenom=0.0;
  wsum = 0.0;
  int jj;

 for (jj=0; jj< num_hits_this_chain[chainnum]; jj++) //fills the weighted data array with the
                                                     //coordinates of the given chain
    {
      if(hh_hitlist[chain_hits[chainnum][jj]].status == 1)
	{
	  xx = hh_hitlist[chain_hits[chainnum][jj]].r*cos(hh_hitlist[chain_hits[chainnum][jj]].phi);
	  yy = hh_hitlist[chain_hits[chainnum][jj]].r*sin(hh_hitlist[chain_hits[chainnum][jj]].phi);
	  charge = hh_hitlist[chain_hits[chainnum][jj]].q;
	  ysum += yy*charge;
	  xsum += xx*charge;
	  yxsum += yy*xx*charge;
	  xsqsum += pow(xx,2)*charge;
	  wsum += charge;
	}
    }
  
      //do a linefit on the x-y plane
      xdenom = wsum * xsqsum - pow(xsum, 2);
      m_y[tracknum] = (wsum * yxsum - xsum * ysum)/xdenom;
      b_y[tracknum] = (ysum * xsqsum - xsum * yxsum)/xdenom;
    
}
