#include "rtpc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

/* stuff for calling fortran routines and linking cernlib */
#define  f2cFortran
 
void sum_hit_charges(int *npads_hit, float *qtot, float *qtot_early, float *qtot_late,
		     float *qtot_win, float *qtot_intime, float qtime[NAL_SAMP],
		     float qpad_win[NAL_CHAN], float qxtra_tot[NUM_XTRA])
{
  int ii,jj,pad,time;
  int numSatL=0, numSatR= 0;
  float charge;

  *npads_hit= 0;
  *qtot= 0.0;
  *qtot_win= 0.0;
  totIntQ_L= 0.0;
  totIntQ_R= 0.0;

  for (jj=0; jj<NAL_SAMP; jj++)
    {qtime[jj]= 0.0;}
  for (ii=0; ii<NUM_XTRA; ii++)
    {qxtra_tot[ii]= 0.0;}
  for (ii=0; ii<NAL_CHAN; ii++)
    {qpad_win[ii]= 0.0;}

  /*------------------------------------------------*/

  for (jj=0; jj< hh_num_hits; jj++)
    {
      pad= hh_hitlist[jj].pad;
      charge= hh_hitlist[jj].q;
      time= hh_hitlist[jj].t;
      *qtot+= charge;
      if(time < TPC_TMIN) 
	{*qtot_early+=  charge;  }
      else if(time > TPC_TMAX) 
	{*qtot_late+=charge;}	  
      else 
	{
	  *qtot_win+= charge;
	  *qtot_intime+= charge;
	}
      qtime[time]+= charge;
      qpad_win[pad]+= charge;


      if( (charge > THRESHA) && (hh_hitlist[jj].status & HISUSED) )
	{
	  *npads_hit += 1;
	  totIntQ += charge;
	  if(pad < 1600) 
	    totIntQ_L += charge;
	  else 
	    totIntQ_R += charge;
	  if(charge > 900) 
	    {
	      if (pad < 1600) 
		numSatL++;
	      else 
		numSatR++;
	    } 
	}
    }
 
  
  for (jj=0; jj< hh_num_xtrs; jj++)
    {
      qxtra_tot[hh_xtrlist[jj].pad]+= hh_xtrlist[jj].q;
    }
}
