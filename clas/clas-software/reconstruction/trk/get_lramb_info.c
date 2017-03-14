/*
Author: David Ciarletta

Purpose: get_lramb_info extracts one fit result for a given fit#, track & SL and returns the values of that fit

Inputs: track - track number segment is in
	sl - SL that segment is in
	nfit - fit # requested for given track and SL
	
Outputs: sl_info - wire, doca, and sigma of specified fit for all six layers
	 sl_info.flag - flags that there are no more fits for current SL & track
*/
#include <ntypes.h>
#include <bostypes.h>
#include <trk_common.h>
#include "tbt.h"

sort_sls get_lramb_info(int track, int sl, int nfit)
{
  sort_sls sl_info;
  int i;

  sl_info.flag = 0;

  if(seg[track-1][sl-1].nfits == 0)
    {
      for(i=0;i<6;i++)
	{
	  sl_info.layer[i].wire = 0;
	  sl_info.layer[i].dtime = 0;
	  sl_info.layer[i].doca = 0;
	  sl_info.layer[i].sigma = 0;
	}
      sl_info.flag = 1;
      
      return sl_info;
    }
  else
    {
      for(i=0;i<6;i++)
	{
	  sl_info.layer[i] = seg[track-1][sl-1].fit[i][nfit-1];
	}
      if(nfit == seg[track-1][sl-1].nfits)
	{
	  sl_info.flag = 1;
	}
      
      return sl_info;
    }
}
