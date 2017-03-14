/*
Author: David Ciarletta

Purpose: sort_fits_ sorts out all possible patterns of segment fits in a track given the track number
	(e.g. if there were 2 good fits for all 6 segments in a track then sort_fits_ finds all 2^6 patterns)
	
Inputs: trk - current track #

Outputs: trkinf[36][4] - wire, drift time, doca, and sigma for all 36 layers for current pattern of fits
	 flag - flags the last pattern for the current track
*/
#include <ntypes.h>
#include <bostypes.h>
#include <trk_common.h>
#include "tbt.h"

void sort_fits_(long *trk, float trkinf[36][4], long *flag)
{
  static int nfit[6] = {1,1,1,1,1,1};
  static int flg[6] = {0};
  int track = (int)*trk;
  int i,j;
  sort_sls sl_info;
  int tfits,slfits;

  if(*flag == 1)
    {
      tfits = 1;
      for(i=0;i<6;i++)
	{
	  nfit[i] = 1;
	  flg[i] = 0;
	  if(seg[track-1][i].nfits == 0)
	    {slfits = 1;}
	  else
	    {slfits = seg[track-1][i].nfits;}
	  tfits = tfits*slfits;
	}
      *flag = 0;
    }

  for(i=0;i<6;i++)
    {
      sl_info = get_lramb_info(track,i+1,nfit[i]);
      for(j=0;j<6;j++)
	{
	  trkinf[i*6+j][0] = sl_info.layer[j].wire;
	  trkinf[i*6+j][1] = sl_info.layer[j].dtime;
	  trkinf[i*6+j][2] = sl_info.layer[j].doca;
	  trkinf[i*6+j][3] = sl_info.layer[j].sigma;
	}
      if(sl_info.flag == 1)
	{flg[i] = 1;}
      else
	{flg[i] = 0;}
    }

  if(flg[5] == 0)
    {nfit[5]++;}
  else
    {
      nfit[5] = 1;
      if(flg[4] == 0)
	{nfit[4]++;}
      else
	{
	  nfit[4] = 1;
	  if(flg[3] == 0)
	    {nfit[3]++;}
	  else
	    {
	      nfit[3] = 1;
	      if(flg[2] == 0)
		{nfit[2]++;}
	      else
		{
		  nfit[2] = 1;
		  if(flg[1] == 0)
		    {nfit[1]++;}
		  else
		    {
		      nfit[1] = 1;
		      if(flg[0] == 0)
			{nfit[0]++;}
		      else
			{*flag = 1;}
		    }
		}
	    }
	}
    }
  return;
}
