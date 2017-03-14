/*
Author: David Ciarletta

Purpose: weed_known_amb eliminates tracks which do not comply with the known ambiguities
	
Inputs: trk - track # segment is in
	sl - SL the segement is in
	
Outputs: none
*/
#include <ntypes.h>
#include <bostypes.h>
#include <trk_common.h>
#include <math.h>
#include "tbt.h"
void weed_known_amb_(long *trk, long *sl)
{
  weed_known_amb((int)*trk, (int)*sl);
}
void weed_known_amb(int trk, int sl)
{
  int i,j,k,l;
  int tflgs,uflag;
  int nhits=0;
  float dchi2_max;

  if(seg[trk-1][sl-1].nfits > 1)
    {
      /* Throw away any fits whose doca sign is disallowed by double(or more) hits in layers */
      /* For each layer in which a double hit was found, check whether this doca matches any 2-hit lr resolution */
      for(i=0;i<seg[trk-1][sl-1].nfits;i++)
	{
	  tflgs=0;
	  for(k=0;k<6;k++)
	    {
	      uflag = 0;
	      for(j=0;j<seg[trk-1][sl-1].nknown[k];j++)
		{
		  if(seg[trk-1][sl-1].fit[k][i].wire == abs(seg[trk-1][sl-1].known_amb[k][j]))
		    {
		      uflag = 1;
		      if(((seg[trk-1][sl-1].fit[k][i].wire == seg[trk-1][sl-1].known_amb[k][j] && seg[trk-1][sl-1].fit[k][i].doca > 0) || 
			  (seg[trk-1][sl-1].fit[k][i].wire == -seg[trk-1][sl-1].known_amb[k][j] && seg[trk-1][sl-1].fit[k][i].doca < 0)))
			uflag = 0;
		    }
		}
	      tflgs += uflag ;
	    }
	  if(tflgs)
	    {
	      for(j=i;j<seg[trk-1][sl-1].nfits-1;j++)
		{
		  for(l=0;l<6;l++)
		    {seg[trk-1][sl-1].fit[l][j] = seg[trk-1][sl-1].fit[l][j+1];}
		  seg[trk-1][sl-1].chi2[j] = seg[trk-1][sl-1].chi2[j+1];
		}
	      seg[trk-1][sl-1].nfits--;
	    }
	}

      /* Now throw out any fits which have a chisquare a lot bigger than the best one */
      for(l=0;l<6;l++)
	{
	  if(seg[trk-1][sl-1].fit[l][0].wire > 0) nhits++;
	} 
      dchi2_max = 6.6/(nhits-1);
      for(i=0;i<seg[trk-1][sl-1].nfits-1;i++)
	{
	  for(j=i+1;j<seg[trk-1][sl-1].nfits;j++)
	    {
	      if((seg[trk-1][sl-1].chi2[i] + dchi2_max) < seg[trk-1][sl-1].chi2[j])
		{
		  for(k=j;k<seg[trk-1][sl-1].nfits-1;k++)
		    {
		      for(l=0;l<6;l++)
			{seg[trk-1][sl-1].fit[l][k] = seg[trk-1][sl-1].fit[l][k+1];}
		      seg[trk-1][sl-1].chi2[k] = seg[trk-1][sl-1].chi2[k+1];
		    }
		  seg[trk-1][sl-1].nfits--;
		  j--;
		}
	      else if((seg[trk-1][sl-1].chi2[j] + dchi2_max) < seg[trk-1][sl-1].chi2[i])
		{
		  for(k=i;k<seg[trk-1][sl-1].nfits-1;k++)
		    {
		      for(l=0;l<6;l++)
			{seg[trk-1][sl-1].fit[l][k] = seg[trk-1][sl-1].fit[l][k+1];}
		      seg[trk-1][sl-1].chi2[k] = seg[trk-1][sl-1].chi2[k+1];
		    }
		  seg[trk-1][sl-1].nfits--;
		  j=i;
		}
	    }
	}
    }  
  return;
}
