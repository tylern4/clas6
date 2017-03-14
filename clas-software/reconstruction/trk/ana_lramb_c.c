/*
Author: David Ciarletta

Purpose: ana_lramb_c obtains the segment information from trk_dcam
	rearanges it and sends it to resolve_lramb for segment fitting
	Then it takes the results of segment fitting stores them in the
	global variable "seg" and sends the results to weed_lramb to 
	sort out similar fits.

Inputs: isect - sector segment is in
	is - SL segment is in
	digi - array of hit information from trk_dcam
	psi - local slope of track
	wgtslp - weighting factor for slope difference
	trk - track number that segment belongs to
	LRflag - flag to trk_dcam that fitting on current segment is complete
	trk_print - tcl variable flaging whether or not to print ana_lramb fitting information
	
Outputs: none
*/

#include <stdio.h>
#include <string.h>

#include <ntypes.h>
#include <bostypes.h>
#include <trk_common.h>
#include "tbt.h"

void ana_lramb_c_(long *isect, long *is, float digi[30], float *psi, float *wgtslp, long *trk, int *LRflag, long *trk_print)
{
  hit_info layer[6];
  int sector;
  int sl;
  int track;
  lramb new_vals;
  int i,j;
  char buff[200];
  static char name[] = "ANA_LRAMB";
  static int fit_total = 0;
  static int slay = 0;
  static int trck = 0;

    /*extract wire, doca, and sigma from digi array*/
  for(i=0;i<6;i++)
    {
      layer[i].wire = (int)digi[i*5];
      layer[i].dtime = digi[i*5+2];
      layer[i].doca = digi[i*5+3];
      layer[i].sigma = digi[i*5+4];
    }

  /*set the current sector, SL, and trk#*/
  sector = (int)*isect;
  sl = (int)*is;
  track = (int)*trk;

  /*reset fitting totals if we are on a new SL/track*/
  if(slay != sl || trck != track)
    {
      slay = sl;
      trck = track;
      fit_total = 0;
      *LRflag = 0;
      for(j=0;j<6;j++)
	{
	  seg[track-1][sl-1].nknown[j] = 0;
	}
    }

  /*send values off to be fit and get the results of the good fits*/
  new_vals = resolve_lramb(sector, sl, layer, *psi, *wgtslp);

  /*check to see if there were errors in the fit*/
  if(new_vals.flag == 1)
    {
      if(*trk_print > 0)
	{
	  sprintf(buff,"No values from resolve_lramb");
	  recmes_(&name,"I",&buff,strlen(name),strlen("I"), strlen(buff));
	}
      return;
    }
  /*if no errors then store the fits in the global variable*/
  else
    {
      /*loop over the good fits*/
      for(i=0;i<new_vals.counter;i++)
	{
	  /*if there are too many fits for this SL in this track then stop storing the fits*/
	  if((1+fit_total)<=MAX_FITS)
	    {
	      /*loop over the layers and store the fit values in the global variable*/
	      for(j=0;j<6;j++)
		{
		  seg[track-1][sl-1].fit[j][fit_total].wire = layer[j].wire;
		  seg[track-1][sl-1].fit[j][fit_total].dtime = layer[j].dtime;
		  seg[track-1][sl-1].fit[j][fit_total].doca = new_vals.doca[j][i];
		  seg[track-1][sl-1].fit[j][fit_total].sigma = layer[j].sigma;
		}
	      seg[track-1][sl-1].chi2[fit_total] = new_vals.chi2[i];
	      /*increment the total number of fits for this SL in this track*/
	      fit_total++;
	      seg[track-1][sl-1].nfits = fit_total;
	      /*print out the fitting information if it was requested*/
	      if(*trk_print>0)
		{
		  j=i+1;
		  printf("  SL: %1d FIT: %-2d CHI2: %-3.3f SLOPE_D: %-1.4f\n",sl, j, new_vals.chi2[i], new_vals.slopeD[i]);
		  printf("  WIRE:  %-4d   %-4d   %-4d   %-4d   %-4d   %-4d\n",layer[0].wire, layer[1].wire,
			 layer[2].wire, layer[3].wire, layer[4].wire, layer[5].wire);
		  printf("  LRAMB: %-1.4f %-1.4f %-1.4f %-1.4f %-1.4f %-1.4f\n\n", new_vals.doca[0][i], 
			 new_vals.doca[1][i], new_vals.doca[2][i], new_vals.doca[3][i], new_vals.doca[4][i], 
			 new_vals.doca[5][i]);
		}
	    }
	  /*if there were too many fits try to weed some out first*/
	  else
	    {
	      weed_lramb(track,sl);
	      fit_total = seg[track-1][sl-1].nfits;
	      /*if there are still too many fits then warn the user that some fits are being neglected*/
	      if(fit_total >= MAX_FITS)
		{
		  *LRflag=1;
		  sprintf(buff,"TOO MANY CALLS!");
		  recmes_(&name,"I",&buff,strlen(name),strlen("I"), strlen(buff));
		  return;
		}
	    }
	}
      /*if there were new fits found and there is at least 2 total fits then try to weed some out*/
      if(new_vals.counter > 0 && fit_total > 1)
	{
	  if(*trk_print>0)
	    {printf("  NFITS (before weed): %d\n",seg[track-1][sl-1].nfits);}
	  weed_lramb(track,sl);
	  if(*trk_print>0)
	    {printf("  NFITS (after weed):  %d\n\n",seg[track-1][sl-1].nfits);}
	  fit_total = seg[track-1][sl-1].nfits;
	}
      /*if good fits were found for this SL in the current track, let trk_dcam know to move on*/
      if(fit_total>0)
	{*LRflag=1;}
    }

  return;
}
