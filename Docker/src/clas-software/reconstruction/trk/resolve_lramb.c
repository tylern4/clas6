/*
Author: David Ciarletta

Purpose: resolve_lramb takes the segment information from ana_lramb_c, sends it to be extracted into (x,y) 
	coordinates, sorts those coordinated by doca/sigma for line fitting, sends the points to line_fit 
	to be fit to a straight line, and interprets the results of the good fits to be stored and sent back
	to ana_lramb_c
	
Inputs: sector - sector that track is in
	SL - SL that segment is in
	layer[6] - array of wire, doca, and sigma for segment
	psi - local slope of track
	wgtslp - weight of slope difference in fitting
	
Outputs: results - slope difference, doca, and chi^2 for each good fit
	 results.counter - number of good fits found
	 results.flag - flags that an error occured and no fits were found
*/
#include <stdlib.h>
#include <math.h>
#include <ntypes.h>
#include <bostypes.h>
#include <trk_common.h>
#include "tbt.h"

static char crname[] = "RESOLVE_LRAMB";
static char crauth[] = "David Ciarletta";

lramb resolve_lramb(int sector, int SL, hit_info layer[6], float psi, float wgtslp)
{
  char crmess[200];
  clasDCGM_t *DCGM = NULL;
  float doca[6];
  float phi[6];
  float dl[6];
  float yw[6];
  int nohit[6] = {0};
  fit_pts coord[6];
  int miss = 0;
  int nlay = 6;
  int i,j,k;
  int first=1;
  line_params good_fits;
  lramb results;
  good_fits.flag = 0;
  results.flag = 0;

  k=(SL-1)*6;
  /*adjust # of layers for super layer 1*/
  if(SL == 1)
    {
      nlay=4;
      coord[4].x = 999;
      coord[4].y[0] = 0;
      coord[4].y[1] = 0;
      coord[4].sigma = 1;
      nohit[4] = 1;
      coord[5].x = 999;
      coord[5].y[0] = 0;
      coord[5].y[1] = 0;
      coord[5].sigma = 1;
      nohit[5] = 1;
    }

  /*get DCGM bank for coordinate system geometry*/
  if(DCGM=(clasDCGM_t *)getGroup(&wcs_,"DCGM",sector))
    {
      /*loop over the layers*/
      for(i=0;i<nlay;i++)
	{
	  /*if layer has no hit then skip geometry*/
	  if(layer[i].wire <= 0)  
	    {
	      coord[i].x = 999;
	      coord[i].y[0] = 0;
	      coord[i].y[1] = 0;
	      coord[i].sigma = 1;
	      nohit[i] = 1;
	      miss++;
	    }
	  /*if layer has hit then get (x,y) pairs for fitting*/
	  else
	    {
	      coord[i] = get_lramb_pts(layer[i], DCGM->dcgm[k+i], psi, &first);
	    }
	  /*check to make sure there are at least 3 hits in the SL*/
	  if((nlay-miss)<2)
	    {
	      results.flag = 1;
	      return results;
	    }
	}
    }
  /*exit if unable to get DCGM bank*/
  else
    {
      sprintf(crmess,"Unable to retrieve DCGM bank!");
      recmes_(&crname,"W",&crmess,strlen(crname),strlen("W"),strlen(crmess));
      results.flag = 1;
      return results;
    }

  /*sort the point array from largest doca/sigma to smallest for line fitting*/
  qsort((void *)coord, 6, sizeof(fit_pts), sort_by_doca);

  /*perform line fitting*/
  good_fits = line_fit(coord, nlay, miss, wgtslp, psi);

  /*exit if the fit failed*/
  if(good_fits.flag == 1)
    {
      sprintf(crmess,"Line fitting failed!");
      recmes_(&crname,"W",&crmess,strlen(crname),strlen("W"),strlen(crmess));
      results.flag = 1;
      return results;
    }

  /*extract the relavent information from line fitting*/
  for(i=0;i<good_fits.num_fit;i++)
    {
      results.chi2[i] = good_fits.chi2[i];
      k = 0;
      /*report +or- doca (-:lower phi side of wire, +:higher phi side of wire, 0:no hit on wire)*/
      for(j=0;j<6;j++)
	{
	  if(nohit[j] == 0)
	    {
	      results.doca[j][i] = good_fits.LR[i][k]*fabs(layer[j].doca);
	      k++;
	    }
	  else
	    {results.doca[j][i] = 0;}
	}
      /*calculate the slope difference for the fit*/
      results.slopeD[i] = good_fits.a[i] - tan(psi);
    }

  /*store the number of good patterns*/
  results.counter = good_fits.num_fit;

  /*return the results of ana_lramb*/
  return results;
}
