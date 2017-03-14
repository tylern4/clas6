/*
Author: David Ciarletta

Purpose: line_fit sorts out all possible patterns of (x,y) points for the segment, fits to the 3 points
	 with the largest sigma(y) and continues to add on points, one at a time as long as the chi^2 remains
	 below a certain value. If all the points have been added and the chi^2 is still good then line_fit
	 stores the pattern as a good fit along with some information about the fit
	 
	 
Inputs: coord[6] - array of (x,y) points and sigma(y) for all the hits in the segment
	nlay - number of layers in current SL
	miss - number of layers that didn't have a hit in the segment
	wgtslp - weight factor for slope difference in fit
	psi - local slope of track
	
Outputs: values -  patterns and fit values for successful fits
*/
#include <stdlib.h>
#include <math.h>
#include <ntypes.h>
#include <bostypes.h>
#include <trk_common.h>
#include "tbt.h"

/* RCS ID string
 * $Id: line_fit.c,v 1.9 1999/09/08 14:03:17 claschef Exp $
 */
float prob_(float *,int *);

line_params line_fit(fit_pts coord[6], int nlay, int miss, float wgtslp, float psi)
{
  line_params values;
  int i,j,k,l,m,n;
  float y[2][4];
  float y2[4][16];
  float temp_nocore;
  fit_results line[6];
  float probchi;
  int ndof = nlay-miss-1;
  int newpat;
  fit_pts coord2[6];
  values.flag = 0;
  n=0;

  for(i=0;i<6;i++)
    {coord2[i] = coord[i];}

  /*sort another set of points by x so we know which pts go with which wires*/
  qsort((void *)coord2,6,sizeof(fit_pts),sort_by_x);

  /*sort the 4 possible patterns for the 2 wires with largest doca/sigma*/
  for(i=0;i<2;i++)
    {
      int upperj = 1 << (i+1);

      for(j=0;j<upperj;j++)
	{
	  int upperk = 4/(1 << (i+1));
	  for(k=0;k<upperk;k++)
	    {  /* for j even(odd), fill y[1] (y[0]) */
	      y[i][j*upperk+k] = coord[i].y[(j+1) % 2];
	    }
	}
    }

  /*sort all the patterns for the remaining wires*/
  for(i=0;i<(nlay-miss-2);i++)
    {
      int upperj = 1 << (i+1);
      for(j=0;j<upperj;j++)
	{
	  int upperk = 1 << ( (nlay-miss-2) - (i+1) );
	  for(k=0;k<upperk;k++)
	    {y2[i][j*upperk+k] = coord[i+2].y[(j+1) % 2];}
	}
    }

  /*loop over the basic 4 patterns for the 2 wires*/
  for(k=0;k<4;k++)   /* k is pattern counter */
    {
      newpat=1;
      /*fit to the 2 points*/
      for(i=0;i<2;i++)
	{line[0] = calc_line(coord[i].x,y[i][k],coord[i].sigma,&newpat,line[0]);}

      /*if determinant is zero exit*/
      if(line[0].chi2 == -1)
	{
	  values.flag = 1;
	  return values;
	}

      /*calculate the chi2 for these 2 hits*/
      for(i=0;i<2;i++)
	{
	  temp_nocore = (line[0].a*coord[i].x + line[0].b - y[i][k])/coord[i].sigma;
	  temp_nocore = temp_nocore * temp_nocore;
	  line[0].chi2 += temp_nocore;
	}

      /*check chi2 to make sure it's worth continuing*/
      if(line[0].chi2 < 25)
	{
	  int upperi = 1 << (nlay-miss-2);
	  /*now add on one wire at a time as long as the chi2 stays below 25*/
	  for(i=0;i<upperi;i++)
	    {
	      int jcand;
	      
	      if(i % 2 == 1) jcand = nlay-miss-3;
	      else if((8*i/upperi) % 2 == 1) jcand = 2;
	      else if((4*i/upperi) % 2 == 1) jcand = 1;
	      else jcand = 0;

	      if((j-jcand) > 0) {
	       j=jcand;

	       while(line[j].chi2 < 25)  /* j is the number of hits beyond 2 used */
		{
		  /*if there are no more wires to add see if we have a good fit*/
		  if(j+2 == nlay-miss)
		    {
		      temp_nocore = line[j].a-tan(psi);
		      temp_nocore = temp_nocore * temp_nocore;
		      line[j].chi2 += wgtslp*temp_nocore;
		      probchi = prob_(&line[j].chi2,&ndof);
		      line[j].chi2 = line[j].chi2/ndof;
		      
		      /*store the fits with "decent" chi2*/
		      if(probchi >= 0.0002)
			{
			  values.a[n] = line[j].a;
			  values.chi2[n] = line[j].chi2;
			  
			  /*store the left/right ambiguity for each wire in the pattern*/
			  for(l=0;l<(nlay-miss);l++)
			    {
			      for(m=0;m<2;m++)
				{
				  if(coord[m].x == coord2[l].x)
				    {
				      if(y[m][k] == coord2[l].y[0])
					{values.LR[n][l] = -1;}
				      else
					{values.LR[n][l] = 1;}
				    }
				}
			      for(m=2;m<nlay-miss;m++)
				{
				  if(coord[m].x == coord2[l].x)
				    {
				      if(y2[m-2][i] == coord2[l].y[0])
					{values.LR[n][l] = -1;}
				      else
					{values.LR[n][l] = 1;}
				    }
				}
			    }
			  /*increment the number of successful fits*/
			  n++;
			}
		      /*make sure we get out of this while loop*/
		      line[j+1].chi2 = 100;
		    }
		  /*if there are more wires add one on and compute the chi2*/
		  else
		    {
		      /*add current wire to fit*/
		      line[j+1] = calc_line(coord[j+2].x,y2[j][i],coord[j+2].sigma,&newpat,line[j]);
		      
		      /*if determinant is zero exit*/
		      if(line[j+1].chi2 == -1)
			{
			  values.flag = 1;
			  return values;
			}
		      
		      /*recalculate chi2*/
		      for(l=0;l<2;l++)
			{
			  temp_nocore = (line[j+1].a*coord[l].x + line[j+1].b - y[l][k])/coord[l].sigma;
			  temp_nocore = temp_nocore * temp_nocore;
			  line[j+1].chi2 += temp_nocore;
			}
		      for(l=2;l<j+3;l++)
			{
			  temp_nocore = (line[j+1].a*coord[l].x + line[j+1].b - y2[l-2][i])/coord[l].sigma;
			  temp_nocore = temp_nocore * temp_nocore;
			  line[j+1].chi2 += temp_nocore;
			}
		    }
		  /*go to the next wire*/
		  j++;
		}
	      }
	    }
	}
    }

  /*store the number of good fits found*/
  values.num_fit = n;

  /*return the fit values*/
  return values;
}


