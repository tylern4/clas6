/*
Author: David Ciarletta

Purpose: calc_line performs incremental sums for a linear regression fit
	and calculates the values for slope and intercept at each step in 
	the sum.

Inputs: x - the x coordinate of the current point
	y - the y coordinate of the current point
	err - the error on the y coordinate for the current point
	reset - flag to signal that a new fit is begining
	line - results from previous step in fit
	
Outputs: out - current totals of sums of fit values and slope and intercept at this point in the fit
*/
#include <math.h>
#include <ntypes.h>
#include <bostypes.h>
#include <trk_common.h>
#include "tbt.h"

#define SMALL 0.000001

fit_results calc_line(float x, float y, float err, int *reset, fit_results line)
{
  float x_sigma2;
  float y_sigma2;
  float x2_sigma2;
  float xy_sigma2;
  float one_sigma2;
  float inv_m[2][2];
  float determinant;
  fit_results out;
  float scale;

  /*if starting from scratch set values to those for first point*/
  if(*reset==1)
    {
      *reset=0;
      out.a = 0;
      out.b = 0;
      out.chi2 = 0;
      out.x = x/(err*err);
      out.x2 = x*x/(err*err);
      out.y = y/(err*err);
      out.xy = x*y/(err*err);
      out.one = 1/(err*err);
      return out;
    }
  /*otherwise pick up where we left off*/
  else
    {
      x_sigma2=line.x;
      x2_sigma2=line.x2;
      one_sigma2=line.one;
      xy_sigma2=line.xy;
      y_sigma2=line.y;
    }
 
  /*sum up values for fit*/
  x_sigma2 += x/(err*err);
  x2_sigma2 += x*x/(err*err);
  one_sigma2 += 1/(err*err);
  xy_sigma2 += x*y/(err*err);
  y_sigma2 += y/(err*err);

  /* if all the terms are very small (ie. err is very large),
   * let's get a sense of the scale for the matrix so that we properly
   * compare the determinant against zero, and aren't tricked by merely
   * small elements.
   */
  scale = (fabs(x_sigma2) + fabs(x2_sigma2) + fabs(one_sigma2));

  /* protect against a zero or nearly-zero scale factor */
  if ( (scale*scale) <= SMALL)
    scale = 1.;
  
  determinant = x2_sigma2*one_sigma2/(scale*scale) - x_sigma2*x_sigma2/(scale*scale);
  /*  determinant = x2_sigma2*one_sigma2 - x_sigma2*x_sigma2; */

  /*if the determinant is 0 exit to avoid dividing by 0*/
  if ( (fabs(determinant) < SMALL) )
    {
      out.chi2 = -1;
      return out;
    }
  /* rescale the determinant */
  determinant *= scale*scale;

  /*calculate the inverse matrix*/
  inv_m[0][0] = one_sigma2/determinant;
  inv_m[0][1] = -x_sigma2/determinant;
  inv_m[1][0] = -x_sigma2/determinant;
  inv_m[1][1] = x2_sigma2/determinant;
  
  /*calculate slope and intercept*/
  out.a = xy_sigma2*inv_m[0][0] + y_sigma2*inv_m[1][0];
  out.b = xy_sigma2*inv_m[0][1] + y_sigma2*inv_m[1][1];
  out.chi2 = 0;
  out.x = x_sigma2;
  out.x2 = x2_sigma2;
  out.y = y_sigma2;
  out.xy = xy_sigma2;
  out.one = one_sigma2;
  
  return out;
}
