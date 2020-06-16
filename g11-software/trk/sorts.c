/*
Author: David Ciarletta

Purpose: sorting fuctions for use by qsort to sort the (x,y) coordinates in the lramb fits either by x or by doca
*/
#include <math.h>
#include <ntypes.h>
#include <bostypes.h>
#include <trk_common.h>
#include "tbt.h"

int sort_by_doca(const void *pts1, const void *pts2)
{
  fit_pts *p1 = (fit_pts *)pts1;
  fit_pts *p2 = (fit_pts *)pts2;

  int result;

  if((fabs(p1->y[0] - p1->y[1])*0.5/p1->sigma) < (fabs(p2->y[0] - p2->y[1])*0.5/p2->sigma))
    {result = 1;}
  else if((fabs(p1->y[0] - p1->y[1])*0.5/p1->sigma) == (fabs(p2->y[0] - p2->y[1])*0.5/p2->sigma))
    {result = 0;}
  else
    {result = -1;}
  
  return result;
}

int sort_by_x(const void *pts1, const void *pts2)
{
  fit_pts *p1 = (fit_pts *)pts1;
  fit_pts *p2 = (fit_pts *)pts2;

  int result;

  if(p1->x < p2->x)
    {result = -1;}
  else if(p1->x == p2->x)
    {result = 0;}
  else
    {result = 1;}
  
  return result;
}
