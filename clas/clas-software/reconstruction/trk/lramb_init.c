/*
Author: David Ciarletta

Purpose: lramb_init_ declares the global variable seg and zeros out the number of fits in that array

Inputs: none

Outputs: none 
*/
#include <ntypes.h>
#include <bostypes.h>
#include <trk_common.h>
#include "tbt.h"

fit_info seg[MAX_TRACKS][6];

void lramb_init_()
{
  int i,j;

  for(i=0;i<MAX_TRACKS;i++)
    {
      for(j=0;j<6;j++)
	{seg[i][j].nfits = 0;}
    }
  return;
}
