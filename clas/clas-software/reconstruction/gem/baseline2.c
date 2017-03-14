/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include "rtpc.h"

/* stuff for calling fortran routines and linking cernlib */
#define  f2cFortran
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
int baseline2(int flag)
     /* subtract  average of first NLINE readings from each channel */
{
  int i,j;
  float s_sum, nsamp;
      for (i=0; i<NAL_CHAN; i++)
	{
	  s_sum=0.0;
	  nsamp=0.0;
	  for (j=BASE_START; j<NLINE+BASE_START; j++)
	    {
	      s_sum+= pad_dat[j][i];
	      nsamp+=1.0;
	    }
	  s_sum /= nsamp;
	  for (j=0; j< NAL_SAMP; j++)
	    {
	      pad_dat[j][i] -= s_sum;
	    }
	}
}
