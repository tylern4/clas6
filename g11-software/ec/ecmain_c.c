
/******************************************************************
         forward calorimeter reconstruction main function
  *****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "eclib.h"

void ecmain_c_(int *iw, float threshold[3], int option[4])
{
  int nsec, i, uvw, io, npsble, nhit, npeak[2][3];
  float x, y;
  ECStrip strip[2][3][36];
  ECPeak peak[2][3][18];
  ECHit hit[1000];

  nhit = 0;

  for(nsec=0; nsec<NSECTOR; nsec++)
  {
    if(ecstrips(iw, threshold[0], option[0], option[1], nsec, strip) > 0)
    {
      for(io=0; io<2; io++)
      {
        for(uvw=0; uvw<3; uvw++)
	{
          npeak[io][uvw] = ecpeak(threshold[1], &strip[io][uvw][0], &peak[io][uvw][0]);
	  /*printf("new: sec, io, uvw, npeak=%d %d %d %d\n",nsec+1,io+1,uvw+1,npeak[io][uvw]);*/
	}
        if( (npsble = echit(io,nsec,&npeak[io][0],&peak[io][0][0],&hit[nhit])) > 0)
        {
          nhit += eccorr(threshold[1],option[3],io,nsec,&npeak[io][0],&peak[io][0][0],npsble,&hit[nhit]);
        }
        else
        {
          ;
        }
      }
    }
  }
  /*  ecbos(nhit,hit); */
  ecshower(nhit,hit);
}
