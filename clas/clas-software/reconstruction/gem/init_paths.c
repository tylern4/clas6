
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include "rtpc.h"

/////////////////////////////////////////////////////////////
//this routine replaces the old read_paths.c routine and instead
//assigns spatial coordinates for each cell according to the electron
//drift path functions
//////////////////////////////////////////////////////////////

int init_paths()

{
#define FALSE 0
  
 float tt_inout;//the time it takes for an electron to travel
                //from the cathode to the first GEM
 float tt_fit;  //time coordinate corrected for 
                //proper spatial projection
 float toff;     //time it takes for electron to travel from
                //1st GEM to pad surface
 int side;      //side of the chamber, 0 = Left, 1 = Right
 int ii, tt, jj, kk;
  
 //pad_locs(0,0); /* initialization */
 generate_pad_locations();
  printf("just initialized pad location.  Starting init_paths() routine\n");
  for (ii = 0; ii<NUM_PADS; ii++)
    {
      if(padLocs[ii].phi > PI/2 && padLocs[ii].phi < 3*PI/2) //right side 
	side = 1;
      else side = 0;//left side
      
      setPadTimes(padLocs[ii].z,&toff,side,&tt_inout);
      //toff = 0;//for now
      for (tt=0; tt<NAL_SAMP; tt++)
	{
	  tt_fit = tt_inout - ((float)tt*NS_PER_TIC - TPC_TMIN*NS_PER_TIC - toff);
	  if(tt_fit < 0) 
	    {
	      //printf("tt_fit %f negative! point went to %f, phi 0\n",tt_fit, SMIN);
	      rawCYL[tt][ii].s =SMIN;
	      rawCYL[tt][ii].phi = 0;
	      //rawCYL[t][i].charge = NODATA;
	    }
	  else if(tt<(TPC_TMIN + toff/NS_PER_TIC)) 
	    {
	      //printf("tt(%d) less than minimum(%f) point went to %f, phi 0\n",tt, (TPC_TMIN + toff/NS_PER_TIC),SMAX);
	      rawCYL[tt][ii].s =SMAX;
	      rawCYL[tt][ii].phi = 0;
	    }
	  else 
	    {
	    rawCYL[tt][ii].s = getS(padLocs[ii].z, tt_fit, side);
	    rawCYL[tt][ii].phi = getPhi(ii, tt_fit, side, tt_inout);
	    if(rawCYL[tt][ii].s > 60 || rawCYL[tt][ii].s < 20)
	      {
		printf("rawCYL[%d][%d].s = %f is bad!\n", tt, ii, rawCYL[tt][ii].s);
	      }
	    if(rawCYL[tt][ii].phi < 0) rawCYL[tt][ii].phi += 2*PI;
	    if(rawCYL[tt][ii].phi > 2*PI) rawCYL[tt][ii].phi -= 2*PI;
	    if(rawCYL[tt][ii].phi < 0 || rawCYL[tt][ii].phi > 2*PI) 
	      {
		printf("rawCYL[%d][%d].phi = %f is bad in read_path()\n", tt, ii, rawCYL[tt][ii].phi);
		if(rawCYL[tt][ii].s > 60 || rawCYL[tt][ii].s < 20)
		  {
		    printf("rawCYL[%d][%d].s = %f is also bad!\n", tt, ii, rawCYL[tt][ii].s);
		  }
		else printf("but rawCYL[%d][%d].s = %f is ok",tt, ii, rawCYL[tt][ii].s);

		printf("just calculated: tt_fit, toff, tt_inout = %.1f %.1f %.1f\n", tt_fit, toff, tt_inout);
		for(jj=0;jj<=ii;jj++)
		  {
		    for(kk=0;kk<=tt;kk++)
		      {
			printf("rawCYL[%d][%d] phi, s = %f %f\n", kk, jj,rawCYL[kk][jj].phi,rawCYL[kk][jj].s);
		      }
		  }
		exit (1);
	      }
	    }
	  
	  rawCYL[tt][ii].z = padLocs[ii].z;
	  rawXYZ[tt][ii].x = rawCYL[tt][ii].s * cos(rawCYL[tt][ii].phi);
	  rawXYZ[tt][ii].y = rawCYL[tt][ii].s * sin(rawCYL[tt][ii].phi);
	  rawXYZ[tt][ii].z = rawCYL[tt][ii].z;
	}//end loop over time bins
    }//end loop over pads
}//end init_paths
