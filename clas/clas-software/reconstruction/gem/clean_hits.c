/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include "rtpc.h"
#include "TPC_BOS.h"

/* stuff for calling fortran routines and linking cernlib */
#define  f2cFortran
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

int clean_hits()

     /* Here is where we disable hits for various reasons
	the first of which is that a hit is out of time. */

{
     int ii;
     
     for (ii=0; ii<hh_num_hits; ii++)
       {
	 /* the following two seem redundant but are not because the possible ePath length varies with z */
	 //if (hh_hitlist[ii].t < TPC_TMIN) hh_hitlist[ii].status= HDISEAR;
	 if (hh_hitlist[ii].r == SMAX) hh_hitlist[ii].status= HDISEAR; /* can be set to SMAX in read_path.c */

	 /* the following two seem redundant but are not because the possible ePath length varies with z */
	 //if (hh_hitlist[ii].t > TPC_TMAX) hh_hitlist[ii].status= HDISLAT;
	 if (hh_hitlist[ii].r == SMIN) hh_hitlist[ii].status= HDISLAT; /* can be set to SMIN in read_path.c */
       }
     return;
}
