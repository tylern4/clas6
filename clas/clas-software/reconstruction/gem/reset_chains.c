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

int reset_chains()

     /* Reset status of all used hits to unused */

{
     int ii;
     
     for (ii=0; ii<hh_num_hits; ii++)
       {
	 if (hh_hitlist[ii].status & HISUSED) hh_hitlist[ii].status= HUNUSED;
       }
     return;
}
