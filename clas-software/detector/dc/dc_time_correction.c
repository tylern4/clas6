/*
   	dc_time_correction.c
      
      2/11/99   D. Lawrence


      This routine does the tzero subtraction and calls the
      timewalk correction routine respectively. This was 
      originally done in dc_time_to_dist(). This routine was
      created so that dc_calib_check and dc_time_to_dist()
      could use exactly the same routine for time corrections.

      The uncorrected time is passed in as an argument and the
      corrected time is returned by the function.
*/


#include <dc.h>

/* this is a parameter in dc_xvst_ana.inc (others are in dc.h) */
#define twtime_lo -50


/* These flags allow one to turn off tzero subtraction and/or */
/* timewalk correction. They are here for diagnostic purposes */
/* and should normally be left set to 1.                      */
int do_tzero_subtraction=1;
int do_timewalk_correction=1;

float dc_time_correction_(float *t,int *layer,int *sector,float *beta)
{
   float timec, timec_beta;
   int tw_bin,beta_bin;
   float tmax;
   int sup;
   float betaslope;
   
   int index;
   int *sec;
   int *lyr;

   sup = 1 + (((*layer)-1)/6);
  
   timec=*t;
   
   /* tzero subtraction */
   if(do_tzero_subtraction)
      timec -= map_parms[*sector][sup].map_parms[1];

   /* correct for timewalk                                 */
   /* the timewalk table is filled in dc_timewalk.c  */
   /* and the actual function is in dc_timewalk.c */
  
   /* The indexing in the map_parms[][].tmax[] array if off by one,
      so layer->layer-1 to fix this
   */

   if(do_timewalk_correction){

		timec_beta = timec; /* uncorrected for beta dependance at this point */
		
		betaslope=map_parms[1][sup].tw_betaslope[sup];
		switch(map_parms[1][sup].tw_functiontype){ /* Use sector 1 function type */
			case DC_TIMEWALK_TYPE_VIPULI:
			case DC_TIMEWALK_TYPE_MAC:
				timec_beta -= betaslope*(*beta); /* this is the "t0" for this beta */
				break;
			case DC_XVST_TYPE_LIMING: /* leave Liming timewalk as legacy code */
				break;
		}
		
		tmax = map_parms[*sector][sup].tmax[*layer - 1];
		timec -= dc_timewalk_table((float)sup, *beta, timec_beta, tmax);
	}
  
   return timec;
}

