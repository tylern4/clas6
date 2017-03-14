#include "rtpc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>


/* the prototype:
  float track_charge(int, float*, float*, float*);
*/
float track_charge(int tracknum,float *vtl,float *sdist,float *edist)

     /* for track number 'tracknum' find the first and last hits along the helix already
	fitted to it.  Returned  value  is the sum of hits'  pulse-heights  along the
	helix within the  visible  portion of the rtpc. Also return:
	*vtl= length along helix from first hit to last  hit
	*sdist= distance from rtpc entrance point  to the first  hit
	*edist= distance, along helix, from  rtpc exit point to last hit

	'first' and  'last'  hits are defined  in the sense  that  the helix originates
	at the fitted point close[tracknum].
     */

{
  float sig_entr, sig_exit, dummy;
  float mtl, dtl, smin, smax, sumq;
  int num_x;

  /* We need the boundary points of this track: where it enters/leaves rtpc */

  num_x= findwall(tracknum, 1, &sig_entr, &sig_exit);
  if(!num_x) 
    {
/*       printf(">>>>> Track never intersects rtpc at all! In function track_charge\n"); */
/*       quiet=FALSE; */
      sig_entr= 999.9;
      sig_exit= 999.0;
    }
/*   printf("testing dca for inside-ness...\n"); */
  if(inside_rtpc(x_close[tracknum],y_close[tracknum],z_close[tracknum]))
    {
      sig_exit= sig_entr;
      if(!findwall(tracknum,-1, &sig_entr,&dummy) )
	{
/* 	  printf(">>>>> Track starts inside RTPC but never hits a wall!! \n"); */
/* 	  quiet= FALSE; */
	  sig_entr= 999.9;
	}
      sig_entr *=-1;
    }

  sumq= sumtrackq(tracknum,&smin,&smax);

  /* remember noteworthy points along this track */
  sig2xyz(tracknum, sig_entr, &point_entry);
  sig2xyz(tracknum, sig_exit, &point_exit);
  sig2xyz(tracknum, smin,     &point_appear);
  sig2xyz(tracknum, smax,     &point_disappear);

  mtl= smin - sig_entr; /* missing track length: from entry point to first signal */
  dtl= sig_exit - smax; /* disappearing track length: from last signal to exit point */ 
  *vtl= smax-smin;
  *sdist= mtl;
  *edist= dtl;



 if(!quiet)
   {
     printf("Track enters rtpc volume at s=%5.1f (x,y,z)=(%6.1f,%6.1f,%6.1f)\n",
	    sig_entr,point_entry.x,point_entry.y,point_entry.z);

     printf("         becomes visible at s=%5.1f (x,y,z)=(%6.1f,%6.1f,%6.1f)\n",
	    smin,    point_appear.x,point_appear.y,point_appear.z);

     printf("              disappears at s=%5.1f (x,y,z)=(%6.1f,%6.1f,%6.1f)\n",
	    smax,    point_disappear.x,point_disappear.y,point_disappear.z);

     printf("  and leaves rtpc volume at s=%5.1f (x,y,z)=(%6.1f,%6.1f,%6.1f)\n",
	    sig_exit,point_exit.x,point_exit.y,point_exit.z);

     printf("Track runs from s=%5.2f to %5.2f (%5.1fmm)\n",sig_entr,sig_exit, sig_exit-sig_entr);
     printf("      missing length= %5.2f disappearing length= %5.2f charge=%6.1f\n",mtl,dtl,sumq);
   }
 return(sumq);
}
