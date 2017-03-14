#include "rtpc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

/* stuff for calling fortran routines and linking cernlib */
#define  f2cFortran

/* return values ch2ph and ch2z must be pointer */
 void RWFTHL(int npt, float* rf, float* pf, float* wfi, float* zf, float* wzf,
	    int iopt, float* vv0, float* ee0, float* ch2ph, float* ch2z, float* del, float* delz) {
  rwfthl_ (&npt,rf,pf,wfi,zf,wzf,&iopt,vv0,ee0,ch2ph,ch2z,del,delz);
}
/* eventually move this to an include file... 
#define RWFTHL(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13) CCALLSFSUB13(RWFTHL,rwfthl,INT,FLOATV,FLOATV,FLOATV,FLOATV,FLOATV,INT,FLOATV,FLOATV,PFLOAT,PFLOAT,FLOATV,FLOATV,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13)
*/
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
void helix_fit(fit_track_to_beamline, tracknum, chainnum)
     int fit_track_to_beamline, tracknum, chainnum;

     /* fit a helix to the points on chain #chainnum and store results as
	track #tracknum. If (fit_track_to_beamline), then DO include 
	the beamline (x=0,y=0,z=unconstrained) in the fit */
{
  int ii,jj;
  float my_phi;
  float xf[MAX_HITS_ON_CHAIN];
  float yf[MAX_HITS_ON_CHAIN];
  float rf[MAX_HITS_ON_CHAIN];
  float pf[MAX_HITS_ON_CHAIN];
  float wfi[MAX_HITS_ON_CHAIN];
  float zf[MAX_HITS_ON_CHAIN];
  float wzf[MAX_HITS_ON_CHAIN];
  int iopt;
  int npt;
  float vv0[5];
  float ee0[15];
  float ch2ph;
  float ch2z;
  float del[MAX_HITS_ON_CHAIN];
  float delz[MAX_HITS_ON_CHAIN];
  float deviationSum;

  cylindrical closest_point;
  float sigma, dcap;

 if(chainnum>MAX_NUM_CHAINS) printf("chainnum is to big in helixfit!\n");

 if(GEMDEBUG) printf(" helix_fit: fitting %d hits on chain %d to make track %d\n",
		  num_hits_this_chain[chainnum],chainnum,tracknum);	 
 npt=0;
 for (jj=0; jj< num_hits_this_chain[chainnum]; jj++)
   {
     if(    (hh_hitlist[chain_hits[chainnum][jj]].status & HISUSED)
	    //	  && (hh_hitlist[chain_hits[chainnum][jj]].status & HEARLST) 
	    )
       {
	 if(GEMDEBUG) printf("fitted hit status= %d\n",hh_hitlist[chain_hits[chainnum][jj]].status);
	 rf[npt] = hh_hitlist[chain_hits[chainnum][jj]].r;
	 pf[npt] = hh_hitlist[chain_hits[chainnum][jj]].phi;
	 zf[npt] = hh_hitlist[chain_hits[chainnum][jj]].z;
	 wfi[npt]= 1.0;
	 wzf[npt]= 1.0;
	 npt++;
       }
   }
 if(fit_track_to_beamline)
   {
     rf[npt]= 0.0;
     pf[npt]= 0.0;
     zf[npt]= 0.0;
     wfi[npt]= 1.0;
     wzf[npt]= 0.0; /* zero weight for Z on the beamline point*/
     npt++;
   }
 if(GEMDEBUG) 
   {
     printf("helix fitting npt = %d\n", npt);
     for (jj=0; jj<npt; jj++)
       printf("(r,phi,z)= (%5.1f, %5.1f, %5.1f)\n",rf[jj],pf[jj],zf[jj]);
   }
 iopt= 1; /* tells rwfthl what kind of fit to do */
 //helix_error = 0;
 RWFTHL(npt,rf,pf,wfi,zf,wzf,iopt,vv0,ee0,&ch2ph,&ch2z,del,delz);

 //nkb - since we aren't using ch2ph I made it return 1 if the track
 //curls too much for the fit code
 if(!helix_error)helix_error = (int)ch2ph;
 //printf("helix_err after rwfthl = %d \n", helix_error);

 if (GEMDEBUG) {
   fprintf (stdout, "ch2ph = %8.2f,   ch2z = %8.2f \n", ch2ph, ch2z);
   fflush(stdout);
 }
 //nkb turned ch2ph into an error bit inside the rwfthl code 2-9-06

 dzds[tracknum]= vv0[1];
 r_0[tracknum] = -1.0/vv0[0]; /* radius of curvature. +ve for +ve tracks in BoNuS*/
 phi0[tracknum]= vv0[2]; /* in xy plane, direction of track relative to x axis */
 my_phi= phi0[tracknum]+PI;
 if (vv0[0]<0.0) my_phi+=PI;
 if(my_phi>2.0*PI) my_phi-=2.0*PI;
 /* dca[tracknum]= fabs(vv0[3]); */ /* dca = distance of closest approach to beamline */
 dca[tracknum]= vv0[3]; /* dca = distance of closest approach to beamline */
 x_close[tracknum]= -sin(my_phi)*(-vv0[3]);
 y_close[tracknum]=  cos(my_phi)*(-vv0[3]);
 z_close[tracknum] =  vv0[4];     /* z at point of closest approach */
 x_0[tracknum] = -sin(my_phi)*((-vv0[3])+fabs(1.0/vv0[0]));
 y_0[tracknum] =  cos(my_phi)*((-vv0[3])+fabs(1.0/vv0[0]));

 deviationSum = 0.0;
 for (jj=0; jj< num_hits_this_chain[chainnum]; jj++)
   {
     if(hh_hitlist[chain_hits[chainnum][jj]].status==1)
       {
	 hh_hitlist[chain_hits[chainnum][jj]].del = 
	   dca_to_helix(r_0[tracknum],  x_0[tracknum], y_0[tracknum], 
			dzds[tracknum],
			x_close[tracknum], y_close[tracknum], 
			z_close[tracknum],
			hh_hitlist[chain_hits[chainnum][jj]], 
			&closest_point, &sigma);
	 deviationSum += hh_hitlist[chain_hits[chainnum][jj]].del;
       } 
   } 

 if(fit_track_to_beamline) deviationSum += fabs(dca[ntracks]);

 if(npt>5) {chi2[tracknum] = deviationSum/(npt-5);} else {chi2[tracknum]=9999.9;}
 //if(GEMDEBUG) printf("helix fitting npt = %d, ch2ph, ch2z = %6.2f, %6.2f\n", npt, ch2ph, ch2z);
 //if(npt>5) {chi2[tracknum]= (ch2ph+ch2z)/(npt-5);} else {chi2[tracknum]=9999.9;}
 if(GEMDEBUG) printf("helix_fit returning (x,y,r) chi2 = %6.2f %6.2f %6.2f %6.2f for track %d\n",
		     x_0[tracknum],y_0[tracknum],r_0[tracknum],chi2[tracknum],tracknum); 
 if(GEMDEBUG) printf("done helixfitting \n");
 
}
