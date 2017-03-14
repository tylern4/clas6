/* trk_swim.c */
/* Author: John McNabb and Robert Feuerbach*/
/* Purpose: swims particle through all planes (Vrt,St,DC,CC,SC,EC)
            note that this is done using trk_swim2pln, a stripped down
	    version of sda_swim.  The result of this is that the swimming
	    only permist crossing one layer at a time, and therefore when a
	    track crosses a plane, the swimming starts from the intersection 
	    of the track with that plane.
*/
/* Inputs: 
   tber_t tber             :      pointer to tber bank row of track
   sdaswim common block to get plane locations
   zangle  - replaces tcl variables trk_zangle
   minstep - replaces tcl variable trk_zstep(1)
   maxstep - replaces tcl variable trk_zstep(2)
*/   
/* Outputs:  
   vector3_t trkint[NPLN]  :      array of track intercepts with planes
   vector3_t trkintdir[NPLN]  :      array of track direction at intercepts
   float trklength[NPLN]   :      array of path length from tbtr point to 
                                      the trackk-plane-intercept 
*/

/*_begin_doc
 *  RCS ID string
 *  $Id: trk_swim.c,v 1.6 2002/04/03 20:50:30 liji Exp $*/
#define USE(var) static void * use_##var = (void *) &var
#define DELTA_L 60.0
static char crcsid[] = 
   "$Id: trk_swim.c,v 1.6 2002/04/03 20:50:30 liji Exp $";
USE(crcsid);   /* make sure it is not optimized away */
/* END of RCS information*/

#include <ntypes.h>
#include <bostypes.h>
#include <stdio.h>
#include <vertex.h>
#include <math.h>
#include <kinematics.h>
#include <string.h>
#include <trk.h>

/* prototype for external call */
int get_tber_info(tber_t *tber, int slinfo[6], int *psec, int *ptrk_in_sect, int *ptrk_in_hbtr);

void trk_swim2pln_(float pln[8], float vins[9], float vect[9], float tintp[7], float *sint, float *zangle, float *minstep, float *maxstep, float *lmax, int *l_int, int *stepnumber);

int trk_swim(tber_t *tber,vector3_t trkint[NPLN],vector3_t trkintdir[NPLN],
     float trklength[NPLN], float zangle,float minstep, float maxstep){
  vector3_t position;
  vector3_t momentum;
  int sector;
  int stepnumber =1; /*used in trk_swim2pln to determine if first step*/
  int oldstepnumber =1; /*used in trk_swim2pln to determine if first step*/

  int slinfo[6];
  int trk_in_sect;
  int trk_in_hbtr;
  float alpha,q,p,lambda,phi;
  float cl,sl,cp,sp,ca,sa;      /*sin and cos of lambda,phi, and alpha */  
  static float Piby3;
  static int first =1;
  vector3_t emptyint = {1000.,1000.,1000.};
  vector3_t emptydir = {0.,0.,0.};
  float emptylen = 1000.;

  float vsav[9];      /*in case have to restart swimming */
  float vins[9];      /*used in trk_swim2pln*/
  float vect[9];      /*gotten from trk_swim2pln*/
  float tintp[7];     /*used in trk_swim2pln*/
  float lmax;         /*used in trk_swim2pln*/
  int l_int;          /*used in trk_swim2pln,=1 if trk intercepted plane*/
  float pln[8];
  float trk_length=0;
  float sint;         /*trklength from vect to tintp*/


  int plane; /*counter of the plane # */
  int i=0;
  
  if(first){
    first=0;
    Piby3 = acos(-1)/3;
  }
 
  /* clear the trkint, trkintdir, trklength arrays */
  for (i=0; i<NPLN; i++) {
    memcpy(&trkint[i],&emptyint,sizeof emptyint);
    memcpy(&trkintdir[i],&emptydir, sizeof emptydir);
    trklength[i] = emptylen;
  }

  get_tber_info(tber, slinfo,&sector,&trk_in_sect,&trk_in_hbtr);
  alpha = 0; /*assumes sector coordinate system*/
  lambda = tber->lambda;
  phi = tber->phi;
  sl = sin(lambda);
  cl = cos(lambda);
  sp = sin(phi);
  cp = cos(phi);
  sa = sin(alpha);
  ca = cos(alpha);
  p = fabs(1/((tber)->q_over_p));
  q = (tber)->q_over_p * p;

  position.x = -tber->z0 * sa;
  position.y = tber->z0 * ca;
  position.z = -1 * tber->d0 / sin(phi);
  momentum.x = p*(cl*sp*ca-sl*sa);
             /* p*(cos(lambda)*sin(phi)*cos(alpha)-sin(lambda)*sin(alpha)*/
  momentum.y = p*(cl*sp*sa+sl*ca);
             /* p*(cos(lambda)*sin(phi)*sin(alpha)+sin(lambda)*cos(alpha)*/
  momentum.z = p*cl*cp;
             /* p*cos(lambda)*cos(phi) */

  /*need to be in tracking coord not sector coord*/
  vins[0]=position.z;
  vins[1]=position.x;
  vins[2]=position.y;
  vins[3]=momentum.z/p;
  vins[4]=momentum.x/p;
  vins[5]=momentum.y/p;
  vins[6]=p;
  vins[7]=0;
  vins[8]=q;
  
  memcpy(vsav,vins,9*(sizeof(float)));

  lmax=200;
  stepnumber=1;

  for(plane=0; plane < NPLN; plane++){
    /* fprintf(stderr,"pln: %d, vins (%8.4f %8.4f %8.4f %8.4f %8.4f %8.4f"
     *   " %8.4f %8.4f %8.4f)\n",plane,vins[0],vins[1],vins[2],vins[3],vins[4],
     *   vins[5],vins[6],vins[7]);
     */
    memcpy(pln,sdaswim_.sda_pln[sector-1][plane],8*(sizeof(float)));
    memset(tintp,0,7*(sizeof(float)));
    oldstepnumber=stepnumber;

    /* before DC's */
    if(plane < NDC_MIN) {

      /* the planes in here are very close and can overlap, so restart
       * the swimming each time here.
       */
      stepnumber = 1;
      trk_length = 0.;
      memcpy(vins,vsav,9*(sizeof(float)));
      pln[6]=0; /*set distance to 0 so trk_swim2pln assumes plane*/
      trk_swim2pln_(pln,vins,vect,tintp,&sint,&zangle,
		    &minstep,&maxstep,&lmax,&l_int,&stepnumber);

      /* in DC's */
    }
    else if (plane <= NDC_MAX) {
      pln[7]=0; /*set distance to 0 so trk_swim2pln assumes cylinder*/
      
      if(plane<NDC_MIN+12) {        /*in region 1*/
	
	/* these layers don't exist in Region 1 */
	if( (plane==(5-1+NDC_MIN)) || (plane==(6-1+NDC_MIN)) ) continue;
	
	/*note that this is pln(8) in fortran code*/
	trk_swim2pln_(pln,vins,vect,tintp,&sint,&zangle,
		      &minstep,&maxstep,&lmax,&l_int,&stepnumber);
	if(tintp[0] < 0 || l_int==0){      /*in planer section of region 1*/
	  /* Note that 0 is only an approximation which should in principle be 
	   * a value gotten from the dc geometry routines
	   */
	  pln[7]=sdaswim_.sda_pln[sector-1][plane][7];
	  pln[6]=0; /*set distance to 0 so trk_swim2pln assumes plane*/
	  trk_swim2pln_(pln,vins,vect,tintp,&sint,&zangle,
			&minstep,&maxstep,&lmax,&l_int,&oldstepnumber);
	  stepnumber=oldstepnumber;
	}	
      }
      else{                        /* Region 2 and 3 (cylindrical) */
	trk_swim2pln_(pln,vins,vect,tintp,&sint,&zangle,
		      &minstep,&maxstep,&lmax,&l_int,&stepnumber);
      }
    }
    /* in CC's */
    else if (plane <= NCC_MAX) {
      pln[7]=0; /*set distance to 0 so trk_swim2pln assumes cylinder*/
      trk_swim2pln_(pln,vins,vect,tintp,&sint,&zangle,
		    &minstep,&maxstep,&lmax,&l_int,&stepnumber);
    }
    /* in SC's or EC's */
    /* you have to be careful with these planes, since it is easy to
     * swim past the higher number (such as EC) looking for a hit
     * in one of the "wrong" SC planes.
     * We have to restart at 1 because of the way the magnetic field
     * information is remembered in trk_swim2pln .
     */
    else if (plane <= NSC_MAX) {
      lmax=900;
      stepnumber = 1;
      trk_length = 0.;
      memcpy(vins,vsav,9*(sizeof(float)));
      pln[6]=0; /*set distance to 0 so trk_swim2pln assumes plane*/
      trk_swim2pln_(pln,vins,vect,tintp,&sint,&zangle,
		    &minstep,&maxstep,&lmax,&l_int,&stepnumber);
    }
    /* in EC's */
    else if (plane <= NEC_MAX) {
      lmax=900;
      stepnumber = 1;
      trk_length = 0.;
      memcpy(vins,vsav,9*(sizeof(float)));
      pln[6]=0; /*set distance to 0 so trk_swim2pln assumes plane*/
      trk_swim2pln_(pln,vins,vect,tintp,&sint,&zangle,
		    &minstep,&maxstep,&lmax,&l_int,&stepnumber);
    }
    else {
      fprintf(stderr,"trk_swim.c: ERROR  plane = %d too large!\n",plane);
    }
    
    if(l_int){
      trk_length+=tintp[6];

      /* there are sets of planes where only 1 can be hit. If we are
       * in this set, then compare track lengths with the previous
       * hit in this set and keep only the hit with the shorter path
       * length.
       */

      /* can only be one hit between the two start counter planes */
      if (plane == NST_MAX) {
	if (trk_length < trklength[plane-1]) {
	  /* keep this plane */
	  trklength[plane]=trk_length;

	  /*record intersection point*/
	  memcpy(&(trkint[plane]),tintp,3*(sizeof(float)));
	  /*record intersection direction*/
	  memcpy(&(trkintdir[plane]),tintp+3,3*(sizeof(float)));

	  /* throw out old plane */
	  trklength[plane-1] = emptylen;
	  memcpy(&(trkint[plane-1]),&emptyint, sizeof emptyint);
	  memcpy(&(trkintdir[plane-1]),&emptydir, sizeof emptydir);
	}
      }
      /* can only be one good hit in the SC planes, compare pairwise
       * since hits must be in neighboring panels
       * 
       * Let's keep the neighboring TWO planes when a track goes through
       * the boundaries of the SC planes, this is consistent with tracking
       */
      else if ( (plane >= (NSC_MIN+1)) && (plane <= NSC_MAX) ) {
	if (trk_length < trklength[plane-1] && fabs(trk_length - trklength[plane-1]) > DELTA_L) {
	  /* keep this plane */
	  trklength[plane]=trk_length;
	  
	  /*record intersection point*/
	  memcpy(&(trkint[plane]),tintp,3*(sizeof(float)));
	  /*record intersection direction*/
	  memcpy(&(trkintdir[plane]),tintp+3,3*(sizeof(float)));

	  /* throw out old plane */
	  trklength[plane-1] = emptylen;
	  memcpy(&(trkint[plane-1]),&emptyint, sizeof emptyint);
	  memcpy(&(trkintdir[plane-1]),&emptydir, sizeof emptydir);
	}else{/*track goes through both planes, keep both*/
	  trklength[plane]=trk_length;

	  /*record intersection point*/
	  memcpy(&(trkint[plane]),tintp,3*(sizeof(float)));
	  /*record intersection direction*/
	  memcpy(&(trkintdir[plane]),tintp+3,3*(sizeof(float)));
	}
      }
      /* also, only either the EC or LAEC can be hit, so... */
      else if ( plane == NEC_MAX ) {
	if (trk_length < trklength[plane-1] && fabs(trk_length - trklength[plane-1]) > DELTA_L) {
	  /* keep this plane */
	  trklength[plane]=trk_length;

	  /*record intersection point*/
	  memcpy(&(trkint[plane]),tintp,3*(sizeof(float)));
	  /*record intersection direction*/
	  memcpy(&(trkintdir[plane]),tintp+3,3*(sizeof(float)));

	  /* throw out old plane */
	  trklength[plane-1] = emptylen;
	  memcpy(&(trkint[plane-1]),&emptyint, sizeof emptyint);
	  memcpy(&(trkintdir[plane-1]),&emptydir, sizeof emptydir);
	}else{
	  trklength[plane]=trk_length;
	  
	  /*record intersection point*/
	  memcpy(&(trkint[plane]),tintp,3*(sizeof(float)));
	  /*record intersection direction*/
	  memcpy(&(trkintdir[plane]),tintp+3,3*(sizeof(float)));
	}
      }
      /* otherwise, no comparison must be made, just keep the hits */
      else {
	  trklength[plane]=trk_length;

	  /*record intersection point*/
	  memcpy(&(trkint[plane]),tintp,3*(sizeof(float)));
	  /*record intersection direction*/
	  memcpy(&(trkintdir[plane]),tintp+3,3*(sizeof(float)));
      }

 /*set length to that of last step before intercept*/
      trk_length-=sint;
 /*set next step to last step before intersection point*/
      memcpy(vins,vect,9*(sizeof(float)));
    }
    else{
      /*      return(0); */  /* no reason to panic unnecessarily for no hit */
    }
  }
  return(1);
}






