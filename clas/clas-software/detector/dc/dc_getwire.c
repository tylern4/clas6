/*      subroutine dc_getwire */
/*
 *_begin_doc
 *  RCS ID string
 *  $Id: dc_getwire.c,v 1.1 1999/01/19 17:16:28 feuerbac Exp $
*/
/*
 *  Documentation for subroutine dc_getwire
 *
 *  Purpose:  
 *  --------
 *   Calculate wire # hit given sector, layer and x, y, z
 *      coordinates of the track in that layer.
 *
 *
 *  Calling Sequence:
 *  ----------------
 *
 *  Input Parameters:  (pos - vector3_t - 1x3 dimensional structure of
 *  ----------------     x, y, z coordinates as floats)
 *                     (sector - int - sector that the track is in)
 *                     (layer - int - layer that position vector falls in)
 *
 *  Output Parameters: (wire - int - wire # of cell that track passed through)
 *  -----------------
 *
 *  Called from:
 *  ------------
 *
 *  Other routines:
 *  ---------------
 *
 *  Notes:
 *  ------
 *
 *  Author:   David Ciarletta      Created:  Mon Apr  6 15:28:01 EDT 1998
 *  -------
 *
 *  Major revisions:
 *  ----------------
 *     Dec1998: R. Feuerbach
 *              modified to take into account any rotations of the chamber
 *              that would cause a phi-dependence in choosing the correct
 *              wire (like it is for stereo wires).
 *
 *_end_doc
 */
/*
 *_begin_inc
 *  include files :
 *  ---------------------
*/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "ntypes.h"
#include "bostypes.h"
/*_end_inc */
/*_begin_var
 *  input/output variables:
 *  -----------------------
 *
 *  Local pre-defined variables:
 *  ---------------------------  
 *  RCS information: 
*/
#define USE(var) static void * use_##var = (void *) &var
      static char cfile[] =  "$RCSfile: dc_getwire.c,v $";
      static char crevis[] = "$Revision: 1.1 $";
      static char cstate[] = "$State: Exp $";
      static char cdate[] =  "$Date: 1999/01/19 17:16:28 $";
      static char cautho[] = "$Author: feuerbac $";
      static char crcsid[] =
         "$Id: dc_getwire.c,v 1.1 1999/01/19 17:16:28 feuerbac Exp $";
USE(crcsid);  /* try to make sure it is not optimized away */
/*   Module information: */
      static char crname[] = "dc_getwire";
      static char crauth[] = "David Ciarletta";
/*
 *  Local User defined variables:
 *  ----------------------------- 
 *
 *_end_var
 *  executable code for routine dc_getwire:
 *  -------------------------------------
*/

#define MAX_WIRE 192
 
int dc_getwire_(vector3_t *ppos, int *psector, int *layer) {
   
  vector3_t pos;
  int ilayer;          /* stores layer as an int */
  float theta;         /* angle of track */
  float theta_c;       /* angle of track WRT center of curvature */
  float theta_pl;      /* angle of planar section WRT c.o.c. */
  int wire;            /* wire number of hit wire */
  float x_c;           /* x of track WRT c.o.c. */
  float y_c;           /* y of track WRT c.o.c. */
  float z_c;           /* z of track WRT c.o.c. */
  float dphi;          /* theta_c - theta wire (for stereo rotation in cyl.) */
  float dz;            /* pos.z - z wire (for stereo rotation in plane) */
  float x_curve;       /* x center of curvature (BANK) */
  float y_curve;       /* y center of curvature (BANK) */
  float z_curve;       /* z center of curvature (BANK) */
  float r_curve;       /* radius of curvature (BANK) */
  float theta_start;   /* angle of first logical wire WRT c.o.c. (BANK) */
  float d_theta;       /* delta theta between wires (BANK) */
  float theta_min;     /* theta of first physical wire (BANK) */
  float theta_max;     /* theta of last physical wire (BANK) */
  int min_wire;        /* minimum physical wire number (BANK) */
  int max_wire;        /* maximum physical wire number (BANK) */
  float stereo;        /* approximate stereo angle (BANK) */
  float p_dist;        /* distance of plane to origin (BANK) */
  float p_sep;         /* planar separation (BANK) */
  int max_cylw;        /* maximum cylindrical wire number (BANK) */
  float max_cylw_z;    /* z coordinate of max_cylw */
  float fkstereo;      /* approximate stereo-like angle, from looking at wires */
  
  int sector = *psector;

  clasDCGM_t *DCGM = NULL; /* pointer structure to BOS BANK */
        /* pointer structure to BOS BANK - DC wire geometry */
  clasDCGW_t *DCGW = (clasDCGW_t *)getGroup(&wcs_,"DCGW",sector);

  static float pi = -1.;

  if (pi < 0.)
    pi = acos(-1.);

  /* Get geometry values from BOS BANK, if NULL exit and return -1 */
  if( (DCGM = (clasDCGM_t *)getGroup(&wcs_,"DCGM",sector)) ) {
    
    /* store layer in ilayer */
    pos = *ppos;
    ilayer = *layer-1;
    
    x_curve = DCGM->dcgm[ilayer].x_curve;
    y_curve = DCGM->dcgm[ilayer].y_curve;
    z_curve = DCGM->dcgm[ilayer].z_curve;
    r_curve = DCGM->dcgm[ilayer].r_curve;
    theta_start = DCGM->dcgm[ilayer].theta_start;
    d_theta = DCGM->dcgm[ilayer].d_theta;
    theta_min = DCGM->dcgm[ilayer].theta_min;
    theta_max = DCGM->dcgm[ilayer].theta_max;
    min_wire = DCGM->dcgm[ilayer].min_wire;
    max_wire = DCGM->dcgm[ilayer].max_wire;
    stereo = DCGM->dcgm[ilayer].stereo;
    p_dist = DCGM->dcgm[ilayer].p_dist;
    p_sep = DCGM->dcgm[ilayer].p_sep;
    max_cylw = DCGM->dcgm[ilayer].max_cylw;
    
    /* Calculate remaining geometry values */
    theta =  atan2(pos.x,pos.z);
    x_c = pos.x - x_curve;
    y_c = pos.y - y_curve;
    z_c = pos.z - z_curve;
    theta_c =  atan2(x_c,z_c);
    theta_pl = theta_start+(max_cylw-1)*d_theta;
    max_cylw_z = z_curve + sin(pi/2.0-theta_pl)*r_curve;
    fkstereo = pi*stereo/180.;
    
    /* Calculate wire hit for cylindrical section */
    if ((theta_c <= theta_pl)&&(theta_c >= (theta_start-0.5*d_theta))) {
      wire = ((theta_c-theta_start)/d_theta+1.5);

      /* get approximate "stereo-like" angle
       * (axial layers can have it too if chambers misaligned)
       * We do not have to use the correct wire, just something close */
      if (DCGW != NULL) {
	int wir_ind;
	vector3_t wdir;
	float tmp,angle,ca,sa;

	angle = (theta_start + (wire-1)*d_theta) - pi/2.0;
	ca = cos(angle);
	sa = sin(angle);

	wir_ind = ilayer*MAX_WIRE + wire-1;
	wdir.x = DCGW->dcgw[wir_ind].x_dir;
	wdir.y = DCGW->dcgw[wir_ind].y_dir;
	wdir.z = DCGW->dcgw[wir_ind].z_dir;

	/* rotate wdir vector about y so that it is like wire is at 90deg */
	tmp = wdir.z;
	wdir.z = tmp*ca + wdir.x*sa;
	wdir.x = -tmp*sa + wdir.x*ca;

	fkstereo = atan2(wdir.z,wdir.y);
      }

      /* Compensate for stereo angle if absolute value > .001 */
      if (fabs(fkstereo) > .001) {
	/* Calculate dphi for rotation */
	dphi = theta_c-((wire-1)*d_theta+theta_start);
	wire = ((wire+0.5)+atan((y_c*sin(fkstereo)+
				 dphi*r_curve*cos(fkstereo))/r_curve)/d_theta);
      }
    }
    
    /* Calculate wire hit for planar section */
    if ((max_wire != max_cylw)&&(theta_c > theta_pl)) {
      wire = (((max_cylw_z-pos.z+0.5*p_sep)/p_sep)+max_cylw);
      
      if (DCGW != NULL) {
	int wir_ind;
	
	wir_ind = ilayer*MAX_WIRE + wire-1;
	fkstereo = atan2(DCGW->dcgw[wir_ind].z_dir,DCGW->dcgw[wir_ind].y_dir);
      }
      
      /* Compensate for stereo angle if absolute value > .001 */
      if (fabs(fkstereo) > .001) {
	/* Calculate dz for rotation */
	dz = (max_cylw_z - pos.z)-(wire-max_cylw)*p_sep;
	wire = ((wire+0.5)+(pos.y*sin(fkstereo)+dz*cos(fkstereo))/p_sep);
      }
    }
    
    /* if calculated wire is not a valid wire number return 0 */
    if (wire > max_wire || wire < min_wire) {
      wire = 0;
    }
    
  }
  else { 
    wire = -1;
  }
  
  return wire;
} 

/*
*-------------------------------------------------------------------------
*/



