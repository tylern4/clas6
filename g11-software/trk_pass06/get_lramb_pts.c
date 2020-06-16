/*
Author: David Ciarletta

Purpose: get_lramb_pts determines the set of (x,y) coordinates and sigma(y) for line fitting given the 
	wire #'s that were hit and doca values for the wires
	
Inputs: layer - wire # and doca value for current hit in segment
	dcgm - the dcgm_t structure from the DCGM bank for the current sector
	psi - local slope of track
	first - flag that this is the first point to be determined in the segment

Outputs: point - x coordinate, 2 possible y coordinates, and sigma(y) for the hit
              x  = positive out from the beamline
              y  = positive toward upstream direction
*/
#include <math.h>
#include <ntypes.h>
#include <bostypes.h>
#include <trk_common.h>
#include "tbt.h"

fit_pts get_lramb_pts(hit_info layer, dcgm_t dcgm, float psi, int *first)
{
  fit_pts point;
  static float x0;
  float phi;
  static float phi0;
  float dl;
  static float dl0;
  float yw;
  float doca;
  static int wire0;
  static int max_cylw0;

  /*hit is on planar section (SLs 1&2)*/
  if(layer.wire > dcgm.max_cylw)
    {
      /* get the angle of the wire from which we are measuring the distance */
      phi = dcgm.theta_start+dcgm.d_theta*(dcgm.max_cylw-1);

      /* NOTE: positive going upstream -- further back on detector */
      dl = ((layer.wire - dcgm.max_cylw)*dcgm.p_sep)*dcgm.y_nmid;
    }
  /*hit is on cylindrical section*/
  else
    {
      phi = (layer.wire-1)*dcgm.d_theta + dcgm.theta_start;
      dl = 0;
    }

  
  if(*first == 1)
    {
      phi0 = phi;
      dl0 = dl;
      x0 = dcgm.r_curve;
      wire0 = layer.wire;
      max_cylw0 = dcgm.max_cylw;
      *first = 0;
    }

  /* always take the phi into account:
   *    takes into account the position of max_cylw too.
   *    The difference between cos(max_cylw_phi = ~88deg) and 1
   *    is only about .01%, or about 100-140 microns. Goes away between
   *    layers.
   */

  /* distance along the plane the wire is from the reference wire */
  yw = dcgm.r_curve*(phi-phi0) + dl-dl0;


  doca = fabs(layer.doca/cos(psi));

  point.x = dcgm.r_curve - x0;
  point.sigma = fabs(layer.sigma/cos(psi));
  point.y[0] = yw - doca;
  point.y[1] = yw + doca;

  return point;
}
