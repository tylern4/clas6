/*      subroutine dc_nominal_geom */
/*
 *_begin_doc
 *  RCS ID string
 *  $Id: dc_nominal_geom.c,v 1.4 1998/04/09 21:05:43 feuerbac Exp $
*/
/*
 *  Documentation for subroutine dc_nominal_geom
 *
 *  Purpose:
 *  --------
 *      To return the nominal wire positions, directions, lengths, and layer
 *   information as well (Radius, midplane normal, etc.). The results can
 *   then be modified by the misorientation parameters by the calling routine.
 *
 *  Calling Sequence:
 *  ----------------
 *
 *  Input Parameters:  (Name - Type - Meaning)
 *  ----------------
 *
 *  Output Parameters:  (Name - Type - Meaning) all in SCS coord system.
 *  -----------------
 *    wire_pos[layer][wire][3] float   Midplane wire position of the wires
 *    wire_dir[layer][wire][3] float   Direction cosines of the wires
 *    wire_len[layer][wire][2] float   Half lengths: [0] to HV end (+y),
 *                                                   [1] to Amp end (-y)
 *            Center[layer][3] float   Center of Curvature (cm)
 *                 R[layer]    float   Radius of curvature of layer (cm)
 *            PhiLog[layer]    float   Phi first logical sense wire WRT
 *                                       the  Center of Curvature (radians)
 *              dPhi[layer]    float   Angular step bt wires in cyl part WRT
 *                                       the Center of Curvature.
 *           MinWire[layer]    int     First physical sense wire
 *           MaxWire[layer]    int     Last physical sense wire
 *            Stereo[layer]    float   Approx. stereo angle (radians)
 *          CellSize[layer]    float   Cell radius for cyl. part (cm)
 *            MidDir[layer][3] float   Normal to midplane (towards Amp end)
 *           CylWire[layer]    int     Last cylindrical wire before plane
 *             PlSep[layer]    float   Separation bt s. wires in plane (cm)
 *            PlNorm[layer][3] float   Normal vector to plane
 *            PlDist[layer]    float   Distance from plane to origin (cm)
 *  Called from:  dc_make_DCG
 *  ------------
 *
 *  Other routines:  dc_vrot, dc_vtranrot
 *  ---------------
 *
 *  Notes:  The SCS coordinate system has the nominal target at the origin,
 *  ------  the z-axis along the beamline, the x-axis parallel in the midplane,
 *          y-axis ZxX = pointing up (for Sector 1) parallel to axial wires
 *          pointing towards the HV side (at least for R1).
 *
 *  Author:   Robert Feuerbach      Created:  Thu Nov 13 09:37:42 EST 1997
 *  -------
 *
 *  Major revisions:
 *  ----------------
 *     
 *
 *_end_doc
 */
/*  RCS information: 
*/
#define USE(var) static void * use_##var = (void *) &var

static char crcsid[] =
   "$Id: dc_nominal_geom.c,v 1.4 1998/04/09 21:05:43 feuerbac Exp $";
USE(crcsid);   /* make sure it is not optimized away */
/*
 *   Module information:
 */
  static char crname[] = "DC_NOMINAL_GEOM";
  static char crauth[] = "Robert Feuerbach";
/*
 *_begin_inc
 *  include files :
 *  ---------------------
 */
#include <math.h>
/* BOS common block  uncomment the next line for BOS include file */
/*#include <bos.inc> */
/* _end_inc */

#define SQR(A)   ( (A)*(A) )

enum {MAX_LAY = 36, MAX_WIRE = 192, MAX_REG = 3};
enum {NSUP = 6,     /* number of superlayers */
      NLAY = 6};    /* number of layers in a superlayer */

extern float vdot_(float a[], float b[], int *c);

void dc_nominal_geom_( float wire_pos[MAX_LAY][MAX_WIRE][3],
		      float wire_dir[MAX_LAY][MAX_WIRE][3],
		      float wire_len[MAX_LAY][MAX_WIRE][2],
		      float Center[MAX_LAY][3], float R[MAX_LAY],
		      float PhiLog[MAX_LAY], float dPhi[MAX_LAY],
		      int MinWire[MAX_LAY], int MaxWire[MAX_LAY],
		      float Stereo[MAX_LAY], float CellSize[MAX_LAY],
		      float MidDir[MAX_LAY][3],
		      int CylWire[MAX_LAY],
		      float PlSep[MAX_LAY], float PlNorm[MAX_LAY][3],
		      float PlDist[MAX_LAY])
{
  /* parameters for each superlayer, keyed on layer 3,
   *  units in degrees and cm. */

  float rsupr[NSUP][7] = 
    /*         1st and last guard.w in each Region, including staggers */ 
    /*   R      PhiMin     PhiMax     X0        Y0    #s.wires    Phi_Plane */
  { { 110.9342, 35.93450, 124.07537, -0.0003, -53.0449, 128.,    89.980 },
    { 118.3130, 33.47888, 123.61834, -0.0003, -53.0449, 140.,    90. },
    { 235.26,   25.00,    133.70,    29.3,    -67.7,    192.,   180. },
    { 251.06,   23.90,    134.30,    29.3,    -67.7,    192.,   180. },
    { 338.2993, 21.18,    138.18,    48.0,    -69.6156, 192.,   180. },
    { 364.6003, 21.18,    138.18,    48.0,    -69.6156, 192.,   180. } };

  /* (X0,Y0) correspond to the Bogdan Center (center of curvature) for the
   * chambers, where X0 is the distance of the center downstream (+) from the
   * nominal target center, Y0 is the distance toward the sector (+) from the
   * same center. This is NOT the same coordinate system as in the rest of
   * this routine.
   */
  /* the BOGDAN center (X0,Y0) for R3 was moved from (48.0,-70.0) to (48.0,-69.6156)
   * due to a different length for the wire-holder internal to the chamber than in
   * the design. The value is from Alexander Skabelin, 24 OCT 97,
   * Results of DC Survey for Region 3.
   */
  /* Also, the trumpets of R2 are shorter than the design value, which causes
   * the wires to be 1 mm closer to the beamline (at the midplane). So the
   * Bogdan Center is moved from (29.3,-67.6) to (29.3,-67.7).
   * Numbers from Liming Qin, Apr 8, 1998.
   */


  /* distance of endplate to the coil plane (in cm) */
  float coil2end[MAX_REG] = { 3.213, 11.5199, 14.97 };

  /* first physical wire number offset for Region 1 */
  int w_offset[6] = { 4, 3, 3, 2, 2, 1 };

  /* last wire number for superlayer 2 */
  int w_last_sl2[6] = { 142, 142, 142, 126, 121, 120 };

  /* mini stagger info (cm) -- by Region */
  float ministag[6] = { 0.03, 0.03, 0., 0., 0., 0. };

  /* the stereo angle (deg) */
  float ster = 6.;

  /* stereo angle correction factors */
  float stercor[MAX_REG][2] = { { -0.79599, 6.3372 },
				{  0., 0. }, { 0., 0. } };
  int ilay;   /* layer number - 1 */
  
  int three=3;
  
  float *slinfo;
  float degrad, cos30, cot30;

  /* Now for the working code: */
  degrad = acos(-1.)/180.;
  cos30 = cos(30.*degrad);
  cot30 = 1/tan(30.*degrad);

  /* process each layer  -- realize that here layer runs from 0 to 35 */
  for (ilay = 0; ilay < MAX_LAY; ilay++) {
    int ireg, isprl, ila;   /* (region, superlayer, and layer in SL) - 1 */
    float *slinfo;
    float plphi, plsep, r, dphi, stereo, phi1, center[3], phi_offset;
    float phi_stag, cospl, sinpl, stc1, stc2, spacy;
    float vec[3];
    int ncell, maxw, minw, iw, cylw;

    ireg = ilay/(2*NLAY);    /* 0 to 2 */
    isprl = ilay/(NSUP);     /* 0 to 5 */
    ila = ilay % NLAY;       /* 0 to 5 */

    slinfo = rsupr[isprl];
    /*  angle of plane wrt beamline */
    plphi = slinfo[6] * degrad;

    /* store stereo angle */
    if (isprl==0 || isprl==3 || isprl==5 ) {
      Stereo[ilay] = ster;
      stereo = Stereo[ilay]*degrad;
    } else
      stereo = Stereo[ilay] = 0.;
    
    /* find dPhi and PlSep */
    ncell = slinfo[5] + 1;   /* phimin and phimax for guard wires, so
				the number of steps is (total#wires - 1),
				or (#sensew+2) - 1 */
    if (slinfo[6] < slinfo[2]) {  /* we have a plane in the back */
      dphi = dPhi[ilay] = (plphi - slinfo[1]*degrad
		    + tan(slinfo[2]*degrad - plphi))/ncell;
      plsep = PlSep[ilay] = dphi*slinfo[0];
    } else {                 /* just a simple cylinder */
      dphi = dPhi[ilay] = (slinfo[2]-slinfo[1])*degrad/ncell;
      plsep = PlSep[ilay] = 0.;
    }
    
    /* Layer radius */
    r = R[ilay] = cos30*slinfo[0]*dphi*(ila-2) + slinfo[0];

    /* cell size */
    CellSize[ilay] = 0.5*r*dphi;

    /* Dealing with the stagger: The 3rd layer includes all staggers, and
     * since the odd and even layers are ministaggered opposite ways, if
     * the layer is even we have to double ministagger and then do the
     * normal half-cell shift.
     */

    if ( (ilay+1) % 2 == 0 )
      phi_stag = -2.*ministag[isprl]/slinfo[0] + 0.5*dphi;
    else
      phi_stag = 0.;
    
    /* take care of MinWire, MaxWire and PhiLog for each layer */
    if (isprl < 2) { /* in region 1 -- special cases */
      minw = MinWire[ilay] = w_offset[ila];
      if (isprl == 1)    /* SL2, extra special case */
	maxw = MaxWire[ilay] = w_last_sl2[ila];
      else               /* SL1 */
	maxw = MaxWire[ilay] = slinfo[5] + w_offset[2] - 1;

      /* account for reference layer not starting at sense wire #1 */
      phi_offset = dphi*(w_offset[2] - 1);
      phi1 = PhiLog[ilay] = slinfo[1]*degrad + phi_stag + dphi - phi_offset;
    } else {  /* Region 2 or 3 are much simpler */
      minw = MinWire[ilay] = 1;
      maxw = MaxWire[ilay] = slinfo[5];
      phi1 = PhiLog[ilay] = slinfo[1]*degrad + dphi + phi_stag;
    }

    /* Center of curvature */
    center[0] = Center[ilay][0] = slinfo[4];
    center[1] = Center[ilay][1] = 0.;
    center[2] = Center[ilay][2] = slinfo[3];

    /* normal to plane  -- in case of cyl, it just points back upstream */
    PlNorm[ilay][0] = sin(plphi);
    PlNorm[ilay][1] = 0.;
    PlNorm[ilay][2] = cos(plphi);
    
    /* normal to midplane, pointing at HV chamber */
    MidDir[ilay][0] = 0.;
    MidDir[ilay][1] = 1.;
    MidDir[ilay][2] = 0.;

    /* prepare for setting up the wires in each layer */
    sinpl = sin(plphi);
    cospl = cos(plphi);
    stc1 = stercor[ireg][0]*degrad;
    stc2 = stercor[ireg][1];
    spacy = coil2end[ireg]/cos30;   /* used to get position of wire end */
    
    /* calculate the position and direction of each wire in this layer */
    for (iw=0; iw < maxw; iw++) {
      float *wp, *wd, *wl;
      float phiw, x, cosph, sinph, st, sinst, cosst, ymove;
      float dxdy, dzdy, wpend[3], wnend[3];

      wp = wire_pos[ilay][iw];
      phiw = phi1 + iw*dphi;
      if (phiw <= plphi) {   /* on cylindrical section */
	
	cylw = iw+1;  /* keep track of maximum wire on cylinder */
	
	/* wire position calculation */
	cosph = cos(phiw);
	sinph = sin(phiw);
	
	wp[0] = x = r*sinph + center[0];
	wp[1] = center[1];
	wp[2] = r*cosph + center[2];

      } else {   /* on the plane section */
	cosph = cospl;
	sinph = sinpl;
	wp[0] = x = wire_pos[ilay][iw-1][0] + plsep*cosph;
	wp[1] = wire_pos[ilay][iw-1][1];
	wp[2] = wire_pos[ilay][iw-1][2] - plsep*sinph;
      }

      /* wire direction part */
      wd = wire_dir[ilay][iw];
      if (stereo < 0.0001) {     /* Axial wires */
	wd[0] = 0.;
	wd[1] = 1.;
	wd[2] = 0.;
      } else {                   /* Stereo wires */
	st = (stereo + stc1/(x-stc2));  /* corrected stereo angle */
	sinst = sin(st);
	cosst = cos(st);
	wd[0] = -cosph*sinst;
	wd[1] =  cosst;
	wd[2] =  sinph*sinst;
      }

      /* and finally, the wire lengths */
      /* slope in the xy and zy planes */
      dxdy = wd[0]/wd[1];
      dzdy = wd[2]/wd[1];

      /* the end with positive Y : HV end */
      wpend[1] = (ymove = (wp[0] - spacy*cot30)/(cot30 - dxdy)) + wp[1];
      wpend[0] = wp[0] + dxdy*ymove;
      wpend[2] = wp[2] + dzdy*ymove;

      /* the end with negative Y: Amp end */
      wnend[1] = (ymove = - (wp[0] - spacy*cot30)/(cot30 + dxdy)) + wp[1];
      wnend[0] = wp[0] + dxdy*ymove;
      wnend[2] = wp[2] + dzdy*ymove;

      /* And then the half lengths */
      wl = wire_len[ilay][iw];

      wl[0] = sqrt(   SQR(wp[0] - wpend[0]) + SQR(wp[1] - wpend[1])
		    + SQR(wp[2] - wpend[2]) );
      wl[1] = sqrt(   SQR(wp[0] - wnend[0]) + SQR(wp[1] - wnend[1])
		    + SQR(wp[2] - wnend[2]) );
    }

    /* That's all the ideal wire position calculations to make. */
    /* Now, just for some cleanup */

    CylWire[ilay] = cylw;
    /* the distance from the origin to the plane */
    vec[0] = r*sinpl + center[0];
    vec[1] = center[1];
    vec[2] = r*cospl + center[2];
    PlDist[ilay] = vdot_(vec,PlNorm[ilay],&three);
  }
  return;
}
/*
*-------------------------------------------------------------------------
*/



