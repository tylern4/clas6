/*      routines geom_dc_c, geom_dc_new */
/*
 *_begin_doc
 *  RCS ID string
 *  $Id: geom_dc_c.c,v 1.2 1999/10/05 15:01:40 holtrop Exp $
*/
/*
 *  Documentation for subroutine GEOM_DC
 *
 *  Purpose:   Setup drift chamber and volumes.
 *  --------
 *
 *  Input Parameters: None.
 *  ----------------
 *
 *  Output Parameters: DC volumes and DCGM, DCGW banks.
 *  -----------------
 *
 *  Called from:  clas_geom.F
 *  ------------
 *
 *  Other routines:  dc/dc_make_dgeo, dc/dc_geom_banks (in dc/dc_geom.c),
 *  ---------------  dc/dc_nominal_geom.c
 *
 *  Notes:  Builds each region from the nominal geometry, then places
 *  ------  according to the offsets (in DGEO bank)
 *
 *  Author:   Robert Feuerbach      Created:  Mon Jun  1 11:01:13 EDT 1998
 *  -------
 *
 *  Major revisions:
 *  ----------------
 *     
 *
 *_end_doc
 */
/*
 *_begin_inc
 *  include files :
 *  ---------------------
 * BOS common block  uncomment the next line for BOS include file */
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <ntypes.h>
#include <bostypes.h>
#include <kinematics.h>
#include "gsim_c_common.h"
#include "geant_shapes.h"
#include "geant_commons.h"  /* Gets a the geant commons as structures. */

/*_end_inc */
/*
 *  Local pre-defined variables:
 *  ---------------------------  
 *  RCS information: 
*/

#define USE(var) static void * use_##var = (void *) &var
/* keep this all on one line so CVS can recognize this */
static char crcsid[] =
   "$Id: geom_dc_c.c,v 1.2 1999/10/05 15:01:40 holtrop Exp $";
USE(crcsid);  /* make sure it is not optimized away */

/*   Module information: */
      static char crname[] = "GEOM_DC_NEW";
      static char crauth[] = "Robert Feuerbach";

/* the real code */

#define MAX_LAY 36
#define MAX_WIRE 192
#define MAX_REG 3

#define MAX(A,B) ( ((A) > (B)) ? (A) : (B) )
#define MIN(A,B) ( ((A) < (B)) ? (A) : (B) )

/* Access to common blocks, as externals. */

extern GCFLAG_t  gcflag_;


void geom_dc_c(void);

int dc_make_chambers(void);

void DC_position_region(char *ctub_name, float ctub[], char *trd1_name, float trd1[],
			int region, int sector, float nth1[]);

/* Copied straight from dcgeom.inc file
 * -- in the file, not in the common block
 */

char *cdet_dcr[6] = { "DC1A","DC1A","DC2 ","DC2 ","DC3 ","DC3 " };
char *cdet_dcs[6] = { "DS1A","DS2A","DS3 ","DS4 ","DS5 ","DS6 " };
char *cdet_dcw[6] = { "DW1A","DW2A","DW3 ","DW4 ","DW5 ","DW6 " };
char *cdet_dch[3] = { "DC1A","DC2 ","DC3 " };
char *cdet_str[2] = { "DC1B","DC1B" };
char *cdet_sts[2] = { "DS1B","DS2B" };
char *cdet_stw[2] = { "DW1B","DW2B" };

void gsvolu(char *detector, char *vol, int medium, float volp[], int nvolp, int *loc) {

  gsvolu_(detector, vol, &medium, volp, &nvolp, loc, strlen(detector),strlen(vol));
}

void gspos(char *detector, int id, char *mother, float x0, float y0, float z0,
	   int rotm, char *only) {
  gspos_(detector, &id, mother, &x0, &y0, &z0, &rotm, only,
	 strlen(detector),strlen(mother),strlen(only));
}

void gsposp(char *detector, int id, char *mother, float x0, float y0, float z0,
	    int rotm, char *only, float volp[], int nvolp) {
  gsposp_(detector, &id, mother, &x0, &y0, &z0, &rotm, only,volp,&nvolp,
	 strlen(detector),strlen(mother),strlen(only));
}

void gsdvn(char *det, char *mother, int ndiv, int axis) {
  gsdvn_(det,mother,&ndiv,&axis,strlen(det),strlen(mother));
}

void gsrotm(int rotm, float th1, float phi1, float th2, float phi2,
	    float th3, float phi3) {
  gsrotm_(&rotm, &th1, &phi1, &th2, &phi2, &th3, &phi3);
}

void geom_dc_new_(void) {  /* callable from FORTRAN */
  geom_dc_c();
  return;
}
void geom_dc_c(void){
  /* idea: make the DC sectors according to the nominal information,
   * and then just place those volumes according to the DGEO offsets.
   * Lastly, create the DCGM and DCGW banks that contain the information
   * for the layers including the offsets.
   */

  int runno=1, force=1;  /* DGEO run number: should be from somewhere else */
  char tmpstr[80];
  runno = gcflag_.idrun;

  /* make the DGEO bank containing the offsets */
  dc_make_dgeo_(&runno, &force);
  sprintf(tmpstr,"Geometry initialized with runnumber %d\n",gcflag_.idrun); 
  recmes_(crname,"I",tmpstr,strlen(crname),1,strlen(tmpstr));   


  /* make the DCGM and DCGW banks */
  dc_geom_banks_();

  /* make DC chambers */
  if ( dc_make_chambers() != 0 ) {  /* volume names in dcvol structure */
    fprintf(stderr,"ERROR: Cannot make DC volumes\n");
    exit(1);
  }

  
  return;
}
/*
*-------------------------------------------------------------------------
*/

int dc_make_chambers(void) {
  /*  makes the DC regions according to the nominal geometry. Volume
   *  names are from the dcvol structure (ie COMMON BLOCK). 
   *
   *  This is a bit ugly because of using arrays from a fortran like
   *  routine instead of vectors.
   *
   *  I'm going to try to get around this with structures, since they
   *  are ordered the way you would expect (vs. arrays).
   *
   */

  extern DCGEOM_t dcgeom_;
  extern DCTMED_t dctmed_;
  extern GSIMCONTROL_t gsimcontrol_;

  /* for CTUB geometry, we need to have: inner_r, outer_r, lo_phi,
     hi_phi,dz, normal_zplus, normal_zminus  packed into an array*/
  float ctub_moth[11];
  float ctub_sl[11];
  float trd1_moth[4];
  float trd1_sl[4];

  float laythick[2], in_rad[2], out_rad[2];
  float phimin[2], phimax[2],dz[2];
  float flatdist[2];
  int indsl1,indsl2; 
  int sector;


  /* for dz calculation */
  float x1,y1,x2,y2;
#define DZ(r,cntx) ((y1-y2)/(x1-x2)*(r+cntx - 0.5*(x1+x2)) + 0.5*(y1+y2))

  float pi = acos(-1.);
  float rad2deg;
  float hasect = pi/6.;
  float cos30 = cos(hasect);

  int i,j;
  int reg,slay,lay,laymin,laymax;
  int sl, ivol;
  int tmed;

  int rotm;

  float try1, try2;

  /* Nominal values for geometry info */
  float nwp[MAX_LAY][MAX_WIRE][3], nwd[MAX_LAY][MAX_WIRE][3];
  float nwl[MAX_LAY][MAX_WIRE][2], ncnt[MAX_LAY][3], nr[MAX_LAY];
  float nth1[MAX_LAY], ndth[MAX_LAY], nster[MAX_LAY], ncell[MAX_LAY];
  float nmddr[MAX_LAY][3], nplsp[MAX_LAY], nplnrm[MAX_LAY][3], npld[MAX_LAY];
  int minw[MAX_LAY],maxw[MAX_LAY],cylw[MAX_LAY];
  /* get nominal geometry */
  dc_nominal_geom_(nwp,nwd,nwl,ncnt,nr,nth1,ndth,minw,maxw,nster,ncell,
                   nmddr,cylw,nplsp,nplnrm,npld);

  rad2deg = 180/pi;

  /* DEFINE the VOLUMES */
  /* first, the normal at the +z and -z ends */
  ctub_moth[5] = ctub_sl[5] = 0.;
  ctub_moth[6] = ctub_sl[6] = -sin(hasect);
  ctub_moth[7] = ctub_sl[7] = -cos(hasect);
  ctub_moth[8] = ctub_sl[8] = ctub_moth[5];
  ctub_moth[9] = ctub_sl[9] = ctub_moth[6];
  ctub_moth[10] = ctub_sl[10] = - ctub_moth[7];

  tmed = dctmed_.dc_med_dcgas;

  /* for each region, determine the boundries */
  for (reg=0; reg <=2; reg++) {
    laymin = reg*12;
    laymax = (reg+1)*12 - 1;
    laythick[0] = nr[laymin+1]-nr[laymin];  /* first SL in region */
    laythick[1] = nr[laymax]-nr[laymax-1];  /* second SL in region */

    in_rad[0] = nr[laymin] - 0.5*laythick[0];
    if (reg == 0) {       /* Region One: Stereo superlayer */
      out_rad[0] = nr[laymin+3] + 0.5*laythick[0];
    } else {
      out_rad[0] = nr[laymin+5] + 0.5*laythick[0];
    }
    in_rad[1] = nr[laymax-5] - 0.5*laythick[1];
    out_rad[1] = nr[laymax] + 0.5*laythick[1];
    ctub_moth[0] = in_rad[0] - 2.*laythick[0]; /* inner radius */
    ctub_moth[1] = out_rad[1] + 2.*laythick[1]; /* outer radius */

    /* use the wire lengths of the wires in the axial layers as a function of x (SCS)
     * to determine the dz of the chamber.
     */
    if (nster[laymin] < 0.01) { /* find the axial layer and stereo layers */
      laymax = laymin + 4;
    } else if (nster[laymax] < 0.01 ) {
      laymin = laymax - 4;
    } else { /* shouldn't be here */
      fprintf(stderr,"ERROR in geom_dc.c: couldn't find axial layer\n");
      exit(1);
    }

    /* in SCS here */
    y1 = nwl[laymax][100][1];
    y2 = nwl[laymin][10][1];
    x1 = nwp[laymax][100][0];
    x2 = nwp[laymin][10][0];
    /* dz: maximum thickness of the region */
    ctub_moth[2] = DZ(0.,ncnt[laymin][0]);
    dz[0] = DZ(0.,ncnt[laymin][0]);
    dz[1] = DZ(0.,ncnt[laymin][0]);

    /* min and maximum phi's for the region */
    /* there is an adjustment to the minimum phi for the lower-most edge
     * of the first stereo wire.
     */

    indsl1 = reg*12+3-1; indsl2 = reg*12+9-1;
    /* min phi */   /* adjustment for stereo angle and 1/2 cell to edge */
    phimin[0] = (nth1[indsl1]
		 - nwl[indsl1][5][0]*tan(nster[indsl1]/rad2deg)/nr[indsl1]/rad2deg
		 - 0.5*ndth[indsl1] )*rad2deg;
    phimin[1] = (nth1[indsl2]
		 - nwl[indsl2][5][0]*tan(nster[indsl2]/rad2deg)/nr[indsl2]/rad2deg
		 - 0.5*ndth[indsl2] )*rad2deg;

    /* guard wire */
    try1 = phimin[0]-1.*ndth[indsl1]*rad2deg;
    try2 = phimin[1]-1.*ndth[indsl2]*rad2deg;
    ctub_moth[3] = MIN(try1,try2);
    

    /* max phi */   /* stereo angle adjustment and 1/2 cell to edge */
    phimax[0] = (nth1[indsl1]
		 + nwl[indsl1][cylw[indsl1]-1][0]*tan(nster[indsl1]/rad2deg)/nr[indsl1]/rad2deg
		 + (cylw[indsl1]-1+0.5)*ndth[indsl1]) *rad2deg;
    phimax[1] = (nth1[indsl2]
		 + nwl[indsl2][cylw[indsl2]-1][0]*tan(nster[indsl2]/rad2deg)/nr[indsl2]/rad2deg
		 + (cylw[indsl2]-1+0.5)*ndth[indsl2]) *rad2deg;


    if (reg == 0) {       /* Region One -- still have flat section to go */
      ctub_moth[4] = MAX(phimax[0],phimax[1]); /* no guard wire */
      ctub_moth[4] = 90.;  /* straight section will continue from here */
    } else {              /* R2 or R3: this is the end of the chamber */
      /* guard wire */
      try1 = phimax[0]+2.*ndth[indsl1]*rad2deg;
      try2 = phimax[1]+2.*ndth[indsl2]*rad2deg;
      ctub_moth[4] = MAX(try1,try2);
    }

    /* CTUB mother ready to be made */
    gsvolu(cdet_dcr[reg*2],"CTUB",tmed,ctub_moth,11,&ivol);

    if (reg == 0) {     /* Region One has straight sections */
      /*  length of flat sections for superlayers */
      flatdist[0] = (maxw[indsl1-1]-cylw[indsl1-1]+1.)*nplsp[indsl1-1]/2.;
      flatdist[1] = (maxw[indsl2-1]-cylw[indsl2-1]+1.)*nplsp[indsl2-1]/2.;

      /* width at lower edge (dx at -dz)*/
      trd1_moth[0] = DZ(ctub_moth[0],ncnt[reg*12][0]);
      /* width at upper edge (dx at +dz)*/
      trd1_moth[1] = DZ(ctub_moth[1],ncnt[reg*12][0]);
      
      /* thickness of region (dz) */
      trd1_moth[3] = 0.5*(ctub_moth[1] - ctub_moth[0]);
      
      /* now need length */
      trd1_moth[2] = MAX(flatdist[0]+1.*nplsp[reg*12+1],flatdist[1]+1.*nplsp[reg*12+7]);
      gsvolu(cdet_str[reg*2],"TRD1",tmed,trd1_moth,4,&ivol);

    }

    /* for each sector, put the region into position */
    for(sector=1; sector<=6; sector++) {
      DC_position_region(cdet_dcr[reg*2],ctub_moth,cdet_str[0],trd1_moth,
			 reg+1,sector,nth1);
    }

    /* copy over information from the Region volume into the superlayer. */
    /* have same phi angular range for the superlayers so that we don't 
     * have any gaps in a layer. The digitization will decide if
     * a hit is in a cell or not. This is especially important for the
     * Region One connection between the cylindrical and straight portions.
     */
    for (i=0; i<11; i++) {
      ctub_sl[i] = ctub_moth[i];
    }

    /* put CTUB superlayers inside mother volume */
    /*    only the radii and dz length differ between the superlayer, layer,
     *    and region CTUB volumes.  Added in SL appropriate min/max phi */

    for (sl=0; sl<=1; sl++) {
      int slid = reg*2+sl+1;
      ctub_sl[0] = in_rad[sl];
      ctub_sl[1] = out_rad[sl];
      ctub_sl[2] = dz[sl];
      ctub_sl[3] = MAX(phimin[sl],ctub_moth[3]);
      ctub_sl[4] = MIN(phimax[sl],ctub_moth[4]);
      
      gsvolu(cdet_dcs[slid-1],"CTUB",tmed,ctub_sl,11,&ivol);
      gspos(cdet_dcs[slid-1],slid,cdet_dcr[reg*2],0.,0.,0.,0,"ONLY");
      
      /* now put CTUB layers inside superlayer. Can't divide CTUB by R (arg!) */
      gsvolu(cdet_dcw[slid-1],"CTUB",tmed,ctub_sl,0,&ivol);

      for (lay = 0; lay <6; lay++) {
	int indlay = reg*12+sl*6+lay;
	int lid = lay+1;
	if (slid == 1 && lid > 4)    /* Region 1, SL 1 has only 4 layers */
	  continue;
	ctub_sl[0] = nr[indlay] - ncell[indlay]*cos30;  /* layer thickness */
	ctub_sl[1] = ctub_sl[0] + 2*ncell[indlay]*cos30;
	ctub_sl[2] = DZ(0.,ncnt[indlay][0]);
	gsposp(cdet_dcw[slid-1],lid,cdet_dcs[slid-1],0.,0.,0.,0,"ONLY",ctub_sl,11);
      }
    } /* finished with superlayers in CTUB */


    if ( reg == 0 ) {  /* Region 1 -- finish the flat/straight section */

      rotm = get_rotm_();

      /* superlayers inside the straight section */
      for (sl=0; sl <= 1; sl++) {
	int nlay;
	int slid = sl+1;
	float angle;
	float trd1_lay[4];
	float trd1_pos[3];
	int layer = reg*12+sl*6;  /* layer number starting at 0 (not 1) */

	float x,y,z,th1,phi1,th2,phi2,th3,phi3;

	/* dx -- width -- at -dz */
	trd1_lay[0] = DZ(in_rad[sl],ncnt[layer][0]); 
	if (sl == 0) {  /* R1 stereo layer : only 4 layers */   
	  nlay = 4;
	} else {                   /* full six layers */
	  nlay = 6;
	}

	/* dx -- width -- at +dz */
	trd1_lay[1] = DZ(out_rad[sl],ncnt[layer+nlay-1][0]);

	/* dz --  thickness */
	trd1_lay[3] = 0.5*(out_rad[sl]-in_rad[sl]);

	/* dy --  length */
	trd1_lay[2] = flatdist[sl];                              
	
	gsvolu(cdet_sts[sl],"TRD1",tmed,trd1_lay,4,&ivol);

	/* now, position the flat SL section inside the mother flat volume */

	/* get the angle the superlayers have wrt the region flat section
	 * from nplnrm.
	 */

	/* nplnrm in SCS, so looks like rotation about y */
	angle = atan2(nplnrm[layer][2],nplnrm[layer][0])*rad2deg;

	/* rotation is about x for the trd1 shape */
	gsrotm(rotm,90,0,90-angle,90,angle,0);
	
	/* position of the center in trd1_moth reference frame */
	trd1_pos[0] = 0.;  /* x -position of the center */
	trd1_pos[1] = -trd1_moth[2] + trd1_lay[2]*cos(angle/rad2deg); /* y-pos */
	
	if (sl == 0) {
	  /* first SL, measure it off from the bottom of the region */
	  trd1_pos[2] = -trd1_moth[3] + (trd1_lay[3] + trd1_lay[2]*sin(angle/rad2deg))
	    + (in_rad[sl]-ctub_moth[0]);

	} else {
	  /* last SL, measure off from the top of the region */
	  trd1_pos[2] = trd1_moth[3] - (trd1_lay[3] + trd1_lay[2]*sin(angle/rad2deg))
	    - (ctub_moth[1]-out_rad[sl]);
	}
	
	gspos(cdet_sts[sl], slid, cdet_str[sl],
	       trd1_pos[0],trd1_pos[1],trd1_pos[2],rotm,"ONLY");

	/* now divide the volume to make the individual layers */
	gsdvn(cdet_stw[sl], cdet_sts[sl], nlay, 3);
      }
    }
  }
  return 0;
}

void DC_position_region(char *ctub_name, float ctub[], char *trd1_name, float trd1[],
			int region, int sector, float nth1[]) {

  /* position the mother volumes for the regions, according to the
   * offsets used. Get the orientation and position from the DCGM bank.
   */

  clasDCGM_t *DCGM = getGroup(&wcs_,"DCGM",sector);
  dcgm_t dcgm;

  int layer;
  int rotm = 0;

  static float PiBy3;
  static float rad2deg;
  float cossect, sinsect;
  float angle;
  float x0, y0, z0, phi[3], theta[3];  /* placement parameters */
  
  vector3_t cc;           /* center of curvature for CTUB volume in DCS */
  vector3_t trd1_c;       /* trd1 center in DCS */
  vector3_t ctub_axes[3]; /* axes'of the ctub in the DCS */
  vector3_t trd_axes[3];  /* axes of the straight section in the DCS */
  vector3_t tmp1,tmp;
  int i;
  

  if (DCGM == NULL) { /* we have a problem */
    fprintf(stderr,"ERROR: geom_dc Cannot make DC volumes -- quitting.\n");
    exit(1);
  }

  if (PiBy3 == 0) { /* first time through */
    PiBy3 = acos(-1.)/3.;
    rad2deg = 180./acos(-1.);
  }

  layer = (region-1)*12 + 1;  /* token layer to get info for region positioning */

  dcgm = DCGM->dcgm[layer-1];

  /* the X0, Y0, Z0 positioning is in the DCS, which DCGM is in the SCS,
   *   so I must rotate the center of curvature into the DCS.
   */
  cossect = cos(PiBy3*(sector-1));
  sinsect = sin(PiBy3*(sector-1));

  cc.x = dcgm.x_curve*cossect - dcgm.y_curve*sinsect;
  cc.y = dcgm.x_curve*sinsect + dcgm.y_curve*cossect;
  cc.z = dcgm.z_curve;

  /* midplane normal of chamber in DCS -- this is actually the ctub z-axis */
  ctub_axes[2].x = dcgm.x_nmid*cossect - dcgm.y_nmid*sinsect;
  ctub_axes[2].y = dcgm.x_nmid*sinsect + dcgm.y_nmid*cossect;
  ctub_axes[2].z = dcgm.z_nmid;
  ctub_axes[2] = v3norm(ctub_axes[2]);

  /* x-axis of the ctub in the DCS -- has to be calculated from angle of first wire */
  tmp1.x = sin(dcgm.theta_start - nth1[layer-1]);
  tmp1.y = 0.;
  tmp1.z = cos(dcgm.theta_start - nth1[layer-1]);

  tmp.x = tmp1.x*cossect - tmp1.y*sinsect;
  tmp.y = tmp1.y*sinsect + tmp1.x*cossect;
  tmp.z = tmp1.z;

  /* make sure it is orthogonal to z */
  ctub_axes[0] = v3norm(v3sub(tmp,v3mult(v3dot(tmp,ctub_axes[2]),ctub_axes[2])));

  /* and I'd really hope I can get the y-axis from Z x X */
  ctub_axes[1] = v3norm(v3cross(ctub_axes[2],ctub_axes[0]));

  /* get the phi and theta angles (in degrees) for gsrotm */
  for (i=0; i < 3; i++) {
    theta[i] = acos(ctub_axes[i].z)*rad2deg;
    phi[i] = atan2(ctub_axes[i].y,ctub_axes[i].x)*rad2deg;
  }

  /* so now, place the CTUB section */
  rotm = get_rotm_();

  gsrotm(rotm,theta[0],phi[0],theta[1],phi[1],theta[2],phi[2]);

  gspos(ctub_name, sector, "CLAS", cc.x, cc.y, cc.z, rotm, "ONLY");

  
  /* if in Region 1, also must place the straight section */
  if (region == 1) {
    /* the straight section axes are the same as those for the CTUB, except:
     *   TRD1x = -CTUBz
     *   TRD1y = -CTUBx
     *   TRD1z =  CTUBy
     */

    trd_axes[0] = v3norm(v3mult(-1.,ctub_axes[2]));
    trd_axes[1] = v3norm(v3mult(-1.,ctub_axes[0]));
    trd_axes[2] = v3norm(ctub_axes[1]);

    /* get the phi and theta angles (in degrees) for gsrotm */
    for (i=0; i < 3; i++) {
      theta[i] = acos(trd_axes[i].z)*rad2deg;
      phi[i] = atan2(trd_axes[i].y,trd_axes[i].x)*rad2deg;
    }

    rotm = get_rotm_();

    gsrotm(rotm,theta[0],phi[0],theta[1],phi[1],theta[2],phi[2]);

    /* calculate the position of the center of the trd1 volume.
     * Fortunately, the straight section starts just at 90. (for the
     * mother volume), so the position can just be calculated from
     * Rin*TRDz + TRDy*TRDdy + TRDz*TRDdz wrt Center of curvature.
     */

    tmp = v3mult(ctub[0],trd_axes[2]);
    
    /* now, add on the offsets to the center of the trd1 */
    tmp1 = v3add(tmp,
		 v3add( v3mult(trd1[2],trd_axes[1]), v3mult(trd1[3],trd_axes[2]))
		 );

    /* finally, add on the location of the center of curvature to get the
     * trd1 center in the DCS.
     */

    trd1_c = v3add(tmp1, cc);

    gspos(trd1_name, sector, "CLAS", trd1_c.x, trd1_c.y, trd1_c.z, rotm, "ONLY");
  }

  return;
}
   



