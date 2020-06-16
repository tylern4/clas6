/*
 *_begin_doc
 *  RCS ID string
 *  $Id: dc_geom.c,v 1.10 2000/06/02 17:27:06 feuerbac Exp $
*/
/*
 *  Documentation for DC_GEOM packages
 *
 *  Purpose:
 *  --------
 *    Set up the DC geometry, including mapmanager modifications, into the
 *    DCGM and DCGW banks. Also will store the misorientation parameters
 *    in the DGEO bank.
 *    The small mis-alignment parameters, put into DGEO, position the wires
 *    and the wire directions in the HSCS.
 * 
 *    Each sector has its own DGEO bank(# = sector). Torus has bank 7.
 *  Calling Sequence:
 *  ----------------
 *  1.  dc_make_dgeo_(int *run_number, int *force);
 *      to create the DGEO geometry offset banks with information from
 *      the mapmanager for run_number, *force = 1 to make sure to write
 *      new DGEO banks, overwriting the old if necessary.
 *  2.  dc_geom_banks_();
 *      to then fill the DCGW and DCGM banks.
 *
 *  Notes:
 *  ------
 *
 *  Author:   Robert Feuerbach      Created:  Fri Nov 14 10:31:49 EST 1997
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
 * BOS common block  uncomment the next line for BOS include file
 */
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <ntypes.h>
#include <bostypes.h>

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

static char crcsid[] = 
   "$Id: dc_geom.c,v 1.10 2000/06/02 17:27:06 feuerbac Exp $";
USE(crcsid);   /* make sure it is not optimized away */
/*
 * END of RCS information
 */

enum {MAX_SEC = 6, MAX_REG = 3, MAX_LAY = 36, MAX_WIRE = 192, NSUP = 6};

/* EXTERNAL functions */
extern int get_dc_map_geom_(int *runno, float xpos[3][6][3],
			    float spos[3][6][3], float *xt, float *st,
			    int ok[3][6]);
extern void dc_nominal_geom_(float wp[][MAX_WIRE][3],float wd[][MAX_WIRE][3],
			     float wl[][MAX_WIRE][2],float c[][3],float r[],
			     float th1[],float dth[],int minw[],int maxw[],
			     float st[],float cel[],float mdr[][3],
			     int clw[],float plsep[], float pnrm[][3],
			     float pd[]);
extern float vdot_(float v1[], float v2[], int *n);

#define RECMES(A,B,C) recmes_(A,B,C,strlen(A),strlen(B),strlen(C))

/*-------------------------------------------*/
/*----------- vtrans ------------------------*/
static void vtrans(float v1[3], float v2[3], float vout[3]) {
  vout[0] = v1[0] + v2[0];
  vout[1] = v1[1] + v2[1];
  vout[2] = v1[2] + v2[2];
  return;
}

/*-------------------------------------------*/
/*----------- vrot --------------------------*/
static void vrot(float v1[3], float rot[3][3], float vout[3]) {
  int i,j;
  for (i=0; i<3; i++) {
    vout[i] = 0.;
    for (j = 0; j < 3; j++)
      vout[i] += rot[i][j]*v1[j];
  }
  return;
}

/*---------------------------------------------------*/
/* ----------- dc_make_dgeo -------------*/
/* callable from FORTRAN
 *  Purpose: use the given run number to get offsets from the
 *           mapmanager and put them into the DGEO bank, so
 *           these offsets may be used by future calls.
 *  input: runno for MapManager, force != 0 to overwrite old DGEO bank
 *  output: DGEO bank
 */
void dc_make_dgeo_(int *runno, int *force) {
  char crname[] = "DC_MAKE_DGEO";
  float xgeom[MAX_REG][MAX_SEC][3], sgeom[MAX_REG][MAX_SEC][3];
  float xt[3],st[3];
  int ok[MAX_REG][MAX_SEC];
  char mess[80];

  clasDGEO_t *DGEO=NULL;
  
  if (DGEO = getGroup(&wcs_, "DGEO",1)) {
    /* the DGEO bank already exists. 
     * We're going to overwrite it if force is != 0.
     */
    if (*force) {
      RECMES(crname,"I","Overwriting old DGEO banks.");
    }
    else {
      RECMES(crname,"I","Using old DGEO bank.");
      return; /* don't overwrite old DGEO if we're not suppose to. */
    }
  }
  /* get parameters from mapmanger */
  sprintf(mess,"Getting DC Geometry from Run %d.",*runno);
  RECMES(crname,"I",mess);
  if (get_dc_map_geom_(runno,xgeom,sgeom,xt,st,ok)) {
    int sector;
    for (sector = 1; sector <= MAX_SEC; sector++) {
      if (DGEO = makeBank(&wcs_,"DGEO",sector,sizeof(dgeo_t)/sizeof(int),
			  MAX_REG)) {
	int region;
	int isec=sector-1;
	for(region = 0; region < MAX_REG; region++) {
	  DGEO->dgeo[region].id_sec = sector;
	  DGEO->dgeo[region].id_reg = region+1;
	  DGEO->dgeo[region].xpos = xgeom[region][isec][0];
	  DGEO->dgeo[region].ypos = xgeom[region][isec][1];
	  DGEO->dgeo[region].zpos = xgeom[region][isec][2];
	  DGEO->dgeo[region].sxpos = sgeom[region][isec][0];
	  DGEO->dgeo[region].sypos = sgeom[region][isec][1];
	  DGEO->dgeo[region].szpos = sgeom[region][isec][2];
	}
      } else {        /* problems making the bank */
	char crmess[80];
	sprintf(crmess,"Cannot create Sector %1i DGEO bank.",sector);
	RECMES(crname,"W",crmess);
      }
    }
    /* fill in the torus bank */
    sector = 7;
    if (DGEO = makeBank(&wcs_,"DGEO",sector,sizeof(dgeo_t)/sizeof(int),1)) {
      DGEO->dgeo[0].id_sec = 0;
      DGEO->dgeo[0].id_reg = 0;
      DGEO->dgeo[0].xpos = xt[0];
      DGEO->dgeo[0].ypos = xt[1];
      DGEO->dgeo[0].zpos = xt[2];
      DGEO->dgeo[0].sxpos = st[0];
      DGEO->dgeo[0].sypos = st[1];
      DGEO->dgeo[0].szpos = st[2];
    } else {
      RECMES(crname,"W","Cannot create Toroid DGEO bank.");
    }
  }
}

/*--------------------------------------------------*/
/*-------------- get_DGEO --------------------------*/
/*  inputs: sector #
 *  output: disp[region][3]     : displacement "vector" for regions
 *          rot[region][3][3]   : rotation matrix for regions
 *          ok[region][sector]  : if info was found okay. (0 = no)
 *  returns: for the sector: AND of ok[region]'s
 */
static int get_DGEO(int sector,
		    float disp[MAX_REG][3],
		    float rot[MAX_REG][3][3],
		    int ok[MAX_REG][MAX_SEC]) {
  clasDGEO_t *DGEO;
  int i;
  int isec = sector-1;
  for (i=0; i < MAX_REG; i++)
    ok[i][isec] = 0;
  if (DGEO = getGroup(&wcs_,"DGEO",sector)) {
    int ireg;
    for ( ireg=0; ireg < MAX_REG; ireg++) {
      disp[ireg][0] = DGEO->dgeo[ireg].xpos;
      disp[ireg][1] = DGEO->dgeo[ireg].ypos;
      disp[ireg][2] = DGEO->dgeo[ireg].zpos;
      rot[ireg][0][0] =  1.;
      rot[ireg][0][1] = -DGEO->dgeo[ireg].szpos;
      rot[ireg][0][2] =  DGEO->dgeo[ireg].sypos;
      rot[ireg][1][0] =  DGEO->dgeo[ireg].szpos;
      rot[ireg][1][1] =  1.;
      rot[ireg][1][2] = -DGEO->dgeo[ireg].sxpos;
      rot[ireg][2][0] = -DGEO->dgeo[ireg].sypos;
      rot[ireg][2][1] =  DGEO->dgeo[ireg].sxpos;
      rot[ireg][2][2] =  1.;
      ok[ireg][isec] = 1;
    }
  }  
  return (ok[0][isec] && ok[1][isec] && ok[2][isec]);
}

/*----------------------------------------*/
/* ----------- dc_geom_banks_ -------------*/
/* callable from FORTRAN */
/* input: DGEO bank
 * output: DCGM and DCGW  banks
 * Calculates the wire positions, directions and lengths and layer
 * dependent information, using misorientation offsets applied to the
 * nominal values.
 */
void dc_geom_banks_(void) {
  char crname[] = "DC_GEOM_BANKS";
  clasDCGM_t *DCGM=NULL;
  clasDCGW_t *DCGW=NULL;
  dcgw_t dcgw[MAX_WIRE];
  dcgm_t dcgm;
  float disp[MAX_REG][3];
  float rot[MAX_REG][3][3];
  int sector;
  int ok[3][6];
  float pi;

  /* Nominal values for geometry info */
  float nwp[MAX_LAY][MAX_WIRE][3], nwd[MAX_LAY][MAX_WIRE][3];
  float nwl[MAX_LAY][MAX_WIRE][2], ncnt[MAX_LAY][3], nr[MAX_LAY];
  float nth1[MAX_LAY], ndth[MAX_LAY], nster[MAX_LAY], ncell[MAX_LAY];
  float nmddr[MAX_LAY][3], nplsp[MAX_LAY], nplnrm[MAX_LAY][3], npld[MAX_LAY];
  int minw[MAX_LAY],maxw[MAX_LAY],cylw[MAX_LAY];
  /* get nominal geometry */
  dc_nominal_geom_(nwp,nwd,nwl,ncnt,nr,nth1,ndth,minw,maxw,nster,ncell,
		   nmddr,cylw,nplsp,nplnrm,npld);

  pi = acos(-1.);
  
  for(sector=1; sector <= MAX_SEC; sector++) {
    int colDCGM = sizeof(dcgm_t)/sizeof(int);
    int colDCGW = sizeof(dcgw_t)/sizeof(int);

    /* read in DGEO bank for this sector and set up displacement vectors
     * and rotation matrices. */
    if (get_DGEO(sector, disp, rot, ok) &&
	(DCGM = makeBank(&wcs_, "DCGM", sector, colDCGM, MAX_LAY)) &&
	(DCGW = makeBank(&wcs_, "DCGW", sector, colDCGW, MAX_LAY*MAX_WIRE)))
      {
	int layer;
	for (layer=0; layer < MAX_LAY; layer++) {
	  int wire;
	  int minwire = minw[layer]-1;
	  int maxwire = maxw[layer]-1;
	  int ireg = layer/(2*NSUP);
	  float vec1[3],vec2[3];
	  int three=3;
	  int layer_offset = layer*MAX_WIRE;

	  /* first do the layer dependent stuff */
	  /*     doesn't change with re-orientation */
	  dcgm.r_curve = nr[layer];
	  dcgm.min_wire = minw[layer];
	  dcgm.max_wire = maxw[layer];
	  dcgm.stereo = nster[layer];
	  dcgm.cell_size = ncell[layer];
	  dcgm.max_cylw = cylw[layer];
	  dcgm.p_sep = nplsp[layer];

	  /*      Center of curvature */
	  vtrans(ncnt[layer],disp[ireg],vec1);
	  vrot(vec1,rot[ireg],vec2);
	  dcgm.x_curve = vec2[0];
	  dcgm.y_curve = vec2[1];
	  dcgm.z_curve = vec2[2];
	  /*      the midplane normal */
	  vrot(nmddr[layer],rot[ireg],vec1);
	  dcgm.x_nmid = vec1[0];
	  dcgm.y_nmid = vec1[1];
	  dcgm.z_nmid = vec1[2];
	  /*      the plane normal */
	  vrot(nplnrm[layer],rot[ireg],vec1);
	  dcgm.x_norm = vec1[0];
	  dcgm.y_norm = vec1[1];
	  dcgm.z_norm = vec1[2];
	  /* distance from plane to origin */
	  vec2[0] = dcgm.r_curve*dcgm.x_norm + dcgm.x_curve;
	  vec2[1] = dcgm.r_curve*dcgm.y_norm + dcgm.y_curve;
	  vec2[2] = dcgm.r_curve*dcgm.z_norm + dcgm.z_curve;
	  dcgm.p_dist = vdot_(vec1,vec2,&three);
	  /*      theta1 -- taking into account rotations */
	  vec1[0] = sin(nth1[layer]);
	  vec1[1] = 0.;
	  vec1[2] = cos(nth1[layer]);
	  vrot(vec1,rot[ireg],vec2);
	  dcgm.theta_start = atan2(vec2[0],vec2[2]);
	  /*      dth  -- taking into account rotations */
	  dcgm.d_theta = ndth[layer]*dcgm.y_nmid;
	  /* Now for the individual wires */
	  for (wire=0; wire < minwire; wire++) {
	    dcgw[wire].x_mid = dcgw[wire].y_mid = dcgw[wire].z_mid = 0.;
	    dcgw[wire].x_dir = dcgw[wire].y_dir = dcgw[wire].z_dir = 0.;
	    dcgw[wire].w_len = dcgw[wire].w_len_hv = 0.;
	  }
	  for (wire = minwire; wire <= maxwire; wire++) {
	    float dist;
	    float wlp = nwl[layer][wire][0];
	    float wln = nwl[layer][wire][1];
	    vtrans(nwp[layer][wire],disp[ireg],vec1);
	    vrot(vec1,rot[ireg],vec2);
	    dcgw[wire].x_mid = vec2[0];
	    dcgw[wire].y_mid = vec2[1];
	    dcgw[wire].z_mid = vec2[2];
	    vrot(nwd[layer][wire],rot[ireg],vec1);
	    dcgw[wire].x_dir = vec1[0];
	    dcgw[wire].y_dir = vec1[1];
	    dcgw[wire].z_dir = vec1[2];
	    /* actually, these (above) are the points on the wire in
	     * the center of the chamber. To get the points in the
	     * Sector midplane, we have to move along the wire to
	     * where y=0 (definition of the Sector midplane).
	     * Adjust the wire lengths as well.
	     */
	    dist = - dcgw[wire].y_mid/dcgw[wire].y_dir; /* signed distance to move */
	    dcgw[wire].x_mid += dist*dcgw[wire].x_dir;  /* slide along the wire */
	    dcgw[wire].y_mid += dist*dcgw[wire].y_dir;  /* should end up 0. */
	    dcgw[wire].z_mid += dist*dcgw[wire].z_dir;
	    
	    wlp -= dist;        /* the wire 1/2 lengths on the 2 sides also changes */
	    wln += dist;
	    

	    /* Region 1, layers 1-12 have the STB on the opposite side */
	    /* of the sector. */
	    if (layer <= 12) {            /* Region 1 */
	      dcgw[wire].w_len = wln;
	      dcgw[wire].w_len_hv = wlp;
	    } else {                      /* Regions 2 and 3 */
	      dcgw[wire].w_len = wlp;
	      dcgw[wire].w_len_hv = wln;
	    }
	  }
	  for (wire=maxwire+1; wire < MAX_WIRE; wire++) {
	    dcgw[wire].x_mid = dcgw[wire].y_mid = dcgw[wire].z_mid = 0.;
	    dcgw[wire].x_dir = dcgw[wire].y_dir = dcgw[wire].z_dir = 0.;
	    dcgw[wire].w_len = dcgw[wire].w_len_hv = 0.;
	  }

	  /* minimum and maximum angle of wires at midplane */
	  dcgm.theta_min=atan2(dcgw[minwire].x_mid,dcgw[minwire].z_mid);
	  dcgm.theta_max=atan2(dcgw[maxwire].x_mid,dcgw[maxwire].z_mid);

	  /* Now copy into the BosBank itself  */
	  DCGM->dcgm[layer] = dcgm;
	  for (wire=0; wire < MAX_WIRE; wire++)
	    DCGW->dcgw[layer_offset+wire] = dcgw[wire];
	}
      } else {
	RECMES(crname,"E","Error: creating DCGW and DCGM Banks.");
      }
  }
}
/*
*-------------------------------------------------------------------------
*/

