/* tools that are useful for working with the geometry */
#include "dcg_tools.h"
#include "fmatrix.h"

/**********************************/
int intoDGEO(float xt[3], float st[3], float xpos[3][18], float sdir[3][18]) {
  /* copies the offset parameters into the DGEO bank */

  clasDGEO_t *DGEO=NULL;

  int sector;

  for (sector = 1; sector <= 6; sector++) {
    if ( (DGEO = makeBank(&wcs_,"DGEO",sector,sizeof(dgeo_t)/sizeof(int),3)) ) {
      int region;
      int isec = sector - 1;
      int ind;

      for (region=0; region < 3; region++) {
	ind = DC_INDEX(isec,region);
	DGEO->dgeo[region].id_sec = sector;
	DGEO->dgeo[region].id_reg = region+1;
	DGEO->dgeo[region].xpos = xpos[0][ind];
	DGEO->dgeo[region].ypos = xpos[1][ind];
	DGEO->dgeo[region].zpos = xpos[2][ind];
	DGEO->dgeo[region].sxpos = sdir[0][ind];
	DGEO->dgeo[region].sypos = sdir[1][ind];
	DGEO->dgeo[region].szpos = sdir[2][ind];
      }
    } else {
      fprintf(stderr,"Error: Could not make DGEO bank!\n");
      exit(1);
    }
  }

  /* make the torus entry into the bank */
  sector = 7;
  if ( (DGEO = makeBank(&wcs_,"DGEO",sector,sizeof(dgeo_t)/sizeof(int),1)) ) {
    DGEO->dgeo[0].id_sec = 0;
    DGEO->dgeo[0].id_reg = 0;
    DGEO->dgeo[0].xpos = xt[0];
    DGEO->dgeo[0].ypos = xt[1];
    DGEO->dgeo[0].zpos = xt[2];
    DGEO->dgeo[0].sxpos = st[0];
    DGEO->dgeo[0].sypos = st[1];
    DGEO->dgeo[0].szpos = st[2];
  } else {
    fprintf(stderr,"Error: Could not make DGEO bank!\n");
    exit(1);
  }
  return -1;
}


void R1TransRot(double dxtotR1[3], double dstotR1[3],
		double dxtot[6][3], double dstot[6][3]) {
  /* takes the parameters for R1S1 (in SCS) from dxtotR1 (shifts)
   * and dstotR1 (rotations) and moves them into the SCS for each sector.
   */

  static int first = -1;
  static double cossec[6];
  static double sinsec[6];
  double R60[3][3] = { {1.,0.,0.}, {0.,1.,0.}, {0.,0.,1.} };  /* rotate the sector */
  double iR60[3][3] = { {1.,0.,0.}, {0.,1.,0.}, {0.,0.,1.} }; /* inverse of R60 */
  double tmp1[3][3];
  double tmp2[3][3];
  double rot[3][3];

  double pi3;

  int s;

  if (first) {
    first = 0;
    pi3 = acos(-1.)/3.;
    for (s=0; s<6; s++) {
      cossec[s] = cos(s*pi3);
      sinsec[s] = sin(s*pi3);
    }
  }

  /* move shifts to the sector */
  for (s=0; s<6; s++) {
    dxtot[s][0] = dxtotR1[0]*cossec[s] + dxtotR1[1]*sinsec[s];
    dxtot[s][1] = dxtotR1[1]*cossec[s] - dxtotR1[0]*sinsec[s];
    dxtot[s][2] = dxtotR1[2];
  }

  /* now get the new rotation */
  make_rot(rot,dstotR1[0],dstotR1[1],dstotR1[2]);

  for (s=0; s<6; s++) {
    R60[0][0] = cossec[s];
    R60[0][1] = -sinsec[s];
    R60[1][0] = -R60[0][1];
    R60[1][1] = cossec[s];
    iR60[0][0] = R60[0][0];
    iR60[0][1] = - R60[0][1];
    iR60[1][0] = - R60[1][0];
    iR60[1][1] = R60[1][1];

    mat_mat_mult(tmp1,rot,R60);
    mat_mat_mult(tmp2,iR60,tmp1);
    get_rot(tmp2,&dstot[s][0],&dstot[s][1],&dstot[s][2]);
  }
  return;
}


void make_rot(double rot[3][3], double sx, double sy, double sz) {
  /* make the standard rotation matrix, small angle approximation */
  /* input are the direction sines of the rotations about the axes */
  rot[0][0] = 1.;
  rot[0][1] = -sz;
  rot[0][2] = sy;
  rot[1][0] = - rot[0][1];
  rot[1][1] = 1.;
  rot[1][2] = -sx;
  rot[2][0] = - rot[0][2];
  rot[2][1] = - rot[1][2];
  rot[2][2] = 1.;
  return;
}


void get_rot(double rot[3][3], double *sx, double *sy, double *sz) {
  /* take the average, since we are using a small angle approximation,
   * the matrix isn't exactly unitary and odd things can happen.
   */
  *sx = (rot[2][1]-rot[1][2])/2.;
  *sy = (rot[0][2]-rot[2][0])/2.;
  *sz = (rot[1][0]-rot[0][1])/2.;
  return;
}

void dvrot(float v1[3], double rot[3][3], float vout[3]) {
  int i,j;
  for (i=0; i<3; i++) {
    vout[i] = 0.;
    for (j = 0; j < 3; j++)
      vout[i] += rot[i][j]*v1[j];
  }
  return;
}

void mat_mat_mult(double matout[3][3], double mat1[3][3], double mat2[3][3]) {
  int i,j,k;
  for (i=0; i<3; ++i) {
    for (j=0; j<3; ++j) {
      matout[i][j] = 0.;
      for (k=0; k<3; ++k)
	matout[i][j] += mat1[i][k]*mat2[k][j];
    }
  }
  return;
}

/************************************/
void addOff(double dx[6][3],double ds[6][3],float xpos[3][18],float sdir[3][18],
	    float outx[3][18], float outs[3][18], int RegMove[3]) {

  int s,reg,i;
  int dcind;

  for (reg = 0; reg < 3; reg++) {     /* add the offset for each region */
    for (s=0; s<6; s++) {                 /* for each sector */
      dcind = DC_INDEX(s,reg);
      for (i=0; i<3; i++) {
	outx[i][dcind] = xpos[i][dcind];
	outs[i][dcind] = sdir[i][dcind];
	if (RegMove[reg]) {
	  outx[i][dcind] += dx[s][i];
	  outs[i][dcind] += ds[s][i];
	}
      }
    }
  }
  return;
}


/************************************/
void addOff_Cons(double dx[6][3],double ds[6][3],
		float xpos[3][18],float sdir[3][18],
		float outx[3][18], float outs[3][18],
		int RegMove[3]) {
  /* consistently applies offsets. the input parameters dx and ds are
   *  rotation and translation parameters from the current wire positions.
   * the output applies these, and then returns what the new "total"
   * offsets are. Done this way so that multiple regions can have the
   * same offsets applied to them with different starting offset values.
   */

  int s,reg,i;
  int dcind;

  static  float Xold[3], Xnew[3], Xmod[3], Xtmp[3];
  static  float Rold[3][3], Rnew[3][3], Rmod[3][3], iRold[3][3], Rtmp[3][3];

  for (reg = 0; reg < 3; reg++) {     /* add the offset for each region */
    for (s=0; s<6; s++) {                 /* for each sector */
      dcind = DC_INDEX(s,reg);
      if (RegMove[reg]) {
	make_frot(Rold,sdir[0][dcind],sdir[1][dcind],sdir[2][dcind]);
	make_frot(Rmod,ds[s][0],ds[s][1],ds[s][2]);
	inv_frot(iRold,Rold);
	for (i=0; i<3; i++) {
	  Xold[i] = xpos[i][dcind];
	  Xmod[i] = dx[s][i];
	}
	
  /* the new vector offsets are given by:
   *     Xnew = Xold + inv(Rold) * Xmod
   *
   * the new rotation matrix is given by:
   *     Rnew = Rmod * Rold
   */
	fmat_mult(&Rmod[0][0], &Rold[0][0], &Rnew[0][0], 3, 3, 3);
	fmat_mult(&iRold[0][0], &Xmod[0], &Xtmp[0], 3, 3, 1);
	fmat_add(&Xold[0], &Xtmp[0], &Xnew[0], 3, 1);

	/* copy angle info into temporary float */
	get_frot(Rnew, &Xtmp[0], &Xtmp[1], &Xtmp[2]);
	/* copy into our offsets */
	for (i=0; i<3; i++) {
	  outx[i][dcind] = Xnew[i];
	  outs[i][dcind] = Xtmp[i];
	}
      } else {
	for (i=0; i<3; i++) {
	  outx[i][dcind] = xpos[i][dcind];
	  outs[i][dcind] = sdir[i][dcind];
	}
      }
    }
  }
  return;
}

/************************************/
void addOff_Cons_BRot(double dx[6][3],double ds[6][3],
		float xpos[3][18],float sdir[3][18],
		float outx[3][18], float outs[3][18],
		int RegMove[3]) {
  /* rotate about a point closer to the center of the chamber (Body Centeric)
   * so that rotations and translations are more independent...
   * (ie: rotation from ds can be done without a large coincidental shift from
   *  dx)
   */

  /* consistently applies offsets. the input parameters dx and ds are
   *  rotation and translation parameters from the current wire positions.
   * the output applies these, and then returns what the new "total"
   * offsets are. Done this way so that multiple regions can have the
   * same offsets applied to them with different starting offset values.
   */

  int s,reg,i;
  int dcind;

  static  float Xold[3], Xnew[3], Xmod[3], Xtmp[3], Xtmp2[3];
  static  float Rold[3][3], iRold[3][3];
  static  float Rmod[3][3], iRmod[3][3];
  static  float Rnew[3][3], Rtmp[3][3];

  
  /* location of Body centers of rotation for the different regions
   * Chosen to be 2/3 out from the target to the maximum distance b/t layers and
   * the nominal target position.
   */

  static float Y[3][3] = { {   0., 0., 0. },    /* R1 */
			   { 122., 0., 0. },    /* R2 */
			   { 196., 0., 0. } };  /* R3 */

  static float Minus_I[3][3] = { { -1.,  0.,  0. },
				 {  0., -1.,  0. },
				 {  0.,  0., -1. } }; 

  for (reg = 0; reg < 3; reg++) {     /* add the offset for each region */
    for (s=0; s<6; s++) {                 /* for each sector */
      dcind = DC_INDEX(s,reg);
      if (RegMove[reg]) {
	make_frot(Rold,sdir[0][dcind],sdir[1][dcind],sdir[2][dcind]);
	make_frot(Rmod,ds[s][0],ds[s][1],ds[s][2]);
	inv_frot(iRold,Rold);
	for (i=0; i<3; i++) {
	  Xold[i] = xpos[i][dcind];
	  Xmod[i] = dx[s][i];
	}
	
  /* the new vector offsets are given by:
   *     Xnew = Xold + inv(Rold) * ( Xmod + ( inv(Rmod) - 1 ) Y )
   *
   * the new rotation matrix is given by:
   *     Rnew = Rmod * Rold
   */
	fmat_mult(&Rmod[0][0], &Rold[0][0], &Rnew[0][0], 3, 3, 3);

	inv_frot(iRmod,Rmod);

	fmat_add(&iRmod[0][0], &Minus_I[0][0], &Rtmp[0][0], 3, 3);
	fmat_mult(&Rtmp[0][0], &Y[reg][0], &Xtmp[0], 3, 3, 1);
	fmat_add(&Xmod[0], &Xtmp[0], &Xtmp2[0], 3, 1);
	fmat_mult(&iRold[0][0], &Xtmp2[0], &Xtmp[0], 3, 3, 1);
	fmat_add(&Xold[0], &Xtmp[0], &Xnew[0], 3, 1);

  /*
   * now for the offset due to a different origin of rotation
   */
	
	
	/* copy angle info into temporary float */
	get_frot(Rnew, &Xtmp[0], &Xtmp[1], &Xtmp[2]);
	/* copy into our offsets */
	for (i=0; i<3; i++) {
	  outx[i][dcind] = Xnew[i];
	  outs[i][dcind] = Xtmp[i];
	}
      } else {
	for (i=0; i<3; i++) {
	  outx[i][dcind] = xpos[i][dcind];
	  outs[i][dcind] = sdir[i][dcind];
	}
      }
    }
  }
  return;
}


