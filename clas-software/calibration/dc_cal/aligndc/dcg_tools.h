#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <ntypes.h>
#include <bostypes.h>

#define DC_INDEX(S,L) ((L)*6 + (S))

int intoDGEO(float xt[3], float st[3], float xpos[3][18], float sdir[3][18]);
  /* copies the offset parameters into the DGEO bank */

void R1TransRot(double dxtotR1[3], double dstotR1[3],
		double dxtot[6][3], double dstot[6][3]);
  /* takes the parameters for R1S1 (in SCS) from dxtotR1 (shifts)
   * and dstotR1 (rotations) and moves them into the SCS for each sector.
   */

void make_rot(double rot[3][3], double sx, double sy, double sz);
  /* make the standard rotation matrix, small angle approximation */
  /* input are the direction sines of the rotations about the axes */

void get_rot(double rot[3][3], double *sx, double *sy, double *sz);

void dvrot(float v1[3], double rot[3][3], float vout[3]);

void mat_mat_mult(double matout[3][3], double mat1[3][3], double mat2[3][3]);

void addOff(double dx[6][3],double ds[6][3],
	    float xpos[3][18],float sdir[3][18],
	    float outx[3][18], float outs[3][18], int RegMove[3]);

void addOff_Cons(double dx[6][3],double ds[6][3],
		float xpos[3][18],float sdir[3][18],
		float outx[3][18], float outs[3][18],
		int RegMove[3]);
  /* consistently applies offsets. the input parameters dx and ds are
   *  rotation and translation parameters from the current wire positions.
   * the output applies these, and then returns what the new "total"
   * offsets are. Done this way so that multiple regions can have the
   * same offsets applied to them with different starting offset values.
   */

void addOff_Cons_BRot(double dx[6][3],double ds[6][3],
		float xpos[3][18],float sdir[3][18],
		float outx[3][18], float outs[3][18],
		int RegMove[3]);
  /* like addOff_Cons, but does rotations about a point closer to the Body center
   * of the chamber (for ds). It does this to make dx and ds more independent.
   */
