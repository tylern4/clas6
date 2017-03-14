#include <stdlib.h>
#include <stdio.h>

void make_frot(float frot[3][3], float sx, float sy, float sz) {
  /* make the standard rotation matrix, small angle approximation
   * input are the direction sines of the rotations about the axes.
   */
  frot[0][0] = 1.;
  frot[0][1] = -sz;
  frot[0][2] = sy;
  frot[1][0] = - frot[0][1];
  frot[1][1] = 1.;
  frot[1][2] = -sx;
  frot[2][0] = - frot[0][2];
  frot[2][1] = - frot[1][2];
  frot[2][2] = 1.;
  return;
}

void inv_frot(float irot[3][3], float rot[3][3]) {
  /* quick and dirty inversion of a rotation matrix
   * all off-diagonal terms switch sign
   * input: rot
   * output: firot
   */
  int i,j;
  for (i=0; i<3; i++)
    for (j=0; j<3; j++)
      if (i==j)
	irot[i][j] = rot[i][j];
      else
	irot[i][j] = - rot[i][j];
  return;
}

void get_frot(float frot[3][3], float *sx, float *sy, float *sz) {
  /* returns the small angle approximation sines of rotation about axes */
  *sx = (frot[2][1]-frot[1][2])/2.;
  *sy = (frot[0][2]-frot[2][0])/2.;
  *sz = (frot[1][0]-frot[0][1])/2.;
  return;
}

void fmat_mult(float *a, float *b, float *c, int rowa, int cola, int colb) {
  /* multiplies matrix a[rowa][cola] by b[cola][colb] into c[rowa][colb] */

  int i,j,k;
  int skipa, skipb, skipc;
  skipa = cola;
  skipb = colb;
  skipc = colb;

  for (i=0; i<rowa; i++)
    for (j=0; j<colb; j++) {
      c[i*skipc+j] = 0.;
      for (k=0; k<cola; k++)
	c[i*skipc+j] += a[i*skipa+k]*b[k*skipb+j];
    }

  return;
}

void fmat_add(float *a, float *b, float *c, int rowa, int cola) {
  /* adds matrices a[rowa][cola] and b[rowa][cola] into c[rowa][cola] */

  int i,j;
  int skipa;
  skipa = cola;

  for (i=0; i<rowa; i++)
    for (j=0; j<cola; j++)
      c[i*skipa+j] = a[i*skipa+j]+b[i*skipa+j];
   

  return;
}

void fmat_factor(float factor, float *a, float *c, int rowa, int cola) {
  /* multiply elements in matrix a[rowa][cola] by factor */
  int i,j;
  int skipa = cola;

  for (i=0; i<rowa; i++)
    for (j=0; j<cola; j++)
      c[i*skipa+j] = a[i*skipa+j]*factor;

  return;
}

void printmatrix(float *a,int rowa, int cola) {
  /* print out contents of matrix a[rowa][cola] */
  int i,j;
  int skipa = cola;

  for (i=0; i<rowa; i++) {
    printf("\n  row %d: ",i);
    for (j=0; j<cola; j++)
      printf(" %f ",a[i*skipa+j]);
  }
  return;
}


