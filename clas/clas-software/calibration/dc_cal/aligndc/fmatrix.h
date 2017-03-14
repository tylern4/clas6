/* fmatrix.h
 * Rob Feuerbach, July 1998
 */

/* prototype definition for the float matrix routines I've built */

void make_frot(float frot[3][3], float sx, float sy, float sz);
  /* make the standard rotation matrix, small angle approximation
   * input are the direction sines of the rotations about the axes.
   */

void inv_frot(float irot[3][3], float rot[3][3]);
  /* quick and dirty inversion of a rotation matrix
   * all off-diagonal terms switch sign
   * input: rot
   * output: firot
   */

void get_frot(float frot[3][3], float *sx, float *sy, float *sz);
  /* returns the small angle approximation sines of rotation about axes */

void fmat_mult(float *a, float *b, float *c, int rowa, int cola, int colb);
  /* multiplies matrix a[rowa][cola] by b[cola][colb] into c[rowa][colb] */

void fmat_add(float *a, float *b, float *c, int rowa, int cola);
  /* adds matrices a[rowa][cola] and b[rowa][cola] into c[rowa][cola] */

void fmat_factor(float factor, float *a, float *c, int rowa, int cola);
  /* multiply elements in matrix a[rowa][cola] by factor */

void printmatrix(float *a,int rowa, int cola);
  /* print out contents of matrix a[rowa][cola] */
