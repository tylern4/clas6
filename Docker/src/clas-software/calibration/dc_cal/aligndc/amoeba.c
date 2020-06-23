/*
 * minimization method from Numerical Recipes in C
 *
 * going to try to use it to get the DC alignment info.
 * would require (in the present form) a separate alignment run for each sector
 */
#include "nrutil.h"
#include <math.h>
#include <stdio.h>
#include <setjmp.h>

#define NMAX 500

#define GET_PSUM  for (j=1; j<=ndim;j++) {\
                  for (sum=0.0,i=1;i<=mpts;i++) sum+= p[i][j];\
		  psum[j]=sum;}

#define SWAP(a,b) {swap=(a); (a)=(b); (b)=swap;}

void amoeba(int sec,float **p, double y[], int ndim, float ftol,
	    double (*funk)(int sec, int ndim, float []),int *nfunk) {
  /* straight from Numerical recipes, with the slight change to include the
   * sector number.
   * inputs:  p[1..ndim+1][1..ndim] : [pt num][vector components] of the
   *           original points.
   *          y[1..ndim+1] : [pt num]    evaluation of funk() at p[]
   *          ndim           number of dimensions to minimize in
   *          ftol           size of variation to end at
   *          (*funk)        pointer to function we're trying to minimize
   *          *nfunk         returns number of calls to funk that were made
   */
  double amotry(int sec, float **p, double y[], float psum[], int ndim,
	       double (*funk)(int sec, int ndim, float []), int ihi, float fac);
  int i, ihi, ilo, inhi, j,mpts=ndim+1;
  float sum,*psum;
  double ysave,ytry,swap,rtol;
  int sigexit = 0;

  extern jmp_buf env;

  psum = vector(1,ndim);
  *nfunk=0;
  GET_PSUM
    printf("\tENTERING AMOEBA LOOP:\n");
  if (setjmp(env) != 0)
    sigexit = -1;
  for (;;) {
    ilo=1;
    /* find the worst, next worst, and best points */
    ihi = y[1]>y[2] ? (inhi=2,1) : (inhi=1,2);
    for (i=1; i<=mpts; i++) {
      if (y[i] <= y[ilo]) ilo=i;
      if (y[i] > y[ihi]) {
	inhi=ihi;
	ihi=i;
      } else if (y[i] > y[inhi] && i != ihi) inhi=i;
    }
    rtol=0.;
    /*    for (i=1; i<=ndim; i++) */
    /*      rtol += SQR( p[ilo][i] - p[ihi][i] ); */ /* look at distance spanned */
    /*    rtol = sqrt(rtol); */
    rtol=2.*fabs(y[ihi]-y[ilo])/(fabs(y[ihi])+fabs(y[ilo]));
    /* what is the fractional change from highest to lowest? */
    if (rtol < ftol || sigexit) { /* return: put best point and value in slot 1. */
      SWAP(y[1],y[ilo])
      for (i=1;i<=ndim;i++) SWAP(p[1][i],p[ilo][i])
      break;
    }
    if (*nfunk >= NMAX) {
      SWAP(y[1],y[ilo])
      for (i=1;i<=ndim;i++) SWAP(p[1][i],p[ilo][i])
      fprintf(stderr,"NMAX exceeded\n");
      break;
    }
    *nfunk += 2;
    /* New iteration: first reflect the simplex from the highest point */
    printf("\tREFLECT FROM HIGHEST POINT.\n");
    ytry = amotry(sec,p,y,psum,ndim,funk,ihi,-1.);
    if (ytry <= y[ilo]) {
      /* better than best point of what we had, so try to expand further this way */
      printf("\tBETTER THAN BEST POINT BEFORE, EXPAND FURTHER THIS WAY.\n");
      ytry = amotry(sec,p,y,psum,ndim,funk,ihi,2.0);
    }
    else if (ytry >= y[inhi]) {
      /* worse than second worse -- maybe still be the worst, so look
       * for point a little closer to the others that is better.
       */
      ysave=y[ihi];
      printf("\tSTILL NOT GREAT: LOOK A LITTLE CLOSER TO THE OTHER POINTS\n");
      ytry=amotry(sec,p,y,psum,ndim,funk,ihi,0.5);
      if (ytry >= ysave) {   /* Still worse -- contract about the best point */
	printf("\t\tSTILL WORSE: CONTRACTING for next %i cycles.\n",ndim);
	for (i=1;i<=mpts;i++) {
	  if ( i != ilo) {
	    for (j=1;j<=ndim;j++)
	      p[i][j] = psum[j] = 0.5*(p[i][j]+p[ilo][j]);
	    y[i]=(*funk)(sec,ndim,psum);
	  }
	}
	*nfunk += ndim;
	GET_PSUM
      }
    } else --(*nfunk);
  }
  free_vector(psum,1,ndim);
}

double amotry(int sec, float **p, double y[], float psum[], int ndim,
	     double (*funk)(int sec, int ndim, float []), int ihi, float fac) {
  /* extrapolate by a factor fac through the face of the simplex across from the
   * high point, tries it, and replaces the high point if the new point is better 
   */

  int j;
  float fac1, fac2, *ptry;
  double ytry;

  ptry=vector(1,ndim);
  fac1=(1.0-fac)/ndim;
  fac2=fac1-fac;
  for (j=1;j<=ndim;j++) ptry[j]=psum[j]*fac1-p[ihi][j]*fac2;
  ytry = (*funk)(sec,ndim,ptry);
  if (ytry < y[ihi]) {        /* if its better, get rid of the old one */
    y[ihi]=ytry;
    for (j=1;j<=ndim;j++) {
      psum[j] += ptry[j]-p[ihi][j];
      p[ihi][j]=ptry[j];
    }
  }
  free_vector(ptry,1,ndim);
  return ytry;
}
