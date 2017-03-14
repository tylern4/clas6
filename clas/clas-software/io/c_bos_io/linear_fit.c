/* This is a modified version of the Numerical Recipes svdfit fitting routine
 * and its support functions. The only modification is that these routines 
 * use "normal" indexing with arrays from [0] to [n-1] instead of the 
 * "Fortranized" method used by NR. Also a matrix is an NxM array, not
 * a pointers to arrays thingy.
 *
 * Full reference: Numerical Recipes in C Chapter 15.4
 * Full text is available at: http://cfatab.harvard.edu/nr/bookcpdf.html
 */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define MAX(a,b) ((a) > (b) ? (a) : (b))   /* Simplified universal MAX */
#define MIN(a,b) ((a) < (b) ? (a) : (b))   /* Simplified universal MIN */
#define SIGN(a,b) ((b) >= 0.0 ? fabs(a) : -fabs(a))
#define SQR(a) ( (a) == 0.0 ? 0.0 : (a)*(a))

#define TOL 1.0e-10    /* using doubles, this can be quite small */

void covsrt(double *covar, int ma, int ia[], int mfit);
void svbksb(double *u, double w[], double *v, int m, int n, double b[],
	    double x[]);
void svdcmp(double *a, int m, int n, double w[], double *v);
void svdvar(double *v, int ma, double w[], double *cvm);
double pythag(double a, double b);


void polynomial(double x,double *p,int np){
/*
 * To fit to a polynomial with np terms: 
 * linear_fit(x,y,sig,ndat,a,ma,u,v,w,&chisq,polynomial);
 */

  int i;
  
  p[0]=1.0;
  for(i=1;i<np;i++)p[i]=p[i-1]*x;
}

int eliminate_zeros(double *x,double *y,double *dy,int *ndat){
/* 
 * This routine eliminates data points that have a zero Sigma. These points
 * "force" the fit, which leads to non-convergence. They are usually non-physical anyway.
 * Call this routine if you are not certain the data has no "zero" sigma's
 */
  int i,j;

  if(x==NULL)return(0);

  for(i=0;i<(*ndat);i++){
    if(dy[i]<1e-9){        /* Sigma is zero, will lead to non-convergence */
      (*ndat)--;              /* This happens when there was only one entry, or TDC over/under flow. */
      for(j=i+1;j<(*ndat);j++){ /* eliminate entry. */
	y[j-1]= y[j];
	dy[j-1]= dy[j];
	x[j-1]= x[j];
      }
      i--; /* do this i again... */
    }
  }
  return(1);
}


void linear_fit(double *x, double *y, double *sig, int ndata, 
	    double *a, int ma,
	    double *u, double *v, double *w, double *chisq,
	    void (*funcs)(double, double *, int))

/*  Given a set of data points x[1..ndata],y[1..ndata] with individual */
/* standard deviations sig[1..ndata], use Chisq minimization to determine */
/* the coefficients a[1..ma] of the fit- ting function y =Sum_i a_i */
/* afunc_i(x). Here we solve the fitting equations using singular value */
/* decomposition of the ndata by ma matrix, as in x 2.6. Arrays */
/* u[1..ndata][1..ma], v[1..ma][1..ma], andw[1..ma] provide workspace on */
/* input; on output they define the singular value decomposition, and can */
/* be used to obtain the covariance matrix. The pro- gram returns values */
/* for the ma fit parameters a, and Chisq ,chisq. The user supplies a */
/* routine funcs(x,afunc,ma) that returns the ma basis functions */
/* evaluated at x = x in the array afunc[1..ma]. */

{
  int j,i;
  double wmax,tmp,thresh,sum,*b,*afunc;

  b=     calloc(ndata,sizeof(double));
  afunc= calloc(ma,sizeof(double));
  for (i=0;i<ndata;i++) { /* Accumulate coefficients of the fitting matrix. */
    (*funcs)(x[i],afunc,ma); 
    tmp=1.0/sig[i];
    for (j=0;j<ma;j++) u[i*ma+j]=afunc[j]*tmp;
    b[i]=y[i]*tmp;
  }
  
  svdcmp(u,ndata,ma,w,v); /* Singular value decomposition. */
  wmax=0.0;               /* Edit the singular values, given TOL from the */
                          /* #define statement, between here ... */
  for (j=0;j<ma;j++) 
    if (w[j] > wmax) wmax=w[j];
  thresh=TOL*wmax;
  for (j=0;j<ma;j++)
    if (w[j] < thresh){
      printf("Singular value encountered. Solution will be inaccurate.\n");
      w[j]=0.0; /* ...and here. */
    }
  svbksb(u,w,v,ndata,ma,b,a);
  *chisq=0.0; /*  Evaluate chi-square. */
  for (i=0;i<ndata;i++) {
    (*funcs)(x[i],afunc,ma);
    for (sum=0.0,j=0;j<ma;j++) sum += a[j]*afunc[j];
    *chisq += (tmp=(y[i]-sum)/sig[i],tmp*tmp);
  }
  free(afunc);
  free(b);
}


void svdcmp(double *a, int m, int n, double *w, double *v)

/* Given a matrix a[1..m][1..n], this routine computes its singular value */
/* decomposition, A = U  W  V T . The matrix U replaces a on */
/* output. The diagonal matrix of singular values W is out-put as a */
/* vector w[1..n]. The matrix V (not the transpose V T ) is output as */
/* v[1..n][1..n]. */

{
  double pythag(double a, double b);
  int flag,i,its,j,jj,k,l,nm;
  double anorm,c,f,g,h,s,scale,x,y,z,*rv1;

  rv1=calloc(n,sizeof(double));
  g=scale=anorm=0.0; /* Householder reduction to bidiagonal form. */
  for (i=0;i<n;i++) {
    l=i+1;
    rv1[i]=scale*g;
    g=s=scale=0.0;
    if (i < m) {
      for (k=0;k<m;k++) scale += fabs(a[k*n+i]);
      if (scale) {
	for (k=i;k<m;k++) {
	  a[k*n+i] /= scale;
	  s += a[k*n+i]*a[k*n+i];
	}
	f=a[i*n+i];
	g = -SIGN(sqrt(s),f);
	h=f*g-s;
	a[i*n+i]=f-g;
	for (j=l;j<n;j++) {
	  for (s=0.0,k=i;k<m;k++) s += a[k*n+i]*a[k*n+j];
	  f=s/h;
	  for (k=i;k<m;k++) a[k*n+j] += f*a[k*n+i];
	}
	for (k=i;k<m;k++) a[k*n+i] *= scale;
      }
    }
    w[i]=scale *g;
    g=s=scale=0.0;
    if(i< m && i != n-1) {
      for (k=l;k<n;k++) scale += fabs(a[i*n+k]);
      if (scale) {
	for (k=l;k<n;k++) {
	  a[i*n+k] /= scale;
	  s += a[i*n+k]*a[i*n+k];
	}
	f=a[i*n+l];
	g = -SIGN(sqrt(s),f);
	h=f*g-s;
	a[i*n+l]=f-g;
	for (k=l;k<n;k++) rv1[k]=a[i*n+k]/h;
	for (j=l;j<m;j++) {
	  for (s=0.0,k=l;k<n;k++) s += a[j*n+k]*a[i*n+k];
	  for (k=l;k<n;k++) a[j*n+k] += s*rv1[k];
	}
	for (k=l;k<n;k++) a[i*n+k] *= scale;
      }
    }
    anorm= MAX(anorm,(fabs(w[i])+fabs(rv1[i])));
  }
  for (i=n-1;i>=0;i--) { /*  Accumulation of right-hand transformations. */
    if(i < n-1) {
      if (g) {
	for (j=l;j<n;j++) /* Double division to avoid possible under ow. */
	  v[j*n+i]=(a[i*n+j]/a[i*n+l])/g;
	for (j=l;j<n;j++) {
	  for (s=0.0,k=l;k<n;k++) s += a[i*n+k]*v[k*n+j];
	  for (k=l;k<n;k++) v[k*n+j] += s*v[k*n+i];
	}
      }
      for (j=l;j<n;j++) v[i*n+j]=v[j*n+i]=0.0;
    }
    v[i*n+i]=1.0;
    g=rv1[i];
    l=i;
  }
  for (i= MIN(m,n)-1;i>=0;i--) { /* Accumulation of left-hand transformations. */
    l=i+1;
    g=w[i];
    for (j=l;j<n;j++) a[i*n+j]=0.0;
    if (g) {
      g=1.0/g;
      for (j=l;j<n;j++) {
	for (s=0.0,k=l;k<m;k++) s += a[k*n+i]*a[k*n+j];
	f=(s/a[i*n+i])*g;
	for (k=i;k<m;k++) a[k*n+j] += f*a[k*n+i];
      }
      for (j=i;j<m;j++) a[j*n+i] *= g;
    } else for (j=i;j<m;j++) a[j*n+i]=0.0;
    ++a[i*n+i];
  }
  for (k=n-1;k>=0;k--) { /* Diagonalization of the bidiagonal form: Loop over */
    /* singular values, and over allowed iterations. */
    for (its=1;its<=30;its++) {
      flag=1;
      for (l=k;l>=1;l--) { /* Test for splitting. */
	nm=l-1;            /* Note that rv1[1] is always zero. */
	if ((double)(fabs(rv1[l])+anorm) == anorm) {
	  flag=0;
	  break;
	}
	if ((double)(fabs(w[nm])+anorm) == anorm) break;
      }
      if (flag) {
	c=0.0; /* Cancellation of rv1[l], ifl>1. */
	s=1.0;
	for (i=l;i<=k;i++) {
	  f=s*rv1[i];
	  rv1[i]=c*rv1[i];
	  if ((double)(fabs(f)+anorm) == anorm) break;
	  g=w[i];
	  h=pythag(f,g);
	  w[i]=h;
	  h=1.0/h;
	  c=g*h;
	  s = -f*h;
	  for (j=0;j<m;j++) {
	    y=a[j*n+nm];
	    z=a[j*n+i];
	    a[j*n+nm]=y*c+z*s;
	    a[j*n+i]=z*c-y*s;
	  }
	}
      }
      z=w[k];
      if (l == k) { /* Convergence. */
	if (z < 0.0) { /* Singular value is made nonnegative. */
	  w[k] = -z;
	  for (j=0;j<n;j++) v[j*n+k] = -v[j*n+k];
	}
	break;
      }
      if (its == 30){
	fprintf(stderr,"no convergence in 30 svdcmp iterations");
	exit(1);
      }
      x=w[l]; /* Shiftfrom bottom 2-by-2minor. */
      nm=k-1;
      y=w[nm];
      g=rv1[nm];
      h=rv1[k];
      f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y);
      g=pythag(f,1.0);
      f=((x-z)*(x+z)+h*((y/(f+SIGN(g,f)))-h))/x;
      c=s=1.0; /* Next QR transformation: */
      for (j=l;j<=nm;j++) {
	i=j+1;
	g=rv1[i];
	y=w[i];
	h=s*g;
	g=c*g;
	z=pythag(f,h);
	rv1[j]=z;
	c=f/z;
	s=h/z;
	f=x*c+g*s;
	g = g*c-x*s;
	h=y*s;
	y *= c;
	for (jj=0;jj<n;jj++) {
	  x=v[jj*n+j];
	  z=v[jj*n+i];
	  v[jj*n+j]=x*c+z*s;
	  v[jj*n+i]=z*c-x*s;
	}
	z=pythag(f,h);
	w[j]=z; /* Rotation can be arbitrary if z = 0. */
	if (z) {
	  z=1.0/z;
	  c=f*z;
	  s=h*z;
	}
	f=c*g+s*y;
	x=c*y-s*g;
	for (jj=0;jj<m;jj++) {
	  y=a[jj*n+j];
	  z=a[jj*n+i];
	  a[jj*n+j]=y*c+z*s;
	  a[jj*n+i]=z*c-y*s;
	}
      }
      rv1[l]=0.0;
      rv1[k]=f;
      w[k]=x;
    }
  }
  free(rv1);
}

void svbksb(double *u, double *w, double *v, int m, int n, double *b, double *x)

/* Solves A  X = B for a vector X, where A is specified by the arrays */
/* u[1..m][1..n], w[1..n], v[1..n][1..n] as returned by svdcmp. m and n */
/* are the dimensions of a, and will be equal for square */
/* matrices. b[1..m] is the input right-hand side. x[1..n] is the output */
/* solution vector.  No input quantities are destroyed, so the routine */
/* may be called sequentially with different b's. */

{
  int jj,j,i;
  double s,*tmp;

  tmp=calloc(n,sizeof(double));
  for (j=0;j<n;j++) { /* Calculate U T B. */
    s=0.0;
    if (w[j]) { /* Nonzero result only if wj is nonzero. */
      for (i=0;i<m;i++) s += u[i*n+j]*b[i];
      s /= w[j]; /* This is the divide by wj . */
    }
    tmp[j]=s;
  }
  for (j=0;j<n;j++) { /* Matrix multiply by V to get answer. */
    s=0.0;
    for (jj=0;jj<n;jj++) s += v[j*n+jj]*tmp[jj];
    x[j]=s;
  }
  free(tmp);
}

double pythag(double a, double b)
/*     Computes (a 2 + b 2 ) 1=2 without destructive under ow or over ow. */
{
  double absa,absb;
  absa=fabs(a);
  absb=fabs(b);
  if (absa > absb) return absa*sqrt(1.0+SQR(absb/absa));
  else return (absb == 0.0 ? 0.0 : absb*sqrt(1.0+SQR(absa/absb)));
}

void svdvar(double *v, int ma, double *w, double *cvm)
/* To evaluate the covariance matrix cvm[1..ma][1..ma] of the fit for ma parameters obtained
   by svdfit, call this routine with matrices v[1..ma][1..ma], w[1..ma] as returned from
   svdfit. */
{
  int k,j,i;
  double sum,*wti;

  wti=calloc(ma,sizeof(double));
  for (i=0;i<ma;i++) {
    wti[i]=0.0;
    if (w[i]) wti[i]=1.0/(w[i]*w[i]);
  }
  for (i=0;i<ma;i++) { /*  Sum contributions to covariance matrix (15.4.20). */
    for (j=0;j<=i;j++) {
      for (sum=0.0,k=0;k<ma;k++) sum += v[i*ma+k]*v[j*ma+k]*wti[k];
      cvm[j*ma+i]=cvm[i*ma+j]=sum;
    }
  }
  free(wti);
}

void covsrt(double *covar, int ma, int *ia, int mfit)

/* Expand in storage the covariance matrix covar, so 
   as to take into account parameters that are being held fixed. 
   (For the latter, return zero covariances.) */
{
  int i,j,k;
  double swap;

#define SWAP(a,b) {swap=(a);(a)=(b);(b)=swap;}

  for (i=mfit;i<ma;i++)
    for (j=0;j<i;j++) covar[i*ma+j]=covar[j*ma+i]=0.0;
  
  k=mfit;
  for (j=ma-1;j>=0;j--) {
    if (ia[j]) {
      for (i=0;i<ma;i++) SWAP(covar[i*ma+k],covar[i*ma+j]);
      for (i=0;i<ma;i++) SWAP(covar[k*ma+i],covar[j*ma+i]);
      k--;
    }
  }
}
