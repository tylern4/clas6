

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>


/* return the points of intersection of two circles*/
/* function value  returned is number of points of intersection*/
/* The method is solve the two circle equations R^2= (x-xc)^2 + (y-yc)^2.
   Below, we solve first for the two values of y providing  a  solution.
   Each of those can yield two solutions for  x on each circle, but
   points on both circles can be identified and returned to the caller.*/

int circle_int(float r1, float xc1, float yc1, float r2, float xc2,  
	       float yc2, float *xx1, float *yy1, float *xx2, float *yy2)
/*      float r1, xc1, yc1,r2,  xc2,  yc2, *xx1, *yy1, *xx2, *yy2; */

{

  double aq, bq, cq, det, x1, y1, x2, y2;
  double x1a, x1b, x1c, x1d, x2a, x2b, x2c, x2d;
  double xf[8],yf[8];
  double  det1, det2, det3, det4;
  float small = 1.0e-3;/* I think we can tolerate a micron error */
  int ii,npt;

  aq= -4.0*( (xc1-xc2)*(xc1-xc2) + (yc2-yc1)*(yc2-yc1) );

  bq= 8.0*yc1*(xc1-xc2)*(xc1-xc2)- 
       4.0*(yc2-yc1)*(r2*r2 - r1*r1 - pow(xc2-xc1, 2.0)+yc1*yc1- yc2*yc2);

  cq= 4.0*pow(xc1-xc2, 2.0)*(r1*r1 - yc1*yc1) - 
    pow( (r2*r2 - r1*r1 - pow(xc2-xc1, 2.0)+yc1*yc1- yc2*yc2),2.0);

  det= bq*bq - 4.0*aq*cq;
/*   printf("circle_int given (%5.2f %5.2f %5.2f)( %5.2f %5.2f %5.2f)\n",r1,xc1,yc1,r2,xc2,yc2); */
/*   printf("  aq,bq,cq,det=%5.2f %5.2f %5.2f %5.2f\n",aq,bq,cq,det); */

  if(det <0.0)  return (0);
  
  y1=( -bq + sqrt(det)  )/(2.0*aq);
  y2=( -bq - sqrt(det)  )/(2.0*aq);

  det1= sqrt(r1*r1 - (y1-yc1)*(y1-yc1) );
  x1a= xc1 + det1;
  x1b= xc1 - det1;

  det2= sqrt(r2*r2 - (y1-yc2)*(y1-yc2) );
  x1c= xc2 + det2;
  x1d= xc2 - det2;

/*   printf("y,x,x,x,x=  %4.1f %4.1f %4.1f %4.1f %4.1f\n", */
/* 	 y1,x1a, x1b,x1c, x1d); */
  /* This is a plodding way to find the two points  that are actually on both circles 
     and not keep any duplicate solutions. There must be a more simple, elegant, clever
     way to do this, but I (hcf) don't feel very clever today. */

  /* at least two of  these should agree, but keep no duplicates.*/
  npt= 0;
  if( fabs(x1a-x1c)<small) {xf[npt]=  x1a; yf[npt]= y1;npt++;}
  if( fabs(x1a-x1d)<small) {xf[npt]=  x1a; yf[npt]= y1;npt++;}
  for(ii=0; ii<npt-1; ii++){if ( (fabs(xf[npt-1]-xf[ii])<small) && (fabs(yf[npt-1]-yf[ii])<small))
      {npt--;break;} }
  if( fabs(x1b-x1c)<small) {xf[npt]=  x1b; yf[npt]= y1;npt++;}
  for(ii=0; ii<npt-1; ii++){if ( (fabs(xf[npt-1]-xf[ii])<small) && (fabs(yf[npt-1]-yf[ii])<small))
      {npt--;break;} }
  if( fabs(x1b-x1d)<small) {xf[npt]=  x1b; yf[npt]= y1;npt++;}
  for(ii=0; ii<npt-1; ii++){if ( (fabs(xf[npt-1]-xf[ii])<small) && (fabs(yf[npt-1]-yf[ii])<small))
      {npt--;break;} }

  det3= sqrt(r1*r1 - (y2-yc1)*(y2-yc1)  );
  x2a= xc1 + det3;
  x2b= xc1 - det3;

  det4= sqrt(r2*r2 - (y2-yc2)*(y2-yc2)  );
  x2c= xc2 + det4;
  x2d= xc2 - det4;
/*   printf("y,x,x,x,x=  %4.1f %4.1f %4.1f %4.1f %4.1f\n", */
/* 	 y2,x2a, x2b,x2c, x2d); */

  /* also at least two of  these should agree, but, again, keep no duplicates */
  if( fabs(x2a-x2c)<small) {xf[npt]=  x2a; yf[npt]= y2;npt++;}
  for(ii=0; ii<npt-1; ii++){if ( (fabs(xf[npt-1]-xf[ii])<small) && (fabs(yf[npt-1]-yf[ii])<small))
      {npt--;break;} }
  if( fabs(x2a-x2d)<small) {xf[npt]=  x2a; yf[npt]= y2;npt++;}
  for(ii=0; ii<npt-1; ii++){if ( (fabs(xf[npt-1]-xf[ii])<small) && (fabs(yf[npt-1]-yf[ii])<small))
      {npt--;break;} }
  if( fabs(x2b-x2c)<small) {xf[npt]=  x2b; yf[npt]= y2;npt++;}
  for(ii=0; ii<npt-1; ii++){if ( (fabs(xf[npt-1]-xf[ii])<small) && (fabs(yf[npt-1]-yf[ii])<small))
      {npt--;break;} }
  if( fabs(x2b-x2d)<small) {xf[npt]=  x2b; yf[npt]= y2;npt++;}
  for(ii=0; ii<npt-1; ii++){if ( (fabs(xf[npt-1]-xf[ii])<small) && (fabs(yf[npt-1]-yf[ii])<small))
      {npt--;break;} }

  *xx1= xf[0]; *yy1= yf[0]; 
  if(npt>1) *xx2= xf[1]; *yy2= yf[1];

/*   if( (fabs(x1-x2)<small) && (fabs(y1-y2)<small) ) return(1); */
  return(npt);
    
}
