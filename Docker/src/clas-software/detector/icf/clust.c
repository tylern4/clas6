/*
 *  Program: Energy and position correction of the cluster
 *
 *  Author : Rustam Niyazov
 *
 *  Date :   Summer, 2005. 
 *   
 *
 *  Modified Jan 19, 2006 - all constants are read from database now!!!
    Modified Jan 02, 2007 - dependance of the deep parameter on energy and angle is included!!!
*/

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include "clust.h"
#include "ic.h"


float stX = 1.346;//These are read from database now!!!
float stY = 1.360;
float zdist=55.;//cm 
int i,j,k;
//We use new definition of the 'deep' parameter, where
//deep is determined along particle trajectory (not the projection to Z axis).

void clusten_(float *energin,float *xin,float *yin,float *enrgyout,float *xout,float *yout){
  k=0;
  stX = recon_.xstep;
  stY = recon_.ystep;
  float energ=*energin;
  float x=*xin;
  float y=*yin;
  float radi=sqrt((x*x)/(stX*stX)+(y*y)/(stY*stY));
  float ddeep=GetDeepHCorrected(energ, radi);
  *enrgyout=GetEnergyCorrected(energ, radi);
  float xx=0;
  float yy=0;
  zdist=recon_.zt0;
  GetCoordCorrected(x, y, zdist,ddeep,&xx,&yy);
  //  printf("check: %8.6f %8.6f \n",ddeep,enrgyout);
  *xout=xx;*yout=yy;
}

float GetEnergyCorrected(float mom, float rdist){
  float corr_es=0;
  float new_m=0;
  int brang[16];
  for(l=0;l<16;l++){brang[l]=0;}
  int fire=0;
  float r0=3.25;//First bin 
  float rdist1=rdist;
  if(rdist>11.5)rdist1=11.5;
  if(rdist<3.25)rdist1=3.251;
  for(i=0;i<16;i++){
    float dr=0.25;
    if(i<3||i>9)dr=0.25;
    else if(i==3||i==9)dr=0.5;
    else dr=1.0;
    float r1=r0+dr;
    brang[i]=(rdist1>r0&&rdist1<=r1);
    if(brang[i]){
      fire=1;
      //hyperbolic function
      corr_es=recon_.encorr[i][0]*(mom-recon_.encorr[i][2])/(mom-recon_.encorr[i][2]+(recon_.encorr[i][1]/recon_.encorr[i][0]));
    }
    r0=r1; 
  }
  if(fire=1){
    new_m=mom/corr_es;//-0.002*mom;
  }
  else {new_m=mom;printf("did not fire; check R range!!! r:%5.2f\n",rdist);}
  return new_m; 
}

float GetDeepHCorrected(float mom, float rdist){
  float corr_es=0;
  float deltaD=0;
  float dD=0;
  int brang[16];
  for(l=0;l<16;l++){brang[l]=0;}
  int fire=0;
  float r0=3.25;//First bin 
  float rdist1=rdist;
  if(rdist>11.5)rdist1=11.5;
  if(rdist<3.25)rdist1=3.251;
  for(i=0;i<16;i++){
    float dr=0.25;
    if(i<3||i>9)dr=0.25;
    else if(i==3||i==9)dr=0.5;
    else dr=1.0;
    float r1=r0+dr;
    brang[i]=(rdist1>r0&&rdist1<=r1);
    if(brang[i]){
      fire=1;
      //hyperbolic function
      corr_es=recon_.encorr[i][0]*(mom-recon_.encorr[i][2])/(mom-recon_.encorr[i][2]+(recon_.encorr[i][1]/recon_.encorr[i][0]));
      float new_mom=mom/corr_es;
      dD=recon_.dcorr[i][0]*(new_mom-recon_.dcorr[i][2])/(new_mom-recon_.dcorr[i][2]+(recon_.dcorr[i][1]/recon_.dcorr[i][0]));
      //printf("check: %8.6f %8.6f %8.6f\n",recon_.dcorr[i][0],recon_.dcorr[i][1],recon_.dcorr[i][2]);
    }
    r0=r1;
  }
  if(fire=1){
    deltaD=dD;
    //    printf("%8.6f %8.6f \n",deltaD,corr_es);
  }
  else {deltaD=recon_.deep;printf("did not fire; check R range!!! r:%5.2f\n",rdist);}
  return deltaD; 
}


void GetCoordCorrected(float x, float y, float z, float d, float *x_new, float *y_new){
  float phi=atan2(y,x);
  float radi=sqrt(x*x+y*y);
  float theta=atan2(radi,z+recon_.deep);//rude estimate
  float zdeep = d*cos(theta);//projection of deep to z axis
  float zz=z+zdeep;
  
  float s=sqrt((x*x+y*y)/(x*x+y*y+zz*zz));
  float dX=-d*s*cos(phi);
  float dY=-d*s*sin(phi);
  float xx=x+dX;
  float yy=y+dY;
  *x_new=xx;*y_new=yy;
}
