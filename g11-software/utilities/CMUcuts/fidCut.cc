#ifndef _FIDCUT_H
#define _FIDCUT_H

#include<stdio.h>
#include<stdlib.h>
#include<math.h>

#define Pi 3.14159265

/* fidCut.h
 *
 * ****************************************************
 *
 * fidCut
 *
 * 09/14/05  M. Bellis
 *
 * This snippet of code will take in the three momentum for some 
 * track in CLAS and return a 1 if it passes the fiducial cut, and 0
 * if it fails the fiducial cut. 
 *
 * pmag - magnitude of the momentum
 * cosTheta - cosine(theta) of the momentum
 * labphi - phi of the track as measured in CLAS. The code will translate phi in the lab
 *       frame to phi in a sector. 
 * sec - Sector of the track used to convert phi to a local phi measurement.
 * run - integer to represent some run, or different cuts for a given run. 
 *       0 - g1c
 *       1 - g11a, The current cuts were motivated by differences in an empirical 
 *                 efficiency calculation between data and MC which indicated discrepencies 
 *                 in the forward region at the edges in phi. There is a hard cut at phi < 0.4 
 *                 and a hard forward cut at cos(theta)<0.985.  There is a more subtle cut 
 *                 in cos(theta) and phi to cut out the edges in the forward region. Some 
 *                 other cuts are placed on backwards going tracks (cosTheta < -0.5) in all 
 *                 sectors and some tighter cosTheta cuts in sectors 1,3 and 5. Some tighter cos
 *                 cuts as momentum gets above 2.3 GeV. Nothing above 3.0 GeV.
 *       2 - g11a, but without some of the backwards cosTheta cuts
 *       3 - just cut on the edges of local phi (-0.4 - 0.4)
 *       4 - just cut on the edges of local phi (-0.3 - 0.3)
 *       5 - just cut on the edges of local phi (-0.2 - 0.2)
 *       6 - Added low momentum (<450MeV) proton cuts to 1. 
 *       7 - Added low momentum (<300MeV) proton cuts to 1. 
 * verbose - option that produces some diagnostic output on the screen.
 * phioption - 0 is assuming lab phi, 1 is assuming sector phi 
 * particle - -1 apply to any particle
 *             14 proton
 *              8 pi+
 *              9 pi-
 *
 *
 * ****************************************************
 *
 * oneSectorPhi
 *
 * 09/14/05 M. Bellis
 *
 * Convert a phi and a sector to a local phi coordinate 
 *
 * float phi - phi as measured in the lab frame of CLAS
 * int sec - sector of CLAS (1-6)
 *
 */

extern "C"{

   int fidCut (float pmag, float cosTheta, float labphi, int sec, int run, int verbose, int phioption, int particle);

   int fidcut_(float *pmag, float *cosTheta, float *labphi, int *sec, int *run, int *verbose, int *phioption, int *particle);

   float oneSectorPhi(int sec, float phi);

   float onesectorphi_(int *sec, float *phi);
}

int fidcut_(float *pmag, float *cosTheta, float *labphi, int *sec, int *run, int *verbose, int *phioption, int *particle)
{
  // fortran wrapper
  int cutvalue;
  cutvalue=fidCut(*pmag,*cosTheta,*labphi,*sec,*run,*verbose,*phioption,*particle);
  return(cutvalue);
}


float onesectorphi_(int *sec, float *phi)
{
  // fortran wrapper
  float newphi;
  newphi=oneSectorPhi(*sec,*phi);
  return(newphi);
}


int fidCut (float pmag, float cosTheta, float labphi, int sec, int run, int verbose=0, int phioption=0, int particle=-1)
{

  float phi;
  float cutparam[4];
  float cosThetacut = 2;
  float phiCut;
  float forwardcosThetacut = 2;
  int ret = 1;


  if(phioption==0) phi = oneSectorPhi(sec, labphi); 
  else          phi = labphi; 
  if(verbose>1) {
  	printf("run: %d\n",run);
  	printf("pmag: %f\t\tcostheta: %f\n",pmag,cosTheta);
  	printf("sector: %d\t\tlabphi: %f\t\tphi: %f\n",sec, labphi, phi);
  }
  	
  if (run==0) /* g1c run */
  {
    cutparam[3] = 1.87603;
    cutparam[2] = -4.54334;
    cutparam[1] = 0.022356;
    cutparam[0] = 0.993861;
    cosThetacut = cutparam[3]*pow(phi,6) + cutparam[2]*pow(phi,4) + cutparam[1]*pow(phi,2) + cutparam[0];
    phiCut = 0.4;
    forwardcosThetacut = 0.985;

    if (cosTheta < cosThetacut &&
        fabs (phi) < phiCut && 
        cosTheta < forwardcosThetacut)
    {
      ret = 1; /* Good event */
    }
    else
    {
      ret = 0; /* Bad event */
    }
  }
  else if(run==1) /* g11a run */
  {
    cutparam[3] = 1.87603;
    cutparam[2] = -4.54334;
    cutparam[1] = 0.022356;
    cutparam[0] = 0.993861;
    cosThetacut = cutparam[3]*pow(phi,6) + cutparam[2]*pow(phi,4) + cutparam[1]*pow(phi,2) + cutparam[0];
    phiCut = 0.4;
    forwardcosThetacut = 0.985;

    if (cosTheta < cosThetacut &&
        fabs (phi) < phiCut && 
        cosTheta < forwardcosThetacut &&
        cosTheta > -0.5 &&
        //pmag < 3.0 &&
        //!(particle==14 && pmag>2.3 && cosTheta>0.96) &&
        //!(particle==8 && pmag>2.3 && cosTheta>0.96) &&
        !(sec==1 && cosTheta<-0.25) &&
        !(sec==3 && cosTheta<0.15) && 
        !(sec==5 && cosTheta<0.0))
    {
      ret = 1; /* Good event */
    }
    else
    {
      ret = 0; /* Bad event */
    }
  }
  else if(run==2) /* g11a run */
  {
    cutparam[3] = 1.87603;
    cutparam[2] = -4.54334;
    cutparam[1] = 0.022356;
    cutparam[0] = 0.993861;
    cosThetacut = cutparam[3]*pow(phi,6) + cutparam[2]*pow(phi,4) + cutparam[1]*pow(phi,2) + cutparam[0];
    phiCut = 0.4;
    forwardcosThetacut = 0.985;

    if (cosTheta < cosThetacut &&
        fabs (phi) < phiCut && 
        cosTheta < forwardcosThetacut)
    {
      ret = 1; /* Good event */
    }
    else
    {
      ret = 0; /* Bad event */
    }
  }
  else if(run==3) /* Just cut on phi */
  {
    phiCut = 0.4;

    if (fabs (phi) < phiCut)
    {
      ret = 1; /* Good event */
    }
    else
    {
      ret = 0; /* Bad event */
    }
  }
  else if(run==4) /* Just cut on phi */
  {
    phiCut = 0.3;

    if (fabs (phi) < phiCut)
    {
      ret = 1; /* Good event */
    }
    else
    {
      ret = 0; /* Bad event */
    }
  }
  else if(run==5) /* Just cut on phi */
  {
    phiCut = 0.2;

    if (fabs (phi) < phiCut)
    {
      ret = 1; /* Good event */
    }
    else
    {
      ret = 0; /* Bad event */
    }
  }
  else if(run==6) /* g11a run */
  {
    cutparam[3] = 1.87603;
    cutparam[2] = -4.54334;
    cutparam[1] = 0.022356;
    cutparam[0] = 0.993861;
    cosThetacut = cutparam[3]*pow(phi,6) + cutparam[2]*pow(phi,4) + cutparam[1]*pow(phi,2) + cutparam[0];
    phiCut = 0.4;
    forwardcosThetacut = 0.985;

    if (cosTheta < cosThetacut &&
        fabs (phi) < phiCut && 
        cosTheta < forwardcosThetacut &&
        cosTheta > -0.5 &&
        pmag < 3.0 &&
        !(sec==1 && cosTheta<-0.25) &&
        !(sec==3 && cosTheta<0.15) && 
        !(sec==5 && cosTheta<0.0) &&
        !(particle==14 && pmag<0.4) &&
        !(particle==14 && pmag>2.3 && cosTheta>0.96) &&
        !(particle==8 && pmag>2.3 && cosTheta>0.96) )
    {
      ret = 1; /* Good event */
    }
    else
    {
      ret = 0; /* Bad event */
    }
  }
  else if(run==7) /* g11a run */
    {
    cutparam[3] = 1.87603;
    cutparam[2] = -4.54334;
    cutparam[1] = 0.022356;
    cutparam[0] = 0.993861;
    cosThetacut = cutparam[3]*pow(phi,6) + cutparam[2]*pow(phi,4) + cutparam[1]*pow(phi,2) + cutparam[0];
    phiCut = 0.4;
    forwardcosThetacut = 0.985;

    if (cosTheta < cosThetacut &&
        fabs (phi) < phiCut && 
        cosTheta < forwardcosThetacut &&
        cosTheta > -0.5 &&
        !(particle==14 && pmag<0.375) &&
        !(sec==1 && cosTheta<-0.25) &&
        !(sec==3 && cosTheta<0.15) && 
        !(sec==5 && cosTheta<0.0))
    {
      ret = 1; /* Good event */
    }
    else
    {
      ret = 0; /* Bad event */
    }
  }
  else
    ret = 1; /* Doesn't match up with any runs */

  return ret;
}


/* Convert a lab angle phi and a sector to a local phi coordinate */
float oneSectorPhi(int sec, float phi)
{
  if(phi>(Pi - (1./6.)*Pi)) phi = phi - 2*Pi;

  if(sec==1)      phi = phi;
  else if(sec==2) phi = phi - (1./3.)*Pi ;
  else if(sec==3) phi = phi - (2./3.)*Pi ;
  else if(sec==4) phi = phi + (3./3.)*Pi ;
  else if(sec==5) phi = phi + (2./3.)*Pi ;
  else if(sec==6) phi = phi + (1./3.)*Pi ;
  else            phi = -999;

  return phi;

}

#endif
