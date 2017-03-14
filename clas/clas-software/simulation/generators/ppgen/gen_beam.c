/*
   gen_p
   
   Antoine Cazes   - University of South Carolina
   6/11/2001

   generate a photon beam with a Bremsstrahlung
   spectrum

   input - none
   output - text out to screen

------------------------------------------- */

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <scalers.h>
#include <pid.h>
#include <utility.h>
#include <kinematics.h>
#include <math.h>



main()
{
  FILE *fp = NULL;
  vector4_t pf;               /* photon final 4 vector */
  vector3_t vert={0.,0.,-100.};
  int pid=1;            /*14=proton, 11=kaon pi+=8 pi-=9 pi0=7 neut=13*/
  int i;
  int numpart = 1;           /* number of particles in event */
  float Ee=2.4;                /* electron energy */
  int t0=0.;                 /* time offset for txt2part */
  float Esq;
  int nevents = 8000000;        /* number of events */
  float r,theta,ctheta;
  int lux = 1;                /* cernlib random number generator vars */
  int k1 = 0;
  int k2 = 0;
  float seed = 362360.;
  float rvec[3]; /*[4];*/
  int len = 3; /*4;*/


  rluxgo_(&lux,&seed,&k1,&k2);     /* init cernlib ran num gen */
  for (i = 0; i < nevents; i++){
    ranlux_(rvec,&len);
    r=rvec[0];
    ctheta = 1.0-2.0*rvec[1];            /* cos theta runs -1 to 1 */
    theta = (float) acos(ctheta);

    //    Esq = 1.5 + rvec[2]*0.9;          /* photon energy 1.5 to 2.4 GeV */
    Esq = .00001*pow(240000,rvec[2]);  /* photon energy between 10keV and 2.4 GeV,
	     BremStrallung Spectrum : (E=min*pow((max/min),rvec[2])         */

    vert.x= r*cos(theta);        /* beam diameter : 2cm */
    vert.y= r*sin(theta);
    
    pf.t=Esq;
    pf.space.x = 0;
    pf.space.y = 0;
    pf.space.z = Esq;

    /*printout is for txt2part */

    printf("%d\n",numpart); 
    printf("%f %f %f\n",Ee,Esq,t0);
    printf("%d %f %f %f %f\n",pid,pf.t,pf.space.x,pf.space.y,pf.space.z);
    printf("%f %f %f\n",vert.x,vert.y,vert.z);
  }

}








