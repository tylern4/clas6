/* Routines for correcting the momentum.  They use some global variables 
 because this seemed like the easiest way to transfer the contents of the map 
 (read only once during initialization) to the momentum correction routines.*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <kinematics.h>
#include <map_manager.h>
#include <pid.h>
#include <particleType.h>

/* global variables needed for the momentum correction */
float sector_fit[6][5];
float theta_sec[6][3];


/* Angle dependent correction to the magnitude of the momentum  lab angles */

float p_corr_angle(int sec, float p,float theta){
  float corr=1.0;
  float xp =  cos(theta);
    corr+=theta_sec[sec-1][0]*xp;
    corr+=theta_sec[sec-1][1]*sin(theta);
    corr+=theta_sec[sec-1][2]/(1.0-xp);
  return p*corr;
}
  
/* Read the map contents into global arrays */
int init_mom_corr(int runno){
  char *dir,map[100];  
  char sec_name[10];
  int i,firsttime,j;


  dir = getenv("CLAS_PARMS");
  sprintf(map,"%s/Maps/mom_corr.map",dir);  
  
  for (i=0;i<6;i++){
    sprintf(sec_name,"sector%d",i+1);
    map_get_float(map,"phi_func",sec_name,5,&sector_fit[i][0],runno,&firsttime);
    map_get_float(map,"theta_func",sec_name,3,&theta_sec[i][0],runno,&firsttime);
  }
  /*  printf("Gordon's original values\n");
  for(i=0;i<6;i++){
   printf("i=%i, sector_fit[i][0]=%f \n",i,sector_fit[i][0]);
   printf("i=%i, sector_fit[i][1]=%f \n",i,sector_fit[i][1]);
   printf("i=%i, sector_fit[i][2]=%f \n",i,sector_fit[i][2]);
   printf("i=%i, sector_fit[i][3]=%f \n",i,sector_fit[i][3]);
   printf("i=%i, sector_fit[i][4]=%f \n",i,sector_fit[i][4]);
  }
  printf("Un-re-normalized values\n");
  for(i=0;i<6;i++){
   printf("i=%i, sector_fit[i][0]=%e \n",i,sector_fit[i][0]*pow(0.001, 0));
   printf("i=%i, sector_fit[i][1]=%e \n",i,sector_fit[i][1]*pow(0.001, 1));
   printf("i=%i, sector_fit[i][2]=%e \n",i,sector_fit[i][2]*pow(0.001, 2));
   printf("i=%i, sector_fit[i][3]=%e \n",i,sector_fit[i][3]*pow(0.001, 3));
   printf("i=%i, sector_fit[i][4]=%e \n",i,sector_fit[i][4]*pow(0.001, 4));
   sector_fit[i][0]=sector_fit[i][0]*pow(0.001, 0);
   sector_fit[i][1]=sector_fit[i][1]*pow(0.001, 1);
   sector_fit[i][2]=sector_fit[i][2]*pow(0.001, 2);
   sector_fit[i][3]=sector_fit[i][3]*pow(0.001, 3);
   sector_fit[i][4]=sector_fit[i][4]*pow(0.001, 4);
  }
  */
}



/* NOTE: the phi correction was based on fitting the lambda mass squared 
   as a function of phi and so is not guaranteed to work for all cases. 
   All sectors were folded onto sector one in the fits, hence the switch 
   at the beginning of the routine. */
float p_corr_phi(int sec,float p,float phi){
  float corr=1.0;
  int i,j;
 
  switch(sec){
  case 1:
    break;
  case 2:
    phi-=60.0;
    break;
  case 3:
    phi-=120.0;
    break;
  case 4:
    if (phi>0) phi-=180.0;
    else phi+=180.0;
    break;
  case 5:
    phi+=120.0;
    break;
  case 6:
    phi+=60.0;
    break;
  default:
    break;
  }
  corr+=(sector_fit[sec-1][0]-LAMBDA_MASS*LAMBDA_MASS)/sector_fit[sec-1][0];
  for (i=1;i<5;i++){
    corr+=sector_fit[sec-1][i]*pow(phi,(float) i)/sector_fit[sec-1][0];
    /*corr+=sector_fit[sec-1][i]*pow(phi,(float) i)*pow(0.001,(float) i)
      /sector_fit[sec-1][0];*/
  }
 
  return p*corr;
}

/* crude dedx correction for kaons */
float p_corr_p(float p){
  float corr=1;

  corr-=(0.0066/p-0.00375/p/p)/1.2448;
  return p*corr;

}

/* Routine returns a corrected 4-vector given an uncorrected 4-vector */
vector4_t p_correction(vector4_t particle){
  float p=v3mag(particle.space);
  vector3_t dir=v3div(p,particle.space);
  vector4_t corrected;
  float mass=v4mass(particle),phi,theta;
  int sector=get_sector(&particle);
  float rho = v3radius(particle.space);
  theta = acos(dir.z);
  phi=180*atan2(particle.space.y,particle.space.x)/M_PI;  
  
  p=p_corr_angle(sector,p,theta);
  p=p_corr_phi(sector,p,phi); 

  corrected.space=v3mult(p,dir);
  corrected.t=sqrt(mass*mass+v3magsq(corrected.space));
 
  return corrected;
}





