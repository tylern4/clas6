
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include "rtpc.h"

/////////////////////////
/*1st version 3/28/06 by N. Baillie

This subroutine takes as input the time in ns, the z position of the pad
in mm and a bit to tell if we are on the left or right half of the chamber
to calculate the phi coordinate of the hit.

We have to also use the polynomial which tells us the phi offset (distance in phi
the electrons travel after hitting the first GEM foil)

side 0 = Left
side 1 = Right

3-29-06 the side is not taken into account right now because there is no voltage dependence in this fit
4-17-06 put the L/R voltage in so that the offsets are correct.


*/

float getPhi(int padnum, float tt, int side, float tt_inout)
{
  float par[42];
  float phioff, phi_0, phi_in,phi_fit;
  float zz, volt;
  
  zz = padLocs[padnum].z;
  
//1. calculate toff.
  if(sol_current == 450)
    {
      if(side) //set parameters for the right side
	{
	  volt = Rvolt;
	  par[0] =  -9.91e-04;
	  par[1] = 0.0736;
	  par[2] = -4.00e-06;
	  par[3] = 1.82e-04;
	  par[4] =  1.10e-10;
	  par[5] = -6.57e-09;
	  par[6] = 1.76;
	  par[7] = -125.8;
	}
      else //set parameters for the left side
	{
	  volt = Lvolt;
	  par[0] =  -1.01e-03;
	  par[1] = 0.0745;
	  par[2] = -4.00e-06;
	  par[3] = 1.83e-04;
	  par[4] =  1.21e-10;
	  par[5] = -7.55e-09;
	  par[6] = 1.76;
	  par[7] = -125.2;
	}
    }
  
  if(sol_current == 534)
    {
      if(side) //set parameters for the right side
	{
	  volt = Rvolt;
	  par[0] = -1.092632e-03; 
	  par[1] = 8.183019e-02;	  
	  par[2] = -4.890155e-06;
	  par[3] = 2.240883e-04;
	  par[4] = 1.041060e-10;
	  par[5] = -5.731347e-09;
	  par[6] = 1.958134e+00;
	  par[7] = -1.409292e+02;
	}
      else
	{
	  par[0] = -1.256155e-03;
	  par[1] = 9.377816e-02;
	  par[2] = -4.654547e-06;
	  par[3] = 2.067998e-04 ;
	  par[4] = 1.038575e-10 ;
	  par[5] = -5.813908e-09;
	  par[6] = 2.198069e+00 ;
	  par[7] = -1.584123e+02;
	}
    }


  //printf("zz, volt, he = %f %f %f\n", zz, volt, he);

  phioff = (par[0]*he + par[1]) * volt +
         (par[2]*he + par[3])*zz*zz +
         (par[4]*he + par[5])*pow(zz,4) +
         (par[6]*he + par[7]);

  phioff *= (3.14159/180);

  if(sol_current == 450)

    {
      par[0] =    1.00000e+00;      
      par[1] =   0.00000e+00;      
      par[2] =   1.00000e+00;      
      par[3] =   0.00000e+00;      
      par[4] =  1.00000e+00;      
      par[5] =  0.00000e+00;      
      par[6] =  1.00000e+00;     
      par[7] =  0.00000e+00;      
      par[8] =  1.00000e+00;      
      par[9] =  0.00000e+00;      
      par[10] =  1.00000e+00;      
      par[11] =  0.00000e+00;      
      par[12] =  1.52334e-05;      
      par[13] =  -2.03509e-07;      
      par[14] =  2.79640e-01;      
      par[15] =  -4.44329e-03;
      par[16] =  8.00732e-03;
      par[17] =  -1.24104e-04;
      par[18] =  2.89233e-03;
      par[19] =  -4.67065e-05;
      par[20] =  -3.83334e-11;
      par[21] =  1.50293e-12;
      par[22] =  1.20346e-12;
      par[23] =  -1.37515e-14;
      par[24] =  -3.48615e-01;
      par[25] =  5.72897e-04;
      par[26] =  -3.53603e-04;
      par[27] =  -2.36926e-03;
      par[28] =  -7.56945e-09;
      par[29] =  2.29214e-08;
      par[30] =  2.57272e-05;
      par[31] =  5.90219e-10;
      par[32] =  6.25458e-11;
      par[33] =  -1.26593e-08;
      par[34] = -6.46531e-13;
      par[35] = -2.18599e-13;
      par[36] = -1.40345e-05;
      par[37] = -1.05578e-09;
      par[38] = -4.70812e-10;
      par[39] = 3.44310e-07;
      par[40] = 3.07446e-11;
      par[41] = 1.61378e-11;
    }

  if(sol_current == 534)
    {
      par[0] = 1;
      par[1] = 0;
      par[2] = 1;
      par[3] = 0;
      par[4] = 1;
      par[5] = 0;
      par[6] = 1;
      par[7] = 0;
      par[8] = 1;
      par[9] = 0;
      par[10] = 1;
      par[11] = 0;  
      par[12] = 1.025040e-05;
      par[13] = -1.609003e-07;
      par[14] = 3.091167e-01;
      par[15] = -5.298007e-03;
      par[16] = 5.827652e-03;
      par[17] = -1.005736e-04;
      par[18] = 3.588710e-03;
      par[19] = -6.228375e-05;
      par[20] = -2.669251e-10;
      par[21] = 4.629185e-12;
      par[22] = -1.706851e-11;
      par[23] = 2.949996e-13;
      par[24] = -1.137467e+01;
      par[25] = -1.017944e-03;
      par[26] = -1.003583e-04;
      par[27] = -3.829848e-03;
      par[28] = 2.067874e-08;
      par[29] = 2.369233e-08;
      par[30] = 8.122283e-05;
      par[31] = -1.857185e-09;
      par[32] = 5.195075e-10;
      par[33] = -3.378654e-08;
      par[34] = 1.480598e-12;
      par[35] = -6.259748e-13;
      par[36] = -5.926002e-05;
      par[37] = 3.766703e-09;
      par[38] = -1.838781e-09;
      par[39] = 4.643884e-08;
      par[40] = -3.801854e-12;
      par[41] = 2.041593e-12;
  }
      
  phi_0=( (par[0]+par[1]*volt)*(par[12]+par[13]*he)*(par[24]+par[25]*zz+par[26]*pow(zz,2)) + 
		   (par[2]+par[3]*volt)*(par[14]+par[15]*he)*(par[27]+par[28]*zz+par[29]*pow(zz,2))*tt + 
		   (par[4]+par[5]*volt)*(par[16]+par[17]*he)*(par[30]+par[31]*zz+par[32]*pow(zz,2))*tt*tt + 
		   (par[6]+par[7]*volt)*(par[18]+par[19]*he)*(par[33]+par[34]*zz+par[35]*pow(zz,2))*tt*tt*tt  + 
		   (par[8]+par[9]*volt)*(par[20]+par[21]*he)*(par[36]+par[37]*zz+par[38]*pow(zz,2))*tt*tt*tt*tt + 
		   (par[10]+par[11]*volt)*(par[22]+par[23]*he)*(par[39]+par[40]*zz+par[41]*pow(zz,2))*tt*tt*tt*tt*tt 
		   );

  phi_in=( (par[0]+par[1]*volt)*(par[12]+par[13]*he)*(par[24]+par[25]*zz+par[26]*pow(zz,2)) + 
		   (par[2]+par[3]*volt)*(par[14]+par[15]*he)*(par[27]+par[28]*zz+par[29]*pow(zz,2))*tt_inout + 
		   (par[4]+par[5]*volt)*(par[16]+par[17]*he)*(par[30]+par[31]*zz+par[32]*pow(zz,2))*tt_inout*tt_inout + 
		   (par[6]+par[7]*volt)*(par[18]+par[19]*he)*(par[33]+par[34]*zz+par[35]*pow(zz,2))*tt_inout*tt_inout*tt_inout  + 
		   (par[8]+par[9]*volt)*(par[20]+par[21]*he)*(par[36]+par[37]*zz+par[38]*pow(zz,2))*tt_inout*tt_inout*tt_inout*tt_inout + 
		   (par[10]+par[11]*volt)*(par[22]+par[23]*he)*(par[39]+par[40]*zz+par[41]*pow(zz,2))*tt_inout*tt_inout*tt_inout*tt_inout*tt_inout 
		   );

  phi_fit = padLocs[padnum].phi + phi_in + phioff - phi_0;
  //printf("phi_fit, padLocs[%d].phi, phioff, phi_in ,phi_0= %f, %f, %f, %f, %f\n", padnum, phi_fit, padLocs[padnum].phi,phioff, phi_in ,phi_0);
  if(phi_fit < 0) phi_fit += 2*PI;
  if(phi_fit > 2*PI) phi_fit -= 2*PI;
  
  if(phi_fit < 0 || phi_fit > 2*PI) 
    {
      printf("phi_fit = %f and that aint right!\n", phi_fit);
      
    }
 return phi_fit; 

}
