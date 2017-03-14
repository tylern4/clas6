/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include "rtpc.h"

/////////////////////////
/*1st version 3/28/06 by N. Baillie

This subroutine takes as input the the z position of the pad
in mm and a bit to tell if we are on the left or right half of the chamber
to calculate the time in a way the fits understand.

note that the path fits were done so that s(tmax) = 60 and s(0) = 30 (It's more like
a total distance traveled) but this is 
opposite of how the data comes in(earliest times are at largest radii).  Therefore TPC_TMIN 
time and the offset time are SUBTRACTED from altro_time, the timing equation is

tt_fit = tt30(z,V,He) - (altro_time - TPC_TMIN*NS_PER_TIC - toff(z,V,He))

where 
--tt_30 is the time it takes an electron to drift the 30 mm from inner to 
outer drift region radius.  
--altro_time is the time in ns when the signal arrived.
--toff is how long it took for the signal to reach the pad after hitting the first GEM.  
We return 
--tt_fit, this value has a maximum when altro time has a minimum (large radius)

the task of this subroutine then is to calculate the toff for the given z position and
chamber half, subtract this value and the TPC_TMIN from the given altro_time, calculate at what
time the s(t) curve has s=60

side 0 = Left
side 1 = Right


*/

void setPadTimes(float zz, float *toff, int side, float *tt_inout)
{
  float par[50];
  float aa,bb,cc,dd,ee,ff,gg;
  float beta,tt_fit, volt;

  
  //1. calculate toff.
  if(sol_current == 450)
    {
      if(side) //set parameters for the right side
	{
	  volt = Rvolt;
	  par[0] =  0.0822;
	  par[1] = -8.42;
	  par[2] = -7.4e-5;
	  par[3] = -3.37e-4;
	  par[4] =  3.70e-9;
	  par[5] = -1.56e-7;
	  par[6] = -142.1;
	  par[7] = 1.50e+4;
	}
      else //set parameters for the left side
	{
	  volt = Lvolt;
	  par[0] =  0.0674;
	  par[1] = -7.33;
	  par[2] = -6.70e-5;
	  par[3] = -7.60e-4;
	  par[4] =  3.10e-9;
	  par[5] = -1.12e-7;
	  par[6] = -115.3;
	  par[7] = 1.29e+4;
	}
      
      *toff = (par[0]*he + par[1]) * volt +
	(par[2]*he + par[3])*zz*zz +
	(par[4]*he + par[5])*pow(zz,4) +
	(par[6]*he + par[7]);
    }

  if(sol_current == 534)
    {  
      if(side) //set parameters for the right side
	{
	  volt = Rvolt;
	  par[0] = 5.061077e-02;
	  par[1] = -6.044650e+00;
	  par[2] = -3.894542e-07;
	  par[3] = 6.462542e-06;
	  par[4] = -6.056460e-10;
	  par[5] = 2.882863e-08;
	  par[6] = -8.802303e+01;
	  par[7] = 1.093886e+04;
	  par[8] = -1.920106e+00;
	  par[9] = 3.609005e+03;
	  par[10] = 2.982536e-02;
	  par[11] = -6.071443e+01;
	}
      else //set parameters for the left side
	{
	  volt = Lvolt;
	  par[0] = 5.577788e-02;
	  par[1] = -6.718285e+00;
	  par[2] = -4.033658e-07;
	  par[3] = 6.797501e-06;
	  par[4] = -5.123393e-11;
	  par[5] = 2.612502e-09;
	  par[6] = -9.520454e+01;
	  par[7] = 1.189088e+04;
	  par[8] = -2.083522e+00;
	  par[9] = 3.813724e+03;
	  par[10] = 3.592948e-01;
	  par[11] = -7.316095e+02;
	}
      
      *toff =  (par[0]*he + par[1])*volt +  
	(par[2]*he + par[3])*(par[8]*volt + par[9])*zz*zz +
	(par[4]*he + par[5])*(par[10]*volt + par[11])*zz*zz*zz*zz +
	(par[6]*he + par[7]);
    }
      
  //2.determine how long it takes the electron to travel from s=30 to s=60
  //the equation for s(t) is  
  //s(t) =( (par[0]+par[1]*volt)*(par[8]+par[9]*he)*(par[16]+par[17]*zz+par[18]*pow(zz,2)) + 
  //	   (par[2]+par[3]*volt)*(par[10]+par[11]*he)*(par[19]+par[20]*zz+par[21]*pow(zz,2))*tt + 
  //	   (par[4]+par[5]*volt)*(par[12]+par[13]*he)*(par[22]+par[23]*zz+par[24]*pow(zz,2))*tt*tt + 
  //	   (par[6]+par[7]*volt)*(par[14]+par[15]*he)*(par[25]+par[26]*zz+par[27]*pow(zz,2))*tt*tt*tt
  //
  //which then needs to be set equal to 60 and solved for t.  There is only one real root for this equation
  if(sol_current == 450)
    {
      par[0]=   1.00000e+00;
      par[1]=          0.00000e+00;
      par[2]=        1.00000e+00;
      par[3]=         0.00000e+00;
      par[4]=          1.00000e+00;
      par[5]=         0.00000e+00;
      par[6]=          1.00000e+00;
      par[7]=         0.00000e+00;
      par[8]=     2.93601e-01;
      par[9]=      -3.17643e-05;
      par[10]=      1.01599e-01;
      par[11]=       -2.54491e-03;
      par[12]=     1.77282e-08;
      par[13]=      -2.83911e-10;
      par[14]=      -5.12081e-12;
      par[15]=       7.36900e-14;
      par[16]=       1.03567e+02;
      par[17]=        -9.88089e-06;
      par[18]=     -4.07028e-05;
      par[19]=     -7.92949e-02;
      par[20]=      -1.55502e-07;
      par[21]=        -1.30904e-06;
      par[22]=        1.32458e+02;
      par[23]=       1.28763e-03;
      par[24]=        9.31866e-03;
      par[25]=        4.63007e+01;
      par[26]=       1.07514e-03;
      par[27]=       8.34109e-03;
    }

  if(sol_current == 534)
    {
      par[0]=   1.00000e+00;
      par[1]=          0.00000e+00;
      par[2]=        1.00000e+00;
      par[3]=         0.00000e+00;
      par[4]=          1.00000e+00;
      par[5]=         0.00000e+00;
      par[6]=          1.00000e+00;
      par[7]=         0.00000e+00;
      par[8]=        2.450314e-01;
      par[9]=      -6.320618e-05;
      par[10]=      5.068604e-02;
      par[11]=      -2.702700e-03;
      par[12]=      1.462032e-08;
      par[13]=      -3.094008e-10;
      par[14]=      -4.622783e-12;
      par[15]=      7.881178e-14;
      par[16]=     8.479063e+01;
      par[17]=      -5.934702e-06;
      par[18]=     -2.397284e-05;
      par[19]=     -6.122214e-02;
      par[20]=     -7.214010e-08;
      par[21]=     -9.608037e-07;
      par[22]=     1.085041e+02;
      par[23]=     2.681108e-04;
      par[24]=     4.010576e-03;
      par[25]=     4.050651e+01;
      par[26]=     1.457830e-04;
      par[27]=     2.416221e-03;
    }

  dd=(par[0]+par[1]*volt)*(par[8]+par[9]*he)*(par[16]+par[17]*zz+par[18]*pow(zz,2))-60;
  cc=(par[2]+par[3]*volt)*(par[10]+par[11]*he)*(par[19]+par[20]*zz+par[21]*pow(zz,2));
  bb=(par[4]+par[5]*volt)*(par[12]+par[13]*he)*(par[22]+par[23]*zz+par[24]*pow(zz,2));
  aa=(par[6]+par[7]*volt)*(par[14]+par[15]*he)*(par[25]+par[26]*zz+par[27]*pow(zz,2));

  ee=bb/aa;
  ff=cc/aa;
  gg=dd/aa;

  beta = 36*ff*ee - 108*gg - 8*ee*ee*ee + 12*sqrt(12*ff*ff*ff - 3*ff*ff*ee*ee 
						  - 54*ff*ee*gg + 81*gg*gg +12*gg*ee*ee*ee);

  *tt_inout = pow(beta,.333333333)/6 - 6*(ff/3 - ee*ee/9)/pow(beta,.333333333) - ee/3;
 
}
