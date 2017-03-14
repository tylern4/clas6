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
and returns the s position of the given t.


side 0 = Left
side 1 = Right

right now we are ignoring the voltage dependence since it is a small correction

*/

float getS(float zz, float tt, int side)
{
  float par[50];
  float sval, volt;


  if(side) //set parameters for the right side, this would be enabled if we cared about voltage dependence
    {
      volt = Rvolt;
    }
  else volt = Lvolt;

      //2.determine how long it takes the electron to travel from s=30 to s=60
      //the equation for s(t) is  
      //s(t) =( (par[0]+par[1]*volt)*(par[8]+par[9]*he)*(par[16]+par[17]*zz+par[18]*pow(zz,2)) + 
      //	   (par[2]+par[3]*volt)*(par[10]+par[11]*he)*(par[19]+par[20]*zz+par[21]*pow(zz,2))*tt + 
      //	   (par[4]+par[5]*volt)*(par[12]+par[13]*he)*(par[22]+par[23]*zz+par[24]*pow(zz,2))*tt*tt + 
      //	   (par[6]+par[7]*volt)*(par[14]+par[15]*he)*(par[25]+par[26]*zz+par[27]*pow(zz,2))*tt*tt*tt
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


    
sval=( (par[0]+par[1]*volt)*(par[8]+par[9]*he)*(par[16]+par[17]*zz+par[18]*pow(zz,2)) + 
		   (par[2]+par[3]*volt)*(par[10]+par[11]*he)*(par[19]+par[20]*zz+par[21]*pow(zz,2))*tt + 
		   (par[4]+par[5]*volt)*(par[12]+par[13]*he)*(par[22]+par[23]*zz+par[24]*pow(zz,2))*tt*tt + 
		   (par[6]+par[7]*volt)*(par[14]+par[15]*he)*(par[25]+par[26]*zz+par[27]*pow(zz,2))*tt*tt*tt
       );


  return sval;
 
}
