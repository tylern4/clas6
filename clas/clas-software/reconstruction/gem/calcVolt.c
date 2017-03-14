#include "rtpc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
/* stuff for calling fortran routines and linking cernlib */
#define  f2cFortran

////////////////////////////////////////////////////////////////
//this routine calculates the actual potential difference for 
//each chamber half given the voltage settings on the cathode
//and the first GEM.  This routine puts the spreadsheet we usually
//use for this purpose into c form.  The resistor values are set 
//from voltage_divider_C_kirchhoff.sxc
////////////////////////////////////////////////////////////////

void calcVolt(float VGL, float VGR, float VKL, float VKR)
{
  float R1, R2, R3, R4, I1;

  //start with the left
  R1  = 1e+06;
  R2  = 1.01e+06;
  R3  = 9.079e+07;
  R4  = 1.446e+07;
  I1  = (VKL-VGL+(VGL*R2)/(R4+R2))/(R2-(R2*R2)/(R4+R2)+R3+R1);
  Lvolt = I1*R3;
  printf("Calculated Left Drift Voltage = %.1f\n", Lvolt);

  //now the right
  R1  = 1.053e+06;
  R2  = 1.14e+06;
  R3  = 8.518e+07;
  R4  = 1.397e+07;
  I1  = (VKR-VGR+(VGR*R2)/(R4+R2))/(R2-(R2*R2)/(R4+R2)+R3+R1);
  Rvolt = I1*R3;
  printf("Calculated Right Drift Voltage = %.1f\n", Rvolt);
  
}
