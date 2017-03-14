
/*this is a subroutine to use the mag field map for the DVCS solenoid to find the avg field
  which a track sees at a given z (calculated in another routine as the midpoint in z between
  the beginning and end of the track
  initial code written by Nate Baillie on 12/03/05

prototype:

inputs: z position(mm) to find avg field
outputs: the average field (tesla) at the given z is returned

*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
//#include <time.h>
#include "/usr/include/time.h"
#include "rtpc.h"

float bofz(float zz)
{

 float ss;
 int ii, jj, kk, ll, mm;
 float tt, uu;
 float Bs, Bz, Btot;
 float pointer;
 
  if(zz < -100 || zz > 100) 
    {
      if(!quiet) fprintf(stderr,"axial coordinate z is out of range [-100,100] mm\n"); 
      return(-1);
    }
  
  /* ss = 30.0; */ //use the middle of the radial coordinate range
  ss= 45.0; /* changed to middle of active radial range by hcf 12/7/05 */


  


  if(zz<0) zz = -zz;

  pointer = -1000;
  ll = -1000;
  ii=0;
  while((ss > pointer) && (ii<NUM_S1))
    {
      pointer = sVals1[ii];
      if (ss == pointer) ll = ii;
      else if (ss <= pointer) ll = ii-1;
      else ii++;
    }
  //printf("ss = %f, ll = %d, ll+1 = %d \n", ss, ll, ll+1);

  pointer = -1000;
  mm = -1000;
  jj=0;
  while((zz > pointer) && (jj<NUM_Z1))
    {
      pointer = zVals1[jj];
      if (zz == pointer) mm = jj;
      else if (zz <= pointer) mm = jj-1;
      else jj++;
    }
  //printf("zz = %f, mm = %d, mm+1 = %d \n", zz, mm, mm+1);
  
  tt = (ss - sVals1[ll])/(sVals1[ll+1] - sVals1[ll]);
  uu = (zz - zVals1[mm])/(zVals1[mm+1] - zVals1[mm]);

  Bs = (1-tt)*(1-uu)*int_Bs[ll][mm] + tt*(1-uu)*int_Bs[ll+1][mm] +
    (1-tt)*uu*int_Bs[ll][mm+1] + tt*uu*int_Bs[ll+1][mm+1]; // in Gauss
  Bz = (1-tt)*(1-uu)*int_Bz[ll][mm] + tt*(1-uu)*int_Bz[ll+1][mm] +
    (1-tt)*uu*int_Bz[ll][mm+1] + tt*uu*int_Bz[ll+1][mm+1]; //in Gauss

  Btot = sqrt(pow(Bs, 2)+pow(Bz, 2))/1000; //in kiloGauss
  

  //printf(" interpolated Bs = %f, Bz = %f \n", Bs, Bz);
  if(!quiet) printf(" Btot = %.2f Tesla at z = %f\n \n", Btot/10.0, zz);  

  return(Bz/1000.0); /* hcf changed this from Btot to Bz 12/7/05 */
      
}
