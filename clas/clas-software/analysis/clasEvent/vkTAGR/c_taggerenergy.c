#include<stdlib.h>
#include<stdio.h>
#include <errno.h>
#include <math.h>


double c_taggerenergy(int runno, float E0)
{

  float Ecorr = 0.0;
//  printf("In c_:\t%d\t%f\n",runno, E0);
  taggerenergy_(&runno, &E0, &Ecorr);
  return Ecorr;

}
