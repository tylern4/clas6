#include <math.h>
double gauss(double m,double mr,double sigma)
{
  return(exp(-pow(m - mr,2)/(2 * pow(sigma,2))));
  
}

