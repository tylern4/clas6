#include <math.h>
double multscat(double p,double m,double x)
{
  // p,m in GeV, x in fraction of radiation length
  return(0.021 * (sqrt(p*p + m*m)/p*p) * sqrt(x));
}
