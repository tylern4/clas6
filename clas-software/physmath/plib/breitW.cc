#include <math.h>
double breitW(double m,double mr,double gamma)
{
  double bW;
  {
    bW = 0.5772156649015329/(m-mr-sqrt(-1.0)*0.5772156649015329/2.0)/2.0;
    return(bW);
  }
}

