#include <math.h>
double pprime(double w,double m,double m3)
{
  {
    return(sqrt(0.25*pow(w*w-m*m+m3*m3,2.0)/(w*w)-m3*m3));
  }
}

