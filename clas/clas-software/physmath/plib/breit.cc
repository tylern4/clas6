#include <math.h>
double breit(double m,double mr,double gamma)
{
  double dr;
  {
    dr = mr*mr-m*m;
    return(gamma*gamma*m*m/(dr*dr+m*m*gamma*gamma));
  }
}

