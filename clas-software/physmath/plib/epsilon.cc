#include <math.h>
#include <plib.h>
double epsilon(double E,double Ep,double theta)
{
  double nu;
  double qsq;
    qsq = Qsq(E,Ep,theta);
    nu = E-Ep;
    return(1/(1.0+2.0*(qsq+nu*nu)/qsq*pow(tan(theta/2.0),2.0)));
}


