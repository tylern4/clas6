#include <math.h>
#include <plib.h>
double epsilonL(double E,double Ep,double theta)
{
  double nu;
  double qsq;
  {
    qsq = Qsq(E,Ep,theta);
    nu = E-Ep;
    return(qsq/nu*epsilon(nu,qsq,theta));
  }
}

