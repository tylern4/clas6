#include <math.h>
#include <plib.h>
double tmin(double plab,double ma,double mb,double mc,double md)
{
  double Ea;
  double Ec;
  double pcma;
  double pcmc;
  double w;

  double ret = -1000.0;

  
  
  w = sqrt(s(plab,ma,mb));
  // check that all masses are greater or equal to 0.0
  if (ma >= 0.0 && mb >= 0.0 && mc >= 0.0 && md >= 0.0) {
    if ((w > ma + mb) && (w >= mc + md)) {
      pcma = pprime(w,ma,mb);
      pcmc = pprime(w,mc,md);
      Ea = ener(pcma,ma);
      Ec = ener(pcmc,mc);
      ret = (ma*ma+mc*mc-2.0*Ea*Ec+2.0*pcma*pcmc);
    }
  }
  return(ret);

}


