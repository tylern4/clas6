#include "Regression.h"

Regression::Regression(): sum(0.), sumx(0.), sumxx(0.),
			  sumy(0.), sumyy(0.), sumxy(0.) {}

void Regression::Fill(double x, double y, double z) {
  sum   += z;
  sumx  += z * x;
  sumxx += z * x * x;
  sumy  += z * y;
  sumyy += z * y * y;
  sumxy += z * x * y;
}

double Regression::AverageX () {
  if (sum == 0) throw "Regression::AverageX: no points";
  return sumx/sum;
}

double Regression::AverageY () {
  if (sum == 0) throw "Regression::AverageY: no points";
  return sumy/sum;
}

double Regression::VariationXX () {
  double avex = AverageX();
  return sumxx / sum - avex * avex;
}

double Regression::M () {
  double nominator = VariationXX ();
  if (nominator == 0) throw "Regression::M: all points have same x";
  return  ( sumxy / sum - AverageX() * AverageY() ) / nominator;
}

double Regression::B () {
  return  AverageY() - M() * AverageX() ;
}

double* Regression::GetParameters () {
  double* retval = new double[2];
  retval[0] = B();
  retval[1] = M();
  return  retval;
}
