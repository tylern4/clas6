#ifndef _REGRESSION_H
#define _REGRESSION_H

#include "jdefine.h"
class Regression {
  double sum;
  double sumx;
  double sumxx;
  double sumy;
  double sumyy;
  double sumxy;
public:
  Regression ();
  void Fill(double x, double y, double z);
  double AverageX();
  double AverageY();
  double VariationXX();
  double M();
  double B();
  double* GetParameters();
};

#endif
