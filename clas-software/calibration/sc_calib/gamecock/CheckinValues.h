#ifndef __CHECKINVALUES_H
#define __CHECKINVALUES_H

#include "jdefine.h"
#include "ReadRoc.h"

class CheckinValues {
  double v[N_CHANNEL];
public:
  CheckinValues(int index, int iserr);
  CheckinValues(int lr, ReadRoc* rroc);
  CheckinValues(int nmip, double* y0, double* y0err);
  const double* GetValues() {return v;}
  int WriteMap(const char* filename);
};

#endif
