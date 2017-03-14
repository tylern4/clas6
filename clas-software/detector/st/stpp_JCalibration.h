#ifndef __JCALIBRATION_H
#define __JCALIBRATION_H

#include <iostream>
#include <string>
#include <map>
#include <stdlib.h>
extern "C" {
#include "ntypes.h"
#include "bostypes.h"
}

#include "stpp_caldbtable.h"

using namespace std;

class JCalibration {
  int runno;
  void* caldb[MAX_CALDB_TABLE];
  const_type type[MAX_CALDB_TABLE];
public:
  JCalibration(int runno_);
  ~JCalibration();
  double GetFloat(int caldbitem, int index);
  int GetInt(int caldbitem, int index);
};

#endif
