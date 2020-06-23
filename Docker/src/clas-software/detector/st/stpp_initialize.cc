#include "stpp_JCalibration.h"
#include "stpp_STposition.h"

extern "C" {
  void stpp_initialize_(int *irun);
}

extern JCalibration* gCalib;

/// initialization at begin of run; function call for C/C++ programs
void stpp_initialize(int irun) {

  /// initialize the global handle to all CalDB constants
  gCalib = new JCalibration(irun);

  /// load ST geometry, init static members of STposition class
  stpp_init_geometry();
}

/// initialization at begin of run; subroutine for FORTRAN programs
void stpp_initialize_(int *irun) {
  stpp_initialize(*irun);
}
