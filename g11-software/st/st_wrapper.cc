#include "st_cpp_wrapper.h"

int st_set_def() {
  st_set_def_();
  return 1;
}

int st_init() {
  st_init_();
  return 1;
}

int st_bevt() {
  st_bevt_();
  return 1;
}

int st_evt(int trk_level) {
  st_evnt_(&trk_level);
  return 1;
}
