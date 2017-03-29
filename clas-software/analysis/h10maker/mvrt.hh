#ifndef MVRT_HH
#define MVRT_HH

#include <TTree.h>

extern "C" {
  struct mvrtnt_ {
    int ntrk_mvrt;
    float x_mvrt;
    float y_mvrt;
    float z_mvrt;
  };
  extern struct mvrtnt_ mvrtnt_;

  extern void fill_mvrt_nt_();
}

void mvrt_branches(TTree* tree);

#endif
