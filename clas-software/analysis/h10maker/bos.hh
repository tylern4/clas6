#ifndef BOS_HH
#define BOS_HH

const int nbcs = 700000;

extern "C" struct {
  int junk[5];
  int iw[nbcs];
} bcs_;

//Macros for operating on the BOS data:

#define IW (bcs_.iw)
#define RW ((float*) bcs_.iw)

#endif
