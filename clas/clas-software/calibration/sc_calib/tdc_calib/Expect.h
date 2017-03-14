#ifndef __EXPECT_H
#define __EXPECT_H
#include <iostream>

class Expect {
  double t;
  double increment;
  int    step;
  int    max;
public:
  int    eventcount;
  Expect (int max_, double increment_);
  Expect operator++(int);
  int    getStep () { return step; }
  double getT ()    { return t; }
};

#endif
