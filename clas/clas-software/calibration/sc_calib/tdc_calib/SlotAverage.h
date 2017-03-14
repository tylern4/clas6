#ifndef _SLOT_AVERAGE_H
#define _SLOT_AVERAGE_H

#include <iostream>
#include <map>

class SlotAverage {
  int n;
  double sum;
public:
  SlotAverage (): n(0), sum(0.) {}
  SlotAverage (const SlotAverage& a): n(a.n), sum(a.sum) {}
  double getAverage() { return n ? sum/n : 0.; }
  SlotAverage& operator+= (double x);
};

#endif
