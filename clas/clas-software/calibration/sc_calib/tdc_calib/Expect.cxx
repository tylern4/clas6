#include "Expect.h"

Expect::Expect (int max_, double increment_) : 
  t(0.), increment(increment_), step(0), max(max_), eventcount(0) {
} 

Expect Expect::operator++(int) {
  eventcount = 0;
  if (++step >= max) { step = 0;  t = 0.; } 
  else  t += increment;
  return *this;
}
