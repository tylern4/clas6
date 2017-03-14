#include "SlotAverage.h"

SlotAverage& SlotAverage::operator+= (double x) {
  n++;
  sum += x;
  return *this;
}

