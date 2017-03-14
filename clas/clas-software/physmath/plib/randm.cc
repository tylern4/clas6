#include <stdlib.h>

double randm (double low, double high)
{
  return ((high - low) * drand48 () + low);
}
