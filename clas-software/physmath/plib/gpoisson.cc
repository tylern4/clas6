using namespace std;
#include <iostream>
#include <math.h>
#include <plib.h>
#include <time.h>

// for a given nu, returns a random # of events

int gpoisson(double nu)
{
  int ret = 0;
  int success = 0;
  int n = 0;
  while (!success) {
    double prob = gammq(n + 1,nu);
     double r2 = randm (0.0, 1.0);
     if (r2 < prob) {
       ret = n;
       success = 1;
     }
     n++;
  }
  return(ret);
}

    
