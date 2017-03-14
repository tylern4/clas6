#include <math.h>
#include <plib.h>
double gammaCM(double pbeam,double mbeam,double mtarget)
{
  double M;
  double e1;
  {
    e1 = ener(pbeam,mbeam);
    M = sqrt(2.0*e1*mtarget+mbeam*mbeam+mtarget*mtarget);
    return((e1+mtarget)/M);
  }
}

