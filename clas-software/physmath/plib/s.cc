#include <math.h>
#include <plib.h>
double s(double pbeam,double mbeam,double mtarget)
{
  {
    return(mbeam*mbeam+mtarget*mtarget+2.0*sqrt(pbeam*pbeam+mbeam*mbeam)*
mtarget);
  }
}

