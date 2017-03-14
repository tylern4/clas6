#include <math.h>
#include <plib.h>
double betaCM(double pbeam,double mbeam,double mtarget)
{
  
    return(pbeam/(ener(pbeam,mbeam)+mtarget));
  
}

