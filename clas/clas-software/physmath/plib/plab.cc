#include <math.h>
#include <plib.h>
double plab(double m,double pbeam,double mbeam,double mtarget,double thetarCM)
{
  double beta;
  double gam;
  double pcm;
  double ppara;
  double pparap;
  double pperp;
  {
    pcm = pprime(sqrt(s(pbeam,mbeam,mtarget)),m,mtarget);
    gam = gammaCM(pbeam,mbeam,mtarget);
    beta = betaCM(pbeam,mbeam,mtarget);
    ppara = pcm*cos(thetarCM);
    pperp = pcm*sin(thetarCM);
    pparap = gam*(ppara+beta*ener(pcm,m));
    return(sqrt(pparap*pparap+pperp*pperp));
  }
}

