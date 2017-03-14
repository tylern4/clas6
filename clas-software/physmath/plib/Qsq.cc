#include <iostream>

#include <math.h>
#include <particleType.h>
#include <plib.h>


double Qsq(double E,double Ep,double theta)
{

    return(-2 * (ELECTRON_MASS*ELECTRON_MASS - E * Ep * (1.0 - cos(theta))));

}

