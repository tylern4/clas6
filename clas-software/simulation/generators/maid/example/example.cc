#include "maid.h"
#include <iostream>

int main()
{
  std::cout << maid_dsigma(//beam_energy
                           2.039,
                           //W
                           2.0,
                           //Q^2
                           0.5,
                           //cos(theta*)
                           0.5,
                           //phi* (degrees)
                           1,
                           //helicity
                           0,
                           //model_opt
                           5,
                           //channel_opt
                           3,
                           //resonance_opt
                           0) << std::endl;
}
