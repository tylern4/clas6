#include <particleType.h>
#include <math.h>

#ifndef PI
#define PI M_PI
#endif

/* general purpose kinematic routines */

/* ------prototypes --------*/
float Nu(float W, float Q2);
 /*---------------------------------------------------------------------------*/
float ePrimeEnergy(float BeamEnergy, float W, float Q2){
  /* give electron beam energy, W and Q2 returns the scattered electron energy */
  return (2*BeamEnergy*PROTON_MASS + PROTON_MASS*PROTON_MASS - Q2 - W*W)/(2*PROTON_MASS);
} 
 /*---------------------------------------------------------------------------*/
float cosElectronTheta(float BeamEnergy, float W, float Q2){

  /* returns the scattered electron angle given incident beam energy, W, and 
     Q2 */

  float ScatteredE = BeamEnergy - Nu(W, Q2),
        ScatteredMo = sqrt(ScatteredE*ScatteredE - ELECTRON_MASS*ELECTRON_MASS),
        BeamMo =      sqrt(BeamEnergy*BeamEnergy -  ELECTRON_MASS*ELECTRON_MASS);

  return (2*BeamEnergy*ScatteredE - Q2 - 2*ELECTRON_MASS*ELECTRON_MASS)/(2*ScatteredMo*BeamMo);
}
/*---------------------------------------------------------------------------*/
float Nu(float W, float Q2){

  /* Nu = E - E' */
  
  return (Q2 + W*W - PROTON_MASS*PROTON_MASS)/(2*PROTON_MASS);

}
/*---------------------------------------------------------------------------*/
float VPhotonFlux(float BeamEnergy, float W, float Q2){
  
  /* return virtual photon flux */

  float  alpha = 1/137.03599,
         EPrime = BeamEnergy - Nu(W, Q2),                           /* scattered electron energy */
         Kappa = (W*W - PROTON_MASS*PROTON_MASS)/(2*PROTON_MASS),   /* common kinematic factor */
	 ETheta = acos(cosElectronTheta(BeamEnergy, W, Q2)),        /* scattered electron angle */
         TanTheta2 = tan(ETheta/2) * tan(ETheta/2),     
         Polar = 1/(1 + 2*(1 + Nu(W,Q2)*Nu(W,Q2)/Q2)*TanTheta2),  /* virtual photon polarization */
         Flux = alpha/(2*PI*PI) * EPrime/BeamEnergy * Kappa/Q2 * 1/(1-Polar);
    
  return Flux;
}
