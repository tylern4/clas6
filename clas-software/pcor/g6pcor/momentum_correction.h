#ifndef _MOM_CORR_H
#define _MOM_CORR_H

//use global variales for holding map entries
//the correction coefficients are sector dependent
//for each sector there are 6 numbers

extern float fit_par_beam;
extern float fit_par_pip[36];//for Pi+
extern float fit_par_pim[36];//for Pi-
extern float fit_par_p[36];//for Proton

//function prototypes
void init_momentum_correction(int runno);
fourVec momentum_correction(fourVec P, int sector, int pid);
fourVec photon_beam_correction(fourVec P);
fourVec photon_beam_correction(fourVec P, float beamErgCorr);

#endif
