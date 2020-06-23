#ifndef HIST_FILL_H
#define HIST_FILL_H

#include <TLorentzVector.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include "global.h"
#include "hist_def.h"
#include "interpol_int.h"

void hist_fill(Float_t E_beam, Float_t W, Float_t W_old, Float_t Q2,
               Float_t Q2_old, Float_t phi_e, Float_t z_EL, Float_t s12,
               Float_t s23, Float_t th_hadr, Float_t alph_hadr, Float_t ph_hadr,
               Float_t sigma_t_final, Float_t sigma_l_final, Float_t eps_l,
               Float_t sigma_total, TLorentzVector P4_E_prime,
               TLorentzVector P4_Pfin, TLorentzVector P4_PIP,
               TLorentzVector P4_PIM, TLorentzVector P4_Eini_new,
               TLorentzVector P4_Eini_fermi, TLorentzVector P4_E_prime_new,
               TLorentzVector P4_E_prime_new2);

#endif
