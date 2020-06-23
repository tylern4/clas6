#ifndef ANTI_ROT_H
#define ANTI_ROT_H

#include <TLorentzVector.h>
#include <math.h>
#include <stdio.h>
#include <iomanip>
#include <iostream>
#include <string>
#include "global.h"

Float_t G_BYCKLING(Float_t x, Float_t y, Float_t z, Float_t u, Float_t v, Float_t w);

void anti_rot(Float_t W, Float_t Q2, Float_t phi_el, Float_t E_beam, Float_t M12, Float_t M23, Float_t theta_hadr,
              Float_t alpha_hadr, Float_t phi_hadr, Float_t m1, Float_t m2, Float_t m3, TLorentzVector &P4_1,
              TLorentzVector &P4_2, TLorentzVector &P4_3);

#endif
