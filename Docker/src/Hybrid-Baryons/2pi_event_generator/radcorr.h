#ifndef RADCORR_H
#define RADCORR_H

#include <TLorentzVector.h>
#include <math.h>
#include <stdio.h>
#include <iomanip>
#include <iostream>
#include <string>
#include "Math/GaussIntegrator.h"
#include "Math/WrappedTF1.h"
#include "TF1.h"
#include "TH1.h"
#include "TMath.h"
#include "get_xsect_14_18_lowq2_fit.h"
#include "global.h"
#include "interpol_int.h"

void radcorr(Float_t R, Double_t R_ini, Double_t R_fin, Float_t E_beam,
             Float_t Q2gen, Float_t Wgen, Float_t &Wnew, Float_t &Q2new,
             Float_t &E_beam_new, Float_t &e_radgam, Float_t &cr_rad_fact);

#endif
