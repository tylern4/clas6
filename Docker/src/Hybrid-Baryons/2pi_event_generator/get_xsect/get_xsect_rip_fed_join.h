#ifndef GET_XSECT_RIP_FED_JOIN_H
#define GET_XSECT_RIP_FED_JOIN_H

#include <TLorentzVector.h>
#include <math.h>
#include <stdio.h>
#include <iomanip>
#include <iostream>
#include <string>
#include "get_xsect_14_18_lowq2_fit.h"
#include "get_xsect_fedotov.h"
#include "get_xsect_ripani.h"
#include "global.h"
#include "interpol.h"
#include "interpol_fedotov.h"
#include "interpol_int.h"

Float_t pol1_fed_0575_065(Float_t x, Short_t wbin, Short_t i);
Float_t pol1_fed_065_095(Float_t x, Short_t wbin, Short_t i);
Float_t pol1_fed_095_130(Float_t x, Short_t wbin, Short_t i);
Float_t func_sigma_t_fed(Float_t x, Short_t wbin);
Float_t pol2_fed(Float_t x, Short_t wbin, Short_t i);

void get_xsect_rip_fed_join(Float_t Q2gen, Float_t Wgen, Float_t s12gen,
                            Float_t s23gen, Float_t thetagen, Float_t alphagen,
                            Float_t phigen, Float_t &sigma_t_final,
                            Float_t &sigma_l_final, Float_t &sigma_c2f_final,
                            Float_t &sigma_s2f_final, Float_t &sigma_cf_final,
                            Float_t &sigma_sf_final);

#endif
