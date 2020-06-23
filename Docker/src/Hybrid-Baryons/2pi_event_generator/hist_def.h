#ifndef HIST_DEF_H
#define HIST_DEF_H

#include <TLorentzVector.h>
#include <TRint.h>
#include <dlfcn.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include "TCanvas.h"
#include "TF1.h"
#include "TF2.h"
#include "TF3.h"
#include "TFile.h"
#include "TGObject.h"
#include "TGraph.h"
#include "TGraphErrors.h"
#include "TH1.h"
#include "TH1D.h"
#include "TH1F.h"
#include "TH2.h"
#include "TH2D.h"
#include "TH2F.h"
#include "TH3.h"
#include "TH3F.h"
#include "THnSparse.h"
#include "TLine.h"
#include "TMacro.h"
#include "TMath.h"
#include "TMinuit.h"
#include "TObject.h"
#include "TROOT.h"
#include "TStyle.h"
#include "TSystem.h"
#include "TText.h"
#include "TTree.h"
#include "TVirtualFitter.h"
#include "global.h"

extern TH1F *h_e_beam_eff;
extern TH1F *h_W;
extern TH1F *h_Q2;
extern TH1F *h_phi_e;
extern TH2F *h_Q2vsW;
extern TH2F *h_Q2vsW2;
extern TH2F *h_Q2vsW_t;
extern TH2F *h_Q2vsW_l;
extern TH1F *h_nu;
extern TH1F *h_zel;
extern TH2F *h_dalitz;

extern TH1F *h_0_miss;
extern TH1F *h_pim_miss;
extern TH1F *h_0_miss_2;
extern TH1F *h_pim_miss_2;
extern TH1F *h_fermi_bonn;
extern TH1F *h_0_miss_en;
extern TH1F *h_0_miss_en_2;
extern TH1F *h_eradgam;

extern TH1F *h_odn_inv_m12[37];
extern TH1F *h_odn_inv_m23[37];
extern TH1F *h_odn_alpha[37];
extern TH1F *h_odn_theta[37];
extern TH1F *h_odn_theta_2[37];

extern TH1F *h_odn_wwide_inv_m12[18];
extern TH1F *h_odn_wwide_inv_m23[18];
extern TH1F *h_odn_wwide_alpha[18];
extern TH1F *h_odn_wwide_theta[18];
extern TH1F *h_odn_wwide_theta_2[18];

extern TH1F *h_odn_wgt3_inv_m12[15];
extern TH1F *h_odn_wgt3_inv_m23[15];
extern TH1F *h_odn_wgt3_alpha[15];
extern TH1F *h_odn_wgt3_theta[15];
extern TH1F *h_odn_wgt3_theta_2[15];

extern TH1F *h_odn_w_dep_t[12];
extern TH1F *h_odn_w_dep_l[12];
extern TH1F *h_odn_w_dep_l2[12];
extern TH1F *h_odn_w_dep_tot[12];

extern TH1F *h_odn_q2_dep_t[33];
extern TH1F *h_odn_q2_dep_l[33];
extern TH1F *h_odn_q2_dep_l2[33];
extern TH1F *h_odn_q2_dep_tot[33];

int hist_def(Float_t E_beam);

#endif