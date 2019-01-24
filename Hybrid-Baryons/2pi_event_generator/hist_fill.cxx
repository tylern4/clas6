#include <iomanip>
#include <string>
#include <stdio.h> 
#include <stdlib.h>  
#include <math.h>
#include <TLorentzVector.h>
#include <iostream>
#include <fstream>
#include "global.h"
#include <sstream>
#include "hist_def.h"
#include "interpol_int.h"

void hist_fill(Float_t E_beam,Float_t W,Float_t W_old, Float_t Q2, Float_t Q2_old, Float_t phi_e,Float_t z_EL,Float_t s12,Float_t s23, Float_t th_hadr,Float_t alph_hadr,Float_t ph_hadr,Float_t sigma_t_final,Float_t sigma_l_final,Float_t eps_l,Float_t sigma_total,TLorentzVector  P4_E_prime,TLorentzVector P4_Pfin,TLorentzVector  P4_PIP,TLorentzVector P4_PIM,TLorentzVector P4_Eini_new,TLorentzVector P4_Eini_fermi,TLorentzVector P4_E_prime_new,TLorentzVector P4_E_prime_new2){


    
Float_t xsect_int_test_t, xsect_int_test_l;
//if (W < 3.0125) interpol_int(Q2,W,xsect_int_test_t, xsect_int_test_l);


TLorentzVector P4_0_miss, P4_0_miss_2,P4_pim_miss,P4_pim_miss_2,P4_0_miss_fermi,P4_0_miss_fermi_2,P4_pim_miss_fermi,P4_pim_miss_fermi_2;

TLorentzVector P4_Pini,P4_Pini_fermi,P4_Eini;

P4_Pini.SetXYZT(0.,0.,0.,MP);
P4_Pini_fermi.SetXYZT(px_fermi,py_fermi,pz_fermi,sqrt(MP*MP+px_fermi*px_fermi+py_fermi*py_fermi+pz_fermi*pz_fermi));
P4_Eini.SetXYZT(0.,0.,E_beam,E_beam);

  P4_0_miss = P4_Pini + P4_Eini - P4_E_prime - P4_PIP - P4_Pfin - P4_PIM;
  P4_0_miss_2 = P4_Pini + P4_Eini_new - P4_E_prime_new - P4_PIP - P4_Pfin - P4_PIM;


  P4_pim_miss = P4_Pini + P4_Eini - P4_E_prime - P4_PIP - P4_Pfin;
  P4_pim_miss_2 = P4_Pini + P4_Eini_new - P4_E_prime_new - P4_PIP - P4_Pfin;

 //----fermi------
  P4_0_miss_fermi = P4_Pini + P4_Eini - P4_E_prime - P4_PIP - P4_Pfin - P4_PIM;
  P4_0_miss_fermi_2 = P4_Pini_fermi + P4_Eini_fermi - P4_E_prime_new2 - P4_PIP - P4_Pfin - P4_PIM;
  
  P4_pim_miss_fermi = P4_Pini + P4_Eini - P4_E_prime - P4_PIP - P4_Pfin;
  P4_pim_miss_fermi_2 = P4_Pini_fermi + P4_Eini_fermi - P4_E_prime_new2 - P4_PIP - P4_Pfin;



 h_W->Fill(W,sigma_total);
 h_Q2->Fill(Q2,sigma_total);
   
 h_phi_e->Fill(phi_e,1.); 
 h_zel->Fill(z_EL,1.);
 
 h_Q2vsW->Fill(W_old,Q2_old,sigma_total);
 h_Q2vsW2->Fill(W_old,Q2_old,1.);
 
 h_nu->Fill((W*W+Q2-MP*MP)/2./MP,1.);
 h_dalitz->Fill(sqrt(s12),sqrt(s23),1.);
 
    
  
     
  h_0_miss->Fill(P4_0_miss.Mag2(),sigma_total);
  h_0_miss_2->Fill(P4_0_miss_2.Mag2(),sigma_total);
   
  h_pim_miss->Fill(P4_pim_miss.Mag2(),sigma_total);
  h_pim_miss_2->Fill(P4_pim_miss_2.Mag2(),sigma_total);

  h_0_miss_en->Fill(P4_0_miss[3],sigma_total);
  h_0_miss_en_2->Fill(P4_0_miss_2[3],sigma_total);
  
//-----for fermi  
   h_0_miss_fermi->Fill(P4_0_miss_fermi.Mag2(),sigma_total);
   h_0_miss_fermi_2->Fill(P4_0_miss_fermi_2.Mag2(),sigma_total);
    
   h_pim_miss_fermi->Fill(P4_pim_miss_fermi.Mag2(),sigma_total);
   h_pim_miss_fermi_2->Fill(P4_pim_miss_fermi_2.Mag2(),sigma_total);

   h_0_miss_en_fermi->Fill(P4_0_miss_fermi[3],sigma_total);
   h_0_miss_en_fermi_2->Fill(P4_0_miss_fermi_2[3],sigma_total);
  
   h_fermi_bonn->Fill(P4_0_miss_fermi.Vect().Mag(),sigma_total);

	
  
  
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
    
 //Single-fold differentil yield for 1.225 < W < 2.15 GeV.   
   if ((W_old>=1.225)&&(W_old<=2.15)&&(Q2>=0.0001)&&(Q2<=1.4))  {
  h_odn_inv_m12[int((W_old-1.225)/0.025)]->Fill(sqrt(s12),sigma_total);
  h_odn_inv_m23[int((W_old-1.225)/0.025)]->Fill(sqrt(s23),sigma_total);
  h_odn_alpha[int((W_old-1.225)/0.025)]->Fill(alph_hadr,sigma_total);
  h_odn_theta[int((W_old-1.225)/0.025)]->Fill(th_hadr,sigma_total);
  };
  
 //Single-fold differentil yield for 2.1625 < W < 3.0375 GeV.  
   if ((W>=2.1625)&&(W<=3.0375)&&(Q2>=0.0001)&&(Q2<=1.3))  {
  h_odn_wwide_inv_m12[int((W-2.1625)/0.05)]->Fill(sqrt(s12),sigma_total);
  h_odn_wwide_inv_m23[int((W-2.1625)/0.05)]->Fill(sqrt(s23),sigma_total);
  h_odn_wwide_alpha[int((W-2.1625)/0.05)]->Fill(alph_hadr,sigma_total);
  h_odn_wwide_theta[int((W-2.1625)/0.05)]->Fill(th_hadr,sigma_total);
  };
  
  
  //Single-fold differentil yield for 3.0875 < W < 4.5375 GeV.  
  if ((W>=3.0875)&&(W<=4.5375)&&(Q2>=0.0001))  {
  h_odn_wgt3_inv_m12[int((W-3.0875)/0.1)]->Fill(sqrt(s12),sigma_total);
  h_odn_wgt3_inv_m23[int((W-3.0875)/0.1)]->Fill(sqrt(s23),sigma_total);
  h_odn_wgt3_alpha[int((W-3.0875)/0.1)]->Fill(alph_hadr,sigma_total);
  h_odn_wgt3_theta[int((W-3.0875)/0.1)]->Fill(th_hadr,sigma_total);
  };



//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%       
     
  //Integral w-yield for 0.1< Q2 < 1.3 GeV2.   
 if ((Q2>=0.1)&&(Q2<=1.3))  {
     h_odn_w_dep_t[int((Q2-0.1)/0.1)]->Fill(W,sigma_t_final);
     h_odn_w_dep_l[int((Q2-0.1)/0.1)]->Fill(W,eps_l*sigma_l_final);
     h_odn_w_dep_l2[int((Q2-0.1)/0.1)]->Fill(W,sigma_l_final);
     h_odn_w_dep_tot[int((Q2-0.1)/0.1)]->Fill(W,sigma_total); 
     };
    
    
   //Integral q2-yield for 1.25< Q2 < 2.075 GeV2.    
   if ((W>1.25)&&(W<=2.075))  { 
     h_odn_q2_dep_t[int((W-1.25)/0.025)]->Fill(Q2,sigma_t_final);
     h_odn_q2_dep_l[int((W-1.25)/0.025)]->Fill(Q2,eps_l*sigma_l_final);
     h_odn_q2_dep_l2[int((W-1.25)/0.025)]->Fill(Q2,sigma_l_final);
     h_odn_q2_dep_tot[int((W-1.25)/0.025)]->Fill(Q2,sigma_total);
       };
     
  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%        


/*
    if ((Q2>=0.00049)&&(Q2<=0.00051))  {
     h_int_crsect_t[0]->Fill(W,xsect_int_test_t);
     h_int_crsect_l[0]->Fill(W,xsect_int_test_l);
      };
          
    if ((Q2>=0.0005)&&(Q2<=1.2505))  {
     h_int_crsect_t[int((Q2-0.0005)/0.05)+1]->Fill(W,xsect_int_test_t);
     h_int_crsect_l[int((Q2-0.0005)/0.05)+1]->Fill(W,xsect_int_test_l);
      };
          
    if ((Q2>=1.29)&&(Q2<=1.3))  {
     h_int_crsect_t[26]->Fill(W,xsect_int_test_t);
     h_int_crsect_l[26]->Fill(W,xsect_int_test_l);
      };  */ 
};
