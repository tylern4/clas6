//This is a header file intended to be used with the G12 data stream.
//Purpose: Output if track passes G12 fiducial cuts
//Input: Lab Mometum (Pmom), Lab angle theta (Theta), Lab angle phi (Phi), charge of track (q), directory of fiducail parameter .txt file and type of cut (type_of_cut)
//type_of_cut: Loose, Tight, Nominal
//Output: pass/fail

//Updated: March 27, 2014

//Corrections were done by Jason Bono
//Program written by Michael C. Kunkel
//mkunkel@jlab.org

//######################### NOTE ##########################

//ALL VARIABLES IN LAB FRAME, ALL ANGLES IN DEGREES!!!!!!

//#########################################################

#ifndef __CLAS_G12_POSPARTICLE_FIDUCIALCUTS_HPP__
#define __CLAS_G12_POSPARTICLE_FIDUCIALCUTS_HPP__

#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>
#include <cmath>
#define DEG_2_RAD_CONV 0.01745328

float FidFunc_pos(float *, float *);
using namespace std;

namespace clas
{
  namespace g12 {
    
    bool g12_PosParticle_fiducial_cuts(float Pmom, float Theta, float Phi, const std::string& type_of_cut ){
      
      const int Ns = 6;  //number of sectors
      float parL[Ns][9];
      float parH[Ns][9];
      
      parL[0][0] = -2295.59; parL[0][1] = 0.000700362; parL[0][2] = 2266.32;
      parL[0][3] = -198822; parL[0][4] = 0.000363836; parL[0][5] = 198706;
      parL[0][6] = -198764; parL[0][7] = 3.78601e-05; parL[0][8] = 198763;
      parL[1][0] = -2295.45; parL[1][1] = 0.000812151; parL[1][2] = 2266.18;
      parL[1][3] = -198812; parL[1][4] = 0.000280686; parL[1][5] = 198716;
      parL[1][6] = -198764; parL[1][7] = 3.58984e-05; parL[1][8] = 198764;
      parL[2][0] = -2295.01; parL[2][1] = 0.000510464; parL[2][2] = 2266.32;
      parL[2][3] = -198801; parL[2][4] = 0.000167734; parL[2][5] = 198726;
      parL[2][6] = -198762; parL[2][7] = 1.81963e-05; parL[2][8] = 198765;
      parL[3][0] = -2295.19; parL[3][1] = 0.000596059; parL[3][2] = 2265.85;
      parL[3][3] = -198819; parL[3][4] = 0.000275454; parL[3][5] = 198709;
      parL[3][6] = -198764; parL[3][7] = 2.2017e-05; parL[3][8] = 198764;
      parL[4][0] = -2294.95; parL[4][1] = 0.000629762; parL[4][2] = 2265.82;
      parL[4][3] = -198817; parL[4][4] = 0.000300642; parL[4][5] = 198710;
      parL[4][6] = -198764; parL[4][7] = 2.36482e-05; parL[4][8] = 198764;
      parL[5][0] = -2294.57; parL[5][1] = 0.000587603; parL[5][2] = 2265.88;
      parL[5][3] = -198801; parL[5][4] = 0.000264759; parL[5][5] = 198726;
      parL[5][6] = -198763; parL[5][7] = 3.49751e-05; parL[5][8] = 198764;
      
      parH[0][0] = -2266.96; parH[0][1] = -0.000136134; parH[0][2] = 2294.8;
      parH[0][3] = -198725; parH[0][4] = -4.85729e-06; parH[0][5] = 198803;
      parH[0][6] = -198764; parH[0][7] = 2.91321e-05; parH[0][8] = 198764;
      parH[1][0] = -2266.48; parH[1][1] = -0.00054583; parH[1][2] = 2294.99;
      parH[1][3] = -198715; parH[1][4] = -0.00022386; parH[1][5] = 198813;
      parH[1][6] = -198763; parH[1][7] = 2.46949e-05; parH[1][8] = 198764;
      parH[2][0] = -2266.63; parH[2][1] = -0.00041358; parH[2][2] = 2294.55;
      parH[2][3] = -198732; parH[2][4] = -0.000165719; parH[2][5] = 198796;
      parH[2][6] = -198762; parH[2][7] = 2.36424e-05; parH[2][8] = 198765;
      parH[3][0] = -2266.56; parH[3][1] = -0.000353913; parH[3][2] = 2294.34;
      parH[3][3] = -198733; parH[3][4] = -0.000160694; parH[3][5] = 198795;
      parH[3][6] = -198762; parH[3][7] = 2.0461e-05; parH[3][8] = 198766;
      parH[4][0] = -2266.22; parH[4][1] = -0.000632337; parH[4][2] = 2294.37;
      parH[4][3] = -198728; parH[4][4] = -0.000223575; parH[4][5] = 198799;
      parH[4][6] = -198763; parH[4][7] = 3.39674e-05; parH[4][8] = 198765;
      parH[5][0] = -2266.3; parH[5][1] = -0.000349999; parH[5][2] = 2294;
      parH[5][3] = -198726; parH[5][4] = -0.00012299; parH[5][5] = 198802;
      parH[5][6] = -198762; parH[5][7] = 1.54008e-05; parH[5][8] = 198765;
      
      
      
      float phi_clas;
      int sector;
      
      if (Phi < -30){phi_clas = 360 + Phi;}
      else{phi_clas = Phi;}
      
      //get sectors
      if ( (phi_clas > -30) && (phi_clas <= 30 )){sector = 1;}
      else if ( (phi_clas > 30) && (phi_clas <= 90 )){sector = 2;}
      else if ( (phi_clas > 90) && (phi_clas <= 150 )){sector = 3;}
      else if ( (phi_clas > 150) && (phi_clas <= 210 )){sector = 4;}
      else if ( (phi_clas > 210) && (phi_clas <= 270 )){sector = 5;}
      else if ( (phi_clas > 270) && (phi_clas <= 330 )){sector = 6;}
      else{cout<<"UNDEFINED SECTOR"<<endl; return false;}
      //get phidiff
      float phidiff = (60.*(float)(sector - 1)) - phi_clas;
      
      float fidvar[2] = {Pmom,Theta};
      
      float lowReg = 0;
      lowReg = FidFunc_pos(fidvar,parL[sector - 1]);
      float highReg = 0;
      highReg = FidFunc_pos(fidvar,parH[sector - 1]);
      
      //Now its time to determine true false based on the input of tight, loose, nominal
      float pass_low, pass_high;
      
      if(type_of_cut=="tight"){
        pass_low = lowReg + 2;
        pass_high = highReg - 2;
        if ((phidiff > pass_low) && (phidiff < pass_high))
        {
          return true;
        }else{return false;}
      }
      else if(type_of_cut=="loose"){
        pass_low = lowReg - 2;
        pass_high = highReg + 2;
        if ((phidiff > pass_low) && (phidiff < pass_high))
        {
          return true;
        }else{return false;}
      }
      else if(type_of_cut=="nominal"){
        pass_low = lowReg;
        pass_high = highReg;
        if ((phidiff > pass_low) && (phidiff < pass_high))
        {
          return true;
        }else{return false;}
      }
      else{cout<<"Did not enter correct type of cut "<<endl;return false;}
    }
  }
}


#endif /* __CLAS_G12_POSPARTICLE_FIDUCIALCUTS_HPP__*/


//////////////////////////////////////////////////
//c style 2d function
float FidFunc_pos(float *x, float *par)
{
  float xx = x[0]; //momentum
  float yy = x[1]; //theta
  float a = (par[0]*pow(xx,par[1]) + par[2]);
  float b = (par[3]*pow(xx,par[4]) + par[5]);
  float c = (par[6]*pow(xx,par[7]) + par[8]);
  float f =  a -  b/(yy - c);
  return 	f;
}

