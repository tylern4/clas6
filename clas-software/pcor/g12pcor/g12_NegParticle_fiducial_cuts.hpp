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

#ifndef __CLAS_G12_NEGPARTICLE_FIDUCIALCUTS_HPP__
#define __CLAS_G12_NEGPARTICLE_FIDUCIALCUTS_HPP__

#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>
#include <cmath>
#define DEG_2_RAD_CONV 0.01745328

float FidFunc_neg(float *, float *);
using namespace std;

namespace clas
{
  namespace g12 {
    
    bool g12_NegParticle_fiducial_cuts(float Pmom, float Theta, float Phi, const std::string& type_of_cut ){
      
      const int Ns = 6;  //number of sectors
      float parL[Ns][9];
      float parH[Ns][9];
      parL[0][0] = -15810.2; parL[0][1] = 9.9663e-05; parL[0][2] = 15783.2;
      parL[0][3] = -538215; parL[0][4] = 9.84788e-05; parL[0][5] = 538135;
      parL[0][6] = -678156; parL[0][7] = 1.42357e-05; parL[0][8] = 678167;
      
      parL[1][0] = -15810.7; parL[1][1] = 0.0001; parL[1][2] = 15783.7;
      parL[1][3] = -538209; parL[1][4] = 8.31014e-05; parL[1][5] = 538141;
      parL[1][6] = -678155; parL[1][7] = 1.32519e-05; parL[1][8] = 678167;
      
      parL[2][0] = -37808.6; parL[2][1] = 4.38676e-05; parL[2][2] = 37782.3;
      parL[2][3] = -678187; parL[2][4] = 3.0748e-05; parL[2][5] = 678136;
      parL[2][6] = -678155; parL[2][7] = 1.13289e-05; parL[2][8] = 678168;
      
      parL[3][0] = -37809.1; parL[3][1] = 5.52776e-05; parL[3][2] = 37781.8;
      parL[3][3] = -678197; parL[3][4] = 4.21772e-05; parL[3][5] = 678126;
      parL[3][6] = -678156; parL[3][7] = 9.59678e-06; parL[3][8] = 678167;
      
      parL[4][0] = -37809.2; parL[4][1] = 5.79073e-05; parL[4][2] = 37781.7;
      parL[4][3] = -678207; parL[4][4] = 7.72987e-05; parL[4][5] = 678115;
      parL[4][6] = -678156; parL[4][7] = 1.63781e-05; parL[4][8] = 678166;
 
      //real from Jason Bono
      
//      parL[5][0] = -37808.9; parL[5][1] = 4.70858e-05; parL[5][2] = 37781.9;
//      parL[5][3] = -678198; parL[5][4] = 9.27108e-05; parL[5][5] = 678125;
//      parL[5][6] = -678155; parL[5][7] = 7.45841e-06; parL[5][8] = 678167;
 
      //End real
      
      //test parameters: changed par[5][7] to par[4][7]
            parL[5][0] = -37808.9; parL[5][1] = 4.70858e-05; parL[5][2] = 37781.9;
            parL[5][3] = -678198; parL[5][4] = 9.27108e-05; parL[5][5] = 678125;
            parL[5][6] = -678155; parL[5][7] = 1.63781e-05; parL[5][8] = 678167;
      
//      // end test
      
      parH[0][0] = -15783.8; parH[0][1] = -0.0001; parH[0][2] = 15809.5;
      parH[0][3] = -538148; parH[0][4] = -8.77852e-05; parH[0][5] = 538201;
      parH[0][6] = -678155; parH[0][7] = 1.67856e-05; parH[0][8] = 678168;
      parH[1][0] = -37782.2; parH[1][1] = -9.78359e-05; parH[1][2] = 37808.7;
      parH[1][3] = -678123; parH[1][4] = -9.99919e-05; parH[1][5] = 678199;
      parH[1][6] = -678155; parH[1][7] = 1.71348e-05; parH[1][8] = 678168;
      parH[2][0] = -37782.3; parH[2][1] = -6.62596e-05; parH[2][2] = 37808.6;
      parH[2][3] = -678133; parH[2][4] = -4.84207e-05; parH[2][5] = 678190;
      parH[2][6] = -678155; parH[2][7] = 1.47442e-05; parH[2][8] = 678168;
      parH[3][0] = -37782.5; parH[3][1] = -3.18908e-05; parH[3][2] = 37808.4;
      parH[3][3] = -678135; parH[3][4] = -1.21321e-05; parH[3][5] = 678188;
      parH[3][6] = -678155; parH[3][7] = 1.22068e-05; parH[3][8] = 678168;
      
      parH[4][0] = -37782.3; parH[4][1] = -6.28596e-05; parH[4][2] = 37808.5;
      parH[4][3] = -678136; parH[4][4] = -0.0001; parH[4][5] = 678187;
      parH[4][6] = -678154; parH[4][7] = 1.79547e-05; parH[4][8] = 678168;
      parH[5][0] = -37782.4; parH[5][1] = -6.08308e-05; parH[5][2] = 37808.4;
      parH[5][3] = -678131; parH[5][4] = -5.88378e-05; parH[5][5] = 678192;
      parH[5][6] = -678155; parH[5][7] = 1.44427e-05; parH[5][8] = 678168;
      
      
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
      lowReg = FidFunc_neg(fidvar,parL[sector - 1]);
      float highReg = 0;
      highReg = FidFunc_neg(fidvar,parH[sector - 1]);
      
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


#endif /* __CLAS_G12_NEGPARTICLE_FIDUCIALCUTS_HPP__*/


//////////////////////////////////////////////////
//c style 2d function
float FidFunc_neg(float *x, float *par)
{
  float xx = x[0]; //momentum
  float yy = x[1]; //theta
  float a = (par[0]*pow(xx,par[1]) + par[2]);
  float b = (par[3]*pow(xx,par[4]) + par[5]);
  float c = (par[6]*pow(xx,par[7]) + par[8]);
  float f =  a -  b/(yy - c);
  return 	f;
}

