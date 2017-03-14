//This is a header file intended to be used with the G12 data stream.
//Corrections were derived from the p pi_plus pi_minus exclusive reaction
//Input: EC inner energy, EC outer energy, U, V, W, and Sector
//Output: Pass or fail

//Updated: February 27, 2014

//Corrections were done by Michael C. Kunkel
//mkunkel@jlab.org
#ifndef __CLAS_G12_ECKNOCKOUT_HPP__
#define __CLAS_G12_ECKNOCKOUT_HPP__

#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

using namespace std;

namespace clas
{
  namespace g12 {
    
    bool pass_g12_ec_knockout(float ECin, float ECout, float u, float v, float w, int sec){
      int passed;// = 1;

//First lets take normal fiducial cuts

      if ((u>=20.0) && (u<=400.0) && (v<=375.0) && (w<=405.0)) {
        passed = 1;
      }
      else{
        return false;
      }
 
//End Fiducial cuts
 
//Start EC Knockout
      if (sec ==1) {
        if (ECin>0.) {
          if (w<=324. && w>=312.) {
            passed = 0;
            //return passed;
          }
        }
      }
      
      else if (sec ==2) {
        if (ECin>0.) {
          if ((u<=108. && u>=96.) || (u<=336. && u>=324.) || (w<=408. && w>=396.)) {
            passed = 0;
            //return passed;
          }
        }
        if (ECout>0.) {
          if ((u<=336. && u>=324.)) {
            passed = 0;
            //return passed;
          }
        }
      }
      
      else if (sec ==3) {
        if (ECin>0.) {
          if ((u<=336. && u>=324.) || (u<=216. && u>=180.) || (u<=337. && u>=324.)|| (w<=408. && w>=396.)) {
            passed = 0;
            //return passed;
          }
        }
        if (ECout>0.) {
          if ((u<=142. && u>=131.) || (u<=216. && u>=204.) || (u<=336. && u>=324.)) {
            passed = 0;
            //return passed;
          }
        }
      }
      
      else if (sec ==5) {
        if (ECin>0.) {
          if ((v<=342. && v>=320.) || (v<=242. && v>=254.) || (w<=372. && w>=336.) || (w<=312. && w>=288.) || (w<=276. && w>=240.) || (w<=228. && w>=168.) || (w<=144. && w>=132.)) {
            passed = 0;
            //return passed;
          }
        }
        if (ECout>0.) {
          if ((u<=192. && u>=180.) || (u<=240. && u>=204.)) {
            passed = 0;
            //return passed;
          }
        }
      }
      
      else if (sec ==6) {
        if (ECin>0.) {
          if ((w>=396.)) {
            passed = 0;
            //return passed;
          }
        }
      }
      //else{passed = 1;}
      if (passed) {
				return true;
			}
			else {
				return false;
			}
      
    }
    
  }
}

#endif /* __CLAS_G12_ECKNOCKOUT_HPP__ */
