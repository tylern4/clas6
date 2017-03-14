//This is a header file intended to be used with the G12 data stream.
//Purpose: Perform TOF knockouts
//Based off pi+ theta vs phi  plots
//Input: theta phi sector
//Output: Pass or fail

//Updated: February 27, 2014

//Corrections were done by Michael C. Kunkel
//mkunkel@jlab.org
#ifndef __CLAS_G12_TOFKNOCKOUT_HPP__
#define __CLAS_G12_TOFKNOCKOUT_HPP__

#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>
//X axis = phi Y axis = theta
//Sector1
//TLine *L2 = new TLine(0,24,30,26)
//TLine *L1 = new TLine(0,21.5,30,23.5)
//
// -10. >=Phi <=0.
//TLine *L2 = new TLine(-30,26,0,24)
//TLine *L1 = new TLine(-30,23.5,0,21.5)
//
//Sector 3
//TLine *L2 = new TLine(120,20.5,150,22)
//TLine *L1 = new TLine(120,16,150,17.5)
//
//TLine *L1 = new TLine(90,17.5,120,16)
//TLine *L2 = new TLine(90,22,120,20.5)

using namespace std;

namespace clas
{
	namespace g12 {
		
		bool pass_g12_TOF_knockout(float theta, float phi, int sec){
			int passed;
			
			
			double phi_shifted = fmod((phi+390.),360);
			phi_shifted = fmod(phi_shifted,60) - 30.;
			
			
			//Start EC Knockout
			if (sec ==1) {
				if (phi<0. && theta <= 24.+((26.-24.)/-30.)*phi && theta >= 21.5+((23.5-21.5)/-30.)*phi) {
					return false;
				}
				else if(phi>=0. && theta <= 24.+((26.-24.)/30.)*phi && theta >= 21.5+((23.5-21.5)/30.)*phi){
					return false;
				}
				else{
					return true;
				}
			}
			
			
			else if (sec ==3) {
				if (phi_shifted<0. && theta <= 20.5+((22.-20.5)/-30.)*phi_shifted && theta >= 16.+((17.5-16.)/-30.)*phi_shifted) {
					return false;
				}
				else if(phi_shifted>=0. && theta <= 20.5+((22.-20.5)/30.)*phi_shifted && theta >= 16.+((17.5-16.)/30.)*phi_shifted){
					return false;
				}
				else{
					return true;
				}
			}
			else{
				return true;
			}
			
		}
		
	}
}

#endif /* __CLAS_G12_TOFKNOCKOUT_HPP__ */
