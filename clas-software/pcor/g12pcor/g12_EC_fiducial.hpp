//This is a header file intended to be used with the G12 data stream.
//Purpose: EC fiducial cuts
//Input: EC u, v, w,
//Output: Pass or fail

//Updated: February 27, 2014

//Corrections were done by Michael C. Kunkel
//mkunkel@jlab.org
#ifndef __CLAS_G12_ECFIDUCIAL_HPP__
#define __CLAS_G12_ECFIDUCIAL_HPP__

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
		
		bool pass_g12_ec_fiducial(float u, float v, float w){
			int passed;
			
			if ((u>=20.0) && (u<=400.0) && (v<=375.0) && (w<=405.0)) {
				return true;
			}
			else{
				return false;
				//return passed;
			}
			
		}
		
	}
}

#endif /* __CLAS_G12_ECFIDUCIAL_HPP__ */
