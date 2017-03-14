
//This is a header file intended to be used with the G12 data stream.
//CProgram to recalculate EC u, v, w from the x y z coordinate system
//Input: x y z
//Output: u v w

//Updated: February 27, 2014

//Corrections were done by Michael C. Kunkel
//mkunkel@jlab.org
#ifndef __CLAS_G12_ECXYZ2UVW_HPP__
#define __CLAS_G12_ECXYZ2UVW_HPP__

#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>
#include <TVector3.h>

using namespace std;

namespace clas
{
  namespace g12 {
    
    TVector3 g12_ECxyz_2uvw(float x, float y, float z){
      TVector3 UVW;
      
      float ec_the = 0.4363323;
      float ylow = -182.974;
      float yhi = 189.956;
      float tgrho = 1.95325;
      float sinrho = 0.8901256;
      float cosrho = 0.455715;
      float u, v, w;
      float rot11, rot12, rot13;
      float rot21, rot22, rot23;
      float rot31, rot32, rot33;
      //float x, y, z;
      float xi, yi, zi;
      float ec_phi, phi;
      
      phi=atan2(y,x)*57.29578;
      if(phi<=0.)phi=phi+360.;
      phi=phi+30.;
      if(phi>=360.)phi=phi-360.;
      ec_phi=int(phi/60.)*1.0471975;
      
      rot11 = cos(ec_the)*cos(ec_phi);
      rot12 = -sin(ec_phi);
      rot13 = sin(ec_the)*cos(ec_phi);
      rot21 = cos(ec_the)*sin(ec_phi);
      rot22 = cos(ec_phi);
      rot23 = sin(ec_the)*sin(ec_phi);
      rot31 = -sin(ec_the);
      rot32 = 0.0;
      rot33 = cos(ec_the);
      
      yi = x*rot11 + y*rot21 + z*rot31;
      xi = x*rot12 + y*rot22 + z*rot32;
      zi = x*rot13 + y*rot23 + z*rot33;
      zi-=510.32;
      
      u = (yi-ylow)/sinrho;
      v = (2.0*yhi-ylow-yi)/tgrho - xi;
      w=((yhi-ylow)/tgrho + xi + (yhi-yi)/tgrho)/2./cosrho;
      
      UVW.SetXYZ(u,v,w);
      return UVW;
      
    }
  }
}

#endif /* __CLAS_G12_ECXYZ2UVW_HPP__ */




