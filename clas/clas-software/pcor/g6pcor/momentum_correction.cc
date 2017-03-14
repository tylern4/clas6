//momentum correction routines for g6c
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <map_manager.h>
#include <particleType.h>

#include <Vec.h>
#include <lorentz.h>
#include <matrix.h>

using namespace std;

//use global variales for holding map entries
//the correction coefficients are sector dependent
//for each sector there are 6 numbers

float fit_par_beam;
float fit_par_pip[36];//for Pi+
float fit_par_pim[36];//for Pi-
float fit_par_p[36];//for Proton

#include <momentum_correction.h>

void init_momentum_correction(int runno)
{
  //this is the initialization routine which reads in the map entries
  //and fills the corresponding parameter array
 
  static int CurrentRun = -1;
  static int Initialized = 0 ;

  char * dir;
  char map[100];
  int firsttime;

  if (runno != CurrentRun){
    CurrentRun = runno;
    Initialized = 1;
    
    dir = getenv("CLAS_PARMS");
    sprintf(map, "%s/Maps/g6c_mom_corr.map", dir);
    
    map_get_float(map, "BEAM", "value", 1, &fit_par_beam, runno, &firsttime);
    
    for(int i = 0; i < 36; i++){
      map_get_float(map, "PROTON", "value", 36, fit_par_p, runno, &firsttime);
      map_get_float(map, "PIPLUS", "value", 36, fit_par_pip, runno, &firsttime);
      map_get_float(map, "PIMINUS", "value", 36, fit_par_pim, runno, &firsttime);
    }
  }else{
    if(!Initialized){
      dir = getenv("CLAS_PARMS");
      sprintf(map, "%s/Maps/g6c_mom_corr.map", dir);
      
      map_get_float(map, "BEAM", "value", 1, &fit_par_beam, runno, &firsttime);
      
      for(int i = 0; i < 36; i++){
	map_get_float(map, "PROTON", "value", 36, fit_par_p, runno, &firsttime);
	map_get_float(map, "PIPLUS", "value", 36, fit_par_pip, runno, &firsttime);
	map_get_float(map, "PIMINUS", "value", 36, fit_par_pim, runno, &firsttime);
      }
    }
  }
}

fourVec momentum_correction(fourVec P, int sector, int pid)
{
  fourVec ret = P;
  float pars[36];
  static int printed = 0;

  switch (pid){
  case Proton:
    for(int i = 0; i < 36; i++){
      pars[i] = fit_par_p[i];
    }
    break;
    
  case PiPlus:
    for(int i = 0; i < 36; i++){
      pars[i] = fit_par_pip[i];
    }
    break;
    
  case PiMinus:
    for(int i = 0; i < 36; i++){
      pars[i] = fit_par_pim[i];
    }
    break;
    
  case KPlus:
    for(int i = 0; i < 36; i++){
      pars[i] = fit_par_pip[i];
    }
    break;
    
  case KMinus:
    for(int i = 0; i < 36; i++){
      pars[i] = fit_par_pim[i];
    }
    break; 

  case Gamma:
  case Neutron:
    break;
    
  default:
    if(printed < 10){
      cerr << "g6c momentum correction: Particle type " << pid << "  not supported !!!" << endl;
      printed++;
    }
    return (ret);
    break;
  }
  
  float cth = P.V().z() / ~P.V();
  float sth = sqrt(1-cth*cth);
  float phi = atan2(P.V().y(), P.V().x());
  float p = ~P.V();
  float Mp = sqrt(P.t()*P.t() - ~P.V()*~P.V());
  
  float dp = pars[6*(sector-1)+0]+pars[6*(sector-1)+1]*cth+pars[6*(sector-1)+2]*p+pars[6*(sector-1)+3]*cth*cth+pars[6*(sector-1)+4]*p*p+pars[6*(sector-1)+5]*cth*p;
  
  float x = p - dp;
  
  float px = x * sth * cos(phi);
  float py = x * sth * sin(phi);
  float pz = x * cth;
  float E = sqrt(Mp*Mp + px*px + py*py + pz*pz);
  
  ret.set(E, px, py, pz);
  
  return (ret);
}

fourVec photon_beam_correction(fourVec P)
{
  fourVec ret = P;
  float beamErgCorr = 0.0;
  float beamErg = P.t();

  //THIS IS FOR THE BEAM PHOTON ENERGY CORRECTION!!!
  beamErgCorr = fit_par_beam;//precautionary measure
  beamErg = (1.0 + beamErgCorr) * P.t();
  ret.set(beamErg, 0.0, 0.0, beamErg);
  
  return (ret);
}

fourVec photon_beam_correction(fourVec P, float beamErgCorr = 0.0)
{
  fourVec ret = P;
  float beamErg = P.t();

  //THIS IS FOR THE BEAM PHOTON ENERGY CORRECTION!!!
  beamErg = (1.0 + beamErgCorr) * P.t();
  ret.set(beamErg, 0.0, 0.0, beamErg);
  
  return (ret);
}
