#include <cmath>
#include <cstdlib>
#include <cstdio>
#include "smear_mc.h"
//_____________________________________________________________________________

// Function Prototypes:
double RandomGaus(double __mean,double __sigma);

// External Functions:

extern double GetLambdaTrack(const float __p3[3]);
extern double GetPhiTrack(const float __p3[3]);
extern void SetP3FromTrackingParameters(double __p,double __lambda,
					double __phi,int __sector,
					float __p3[3]);
extern int GetSector(const float __p3[3]);
//_____________________________________________________________________________

// FORTRAN wrappers:

void InitSmearMC_(int *__seed){
  InitSmearMC(*__seed);
}

void SmearMomentum_(float __p3[3],float *__mass,float *__sig_p_track,
		    float *__sig_lam_track,float *__sig_phi_track,int *__q,
		    float __p3smear[3]){
  SmearMomentum(__p3,*__mass,*__sig_p_track,*__sig_lam_track,*__sig_phi_track,
		*__q,__p3smear);
}
//_____________________________________________________________________________

void InitSmearMC(int __seed){
  srand((unsigned)__seed);
}
//_____________________________________________________________________________

void SmearMomentum(const float __p3[3],float __mass,float __sig_p_track,
		   float __sig_lam_track,float __sig_phi_track,int __q,
		   float __p3smear[3]){

  double px = __p3[0],py = __p3[1],pz = __p3[2];
  double p = sqrt(px*px + py*py + pz*pz);
  double theta = atan2(sqrt(px*px + py*py),pz)*180./3.14159;
  double dp_ex = 0.002;
  if(__mass > 0.7) { // protons
    if(p > 0.4 && p < 1.) dp_ex += 0.0005;
    if(p > 0.4 && p < 0.6) dp_ex += 0.0005;
  }
  else{ // pions or kaons
    if(__q > 0){
      if(theta > 80.) dp_ex -= 0.002;
      else if(theta > 35.){
	if(p > 1.) dp_ex -= 0.002;
	if(p < .3) dp_ex += 0.0005;
      }
      else if(theta > 25.) if(p < 0.5) dp_ex += 0.001;	
      else if(theta > 15.) {
	if(p > 1.5) dp_ex -= 0.002;
	if(p < 1.) dp_ex += 0.001;
      }
      else dp_ex += 0.001;
    }
    else{
      if(p > 2. && p < 2.5) dp_ex -= 0.0015;
      if(theta > 25. && theta < 35. && p < 0.8) dp_ex += 0.002;
      if(p > 0.4 && p < 0.7 && theta > 35. && theta < 45.) dp_ex += 0.0015;   
    }
  }

  double dp = RandomGaus(0.,__sig_p_track*1. + dp_ex);
  double dlam = RandomGaus(0.,__sig_lam_track*1.85);
  double dphi = RandomGaus(0.,__sig_phi_track*1.84);

  SetP3FromTrackingParameters(p + dp,GetLambdaTrack(__p3) + dlam,
			      GetPhiTrack(__p3) + dphi,GetSector(__p3),
			      __p3smear);
}
//_____________________________________________________________________________

/// Throws numbers according to gaussian distribution
double RandomGaus(double __mean,double __sigma){

  double y = (double)rand()/RAND_MAX;
  double z = (double)rand()/RAND_MAX;
  double x = z * 6.28318530717958623; // z * 2pi

  return __mean + __sigma*sin(x)*sqrt(-2.*log(y));
}
//_____________________________________________________________________________
