#include <iostream>
#include <unistd.h>
#include <plib.h>
#include <Vec.h>
#include <matrix.h>

matrix<double> lowq2DenMat(fourVec ein,fourVec eout)
{
  double cos_t,sin_t;
  matrix<double> R(2,2);
  matrix<double> Rinv(2,2);
  matrix<double> Rot(2,2);
  matrix<double> Rotinv(2,2);
  matrix<double> denMat(2,2);
  matrix<double> denMatTrans(2,2); 

  threeVec Cross;

  double qsq = Qsq(ein.t(),eout.t(),((ein.V() - eout.V()).theta()));
  double eps = epsilon(ein.t(),eout.t(),((ein.V() - eout.V()).theta()));


  denMat.el(0,0) = 0.5 * (1 + eps);
  denMat.el(0,1) = 0.0;
  denMat.el(1,0) = 0.0;
  denMat.el(1,1) = 0.5 * (1 - eps);   

  // what is the angle between the electron plane and the y axis?

  Cross = ein.V() / eout.V();
  cos_t = Cross.y()/Cross.r();
  sin_t = 1. - cos_t * cos_t;
  Rot.el(0,0) = Rot.el(1,1) = cos_t;
  Rot.el(0,1) = -sin_t;
  Rot.el(1,0) = sin_t;
  Rotinv = Rot.inv();

  // rotate the density matrix

  denMatTrans  = Rotinv * denMat *  Rot;

  // now transform into helicity basis 
  R.el(0,0) = 1.0/sqrt(2.0);
  R.el(1,0) = 1.0/sqrt(2.0);
  R.el(0,1) = -1.0/sqrt(2.0);
  R.el(1,1) = 1.0/sqrt(2.0);

  Rinv = R.inv();
	

   denMat = Rinv * denMatTrans * R;
  return(denMat);
}
