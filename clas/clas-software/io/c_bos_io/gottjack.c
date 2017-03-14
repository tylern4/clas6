/*
*
*  gottjack.c
*
*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <ntypes.h>
#include <kinematics.h>

float costheta_gj(vector4_t decay, vector4_t beam, vector4_t product, vector4_t target, vector4_t recoil)
{
  vector4_t decayboost, beamboost, productboost, targetboost, recoilboost;
  vector3_t  beta, x_hat, y_hat, z_hat;
  float r,cosine;

  beta = v3div(decay.t,decay.space);

  decayboost = v4boost(beta,decay);
  beamboost = v4boost(beta,beam);
  productboost = v4boost(beta,product);
  targetboost = v4boost(beta,target);
  recoilboost = v4boost(beta,recoil);

  z_hat = v3norm(beamboost.space);

  r = v3mag(productboost.space);

  cosine = v3dot(productboost.space,z_hat) / r;

  return cosine;
}

float phi_ty(vector4_t decay, vector4_t beam, vector4_t product, vector4_t target, vector4_t recoil)
{
  vector4_t decayboost, beamboost, productboost, targetboost, recoilboost;
  vector3_t beta, x_hat, y_hat, z_hat, cross_product1, cross_product2;
  float r,phi,costheta,sintheta,cosphi,sinphi;
  float check;

  beta = v3div(decay.t,decay.space);

  decayboost = v4boost(beta,decay);
  beamboost = v4boost(beta,beam);
  productboost = v4boost(beta,product);
  targetboost = v4boost(beta,target);
  recoilboost = v4boost(beta,recoil);

  cross_product1 = v3cross(targetboost.space, recoilboost.space);

  z_hat = v3norm(beamboost.space);
  y_hat = v3norm(cross_product1);
  cross_product2 = v3cross(y_hat,z_hat);
  x_hat = v3norm(cross_product2);

  r = v3mag(productboost.space);

  costheta = v3dot(productboost.space,z_hat) / r;
  sintheta = sqrt(1 - (costheta*costheta));

  if (sintheta == 0.0) {
    cosphi = 1.0;
    sinphi = 0.0;
  }
  else {
    cosphi = v3dot(productboost.space,x_hat) / (r * sintheta);
    sinphi = v3dot(productboost.space,y_hat) / (r * sintheta);
  }

  if(fabs(cosphi)>1.0){
    phi=-9999.0;
  }
  else{
    phi = acos(cosphi);
    if(sinphi<0) phi = 2*PI - phi;
  }

  return phi;
}

float phi_ty_deg(vector4_t decay, vector4_t beam, vector4_t product, vector4_t target, vector4_t recoil){
  return phi_ty(decay, beam, product, target, recoil)*180/PI;
}


float costheta_cm(vector4_t rest, vector4_t analyzer){
  vector3_t beta_cm, dir_cos;
  vector4_t analyzer_cm;
  float cosine;
  /*assume beam is in the z direction*/

  beta_cm = v3div(rest.t,rest.space);  
  analyzer_cm = v4boost(beta_cm, analyzer);
  dir_cos = v3norm(analyzer_cm.space);

  return dir_cos.z;
}







