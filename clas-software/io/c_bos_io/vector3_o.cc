// three vector utilities based on vector3.c

extern "C" {
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <clas_cern.h>
#include <particleType.h>
#include <kinematics.h>
#include <printBOS.h>
#include <utility.h>
}

//#include <iostream.h>
#include <math.h>
#include <kinematics_o.h>


vector3_o::vector3_o(){
  v.x = v.y = v.z = 0;
}

void vector3_o::init(vector3_t vec){
  v = vec;
}

vector3_o::vector3_o(vector3_t vec){
  init(vec);
}

vector3_o vector3_o::operator+(vector3_o &vec){
  vector3_o tmp(v3add(v, vec.v));
  return tmp;
}

vector3_o vector3_o::operator-(vector3_o &vec){
  vector3_o tmp(v3sub(v, vec.v));
  return tmp;
}

float vector3_o::operator*(vector3_o &vec){
  return v3dot(v, vec.v);
}

vector3_o vector3_o::operator/(vector3_o &vec){
  vector3_o tmp(v3cross(v, vec.v));
  return tmp;
}

float vector3_o::mag(){
  return v3mag(v);
}

float vector3_o::magsq(){
  return v3magsq(v);
}
