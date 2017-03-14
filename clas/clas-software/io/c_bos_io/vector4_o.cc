// C++ four vector utilities 

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

vector4_o::vector4_o(){
  t = 0.0;
}

vector4_o::vector4_o(vector4_t vec){
  this->init(vec);
}

void vector4_o::init(vector4_t vec){
  t = vec.t;
  space.init(vec.space);  
}

vector4_o vector4_o::operator+(vector4_o &vec){
  vector4_o tmp;
  tmp.space = space + vec.space;
  tmp.t = t + vec.t;
  return tmp;
}

vector4_o vector4_o::operator-(vector4_o &vec){
  vector4_o tmp;
  tmp.space = space - vec.space;
  tmp.t = t - vec.t;
  return tmp;
}

float vector4_o::operator*(vector4_o &vec){
  return ( t*vec.t - space*vec.space);
}

float vector4_o::mag(){
  float tmp = (*this) * (*this);
  if (tmp > 0.0) return sqrt(tmp);
  return -1.0;
}

float vector4_o::magsq(){
  return (*this)*(*this);
}

vector4_t vector4_o::vector4_c(){
  vector4_t tmp;
  tmp.t = t;
  tmp.space.x = space.v.x;
  tmp.space.y = space.v.y;
  tmp.space.z = space.v.z;
  return tmp;
}

void vector4_o::print(FILE *fp){
  fprintf(fp, "t: %f, space: x: %f, y: %f, z:%f", t, space.v.x, space.v.y, space.v.z );
}
