#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <math.h>
#include <bostypes.h>
#include <utility.h>

vector3_t lab2sector(vector3_t lab, int sector){
  vector3_t sec;
  float rot_angle = (60.0*(sector-1)/180)*3.14159;
  sec.x = lab.x*cos(rot_angle) + lab.y*sin(rot_angle);
  sec.y = - lab.x*sin(rot_angle) + lab.y*cos(rot_angle);
  sec.z = lab.z;
  return(sec);
}

vector3_t sector2lab(vector3_t sec, int sector){
  vector3_t lab;
  float rot_angle = -(60.0*(sector-1)/180)*3.14159;
  lab.x = sec.x*cos(rot_angle) + sec.y*sin(rot_angle);
  lab.y = - sec.x*sin(rot_angle) + sec.y*cos(rot_angle);
  lab.z = sec.z;
  return(lab);
}


vector3_t trk2sec(vector3_t trk){
  vector3_t sec;
  sec.x = trk.y;
  sec.y = trk.z;
  sec.z = trk.x;
  return(sec);
}

vector3_t sec2trk(vector3_t sec){
  vector3_t trk;
  trk.x = sec.z;
  trk.y = sec.x;
  trk.z = sec.y;
  return(trk);
}


