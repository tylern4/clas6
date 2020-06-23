#include <stdio.h>
#include <math.h>
#include <ntypes.h>
#include <bostypes.h>
#include <kinematics.h>
#include <utility.h>


vector4_t btr2vec4(tbtr_t *tbtr, float mass){
  vector4_t track;
  track.t = sqrt(mass*mass + v3dot(tbtr->p, tbtr->p));
  return track;
}

int get_sector(vector4_t *V4){
  float tmp_phi = get_phi(V4) + 30;
  if (get_phi(V4) < -30 ) tmp_phi += 360;
  return(((int)(tmp_phi))/60 + 1);
}

float get_phi(vector4_t *V4){
  float phi;
  if (V4->space.y > 0){
    phi = acos(V4->space.x/(sqrt(pow(V4->space.x,2) + pow(V4->space.y,2))))*(RAD2DEG);
  }else if (V4->space.y < 0){
    phi = - acos(V4->space.x/(sqrt(pow(V4->space.x,2) + pow(V4->space.y,2))))*(RAD2DEG);
  }
  return(phi);
}

float get_theta(vector4_t *V4){
  return(acos(V4->space.z/(v3mag(V4->space)))*(RAD2DEG));
}

int on_centerline(vector4_t vec, float cut){
  float theta = get_phi(&vec);
  float centerline;

  for(centerline= -180.0; centerline <= 180.0; centerline+= 60.0 ) {
    if ( fabs(theta - centerline) < cut ) return 1;
  }
  return 0;
}


vector3_t lab2bogdan(vector3_t lab){
  vector3_t bogdan;
  vector4_t lab_v4;
  float alpha = 0.0;


  lab_v4.t =  1.0;
  lab_v4.space = lab; 

  alpha = (get_sector(&lab_v4) -1)*PI/3;

  bogdan.x = lab.z;
  bogdan.y = lab.x*cos(alpha) + lab.y*sin(alpha);
  bogdan.z = - lab.x*sin(alpha) + lab.y*cos(alpha);

  return(bogdan);
  
}

