/* -=======================================================- *
$Id: vector3.c,v 1.8 2004/06/27 22:17:30 fklein Exp $
$Author: fklein $
$Revision: 1.8 $
$Date: 2004/06/27 22:17:30 $
* -=======================================================- */
/*
*
*  vector3.c
*
*/

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <ntypes.h>
#include <kinematics.h>

#ifndef TINY
#define TINY 0.000001
#endif

/*-----------------------------------------------------------------------------------------*/
float v3dot(vector3_t v1,vector3_t v2)
{
  float dot;
  
  dot = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;

  return dot;
}
/*-----------------------------------------------------------------------------------------*/
vector3_t v3cross(vector3_t v1,vector3_t v2)
{
  vector3_t cross;

  cross.x = v1.y * v2.z - v1.z * v2.y;
  cross.y = v1.z * v2.x - v1.x * v2.z;
  cross.z = v1.x * v2.y - v1.y * v2.x;

  return cross;
}
/*-----------------------------------------------------------------------------------------*/
float v3magsq(vector3_t v)
{
  float magsq;

  /*  magsq = v3mag(v); */
  /*  magsq *= magsq; */
  magsq = (v.x)*(v.x) + (v.y)*(v.y) + (v.z)*(v.z);

  return magsq;
}
/*-----------------------------------------------------------------------------------------*/
float v3mag(vector3_t v)
{
  float mag;

  mag = sqrt((v.x)*(v.x) + (v.y)*(v.y) + (v.z)*(v.z));

  return mag;
}
/*-----------------------------------------------------------------------------------------*/
float v3radius(vector3_t v)
{
  float radius;

  radius = sqrt( (v.x)*(v.x) + (v.y)*(v.y));

  return radius;
}
/*-----------------------------------------------------------------------------------------*/
vector3_t v3norm(vector3_t v)
{
  vector3_t norm;
  float mag = v3mag(v);

  if( mag > TINY ) {
    norm.x = v.x / mag;
    norm.y = v.y / mag;
    norm.z = v.z / mag;
  }
  else { 
    norm.x=norm.y=norm.z=0.0; 
  }
  return norm;
}
/*-----------------------------------------------------------------------------------------*/
void v3dir(vector3_t v,float *theta_p,float *phi_p)
{
  float r=v3mag(v);

  if( r > TINY ) {
    *theta_p = acos(v.z / r);
    *phi_p   = atan2(v.y , v.x);
  }
  else {
    *theta_p=0.0; *phi_p=0.0;
  }
}

/*-----------------------------------------------------------------------------------------*/
void v3dir_deg(vector3_t v,float *theta_p,float *phi_p)
{
  float r=v3mag(v);

  if( r > TINY ) {
    *theta_p = acos(v.z / r) * 180 / PI;
    *phi_p   = atan2(v.y,v.x) * 180 / PI;
  }
  else {
    *theta_p=0.0; *phi_p=0.0;
  }
}
/*-----------------------------------------------------------------------------------------*/
float v3cos_angle(vector3_t v1,vector3_t v2)
{
  float cosine;
  float mag_v1=v3mag(v1);
  float mag_v2=v3mag(v2);

  if( mag_v1 > TINY && mag_v2 > TINY ){
    cosine = v3dot(v1,v2)/mag_v1/mag_v2;
  }
  else {
    cosine = 1.0;
  }
  return cosine;
}
/*-----------------------------------------------------------------------------------------*/
float v3angle(vector3_t v1,vector3_t v2)
{
  float angle;

  angle = acos(v3cos_angle(v1,v2));

  return angle;
}
/*-----------------------------------------------------------------------------------------*/
float v3angle_deg(vector3_t v1,vector3_t v2)
{
  float angle;

  angle = acos(v3cos_angle(v1,v2)) * 180 / PI;

  return angle;
}
/*-----------------------------------------------------------------------------------------*/
vector3_t v3add(vector3_t v1,vector3_t v2)
{
  vector3_t sum;

  sum.x = v1.x + v2.x;
  sum.y = v1.y + v2.y;
  sum.z = v1.z + v2.z;

  return sum;
}
/*-----------------------------------------------------------------------------------------*/
vector3_t v3sub(vector3_t v1,vector3_t v2)
{
  vector3_t diff;

  diff.x = v1.x - v2.x;
  diff.y = v1.y - v2.y;
  diff.z = v1.z - v2.z;

  return diff;
}
/*-----------------------------------------------------------------------------------------*/
vector3_t v3mult(float factor,vector3_t v)
{
  vector3_t mult;

  mult.x = v.x * factor;
  mult.y = v.y * factor;
  mult.z = v.z * factor;

  return mult;
}
/*-----------------------------------------------------------------------------------------*/
vector3_t v3div(float divisor,vector3_t v)
{
  vector3_t div;

  if( fabs(divisor) > TINY ){
    div.x = v.x / divisor;
    div.y = v.y / divisor;
    div.z = v.z / divisor;
  }
  else {
    div.x=div.y=div.z=0.0;
  }
  return div;
}
/*-----------------------------------------------------------------------------------------*/
vector3_t xyz2v3(void *xyz){
  vector3_t v3;
  return(*((vector3_t *)(xyz)));
}
/*-----------------------------------------------------------------------------------------*/
void v3print(FILE *stream, vector3_t vec){
  fprintf(stream, "x: %f, y:%f, z:%f\n", vec.x, vec.y, vec.z);
}
/*---------------------------------------------------------------------------*/
vector3_t Rotate3(vector3_t NewCoord3[3], vector3_t V3){
  vector3_t new_v4;

  new_v4.x = v3dot(V3, v3norm(NewCoord3[0]));
  new_v4.y = v3dot(V3, v3norm(NewCoord3[1]));
  new_v4.z = v3dot(V3, v3norm(NewCoord3[2]));

  return new_v4;
}
