/*
*
*  vector4.c
*
*/

#include <math.h>
#include <stdlib.h>
#include <ntypes.h>
#include <kinematics.h>

float v4dot(vector4_t p1,vector4_t p2)
{
  float dot;

  dot = p1.t * p2.t - p1.space.x * p2.space.x - p1.space.y * p2.space.y
	  - p1.space.z * p2.space.z;

  return dot;
}

float v4mass(vector4_t p)
{
  float mag = 0;
  float mmsq = v4dot(p,p);

  if (mmsq >= 0){
    mag = sqrt(mmsq);
  } else {
    mag = -1;
  }

  return mag;
}
float v4magsq(vector4_t p)
{
  float magsq;

  magsq = v4dot(p,p);

  return magsq;
}

vector4_t v4add(vector4_t p1,vector4_t p2)
{
  vector4_t sum;

  sum.t = p1.t + p2.t;
  sum.space.x = p1.space.x + p2.space.x;
  sum.space.y = p1.space.y + p2.space.y;
  sum.space.z = p1.space.z + p2.space.z;

  return sum;
}

vector4_t v4sub(vector4_t p1,vector4_t p2)
{
  vector4_t diff;

  diff.t = p1.t - p2.t;
  diff.space.x = p1.space.x - p2.space.x;
  diff.space.y = p1.space.y - p2.space.y;
  diff.space.z = p1.space.z - p2.space.z;

  return diff;
}

vector4_t v4mult(float factor,vector4_t p)
{
  vector4_t result;

  result.t = p.t * factor;
  result.space.x = p.space.x * factor;
  result.space.y = p.space.y * factor;
  result.space.z = p.space.z * factor;

  return result;
}

vector4_t v4div(float divisor,vector4_t p)
{
  vector4_t result;

  result.t = p.t / divisor;
  result.space.x = p.space.x / divisor;
  result.space.y = p.space.y / divisor;
  result.space.z = p.space.z / divisor;

  return result;
}


vector4_t v3_to_v4(vector3_t *vec3, float mass){
  vector4_t v4;
  
  v4.space.x = vec3->x;
  v4.space.y = vec3->y;
  v4.space.z = vec3->z;

  v4.t = sqrt(mass*mass + v3dot(v4.space, v4.space));

  return v4;
}

vector4_t txyz2v4(void *txyz){
  vector4_t v4;
  return(*((vector4_t *)(txyz)));
}


void v4print(FILE *stream, vector4_t vec){
  fprintf(stream, "t: %f, x: %f, y:%f, z:%f\n", vec.t, vec.space.x, vec.space.y, vec.space.z);
}



