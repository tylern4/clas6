// c wrapper for g10 momentum correction routine
// returns new four vector
// use include/g10pcor.h for function prototype in calling routine
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <math.h>
#include <kinematics.h>

int c_g10pcor(int itorus, int iipid, int iicor, vector4_t *p4)
{
  int iistat=-9;
  float mass=-9.;
  float pold[3]={0.,0.,0.};
  float pnew[3]={0.,0.,0.};

  //  printf("p4 =  before corr "); v4print(stdout,*p4);

  mass = v4mass(*p4);
  pold[0] = p4->space.x;
  pold[1] = p4->space.y;
  pold[2] = p4->space.z;

  g10pcor_(&itorus,&iipid,&pold,&iicor,&pnew,&iistat);

  p4->space.x = pnew[0];
  p4->space.y = pnew[1];
  p4->space.z = pnew[2];
  p4->t = sqrt( mass*mass + v3magsq(p4->space) );

  //  printf("p4c = after corr "); v4print(stdout,*p4);

  return iistat;
}
