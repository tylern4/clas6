#include "rtpc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

int inside_rtpc(float xx, float yy, float zz)
/* return true if the specified point is within the active rtpc volume */
#define ANG_FUZ 0.001
#define Z_FUZ 0.01
#define R_FUZ 0.01
{
  float rr, phi;
  int answer;

  answer=0;
  rr= sqrt(xx*xx + yy*yy);
  for(phi= atan2(yy,xx);phi<0.0; phi+=2.0*PI);
  if(phi> 1.5*PI) phi-=2.0*PI; /* good grief!!*/
  if ( (rr >= CAT_RAD-R_FUZ) && 
       (rr <= GEM_RAD+R_FUZ) &&
       ( ( (phi    >= -(FBANG+ANG_FUZ)) && (phi    <= (FBANG+ANG_FUZ))) || 
	 ( (phi-PI >= -(FBANG+ANG_FUZ)) && (phi-PI <= (FBANG+ANG_FUZ))) ) &&
       (zz >= -(ZEND+Z_FUZ)) &&
       (zz <=  (ZEND+Z_FUZ)) ) answer= 1;

/*    if(!quiet) printf("inside_rtpc tested (x,y,z)= (%5.1f, %5.1f, %5.1f) (r,phi,z)=(%5.2f, %5.2f, %5.1f)and returned %d\n",xx,yy,zz,rr,phi,zz,answer); */
  return(answer);
}
       
		       
