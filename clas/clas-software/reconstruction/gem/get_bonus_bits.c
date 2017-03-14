/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include "rtpc.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
int get_bonus_bits()
/* Extract bonusbits pattern from special channels of altro data */
{
  int i,j,bits;
  float xtrq[8];

  for (i=0; i<8; i++){ xtrq[i]= 0.0;}
  for (j=15; j<25; j++)
    {
      xtrq[0]+= xtra_dat[j][58];  /* left bit #0 */
      xtrq[4]+= xtra_dat[j][122]; /* right bit #0 */
    }
  bits= 0;
  for (i=0; i<8; i++)
    {
      if(xtrq[i]>1000) bits |= (1 << i);
    }
  return bits;
}
