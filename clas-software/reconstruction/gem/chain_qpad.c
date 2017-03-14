#include "rtpc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

/* stuff for calling fortran routines and linking cernlib */
#define  f2cFortran

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
void chain_qpad(int chainnum)

     /* for each pad contributing to the chain, computes and fills
    histogram with the ratio of that pad's measured charge to
        the average charge/pad along the whole chain*/
{

  int ii, jj, pad_num, old_pad;
  int num_pads, used;
  float sumq, thisq, prow, padz;
  typedef struct {
    int pad;
    float qsum;
  } listt;
#define MAXPADLIST 100
  listt pad_list[MAXPADLIST];

/*       chainnum= track_chain[tracknum]; */

  num_pads=0;
  sumq= 0.0;

  for (jj=0; jj< num_hits_this_chain[chainnum]; jj++)
    {
      if(hh_hitlist[chain_hits[chainnum][jj]].status & HISUSED)
    {
      /* get the total charge on this chain */
      sumq+= hh_hitlist[chain_hits[chainnum][jj]].q;

      /* identify this pad */
      pad_num= hh_hitlist[chain_hits[chainnum][jj]].pad;
      thisq= hh_hitlist[chain_hits[chainnum][jj]].q;

      /* if this pad has already been seen on this chain add this hit's
         charge to the sum for this pad */
      used= FALSE;
      for (ii=0; (ii<num_pads)&&(!used); ii++)
        {
          old_pad= pad_list[ii].pad;
          if(old_pad == pad_num)
        {
          pad_list[ii].qsum += thisq;
          used= TRUE;
        }
        }
      /* if it wasn't already seen, create and initialize a new entry */
      if(!used)
        {
          if(num_pads < MAXPADLIST)
        {
          pad_list[num_pads].pad= pad_num;
          pad_list[num_pads].qsum = thisq;
          num_pads++;
        }
          else
        {
          printf("Too many pads on padlist in chain_qpad. Pad ignored.\n");
        }
        }
    }
    }
  /* Require that at least 5 pads contributed to this chain and fill histogram */
  if(num_pads >4)
    {
      sumq /= (float)num_pads;  /* average charge per pad on this chain */
      for(ii=0; ii<num_pads; ii++)
    {
      pad_num= pad_list[ii].pad;
      thisq= pad_list[ii].qsum;
      //prow= pad_row(padLocs[pad_num].phi);
      padz= padLocs[pad_num].z;

      /* histo of number of uses of this pad on a chain */
      //hf2(2030, padz, prow, 1.0);

      /* histo of pad's charge relative to other pads on this chain */
      //hf2(2031, padz, prow, thisq/sumq);

    }
    }
  return;
}

