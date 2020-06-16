/*----------------------- sc_channel_status.c ---------------------*/
/*   Contains functions to return the status (hardware, and eventually
 *   calibration status) of a given channel. Provided so you can
 *   cut out these channels simply.
 *   Assumes that sc_init has been called so set up the TOF calibration
 *   structure.
 */

#include <stdio.h>
#include <stdlib.h>
#include <ntypes.h>
#include <bostypes.h>
#include <sc.h>
#include <scExtern.h>

int sc_hdwr_channel_status_(int *sector, int *channel) {
  return sc_HDWR_channel_status(*sector,*channel);
}

int sc_HDWR_channel_status(int sector, int channel) {
  /*
   *   Purpose:
   *      Returns the HARDWARE status for a given channel.
   *   input:
   *      sector, channel
   *   returns:
   *      10*(left status) + (right status)
   *       where left status and right status come from
   *       the Map -> status -> left, right.
   *
   *       left status, right status are coded as:
   *       (from Costy's e-mail 21-Jul-98, "TOF status word"
   *             0 ----> all's fine (as it seems);
   *             1 ----> no ADC signal;
   *             2 ----> no TDC signal;
   *             3 ----> no ADC, no TDC;
   *             5 ----> something funny on the reconstruction level.
   */
  
  int ret;
  if ( (sector >=1) && (sector <=6) &&
       (channel >= 1) && (channel <= SC_NPADDLES_SEC ) ) {

    int index = sc_index(sector,channel);

    ret = sc_status.left[index]*10 + sc_status.right[index];
  }
  else {
    fprintf(stderr,"sc_hdwr_channel_status: Bad Sector/Channel number (%d : %d)\n",sector,channel);
    ret = 55;
  }
  
  return ret;
}
