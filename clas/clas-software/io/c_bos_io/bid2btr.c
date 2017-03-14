/*
 *  routines to go from ?bid banks -> ?btr banks
 *
 *   so we can go backwards to check the track of a particle,
 *   as well as just forwards.
 */

#include <stdio.h>
#include <ntypes.h>
#include <bostypes.h>
#include <utility.h>

tbtr_t * tbid2tbtr(tbid_t *tbid) {
  /* pass in pointer to the tbid element for the track you want.
   *  output: pointer to tbtr track element
   */

  int hbtrk = tbid->track;  /* number of the track in HB tracking */

  clasTBTR_t *TBTR = getBank(&bcs_,"TBTR");

  tbtr_t *ret = NULL;  /* just fall through and return NULL if track not found */

  tbtr_t *tbtr;
  int i;

  if ( (TBTR) &&
       (hbtrk > 0) ) { /* if this actually has an associated track (not neutral) */
    for (i=0; i < TBTR->bank.nrow; i++) {
      tbtr = &(TBTR->tbtr[i]);
      
      if (tbtr->itr_hbt == hbtrk) {
	ret = tbtr;
	break;
      }
    }
  }

  return ret;
}

hbtr_t * hbid2hbtr(hbid_t *hbid) {
  /* input: pointer to hbid element (row)
   * return: pointer to hbtr element for the associated track
   *         NULL if track not found (probably a neutral)
   */

  int hbtrk = hbid->track;  /* number of track in HB tracking */

  hbtr_t *ret = NULL;

  clasHBTR_t *HBTR = getBank(&bcs_,"HBTR");

  if ( (HBTR) &&
           /* check track number is in range */
       (hbtrk > 0) && (hbtrk <= HBTR->bank.nrow) ) {
    ret = &(HBTR->hbtr[hbtrk-1]);
  }

  return ret;
}
