#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <math.h>
#include <bostypes.h>
#include <sc.h>
#include <pid.h>
#include <utility.h>

int reMatchSCRC()
{
  match_t mat;
  int sec,ntrk;
  int ihbid;

  /* fixes a bug... SCRC bank is remade before time-based tracking,
     but HBID sc.id index is into old SCRC bank. So... after time-based
     tracking, redo HBID matching into new SCRC bank. 
     D. P. Weygand Oct 9, 2002  */


  clasHBTR_t *HBTR = (clasHBTR_t *)getBank(&bcs_, "HBTR");
  clasHBID_t *HBID = (clasHBID_t *) getBank(&bcs_,"HBID");
  clasSCRC_t *SCRC;

  if (HBTR && HBID) {
    for (ihbid = 0; ihbid < HBID->bank.nrow; ++ihbid) {
      ntrk = HBID->hbid[ihbid].track - 1;
      sec = HBTR->hbtr[ntrk].itr_sec/100;
      if (SCRC =  (clasSCRC_t *) getGroup(&bcs_,"SCRC",sec)) {
     	match_to_tof(hbtr2hdpl(&(HBTR->hbtr[ntrk])), SCRC, &mat);
	HBID->hbid[ihbid].sc_id = mat.id;
      }
    }
  }
}
