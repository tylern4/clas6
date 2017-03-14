#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>


int trk_dcam_c(int sec, int nhbt, trks_t *trks){
  int trk_wire[36],road_width=1;
  int ntbt_seg[6],tbt_seg[6][30][12];
  int ntrk=0,i;
  float trk_doca[36];
  float beta,chibest;
  
  if (trks){
    if (trks->beta>0){
      int success=0;
      int trkno=trks->trk_lev%100;

      trk_calc_wire_(&sec,&trkno,&(trks->beta),trk_wire,trk_doca);
      tbt_segm_(&sec,&road_width,trk_wire,ntbt_seg,tbt_seg);
      trk_lramb_(&sec,&trkno,&(trks->beta),&(trks->st_time),ntbt_seg,tbt_seg,
		 &success);
      
      return success;
    }
    else return 0;
  }
  else return 0;
}






















