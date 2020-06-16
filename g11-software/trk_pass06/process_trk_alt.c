#include <trk_run_control.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>


/*an attempt to streamline the tracking run control routines*/

int NewProcessTimeBased(){
  int ntracks=0, ntr_fit, ntrk_lnk=0;
  int ret = 0,i,sec,nhbt=0;
  int TRK_LEVEL_LRAMB=3;
  clasTRKS_t *TRKS=NULL;
  clasHBTR_t *HBTR=getBank(&bcs_,"HBTR");

  if (HBTR){
    nhbt=HBTR->bank.nrow;
  }
  setup_anapatt_(); /*do it again*/

  lramb_init_();
  for (sec=1;sec<7;sec++){
    if (TRKS=getGroup(&bcs_,"TRKS",sec)){
      for (i=0;i<TRKS->bank.nrow;i++){
	if(trk_dcam_c(sec,nhbt,&(TRKS->trks[i]))) ntracks++;
      }
    }
  }
 
  trk_delete_(&ntracks,&nhbt,&TRK_LEVEL_LRAMB);
  if(ntracks){
    ana_fit_(&ntracks);
    if (ntracks) ret = 1;
  }
  return ret;
}


