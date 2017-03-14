#include <trk_run_control.h>


/*an attempt to streamline the tracking run control routines*/

int ProcessTimeBased(){
  int ntracks, ntr_fit;
  int ret = 0;

  setup_anapatt_(); /*do it again*/
  /*trk_read_bank_(&ntracks);*/
  trk_dcam_(&ntr_fit, &ntracks);
  if(ntr_fit){
    ana_fit_(&ntracks);
    if (ntracks) ret = 1;
  }
  
  return ret;
}


int ProcessHitBased(){
  int sec;
  int fail;

  setup_anapatt_();
  for (sec=1; sec <= 6; sec++){
    ana_segm_(&sec, &fail);
    trk_link_(&sec, &fail);
  }
  ana_prfit_(&fail);

  return fail;
}
