#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <sh.h>  
#include "sh_global.h"
#include "sh_extern.h"

void sh_evnt_(){
  sh_evnt(); 
}

void sh_evnt(){
  int shn=0, shtn=0;
  int adcn=0,tdcn=0;
  if(RUNNUMBER>=56363 && RUNNUMBER<=57323){
    shn =sh_read_adc_g12();
    shtn=sh_read_tdc_g12();
  }
  if(RUNNUMBER<11 || RUNNUMBER>57323){
    shn =sh_read_adc();
    shtn=sh_read_tdc();
  }
  if(shn){
    adcn=sh_adc2_nphe(shn);
    tdcn=sh_tdc2_nsec(shtn);
    sh_hb_pb(adcn,tdcn);
  }
}
 
