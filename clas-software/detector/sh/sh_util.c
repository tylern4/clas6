#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "sh.h"
#include "sh_global.h" 
#include "sh_extern.h"

int get_chan_id (int raw_id){
  int quad= (int)(raw_id/256);
  int chan= (raw_id & 255);
  int chan_id=quad*100+chan;
  return chan_id;
}


int get_chan_id_g12 (int raw_id){
  int quad= (int)(raw_id/2560);
  int chan= (raw_id & 255);
  int chan_id=quad*100+chan;
  return chan_id;
}


int get_db_ind (int id){
  int quad= id/100;
  int chan= id%100;
  int db_ind=(quad-1)*18+chan-1;
  return db_ind;
}


int coin_check(int pixid){
  int g_ind=0, coin_pixid=0;
  int quad=pixid/100; int chan=pixid%100; 
  if(chan>= 1 && chan< 5){g_ind= 1;}
  if(chan>= 5 && chan<10){g_ind= 5;}
  if(chan>=10 && chan<15){g_ind=10;}
  if(chan>=15 && chan<19){g_ind=15;}
  coin_pixid=quad*100+g_ind;
  return coin_pixid;//pixid # of straight fiber
}


float get_nphe(int adc, float pedestal, float gain){
  if(adc-pedestal<=0) return(0);
  float nphe=(adc-pedestal)/gain;
  return nphe;
}


float get_time (int tdc, float t2ch, float t0){
  float nsec=tdc*t2ch+t0;
  return nsec;
}


