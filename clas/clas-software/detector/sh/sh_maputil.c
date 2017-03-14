#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "sh.h"
#include "sh_global.h"
#include "sh_extern.h"

int sh_maputil (char *shmap,sh_calib_t *shcalib,int runno ){

  int time; int st; 
  char *item;

  //===ADC===
  item="status";
  st=map_get_int(shmap,"ADC",item,SH_CHANNELS,shcalib->adcstat,runno,&time);
  if(st){printf(" Error trying to read the item : %s \n",item);return(1);}
  
  item="pedestal";
  st=map_get_float(shmap,"ADC",item,SH_CHANNELS,shcalib->pedestal,runno,&time); 
  if(st){printf(" Error trying to read the item : %s \n",item);return(1);}
  
  item="gain";
  st=map_get_float(shmap,"ADC",item,SH_CHANNELS,shcalib->gain,runno,&time);  
  if(st){printf(" Error trying to read the item : %s \n",item);return(1);}
  


  //===TDC===
  item="status";
  st=map_get_int(shmap,"TDC",item,SH_CHANNELS,shcalib->tdcstat,runno,&time);
  if(st){printf(" Error trying to read the item : %s \n",item);return(1);}

  item="tch";
  st=map_get_float(shmap,"TDC",item,SH_CHANNELS,shcalib->t2channel,runno,&time);
  if(st){printf(" Error trying to read the item : %s \n",item);return(1);}
    
  item="t0";
  st=map_get_float(shmap,"TDC",item,SH_CHANNELS,shcalib->t0,runno,&time);
  if(st){printf(" Error trying to read the item : %s \n",item);return(1);}

  item="tmean";
  st=map_get_float(shmap,"TDC",item,SH_CHANNELS,shcalib->tcoi_m,runno,&time);
  if(st){printf(" Error trying to read the item : %s \n",item);return(1);}
  
  item="tsigm";
  st=map_get_float(shmap,"TDC",item,SH_CHANNELS,shcalib->tcoi_s,runno,&time);
  if(st){printf(" Error trying to read the item : %s \n",item);return(1);}



  //===Geometry===
  item="x0";
  st=map_get_float(shmap,"Geometry",item,3,shcalib->x0,runno,&time);
  if(st){printf(" Error trying to read the item : %s \n",item);return(1);}
    
  item="alpha";
  st=map_get_float(shmap,"Geometry",item,3,shcalib->alpha,runno,&time);
  if(st){printf(" Error trying to read the item : %s \n",item);return(1);}
  
 
  item="pixelsize";
  st=map_get_float(shmap,"Geometry",item,1,shcalib->pixelsize,runno,&time);
  if(st){printf(" Error trying to read the item : %s \n",item);return(1);}
  
  return (0);
}
