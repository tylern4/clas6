
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <sh.h>
#include <sh_global.h>

void sh_brun_(int *runno){
  sh_brun(*runno); 
}

int sh_brun(int runno){
  RUNNUMBER=runno;
  char *sh_dir, sh_map[100];
  if( (sh_dir = getenv("CLAS_PARMS")) ) {     
    sprintf(sh_map,"%s/Maps/HODOSCOPE.map",sh_dir); 
    if(! (sh_maputil(sh_map,&sh_calib,runno)) ){
      printf("Reading SH calibration constants for RUN# %d\n",runno);
    }
    else{
      printf("Error in reading SH calibration constants for RUN# %d \n",runno);
    }
  }  
  else{
    printf(" Error trying to read the Map : %s \n",sh_map);
    return(1);
  }
  sh_coordinates(sh_calib.x0,sh_calib.alpha,sh_calib.pixelsize);
  return(0);
}
