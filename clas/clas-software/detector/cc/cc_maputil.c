/*#include<mapmanager.h>*/
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#define CC_CHANNELS 216

/*prototypes*/
int cc_channel(int index);
int cc_sector(int index);
int cc_index(int sector, int channel);
int create_CC_map(char *map);
/* pedestal routines*/
int get_cc_pedestals_(char *map, int runno, float mean[], float sigma[]);
int put_cc_pedestals_(char *map, int runno, float mean[], float sigma[]);
int remove_cc_pedestals_(char *map, int runno);
/* gain routines*/
int get_cc_gain_(char *map, int runno, float mean[], float sigma[]);
int put_cc_gain_(char *map, int runno, float mean[], float sigma[]);
int remove_cc_gain_(char *map, int runno);
/*serial number routines*/
int get_cc_serno_(char *map, int runno, float mean[], float sigma[]);

/* FORTRAN callable routines */
int get_cc_map_ped_mean_(int *runno, float ccpedes[36][6]);
int get_cc_map_pe_amp_(int *runno, float opheamp[36][6]);
int put_cc_map_ped_mean_(int *runno, float ccpedes[36][6]);

/*remember sector zero in C is really sector 1*/
int cc_channel(int index){
  return(index%36);
}

/*remember region zero in C is really region 1*/
int cc_sector(int index){
  return(index/36);
}

int cc_index(int sector, int channel){
  return(sector*36 + channel);
}

int create_CC_map(char *map){
  /* this is how I made the map CC_CALIB.map */
  /*sector and channel are coded by an index*/
  map_create(map);
  map_add_item(map,"pedestals", "mean", CC_CHANNELS, 1);
  map_add_item(map,"pedestals", "sigma", CC_CHANNELS, 1);
  map_add_item(map,"PEcalib", "mean", CC_CHANNELS, 1);
  map_add_item(map,"PEcalib", "width", CC_CHANNELS, 1);
  map_add_item(map,"ADCsoftthresh", "value", CC_CHANNELS, 1);
  map_add_item(map,"tdcgain", "value", CC_CHANNELS, 1);
  map_add_item(map,"PMT", "serialno", CC_CHANNELS, 1);

  return(1);
}

/*pedestal routines*/

int get_cc_pedestals_(char *map, int runno, float mean[], float sigma[]){
  int firsttime;

  map_get_float(map,"pedestals", "mean", CC_CHANNELS, mean, runno, &firsttime);
  map_get_float(map,"pedestals", "sigma", CC_CHANNELS, sigma, runno, &firsttime);
  if (firsttime < 0 ) return( 0);
  return(1);
}

int put_cc_pedestals_(char *map, int runno, float mean[], float sigma[]){
  int firsttime;

  map_put_float(map,"pedestals", "mean", CC_CHANNELS, mean, runno);
  map_put_float(map,"pedestals", "sigma", CC_CHANNELS, sigma, runno);
  return(1);
}

int remove_cc_pedestals_(char *map, int runno){
  map_rem_arr(map, "pedestals", "mean",runno);
  map_rem_arr(map, "pedestals", "sigma",runno);
  return(1);
}

/*----------ADC gain routines------------*/
int get_cc_gain_(char *map, int runno, float mean[], float sigma[]){
  int firsttime;

  map_get_float(map,"PEcalib", "mean", CC_CHANNELS, mean, runno, &firsttime);
  map_get_float(map,"PEcalib", "width", CC_CHANNELS, sigma, runno, &firsttime);
  if (firsttime < 0 ) return( 0);
  return(1);
}

int put_cc_gain_(char *map, int runno, float mean[], float sigma[]){
  int firsttime;

  map_get_float(map,"PEcalib", "mean", CC_CHANNELS, mean, runno, &firsttime);
  map_get_float(map,"PEcalib", "width", CC_CHANNELS, sigma, runno, &firsttime);
  if (firsttime < 0 ) return( 0);
  return(1);
}

int remove_cc_gain_(char *map, int runno){
  map_rem_arr(map, "PEcalib", "mean",runno);
  map_rem_arr(map, "PEcalib", "width",runno);
  return(1);
}


/*-------------Serial No.---------*/
int get_cc_serno_(char *map, int runno, float mean[], float sigma[]){
  int firsttime;
  map_get_float(map,"PEcalib", "width", CC_CHANNELS, sigma, runno, &firsttime);
  if (firsttime < 0 ) return( 0);
  return(1);
}

/*----FORTRAN Callable read routine------*/
int get_cc_map_ped_mean_(int *runno, float ccpedes[36][6]){
  int i, firsttime;
  float mean[CC_CHANNELS];
  char *dir, def_map[100];

  dir = getenv("CLAS_PARMS");
  sprintf(def_map,"%s/Maps/CC_CALIB.map",dir);

  map_get_float(def_map,"pedestals", "mean", CC_CHANNELS, mean, *runno, &firsttime);
  if (firsttime > 0){
    for(i=0; i< CC_CHANNELS; i++)
      ccpedes[cc_channel(i)][cc_sector(i)]=mean[i];
  } else {
    /* map read failed */
    for(i=0; i< CC_CHANNELS; i++)
	ccpedes[cc_channel(i)][cc_sector(i)]= 0.0;
    return(0);
  }
  
  return(1);
}

int get_cc_map_pe_amp_(int *runno, float opheamp[36][6]){
  int i, firsttime;
  float amp[CC_CHANNELS];
  char *dir, def_map[100];

  dir = getenv("CLAS_PARMS");
  sprintf(def_map,"%s/Maps/%s.map",dir);

  map_get_float(def_map,"photoE", "amplitude", CC_CHANNELS, amp, *runno, &firsttime);
  if (firsttime > 0){
    for(i=0; i< CC_CHANNELS; i++)
      opheamp[cc_channel(i)][cc_sector(i)]=amp[i];
  } else {
    /* map read failed */
    for(i=0; i< CC_CHANNELS; i++)
	opheamp[cc_channel(i)][cc_sector(i)]= 0.0;
    return(0);
  }
  
  return(1);
}

/*--------general read utility callable from FORTRAN----------*/
int cc_read_map_(int *runno, float item_arr[36][6], char *map_f, char *system_f, char *item_f, int maplen_f, int syslen_f, int itemlen_f){
  int i, firsttime;
  char system_c[50], item_c[50], map_c[50];
  float arr[CC_CHANNELS];
  char *dir, def_map[100];

  /* convert FORTRAN strings into C */
  strncpy(map_c,map_f, maplen_f);
  map_c[maplen_f] = '\0';
  strncpy(system_c, system_f, syslen_f);
  system_c[syslen_f]= '\0';
  strncpy(item_c,item_f, itemlen_f);
  item_c[itemlen_f]= '\0';

  dir = getenv("CLAS_PARMS");
  sprintf(def_map,"%s/Maps/%s.map",dir, map_c);
  map_get_float(def_map, system_c, item_c, CC_CHANNELS, arr, *runno, &firsttime);
  if (firsttime > 0){
    for(i=0; i< CC_CHANNELS; i++)
      item_arr[cc_channel(i)][cc_sector(i)]=arr[i];
  }else{
    for(i=0; i< CC_CHANNELS; i++)
      item_arr[cc_channel(i)][cc_sector(i)]=0.0;    
    return(0);
  }
  return(1);
}



/*----FORTRAN callable write routine------*/

int put_cc_map_ped_mean_(int *runno, float ccpedes[36][6]){
  int i, firsttime;
  float mean[CC_CHANNELS];
  char *dir, def_map[100];

  dir = getenv("CLAS_PARMS");
  sprintf(def_map,"%s/Maps/CC_CALIB.map",dir);
  
  for(i=0; i< CC_CHANNELS; i++)
    mean[i] = ccpedes[cc_channel(i)][cc_sector(i)];

  map_put_float(def_map,"pedestals", "mean", CC_CHANNELS, mean, *runno);
  
  return(1);
}



