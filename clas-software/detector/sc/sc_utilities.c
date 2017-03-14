/*--------------------- sc_utilities.c -------------------------- */
/*  This file contains a set of useful functions to help navigate the sc
    analysis code.
    ROUTINES:
	(NOTE: There are 288 'scintillators' in the TOF system.)  
	sc_channel(i):  given an index i into an array of 288 numbers, returns
	                the corresponding scintillator id
	sc_sector(i):   returns the sector number corresponding to index i in
		        an array of length 288.
        sc_index(s,i):  returns the index in the array that corresponds to 
		        scintillator i in sector s
	get_panel(i):   returns the panel number to which scintillator i 				belongs.
*/
/* ------------------------------------------------------------*/
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <math.h>
#include <bostypes.h>
#include <scExtern.h>

/* -=======================================================- *
$Id: sc_utilities.c,v 1.2 2008/03/16 04:33:22 fklein Exp $
$Author: fklein $
$Revision: 1.2 $
$Date: 2008/03/16 04:33:22 $
* -=======================================================- */
/*remember region zero in C is really region 1*/
int sc_channel(int index){
  return(index%SC_NPADDLES_SEC);
}

int sc_sector(int index){
  return(index/SC_NPADDLES_SEC);
}

int sc_index(int sector, int channel){
  return((sector-1)*SC_NPADDLES_SEC + (channel-1));
}
 
int get_panel(int id){
  if (id<24) return 1;
  if (id<35) return 2;
  if (id< (SC_VERSION_FLAG ==1 ? 43:46) ) return 3;
  return 4;
}

int get_sc_version_flag_(int *runno){
  return get_sc_version_flag(*runno);
}
int get_sc_version_flag(int runno){
  char *dir, map[100];
  int locVersionFlag=1, firsttime=0;
  dir = getenv("CLAS_PARMS");
  sprintf(map,"%s/Maps/SC_CALIBRATIONS.map",dir);
  map_get_int(map,"version","flag",1,&locVersionFlag,runno,&firsttime);
  return (locVersionFlag!=2? 1:2);
}

void set_sc_version_constants(int runno){
  int sc_version= get_sc_version_flag(runno);
  printf("set_sc_version_constants for run %d: flag=%d\n",runno,sc_version);
  if( sc_version != 2 ) {
   SC_NPADDLES_SEC = 48;
    SC_NPADDLES = 288;
    SC_VERSION_FLAG = 1;
  }
  else {
    SC_NPADDLES_SEC = 57;
    SC_NPADDLES = 342;
    SC_VERSION_FLAG = 2;
  }
}
