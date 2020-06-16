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
#include <sc.h>

/* -=======================================================- *
$Id: sc_utilities.c,v 1.1 1998/03/04 15:31:36 staylor Exp $
$Author: staylor $
$Revision: 1.1 $
$Date: 1998/03/04 15:31:36 $
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
  if (id<43) return 3;
  return 4;
}
