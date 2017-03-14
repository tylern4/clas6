#include <stdio.h>
#include <stdlib.h>
#include <clas_cern.h>
#include "pdu.h"

#ifndef NULL
#define NULL (void*)0
#endif

/*------------ PAW DEFINES ----------------- */
#define LREC 1024 /* record length of hbook direct access file in WORDS */
#define LUN 3 /* logical unit number of hbook file */
#define ID 99 /* column-wise-ntuple id */

/* declare the hbook common */
extern float pawc_[MEMH];
extern float hcfitd_[25];
extern int quest_[100];
extern int PulserData;

void hini(char *out)
{
  int lun = LUN, lrec = LREC, memh = MEMH;
  int istat, icycle;
  char *def_out = "bos2hb.hb4";

  if(out == NULL) out = &def_out[0];
  quest_[9] = 65000;
  hlimit_(&memh);
  hropen_(&lun, "esr", out , "N", &lrec, &istat, 3L, strlen(out), 1L);
  book_histos();

  return;
}

void book_histos(){
  int   HistID;
  int   Sector;
  char  title[50];

  for (Sector = 1; Sector <7; Sector++){
    sprintf(title, "sector %i occupancy", Sector);
    HistID = 200 + 10*Sector;
    hbook2(HistID, title, 192, 0.5, 192.5, 36, 0.5, 36.5, 100000.0);
    
    sprintf(title, "sector %i change", Sector);
    HistID = 300 + 10*Sector;
    hbook2(HistID, title, 192, 0.5, 192.5, 36, 0.5, 36.5, 100.0);
  
    sprintf(title, "sector %i pathology", Sector);
    HistID = 700 + 10*Sector;
    hbook2(HistID, title, 192, 0.5, 192.5, 36, 0.5, 36.5, 100.0);
    
  } /* end of sector loop */	  
  
}
