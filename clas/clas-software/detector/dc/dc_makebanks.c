#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <makebanks.h>
#include <status.h>

/*converts an index into the ddly bank to a wire id*/
int ddly2id(int i){
  return((i/DC_WIRES_LAYER+1)*256+(i%DC_WIRES_LAYER)+1);
}

/*the inverse of above, converts an id into an index*/
int id2ddly(int id){
  return(((id/256)-1)*DC_WIRES_LAYER + (id%256 - 1));
}

/* can be replaced with preprocessor definitions*/
int ind2wire(int id){
  return(id%256);
}

int ind2region(int id){
  return(id/(DC_WIRES_LAYER*12));
}

int id2region(int wireid){           
/* converts DC0 - style wire ids to region */
  int layer = wireid/256;
  return (layer-1)/12 + 1;
}

int ind2layer(int id){
  return(id/256);
}

int wire2ind(int wire, int layer){
  return(256*layer + wire);
}

/* FOTRAN callable routine to make DDLY banks*/
int make_ddly_banks_(int *runno){
  int sector;

  for (sector=1; sector<7; sector++){
    make_DDLY_bank(*runno, sector);
  }
}

int make_DDLY_bank(int runno, int sector){
  int firsttime;
  int status[DC_WIRES_SEC];
  float dc2tof_time[1];
  float tdelay_global[3];
  float T0[DC_WIRES_SEC];
  char status_map[100], tdly_map[100], subname[50];
  clasDDLY_t *DDLY = NULL;
  char *dir;
  int i;

  /*read the map*/
  dir = getenv("CLAS_PARMS");
  sprintf(status_map,"%s/Maps/DC_STATUS.map",dir);
  sprintf(tdly_map,"%s/Maps/DC_TDLY.map",dir);
  sprintf(subname,"sector%d", sector);
  map_get_int(status_map, subname, "status", DC_WIRES_SEC, status, runno, &firsttime);
  map_get_float(tdly_map, "Delays", "DC2TOF", 1, dc2tof_time, runno, &firsttime);
  map_get_float(tdly_map, "Delays", "Region4Pulser", 3, tdelay_global, runno, &firsttime);
  map_get_float(tdly_map, subname, "T0", DC_WIRES_SEC, T0, runno, &firsttime);

  /* make the ddly BOS bank*/
  if(DDLY = (clasDDLY_t *)makeBank(&wcs_, "DDLY", sector, 3, DC_WIRES_SEC)){
    for(i=0; i< DC_WIRES_SEC; i++){
      DDLY->ddly[i].id = ddly2id(i);
      DDLY->ddly[i].tidly = T0[i] + tdelay_global[ind2region(i)] + dc2tof_time[0]; 
      DDLY->ddly[i].stat = status[i];
    }
  }
  return(1);
}

int ddly_to_status_(int arr[6][36][192]){
  clasDDLY_t *DDLY = NULL;
  int sec, layer, wire, index;

  for(sec = 0; sec < 6; sec++){
    if(DDLY = (clasDDLY_t*)getGroup(&wcs_, "DDLY", sec + 1)){
      index = 0;
      for(layer=0; layer<36; layer++){
        for(wire=0; wire<192; wire++){
          arr[sec][layer][wire] = DDLY->ddly[index++].stat;
        }
      }
    }
  }

}

int ddly_to_t0_(float arr[6][36][192]){
  clasDDLY_t *DDLY = NULL;
  int sec, layer, wire, index;

  for(sec = 0; sec < 6; sec++){
    if(DDLY = (clasDDLY_t*)getGroup(&wcs_, "DDLY", sec + 1)){
      index = 0;
      for(layer=0; layer<36; layer++){
        for(wire=0; wire<192; wire++){
          arr[sec][layer][wire] = DDLY->ddly[index++].tidly;
        }
      }
    }
  }

}

int make_dcdw_banks_(int runno){
  int sector;

  for(sector=1; sector< 7; sector++)
    make_DCDW_bank(runno,sector);

}


int make_DCDW_bank(int runno,int sector){
  clasDCDW_t *DCDW = NULL;
  dcdw_t dcdwarr[DC_WIRES_SEC];
  int i, ndcdw=0,firsttime,status[DC_WIRES_SEC];
  char status_map[100],subname[50];
  char *dir;

  dir= getenv("CLAS_PARMS");
  sprintf(status_map,"%s/Maps/DC_STATUS.map",dir);
  sprintf(subname,"sector%d", sector);
  fprintf(stderr,"make DCDW run %d sector %d\n",runno, sector);  
  map_get_int(status_map, subname, "status", DC_WIRES_SEC, status, runno, &firsttime);
    for(i=0; i<DC_WIRES_SEC; i++){
      if(status[i] == DC_DEAD_WIRE){
	dcdwarr[ndcdw].id = ddly2id(i);
	dcdwarr[ndcdw].stat = status[i];
	ndcdw++;
      }
    }

  /*fill the bank*/
  if(DCDW = (clasDCDW_t *)makeBank(&wcs_,"DCDW", sector, 3, ndcdw)){
    for(i=0; i < ndcdw; i++){
      DCDW->dcdw[i] = dcdwarr[i];
/*       fprintf(stderr,"%d Layer %d wire %d\n",DCDW->dcdw[i].id,DCDW->dcdw[i].id/256,DCDW->dcdw[i].id%256); */
    }
  }

}









