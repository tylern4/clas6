#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <makebanks.h>
#include <utility.h>


int trk_trans[NSEC][DC_WIRES_SEC];

int init_trk_trans_(int *runno){
  init_trk_trans(*runno);
}

int init_trk_trans(int runno){
  int firsttime;
  char *dir;
  char itemname[100], trans_map[100];
  int array[DC_WIRES_SEC];
  int i, sec;

  dir = getenv("CLAS_PARMS");
  sprintf(trans_map,"%s/Maps/DC_TRANS.map",dir);
  for (sec=1; sec <= NSEC; sec++){
    sprintf(itemname,"sector%d", sec);
    map_get_int(trans_map, "trans", itemname, DC_WIRES_SEC, array, runno, &firsttime);
    /* being safe */
    for (i=0; i < DC_WIRES_SEC; i++) trk_trans[sec-1][i] = array[i];
  }
}


int make_dc1_banks_(int dc_digi[NSEC][NLAY][MAXHITS][2], int dc_ndig[NSEC][NLAY], int pt_digi[NSEC][NLAY][MAXHITS]){
  int sec, dc[NSEC][NLAY][MAXHITS][2];
  
  for(sec=1; sec<= NSEC; sec++){
    make_DC1_bank(sec);
    DC1_to_digi(sec, dc_digi, dc_ndig, pt_digi);
  }
}

int make_dc1_(){
  clasDC1_t *DC1 = NULL;
  int sec;
  
  /* if there is no DC1 bank, make it*/
  if (!(DC1 = getBank(&bcs_, "DC1 "))){
    for (sec=1; sec <= NSEC; sec++){ 
      make_DC1_bank(sec);
    }
  }

}

int make_digi_common_(int dc_digi[NSEC][NLAY][MAXHITS][2], int dc_ndig[NSEC][NLAY], int pt_digi[NSEC][NLAY][MAXHITS]){
  int sec, dc[NSEC][NLAY][MAXHITS][2];

  for(sec=1; sec<= NSEC; sec++){
    DC1_to_digi(sec, dc_digi, dc_ndig, pt_digi);
  }
  
}

int make_DC1_bank(int sector){
  /* input: DC0 bank, DCDW bank, DDLY bank, sector
     output: DC1 bank
     Purpose: Filter the dc0 bank to make dc hits more amenable to tracking*/
  clasDCDW_t *DCDW =  getGroup(&wcs_,"DCDW", sector);
  clasDC0_t *DC0 = getGroup(&bcs_, "DC0 ", sector);
  clasDC1_t *DC1 = NULL;
  clasDDLY_t *DDLY = getGroup(&wcs_, "DDLY", sector);
  dc1_t dc1arr[DC_WIRES_SEC];
  int nDC1 = 0, i = 0, dead_flag,dw_index;


  if (DC0){
    int dc0_index;
    for(dc0_index=0;dc0_index < DC0->bank.nrow; dc0_index++){
      /*check to see if it is a valid wire id and a valid TDC value*/
      if ( ind2wire(DC0->dc0[dc0_index].id) >= MIN_ID && ind2wire(DC0->dc0[dc0_index].id) <= MAX_ID && ind2layer(DC0->dc0[dc0_index].id) >= MIN_LAYER && ind2layer(DC0->dc0[dc0_index].id) <= MAX_LAYER && DC0->dc0[dc0_index].tdc > DC_TDC_MIN && DC0->dc0[dc0_index].tdc < DC_TDC_MAX ){
	/* L.Todor,CMU, Jun2001; I put a wire in DC1 only if it's not dead */
        dead_flag=0; 
	if (DCDW){  
	  for(dw_index=0;dw_index< DCDW->bank.nrow;dw_index++){
           if (DCDW->dcdw[dw_index].id == DC0->dc0[dc0_index].id){
	       /*	       fprintf(stderr,"dwid-%d dc0id-%d\n",DCDW->dcdw[dw_index].id,DC0->dc0[dc0_index].id);*/
             dead_flag=1;
           }
          }
	}
	if (!dead_flag){
	  dc1arr[nDC1].id = trk_trans[sector - 1][id2ddly(DC0->dc0[dc0_index].id)];
	  dc1arr[nDC1].time = (float)DC0->dc0[dc0_index].tdc;
	  nDC1++;
	}
      }
    }
  }


  if (nDC1) {

      if(DC1 = (clasDC1_t *)makeBank(&bcs_, "DC1 ",sector, 2, nDC1)){
	for(i=0; i < nDC1; i++) 
	  DC1->dc1[i] = dc1arr[i];
      }else {
	fprintf(stderr,"Unable to make the DC1 bank\n");
      }
  }
}


int DC1_to_digi(int sector, int dc_digi[NSEC][NLAY][MAXHITS][2], int dc_ndig[NSEC][NLAY], int pt_digi[NSEC][NLAY][MAXHITS]){
  clasDC1_t *DC1 = NULL;
  int i, j, c_sec;

  c_sec = sector-1;


  /*zero out digi array*/
  for(i=0; i < NLAY; i++){
    dc_ndig[c_sec][i] = 0;
    /*    for(j=0; j< MAXHITS; j++){
      dc_digi[c_sec][i][j][WIRE_ID] = 0;
      dc_digi[c_sec][i][j][TDC] = 0;
      pt_digi[c_sec][i][j] = 0;
    }*/
  }

  /*fill digi arrays from DC1 bank*/
  if(DC1 = (clasDC1_t *)getGroup(&bcs_, "DC1 ", sector)){
      for(i=0; i < DC1->bank.nrow; i++){
	int layer = ind2layer(DC1->dc1[i].id) -1;
	if (dc_ndig[c_sec][layer] < MAXHITS){
	  dc_digi[c_sec][layer][dc_ndig[c_sec][layer]][TDC] = (int)DC1->dc1[i].time;
	  dc_digi[c_sec][layer][dc_ndig[c_sec][layer]][WIRE_ID] = ind2wire(DC1->dc1[i].id);
	  /*I will assume whoever needs pt_digi is calling from FORTRAN */
	  pt_digi[c_sec][layer][dc_ndig[c_sec][layer]] = i+1;
	  dc_ndig[c_sec][layer]++;
	}
      }
  }
  
}


int hbtr2dc0(hbtr_t *hbtr){
  /* wierd routine that converts HBLA and DC1 into a reduced DC0 bank
     input: DC1, HBLA, HBTR
     output: DC0 */
  clasHBLA_t *HBLA=NULL;
  clasDC1_t *DC1=NULL;
  clasDC0_t *DC0=NULL;
  dc0_t dc0[DC_WIRES_SEC];
  int nDC0 = 0, i, ret;
  
  /* now add a track */
  if ((DC1 = getGroup(&bcs_, "DC1 ", (hbtr->itr_sec)/100 )) &&
      (HBLA= getGroup(&bcs_, "HBLA", (hbtr->itr_sec)/100 ))){
    hbla_t *hbla = NULL; 
    /*for the case if two tracks in the same sector 
      i.e. save whatever is already in the DC0 bank 
      - this is a little ugly */
    if (DC0 = getGroup(&bcs_, "DC0 ", (hbtr->itr_sec)/100)){
      for(i=0; i < DC0->bank.nrow; i++) {
        dc0[i] = DC0->dc0[i]; 
        nDC0++;
      }
      dropBank(&bcs_, "DC0 ", (hbtr->itr_sec)/100);
    }
    if (hbla = hbtr2hbla(hbtr)){
      for(i=0; i < HBLAPlanesPerTrack; i++){
        if ((hbla[i].dc1 > 0) && (hbla[i].dc1 <= DC1->bank.nrow)){
          int dc1ptr = hbla[i].dc1 -1;
          uint16 id, tdc;
          id = DC1->dc1[dc1ptr].id;
          dc0[nDC0].id = id;
          tdc = DC1->dc1[dc1ptr].time;
          dc0[nDC0].tdc = tdc;
          nDC0++;
        }
      }
    }
    /* notice I used (sizeof(dc0_t)/4)*2 instead of (sizeof(dc0_t)/4
       like with all other banks - this is because the format for dc0
       is b16 with two columns*/
    if (nDC0){
     if (DC0 = makeBank(&bcs_, "DC0 ", (hbtr->itr_sec)/100, (sizeof(dc0_t)/4)*2, nDC0)){
        for(i=0; i < nDC0; i++) DC0->dc0[i] = dc0[i];
        ret = 1;
      }
    }
  } else {
    ret = 0;
  }
  return ret;
  
}


