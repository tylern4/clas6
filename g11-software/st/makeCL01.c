#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ntypes.h>
#include <bostypes.h>
#include <map_manager.h>
#include <call.h>
#include <math.h>

#define FACTOR 100

call_const_t call_calib;
float rf_offset = 0;
int i, status = 1;
/*typedef struct{
  float low;
  float high;
  float p0;
  float p1;
  float p2;
  float p3;
  } F_t;*/

F_t f[4];

int make_cl01_bank_(){
  make_CL01_bank();
}

int make_CL01_bank(){
  /*input: Call bank, calibration constants (must run initCL01)
    output: CL01 bank */
  clasCALL_t *CALL = NULL;
  clasCL01_t *CL01 = NULL;
  cl01_t cl01;
  int i, n_region = 0, rf_region = 0;
  float offset = 0;

  cl01.ac_amp = cl01.fc_diode_amp = cl01.nc_diode_amp = 
    cl01.sc_diode_amp = cl01.sf_diode_amp = NO_CL01_HIT;
  cl01.fc_diode_t = cl01.nc_diode_t = cl01.sc_diode_t = 
    cl01.sf_diode_t = cl01.rf1 = cl01.rf2 = NO_CL01_HIT;

  if ( CALL = getBank(&bcs_, "CALL")){
    int i;
    /* printCALLbank(stderr,CALL); */
    for (i=0; i < CALL->bank.nrow; i++){
      switch ((int)CALL->call[i].id){
      case AC60HZ:
	cl01.ac_amp = getCL01adc(AC60HZ, CALL->call[i].adc);
	break;
      case FWD_DIODE:
	cl01.fc_diode_amp = getCL01adc(FWD_DIODE, CALL->call[i].adc);
	cl01.fc_diode_t = getCL01time(FWD_DIODE, CALL->call[i].tdc);
	break;
      case N_CLMSH_DIODE:
	cl01.nc_diode_amp = getCL01adc(N_CLMSH_DIODE, CALL->call[i].adc);
	cl01.nc_diode_t = getCL01time(N_CLMSH_DIODE, CALL->call[i].tdc);
	break;
      case S_CLMSH_DIODE:
	cl01.sc_diode_amp = getCL01adc(S_CLMSH_DIODE, CALL->call[i].adc);
	cl01.sc_diode_t = getCL01time(S_CLMSH_DIODE, CALL->call[i].tdc);
	break;
      case SPCFM_DIODE:
	cl01.sf_diode_amp = getCL01adc(SPCFM_DIODE, CALL->call[i].adc);
	cl01.sf_diode_t = getCL01time(SPCFM_DIODE, CALL->call[i].tdc);
	break;
      case RF1:
	cl01.rf1 = getCL01time(RF1, CALL->call[i].tdc);
	break;
      case RF2:
	cl01.rf2 = getCL01time(RF2, CALL->call[i].tdc);
	break;
      }
    }
   
    /* good RF */

     cl01.rf = 0.0; /* initialization */      

  /* use RF1 */
  if ((cl01.rf1 != NO_CL01_HIT) && status == 1 ){

    for(i=0; i < 4; i++){
     if(cl01.rf1 >= f[i].low && cl01.rf1 < f[i].high){
      rf_region = i + 1;
      n_region++;
      }
    }
    if(rf_region >= 1 && rf_region <= 4 && n_region == 1){
    offset = f[rf_region-1].p0+f[rf_region-1].p1*cl01.rf1+f[rf_region-1].p2*(cl01.rf1*cl01.rf1)+
             f[rf_region-1].p3*(cl01.rf1*cl01.rf1*cl01.rf1); 
    cl01.rf = cl01.rf1 + rf_offset + offset;
    }else if(n_region > 1){
    fprintf(stderr,"WARNING: no RF1 correction at %f2 ns. Overlapping calibration regions: %d \n",cl01.rf1, n_region); 
    }
  }


   /* use RF2 */ 
  if ((cl01.rf2 != NO_CL01_HIT) && status == 2 ){
    for(i=0; i < 4; i++){
      if(cl01.rf2 >= f[i].low && cl01.rf2 < f[i].high){
      rf_region = i + 1;
      n_region++;
      }
    }
    if(rf_region >= 1 && rf_region <= 4 && n_region == 1){
    offset = f[rf_region-1].p0+f[rf_region-1].p1*cl01.rf2+f[rf_region-1].p2*(cl01.rf2*cl01.rf2)+
             f[rf_region-1].p3*(cl01.rf2*cl01.rf2*cl01.rf2); 
    cl01.rf = cl01.rf2 + rf_offset + offset;
    }else if(n_region > 1){
    fprintf(stderr,"WARNING: no RF2 correction at %f2 ns. Overlapping calibration regions: %d \n",cl01.rf2, n_region); 
    }

  }
       

    if (CL01 = makeBank(&bcs_, "CL01", 0, sizeof(cl01_t)/4, 1)){
      CL01->cl01[0] = cl01;
    } 
  }
}

int initcl01_(int *runno){
  initCL01(*runno);
}

int CL01_begin_run(int runno){
  clasHEAD_t *HEAD = getBank(&bcs_, "HEAD");

  if (HEAD){
    if (HEAD->head[0].type >= 0) {
      /* data */
      initCL01(runno);
    } else {
      /*monte carlo - do something else */
      initCL01(1);
    }
  }
}

int initCL01(int runno){
  char *dir,map[128], rfmap[128];
  int  i,firsttime;
  
  dir=getenv("CLAS_PARMS");
  sprintf(map,"%s/Maps/CALL_CALIB.map",dir);
  sprintf(rfmap, "%s/Maps/RF_OFFSETS.map", dir);
  /*  dir=getenv("TOP_DIR");
  sprintf(map,"%s/st/CALL_CALIB.map",dir); */ 
  map_get_float(map,"T0","value",CALL_IDS,call_calib.t0,runno,&firsttime);    
  map_get_float(map,"T1","value",CALL_IDS,call_calib.t1,runno,&firsttime); 
  map_get_float(map,"T2","value",CALL_IDS,call_calib.t2,runno,&firsttime); 
  map_get_float(map,"pedestals","value",CALL_IDS,call_calib.ped,runno,&firsttime); 
  map_get_float(rfmap, "offset", "value", 1, &rf_offset, runno, &firsttime);
  map_get_int(rfmap, "status", "value", 1, &status, runno, &firsttime);

for(i=0; i < 4; i++){
  if(i==0){
    map_get_float(rfmap, "F1", "low",1,&f[i].low,runno,&firsttime);
    map_get_float(rfmap, "F1", "high",1,&f[i].high,runno,&firsttime);
    map_get_float(rfmap, "F1", "p0",1,&f[i].p0,runno,&firsttime);
    map_get_float(rfmap, "F1", "p1",1,&f[i].p1,runno,&firsttime);
    map_get_float(rfmap, "F1", "p2",1,&f[i].p2,runno,&firsttime);
    map_get_float(rfmap, "F1", "p3",1,&f[i].p3,runno,&firsttime);
  }else if(i==1){
    map_get_float(rfmap, "F2", "low",1,&f[i].low,runno,&firsttime);
    map_get_float(rfmap, "F2", "high",1,&f[i].high,runno,&firsttime);
    map_get_float(rfmap, "F2", "p0",1,&f[i].p0,runno,&firsttime);
    map_get_float(rfmap, "F2", "p1",1,&f[i].p1,runno,&firsttime);
    map_get_float(rfmap, "F2", "p2",1,&f[i].p2,runno,&firsttime);
    map_get_float(rfmap, "F2", "p3",1,&f[i].p3,runno,&firsttime);
  }else if(i==2){
    map_get_float(rfmap, "F3", "low",1,&f[i].low,runno,&firsttime);
    map_get_float(rfmap, "F3", "high",1,&f[i].high,runno,&firsttime);
    map_get_float(rfmap, "F3", "p0",1,&f[i].p0,runno,&firsttime);
    map_get_float(rfmap, "F3", "p1",1,&f[i].p1,runno,&firsttime);
    map_get_float(rfmap, "F3", "p2",1,&f[i].p2,runno,&firsttime);
    map_get_float(rfmap, "F3", "p3",1,&f[i].p3,runno,&firsttime);
  }else if(i==3){
    map_get_float(rfmap, "F4", "low",1,&f[i].low,runno,&firsttime);
    map_get_float(rfmap, "F4", "high",1,&f[i].high,runno,&firsttime);
    map_get_float(rfmap, "F4", "p0",1,&f[i].p0,runno,&firsttime);
    map_get_float(rfmap, "F4", "p1",1,&f[i].p1,runno,&firsttime);
    map_get_float(rfmap, "F4", "p2",1,&f[i].p2,runno,&firsttime);
    map_get_float(rfmap, "F4", "p3",1,&f[i].p3,runno,&firsttime);
  }
}
  fprintf(stderr,"Reading CALL calibration constants for run %d.\n",runno);
                
}


void rf_numbers_(float *rf1, float *rf2, float *good_rf, float *rf_corr){
  clasCL01_t *CL01 = getBank(&bcs_, "CL01");
  if (CL01){
    *rf1 = CL01->cl01[0].rf1;
    *rf2 = CL01->cl01[0].rf2;
    *good_rf = CL01->cl01[0].rf;
    *rf_corr = fmod((- (*good_rf) + FACTOR*RF_STRUCTURE), RF_STRUCTURE) 
      - RF_STRUCTURE/2.0;
  } else {
    *rf1 = *rf2 = *good_rf = *rf_corr = 0.0;
  } 
} 

float rf_corr_time_(float *time){
  return (rf_corr_time(*time));
}

float rf_corr_time(float time){
  clasCL01_t *CL01 = getBank(&bcs_, "CL01");
  float rf_st_time = time;

  if (CL01){ 
    rf_st_time = time - rf_correction(time, CL01->cl01[0].rf);
  } 
  return rf_st_time;
}


float rf_correction(float time, float rf){
  float del_time = 0;
  
  if(rf > 0.0){
  del_time = time - rf + FACTOR*RF_STRUCTURE;
  return(fmod(del_time,RF_STRUCTURE) - RF_STRUCTURE/2.0);
  }else{
  return(0.0);
  }
}

float getCL01time(int ind_cl01, int tdc){
  float t0 = call_calib.t0[ind_cl01-1];
  float t1 = call_calib.t1[ind_cl01-1];
  float t2 = call_calib.t2[ind_cl01-1];
  return ( t0 + t1*tdc + t2*tdc*tdc);
} 

int getCL01adc(int ind_cl01, int adc){
  extern call_const_t call_calib;
  int ped = (int)call_calib.ped[ind_cl01-1];
  return ((int)(adc - ped));
}

int get_diode_id(int id,int sec){
  if (id<24) return FWD_DIODE;
  if (id<43&&sec>=2&&sec<=5) return N_CLMSH_DIODE;
  if (id<43) return S_CLMSH_DIODE;
  if (id<49) return SPCFM_DIODE;
  return -1;
}


float get_diode_time(cl01_t cl01,int id,int sec){
  if (id<24) return cl01.fc_diode_t;
  if (id<43&&sec>=3&&sec<=5) return cl01.nc_diode_t;
  if (id<43) return cl01.sc_diode_t;
  if (id<49) return cl01.sf_diode_t;
  return 0.0;
}




