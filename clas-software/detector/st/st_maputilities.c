/* Routines for obtaining calibration constants for the start counter from 
   $CLAS_PARMS/Maps/ST_CALIB.map */
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ntypes.h>
#include <bostypes.h>
#include <map_manager.h>
#include <st.h>

/*let fortran allocate the memory*/
extern StCal_t stcal_;
extern StnCal_t stncal_;

int st_begin_run(int runno){
  int one = 1;
  clasHEAD_t *HEAD = getBank(&bcs_, "HEAD");

  StConfigGeom(&runno);

  if (HEAD){
    if (HEAD->head[0].type >= 0) {
      /* data */
      st_brun_(&runno);
    } else {
      /*monte carlo - do something else */
      st_brun_(&one);
    }
  }
}

int stcal_read_map_(int *runno,float StCal_values[6][13],float StCal_uncs[6][13]){
  char *dir,map[100];
  float st2tof,offsets[6],side2side[3],pair2pair[3];
  float veff_leg_near[6],veff_leg_far[6];
  float veff_nose_near[6],veff_nose_far[6];
  int i,firsttime;
 
  dir = getenv("CLAS_PARMS");
  sprintf(map,"%s/Maps/ST_CALIB.map",dir); 

  st_read_map(map,*runno,"pedestals",PEDESTAL,StCal_values,StCal_uncs);
  st_read_map(map,*runno,"T0",TDC_T0,StCal_values,StCal_uncs);
  st_read_map(map,*runno,"T1",TDC_T1,StCal_values,StCal_uncs);
  st_read_map(map,*runno,"T2",TDC_T2,StCal_values,StCal_uncs);
  st_read_map(map,*runno,"W0",WALK0,StCal_values,StCal_uncs);
  st_read_map(map,*runno,"W1",WALK1,StCal_values,StCal_uncs);
  st_read_map(map,*runno,"W2",WALK2,StCal_values,StCal_uncs);
  st_read_map(map,*runno,"M0",M0,StCal_values,StCal_uncs);
  st_read_map(map,*runno,"LAMBDA",LAMBDA,StCal_values,StCal_uncs);
  map_get_float(map,"veff_leg","near",6,veff_leg_near,*runno,&firsttime);
  map_get_float(map,"veff_leg","far",6,veff_leg_far,*runno,&firsttime);
  map_get_float(map,"veff_nose","near",6,veff_nose_near,*runno,&firsttime);
  map_get_float(map,"veff_leg","far",6,veff_nose_far,*runno,&firsttime); 
  map_get_float(map,"st2tof","value",1,&st2tof,*runno,&firsttime);
  map_get_float(map,"delta_T","value",6,offsets,*runno,&firsttime);
  for (i=0;i<6;i++){
    StCal_values[i][VEFF_LEG_NEAR]=veff_leg_near[i];
    StCal_values[i][VEFF_LEG_FAR]=veff_leg_far[i];
    StCal_values[i][VEFF_NOSE_NEAR]=veff_nose_near[i];
    StCal_values[i][VEFF_NOSE_FAR]=veff_nose_far[i];  
    StCal_values[i][TDC_T0]-=offsets[i]+st2tof;
    /* printf("%f %f\n",offsets[i],StCal_values[i][TDC_T0]); */
   }
 fprintf(stderr,"Reading map file %s for run %d.\n",map,*runno);
}

int stncal_read_map_(int *runno,float StnCal_values[24][15],float StnCal_uncs[24][15]){
  char *dir,map[100];
  float st2tof,s2s[6],pd2pd[24];
  float t0_vec[24],t1_vec[24],t2_vec[24];
  float w0_vec[24],w1_vec[24],w2_vec[24];
  float ped_vec[24], m0_vec[24], adc_max[24];
  float veff_leg[24];
  float veff_nose[24];
  float veff_nose1[24];
  float veff_nose2[24];
  float TDC_min, TDC_max;
  int i,j,k,firsttime;
 
  dir = getenv("CLAS_PARMS");
  sprintf(map,"%s/Maps/STN_CALIB.map",dir); 

  map_get_float(map,"M0","value",24,m0_vec,*runno,&firsttime);
  map_get_float(map,"T0","value",24,t0_vec,*runno,&firsttime);
  map_get_float(map,"T1","value",24,t1_vec,*runno,&firsttime);
  map_get_float(map,"T2","value",24,t2_vec,*runno,&firsttime);
  map_get_float(map,"W0","value",24,w0_vec,*runno,&firsttime);
  map_get_float(map,"W1","value",24,w1_vec,*runno,&firsttime);
  map_get_float(map,"W2","value",24,w2_vec,*runno,&firsttime);
  map_get_float(map,"pedestals","value",24,ped_vec,*runno,&firsttime);
  map_get_float(map,"adc_max","value",24,adc_max,*runno,&firsttime);
  map_get_float(map,"veff","leg",24,veff_leg,*runno,&firsttime);
  map_get_float(map,"veff","nose",24,veff_nose,*runno,&firsttime);
  map_get_float(map,"veff","nose1",24,veff_nose1,*runno,&firsttime);
  map_get_float(map,"veff","nose2",24,veff_nose2,*runno,&firsttime);
  map_get_float(map,"delta_T","pd2pd",24,pd2pd,*runno,&firsttime);
  map_get_float(map,"delta_T","s2s",6,s2s,*runno,&firsttime);
  map_get_float(map,"delta_T","st2tof",1,&st2tof,*runno,&firsttime);
  map_get_float(map,"TDC_min","value",1,&TDC_min,*runno,&firsttime);
  map_get_float(map,"TDC_max","value",1,&TDC_max,*runno,&firsttime);

  for (i=0;i<24;i++){
    j=i/4;
    k=i-4*j;
    StnCal_values[i][M0]=m0_vec[i];
    StnCal_values[i][TDC_T0]=t0_vec[i];
    StnCal_values[i][TDC_T1]=t1_vec[i];
    StnCal_values[i][TDC_T2]=t2_vec[i];
    StnCal_values[i][WALK0]=w0_vec[i];
    StnCal_values[i][WALK1]=w1_vec[i];
    StnCal_values[i][WALK2]=w2_vec[i];
    StnCal_values[i][PEDESTAL]=ped_vec[i];
    StnCal_values[i][ADCMAX]=adc_max[i];
    StnCal_values[i][VEFF_LEG]=veff_leg[i];
    StnCal_values[i][VEFF_NOSE]=veff_nose[i];
    StnCal_values[i][VEFF_NOSE1]=veff_nose1[i];
    StnCal_values[i][VEFF_NOSE2]=veff_nose2[i];
    StnCal_values[i][TDC_MIN]=TDC_min;
    StnCal_values[i][TDC_MAX]=TDC_max;
    
    StnCal_values[i][TDC_T0]-=-pd2pd[i]+st2tof+s2s[j];
    /* printf("%f %f\n",offsets[i],StCal_values[i][TDC_T0]); */
   }
 fprintf(stderr,"Reading map file %s for run %d.\n",map,*runno);
}

int st_read_map(char *map,int runno,char *subsystem, int index,
		float StCal_values[6][13],float StCal_uncs[6][13]){
 float values[6],uncertainties[6];
  int i, firsttime;

  map_get_float(map,subsystem,"value",ST_TUBES,values, runno, &firsttime);
  map_get_float(map,subsystem,"unc",ST_TUBES,uncertainties,runno,&firsttime);
  for(i=0;i<6;i++){
    StCal_values[i][index]=values[i];
    StCal_uncs[i][index]=uncertainties[i];
  }
}
