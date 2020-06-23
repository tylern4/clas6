/* -=======================================================-
_begin_doc
RCS ID string
$Id: scaler_util.c,v 1.13 2002/04/10 15:52:59 liji Exp $
$Author: liji $
$Revision: 1.13 $
$Date: 2002/04/10 15:52:59 $
_end_doc
* -=======================================================- */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ntypes.h>
#include <bostypes.h>
#include <math.h>
#include <bosddl.h>
#include <scalers.h>
#include <clas_cern.h>

void addarr(uint32 *first, uint32 *second, uint32 *sum, int size){
  int i;
  for (i=0; i<size; i++){
    sum[i] = first[i] + second[i];
  }
}


void subarr(uint32 *first, uint32 *second, uint32 *sub, int size){
  int i;
  for (i=0; i<size; i++){
    sub[i] = first[i] - second[i];
  }
}

int scaler_event_(){
  clasHEAD_t *HEAD = getBank(&bcs_, "HEAD");
  scaler_event(HEAD->head[0].nrun);
}


int scaler_event(int runno){
  clasS1ST_t *S1ST = getBank(&bcs_, "S1ST");

  make_trgs_sum(runno); 
  make_tgs_sum(runno);
  make_s1st_sum(runno);
}

int printarr(FILE *file, uint32 *arr, int size){
  int i;
  for (i=0; i < size; i++) fprintf(file, "%u ,", arr[i]);
  fprintf(file, "\n");
  
}

clasS1ST_t *make_s1st_sum(int runno){
  clasS1ST_t *S1ST = getBank(&bcs_, "S1ST");
  clasS1ST_t *NEW = NULL;
  clasS1ST_t *OLD = NULL;
  uint32 result[sizeof(s1st_t)/4];
  static uint32 init[sizeof(s1st_t)/4];
  static uint32 old_runs[sizeof(s1st_t)/4];
  static int current_run = -100;
  static int a01_flag = 0;
  static int first = 0;

  /* first event read*/
  if (first == 0){
    clasHEAD_t *HEAD = getBank(&bcs_, "HEAD");
    memset(&old_runs[0], 0, sizeof(s1st_t));
    if (HEAD) {
      if (HEAD->head[0].type == 0){
	memset(init, 0, sizeof(s1st_t));
      } else {
	a01_flag = 1;
      }
    }
    first =1;
  }

  if (S1ST){
    if (runno != current_run){
      
      /* handle A01 etc... files first event is physics event*/
      if (a01_flag) memcpy(init, &(S1ST->s1st[0]), sizeof(s1st_t));

      if (OLD = getBank(&wcs_, "S1ST")){
	memcpy(old_runs, &(OLD->s1st[0]), sizeof(s1st_t));
      }
      current_run = runno;
    }
    /*throw it into a bank*/
    dropBank(&wcs_, "S1ST", 0);
    if ( NEW = makeBank(&wcs_, "S1ST", 0, sizeof(s1st_t)/4, 1)){
      subarr((uint32 *)&(S1ST->s1st[0]), init, result, sizeof(s1st_t)/4);
      addarr(&(result[0]), old_runs, result, sizeof(s1st_t)/4);
      memcpy(&(NEW->s1st[0]), result, sizeof(s1st_t));
    }
  }
  return(NEW);
}



clasSCALER_t *make_trgs_sum(int runno){
  clasSCALER_t *SCALER = getBank(&bcs_, "TRGS");
  clasSCALER_t *NEW = NULL;
  clasSCALER_t *OLD = NULL;
  uint32 result[sizeof(trgs_t)/4];
  static uint32 init[sizeof(trgs_t)/4];
  static uint32 old_runs[sizeof(trgs_t)/4];
  static int current_run = -100;
  static int a01_flag = 0;
  static int first = 0;

  /* first event read*/
  if (first == 0){
    clasHEAD_t *HEAD = getBank(&bcs_, "HEAD");
    memset(&old_runs[0], 0, sizeof(trgs_t));
    if (HEAD) {
      if (HEAD->head[0].type == 0){
	memset(init, 0, sizeof(trgs_t));
      } else {
	a01_flag = 1;
      }
    }
    first =1;
  }

  if (SCALER){
    if (runno != current_run){
      
      /* handle A01 etc... files first event is physics event*/
      if (a01_flag) memcpy(init, &(SCALER->scaler[0]), sizeof(trgs_t));

      if (OLD = getBank(&wcs_, "TRGS")){
	memcpy(old_runs, &(OLD->scaler[0]), sizeof(trgs_t));
      }
      current_run = runno;
    }
    /*throw it into a bank*/
    dropBank(&wcs_, "TRGS", 0);
    if ( NEW = makeBank(&wcs_, "TRGS", 0, sizeof(trgs_t)/4, 1)){
      subarr(&(SCALER->scaler[0]), init, result, sizeof(trgs_t)/4);
      addarr(&(result[0]), old_runs, result, sizeof(trgs_t)/4);
      memcpy(&(NEW->scaler[0]), result, sizeof(trgs_t));
    }
  }
  return(NEW);
}

clasSCALER_t *make_tgs_sum(int runno){
  clasSCALER_t *SCALER = getBank(&bcs_, "TGS ");
  clasSCALER_t *NEW = NULL;
  static uint32 sum[192];
  static int current_run = -100;

  if (current_run == -100) memset(&sum[0], 0, sizeof(tgs_t));

  if (SCALER){
    if (runno != current_run){
      current_run = runno;
    }
    /*throw it into a bank*/
    dropBank(&wcs_, "TGS ", 0);
    if ( NEW = makeBank(&wcs_, "TGS ", 0, sizeof(tgs_t)/4, 1)){
      addarr(&(SCALER->scaler[0]), sum, sum, sizeof(tgs_t)/4);
      memcpy(&(NEW->scaler[0]), sum, sizeof(tgs_t));
    }
  }
  return(NEW);
}

int fill_rtsl(clasSCALER_t *TGS, uint32 *rtsl){ /*kinda trivial*/
  int ret = 0;
  memset(rtsl, 0, sizeof(rtsl_t));
  if (TGS){
    memcpy(rtsl, TGS->scaler, sizeof(rtsl_t));
    ret = 1;
  }
  return ret;
}

clasSCALER_t *make_RTSL_bank(BOSbank *bcs, clasSCALER_t *TGS, int banknum){
  clasSCALER_t *RTSL = NULL;
  int i;

  if (TGS){
    if(RTSL = makeBank(bcs, "RTSL",banknum, sizeof(rtsl_t)/4, 1)){
      fill_rtsl(TGS, RTSL->scaler);
    } 
  }
  return ( RTSL );
}

int fill_g1sl( clasSCALER_t *TGS, uint32 *g1sl){
  int i, ret = 0;

  memset(g1sl, 0, sizeof(g1sl_t));
  if (TGS){
    for(i=0; i < 16; i++){
      g1sl[i*2] = TGS->scaler[i+64];
      g1sl[i*2 + 32] = TGS->scaler[i+96];
    }
    ret=1;
  }
  return ret;
}

clasSCALER_t *make_G1SL_bank(BOSbank *bcs, clasSCALER_t *TGS, int banknum){
  clasSCALER_t *G1SL = NULL;
  int i;
  if (TGS) {
    if(G1SL = makeBank(bcs, "G1SL",banknum, sizeof(g1sl_t)/4, 1)){
      fill_g1sl(TGS, G1SL->scaler);
    }
  } 
  return( G1SL);
}

int fill_g2sl( clasSCALER_t *TGS, uint32 *g2sl){
  int i, ret = 0;

  memset(g2sl, 0, sizeof(g2sl_t));
  if (TGS){
    for(i=0; i < 16; i++){
      g2sl[i*2 + 1] = TGS->scaler[i+80];
      g2sl[i*2 + 33] = TGS->scaler[i+112];
    }
    ret=1;
  }
  return ret;
}

clasSCALER_t *make_G2SL_bank(BOSbank *bcs, clasSCALER_t *TGS, int banknum){
  clasSCALER_t *G2SL = NULL;
  int i;
  if (TGS) {
    if(G2SL = makeBank(bcs, "G2SL",banknum, sizeof(g2sl_t)/4, 1)){
      fill_g2sl(TGS, G2SL->scaler);
    }
  } 
  return( G2SL);
}

int fill_g3sl( clasSCALER_t *TGS, uint32 *g3sl){
  int i, ret = 0;

  memset(g3sl, 0, sizeof(g3sl_t));
  if (TGS){
    for(i=0; i < 16; i++){
      g3sl[i*2] = TGS->scaler[i+128];
      g3sl[i*2 + 32] = TGS->scaler[i+160];
    }
    ret=1;
  }
  return ret;
}

clasSCALER_t *make_G3SL_bank(BOSbank *bcs, clasSCALER_t *TGS, int banknum){
  clasSCALER_t *G3SL = NULL;
  int i;
  if (TGS) {
    if(G3SL = makeBank(bcs, "G3SL",banknum, sizeof(g3sl_t)/3, 1)){
      fill_g3sl(TGS, G3SL->scaler);
    }
  } 
  return( G3SL);
}

int fill_g4sl( clasSCALER_t *TGS, uint32 *g4sl){
  int i, ret = 0;

  memset(g4sl, 0, sizeof(g4sl_t));
  if (TGS){
    for(i=0; i < 16; i++){
      g4sl[i*2 + 1] = TGS->scaler[i+144];
      g4sl[i*2 + 33] = TGS->scaler[i+176];
    }
    ret=1;
  }
  return ret;
}

clasSCALER_t *make_G4SL_bank(BOSbank *bcs, clasSCALER_t *TGS, int banknum){
  clasSCALER_t *G4SL = NULL;
  int i;
  if (TGS) {
    if(G4SL = makeBank(bcs, "G4SL",banknum, sizeof(g4sl_t)/4, 1)){
      fill_g4sl(TGS, G4SL->scaler);
    }
  } 
  return( G4SL);
}

/* routine extracted from scaler_mon - written by Burin*/
void printTRGSinfo(FILE *fp, clasTRGS_t *TRGS){
  unsigned int clock=0,clocksum=0,liveclock=0,runclock=0;
  unsigned int faraday=0,faraday_live=0,faraday_run=0;
  unsigned int MasterOr=0,PC=0,PS=0,TAC=0;
  float clock_s=0.,clocksum_s=0.,liveclock_s=0.,runclock_s=0.;
  float trig_rate=0.,event_rate=0.;
  float Mor_rate=0.,PC_rate=0.,PS_rate=0.,TAC_rate=0.;
  float clock_ratio=0.;
  unsigned int MOR=0;
  unsigned int ntrigs=0;
  float live_clock,live_events,live_run;
  float live_fcup=0.0;

  if (TRGS){
    clock = TRGS->trgs[0].clock_g1_2; 
    clocksum=TRGS->trgs[0].clock_ug; 
    runclock=TRGS->trgs[0].clock_g1; 
    liveclock=TRGS->trgs[0].clock_g2; 
    faraday=TRGS->trgs[0].fcup_ug; 
    MasterOr=TRGS->trgs[0].mor_rg; 
    MOR = TRGS->trgs[0].mor;
    PC=TRGS->trgs[0].pc_rg; 
    PS=TRGS->trgs[0].ps_rg; 
    TAC=TRGS->trgs[0].tac_rg; 
    faraday_live=TRGS->trgs[0].fcup_g2; 
    faraday_run=TRGS->trgs[0].fcup_g1; 
    ntrigs=TRGS->trgs[0].trig_or_ug;
    
    clock_s = (float)clock/(CLOCK_RATE*1000.);
    clocksum_s = (float)clocksum/(CLOCK_RATE*1000.);
    runclock_s = (float)runclock/(CLOCK_RATE*1000.);
    liveclock_s = (float)liveclock/(CLOCK_RATE*1000.);
    clock_ratio = clock_s/runclock_s;
    trig_rate = (float)ntrigs/clocksum_s;
    Mor_rate = (float)MasterOr/runclock_s;
    PC_rate = (float)PC/runclock_s;
    PS_rate = (float)PS/runclock_s;
    TAC_rate = (float)TAC/runclock_s;
    
    if(runclock>0){
      live_clock=(float)liveclock/(float)runclock;
    }  
    if (faraday_run>0){
      live_fcup=(float)faraday_live/(float)faraday_run;
    }
    
    fprintf(fp,"Total number of triggers: %d\n",ntrigs);
    fprintf(fp,"Trigger Rate : %f per s\n\n",trig_rate);
    fprintf(fp,"Ungated Faraday cup: %d counts\n",faraday);
    fprintf(fp,"Ungated Faraday cup: %e C\n\n",(float)faraday*1.e-10);
    fprintf(fp,"Ungated Average current: %f nA \n",0.1*(float)faraday/
	    clocksum_s);
    fprintf(fp,"Run Average current: %f nA \n",0.1*(float)faraday_run/
	    runclock_s);
    fprintf(fp,"Live Average current: %f nA \n\n",0.1*(float)faraday_live/
	    liveclock_s);
    fprintf(fp,"Non-reset clock ungated: %u\n",clocksum);
    fprintf(fp,"Non-reset clock gated on run gate: %u\n",runclock);
    fprintf(fp,"Non-reset clock gated on live time: %u\n",liveclock);
    fprintf(fp,"Sum of gated clock scalers: %u\n\n",clock);
    fprintf(fp,"Non-reset clock ungated: %f s\n",clocksum_s);
    fprintf(fp,"Non-reset clock gated on run gate: %f s\n",runclock_s);
    fprintf(fp,"Non-reset clock gated on live time: %f s\n",liveclock_s);
    fprintf(fp,"Sum of reset clock: %f s\n\n",clock_s);
    
    fprintf(fp,"Ratio of clocks (reset sum/non-reset): %f\n\n",clock_ratio);
    
    fprintf(fp,"Master OR: %f kHz\n",Mor_rate/1000.);
    fprintf(fp,"Master OR counts: %d \n",MOR);
    fprintf(fp,"PC   rate: %f kHz\n",PC_rate/1000.);
    fprintf(fp,"PS   rate: %f kHz\n",PS_rate/1000.);
    fprintf(fp,"TAC  rate: %f kHz\n\n",TAC_rate/1000.);
    
    fprintf(fp,"Live time percentage by clock: %5.2f%%\n",100.0*live_clock);
    fprintf(fp,"Live time percentage by Fcup: %5.2f%%\n\n",100.0*live_fcup);
  } 
}

void printTGSinfo(FILE *fp, clasSCALER_t *TGS){
  uint32 rtsl[SCALER_BANK_SIZE], g1sl[SCALER_BANK_SIZE], g2sl[SCALER_BANK_SIZE], g3sl[SCALER_BANK_SIZE], g4sl[SCALER_BANK_SIZE];
  int i;

  if ( fill_rtsl(TGS, rtsl) && fill_g1sl(TGS, g1sl) && fill_g2sl(TGS, g2sl) && fill_g3sl(TGS, g3sl) && fill_g4sl(TGS, g4sl)){
    
    fprintf(fp,"Tag Scaler Summary (counts):\n");
    fprintf(fp, 
	    "T-counter     Raw         *Set1         *Set2        *Set3        *Set4  \n");
    for(i=0;i<NUM_T_COUNTERS;i++){
      fprintf(fp, "    %2d  %12u %12u %12u %12u %12u \n",i+1, 
	      rtsl[i], g1sl[i], g2sl[i], g3sl[i], g4sl[i]);
    }
  }
}

void printTGSrates(FILE *fp, clasSCALER_t *TGS, clasTRGS_t *TRGS){
  uint32 rtsl[SCALER_BANK_SIZE], g1sl[SCALER_BANK_SIZE], g2sl[SCALER_BANK_SIZE], g3sl[SCALER_BANK_SIZE], g4sl[SCALER_BANK_SIZE];
  int i;

  if ( fill_rtsl(TGS, rtsl) && fill_g1sl(TGS, g1sl) && fill_g2sl(TGS, g2sl) && fill_g3sl(TGS, g3sl) && fill_g4sl(TGS, g4sl) && TRGS->trgs[0].clock_g1_2){
    
    fprintf(fp,"Tag Scaler Summary (Rates in KHz):\n");
    fprintf(fp, 
	    "T-counter   Raw       *Set1       *Set2      *Set3      *Set4  \n");
    for(i=0;i<NUM_T_COUNTERS;i++){
      fprintf(fp, "    %2d  %10.5f %10.5f %10.5f %10.5f %10.5f \n",i+1, 
	      CLOCK_RATE*(float)rtsl[i]/(float)TRGS->trgs[0].clock_g1_2,
	      CLOCK_RATE*(float)g1sl[i]/(float)TRGS->trgs[0].clock_g1_2,
	      CLOCK_RATE*(float)g2sl[i]/(float)TRGS->trgs[0].clock_g1_2,
	      CLOCK_RATE*(float)g3sl[i]/(float)TRGS->trgs[0].clock_g1_2,
	      CLOCK_RATE*(float)g4sl[i]/(float)TRGS->trgs[0].clock_g1_2);
    }
  }

}


int get_eff(uint32 px1[64], uint32 px2[64], float eff[64]) {
  int i;
    
  memset(eff,0.,sizeof(float)*64);
  for(i=0;i<61;i++){
    if(px2[i]>0) {
      if(i>1 && px1[i]!=0 && px1[i-2]!=0 && px1[i-2] > px1[i]*10) px1[i] = 0;
      eff[i] = (float)px1[i]/(float)px2[i];
    }  
  }
  return(1);
}

int smooth(int histo,int fit_type,int parms,float *matrix){
  float fit_results[10];
  int i,j,bad_id=63,bad=0;
  float step[10],pmin[10],pmax[10],sigpar[10],chi2;
  char func[2]="P0";
  float f1,f2;

  memset(fit_results,0,sizeof(float)*10);
  switch (fit_type){
  case GAUSS:
    fit_results[0]=0.02;
    fit_results[1]=23.;
    fit_results[2]=10.;
    hfithn(histo,"G"," ",parms,fit_results,step,pmin,pmax,sigpar,&chi2);
    if(matrix[0]==0.) {
      matrix[0] = matrix[1];
      matrix[60] = matrix[59];
      j = 2;
    }
    else j = 1;     
    for(i=j;i<60;i=i+2) {  
      f1 =  fit_gauss(i-1,fit_results); 
      f2 =  fit_gauss(i+1,fit_results);
      bad = 0;
      if(matrix[i-1]==0.) {
	matrix[i-1] = f1;
	bad_id = i-1;
	bad = 1;
      }  
      if(matrix[i+1]==0.) {
	matrix[i+1] = f2;
	bad_id = i+1;
	bad = 1;
      } 
      matrix[i] = (f1*matrix[i-1]+f2*matrix[i+1])/(f1+f2); 
      if(bad) matrix[bad_id] = 0.;
    }
    break;
  case POLYNOMIAL:
    sprintf(func,"P%d",parms-1);
    hfithn(histo,func," ",parms,fit_results,step,pmin,pmax,sigpar,&chi2);
    if(matrix[0]==0.) {
      matrix[0] = fit_polynomial(1.,fit_results,parms);
      matrix[60] = fit_polynomial(61.,fit_results,parms);
      j = 2;
    }
    else j = 1;     
    for(i=j;i<60;i=i+2) {  
      f1 =  fit_polynomial(i-1,fit_results,parms); 
      f2 =  fit_polynomial(i+1,fit_results,parms);
      bad = 0;
      if(matrix[i-1]==0.) {
	matrix[i-1] = f1;
	bad_id = i-1;
	bad = 1;
      }  
      if(matrix[i+1]==0.) {
	matrix[i+1] = f2;
	bad_id = i+1;
	bad = 1;
      } 
      matrix[i] = (f1*matrix[i-1]+f2*matrix[i+1])/(f1+f2); 
      if(bad) matrix[bad_id] = 0.;
    }
    break;
  default:
    break;
  }
}

float fit_polynomial(float tcounter,float fit_pc[5],int order) {
  float sum=0.;
  int i;
  
  for(i=0;i<order;i++) 
    sum += fit_pc[i]*pow(tcounter,i);
   
  return(sum);    
}

float fit_gauss(float tcounter,float fit_ps[3]) {
  float sum=0.;
  
  sum = fit_ps[0]*exp(-0.5*pow((tcounter-fit_ps[1])/fit_ps[2],2));
   
  return(sum);    
}  

int DSD_efficiency(int gate,clasSCALER_t *TGS,float PC_Eff[64],
    float PS_Eff[64],float Tag_Eff[64]) {
  int i;	
  float PC_T[64];
  float PS_T[64];
  float temp1[64];
  float temp2[64];
  float temp3[64];
  float temp4[64];
  uint32 pc[64];
  uint32 ps1[64];
  uint32 ps2[64];
  uint32 pc_delay[64];
  uint32 ps_delay[64];
  uint32 tac1[64];
  uint32 tac2[64];
  uint32 rawt[64];
  uint32 temp32[64];
  
  memset(PC_Eff,0.,sizeof(float)*64);
  memset(PS_Eff,0.,sizeof(float)*64);
  memset(Tag_Eff,0.,sizeof(float)*64);
  memset(PC_T,0.,sizeof(float)*64);
  memset(PS_T,0.,sizeof(float)*64);
  memset(temp1,0.,sizeof(float)*64);
  memset(temp2,0.,sizeof(float)*64);
  memset(temp3,0.,sizeof(float)*64);
  memset(temp4,0.,sizeof(float)*64);
  memset(pc,0,sizeof(uint32)*64);
  memset(ps1,0,sizeof(uint32)*64);
  memset(ps2,0,sizeof(uint32)*64);
  memset(pc_delay,0,sizeof(uint32)*64);
  memset(ps_delay,0,sizeof(uint32)*64);
  memset(tac1,0,sizeof(uint32)*64);
  memset(tac2,0,sizeof(uint32)*64);
  memset(rawt,0,sizeof(uint32)*64);
  memset(temp32,0,sizeof(uint32)*64);
  if (TGS){
    fill_rtsl(TGS,rawt);
    for(i=0;i<61;i++) hf1(90,(float)(i+1),rawt[i]);
    switch (gate) {
    case PC_PS_TAC_TAC:  /* best case */
      fill_g1sl(TGS,pc);
      fill_g2sl(TGS,ps1);
      fill_g3sl(TGS,tac1);
      fill_g4sl(TGS,tac2);
      get_eff(pc,rawt,PC_T);
      get_eff(ps1,rawt,PS_T);
      get_eff(pc,tac1,PC_Eff);
      get_eff(ps1,tac2,PS_Eff);
      get_eff(tac1,rawt,temp1);
      get_eff(tac2,rawt,temp2);
      for(i=0;i<61;i++) {
	hf1(100,(float)(i+1),PC_Eff[i]);
	hf1(110,(float)(i+1),PS_Eff[i]);
	hf1(115,(float)(i+1),temp1[i]);
	hf1(115,(float)(i+1),temp2[i]);
	hf1(120,(float)(i+1),pc[i]);
	hf1(130,(float)(i+1),ps1[i]);
	hf1(140,(float)(i+1),tac1[i]);
	hf1(140,(float)(i+1),tac2[i]);
      }

      smooth(100,POLYNOMIAL,5,PC_Eff);
      smooth(110,GAUSS,3,PS_Eff);
      for(i=0;i<61;i++) {
	if(temp1[i]!=0.) Tag_Eff[i] = temp1[i];
	if(temp2[i]!=0.) Tag_Eff[i] = temp2[i];
	hf1(150,(float)(i+1),PC_Eff[i]);
	hf1(160,(float)(i+1),PS_Eff[i]);
	hf1(170,(float)(i+1),Tag_Eff[i]);
      }
      fprintf(stdout,"Normalization PS Scalers: PC_PS_TAC_TAC gate\n");
      for(i=0;i<64;i++) fprintf(stdout,"%f\n", PS_Eff[i]*Tag_Eff[i]*rawt[i]);

      break;
    case PC_TAC_PS_TAC:  /* stupid case */
      fill_g1sl(TGS,pc);
      fill_g2sl(TGS,tac1);
      fill_g3sl(TGS,ps1);
      fill_g4sl(TGS,tac2);
      get_eff(pc,rawt,PC_T);
      get_eff(ps1,rawt,PS_T);
      get_eff(tac1,rawt,Tag_Eff);
      for(i=0;i<61;i++) {
	hf1(100,(float)(i+1),PC_T[i]);
	hf1(110,(float)(i+1),PS_T[i]);
	hf1(115,(float)(i+1),Tag_Eff[i]);
	hf1(120,(float)(i+1),pc[i]);
	hf1(130,(float)(i+1),ps1[i]);
	hf1(140,(float)(i+1),tac1[i]);
	hf1(140,(float)(i+1),tac2[i]);
      }  
      smooth(100,POLYNOMIAL,5,PC_T);      
      smooth(110,GAUSS,3,PS_T);
      smooth(115,POLYNOMIAL,1,Tag_Eff);
      for(i=0;i<61;i++) {
	if(Tag_Eff[i]!=0.) {
	  PC_Eff[i] = PC_T[i]/Tag_Eff[i];
	  PS_Eff[i] = PS_T[i]/Tag_Eff[i];
	}
	hf1(150,(float)(i+1),PC_Eff[i]);
	hf1(160,(float)(i+1),PS_Eff[i]);
	hf1(170,(float)(i+1),Tag_Eff[i]);
      }
      fprintf(stdout,"Normalization PS Scalers: PC_TAC_PS_TAC gate\n");
      for (i=0;i<64;i++) fprintf(stdout,"%f\n",PS_T[i]*rawt[i]);

      break;
    case TAC_PS_PS_TAC:  /* g1c case */
      fill_g1sl(TGS,tac1);
      fill_g2sl(TGS,ps1);
      fill_g3sl(TGS,ps2);
      fill_g4sl(TGS,tac2);
      addarr(ps1,ps2,temp32,64); 
      fprintf(stdout,"Normalization PS Scalers: TAC_PS_PS_TAC gate\n");
      for (i=0;i<64;i++) fprintf(stdout,"%f\n",(float)temp32[i]);
      
      get_eff(ps2,tac1,temp1);
      get_eff(ps1,tac2,temp2); 
      get_eff(tac1,rawt,temp3);
      get_eff(tac2,rawt,temp4);
      for(i=0;i<61;i++) {
	if(temp1[i]!=0.) PS_Eff[i] = temp1[i];
	if(temp2[i]!=0.) PS_Eff[i] = temp2[i];
	if(temp3[i]!=0.) Tag_Eff[i] = temp3[i];
	if(temp4[i]!=0.) Tag_Eff[i] = temp4[i];
	hf1(160,(float)(i+1),PS_Eff[i]);
	hf1(170,(float)(i+1),Tag_Eff[i]);
      }  
      break;
    default:
      break;
    }

   

    printf("T-counter   PC Eff.     PS Eff.    Tagging Eff.\n");
    for(i=0;i<61;i++) 
      printf("%5d %12.5f %12.5f %12.5f\n",i+1,PC_Eff[i],PS_Eff[i],Tag_Eff[i]);
    return 1;
  }
  return 0;
} 

void book_scaler_histos( int runno){
  int i;
  char title[100];
  char runmes[50];
  
  sprintf(runmes,"Run %d", runno);

  /* "Generic Histograms for scaler monitor" */

  sprintf(title,"Raw T, %s",runmes);
  hbook1(90, title, 61, 1., 62.,0.);

  sprintf(title, "PC Efficiency, %s", runmes);
  hbook1(100, title, 61, 1., 62., 0.);
  sprintf(title, "PS Efficiency, %s", runmes);
  hbook1(110, title, 61, 1., 62., 0.);
  sprintf(title, "Tagging Efficiency, %s", runmes);
  hbook1(115, title, 61, 1., 62., 0.);
  sprintf(title, "PC, %s", runmes);
  hbook1(120, title, 61, 1., 62., 0.);
  sprintf(title, "PS, %s", runmes);
  hbook1(130, title, 61, 1., 62., 0.);
  sprintf(title, "TAC, %s", runmes);
  hbook1(140, title, 61, 1., 62., 0.);
  sprintf(title, "PC Efficiency All, %s", runmes);
  hbook1(150, title, 61, 1., 62., 0.);
  sprintf(title, "PS Efficiency All, %s", runmes);
  hbook1(160, title, 61, 1., 62., 0.);
  sprintf(title, "Tagging Efficiency All, %s", runmes);
  hbook1(170, title, 61, 1., 62., 0.);
  sprintf(title, "E-T, %s", runmes);
  hbook2(500, title, 768,1., 769.,121,1.,122., 0.);
  sprintf(title, "E, %s", runmes);
  hbook1(510, title, 768,1., 769., 0.);
  sprintf(title, "T, %s", runmes);
  hbook1(520, title, 121,1.,122., 0.);
  sprintf(title, "PC Production, %s", runmes);
  hbook1(1000, title, 61, 1., 62., 0.);
  sprintf(title, "PCdelay Production, %s", runmes);
  hbook1(1100, title, 61, 1., 62., 0.);
  sprintf(title, "PS Production, %s", runmes);
  hbook1(1200, title, 61, 1., 62., 0.);
  sprintf(title, "PSdelay Production, %s", runmes);
  hbook1(1300, title, 61, 1., 62., 0.);
  sprintf(title, "PC All Production, %s", runmes);
  hbook1(2000, title, 61, 1., 62., 0.);
  sprintf(title, "PCdelay All Production, %s", runmes);
  hbook1(2100, title, 61, 1., 62., 0.);
  sprintf(title, "PS All Production, %s", runmes);
  hbook1(2200, title, 61, 1., 62., 0.);
  sprintf(title, "PSdelay All Production, %s", runmes);
  hbook1(2300, title, 61, 1., 62., 0.);
  sprintf(title, "Ngamma Production, %s", runmes);
  hbook1(5000, title, 767, 0.5, 767.5, 0.);
  sprintf(title, "Ngamma Normalisation, %s", runmes);
  hbook1(6000, title, 767, 0.5, 767.5, 0.);
  hbook1(7000,"tagr no taco required",767,0.5,767.5,0);
  hbook2(9000,"tac tdc vs. e_id",767,0.5,767.5,120,-25.0,5.,0);
}  

