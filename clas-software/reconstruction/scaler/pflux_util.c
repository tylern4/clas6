#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <makebanks.h>
#include <call.h>
#include <pid.h>
#include <scalers.h>
#include <math.h>
#include <bosddl.h>
#include <clas_cern.h>
#include <kinematics.h>
#include <map_manager.h>

/* ------------------------------------------------------------- */
int calc_flux_data(int runno, int gate, int norm_run, float *F_NORM, float *DELTA_F_NORM, float *Ngam, float *Delta_Ngam, int batch)
{
  /**
     To call these functions for your programs:

     - include the file pflux.h from $CLAS_PACK/include

     - define the variables:
       runno = run number
       gate = scaler gate of the run from TAG_SCALER.map
       norm_run = number of the run for normalization (only if runno=prod. run)

       batch = 1 if you don't want printout on the screen

       Ngam[1:767] = number of normalization photons for the production run
       Delta_Ngam[1:767] = Statistical error of Ngam

       F_NORM[1:61] = Normalization factor for each T-counter
       DELTA_F_NORM[1:61] = Statistical error of F_NORM
  **/
     

  int i;
  clasTRGS_t *TRGS = getBank(&wcs_, "TRGS");
  clasSCALER_t *TGS = getBank(&wcs_, "TGS ");
  clasS1ST_t *S1ST = getBank(&wcs_, "S1ST");

  char *dir = getenv("CLAS_PARMS"); 
  char map[100];
  int firsttime;


  if( (gate==PC_PS_PCdelay_PSdelay) || (gate==PSdelay_PS_PS_PSdelay) ) {
    /*** production run ***/
    float prod_live_time=1., prod_live_time_clock=1.;
    float prescale_prod=0., prescale_norm=0.;
    uint32 RAW_T[NUM_T_COUNTERS+1];
    
    uint32 rawt[64];

    FILE *fp=NULL;
    char *filename=NULL;
    char name[200];

    memset(&RAW_T[0], 0, sizeof(RAW_T));
    memset(&rawt[0],0,sizeof(rawt));

    /*** prescaling factors from map **/
    sprintf(map,"%s/Maps/NORM.map",dir);

    map_get_float(map,"normalization","prescale",1,&prescale_norm,runno,&firsttime);
    if (!batch) fprintf(stderr,"Normalisation prescale factor: %f\n",prescale_norm);
    map_get_float(map,"production","prescale",1,&prescale_prod,runno,&firsttime);
    if (!batch) fprintf(stderr,"Production prescale factor: %f\n",prescale_prod);

    /* prod. livetime */
    sprintf(name,"livetime_prod%d.dat",runno);
    filename=&name[0];
    fp=fopen(filename,"w");
    if (S1ST && TRGS) {
      unsigned int clock=TRGS->trgs[0].trig_or_ug; 
      unsigned int liveclock=S1ST->s1st[0].event_count;
      if (clock&&liveclock){
	prod_live_time=(float)liveclock/(float)clock;
      }
      /*** livetime from clock ***/
      liveclock=TRGS->trgs[0].clock_g2;
      clock=TRGS->trgs[0].clock_g1;
      if (clock&&liveclock){
	prod_live_time_clock=(float)liveclock/(float)clock;
      }
    }
    if (!batch) {
      fprintf(stderr,"Production livetime from s1st trig bit 7: %f\n",prod_live_time);
      fprintf(fp,"%f\n",prod_live_time);
      fprintf(stderr,"Production livetime from clock: %f\n",prod_live_time_clock);
      /* fprintf(fp,"%f\n",prod_live_time_clock); */
    }
    fclose(fp);
    
    /*** scalers ***/
    if (TGS){
      /* RAW T */
      fill_rtsl(TGS,rawt);
      
      /* output file of RAWT */
      sprintf(name,"rawt_prod%d.dat",runno);
      filename=&name[0];
      fp=fopen(filename,"w");
      
      for (i=1; i<=NUM_T_COUNTERS; i++) {
	RAW_T[i]=rawt[i-1];
	hf1(90,i, RAW_T[i]);
	fprintf(fp,"%d\n",RAW_T[i]);
      }
      fprintf(fp,"%d\n%d\n%d\n",0,0,0);
      fclose(fp);
    }
    else {
      fprintf(stderr," ERROR: NO scaler events found\n");
      return(0);
    }
    
    
    flux_prod(runno, prod_live_time, prescale_prod, prescale_norm, norm_run, &RAW_T[0], &Ngam[0], &Delta_Ngam[0], batch);
  }
  else if ( (gate==PC_PS_TAC_TAC) || (gate==PC_TAC_PS_TAC) || (gate==TAC_PS_PS_TAC) ) {
    /*** normalization run ***/
    flux_norm(runno, TGS, TRGS, S1ST, &F_NORM[0], &DELTA_F_NORM[0], batch);
  }

  return(1);
}
/* ----------------------------------------------------------------- */
int flux_norm(int runno, clasSCALER_t *TGS, clasTRGS_t *TRGS, clasS1ST_t *S1ST, float *F_NORM, float *DELTA_F_NORM, int batch)
{
  /**
     RAW_T[1:61] = Number of scaler counts in each T-counter
     TAC[1:61] = Number of scaler counts in each T-counters 
                 with TAC coincidence

     TC[1:121] = Number of good photons (TAGR && TACO) for each T-channel

     F_NORM[1:61] = Normalization factor for each T-counter 
                  = TCtot[t]/RAW_T[t]
		    TCtot summed over T-channels counts TC[t] from T2Tbinmin(t) to T2Tbinmax(t)

     DELTA_F_NORM[1:61] = Statistical error of F_NORM

     norm_live_time = livetime from S1ST trigger bit 7
  **/

  FILE *fp=NULL;
  char *f_norm=NULL;
  char name[200];

  char *dir = getenv("CLAS_PARMS"); 
  char map[100];
  int firsttime,gate;
  
  int n,i;

  float TC[NUM_T_BINS+1];
  uint32 RAW_T[NUM_T_COUNTERS+1];
  uint32 TAC[NUM_T_COUNTERS+1];

  float arr[NUM_T_BINS+1];
  float arr2[NUM_E_BINS+1];
  uint32 tac1[64];
  uint32 tac2[64];
  uint32 rawt[64];

  float norm_live_time=1.,norm_live_time_clock=1.;
  float norm_prescale=0.;

  memset(&RAW_T[0], 0, sizeof(RAW_T));
  memset(&TAC[0], 0, sizeof(TAC));
  memset(&TC[0], 0, sizeof(TC));

  memset(&arr[0], 0, sizeof(arr));
  memset(&arr2[0], 0, sizeof(arr2));
  memset(&tac1[0], 0, sizeof(tac1));
  memset(&tac2[0], 0, sizeof(tac2));
  memset(&rawt[0], 0, sizeof(rawt));

  /** output file **/
  sprintf(name,"livetime_norm%d.dat",runno);
  f_norm=&name[0];
  fp=fopen(f_norm,"w");

  /* prod. livetime */
  if (S1ST && TRGS) {
    /*** livetime from S1ST ***/
    unsigned int clock=TRGS->trgs[0].trig_or_ug; 
    unsigned int liveclock=S1ST->s1st[0].event_count;
    if (clock&&liveclock){
      norm_live_time=(float)liveclock/(float)clock;
    }
    /*** livetime from clock ***/
    liveclock=TRGS->trgs[0].clock_g2;
    clock=TRGS->trgs[0].clock_g1;
    if (clock&&liveclock){
      norm_live_time_clock=(float)liveclock/(float)clock;
    }
  }
  if (!batch) {
    fprintf(stderr,"Normalisation livetime from s1st trig bit 7: %f\n",norm_live_time);
    fprintf(fp,"%f\n",norm_live_time);
    fprintf(stderr,"Normalisation livetime from clock: %f\n",norm_live_time_clock);
    /* fprintf(fp,"%f\n",norm_live_time_clock); */
  }
  fclose(fp);

  /** read gate from map **/
  sprintf(map,"%s/Maps/TAG_SCALER.map",dir);
  map_get_int(map,"gate","value",1,&gate,runno,&firsttime);

  /*** scalers ***/
  if (TGS){
    /* RAW T */
    fill_rtsl(TGS,rawt);
    /* TAC */
    switch (gate) {
    case PC_PS_TAC_TAC:
      fill_g3sl(TGS,tac1);
      fill_g4sl(TGS,tac2);
      break;
    case PC_TAC_PS_TAC:
      fill_g2sl(TGS,tac1);
      fill_g4sl(TGS,tac2);
      break;
    case TAC_PS_PS_TAC:
      fill_g1sl(TGS,tac1);
      fill_g4sl(TGS,tac2);
      break;
    default:
      break;
    }

    for (i=0; i<NUM_T_COUNTERS; i++) {
      RAW_T[i+1]=(int)rawt[i];
      TAC[i+1]=(int)tac1[i]+(int)tac2[i];
      
      hf1(90,(i+1), RAW_T[i+1]);
      hf1(140,(i+1), TAC[i+1]);
      if (RAW_T[i+1]>0) hf1(170,(i+1), (float)TAC[i+1]/(float)RAW_T[i+1]);
    }
  }
  else {
    fprintf(stderr,"ERROR: NO scaler events found\n");
    return(0);
  }

  /*** Number of good photons for each Tchannel TC[i] ***/
  hunpak(6001,arr,"HIST",1);
  for (i=1; i<=NUM_T_BINS; i++) {
    TC[i]=arr[i-1];
  }

  /*** Number of good photons for each Echannel ***/
  sprintf(name,"ngamma_norm%d.dat",runno);
  f_norm=&name[0];
  fp=fopen(f_norm,"w");  
  hunpak(6000,arr2,"HIST",1);
  for (i=1; i<=NUM_E_BINS; i++) {
    fprintf(fp,"%f\n",arr2[i-1]);
  }
  fclose(fp);

  /*** F_NORM calculation ***/

  /** output file **/
  sprintf(name,"f_norm%d.dat",runno);
  f_norm=&name[0];
  fp=fopen(f_norm,"w");

  for (n = 1; n<= NUM_T_COUNTERS; n++){
    int TCmin=T2Tbinmin(n);
    int TCmax=T2Tbinmax(n);
    int tc;
    float TCtot=0.;

    for (tc=TCmin; tc<=TCmax; tc++) {
      TCtot=TCtot+TC[tc];
    }
    if(RAW_T[n] != 0) {
      F_NORM[n]=TCtot/(float)(RAW_T[n]);
      if (TCtot != 0)
	DELTA_F_NORM[n]=F_NORM[n]*sqrt(1./TCtot+1./(float)(RAW_T[n]));
    }
      
    if (!batch) fprintf(stdout,"%f  %f\n",F_NORM[n],DELTA_F_NORM[n]);
    fprintf(fp,"%f  %f\n",F_NORM[n],DELTA_F_NORM[n]);
    hf1(180,n,F_NORM[n]);
  }

  fclose (fp);

  return (1);
} 
/*-------------------------------------------------------------------*/
int flux_prod(int runno, float prod_live_time, float prescale_prod, float prescale_norm, int norm_run, uint32 *RAW_T, float *Ngam, float *Delta_Ngam, int batch)
{
  /** 
     RAW_T[1:61] = Number of scaler counts in each T-counter

     TC[1:121] = Number of TDC events in each T-channel
     Tcount[1:61]= Number of TDC events in each T-counter summed over 
                   T-channels counts TC[t] from T2Tbinmin(t) to T2Tbinmax(t)
		   (functions in packages/scaler/E_T_util.c)
     NE[1:767][1:121] = Number of TDC events in each E-channel and T-channel
     NETcount[1:767][1:61] = Number of TDC events in each E-channel and T-counter
                             summed from NE[m][tc] over "first hit" channels
			     (only 2 contributions for each T-counter)

     Coincidence E-channel - T-counter found from general functions E_binmin(m)
     and E_binmax(m) in packages/scaler/E_T_util.c

     Ngam[1:767] = number of normalization photons for the run
     Delta_Ngam[1:767] = Statistical error of Ngam

     F_NORM[1:61] = Normalization factor for each T-counter (from file "FnormXXXXX.dat")
     DELTA_F_NORM[1:61] = Statistical error of F_NORM

     norm_live_time = normalization run livetime from S1ST trigger bit 7 
                      (from first row of file "livetimeXXXXX.dat")
     prod_live_time = production run livetime from S1ST trigger bit 7
  **/
      
  char *f_norm = NULL;
  char *livetime_norm = NULL;
  char *f_prod = NULL;
  char name[200];
  char string[50];

  int tbinmin[NUM_E_BINS+1];
  int tbinmax[NUM_E_BINS+1];
  FILE *fp = NULL;  

  char *dir = getenv("CLAS_PARMS"); 
  char map[100];
  int firsttime;
  int n,i,j,m,t,tc;

  float norm_live_time=1., norm_live_time_clock=1.;

  float NE[NUM_E_BINS+1][NUM_T_BINS+1];
  float NETcount[NUM_E_BINS+1][NUM_T_COUNTERS+1];
  float vect[NUM_E_BINS*NUM_T_BINS];
  float arr[NUM_T_BINS+1];
  float TC[NUM_T_BINS+1];
  float Tcount[NUM_T_COUNTERS+1];
  float F_NORM[NUM_T_COUNTERS+1];
  float DELTA_F_NORM[NUM_T_COUNTERS+1];

  float nom[NUM_E_BINS+1][3];
  float denom[NUM_E_BINS+1][3];
  float term[NUM_E_BINS+1][3];

  /* Echannels-Tcounters coincidence matrix */
  int Tmin_of_EC[NUM_E_BINS+1];
  int Tmax_of_EC[NUM_E_BINS+1];

  memset(&tbinmin[0], 0,sizeof(int));
  memset(&tbinmax[0], 0,sizeof(int));

  memset(&TC[0], 0, sizeof(TC));
  memset(&Tcount[0], 0, sizeof(Tcount));
  memset(&F_NORM[0], 0, sizeof(F_NORM));
  memset(&DELTA_F_NORM[0], 0, sizeof(DELTA_F_NORM));
  memset(&NE[0][0], 0, sizeof(NE));
  memset(&NETcount[0][0], 0, sizeof(NETcount));
  memset(&vect[0], 0, sizeof(vect));
  memset(&arr[0], 0, sizeof(arr));

  memset(&nom[0][0],0,sizeof(nom));
  memset(&denom[0][0],0,sizeof(denom));
  memset(&term[0][0],0,sizeof(term));

  memset(&Tmin_of_EC[0], 0,sizeof(Tmin_of_EC));
  memset(&Tmax_of_EC[0], 0,sizeof(Tmax_of_EC));


  sprintf(map,"%s/Maps/NORM.map",dir);
  printf("Using the map %s/Maps/NORM.map\n",dir);

  /*** reading stuff from normalization run ***/
  map_get_float(map,"normalization","livetime",1,&norm_live_time,norm_run,&firsttime);
  if (firsttime!=norm_run) 
    fprintf(stdout," ===> WARNING: no livetime for NORM. run %d in %s map, using run %d\n",norm_run,map,firsttime);
  if (!batch) fprintf(stdout,"Normalisation livetime: %f\n",norm_live_time);
  map_get_float(map,"normalization","prescale",1,&prescale_norm,norm_run,&firsttime);
  if (!batch) fprintf(stdout,"Normalisation prescale factor: %f\n",prescale_norm);

  map_get_float(map,"normalization","F_norm",61,&arr[0],norm_run,&firsttime);
  for (t=1; t<=NUM_T_COUNTERS; t++) {
    F_NORM[t]=arr[t-1];
    hf1(180,t,F_NORM[t]);
  }

  map_get_float(map,"normalization","F_norm_u",61,&arr[0],norm_run,&firsttime);
  if (firsttime!=norm_run) 
    fprintf(stdout," ===> WARNING: no F_NORM for NORM. run %d in %s map, using run %d\n",norm_run,map,firsttime);
  for (t=1; t<=NUM_T_COUNTERS; t++) DELTA_F_NORM[t]=arr[t-1];
  if (!batch) {
    fprintf(stdout,"F_NORM factor \n");
    for (t=1; t<=NUM_T_COUNTERS; t++) fprintf(stdout,"%f %f\n",F_NORM[t],DELTA_F_NORM[t]);
  }

  /* prescalig factor for prod. run */
  map_get_float(map,"production","prescale",1,&prescale_prod,runno,&firsttime);
  if (!batch) fprintf(stdout,"Production prescale factor: %f\n",prescale_prod);

  /*** Number of photons for each Tchannel TC[i] ***/
  hunpak(520,arr,"HIST",1);
  for (i=1; i<=NUM_T_BINS; i++) {
    TC[i]=arr[i-1];
  }

  /*** Number of photons in Echannel m and Tchannel t NE[m][t] **/
  hunpak(500,vect,"HIST",1);
  for (m=1; m<=NUM_E_BINS; m++) {
    for (t=1; t<=NUM_T_BINS; t++) {
      int iv=(m-1)+NUM_E_BINS*(t-1);
      NE[m][t]=vect[iv];
    }
  }

  /* Number of photons Tcount[t] in each Tcounter summed from TC[i] */
  for (t=1; t<=NUM_T_COUNTERS; t++) {
    int TCmin=T2Tbinmin(t);
    int TCmax=T2Tbinmax(t);
    int tc;
    for (tc=TCmin; tc<=TCmax; tc++) {
      Tcount[t]=Tcount[t]+TC[tc];
    }
    hf1(5002,t,Tcount[t]);
  }

  /* Number of photons NETcount[m][t] in Echannel m and Tcounter t summed from NE[m][tc] */
  /* Only "first hit" in TC->T conversion */
  for (m=1; m<=NUM_E_BINS; m++) {
    int Tmin=0;
    int Tmax=0;
    if (m%2) {
      Tmin_of_EC[m]=tmin((m+1)/2);
      Tmax_of_EC[m]=tmax((m+1)/2);
    }
    else {
      Tmin_of_EC[m]=tmin(m/2);
      Tmax_of_EC[m]=tmax((m+2)/2);
    }

    for (t=Tmin_of_EC[m]; t<=Tmax_of_EC[m]; t++) {
      int TCmin=T2Tbinmin(t);
      int TCmax=T2Tbinmax(t);
      int tc;
      /** T-counters from 2 to 120 have contributions from 3 channels **/
      /** "first hit" contributions from last 2 only                  **/
      if ( (t!=1) && (t!=121) ) TCmin=TCmin+1;

      for (tc=TCmin; tc<=TCmax; tc++) { 
	NETcount[m][t]=NETcount[m][t]+NE[m][tc];
      }

      hf2(5003,m,t,NETcount[m][t]);
    }

  }

  /* Total number of photons */
  sprintf(name,"ngamma_prod%d.dat",runno);
  f_prod=&name[0];
  fp = fopen(f_prod, "w");
  for (m=1; m<=NUM_E_BINS; m++) {
    for (t=Tmin_of_EC[m]; t<=Tmax_of_EC[m]; t++) {
      if ( (NETcount[m][t]>0) && (Tcount[t]>0) && (RAW_T[t]>0) && (F_NORM[t]>0) ) {
	float term = (NETcount[m][t]/Tcount[t]) * (float)RAW_T[t] * F_NORM[t];
	float err_prod = 1./NETcount[m][t] + 1./Tcount[t] + 1./(float)RAW_T[t];
	float err_norm = pow((DELTA_F_NORM[t]/F_NORM[t]),2);
	float err_tot=0.;
	Ngam[m]=Ngam[m]+term;
	err_tot=term*term*(err_prod+err_norm);
	Delta_Ngam[m]=Delta_Ngam[m]+err_tot;
      }
    }

    Ngam[m]=Ngam[m]*(prod_live_time/norm_live_time)*(1.+prescale_norm)/(1.+prescale_prod);

    Delta_Ngam[m]=sqrt(Delta_Ngam[m])*(prod_live_time/norm_live_time)*
      (1.+prescale_norm)/(1.+prescale_prod);

    hf1(5000,m,Ngam[m]);
    fprintf(fp,"%f  %f\n",Ngam[m],Delta_Ngam[m]);
  }
  fclose(fp);

  return(1);
}
/* --------------------------------------------------------------------------------------- */
/* This function returns the number of photons for a part of a production run
   by scaling with the ratio of T-counters scalers. 
   It need the counts of tagger scalers in the array SCALER and livetime for production run.
   It reads all other informations from the maps */
int ngamma_flux_prod(int runno, uint32 *SCALERS, float prod_live_time_temp, int norm_run, float *Ngam, float *Delta_Ngam)
{
  int t,m;
  FILE *fp=NULL;
  char *ff;
  float Ngam_temp=0.;
  float deltaNgam_temp=0.;
  char map[100];
  int firsttime=0;
  char *dir = getenv("CLAS_PARMS"); 

  float prod_live_time=1;

  int array[64];

  uint32 RAW_T[NUM_T_COUNTERS+1];
  float rawt_ratio[NUM_T_COUNTERS+1];
  float ngamma_tot[NUM_E_BINS];
  float delta_ngamma_tot[NUM_E_BINS];

  /* Echannels-Tcounters coincidence matrix */
  int Tmin_of_EC[NUM_E_BINS+1];
  int Tmax_of_EC[NUM_E_BINS+1];

  memset (&array[0], 0, sizeof(array));

  memset (&RAW_T[0], 0, sizeof(RAW_T));
  memset (&rawt_ratio[0], 0, sizeof(rawt_ratio));
  memset (&ngamma_tot[0], 0, sizeof(ngamma_tot));
  memset (&delta_ngamma_tot[0], 0, sizeof(delta_ngamma_tot));

  /* Ecounter-Tcounter coincidence matrix */
  fill_E_T_map();

  sprintf(map,"%s/Maps/NORM.map",dir);

  /*** Data from the full production run ***/
  /* livetime and prescaling */
  map_get_float(map,"production","livetime",1,&prod_live_time,runno,&firsttime);
  if (firsttime!=runno) 
    fprintf(stdout,"\n===> WARNING: no livetime for PROD. run %d, using run %d\n",runno, firsttime);
  fprintf(stdout,"PROD: livetime from run %d: %f\n",firsttime,prod_live_time);

  /* total number of photons */
  map_get_float(map,"production","pflux_tscal",NUM_E_BINS, &ngamma_tot[0], runno, &firsttime);
  map_get_float(map,"production","pflux_tscal_u",NUM_E_BINS, &delta_ngamma_tot[0], runno, &firsttime);
  for (m=1; m<=NUM_E_BINS; m++) hf1(5000,m,ngamma_tot[m]);
  if (firsttime!=runno) 
    fprintf(stdout,"\n===> WARNING: no NGAMMA in %s for PROD. run %d, using run %d\n",map,runno, firsttime);

  /* RAWT scalers */
  /* da mettere in NORM.map? */
  sprintf(map,"%s/Maps/TAG_SCALER.map",dir);
  map_get_int(map,"RTSL","value", 64, &array[0], runno, &firsttime);
  if (firsttime!=runno) 
    fprintf(stdout,"\n===> WARNING: no RAWT data in %s for PROD. run %d, using run %d\n",map,runno, firsttime);
  for (t=1; t<=NUM_T_COUNTERS; t++) {
    RAW_T[t]=array[t-1];
    hf1(90,t,RAW_T[t]);
    hf1(91,t,*(SCALERS+t));
  }
  
  /* ratio of T-counter scalers */
  for (t=1; t<=NUM_T_COUNTERS; t++) {
    if (RAW_T[t]>0) {
       rawt_ratio[t]=(float)(*(SCALERS+t)) / (float)RAW_T[t];
      hf1(92,t,rawt_ratio[t]);
    }
  }


  /* ngamma calculation for partial run */
  sprintf(map,"ngamma_prod%d.dat",runno);
  ff=&map[0];
  fp=fopen(ff,"w");
  for (m=1; m<=NUM_E_BINS; m++) {
    float frac=0.;
    int ntcount=0;
    float ratio=0;

    if (m%2) {
      Tmin_of_EC[m]=tmin((m+1)/2);
      Tmax_of_EC[m]=tmax((m+1)/2);
    }
    else {
      Tmin_of_EC[m]=tmin(m/2);
      Tmax_of_EC[m]=tmax((m+2)/2);
    }

    for (t=Tmin_of_EC[m]; t<=Tmax_of_EC[m]; t++) {
      /* sum over T-counters contributions */
      if (rawt_ratio[t]>0) {
	frac=frac+rawt_ratio[t];
	ntcount++;
      }
    }
    frac=frac/(float)ntcount;

    Ngam_temp=ngamma_tot[m]*frac;
    deltaNgam_temp=delta_ngamma_tot[m]*frac;

    /* correction for livetime */
    Ngam_temp=Ngam_temp*prod_live_time_temp/prod_live_time;
    deltaNgam_temp=deltaNgam_temp*prod_live_time_temp/prod_live_time;

    fprintf(fp,"%f %f\n",Ngam_temp,deltaNgam_temp);
    hf1(5010,m,Ngam_temp);
    if(ngamma_tot[m]>0) ratio=Ngam_temp/ngamma_tot[m];
    hf1(5020,m,ratio);

    Ngam[m] = Ngam_temp;
    Delta_Ngam[m] = deltaNgam_temp;
  }
  fclose(fp);

  return(1);
}
/* ------------------------------------------------------------------ */
void book_histos_test(int runno)
{
  char title[100];
  int i,j,k,l,m;

  char runmes[50];
  sprintf(runmes,"Run %d", runno);

  sprintf(title,"Ungated T-counters (scalers), full run %d",runno);
  hbook1(90, title, NUM_T_COUNTERS, 0.5, NUM_T_COUNTERS+0.5,0.);
  sprintf(title,"Ungated T-counters (scalers), partial run %d",runno);
  hbook1(91, title, NUM_T_COUNTERS, 0.5, NUM_T_COUNTERS+0.5,0.);
  sprintf(title,"Ratio of T-counters (scalers), %s",runmes);
  hbook1(92, title, NUM_T_COUNTERS, 0.5, NUM_T_COUNTERS+0.5,0.);

  sprintf(title, "Ngamma (production run) vs E-channel, full run %d",runno);
  hbook1(5000, title, NUM_E_BINS, 0.5, NUM_E_BINS+0.5, 0.);
  sprintf(title, "Ngamma (production run) vs E-channel, partial run %d",runno);
  hbook1(5010, title, NUM_E_BINS, 0.5, NUM_E_BINS+0.5, 0.);
  sprintf(title, "Ratio of Ngamma (production run) vs E-channel, %s",runmes);
  hbook1(5020, title, NUM_E_BINS, 0.5, NUM_E_BINS+0.5, 0.);

  return;
}
/*-------------------------------------------------------------------*/
int get_pflux_tscal_map(int runno, float *Ngam, float *Delta_Ngam)
{
  char *dir, map[100];
  int firsttime;

  dir = getenv("CLAS_PARMS"); 
  sprintf(map,"%s/Maps/NORM.map",dir); 
  map_get_float(map,"production","pflux_tscal",NUM_E_BINS,&Ngam[0],runno,&firsttime);
  map_get_float(map,"production","pflux_tscal_u",NUM_E_BINS,&Delta_Ngam[0],runno,&firsttime);

  return(1);
}
/*-------------------------------------------------------------------*/
int ngamma_flux_prod_(int runno, float *Ngam, float *Delta_Ngam)
{
  float prod_live_time_temp=1.0;
  int i;
  int norm_run=0;

  clasTRGS_t *TRGS = (clasTRGS_t *) getBank(&wcs_, "TRGS");
  clasSCALER_t *TGS = (clasSCALER_t *) getBank(&wcs_, "TGS "); 
  clasS1ST_t *S1ST = (clasS1ST_t *)getBank(&wcs_, "S1ST");
  
  uint32 SCALERS[NUM_T_COUNTERS+1];
  uint32 rawt[64];

  memset(&SCALERS[0], 0, sizeof(SCALERS));
  memset(&rawt[0], 0, sizeof(rawt));
  memset(&Ngam[0], 0, sizeof(Ngam));
  memset(&Delta_Ngam[0], 0, sizeof(Delta_Ngam));

  if (S1ST && TRGS) {
    unsigned int clock=TRGS->trgs[0].trig_or_ug; 
    unsigned int liveclock=S1ST->s1st[0].event_count;
    if (clock&&liveclock){
      prod_live_time_temp=(float)liveclock/(float)clock;
    }
  }

  if (TGS){
    fill_rtsl(TGS,rawt);
    for (i=1; i<=NUM_T_COUNTERS; i++) {
      SCALERS[i]=rawt[i-1];
    }
  } 
  else {
    fprintf(stderr," ERROR: NO scaler events found\n");
    return(0); 
  }  
  ngamma_flux_prod(runno, &SCALERS[0], prod_live_time_temp, norm_run, &Ngam[0], &Delta_Ngam[0]); 

  /*  for (i=0; i < NUM_E_BINS; i++){
    Ngam[i] = Ngam[i+1];
    Delta_Ngam[i] = Delta_Ngam[i+1];
    } */

  return(1);
}











