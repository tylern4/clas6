#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ntypes.h>
#include <bostypes.h>
#include <map_manager.h>
#include <call.h>
#include <math.h>
#include <scExtern.h>

#ifndef ABS
#define ABS(x)   ((x) < 0 ? -(x) : (x))
#endif

#define FACTOR 10000

/* RF signal is prescaled in discrimator-prescaler board */
#define PRESCALED_CYCLES 80

/* correct RF on event basis:
 use running mean of last 'evts_rfcorr' time corrections (Tagtime-RF)%RFcycle), 
 removing earlier entries (i.e. keep only last 'evts_rfcorr').
 Turn on by setting:   float evts_rfcorr=20000.; (or similar #)
   or by setting RF_OFFSETS.F1.p3=20000. (caldb,map).
 Range of detector counters taken into account for calculation of running mean 
   is given by 'detid_rfcorr_min' and 'detid_rfcorr_max'  (e.g. Tag-T: detid=1...121)
   or by RF_OFFSETS.F1.low (min.detid) and RF_OFFSETS.F1.high (max.detid) (caldb,map).
 Note that for these counters only entries in range 'det_time_min'<time<'det_time_max'
   are taken to determine the running mean; this can be set in caldb,map by
   RF_OFFSETS.F1.p2=111. and RF_OFFSETS.F1.p0 (det_time_min), .F1.p1 (det_time_max).
 Turn off the running mean correction by setting:  float evts_rfcorr=1.;
*/
float evts_rfcorr=0.0;
float det_time_min=0.;
float det_time_max=1000.;
int detid_rfcorr_min =0;
int detid_rfcorr_max =1000;

call_const_t call_calib;
float rf_offset = 0;
int i, status = 1;
int warning_msg_count = 3;  /* write warning message up to 3 times */
int no_cl01_count = 0;
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

int make_cl01_bank_using(int use_rf) { /* select raw RF: 1=CALL6 2=CALL7 3=RFT6 4=RFT7 */
  int save_status = status;
  status = use_rf;
  make_CL01_bank();
  status = save_status;
}

int make_CL01_bank(){
  /*input: Call bank, calibration constants (must run initCL01)
    output: CL01 bank */
  clasCALL_t *CALL = NULL;
  clasCL01_t *CL01 = NULL;
  clasRFT_t  *RFT  = NULL;
  clasRFT_t  *LSRT = NULL;  // same structure as RFT
  clasHEAD_t *HEAD = NULL;
  cl01_t cl01;
  int n_region = 0, rf_region = 0;
  float pol4 = 0;

  cl01.ac_amp = cl01.fc_diode_amp = cl01.nc_diode_amp = 
    cl01.sc_diode_amp = cl01.sf_diode_amp = NO_CL01_HIT;
  cl01.fc_diode_t = cl01.nc_diode_t = cl01.sc_diode_t = 
    cl01.sf_diode_t = cl01.rf1 = cl01.rf2 = NO_CL01_HIT;

  float rf_selct[4] = { NO_CL01_HIT, NO_CL01_HIT, NO_CL01_HIT, NO_CL01_HIT}; 
  float rf_first[4] = { NO_CL01_HIT, NO_CL01_HIT, NO_CL01_HIT, NO_CL01_HIT}; 
  int   rf_found[4] = { 0, 0, 0, 0};

  if ( RFT = getBank(&bcs_, "RFT ")) {  //channel 96 and 99 of tdc 18 in ROC crate 22
    /* procedure:
        RFT contains typically 6-8 entries per RF tdc,
        only those entries are used that have the expected tdc difference 
        (taking the tdc difference with most entries);
       if that doesn't work, take entries with roughly same offset within the RF cycle.
    */
#define MAX_RFT_ENTRIES 16
    int rft_entry[2][MAX_RFT_ENTRIES];   // save all entries from RFT bank
    int nrft_entries[2]={0,0};
    int i, j;
    int raw_diff[2];         // tdc difference from slope (80*RF_STRUCTURE)
    raw_diff[0] = (int)(-PRESCALED_CYCLES*RF_STRUCTURE/call_calib.t1[8]);
    raw_diff[1] = (int)(-PRESCALED_CYCLES*RF_STRUCTURE/call_calib.t1[9]);

    for (i=0; i < RFT->bank.nrow; i++) {
      switch (RFT->rft[i].id) {
      case RF1:  /* RFT raw bank uses same id=6,7 coding as CALL */
      case RF2:
	j = RFT->rft[i].id - 6;
	if ( nrft_entries[j] < MAX_RFT_ENTRIES )
	  rft_entry[j][nrft_entries[j]++]=RFT->rft[i].tdc;
      }
    }

    /* histogram the tdc hits (in bins +/-10 tdc counts around expected RF difference)
       use first tdc hit in bin with most entries,
       (in case of signal reflections try the same for half the expected RF diff.),
       if there aren't enough entries then calculate the RF offset (within the RF cycle)
    */
    for( j=0; j<2; j++) {

      int ir, ic, i2, k=-1;
      int nloops, nfound, idiff, idiff1;
      float t_ns, tdiff, taver, offset;
      float rft_time[MAX_RFT_ENTRIES];
      int nrft_diff[20];     // distribution of entries around expected tdc difference
      int rft_diff[20];      // first entry in the corresponding bin
      int imax, nmax, imx[5], nmxsum;  //mean and #entries around mean in distribution
      int ival0, ival1;                // previous entry in tdc and entry before previous
      ir = j+2;    // RFT->rft[i].id - 4;    /* store result     -> 2,3  */
      ic = j+9;    // RFT->rft[i].id + 3;    /* calib. constants -> 9,10 */ 

      // first try to find max. around expected RF tdc difference, then for half of it
      while( ++k < 2 ) { 
	memset(nrft_diff,0,20*sizeof(int));
	ival0 = ival1 = 0;
	for( i=0; i<nrft_entries[j]; i++ ) {
	  if( ival0 ) {
	    idiff = ABS(ival0 - rft_entry[j][i]) - raw_diff[j] +8;
	    if(k) idiff += raw_diff[j]/2 -4;
	    if (idiff >=0 && idiff <20) {
	      if(!nrft_diff[idiff]) rft_diff[idiff]=ival0;
	      nrft_diff[idiff]++;
	    }
	    if( ival1 ) {
	      idiff = ABS(ival1 - rft_entry[j][i]) - raw_diff[j] +8;
	      if(k) idiff += raw_diff[j]/2 -4;
	      if (idiff >=0 && idiff <20) {
		if(!nrft_diff[idiff]) rft_diff[idiff]=ival1;
		nrft_diff[idiff]++;
	      }
	    }
	    ival1 = ival0; 
	  }
	  ival0 = rft_entry[j][i];
	}

	imax = nmax = 0;
	for( i=0; i<20; i++ ) {
	  if( nrft_diff[i] > nmax ) {
	    nmax = nrft_diff[i];
	    imax = i;
	  }
	}
	// printf(" for cl %d (%d) n=%d\n",j,k,nrft_entries[j]);
	// requirements: mean not in first or last bin of distribution,
	//               more than 3 entries for mean +/-1 bin
	if( imax>1 && imax<17 ) {
	  for( i2=0; i2<5; i2++ ) {
	    imx[i2] = i2<2? imax+i2-2 : imax+i2-1;
	  }
	  nmxsum = nmax + nrft_diff[imx[1]] + nrft_diff[imx[2]]; 
	  if( nmax == nrft_diff[imx[2]] || nmax == nrft_diff[imx[3]] ) {
	    nmxsum += nrft_diff[imx[3]];
	    if( nmxsum < 4 ) 
	      nmxsum += nrft_diff[imx[0]] + nrft_diff[imx[4]]; 
	    else 
	      imx[0] = imx[4] = 0;
	  }
	  else {
	    if( nmxsum < 4 ) 
	      nmxsum += nrft_diff[imx[0]] + nrft_diff[imx[3]] + nrft_diff[imx[4]]; 
	    else
	      imx[0] = imx[3] = imx[4] = 0;
	  }
	  if( nmxsum > 3 ) {
	    rf_found[ir] = nmxsum;
	    t_ns = getCL01time(ic, rft_diff[imax]);
	    rf_first[ir] = t_ns;
	    rf_selct[ir] = nmax * t_ns;
	    for( i2=0; i2<5; i2++ ) {
	      if( imx[i2] && nrft_diff[imx[i2]] ) {
		t_ns = getCL01time(ic, rft_diff[imx[i2]]);
		rf_selct[ir] += nrft_diff[imx[i2]] * time2rfregion(t_ns,rf_first[ir]);
	      }
	    }
	    rf_selct[ir] /= rf_found[ir];
	    k = 2;   // success: end the loop over k
	  }
	}
      }
      // if there were not enough entries to get the mean from tdc counts,
      //  use mean of all RF times that have roughly the same RF offset within a RF cycle
      // fk 5/08: skip this part: it causes close to random times for noisy RF in g9a
      /*      if( !rf_found[ir] ) {
	if( nrft_entries[j] == 1 ) {
	  rf_found[ir] = 1;
	  rf_selct[ir] = getCL01time(ic,rft_entry[j][0]);
	}
	else if( nrft_entries[j] > 1 ){
	  for( i=0; i<nrft_entries[j]; i++ ) 
	    rft_time[i] = getCL01time(ic,rft_entry[j][i]);

	  nloops = nfound = 0;
	  taver  = offset = 0.0;
	  while( ++nloops<5 ) {
	    for( i=0; i<nrft_entries[j]-1; i++){
	      for(i2=i+1; i2<nrft_entries[j]; i2++){
		tdiff = rf_correction_center(rft_time[i2],rft_time[i]);
		if( fabs(tdiff) < 0.1*nloops ) {
		  taver += tdiff;
		  if(!nfound) offset=rft_time[i];
		  nfound++;
		}
	      }
	    }
	    if( nfound>4 ) nloops=6;
	  }
	  if( nfound ) {
	    rf_found[ir] = nfound;
	    rf_selct[ir] = taver/rf_found[ir] + offset;
	  }
	  else {
	    rf_found[ir] = 1;
	    rf_selct[ir] = rft_time[0];
	  }
	}
	} */
    }
  }

  int i, j, locTDC;
  CALL = getBank(&bcs_, "CALL");
  LSRT = getBank(&bcs_, "LSRT");

  if(LSRT){
    for(j = 2; j < 6; j++){ //sometimes more than one hit in lsrt for a given pmt in a given event, choose first sensical one
      locTDC = -1;
      for(i = 0; i < LSRT->bank.nrow; i++){
        if((LSRT->rft[i].tdc < 8192) && ((int)LSRT->rft[i].id == j)){
          locTDC = LSRT->rft[i].tdc;
          break;
        }
      }
      if(locTDC == -1)
        continue; //laser not in use for event, continue to next one
      switch(j){
        case FWD_DIODE:
          cl01.fc_diode_t = getCL01time(FWD_DIODE, locTDC);
          break;
        case N_CLMSH_DIODE:
          cl01.nc_diode_t = getCL01time(N_CLMSH_DIODE, locTDC);
          break;
        case S_CLMSH_DIODE:
          cl01.sc_diode_t = getCL01time(S_CLMSH_DIODE, locTDC);
          break;
        case SPCFM_DIODE:
          cl01.sf_diode_t = getCL01time(SPCFM_DIODE, locTDC);
          break;
      }
    }
  }

  if(CALL){
    for(i=0; i < CALL->bank.nrow; i++){
      locTDC = CALL->call[i].tdc;
      switch ((int)CALL->call[i].id){
      case AC60HZ:
	cl01.ac_amp = getCL01adc(AC60HZ, CALL->call[i].adc);
	break;
      case FWD_DIODE:
	cl01.fc_diode_amp = getCL01adc(FWD_DIODE, CALL->call[i].adc);
	if(!LSRT && (locTDC > 0)) cl01.fc_diode_t = getCL01time(FWD_DIODE, locTDC);
	break;
      case N_CLMSH_DIODE:
	cl01.nc_diode_amp = getCL01adc(N_CLMSH_DIODE, CALL->call[i].adc);
	if(!LSRT && (locTDC > 0)) cl01.nc_diode_t = getCL01time(N_CLMSH_DIODE, locTDC);
	break;
      case S_CLMSH_DIODE:
	cl01.sc_diode_amp = getCL01adc(S_CLMSH_DIODE, CALL->call[i].adc);
	if(!LSRT && (locTDC > 0)) cl01.sc_diode_t = getCL01time(S_CLMSH_DIODE, locTDC);
	break;
      case SPCFM_DIODE:
	cl01.sf_diode_amp = getCL01adc(SPCFM_DIODE, CALL->call[i].adc);
	if(!LSRT && (locTDC > 0)) cl01.sf_diode_t = getCL01time(SPCFM_DIODE, locTDC);
	break;
      case RF1:
	rf_selct[0] = cl01.rf1 = getCL01time(RF1, CALL->call[i].tdc);
	rf_found[0]++;
	break;
      case RF2:
	rf_selct[1] = cl01.rf2 = getCL01time(RF2, CALL->call[i].tdc);
	rf_found[1]++;
	break;
      }
    }
  }


  /* good RF */

  cl01.rf = 0.0; /* initialization */      

  if (status < 1 || status > 4) {
    if (warning_msg_count < 3) {  
      fprintf (stderr, 
	       "WARNING: RF status is set to %d (valid choices 1..4), no RF correction done\n",
	       status);
      warning_msg_count++;
    }
  }
  else { /* status OK */
    int id=status-1;
      
      /* 4th deg. polynomial correction for CALL TDC */
    if (status <= 2) {
      if (rf_found[id]) { /* raw RF (1,2) found ? */
	
	for(i=0; i < 4; i++) {  /* find region */
	  if (rf_selct[id] >= f[i].low && rf_selct[id] < f[i].high) {
	    rf_region = i;
	    n_region++;
	  }
	}
	
	switch (n_region) {
	case 0:  /* outside of any region*/ 
	  break;
	case 1:
	  if (rf_region < 0 || rf_region >= 4) { /* impossible unless code changed */
	    fprintf (stderr, "makeCL01 programming error\n"); exit(-1);
	  }
	  pol4 =  f[rf_region].p0
	    +     f[rf_region].p1 * rf_selct[id]
	    +     f[rf_region].p2 * rf_selct[id] * rf_selct[id]
	    +     f[rf_region].p3 * rf_selct[id] * rf_selct[id] * rf_selct[id]; 
	  rf_selct[id] += rf_offset + pol4;
	  break;
	default: /* RF value found in more than one region */
	  fprintf(stderr,
		  "WARNING: no RF%d correction at %f2 ns. Overlapping calibration regions: %d \n",
		  status, rf_selct[id], n_region); 
	}
	cl01.rf = rf_selct[id];
      }
    }

    /* no correction for RFT TDC but switch RF tdc if the other one is too noisy */
    else {
      if( rf_found[id] )    cl01.rf1 = rf_selct[id] + rf_offset;
      if( rf_found[5-id] )  cl01.rf2 = rf_selct[5-id] + rf_offset;
      // switch RF tdc if <status> tdc too noisy
      if( rf_first[id]!=NO_CL01_HIT )
	cl01.rf = cl01.rf1;
      else if( rf_first[5-id]!=NO_CL01_HIT ) 
	cl01.rf = cl01.rf2;
    }
  }

  if ( CALL || cl01.rf != 0.0 ) {
    if (CL01 = makeBank(&bcs_, "CL01", 0, sizeof(cl01_t)/4, 1)){
      CL01->cl01[0] = cl01;
    } 
  }
  //  else if ( !(++no_cl01_count%5000) ) {
  //    HEAD=getBank(&bcs_, "HEAD");
  //    fprintf(stderr,"event %d: no CL01 found (%d times)\n",HEAD->head[0].nevent,no_cl01_count);
  //  }
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
  char *dir,map[128], rfmap[128], subname[4];
  int  i,firsttime;

  dir=getenv("CLAS_PARMS");
  sprintf(map,"%s/Maps/CALL_CALIB.map",dir);
  sprintf(rfmap, "%s/Maps/RF_OFFSETS.map", dir);
  map_get_float(map,"T0","value",CALL_IDS,call_calib.t0,runno,&firsttime);    
  map_get_float(map,"T1","value",CALL_IDS,call_calib.t1,runno,&firsttime); 
  map_get_float(map,"T2","value",CALL_IDS,call_calib.t2,runno,&firsttime); 
  map_get_float(map,"pedestals","value",CALL_IDS,call_calib.ped,runno,&firsttime); 
  map_get_float(rfmap, "offset", "value", 1, &rf_offset, runno, &firsttime);
  map_get_int(rfmap, "status", "value", 1, &status, runno, &firsttime);
  for(i=0; i < 4; i++){
    sprintf(subname,"F%d",i+1);
    map_get_float(rfmap, subname, "low", 1, &f[i].low, runno,&firsttime);
    map_get_float(rfmap, subname, "high",1, &f[i].high,runno,&firsttime);
    map_get_float(rfmap, subname, "p0",  1, &f[i].p0,  runno,&firsttime);
    map_get_float(rfmap, subname, "p1",  1, &f[i].p1,  runno,&firsttime);
    map_get_float(rfmap, subname, "p2",  1, &f[i].p2,  runno,&firsttime);
    map_get_float(rfmap, subname, "p3",  1, &f[i].p3,  runno,&firsttime);
  }
  fprintf(stderr,"Reading CALL calibration constants for run %d.\n",runno);

  if (evts_rfcorr==0.0) {
    evts_rfcorr =1.0;  /* running mean RF correction turned off */
    if( status>2 && f[0].p3>999.) {
      evts_rfcorr = f[0].p3;   /* number of entries kept for running mean */
      if( f[0].low<=f[0].high &&
	floor(f[0].low)==f[0].low && floor(f[0].high)==f[0].high ) {
	detid_rfcorr_min = f[0].low;  /* detector counter range */
	detid_rfcorr_max = f[0].high;
	if( f[0].p2==111.) {
	  det_time_min = f[0].p0;
	  det_time_max = f[0].p1;
	}
      }
      fprintf(stderr,"RF adjustment using running mean from %d calls for detector ids %d to %d in time range %6.1f to %6.1f\n",
	      (int)evts_rfcorr,detid_rfcorr_min,detid_rfcorr_max,det_time_min,det_time_max);
    }
  }
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

float rf_corr_time_center(float time){
  clasCL01_t *CL01 = getBank(&bcs_, "CL01");
  float rf_st_time = time;

  if (CL01){ 
    rf_st_time = time - rf_correction_center(time, CL01->cl01[0].rf);
  } 
  return rf_st_time;
}

float rf_correction(float time, float rf){
  float del_time = 0;
  
  if (rf <= 0.) return 0.;

  del_time = time - rf + FACTOR*RF_STRUCTURE;
  return (fmod(del_time,RF_STRUCTURE) - 0.5 * RF_STRUCTURE);
}

float rf_correction_center(float time, float rf){
  float  del_time;

  if (rf <= 0.) return 0.;

  del_time = time - rf + (FACTOR+0.5)*RF_STRUCTURE;
  return (fmod(del_time,RF_STRUCTURE) - 0.5 * RF_STRUCTURE);
}

float rf_corr_time_counter_(int *counter, float *time){
  if (evts_rfcorr<1000)              /* no RF correction using running mean) */
    return (rf_corr_time(*time));
  else
    return (rf_corr_time_counter(*counter,*time));
}

float rf_corr_time_counter(int det_index, float time){
  /* keep large number ('evts_rfcorr') of rf_time_differences in rfarray 
     in oder to allow for calculation of running mean of latest entries (fifo) 
       (only for detectors with ids in a range given in caldb (or map) for 
        RF_OFFSETS.F1.low to RF_OFFSETS.F1.high; e.g. for TAGT: bin_id 1...121 )
*/ 
  static float eventcnt= 0.0;
  static float diffsq= 0.0;
  static float sum   = 0.0;
  static float mean  = 0.0;
  static float *rfarray = NULL;
  int i, eventptr;
  float del_time;
  clasCL01_t *CL01 = getBank(&bcs_, "CL01");
  clasHEAD_t *HEAD = NULL;
  float rf_st_time = time;

  if (!CL01) return rf_st_time;
  del_time = rf_correction(time,CL01->cl01[0].rf);

  if (evts_rfcorr<1000) return (rf_st_time - del_time);

  if (det_index<detid_rfcorr_min || det_index>detid_rfcorr_max || 
      time<det_time_min || time>det_time_max) 
    return (rf_st_time - del_time + mean);

  if (!rfarray) {
    rfarray = (float*)malloc((int)(evts_rfcorr)*sizeof(float));
    if (!rfarray) return (rf_st_time - del_time);
  }
  eventptr = (int)fmod(eventcnt,evts_rfcorr);
  eventcnt+=1.0;

  sum += del_time;
  if (eventcnt <= evts_rfcorr) {
    rfarray[eventptr] = del_time;
    if (eventcnt > evts_rfcorr*0.8) mean = sum/eventcnt;
  } 
  else {
    /*    if (eventcnt==evt_rfcorr+1) {
      for (i=0; i<(int)evt_rfcorr; i++) 
	diffsq += (rfarray[i]-mean)*(rfarray[i]-mean);
	} */
    sum -= rfarray[eventptr];
    diffsq -= (rfarray[eventptr]-mean)*(rfarray[eventptr]-mean);
    mean = sum/evts_rfcorr;
    diffsq += (del_time-mean)*(del_time-mean);
    rfarray[eventptr] = del_time;
  }
  diffsq += (del_time-mean)*(del_time-mean);

  if( !((int)eventcnt)%50500 ) {
    HEAD=getBank(&bcs_, "HEAD");
    fprintf(stderr,"RF running mean correction (event %d): mean difference %f +/- %f\n",
	    HEAD->head[0].nevent,mean,sqrt(diffsq/(evts_rfcorr-1.)));
  }
  return (rf_st_time - del_time + mean);
}


float time2rfregion(float time, float rf) {
  return rf + rf_correction_center(time, rf);
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
  if (id < (SC_VERSION_FLAG == 1 ? 43:46))
    return (((sec >= 3) && (sec < 5)) ? N_CLMSH_DIODE : S_CLMSH_DIODE);
  if (id<SC_NPADDLES_SEC) return SPCFM_DIODE;
  return -1;
}

float get_diode_time(cl01_t cl01,int id,int sec){
  if (id<24) return cl01.fc_diode_t;
  if (id < (SC_VERSION_FLAG == 1 ? 43:46))
    return (((sec >= 3) && (sec < 5)) ? cl01.nc_diode_t : cl01.sc_diode_t);
  if (id < SC_NPADDLES_SEC) return cl01.sf_diode_t;
  return 0.0;
}
