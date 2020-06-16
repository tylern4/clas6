/*--------------------- sc_init.c -------------------------- */
/*  This file contains a set of routines intended to retrieve calibration
    constants for the Time-of-Flight analysis code from the map and store
    them in global memory. 
    ROUTINES:
	sc_init:   this is the main routine for retrieving and storing 
		   constants from $CLAS_PARMS/Maps/SC_CALIBRATIONS.map.
	           The argument 'flag' enables the disabling of certain 
		   constants for calibration purposes.
	initialize_tof_:  creates the SCG and SCP banks and calls sc_init.
		   (Fortran-callable)
	initialize_tof:   c-wrapper for previous routine.
	fill_sc_calib:  gets the left and right constants for the requested 
		   subsystem.	

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
#include <scGlobal.h>

/* -=======================================================- *
$Id: sc_init.c,v 1.16 2003/10/13 21:41:48 holtrop Exp $
$Author: holtrop $
$Revision: 1.16 $
$Date: 2003/10/13 21:41:48 $
* -=======================================================- */
#ifndef USE
#define USE(var) static void * use_##var = (void *) &var
#endif

static char sc_init_crcsid[] =
    "$Id: sc_init.c,v 1.16 2003/10/13 21:41:48 holtrop Exp $";
USE(sc_init_crcsid);   /* make sure it is not optimized away */

/* Initialisation routine for TOF: fills calibration structures */
int sc_init(int runno,int flag){
  char *dir,map[100];
  float lr0[SC_NPADDLES],c2c[SC_NPADDLES];
  float p2p[SC_NPANELS];
  int panel;
  int i,firsttime,j;
  
  dir = getenv("CLAS_PARMS");
  sprintf(map,"%s/Maps/SC_CALIBRATIONS.map",dir);  
  /*  dir = getenv("TOP_DIR");
  sprintf(map,"%s/sc/SC_CALIBRATIONS.map",dir); */ 
   fprintf(stderr,"Reading calibration data from database or %s\n",map); 
  
  fill_sc_calib(map, "T0_TDC", &sc_t0, runno);
  fill_sc_calib(map, "T1", &sc_t1, runno);
  fill_sc_calib(map, "T2", &sc_t2, runno);
  fill_sc_calib(map, "T2u", &sc_t2u, runno);
  fill_sc_calib(map, "pedestals", &sc_pedestals, runno);
  fill_sc_calib(map, "pedu",&sc_pedu, runno);
  fill_sc_calib(map, "NMIP_ADC", &sc_m0, runno);
  fill_sc_calib(map, "NMIP_ADCu",&sc_m0u, runno);
  fill_sc_calib(map, "WALK_A0", &sc_walk0, runno);
  fill_sc_calib(map, "WALK1", &sc_walk1, runno);
  fill_sc_calib(map, "WALK2", &sc_walk2, runno);
  fill_sc_calib(map, "veff", &sc_veff, runno);
  fill_sc_calib(map, "veffu",&sc_veffu, runno);
  fill_sc_calib(map, "atten_length", &sc_lambda, runno);
  fill_sc_calib(map, "atten_u", &sc_lambdau,runno);
  map_get_float(map,"delta_T","left_right",SC_NPADDLES,lr0,runno,&firsttime);
  map_get_float(map,"pulser","normalisation",1,&pulse_norm,runno,&firsttime);
  map_get_float(map,"time_walk","ref_adc",1,&adc_ref,runno,&firsttime);
  map_get_float(map,"Yoffset","value",SC_NPADDLES,&yoffset,runno,&firsttime);
  map_get_float(map,"delta_T","coupled_paddle1",54,&paddle1,runno,&firsttime);
  map_get_float(map,"delta_T","coupled_paddle2",54,&paddle2,runno,&firsttime);
  sc_status_setup(runno);
  sc_swapindex_setup(runno);

  /* Turn off usage of certain constants for calibration purposes */
  switch(flag){
  case LR_CALIB:
    fprintf(stderr,"Initializing ToF package for Left-right time calibration, run %d.\n",runno);
    break;
  case TW_CALIB:
    for (i=0;i<SC_NPADDLES;i++){
      sc_walk0.left[i]=sc_walk0.right[i]=0.0;
      sc_walk1.left[i]=sc_walk1.right[i]=0.0;
      sc_walk2.left[i]=sc_walk2.right[i]=0.0;
      sc_m0.left[i]=sc_m0.right[i]=DEDX_NMIP*5.08; /* need to fix! */
    } 
    fprintf(stderr,"Initializing ToF package for Time-walk calibration, run %d.\n",runno);
    break;
  case ADC_CALIB:
    for (i=0;i<SC_NPADDLES;i++) sc_m0.left[i]=sc_m0.right[i]=1.0;
    fprintf(stderr,"Initializing ToF package for ADC calibration, run %d.\n",runno);
    break;
  case ALL_CALIB:
    /* Left-right alignment */
    for(i=0;i<SC_NPADDLES;i++){
     sc_t0.left[i]=sc_t0.left[i]-lr0[i]/2.0;
     sc_t0.right[i]=sc_t0.right[i]+lr0[i]/2.0;
    }
    /* Counter-to-counter constants */
    map_get_float(map,"delta_T","paddle2paddle",SC_NPADDLES,c2c,runno,
		  &firsttime);
    for(i=0;i<SC_NPADDLES;i++){
      sc_t0.left[i]=sc_t0.left[i]+c2c[i];
      sc_t0.right[i]=sc_t0.right[i]+c2c[i];
    }
    /* Panel-to-panel constants */
    map_get_float(map,"delta_T","panel2panel",SC_NPANELS,p2p,runno,&firsttime);
    for (i=0;i<SC_NPADDLES;i++){
      panel=get_panel(sc_channel(i));      
      sc_t0.left[i]=sc_t0.left[i]+p2p[6*(panel-1)+sc_sector(i)];
      sc_t0.right[i]=sc_t0.right[i]+p2p[6*(panel-1)+sc_sector(i)];   
    }
    fprintf(stderr,"ToF package initialized for run %d.\n",runno);
    break;
  }
}

int initialize_tof_gc_(int *GrunNo, int *CrunNo){
  /*
   * Geometry and timing calibration run-numbers can be different.
   *  This is so that MC data can have known (simple) timing constants
   *  from run 1, but the geometry can change with the run-number.
   */
  int sec;
  clasSCP_t *SCP;

  dropAllBanks(&wcs_,"SCG SCP "); /* just in case if there is a leftover */
  
  make_SCG_banks(*GrunNo);
  sc_init(*CrunNo,ALL_CALIB);
  for(sec=1; sec <= 6; sec++){
    make_SCP_bank(sec);
  }
}

int initialize_tof_gc(int GrunNo, int CrunNo)
{
  /* geometry and timing calibration run-numbers can be different */
  int gr = GrunNo;
  int cr = CrunNo;
  initialize_tof_gc_(&gr,&cr);
}

int sc_begin_run(int runno){
  clasHEAD_t *HEAD = getBank(&bcs_, "HEAD");

  if (HEAD){
    if (HEAD->head[0].type >= 0) {
      /* data */
      initialize_tof_gc(runno,runno);
    } else {
      /*monte carlo - do something else */
      initialize_tof_gc(runno,1);
    }
  }
}

int sc_begin_run_(int *runno){
  sc_begin_run(*runno);
}

/* OLD VERSIONS of initialize_tof */  
/*initialize the whole tof package from C*/
int initialize_tof(int runNo)
{
  int r = runNo;
  initialize_tof_(&r);
}

int initialize_tof_(int *runNo){
  int sec;
  clasSCP_t *SCP;

  dropAllBanks(&wcs_,"SCG SCP "); /* just in case if there is a leftover */

  make_SCG_banks(*runNo);
  sc_init(*runNo,ALL_CALIB);
  for(sec=1; sec <= 6; sec++){
    make_SCP_bank(sec);
  }
}
/* END old versions */


/* Retrieve calibration constants from the SC_CALIBRATIONS map */
int fill_sc_calib(char *map, char *subsystem, sc_const_t *sc_cal, int runno){
  int firsttime, i;

  map_get_float(map,subsystem,"left",SC_NPADDLES,sc_cal->left,runno,&firsttime);
  map_get_float(map,subsystem,"right",SC_NPADDLES,sc_cal->right,runno,&firsttime);
}


void sc_status_setup(int runno) {
  /* sets up the sc_status structures */
  /* don't have to call sc_init to set up */

  char *dir, map[100];
  int firsttime;

  dir = getenv("CLAS_PARMS");
  sprintf(map,"%s/Maps/SC_CALIBRATIONS.map",dir);

  map_get_int(map,"status","left",SC_NPADDLES,sc_status.left,runno,&firsttime);
  map_get_int(map,"status","right",SC_NPADDLES,sc_status.right,runno,&firsttime);

  return;
}


void sc_swapindex_setup(int runno) {
  /* sets up the sc_status structures */
  /* don't have to call sc_init to set up */

  char *dir, map[100];
  int firsttime, i, ierrl =0, ierrr =0;

  dir = getenv("CLAS_PARMS");
  sprintf(map,"%s/Maps/SC_CALIBRATIONS.map",dir);

  if (ierrl = map_get_int(map,"swap_TDCi","left",SC_NPADDLES,sc_swapindex.left,runno,&firsttime)) {
    for (i = 0; i < SC_NPADDLES; i++) sc_swapindex.left [i] = i;
  }
  if (ierrr = map_get_int(map,"swap_TDCi","right",SC_NPADDLES,sc_swapindex.right,runno,&firsttime)) {
    for (i = 0; i < SC_NPADDLES; i++) sc_swapindex.right [i] = i;
  }
  
  if (ierrl || ierrr) {
    fprintf (stderr, "Error (L:%d,R:%d) reading SubSystem swap_TDC, using default values\n", 
	     ierrl, ierrr);
  } 
  return;
}


