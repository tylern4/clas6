/*
 * fit_offsets:   program for fitting the histograms produced by the 
 *                hist_offsets program 
 */

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <sc.h>
#include <clas_cern.h>
#include <map_manager.h>


/*------------ PAW DEFINES ----------------- */
#define MEMH 2000000
#define LREC 1024 /* record length of hbook direct access file in WORDS */
#define LUN 3 /* logical unit number of hbook file */

#define RAD2DEG (180.0/3.14159)

#define MINIMUM_ENTRIES 50 /* Minimum number of entries in a histogram for
			      performing the fit */

#define MEAN 1 /* For hstati */
#define STD_DEV 2 /* For hstati */


/* declare the hbook common */
float pawc_[MEMH];
float hcfitd_[25];
int iquest_[100];

typedef struct{
  float mean;
  float sigma;
  float err_mean;
  float err_sigma;
  float chi2;
}fit_t;


float par_[3];

/* ----------- Function prototypes ---------------- */
void PrintUsage(char *processName);
void ctrlCHandle(int);
void book_histos(int runno);
void open_hbook(char *hbookfile);
int fit_histos(fit_t st_values[6],fit_t tof_values[SC_NPADDLES]);
int output_results(int runno, char *parmfile, fit_t st_values[6],
		   fit_t tof_values[SC_NPADDLES]); 

/* --------------------------------------------------- */

void PrintUsage(char *processName)
{
  fprintf(stderr,"Usage: %s [-p<parmfile>] -r<runno> hbookfile\n\n",
	  processName);
  fprintf(stderr,"  Options:\n");
  fprintf(stderr,"\t-p[parm_file]\tParameter file name\n");
  fprintf(stderr,"\t-r[runno]\tUse this run number (required for map)\n"); 
  fprintf(stderr,"\t-h\tPrint this message.\n\n");
  exit(0);
}

main(int argc,char **argv)
{
  FILE *fp = NULL;
  int i,nmax=200;
  char *argptr, *hbookfile = NULL, *parmfile=NULL;
  int Nevents = 0;
  int max = 0;
  char mess[100];
  int icycle, ret;
  int id = 99;
  int time_based = 0;
  int runno = 0;
  fit_t st_values[6];
  fit_t tof_values[SC_NPADDLES];
  char def_parm[100];
 

  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  if (argc==1) PrintUsage(argv[0]);
  for (i=1; i<argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {
      case 'h':
	PrintUsage(argv[0]);
	break;
      case 'r':
	runno=atoi(++argptr);
	break;
      case 'p':
	parmfile = ++argptr;
	break;
      default:
	fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
	PrintUsage(argv[0]);
	break;
      }
    }
  }
  if (!runno) {
    fprintf(stderr,"You must specify a run number for use with the map.\n");
    exit(1);
  }

  hbookfile=&argv[argc-1][0];
  fprintf(stderr,"Reading %s...\n",hbookfile);

  if (parmfile==NULL) {
    sprintf(def_parm,"photon_%d.parm",runno);
    parmfile=&def_parm[0];
    fprintf(stderr,"Default file name is %s.\n",parmfile);
  }

  /* Zero the parameter arrays */
  memset(st_values,0,6*sizeof(fit_t));
  memset(tof_values,0,SC_NPADDLES*sizeof(fit_t));

  /* Open the hbook file and put the histograms into memory */
  open_hbook(hbookfile);
  
  /* Fit the time difference histograms */
  fit_histos(st_values,tof_values);

  /* Write the results to parmfile */
  output_results(runno,parmfile,st_values,tof_values);

  /* Close the hbook file cleanly */
  hrout(0,0,"T");
  hrend_("esr", 3L);
}
 



/* Open the HBOOK file for reading  the histograms */
void open_hbook(char *hbookfile)
{
  int lun = LUN, lrec = LREC, memh = MEMH, id=0,icycle=0,iofset=0;
  int istat;

  if(hbookfile == NULL) { 
    fprintf(stderr, "You must specify an hbook file as input.\n");
    exit(1);
  }
  iquest_[9] = 65000;
  hlimit_(&memh);
  hropen_(&lun,"esr",hbookfile,"U",&lrec, &istat, 3L, strlen(hbookfile), 1L);
  hrin_(&id,&icycle,&iofset);
}




/* Fit all the histograms with gaussians; store the results in the 
   st_values and tof_values arrays */
int fit_histos(fit_t st_values[6],fit_t tof_values[SC_NPADDLES]){
  int sec,scint; 
  float sigpar[3],chi2,step[3],pmin[3],pmax[3];
  extern float hx_(); /* Hbook function that returns the channel contents
			 of the bin containing a floating-point number x */
    
  /* memset(step,0,3*sizeof(float));
  memset(pmin,0,3*sizeof(float));
  memset(pmax,0,3*sizeof(float));
  memset(par_,0,3*sizeof(float));
  memset(sigpar,0,3*sizeof(float));
  chi2=0.0; */

  for (sec=1;sec<7;sec++){
    int noent=0;
    hnoent_(&sec,&noent);
    if (noent>=MINIMUM_ENTRIES){
      par_[1]=(float)hstati(sec,MEAN," ",1);
      par_[2]=(float)hstati(sec,STD_DEV," ",1);
      par_[0]=(float)hx_(&sec,&par_[1]);
      hfithn(sec,"G","QF",3,par_,step,pmin,pmax,sigpar,&chi2);
      st_values[sec-1].mean=par_[1];
      st_values[sec-1].sigma=par_[2];
      st_values[sec-1].err_mean=sigpar[1];
      st_values[sec-1].err_sigma=sigpar[2];
      st_values[sec-1].chi2=chi2;
    }
    for (scint=1;scint<49;scint++){
      int histo=100*sec+scint;
      hnoent_(&histo,&noent);
      par_[1]=(float)hstati(histo,MEAN," ",1);
      par_[2]=(float)hstati(histo,STD_DEV," ",1)/3.0;
      par_[0]=(float)hx_(&histo,&par_[1]);
      if (noent>=MINIMUM_ENTRIES){
	hfithn(100*sec+scint,"G","QF",3,par_,step,pmin,pmax,sigpar,&chi2);
	if (par_[1]>10) par_[1]=10.0;
	if (par_[1]<-10) par_[1]=-10.0;
      }
      tof_values[sc_index(sec,scint)].mean=par_[1];
      tof_values[sc_index(sec,scint)].sigma=par_[2];
      tof_values[sc_index(sec,scint)].err_mean=sigpar[1];
      tof_values[sc_index(sec,scint)].err_sigma=sigpar[2];
      tof_values[sc_index(sec,scint)].chi2=chi2;
    }
    
  }
}


/* Retrieves old calibration constants from the map, adjusts these values
   with the new offsets and outputs the new offsets and the results of the
   adjustment to a parameter file */
int output_results(int runno,char *parmfile,fit_t st_values[6],
		   fit_t tof_values[SC_NPADDLES]){
  FILE *fp=NULL,*st_fp=NULL,*tof_fp=NULL;
  char st_parmfile[256], tof_parmfile[256];
  int firsttime,sec,sc_ind;
  char *dir,tof_map[128],st_map[128];
  float tof_offsets[SC_NPADDLES],tof_offset_sigmas[SC_NPADDLES],tof_avg=0;
  float st_offsets[6],st_offset_sigmas[6],st_avg=0;
  float st2tof=0;

  /* Retrieve previous offset values from the map */
  dir=getenv("CLAS_PARMS");
  sprintf(tof_map,"%s/Maps/SC_CALIBRATIONS.map",dir);
  fprintf(stderr,"Reading old values from %s.\n",tof_map);
  /*dir=getenv("TOP_DIR");*/ /* For debugging */
  sprintf(st_map,"%s/Maps/ST_CALIB.map",dir);
  fprintf(stderr,"Reading old values from %s.\n",st_map);
  map_get_float(tof_map,"delta_T","paddle2paddle",SC_NPADDLES,tof_offsets,
		runno,&firsttime);
  map_get_float(st_map,"delta_T","value",6,st_offsets,runno,
		&firsttime);
  map_get_float(st_map,"st2tof","value",1,&st2tof,runno,&firsttime);

  /* Calculate new offsets based on the fits and the old offsets for the 
     start counter */
  for(sec=0;sec<6;sec++){
    st_offsets[sec]-=st_values[sec].mean;
    st_avg+=st_offsets[sec]/6.0;
    st_offset_sigmas[sec]=st_values[sec].err_mean;
  }  
  for (sec=0;sec<6;sec++){
    st_offsets[sec]-=st_avg;
  }
  
  /* Calculate new offsets based on the fits and the old offsets for the 
     scintillation counters */
  for(sc_ind=0;sc_ind<SC_NPADDLES;sc_ind++){
    tof_offsets[sc_ind]-=tof_values[sc_ind].mean;
    tof_avg+=tof_offsets[sc_ind]/SC_NPADDLES;
    tof_offset_sigmas[sc_ind]=tof_values[sc_ind].err_mean;
  }  
  for (sc_ind=0;sc_ind<SC_NPADDLES;sc_ind++){
    tof_offsets[sc_ind]-=tof_avg;
  }


  fprintf(stderr,"Writing fit results to %s...\n",parmfile);

  /* Write all the results to a summary file for both ST and TOF */
  fp=fopen(parmfile,"w");
  if(fp){
    fprintf(fp,"Start Counter parameters:\n");
    fprintf(fp,"  Average offset before correction = %f\n",st_avg);
    fprintf(fp,"  New st2tof = %f\n",st2tof+st_avg);
    fprintf(fp,"Sec    Mean       dMean      Sigma      dSigma     Offset    dOffset     Chi2\n");
    for(sec=0;sec<6;sec++){
      fprintf(fp,"%2d %10.5f %10.5f %10.5f %10.5f %10.5f %10.5f %10.5f\n",
              sec+1,st_values[sec].mean,
	      st_values[sec].err_mean,st_values[sec].sigma,
	      st_values[sec].err_sigma, st_offsets[sec],
	      st_offset_sigmas[sec], st_values[sec].chi2);
    }
    fprintf(fp,"Time-of-Flight parameters:\n");
    fprintf(fp,"  Average offset before correction = %f\n",tof_avg);
    fprintf(fp,"Sec Id     Mean       dMean      Sigma      dSigma     Offset    dOffset     Chi2\n");
    for(sc_ind=0;sc_ind<SC_NPADDLES;sc_ind++){
      fprintf(fp,"%2d %3d %10.5f %10.5f %10.5f %10.5f %10.5f %10.5f %10.5f\n",
	      sc_sector(sc_ind)+1,
	      sc_channel(sc_ind)+1,tof_values[sc_ind].mean,
	      tof_values[sc_ind].err_mean,tof_values[sc_ind].sigma,
	      tof_values[sc_ind].err_sigma, tof_offsets[sc_ind],
	      tof_offset_sigmas[sc_ind], tof_values[sc_ind].chi2);
    }
  }
  else{
    fprintf(stderr,"Cannot open %s for writing!\n",parmfile);
    exit(1);
  }
  close(fp);

  /* Write the new offsets and the errors on the offsets for the Start counter
     to a file with a .start extension */ 
  sprintf(st_parmfile,"%s.start",parmfile);
  st_fp=fopen(st_parmfile,"w");
  if (st_fp){
    for(sec=0;sec<6;sec++){
      fprintf(st_fp,"%f %f\n",st_offsets[sec],st_offset_sigmas[sec]);
    }
  }
  else{
    fprintf(stderr,"Cannot open %s for writing!\n",st_parmfile);
  }
  close(st_fp);
  
  /* Write the new offsets and the errors on the offsets for the scintillators 
     to a file with a .tof extension */ 
  sprintf(tof_parmfile,"%s.tof",parmfile);
  st_fp=fopen(tof_parmfile,"w");
  if (st_fp){
    for(sc_ind=0;sc_ind<SC_NPADDLES;sc_ind++){
      fprintf(st_fp,"%f %f\n",tof_offsets[sc_ind],tof_offset_sigmas[sc_ind]);
    }
  }
  else{
    fprintf(stderr,"Cannot open %s for writing!\n",tof_parmfile);
  }
  close(tof_fp);
}





void ctrlCHandle(int x)
{
  int icycle;

  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  fprintf(stderr,"\n\n\t\t\t***  INTERRUPTED!!!  ***\n\n");

  /* Close the hbook file cleanly */
  hrend_("esr", 3L);

  exit(1);
}

/* end file */





