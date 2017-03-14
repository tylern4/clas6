/*
read_map:  read old left/right and veff info. from the map and new info from
a flatfile; adjust constants.

 */

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <scExtern.h>
#include <clas_cern.h>
#include <map_manager.h>

typedef struct{
  float dt;
  float sig_dt;
  float veff;
  float sig_veff;
}fit_t;


float par_[3];

/* ----------- Function prototypes ---------------- */
void PrintUsage(char *processName);
void ctrlCHandle(int);
int read_flatfile(char *infile,fit_t tof_values[SC_NPADDLES]);
int output_results(int runno, char *parmfile,fit_t tof_values[SC_NPADDLES]); 

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
  char *argptr, *infile = NULL, *parmfile=NULL;
  int Nevents = 0;
  int max = 0;
  char mess[100];
  int icycle, ret;
  int id = 99;
  int time_based = 0;
  int runno = 0;
  fit_t st_values[6];
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

  set_sc_version_constants(runno);
  fit_t tof_values[SC_NPADDLES_TOTAL];
  
  memset(tof_values,0,SC_NPADDLES_TOTAL*sizeof(fit_t));
  read_flatfile(&argv[argc-1][0],tof_values);
  output_results(runno,parmfile,tof_values);

}

int read_flatfile(char *infile,fit_t tof_values[SC_NPADDLES_TOTAL]){
  FILE *fp=fopen(infile,"r");
  int id,stat;
  float delta_t,veff,sig_delta_t,sig_veff,chi2ndf;

  if (fp){
    while(fscanf(fp,"%d %f %f %f %f %f %d\n",&id,&delta_t,&veff,&sig_delta_t,
		 &sig_veff,&chi2ndf,&stat)!=EOF){
      if (stat>2 && chi2ndf<100 && chi2ndf>0.0 && sig_delta_t<1.0 && veff>12 
	  && veff<19){ 
	int sector=(id&0xFF)/100;
	int scint= (id&0xFF)%100;
	int index=sc_index(sector,scint);
	tof_values[index].dt=delta_t;
	tof_values[index].sig_dt=sig_delta_t;
	tof_values[index].veff=veff;
	tof_values[index].sig_veff=sig_veff;
      }
    }
    close(fp);
  } 
} 



/* Retrieves old calibration constants from the map, adjusts these values
   with the new offsets and outputs the new offsets and the results of the
   adjustment to a parameter file */
int output_results(int runno,char *parmfile,fit_t tof_values[SC_NPADDLES_TOTAL]){
  FILE *fp=NULL;
  char st_parmfile[256], tof_parmfile[256];
  int firsttime,sec,sc_ind;
  char *dir,tof_map[128],st_map[128],tag_map[128];
  float tof_offsets[SC_NPADDLES_TOTAL],tof_offset_sigmas[SC_NPADDLES_TOTAL],tof_avg=0;
  float veff[SC_NPADDLES_TOTAL];
  float st2tof=0,tag2tof=0;

  /* Retrieve previous offset values from the map */
  dir=getenv("CLAS_PARMS");
  if(SC_VERSION_FLAG == 2)
    sprintf(tof_map,"%s/Maps/SC_CALIBRATIONS_V2.map",dir);
  else{sprintf(tof_map,"%s/Maps/SC_CALIBRATIONS.map",dir);}
  fprintf(stderr,"Reading old values from %s.\n",tof_map);
  map_get_float(tof_map,"delta_T","left_right",SC_NPADDLES,tof_offsets,
		runno,&firsttime);
  map_get_float(tof_map,"veff","left",SC_NPADDLES,veff,runno,&firsttime);
  
  /* Calculate new offsets based on the fits and the old offsets for the 
     scintillation counters */
  for(sc_ind=0;sc_ind<SC_NPADDLES;sc_ind++){
    tof_offsets[sc_ind]+=tof_values[sc_ind].dt;
    tof_avg+=tof_offsets[sc_ind]/SC_NPADDLES;
    if (tof_values[sc_ind].veff==0.0) tof_values[sc_ind].veff=veff[sc_ind];
  }  
  for (sc_ind=0;sc_ind<SC_NPADDLES;sc_ind++){
    tof_offsets[sc_ind]-=tof_avg;
  }

  if (parmfile==NULL) {
    sprintf(tof_parmfile,"lr_%d.parm",runno);
    parmfile=&tof_parmfile[0];
  }
  fprintf(stderr,"Writing fit results to %s...\n",parmfile);

  /* Write all the results to a summary file for both ST and TOF */
  fp=fopen(parmfile,"w");
  if(fp){
    fprintf(fp,"Time-of-Flight parameters:\n");
    fprintf(fp,"  Average offset before correction = %f\n",tof_avg);
    fprintf(fp,"Sec Id   Offset\n");
    for(sc_ind=0;sc_ind<SC_NPADDLES;sc_ind++){
      fprintf(fp,"%2d %3d %10.5f %10.5f %10.5f %10.5f\n",
	      sc_sector(sc_ind)+1,
	      sc_channel(sc_ind)+1,tof_values[sc_ind].dt,tof_offsets[sc_ind],
	      veff[sc_ind],tof_values[sc_ind].veff);
    }
  }
  else{
    fprintf(stderr,"Cannot open %s for writing!\n",parmfile);
    exit(1);
  }
  close(fp);

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





