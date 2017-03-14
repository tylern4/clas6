/*
 * hist_offsets:   program for histogramming counter-to-counter offsets for 
 * 		  photon-beam runs using the tagger (=RF) time as reference  
 */

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <clas_cern.h>
#include <kinematics.h>
#include <map_manager.h>
#include <sc.h>
#include <call.h>
#include <ec.h>
#include <pid.h>
#include <utility.h>
#include <PartUtil.h>
#include <particleType.h>

/*------------ PAW DEFINES ----------------- */
#define MEMH 2000000
#define LREC 1024 /* record length of hbook direct access file in WORDS */
#define LUN 3 /* logical unit number of hbook file */

#define RAD2DEG (180.0/3.14159)

#define MINIMUM_ENTRIES 50 /* Minimum number of entries in a histogram for
			      performing the fit */

#define MEAN 1 /* For hstati */
#define STD_DEV 2 /* For hstati */

/* re-declare the bos common */
BOSbank bcs_;
BOSbank wcs_;

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



/*globally useful variables*/
int runno = 0;
int regen = 0;

/* ----------- Function prototypes ---------------- */
void PrintUsage(char *processName);
int ProcessEvent();
void ctrlCHandle(int);
void book_histos(int runno);
void hini(char *out);
int clean_up();


/* --------------------------------------------------- */

void PrintUsage(char *processName)
{
  fprintf(stderr,"Usage: %s [-M] file1 [file2] etc....\n\n",processName);
  fprintf(stderr,"  Options:\n");
  fprintf(stderr,"\t-M[#]\tProcess only # number of events\n");
  fprintf(stderr,"\t-o[hbook_file]\tHBOOK file name\n");
  fprintf(stderr,"\t-R\tRegenerate the SC, CC and TBID banks\n"); 
  fprintf(stderr,"\t-h\tPrint this message.\n\n");
  exit(0);
}

main(int argc,char **argv)
{
  FILE *fp = NULL;
  int i,nmax=200;
  char *argptr, *outfile = NULL;
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
  clasHEAD_t *HEAD=NULL;
  int F_false = 0, F_true = -1;

  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  set_level_(&F_false,&F_false,&F_false,&F_false,&F_false);
  if (argc==1) PrintUsage(argv[0]);
  for (i=1; i<argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {
      case 'h':
	PrintUsage(argv[0]);
	break;
      case 'o':
	outfile = ++argptr;
	break;
      case 'M':
	max = atoi(++argptr);
	break;
      case 'R':
	regen =1;
	break;
      default:
	fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
	PrintUsage(argv[0]);
	break;
      }
    }
  } 

  /* Zero the parameter arrays */
  memset(st_values,0,6*sizeof(fit_t));
  memset(tof_values,0,SC_NPADDLES*sizeof(fit_t));

  /*BOS initialization */
  bnames_(&nmax);
  initbos();

  /* Open the bos files and loop through the events. */
  for (i = 1;i < argc; ++i) {
    argptr = argv[i];
    if (*argptr != '-') {
      sprintf(mess,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", argptr);
      if (!fparm_c(mess)) {
	fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],argptr,strerror(errno));
      }
      else {
	while ((max ? Nevents < max : 1) && getBOS(&bcs_,1,"E")) {
	  Nevents++;
	  if(Nevents == 1){
	    /* Initialize the book file and the scintillator, st, etc. 
	       packages */
	    hini(outfile);
	    if (HEAD=getBank(&bcs_,"HEAD")) runno=HEAD->head[0].nrun;
	    ConfigEvent(runno,regen);
	  }
	  if (Nevents % 100 == 0) {
	    fprintf(stderr,"%d\r",Nevents);
	    fflush(stderr);
	  }
	  ProcessEvent();
	  dropAllBanks(&bcs_,"E");
	  cleanBanks(&bcs_);
	}
	fprintf(stderr,"#  of events processed: %d\n",Nevents);
	/*close file*/
	sprintf(mess,"CLOSE BOSINPUT", argptr);
	fparm_c(mess);
      }
    }
  }

  /* Close the hbook file cleanly */
  clean_up();
}


int clean_up(){
  int i, icycle,all=0;
  
  hrout(0,icycle," ");
  /*hldir_(" ", " ", 1L, 1L); */ /* don't remove this line */
  hrend_("esr", 3L);
}
 
/* Open the HBOOK file for writing the histograms */
void hini(char *out)
{
  int lun = LUN, lrec = LREC, memh = MEMH;
  int istat;
  char def_out[100];
  clasHEAD_t *HEAD = getBank(&bcs_, "HEAD");

  if(out == NULL) {
    if (HEAD){
      sprintf(def_out,"tof%d.hbook", HEAD->head[0].nrun);
    } else {
      sprintf(def_out,"tof.hbook");
    }
    out = &def_out[0];
  }  
  fprintf(stderr, "HBOOK file name is: %s\n", out);
 
  iquest_[9] = 65000;
  hlimit_(&memh);
  hropen_(&lun, "esr", out , "N", &lrec, &istat, 3L, strlen(out), 1L);
  book_histos(HEAD->head[0].nrun);
}


/* Book the histograms */
void book_histos( int runno){
  int i, j,bins;
  float min, max, v;
  char title[100];
  char runmes[50];
  int sec,all=0;
  
  sprintf(runmes,"Run %d", runno);
  for (i=1;i<7;i++){
    sprintf(title, "ST vertex time -tag vertex time,sec %d,%s",i,
	  runmes);
    hbook1(i,title,300,-30.0,30.0,0); 
    for (j=1;j<49;j++){
      sprintf(title, "TOF pion vertex time - tag time, sec %d, id %d, %s",
	      i,j,runmes);
      hbook1(100*i+j,title,200,-10,10,0);
    }
  }
}


int ProcessEvent()
{
  /*----RAW Event Banks------*/
  clasHEAD_t *header = NULL;

  /*----Reconstruction Banks---*/
  clasTBTR_t *TBTR = NULL;
  clasBID_t *TBID = NULL;
  clasSCRC_t *SCRC = NULL;
  clasSTR_t *STR = NULL;
  float dedx;
  int q,scid;
  int i,j,sec,trk_ind;
  float mass,gamma,beta,p;
  float pion_vertex_time=0,pion_beta,t_diff;
  tdpl_t *tdpl=NULL;

  /*re-make HBID and TBID banks*/
  if (regen) TBID=make_BID_banks(regen);
  else TBID = getBank(&bcs_, "TBID");
 
  TBTR = getBank(&bcs_, "TBTR");
  STR = getBank(&bcs_, "STR ");

  /* Calculate dEdx in the scintillators and compare vertex times between the 
     tof and the start counter/tagger with pions identified this way */

  if (TBTR && TBID){
    for (i=0; i<TBID->bank.nrow;i++) {
      trk_ind=TBID->bid[i].track;
      if(trk_ind){
	p = v3mag(TBTR->tbtr[trk_ind-1].p);
	q = TBTR->tbtr[trk_ind-1].q;
	
	dedx=tbid2dedx(&TBID->bid[i]); 
	
	if(dedx>4 && dedx<14 && p>0.3 && p<0.7){
	  /* Assume this is a pion*/
	  
	  tdpl=tbtr2tdpl(&(TBTR->tbtr[trk_ind-1]));
	  pion_beta=p/sqrt(p*p+PI_CHARGED_MASS*PI_CHARGED_MASS);
	  pion_vertex_time=TBID->bid[i].sc.time- 
	    pathlen2sc((bid_t *)&TBID->bid[i],(hdpl_t *)tdpl)/
	    SPEED_OF_LIGHT*1e9/pion_beta;
	  t_diff = pion_vertex_time - TBID->bid[i].vtime;
	  if (SCRC=getGroup(&bcs_,"SCRC",TBID->bid[i].sec)){
	    scrc_t scrc=SCRC->scrc[TBID->bid[i].sc.id-1];
	    if(scrc.status<100){
	      hf1(TBID->bid[i].sec*100+scrc.id,t_diff,1);
	    }
	  }
	  if(TBID->bid[i].st.stat){
	    int str_ind=TBID->bid[i].st.id-1;
	    if (STR->str[str_ind].status==1){
	      pion_vertex_time=TBID->bid[i].st.time-STR->str[str_ind].st_l/
	        SPEED_OF_LIGHT*1e9/pion_beta;
	      t_diff=pion_vertex_time-TBID->bid[i].vtime;
	      hf1(TBID->bid[i].sec,t_diff,1);
	    }
	  }
	}
      }
    }
  }
  return(1);
}


void ctrlCHandle(int x)
{
  int icycle;

  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  fprintf(stderr,"\n\n\t\t\t***  INTERRUPTED!!!  ***\n\n");

  /* Close the hbook file cleanly */
  clean_up();

  exit(1);
}

/* end file */





