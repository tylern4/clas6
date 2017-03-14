/*
 *  Program: Calibrate Gmean for MIP
 *
 *  Author : Konstantin Loukachine
 *
 *  Adding Gamecock mode: Jörn Langheinrich
 *
 *  Date :   May, 24th, 1999. 
 *
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
#include <scExtern.h>
#include <utility.h>
#include <particleType.h>
#include <pid.h>

/* PAW defines */

#define MEMH 500000
#define LREC 1024 /* record length of hbook direct access file in WORDS */
#define LUN 3 /* logical unit number of hbook file */
#define ID 99 /* column-wise-ntuple id */

#define RAD2DEG (180.0/3.14159)

int gamecockMode = 0;  /* program called from gamecock, creating root histograms */
int testcalibMode = 0; /* the NMIP_ADC and Yoffset constants are read and applied to ADC */


/* declare the bos common */
BOSbank bcs_;
BOSbank wcs_;

/* declare the hbook common */
float pawc_[MEMH];
float hcfitd_[25];
int quest_[100];

/* ----------- Function prototypes ------------------ */

void PrintUsage(char *processName);
int ProcessEvent(float beam_energy);
void ctrlCHandle(int);
void book_histos(int runno, float beam_energy);
void hini(char *out);
int configure_run(float *beam_energy);
void copy_pipeline_tdc_();

#ifdef VINTAGE_BUILD
void rootopenw(const char* filename) {}
void rootend() {}
void rootbook1(int hid, const char* title, int xbins, double xmin, double xmax) {}
void rootf1(int hid, double x, double value) {}
void count_initialize(int nfiles, char* filelist[]) {}
double get_percent(int ifile, int eventNo) {return 0.;}
int  coupled_paddle_index(int sector, int stripe, int itbtr) {return 0;}
#else
extern  void rootopenw(const char* filename);
extern  void rootend();
extern  void rootbook1(int hid, const char* title, int xbins, double xmin, double xmax);
extern  void rootf1(int hid, double x, double value);
extern  void count_initialize(int nfiles, char* filelist[]);
extern  double get_percent(int ifile, int eventNo);
extern  int  coupled_paddle_index(int sector, int stripe, int itbtr);
#endif
/* --------------------------------------------------- */

void PrintUsage(char *processName)
{
  fprintf(stderr,"Usage: %s [-n] [-o] file1 [file2] etc....\n\n",processName);
  fprintf(stderr,"  Options:\n");
  fprintf(stderr,"\t-h\tPrint this message.\n\n");
  fprintf(stderr,"\t-n[#]\tProcess only # number of events\n\n");
  fprintf(stderr,"\t-o[#]\tOutput <filename>\n\n");
  fprintf(stderr,"\t-T   \tTest calibration by applying\n\t\t\tthe current calibration constants on ADC\n\n");
  fprintf(stderr,"\t-G   \tgamecock support:\n\t\t\troot histograms rather than hbook\
                  \n\t\t\tsplit coupled channels\n\t\t\tmachine readable progress meter\n\n");
  exit(0);
}

int main(int argc,char **argv)
{
  FILE *fp = NULL;
  char *argptr, *outfile = NULL;
  int i, Nevents = 0, Nep = 0, max = 0;
  int icycle, ret;
  int id = 99, flag = 0;
  float beam_energy;
  char mess[1000];

  /* copy filename to list */
  double curPercent = 0.;
  char* dataFile[256];
  int nfiles = 0;

  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  for (i=1; i<argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {
      case 'h':
	PrintUsage(argv[0]);
	break;
      case 'G':
	gamecockMode = 1;
	break;
      case 'T':
	testcalibMode = 1;
	break;
      case 'o':
	outfile = ++argptr;
	break;
      case 'n':
	max = atoi(++argptr);
	break;
      default:
	fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
	PrintUsage(argv[0]);
	break;
      }
    }
    else   /* it is a data file */
      if (nfiles < 255) {
	dataFile[nfiles++] = argv[i];
      }
      else {
	fprintf(stderr, "can't process more than 255 data files\n"); 
	PrintUsage(argv[0]);
      }
  }
  
  initbos();
  hini(outfile);

    
  if (gamecockMode) {
    count_initialize(nfiles, dataFile);
  }
	  
  for (i = 0;i < nfiles; i++) {
    sprintf(mess,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", dataFile[i] );
    if (!fparm_c(mess)) {
      fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",
	      argv[0], dataFile[i],strerror(errno));
    }
    else {
      while ((max ? Nevents < max : 1) && getBOS(&bcs_,1,"E")){
	clasHEAD_t *HEAD = getBank(&bcs_, "HEAD"); 

	int j;

	Nevents++;
	if(Nevents == 1)
	  configure_run(&beam_energy);

	if (Nevents % 1000 == 0) {
	  if (gamecockMode) {
	    if (HEAD) curPercent = get_percent(i, HEAD->head[0].nevent);
	    fprintf(stdout, "Processed: %7.3f \%\n", curPercent);
	    fflush(stdout);
	  }
	  else {
	    fprintf(stderr,"%d\r",Nevents);
	    fflush(stderr);
	  }
	}
	ProcessEvent(beam_energy);
	dropAllBanks(&bcs_,"E");
	cleanBanks(&bcs_);

      }

	/*close file*/
      fprintf(stderr,"#  of events processed: %d\n",Nevents);
      sprintf(mess,"CLOSE BOSINPUT", argptr);
      fprintf(stderr, "try to close file ... %s\n", (fparm_c(mess) ? "OK" : "failed"));
    }    
  }

  if (gamecockMode) 
    rootend();
  else {
    hrout(0,icycle," ");
    if (!gamecockMode) hldir_(" ", " ", 1L, 1L);
    hrend_("esr", 3L);
  }
}


int configure_run( float *beam_energy){
  int firsttime;
  int runno;
  clasHEAD_t *HEAD=NULL;
  char  *dir, def_map[256];

    dir = getenv("CLAS_PARMS");
    sprintf(def_map,"%s/Maps/RUN_CONTROL.map",dir);
    if(HEAD = getBank(&bcs_, "HEAD")){
    runno = HEAD->head[0].nrun;
    map_get_float(def_map, "beam","energy", 1, beam_energy,runno,&firsttime);
    *beam_energy = *beam_energy/1000.0; /*convert to GeV*/
    fprintf(stderr,"Run number: %d\n", runno);
    fprintf(stderr,"Beam energy: %f\n", *beam_energy);
    initialize_tof(runno);
    book_histos(runno, *beam_energy);
      /*    Initialize the SC package    */
    }else { 
    fprintf(stderr,"no head bank, run number unknown");
    exit(1);
    }
  }


void hini(char *out)
{
  int lun = LUN, lrec = LREC, memh = MEMH;
  int istat, icycle;
  char *def_out = gamecockMode ? "gmean.root" : "bos2hb.hbook";

  if(out == NULL) out = &def_out[0];

  if (gamecockMode) {
    rootopenw (out);
  }
  else {
    quest_[9] = 65000;
    hlimit_(&memh);
    hropen_(&lun, "esr", out , "N", &lrec, &istat, 3L, strlen(out), 1L);
  }

  return;
}


void book_histos( int runno, float beam_energy){
  int i, id, hid, index, sec;
  char title[100];
  char name[20];
  
  printf ("Booking histograms for run %d\n", runno);
/* booking  histos */

  for (sec=1; sec<=6; sec++){
    for (id=1;id<=SC_NPADDLES_SEC;id++){
      sprintf(title,"geometric mean, sec %d, id %d, Run %d", sec, id, runno);
      hid = 100*sec+id;
      if (gamecockMode) {   /* call to C++ constructor */
	   rootbook1(hid, title, 100, 50.0, 2500.0);
	   if((id >= 40) && (SC_VERSION_FLAG == 1)){
	     sprintf(title,"geometric mean, sec %d, id %dA, Run %d", sec, id, runno);
	     rootbook1(hid+9, title, 100, 50.0, 2500.0);
	     sprintf(title,"geometric mean, sec %d, id %dB, Run %d", sec, id, runno);
	     rootbook1(hid+18, title, 100, 50.0, 2500.0);
	   }
      }else
	   hbook1(hid, title, 100, 50.0, 2500.0 ,0);
    }
  }
}


int ProcessEvent(float beam_energy){

  clasHEAD_t *HEAD = getBank(&bcs_, "HEAD");
  clasTBID_t *TBID = NULL;
  clasPART_t *PART = NULL;
  clasSCRC_t *SCRC = NULL;
  clasSCR_t  *SCR  = NULL;
  clasSC_t   *SC   = NULL;
  clasHEVT_t *HEVT = NULL;

  int   i,j,runno,charge,stat_ec,stat_sc,stat_trk,index_tbid;
  int   sec,sector,sc_nn,cntr_nn,index,stat_scrc,iscr;
  float gmean, Part_beta;
  
  cntr_nn = 0;
  sector = 0;

/* ------------------------------------------------------------ */   
  dropAllBanks(&bcs_,"SC1 SCR SCRC");
  for(sec=1; sec <= 6; sec++) {        
    make_SC1_bank(sec);   
    make_SCR_bank(sec,"TDPL");
    make_SCRC_bank(sec);
  }      
  TBID = getGroup(&bcs_, "TBID", 1);  
  PART = getGroup(&bcs_, "PART", 1);  
  HEVT = getBank(&bcs_, "HEVT");
/* ------------------------------------------------------------ */

  // if all banks present
  if(HEAD && TBID && PART && HEVT){

    runno = HEAD->head[0].nrun;
    if(runno >= 55357)
      copy_pipeline_tdc_();

    // loop particles
    for (j = 0; j < PART->bank.nrow; j++){

      index_tbid = PART->part[j].trkid - 1;   
      stat_sc    = TBID->tbid[index_tbid].sc_stat;
      //printf("PID:%d\n",PART->part[j].pid);
      //printf("nrow:%d\n",j);
      //printf("index_tbid:%d\n\n",index_tbid);
      //printf("qpid:%d\tqtrk:%d\t\n",PART->part[j].qpid, PART->part[j].qtrk);
      //printf("beta:%f\tvtime:%f\n",TBID->tbid[index_tbid].beta,TBID->tbid[index_tbid].vtime);
      //printf("track:%d\tsc_id:%d\t\n", TBID->tbid[index_tbid].track,TBID->tbid[index_tbid].sc_id);
      //printf("sc_qual:%f\t\n", TBID->tbid[index_tbid].sc_qual);
      //printf("stat_sc:%d\n\n",stat_sc);
      
      // good match between track and SC
      if(stat_sc == GOOD_MATCH){
	
	sector = TBID->tbid[index_tbid].sec;
	sc_nn  = TBID->tbid[index_tbid].sc_id - 1;
	charge = PART->part[j].q;
	index_tbid = PART->part[j].trkid - 1;
	Part_beta = TBID->tbid[index_tbid].beta;
	
	if(SCRC = getGroup(&bcs_, "SCRC", sector)) {
	  cntr_nn = SCRC->scrc[sc_nn].id;
	  stat_scrc = SCRC->scrc[sc_nn].status;
	}
	
	// cut on fast pions
	if( charge!=0 && Part_beta > 0.85){
	  
	  
	  if(SCRC && stat_scrc < 100){
	    SC = getGroup(&bcs_, "SC  ",sector);
	    
	    for (i = 0; i < SC->bank.nrow; i++){
	      index=sc_index(sector,SC->sc[i].id & 0xFF);
	      
	      // the counter from SCRC bank
	      if((SC->sc[i].id & 0xFF) == cntr_nn){
		if((((float)SC->sc[i].adcl-sc_pedestals.left[index]) > 40)  &&
		   (((float)SC->sc[i].adcr-sc_pedestals.right[index]) > 40) ){
	       
		  
		  gmean = sqrt( ((float)SC->sc[i].adcl-sc_pedestals.left[index])*
				((float)SC->sc[i].adcr-sc_pedestals.right[index]) );
		
		  SCR = NULL;

		  if (testcalibMode && (SCR = (clasSCR_t*) getGroup(&bcs_, "SCR ", sector))) {
		    gmean = 0;
		    for (iscr = 0; iscr < SCR->bank.nrow; iscr++) {
		      if (SCR->scr[iscr].id == cntr_nn) {
			gmean = SCR->scr[iscr].energy * 60.;
			break;
		      }
		    }
		  }

		  if (gamecockMode) {
		    int stripe = SC->sc[i].id & 0xFF;
		    int icoupled = 0;
		    rootf1(100*sector+stripe,gmean,1.);
              if(SC_VERSION_FLAG == 1){
		      if((icoupled =
			  coupled_paddle_index(sector, stripe, TBID->tbid[index_tbid].track-1)) > 0)
		        rootf1(100*sector+stripe+icoupled*9,gmean,1.);
              }
		  }
		  else
		    hf1(100*sector+(SC->sc[i].id & 0xFF),gmean,1);
		  
		  //fprintf(stderr,"cntr_nn: %d, id: %d, \n",cntr_nn,SC->sc[i].id); 
		  //fprintf(stderr,"sector: %d, id: %d, gmean: %f \n",sector,cntr_nn,gmean); 
		}
		
	      }
	    }    
	  }
	}
	
	
      }
    }
  }
  return(1);  
}


void ctrlCHandle(int x){
  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  fprintf(stderr,"\n\n\t\t\t*** !!! HASTA LA VISTA !!!  ***\n\n");
  exit(1);
}

/* end file */
