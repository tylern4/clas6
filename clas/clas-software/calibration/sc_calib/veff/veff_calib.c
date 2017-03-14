/* veff_calib.c */
/* Author: S. Taylor
 *  Adding Gamecock mode: Jörn Langheinrich
       */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ntypes.h>
#include <bostypes.h>
#include <map_manager.h>
#include <clas_cern.h> 
#include <scExtern.h>
#include <signal.h>

#define ADC_PEDESTALS 6
#define T0 0
#define T1 1
#define T2 2
#define LEFT 0
#define RIGHT 2
#define STAND_DEV 2
#define MEAN 1
#define MEMH 50000000
float pawc_[MEMH];

/*prototypes*/
int book_histogram(char *histogfile, int sector, int runno);  
int ProcessEvent(int sector);   /*sector == 0 => all*/
void PrintUsage(char *processName);
int gamecockMode = 0;

#ifdef VINTAGE_BUILD
void rootopenw(const char* filename) {}
void rootend() {}
void rootbook1(int hid, const char* title, int xbins, double xmin, double xmax) {}
void rootf1(int hid, double x, double value) {}
void count_initialize(int nfiles, char* filelist[]) {}
double get_percent(int ifile, int eventNo) {return 0.;}
int  coupled_paddle_z2index(int sector, int stripe, double zpos) {return 0;}
#else
extern  void rootopenw(const char* filename);
extern  void rootend();
extern  void rootbook1(int hid, const char* title, int xbins, double xmin, double xmax);
extern  void rootf1(int hid, double x, double value);
extern  void count_initialize(int nfiles, char* filelist[]);
extern  double get_percent(int ifile, int eventNo);
extern  int  coupled_paddle_z2index(int sector, int stripe, double zpos);
#endif

void ctrlCHandle(int);
/*----------------------------------------------------------------------*/
void PrintUsage(char *processName)
{
  fprintf(stderr,"Usage: %s [-n] [-o] file1 [file2] etc....\n\n",processName);
  fprintf(stderr,"  Options:\n");
  fprintf(stderr,"\t-n[#]\t\tProcess only # number of events\n\n");
  fprintf(stderr,"\t-o[filename]\tHistogram output file\n\n");
  fprintf(stderr,"\t-s[#]\t\tSector number [Default: process all]\n\n");
  fprintf(stderr,"\t-i\t\tBatch mode, no counter\n\n");
  fprintf(stderr,"\t-G\t\tGamecock mode\n\n");
  fprintf(stderr,"\t-h\t\tPrint this message.\n\n");
  exit(0);
}


int main(int argc,char *argv[]){
  sc_const_t dummy1,dummy2;
  char *argptr;
  char mess[128];
  char *outfile=NULL;
  char *number;
  FILE *inputfileptr;
  FILE *outputfile;
  int batchmode=0;
  clasHEAD_t *HEAD=NULL;
  int Nevents=0,i,j,k,errno,runno,max=9999999;
  int sector=0;
  int memh = MEMH;
  int isec;

  /* copy filename to list */
  double curPercent = 0.;
  char* dataFile[256];
  int nfiles = 0;
  
  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  /* Parse command-line arguments */
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
      case 'n':
	max = atoi(++argptr);
	break;
      case 's':
	sector=atoi(++argptr);
	if(sector>6 || sector <0) exit(1);
	break;
      case 'G':
	gamecockMode = 1;
	break;
      case 'i':
	batchmode=1;
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


  if (gamecockMode) {
    count_initialize(nfiles, dataFile);
  }
  else {
    /* Initialize PAW system */
    hlimit_(&memh);
  }

  /* Initialize BOS */
  initbos();
  Nevents=0;
  
  /* Loop over the datafiles specified in command-line args. */
  for (i = 0;i < nfiles; ++i) {
    sprintf(mess,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", dataFile[i]);
    if (!fparm_c(mess)) {
      fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",
	      argv[0],dataFile[i],strerror(errno));
    }
    else {
      printf("Processing %s...\n", dataFile[i]);
      while ((max ? Nevents < max : 1) && getBOS(&bcs_,1,"E")) {
	Nevents++;
	HEAD=(clasHEAD_t *)getBank(&bcs_,"HEAD");
	if(Nevents==1){
	  /* Get run number from HEAD bank */
	  if (HEAD!=NULL)
	    runno=HEAD->head[0].nrun;
	  else{
	    printf("No HEAD bank!\n");
	    exit(1);
	  }
	  /* Initialize the SC package */
	  initialize_tof(runno);
	  /* book histograms */
	  if (!i) book_histogram(outfile, sector, runno);
	}
	
	ProcessEvent(sector); /* process event, sector=0 => all */

	if (!sector) { 
	  for (isec=1; isec<=6; isec++) {
	    dropBank(&bcs_, "SC  ", isec);
	    dropBank(&bcs_, "SC1 ", isec);
	    dropBank(&bcs_, "TDPL", isec);
	  }
	}
	else {
	  dropBank(&bcs_, "SC  ", sector);
	  dropBank(&bcs_, "SC1 ", sector);
          dropBank(&bcs_, "TDPL", sector);
	}
        dropAllBanks(&bcs_, "E");
	cleanBanks(&bcs_);
	
	if (Nevents%100==0){
	  if (gamecockMode) {
	    if (HEAD) curPercent = get_percent(i, HEAD->head[0].nevent);
	    fprintf(stdout, "Processed: %7.3f \%\n", curPercent);
	    fflush(stdout);
	  }
	  else {
	    if (!batchmode) {
	      fprintf(stderr,"%d\r",Nevents);
	      fflush(stderr);
	    }
	  }
	}	
      }
    }
    fparm_c("CLOSE BOSINPUT");
  }

  if (gamecockMode) { 
    rootend();
  }
  else {
    hrout(0,2," ");
    hldir_(" ", " ", 1L, 1L); 
    hrend("junk");
  }
}

void ctrlCHandle(int x)
{
  int icycle,j;

  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  fprintf(stderr,"\n\n\t\t\t***  INTERRUPTED!!!  ***\n\n");
 
  hrout(0,2," ");
  hldir_(" ", " ", 1L, 1L); /* don't remove this line */
  hrend("junk");
  exit(1);
}

/* Initialize the paw system and book histograms */
int book_histogram(char *histogfile, int sector, int runno){
  int id,j,k,sec;
  char defaultname[255];
  char title[128];
		 
  int s0 = sector ? sector : 1;
  int s1 = sector ? sector : 6;

  sprintf (defaultname, "veff_%d.%s", runno, (gamecockMode?"root":"hbook") );
  if (!histogfile) histogfile = defaultname;

  printf("Booking histograms...\n");

  if (gamecockMode) {
    rootopenw (histogfile);
  }
  else {
    hropen(2,"junk",histogfile,"N",1024,0);  
  }

  for(sec=s0;sec<=s1;sec++){
    for (id=1;id<=SC_NPADDLES_SEC;id++){
      sprintf(title,"time diff vs. y, sec %d id %d", sec, id);
      if (gamecockMode) {
        rootbook2(100*sec+id, title, 100, -200.0, 200.0,100,-50.0,50.0);
        if((id >= 40) && (SC_VERSION_FLAG == 1)){
          sprintf(title,"time diff vs. y, sec %d id %dA", sec, id);
          rootbook2(100*sec+id+9, title, 100, -200.0, 200.0,100,-50.0,50.0);
          sprintf(title,"time diff vs. y, sec %d id %dB", sec, id);
          rootbook2(100*sec+id+18, title, 100, -200.0, 200.0,100,-50.0,50.0);
        }
      }else {hbook2(100*sec+id, title, 100, -200.0, 200.0,100,-50.0,50.0,0);}
    }
  }
}


int ProcessEvent(int sector) {
  clasSC1_t  *SC1  = NULL;
  clasTDPL_t *TDPL = NULL;
  clasSCG_t  *SCG  = NULL;
  int i,j,plane, sec, stripe;
  float x,ratio, pos_y, pos_z, t_diff;
  extern sc_const_t sc_m0;
  
  int s0 = sector ? sector : 1;
  int s1 = sector ? sector : 6;

  for(sec=s0;sec<=s1;sec++){  
    if(!(SC1=getBank(&bcs_, "SC1 "))){
      make_SC1_bank(sec);
    }
    
    if(   (SC1 = getGroup(&bcs_,"SC1 ", sec))
       && (TDPL= getGroup(&bcs_,"TDPL", sec))
       && (SCG = getGroup(&wcs_,"SCG ", sec))){

      for(j=0;j<TDPL->bank.nrow/10;j++){
	for (i=0; i < SC1->bank.nrow; i++){
	  
	  if((dpl2sc_id(sec,(hdpl_t *)&(TDPL->tdpl[j*10]))==SC1->sc1[i].id)){
	    stripe = SC1->sc1[i].id;
	    plane  = SCG->scg[SC1->sc1[i].id-1].panel+3;
	    pos_y  = TDPL->tdpl[j*10+plane].pos.y;
	    pos_z  = TDPL->tdpl[j*10+plane].pos.z;
	    t_diff = SC1->sc1[i].time_l-SC1->sc1[i].time_r;
	    if (gamecockMode) {
	      int icoupled = 0;
	      rootf2(100*sec+stripe, pos_y, t_diff, 1.);

           if(SC_VERSION_FLAG == 1){
	      if ((icoupled = coupled_paddle_z2index(sec, stripe, pos_z)) > 0)
		rootf2(100*sec+stripe+icoupled*9, pos_y, t_diff, 1.);
           }
	    }
	    else {
	      hf2(100*sec+stripe, pos_y, t_diff, 1.);
	    }
	  }
	}
      }
    } 
  }

  return(1);
}

