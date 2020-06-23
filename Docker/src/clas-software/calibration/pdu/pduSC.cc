#include <iostream.h>
int Nout = 0;


#define NSECTORS 7
#define NPADDLES 48
#define LR 2
#define ATS 2
#define ADC 0
#define TDC 1
#define L 0
#define R 1




/* ----------- Function prototypes ---------------- */

extern "C" {

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <bosddl.h>
#include <particleType.h>
#include <map_manager.h>


/*--------------- function prototypes ----------------*/
  //int ProcessEvent(int RunNo,int verbose);
void PrintUsage(char *processName);
void initbos();
int fparm_c(char *mess);
int getBOS(BOSbank *bcs, int lun, char *list);
void cleanBanks(BOSbank *bcs);
void dropAllBanks(BOSbank *bcs, char *list);
void *getBank(BOSbank *,const char *);

extern BOSbank bcs_;

}

/* --------------------------------------------------- */

main(int argc,char **argv)
{
  int i,a,b,c,d,isec;
  int padsum[NSECTORS][NPADDLES][ATS][LR];
  FILE *fp = NULL;
  char *argptr;
  int Nevents = 0;
  int max = 0;
  char mess[200];
  char *outfile = NULL;
  char out[100];
  int RunNo = 1;
  int verbose = 0;
  static int CurrentRun = 0;
  clasHEAD_t *HEAD;
  clasSC_t *SC;



  for (i=1; i<argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {
      case 'h':
	PrintUsage(argv[0]);
	break;
      case 'M':
	max = atoi(++argptr);
	break;
      case 'R':
	RunNo = atoi(++argptr);
	break;
      case 'v':
	verbose = 1;
	break; 
      case 'o':
	outfile =  *(++argptr) ? argptr : "/dev/fd/1";
	fprintf(stderr,"Output file: %s\n",outfile);	
	unlink(outfile);
	sprintf(out, "OPEN BOSOUTPUT UNIT=7 FILE=\"%s\" WRITE STATUS=NEW RECL=3600", outfile);
	if (!fparm_c(out)) {
	  fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],out,strerror(errno));	 
	  exit(1);
	}
	break;
      default:
	fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
	PrintUsage(argv[0]);
	break;
      }
    }
  }
       
  initbos();

  //-------------------------------------------------------------------------
  // zero the BAD arrays
  for (a=0;a<LR;++a){
    for (b=0;b<ATS;++b){
      for (c=0;c<NPADDLES;++c){
	for (d=0;d<NSECTORS;++d){
	  padsum[d][c][b][a]=0;
	}
      }
    }
  }
  for (i = 1;i < argc; ++i) {
    argptr = argv[i];
    if (*argptr != '-') {
      sprintf(mess,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", argptr);
      if (!fparm_c(mess)) {
	fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],argptr,strerror(errno));
      }
      else {
	
	while ((max ? Nevents < max : 1) && getBOS(&bcs_,1,"E") ) {
	  Nevents++;
	  if (Nevents % 100 == 0) {
	    fprintf(stderr,"%d %d\r",Nevents,Nout);
	    fflush(stderr);
	  }
	  
	  if (HEAD = (clasHEAD_t *)getBank(&bcs_,"HEAD")) {
	    if (CurrentRun != HEAD->head[0].nrun) {
	      CurrentRun = HEAD->head[0].nrun;
	    }

	    for (isec = 1; isec < NSECTORS; ++isec) {
	      clasSC_t *SC = (clasSC_t *)getGroup(&bcs_,"SC  ",isec);
	      if (SC) {
		for (i = 0; i < SC->bank.nrow; ++i) {
		  if (SC->sc[i].adcl)
		    padsum[isec][SC->sc[i].id][ADC][L]++;
		  if (SC->sc[i].adcr)
		    padsum[isec][SC->sc[i].id][ADC][R]++;
		  if (SC->sc[i].tdcl)
		    padsum[isec][SC->sc[i].id][TDC][L]++;
		  if (SC->sc[i].tdcr)
		    padsum[isec][SC->sc[i].id][TDC][R]++;
		  if (verbose==1)
		    cout << isec << " " << SC->sc[i].id << " " 
			 << padsum[isec][SC->sc[i].id][ADC][L] << " "
			 << padsum[isec][SC->sc[i].id][ADC][R] << " "  
			 << padsum[isec][SC->sc[i].id][TDC][L] << " "  
			 << padsum[isec][SC->sc[i].id][TDC][R] << endl ;		    

		}
	      }
	    }
	    Nout++;
	    putBOS(&bcs_, 7, "E");

	  }

	  dropAllBanks(&bcs_,"E");
	  cleanBanks(&bcs_);
	}


	fprintf(stderr,"#  of events: %d\n",Nevents);

	/*close file*/
	sprintf(mess,"CLOSE BOSOUTPUT UNIT=7");
	fparm_c(mess);


      }
    
    }

  }

  for (c=1;c<NSECTORS;++c){
    for (d=0;d<NPADDLES;++d){
      cout << c << " " << d << " " << padsum[c][d][ADC][L] << " "  << padsum[c][d][ADC][R] << " "  << padsum[c][d][TDC][L] << " "  << padsum[c][d][TDC][R] << endl ;
    }
  }    
}


  //-------------------------------------------------------------------------
 

void PrintUsage(char *processName)
{
  fprintf(stderr,"\nUsage: %s [options] input.file > output.file\n",processName);
  fprintf(stderr,"\toptions:\n");
  fprintf(stderr,"\t-M#\t# number of events to process\n");
  fprintf(stderr,"\t-v\tprints out intermediate steps\n");
  fprintf(stderr,"\t-h\tPrint this message.\n");

  exit(0);
}


