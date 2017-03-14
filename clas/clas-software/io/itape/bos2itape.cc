/*
 * bos2itape.cc   
 * Purpose: convert bos banks to itape format
 */


extern "C" {
  
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <clas_cern.h>
#include <particleType.h>
#include <kinematics.h>
#include <pdgutil.h>
#include <pid.h>
#include <scalers.h>
#include <utility.h>
#include <printBOS.h>
#include <ec.h>
#include <PartUtil.h>
#include <dataIO.h>
#include <itape.h>

	   }

#include <iostream.h>


#define BUFSIZE 900000

  
  /* ----------- Function prototypes ---------------- */
int ProcessEvent(itape_header_t *);
void ctrlCHandle(int);
void PrintUsage(char *processName);
  /* declare the bos common */
BOSbank bcs_;
  
 

int max = 0;

/* --------------------------------------------------- */

void PrintUsage(char *processName)
{
  cerr << "Usage: " << processName << " [-M] file1 [file2] etc....\n\n";
  cerr << "  Options:\n";
  cerr << "\t-M[#]      \tPrint out only # number of events\n";
  cerr << "\t-p[#]      \tPrint this number part bank out\n";
  cerr << "\t-C         \tmonteCarlo mode, generate beam from TAGR->tag[0].erg\n";
  cerr << "\t-h         \tPrint this message\n";
  exit(0);
}



main(int argc,char **argv)
{
  FILE *fout = stdout;
  FILE *fp = NULL;
  int i;
  char *argptr, *outfile = NULL;
  int Nevents = 0;
  int nwrite = 0;
  char mess[100];
  int ret;
  ios::sync_with_stdio();
  int partno = 0;
  int monteCarlo = 0;
  int verbose = 0;
  int Verbose = 0;
  clasHEAD_t *HEAD;
  static itape_header_t *buffer = NULL;
  
  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  for (i=1; i<argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) { 
      case 'o':
	if ( !(fout = fopen(++argptr,"w"))) {
	 cerr << "Unable to open file: " << argptr << endl;
	 exit(0);
	}
	break;
      case 'v':
	verbose = 1;
	break; 
      case 'V':
	Verbose = 1;
	break;
      case 'h':
	PrintUsage(argv[0]);
	break;      
      case 'M':
	max = atoi(++argptr);
	break;
      default:
	fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
	PrintUsage(argv[0]);
	break;
      }
    }
  }
  
  /*initialize bos and hbook*/
  initbos();


  /* initialize itape */
  if (!buffer) {
    if(!(buffer = (itape_header_t *)malloc(BUFSIZE))) {
      fprintf(stderr,"Error on the memory allocation\n");
      fprintf(stderr,"look at code of file %s at line %d\n", __FILE__, __LINE__);
      exit(1);
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
	int Nwrite = 0;
	while ((max ? Nevents < max : 1) && getBOS(&bcs_,1,"E")) {
	  Nevents++;
	  if (ProcessEvent(buffer)) {
	    if (fout) {
	      data_write(fileno(fout),buffer);
	      nwrite++;
	    }
	  }
	  
	    if (verbose) {
	      if (! (Nevents % 100))
		cerr << Nevents << "\t\r" << flush;
	    }

	    dropAllBanks(&bcs_,"E");
	    cleanBanks(&bcs_);
	  }
      }
	fprintf(stderr,"#  of events processed: %d\n",Nevents);
	sprintf(mess,"CLOSE BOSINPUT", argptr);
	fparm_c(mess);
    }
    
  }

}


int ProcessEvent(itape_header_t *buffer)
{
  data_newItape(buffer);
  fillHeader(buffer);
  GenBanks(buffer,BUFSIZE,bcs_.iw);
  return(1);
}

void ctrlCHandle(int x)
{
  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  max = 1;
  fprintf(stderr,"\n\n\t\t\t***  INTERRUPTED!!!  ***\n\n");
}

/* end file */
