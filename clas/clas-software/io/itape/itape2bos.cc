/*
 * itape2bos.cc   
 * Purpose: convert itape format to bos format
 */


extern "C" {
  
#include <stdio.h>
#include <string.h>
#include <unistd.h>
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



#define BUFSIZE 200000

  
  /* ----------- Function prototypes ---------------- */
int ProcessEvent(itape_header_t *,char *,int);
int GetDat(FILE *finput,char *,int);
void ctrlCHandle(int);
void PrintUsage(char *processName);
int StartRun(int);
int EndRun(int);


extern "C" {
void bnames_(int *);

  /* declare the bos common */
BOSbank bcs_;
BOSbank wcs_;

	   }



int CurrentRun = 0;
  
void PrintUsage(char *processName)
{
  cerr << processName << endl;
}



main(int argc,char **argv)
{
  FILE *fp = NULL;
  int max = 0;

  int verbose = 0;

  int ret;

  unsigned int triggerMask = 0xffff;
  
  int Nevents = 0;
  int Nout = 0;
  int nfile = 0;
  time_t time1; 
  float rate; 
  int i;
  char *argptr;
  char *word;

  // itape stuff
  FILE *finput = stdin;

  // bos stuff
  char *BOSoutfile = NULL;
  int OutputUnitNo = 2,
    MaxBanks = 1000; 
  char  out[300];
  char Elist[5 * MaxBanks];


  if (argc == 1)
    PrintUsage(argv[0]);
  else {
    
    for (i=1; i<argc; i++) {
      argptr = argv[i];
      if (*argptr == '-') {
	argptr++;
	switch (*argptr) {
	case 'o':
	  if(*(++argptr))
	    BOSoutfile = argptr;


	  if(OutputUnitNo){
	    fprintf(stderr,"Output file: %s\n",BOSoutfile);
	    unlink(BOSoutfile);
	    sprintf(out, "OPEN BOSOUTPUT UNIT=2 FILE=\"%s\" READWRITE STATUS=NEW RECL=3600", BOSoutfile);
	    if (!fparm_c(out)) {
	      fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0], out, strerror(errno));	 
	      exit(1);
	    }
	  }



	  break;
	case 'v':
	  verbose = 1;
	  break;
	case 'M':
	  max = atoi(++argptr);
	  break;
	case 't':
	  // trigger mask
	  triggerMask = strtoul(++argptr,NULL,0); 
	  break;

	default:
	  fprintf(stderr,"Unrecognized argument %s\n",argptr);
	  break;
	  
	}

      }
    } 

    bnames_(&MaxBanks);
    initbos();
    configure_banks(stderr,0);

    /* count the number of input files */
    for (nfile=0,i=1;i < argc; ++i)
      if (*argv[i] != '-')
	nfile++;
  
      
    for (i=1; i<argc; i++) {
	
      if (!nfile) {
	fp = stdin;
	i = argc;
      }
	
      else {
	argptr = argv[i];
	if (*argptr != '-') {
	  if ( (!(fp = fopen(argptr,"r"))))
	    fprintf(stderr,"%s- Unable to open file %s\n",argptr);
	}
	else if ((*argptr == '-') && (strlen(argptr) == 1))
	  fp = stdin;
	else
	  fp = NULL;
      
      
	if (fp) {
  
	  while ((max ? Nevents < max : 1) &&  (ret = GetDat(fp,Elist,verbose)) ) {
	    clasHEAD_t *HEAD = (clasHEAD_t *) getBank(&bcs_,"HEAD");
	    if (HEAD) {
	      if (triggerMask & HEAD->head[0].trigbits) {
		putBOS(&bcs_, OutputUnitNo, "E");
		Nout++;
	      }
	      /* tidy up */
	      dropAllBanks(&bcs_,"E");
	      cleanBanks(&bcs_);	  
	      Nevents++;
	      if (! (Nevents % 100) ) {
		cerr << Nevents << "\r" << flush;
	      }
	    }
	  }
	}
	  
      }
    }
  }
  
  

  cerr << "\nTotal number of itape records:\t" << Nevents << endl;
  cerr << "Total number of itape records written:\t" << Nout << endl;
  putBOS(&bcs_, 2, "0");
  sprintf(out, "CLOSE BOSOUTPUT UNIT=2");
  fparm_c(out);  
    
}

int GetDat(FILE *finput,char *Elist,int verbose)
{

  static int Nevents = 0;
  static int nwrite = 0;
  static itape_header_t *buffer = NULL;
  static int LastRead = DATAIO_OK;
  int GoodEvent = 0;

  int rd;
  int size;
  int ret = 0;
  static int n = 0;
  time_t Ctime;

  if (!buffer)
    buffer = (itape_header_t *)malloc(BUFSIZE);
  

  if (finput) {

    /* read from input */
    ret = data_read(fileno(finput),buffer,BUFSIZE);
    switch (ret) {
    case DATAIO_OK:
      LastRead = ret;
      GoodEvent = 1;
      break;
    case DATAIO_EOF:
    case DATAIO_EOT:
      LastRead = ret;
      fclose(finput);
      break;
    case DATAIO_BADCRC:
    case DATAIO_ERROR:
      if (LastRead == DATAIO_ERROR) {
	fclose(finput);
	ret = DATAIO_EOF;
	fprintf(stderr,"Premature end of data\n");
      }
      LastRead = DATAIO_ERROR;
      fprintf(stderr,"from_stdin: data_read() failed\n");
      break;
    default:
      fprintf(stderr,"from_stdin: unknown return code %d\n",ret);
      break;
    }
    //   ret = (ret != DATAIO_EOT) && (ret != DATAIO_EOT);
    ret = (ret == DATAIO_OK);

    if (buffer && GoodEvent) {
      if (buffer->runNo != CurrentRun) {
	fprintf(stderr,"Run %d->%d\n",CurrentRun,buffer->runNo);
	EndRun(CurrentRun);
	if (StartRun(buffer->runNo)) return -1;
	CurrentRun = buffer->runNo;
	
      }
      if (GoodEvent)
	ProcessEvent(buffer,Elist,verbose);
    
    }
  }

  return(ret);

}	

int ProcessEvent(itape_header_t *buffer,char *Elist,int verbose)
{
  itape2bos(buffer,&bcs_,verbose);

  return(1);
}
int StartRun(int runNo)
{  
  return 0;
}


int EndRun(int run)
{

	return 0;
}

