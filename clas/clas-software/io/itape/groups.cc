/*
 * groups.cc   
 * Purpose: print out groups in an itape
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
	   }

#include <iostream.h>
#include <itape.h>


#define BUFSIZE 200000

  
  /* ----------- Function prototypes ---------------- */
int ProcessEvent(itape_header_t *);
int GetDat(FILE *finput);
void ctrlCHandle(int);
void PrintUsage(char *processName);
int StartRun(int);
int EndRun(int);


extern "C" {
int data_listGroups(itape_header_t *event,int *ngroups_ret,int **list_ret,int **size_ret);
	   }


  /* declare the bos common */
BOSbank bcs_;

int CurrentRun = 0;
  
void PrintUsage(char *processName)
{
  cerr << processName << endl;
}
main(int argc,char **argv)
{
  FILE *fp = NULL;
  int max = 0;

  int ret;
  
  int Nevents = 0;
  int fieldflag = 0;
  int nfile = 0;
  time_t time1; 
  float rate; 
  int i;
  char *argptr;
  char *word;
  FILE *finput = stdin;
  FILE *foutput = NULL;
   
  if (argc == 1)
    PrintUsage(argv[0]);
  else {
    
    for (i=1; i<argc; i++) {
      argptr = argv[i];
      if (*argptr == '-') {
	argptr++;
	switch (*argptr) {


	default:
	  fprintf(stderr,"Unrecognized argument %s\n",argptr);
	  break;
	  
	}

      }
    } 
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
      }
	
      if (fp) {
	while ((max ? Nevents < max : 1) && (ret = GetDat(fp))) {
	  cerr << "ret: " << ret << endl;
	  Nevents++;
	}
	  
      }
    }
  }
  
  

  fprintf(stderr,"\nTotal number of itape records: %d\n",Nevents);
  if (foutput)
    fclose(foutput);
    
    
}

int GetDat(FILE *finput)
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
    cerr << "data_read " << ret << endl;
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
    ret = (ret != DATAIO_EOT);
  

    if (buffer && GoodEvent) {
      if (buffer->runNo != CurrentRun) {
	fprintf(stderr,"Run %d->%d\n",CurrentRun,buffer->runNo);
	EndRun(CurrentRun);
	if (StartRun(buffer->runNo)) return -1;
	CurrentRun = buffer->runNo;
	
      }
      if (GoodEvent)
	ProcessEvent(buffer);
    
    }
  }
  return(ret);

}	

int ProcessEvent(itape_header_t *buffer)
{

  int ngroups,*groups,*size;
  char name[5];
  int sec;
  cerr << "event" << endl;
  data_listGroups(buffer,&ngroups,&groups,&size);
  for(int i = 0; i < ngroups; ++i) {
    strcpy(name,GroupName(groups[i],&sec));
	   cerr << i << "\t" << groups[i] << "\t" << size[i] << "\t" << name << "\t" << sec <<endl;
  }
  free(groups);
  free(size);
  
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

