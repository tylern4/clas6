/*
 * Disp2bos.cc   
 * Purpose: convert itape format from Dispatcher to a bos file
 */


extern "C" {
  
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <time.h>
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
int GetDispatcherCommand();
int ProcessEvent();
int GetDat(FILE *finput,char *,int);
void ctrlCHandle(int);
void PrintUsage(char *processName);
int StartRun(int);
int EndRun(int);
int ProcessData(itape_header_t *buffer);
int dispatcherReconnect(const char*host,int pipelinePrefill);

extern "C" {
void bnames_(int *);

  /* declare the bos common */
BOSbank bcs_;
BOSbank wcs_;

	   }


static int requestedRawData = 0;
int CurrentRun = 0;
  
void PrintUsage(char *processName)
{
  cerr << processName << endl;
}



main(int argc,char **argv)
{
  FILE *fp = NULL;
  int max = 0;
  int OpenOutput = 0;

  int verbose = 0;

  int ret = 1;
  
  int Nevents = 0;
  int nfile = 0;
  time_t time1; 
  float rate; 
  int i;
  char *argptr;
  char *word;
  
  int RawMode = 0;
  int GenerateOutput = 0;

  // Dispatcher
  char *server = "localhost:10099";
  int dispatcherPipelinePrefill = 5;

  // itape stuff
  FILE *finput = stdin;

  // bos stuff
  char *BOSoutfile = NULL;
  int OutputUnitNo = 9,
    MaxBanks = 1000; 
  char  out[300];
  char Elist[5 * MaxBanks];


    
  for (i=1; i<argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {
      case 'r':
	RawMode = 1;
	break;
      case 'D':
	server = ++argptr;
	break;
      case 'o':
	if(*(++argptr)) {
	  BOSoutfile = argptr;


	if(OutputUnitNo){
	  fprintf(stderr,"Output file: %s\n",BOSoutfile);
	  unlink(BOSoutfile);
	  sprintf(out, "OPEN BOSOUTPUT UNIT=9 FILE=\"%s\" WRITE STATUS=NEW RECL=3600", BOSoutfile);
	  if (!fparm_c(out)) {
	    fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0], out, strerror(errno));	 
	    exit(1);
	  }
	  OpenOutput = 1;
	}
	}
	else
	  GenerateOutput = 1;
       



	break;
      case 'v':
	verbose = 1;
	SetVerbose(verbose);
	break;
      case 'M':
	max = atoi(++argptr);
	break;

      default:
	fprintf(stderr,"Unrecognized argument %s\n",argptr);
	break;
	  
      }

    }
  } 

  fprintf(stderr,"\nE852 Dispatcher DISIO_OK: %d DISIO_EOF: %d\n",DISIO_OK,DISIO_EOF);
  // Initialize BOS
  bnames_(&MaxBanks);
  initbos();
  configure_banks(stderr,0);

  // Connect to Dispatcher
  if (server){
    if (RawMode) {
      initDispatcher(server,10);
    }
    
    else { 
      if (initDispatcherDisplay(server,10) != 0) {
 	exit(1);
      }
    }
  }
  else 
    {
      fprintf(stderr,"Error: You must tell me where the Dispatcher is (-Dhost:port) when running from the Dispatcher (-s0 switch)!\n\n");
      exit(1);
    }
  
  
  while ((max ? Nevents < max : 1) && ret) {
    ret = getData(&bcs_,"E");
    fprintf(stderr,"main: return: %d\n",ret);
    if (ret == DISIO_DATA) {
      ProcessEvent();
   Nevents++;
      if (OpenOutput)
	putBOS(&bcs_,9,"E");
	  dropAllBanks(&bcs_,"E");
	  cleanBanks(&bcs_);
    }
    else if (ret == DISIO_COMMAND) {
      char *cmd;
      char *local;
      char *word,*word2;
 
      fprintf(stderr,"Message from Giant Head: %s\n",getBuffer());
      cmd = getBuffer();
      local = malloc(strlen(cmd) + 1);
      strcpy(local,cmd);
      word = strtok(local,":");
      if (strcmp(word,"BEGINTAPE") == 0) {
	word2 = strtok(NULL,": ");
	if (word2 && GenerateOutput) {
	  if (OpenOutput) { 
	    sprintf(out, "CLOSE BOSOUTPUT UNIT=9");
	    fparm_c(out);
	    OpenOutput = 0;
	  }
	  sprintf(out, "OPEN BOSOUTPUT UNIT=9 FILE=\"c_%s.dat\" WRITE STATUS=NEW RECL=3600", word2);
	  fprintf(stderr,"%s\n",out);
	  if (!fparm_c(out)) {
	    fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0], out, strerror(errno));	 
	    exit(1);
	  }
	  else
	    OpenOutput = 1;
	}
      }
	  
      
      
    }
 
  }
  disIO_command("FINISHED");
  fprintf(stderr,"\nTotal number of itape records: %d\n",Nevents);
  sprintf(out, "CLOSE BOSOUTPUT UNIT=9");
  fparm_c(out);  
    
}

int ProcessEvent()
{
  clasDC0_t *DC0;
  clasHEAD_t *HEAD = (clasHEAD_t *)getBank(&bcs_,"HEAD");
  if (HEAD) {
    cout << HEAD->head[0].nrun << " " << HEAD->head[0].nevent << endl;
    cout << ctime((const long *) &HEAD->head[0].time) << endl;
  }
  else
    cout << "Unable to find head bank" << endl;

}

int StartRun(int runNo)
{  
  return 0;
}


int EndRun(int run)
{

	return 0;
}

int GetDispatcherCommand()
{
  int ret;
  int maxSend = 2;
  int OkToSendData = 1;
  int WaitForALLFINISHED = 0;
  int runNo;

  /* wait for command from Dispatcher */

  fd_set rset;
  fd_set wset;
  struct timeval timeout;
  int maxfd = 0;

  FD_ZERO(&rset);
  FD_ZERO(&wset);

  FD_SET(disIO_socket,&rset); if (disIO_socket > maxfd) maxfd = disIO_socket;

  if (OkToSendData && (requestedRawData > 0))
    {
      FD_SET(disIO_socket,&wset); if (disIO_socket > maxfd) maxfd = disIO_socket;
    }

  timeout.tv_sec  = 1;
  timeout.tv_usec = 0;

  if (OkToSendData && (requestedRawData > 0))
    {
      timeout.tv_sec  = 0;
      timeout.tv_usec = 0;
    }

  ret = select(maxfd + 1, &rset, &wset, NULL, &timeout);
  if (ret < 0)
    {
      fprintf(stderr,"DisFeedDAQ: Error: select() returned %d, errno: %d (%s)\n",ret,errno,strerror(errno));
      //	  exitFlag = 1;
      exit(0);
    }    

  /* ok, we got a command. Now parse it */
  static char *msg = NULL;
  static int msglen = 0;
  char *cmd0;
  char *word;
  int maybeUnexpected = 0;
  char *local = NULL;

  if (msg)
    free(msg);

  msg = NULL;

  ret = disIO_readRAW_alloc((void **)&msg,&msglen,0);


  if (msg) {
  local = (char *) malloc(msglen + 1);
  strncpy(local,msg,msglen);
  local[msglen + 1] = NULL;
    word = strtok(local,":");
    cmd0 = word;
    if (word) {
      cerr << "COMMAND: " << cmd0 << endl;

      if (strcmp(word,"NOP") == 0)
	{
	  /* do nothing */
	}
      else if (strcmp(word,"PING") == 0)
	{
	  printf("DisFeedDAQ: Command from Dispatcher: %s\n",word);
	  disIO_command("PING-REPLY");
	}
      else if (strcmp(word,"REQUEST_DATA") == 0)
	{
	  int imore;

	  /* fprintf(stderr,"DisFeedDAQ: Command from Dispatcher: %s\n",word); */

	  word = strtok(NULL,":");

	  if (word)
	    imore = strtoul(word,NULL,0);
	  else
	    imore = 1;

	  /* printf("REQUEST_DATA: more: %d, requested events: %d, sent: %d\n",imore,requestedRawData,isent); */

	  requestedRawData += imore;
	}
      else if (strcmp(word,"MAXSEND") == 0)
	{
	  fprintf(stderr,"DisFeedDAQ: Command from Dispatcher: %s\n",word);

	  word = strtok(NULL,":");

	  if (word)
	    maxSend = strtoul(word,NULL,0);

	  printf("DisFeedDAQ: New maxSend is %d\n",maxSend);
	}
      else if (strcmp(word,"ALLFINISHED") == 0)
	{
	  if (WaitForALLFINISHED)
	    {
	      fprintf(stderr,"DisFeedDAQ: Command ALLFINISHED from Dispatcher: %s\n",word);
	      //	    SendBeginTape(runNo);
	      OkToSendData = 1;
	      WaitForALLFINISHED = 0;
	    }
	  else
	    {
	      fprintf(stderr,"DisFeedDAQ: Unexpected command ALLFINISHED from Dispatcher was ignored.\n");
	    }
	}
      else if (strcmp(word,"QUIT") == 0)
	{
	  fprintf(stderr,"DisFeedDAQ: Command QUIT from Dispatcher: %s\n",word);
	  exit(0);
	}
      else if (strcmp(word,"EXIT") == 0)
	{
	  fprintf(stderr,"DisFeedDAQ: Command EXIT from Dispatcher: %s\n",word);
	  exit(0);
	}
      else
	{
	  fprintf(stderr,"DisFeedDAQ: Unexpected command from Dispatcher: [%s] was ignored.\n",msg);
	}
    }
  }
  else {
    cerr << "Received empty message from the Dispatcher" << endl;
  }


  if (local)
    free(local);
  return(ret);
}
extern "C" {
int ir_isnan_(float *x)
{
  return(0);
}

int ir_isinf_(float *x)
{
 
  return(0);
}
}


