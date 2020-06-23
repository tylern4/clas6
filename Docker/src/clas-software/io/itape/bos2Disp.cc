/*
 * bos2Disp.cc   
 * Purpose: Send BOS data from a file to the Dispatcher
 */


extern "C" {
  
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h> 
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

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

  int SetVerbose(int);
void nGenBanks(itape_header_t *buffer,int bufsize,int *jw,char *list);
}

#include <iostream.h>


#define BUFSIZE 900000

int eventsTotal;
int eventsSent;
int eventsSkipped;
int nevents = 0;

static int requestedRawData = 0;

static const void* dataPtr = NULL;
static const void* dataNext = NULL;

  /* ----------- Function prototypes ---------------- */
int GetDispatcherCommand(itape_header_t *);
int SendData2Dispatcher(itape_header_t *buffer);
int ProcessEvent(unsigned int);
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
  cerr << "\t-M[#]      \tProcess only # number of events\n";
  cerr << "\t-Daddress  \tDispatcher address" << endl;
  cerr << "\t-v         \tverbose mode" << endl;
  cerr << "\t-V         \tVerbose mode" << endl;
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
  unsigned int triggerMask = 0xffff;

  char*server = getenv("CLAS_DISPATCHER");

  if (server==NULL) server = "localhost:10099";

  setbuf(stderr,NULL);
  setbuf(stdout,NULL);
 
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
      case 'D':
	server = ++argptr;
	break;
      case 'v':
	verbose = 1;
	SetVerbose(1);
	break; 
      case 'V':
	Verbose = 1;
	SetVerbose(1);
	break;
      case 'h':
	PrintUsage(argv[0]);
	break;      
      case 'M':
	max = atoi(++argptr);
	break;
      case 't':
	triggerMask = strtoul(++argptr,NULL,0);
	break;
      default:
	fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
	PrintUsage(argv[0]);
	break;
      }
    }
  }
  
  /*initialize bos */
  initbos();
 
  if (server==NULL) server = "localhost:10098";

  setbuf(stderr,NULL);
  setbuf(stdout,NULL);

  if (!server)
    {
      fprintf(stderr,"No dispatcher connection specified, exiting...\n");
      exit(1);
    }
  /* connect to the Dispatcher */

  ret = disIO_connect(server,-1);
  if (ret) {
      fprintf(stderr,"Cannot connect to the Dispatcher at [%s].\n",server);
      exit(1);
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
	while ((max ? Nevents < max : 1) && ProcessEvent(triggerMask) ) {
	  Nevents++;
	  
	  if (verbose) {
	    if (! (Nevents % 100))
	      cerr << Nevents << "\t\r" << flush;
	  }

	  dropAllBanks(&bcs_,"E");
	  cleanBanks(&bcs_);
	}
	cerr << "end loop" << endl;
      }
	fprintf(stderr,"#  of events processed: %d\n",Nevents);
	sprintf(mess,"CLOSE BOSINPUT", argptr);
	fparm_c(mess);
	SendEndTape();
	disIO_disconnect(); /* make sure we disconnect from the Dispatcher */
    }
    
  }

}

int SendData2Dispatcher(itape_header_t *buffer)
{ 


  int ret;
  int retval;
  /* initialize itape */

    ret = disIO_writeRAW(buffer,buffer->length + 4);
    retval = 1;
    
    return(retval);
}
  

int ProcessEvent(unsigned int triggerMask)
{  
  static itape_header_t *buffer = NULL;
  static int CurrentRun = 0;
  int Run;
  int ret = 0;
  int continueReading = 1;
  int retval = 1;

  cerr << "entering ProcessEvent " << endl;


  if (!buffer) {
    if(!(buffer = (itape_header_t *)malloc(BUFSIZE))) {
      fprintf(stderr,"Error on the memory allocation\n");
      fprintf(stderr,"look at code of file %s at line %d\n", __FILE__, __LINE__);
      exit(1);
    }
  }
  while (continueReading){
    if ( (retval = getBOS(&bcs_,1,"E"))) {
      clasHEAD_t *HEAD = (clasHEAD_t *) getBank(&bcs_,"HEAD");
      if (HEAD) {
	cerr << "Event:\t" << HEAD->head[0].nrun << "\t" << HEAD->head[0].nevent << " " << nevents++ << endl;

      }
      else {
	cerr << "Head bank not found" << endl;
      }
      continueReading = retval ? (!HEAD || !(triggerMask & HEAD->head[0].trigbits)) : 0;
      if (!continueReading) {
	ret = 1;
	data_newItape(buffer);
	if ( (Run = fillHeader(buffer)) != CurrentRun) {
	  SendBeginTape(CurrentRun = Run);
	  cerr << "Begin Run: " << Run << endl;
	}
	cerr << "Run: " << Run << endl;
	nGenBanks(buffer,BUFSIZE,bcs_.iw,"E");
	cerr << "Waiting for a message from the Giant Head..." << endl;
	cerr << "Dispatcher: " << (ret = GetDispatcherCommand(buffer)) << endl;
      }

      else {
	fprintf(stderr,"read again...\n");
      }
    }
    continueReading = 0;
  }
      
  cerr << "Process Event returning: " << ret << endl;
    
    return(ret);
}


    

void ctrlCHandle(int x)
{
  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  max = 1;
  fprintf(stderr,"\n\n\t\t\t***  INTERRUPTED!!!  ***\n\n");
}

extern "C" {



void SendBeginTape(int runNo)
{
  char msg[1024];

  sprintf(msg,"BEGINTAPE:%d",runNo);
  
  fprintf(stderr,"DisFeedDAQ: Sending BEGINTAPE:%d...\n",runNo);
  disIO_command(msg);
}

void SendEndTape(void)
{
  printf("DisFeedDAQ: Sending ENDTAPE...\n");
  disIO_command("ENDTAPE");
}


const itape_header_t *GetEvent(void)
{

      eventsSent ++;
      return GetEvent1();

}


const itape_header_t *GetEvent1(void)
{
  const itape_header_t *event;

 

  event = (itape_header_t *)data_getItape(dataPtr,&dataNext);

  dataPtr = dataNext;

  return event;
}

}

int GetDispatcherCommand(itape_header_t *buffer)
{
  int retval = 0;
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

  timeout.tv_sec  = 30;
  timeout.tv_usec = 0;

  if (OkToSendData && (requestedRawData > 0))
    {
      timeout.tv_sec  = 30;
      timeout.tv_usec = 0;
    }

  //  ret = select(maxfd + 1, &rset, &wset, NULL, &timeout);
  ret = select(maxfd + 1, &rset, NULL, NULL, &timeout);
  if (ret < 0)
    {
      fprintf(stderr,"DisFeedDAQ: Error: select() returned %d, errno: %d (%s)\n",ret,errno,strerror(errno));
      //	  exitFlag = 1;
      exit(0);
    }    

  cerr << "ret: " << ret << endl;

  if (ret) {

    /* ok, we got a command. Now parse it */
    static char *msg = NULL;
    static int msglen = 0;
    char *cmd0;
    char *word;
    int maybeUnexpected = 0;

    if (msg)
      free(msg);

    msg = NULL;

    ret = disIO_readRAW_alloc((void **) &msg,&msglen,0);

    switch (ret) {
    case DISIO_EOF:
      cerr << "End-of-File" << endl;
      retval = 0;
      break;
    case DISIO_COMMAND:
      retval = ret;
      //      cerr << "COMMAND FROM GIANT HEAD" << endl;


      if (msg) {

	word = strtok(msg,":");
	if (word) {
	  cerr << "COMMAND: " << word << endl;

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
	      retval = SendData2Dispatcher(buffer);
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
		  SendBeginTape(runNo);
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
	cerr << "Received NULL message from the Dispatcher" << endl;
      }
    }
  }
  else {
    cerr << "timeout" << endl;
    retval = 1;
  }
  return(retval);
}


/* end file */
