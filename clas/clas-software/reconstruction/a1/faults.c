#include <stdio.h>
#include <stdlib.h> 
#include <signal.h>


/*------------- Utilities: Not currently implemented ------------*/


static int signalName(const char*signame)
{
  if (strcmp(signame,"SIGHUP")==0) 
    return SIGHUP;
  else if (strcmp(signame,"SIGINT")==0) 
    return SIGINT;
  else if (strcmp(signame,"SIGQUIT")==0) 
    return SIGQUIT;
  else if (strcmp(signame,"SIGKILL")==0) 
    return SIGKILL;
  else if (strcmp(signame,"SIGTERM")==0) 
    return SIGTERM;
  else if (strcmp(signame,"SIGUSR1")==0) 
    return SIGUSR1;
  else if (strcmp(signame,"SIGUSR2")==0) 
    return SIGUSR2;
  else if (strcmp(signame,"SIGSTOP")==0) 
    return SIGSTOP;
  else if (strcmp(signame,"SIGCONT")==0) 
    return SIGCONT;
  else
    return -1;
}

static void signalTerminate(int isig)
{
  fprintf(stderr,"signalTerminate: Caught signal %d, start the shutdown.\n",isig);

  signal(isig,signalTerminate);
}

static void signalStop(int isig)
{
  fprintf(stderr,"signalStop: Caught signal %d, suspend.\n",isig);
  signal(isig,signalStop);
}

static void signalContinue(int isig)
{
  fprintf(stderr,"signalContinue: Caught signal %d, unsuspend.\n",isig);
  signal(isig,signalContinue);
}

static int catchSignal(int s,const char*signame)
{
  int isig = signalName(signame);

  if (isig < 0) {
      fprintf(stderr,"catchSignal: Unexpected signal name: %s\n",signame);
      return -1;
    }
  
  fprintf(stderr,"catchSignal: code %2d, name: %s, isig: %2d\n",s,signame,isig);

  switch (s) {
    case 1: 
      signal(isig,signalTerminate); 
      break;
    case 2: 
      signal(isig,signalStop);      
      break;
    case 3: 
      signal(isig,signalContinue);  
      break;
    }
  
  return 0;
}

