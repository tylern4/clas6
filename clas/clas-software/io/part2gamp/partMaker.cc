/*
 * partMaker.cc
 *
 */


extern "C" {

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
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

#include <iostream>
#include <Vec.h>
#include <lorentz.h>
#include <pputil.h>

using namespace std;


#define BUFSIZE 200000


  /* ----------- Function prototypes ---------------- */
int GetDispatcherCommand();
int ProcessEvent(unsigned int,int,int);
int addBeam(int srcPart,int tgtPart);
int GetDat(FILE *finput,char *,int);
void ctrlCHandle(int);
void PrintUsage(char *processName);
int StartRun(int);
int EndRun(int);
int dispatcherReconnect(const char*host,int pipelinePrefill);
int printGeneralLabels(char *);
extern "C" {
void bnames_(int *);
int SetVerbose(int);
int initDisplay(char *,int);
int getData(BOSbank *,char *);

  /* declare the bos common */
BOSbank bcs_;
BOSbank wcs_;

       }


static int requestedRawData = 0;
int CurrentRun = 0;

void PrintUsage(char *processName)
{
  cerr << processName << endl;
  cerr << "\t-oFileName\tWrite bos output to FileName" << endl;
  cerr << "\t-v\t\tVerbose mode" << endl;
  cerr << "\t-s\tSilent mode: no printout" << endl;
  cerr << "\t-t#\ttrigger mask" << endl;
  cerr << "\t-M#\tprocess # events" << endl;
  cerr << "\t-p#\tUse part bank # as source (default = 0)" << endl;
  cerr << "\t-P#\tUse part bank # as target (default = 99)" << endl;
  cerr << "\t-h\tprint the above" << endl;
}


int SelectEvent()
{
  int ret = 1;
  return(ret);
}



main(int argc,char **argv)
{
  FILE *fp = NULL;
  int max = 0;

  int verbose = 0;

  int ret = 1;

  int Nevents = 0;
  int nfile = 0;
  int Nwrite = 0;
  time_t time1;
  float rate;
  int i;
  char *argptr;
  char *word;
  int Dispatch;
  unsigned int triggerMask = 0;
  int partbank = 0;
  int tgtBank = 99;

  int modeN[10];
  int nmode = 0;

  int silentMode = 0;

  // Dispatcher
  char *server = NULL;
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

      case 'o':
    if(*(++argptr))
      BOSoutfile = argptr;


    if(OutputUnitNo){
      fprintf(stderr,"Output file: %s\n",BOSoutfile);
      unlink(BOSoutfile);
      sprintf(out, "OPEN BOSOUTPUT UNIT=9 FILE=\"%s\" WRITE  STATUS=NEW RECL=3600", BOSoutfile);
      if (!fparm_c(out)) {
        fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0], out, strerror(errno));
        exit(1);
      }
    }



    break;
      case 's':
    silentMode = 1;
    break;
      case 'P':
    tgtBank = atoi(++argptr);
    break;
      case 'v':
    verbose = 1;
    SetVerbose(verbose);
    break;
      case 'p':
    partbank = atoi(++argptr);
    break;
      case 'M':
    max = atoi(++argptr);
    break;

      case 't':
    triggerMask = strtoul(++argptr,NULL,0);
    break;


      case 'h':
    PrintUsage(argv[0]);
    exit(0);
    break;

      default:
    fprintf(stderr,"Unrecognized argument %s\n",argptr);
    break;

      }

    }
  }

  for (i = 1; i < argc; ++i) {
    argptr = argv[i];
    if (*argptr != '-') {

      cerr << "initialize:\t" << argptr << endl;

      Dispatch = isDispatcher(argptr);
      if (initDisplay(argptr,10)) {
      ret = 1;

      // Initialize BOS
      bnames_(&MaxBanks);
      initbos();
      configure_banks(stderr,0);

      while ((max ? Nevents < max : 1) && ret) {
    ret = getData(&bcs_,"E");
    if (ret == DISIO_DATA) {
      if (!silentMode && SelectEvent())
        ProcessEvent(triggerMask,partbank,tgtBank);
      if (BOSoutfile && SelectEvent()) {
        putBOS(&bcs_,9,"E");
        Nwrite++;
      }
      dropAllBanks(&bcs_,"E");
      cleanBanks(&bcs_);
      Nevents++;
    }
    else if (ret == DISIO_COMMAND) {

      fprintf(stderr,"Message from Giant Head: %s\n",getBuffer());

    }

      }
      }
      else {
    cerr << "Unable to open " << argptr << endl;
      }
    }


    fprintf(stderr,"\nTotal number of itape records:\t%d\n",Nevents);
    fprintf(stderr,"\tTotal number of records written:\t%d\n",Nwrite);
  }
  if (Dispatch)
    disIO_command("FINISHED");
  fprintf(stderr,"\nTotal number of itape records: %d\n",Nevents);
  sprintf(out, "CLOSE BOSOUTPUT UNIT=9");
  fparm_c(out);

}

int ProcessEvent(unsigned int triggermask,int partbank,int tgtPart)
{
  addBeam(partbank,tgtPart);
}
int addBeam(int srcPart,int tgtPart)
{
  clasTAGR_t *TAGR = (clasTAGR_t *)getBank(&bcs_,"TAGR");
  clasPART_t *PART = (clasPART_t *)getGroup(&bcs_,"PART",srcPart);
  clasTBID_t *TBID  =  (clasTBID_t *)getBank(&bcs_,"TBID");
  clasPART_t *NP;
  int nPart;
  float beamp;
  int genBeam  = 0;

  if (PART) {
    nPart = PART->bank.nrow;
    if (TBID && TAGR) {
      beamp = get_photon_energy(TAGR,(clasBID_t *)TBID);
      genBeam = 1;
    }
    else {
      nPart--;
    }


    if (NP =(clasPART_t *)makeBank(&bcs_,"PART",tgtPart,sizeof(part_t)/sizeof(uint16),nPart+1)) {
      int ipart = 0;
      if (genBeam) {
    NP->part[ipart].pid = Gamma;
    NP->part[ipart].vert.x = NP->part[ipart].vert.y = 0.0;
    NP->part[ipart].vert.z = beamp;
    NP->part[ipart].q = 0;
    NP->part[ipart].trkid = -1;
    NP->part[ipart].qpid = -1;
    NP->part[ipart].qtrk = -1;
    NP->part[ipart++].flags = PART_BEAM;
      }

      for (int i=0; i< PART->bank.nrow; i++){
    NP->part[ipart++] = PART->part[i];
      }
    }
  }
}

