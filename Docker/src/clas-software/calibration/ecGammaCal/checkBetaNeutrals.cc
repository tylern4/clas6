#include <iostream>
#include <fstream>

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
#include <vertex.h>

}

#include <Vec.h>
#include <lorentz.h>
#include <pputil.h>
#include <clasEvent.h>

// ROOT headers

#include "TROOT.h"
#include "TFile.h"
#include "TH1.h"
#include "TNtuple.h"
#include "TCalibGamma.h"

/*bitwise drop flags*/
#define DROP_RAW BIT(0)
#define DROP_DC0 BIT(1)
#define DROP_DC1 BIT(2)
#define DROP_HBLA BIT(3)
#define DROP_TBLA BIT(4)
#define DROP_HBTB BIT(5)
#define DROP_SC BIT(6)
#define DROP_EC BIT(7)
#define DROP_HBID BIT(8)
#define DROP_CL01 BIT(9)
#define DROP_SEB BIT(10)
#define DROP_TBID BIT(11)
#define DROP_HDPL BIT(12)
#define DROP_LAC BIT(13)
#define DROP_CC BIT(14)
#define DROP_ST BIT(15)
#define DROP_DHCL BIT(16)
#define DROP_TAGR BIT(17)
#define CC_BANKS "CCRCCC01"
#define SEB_BANKS "HEVTEVNTDCPBSCPBCCPBUNUSEVHBTRKSSTPBTGPBLCPB"
#define SC_BANKS "SC1 SCR SCRC"
#define EC_BANKS "EC01ECHBECPIECPCECPBEC  EC1 "
#define ST_BANKS "ST1 STR "
#define REGION1_BANKS "RGLK"

#define BUFSIZE 200000

extern "C" {
void bnames_(int *);
int SetVerbose(int);
}

int StartRun(int);
int ProcessEvent(clasEvent &event, TNtuple& nt, TH1F& h1);
int DropList(int DropFlag);
void PrintUsage(char *processName);

int CurrentRun = 0;
int CurrentEvent = 0;
int partbank0 = 1;
int partbank = 1;

// ROOT variables

TROOT check("check","Beta for neutrals"); // initiate Root
TFile* fileROOT; // file to write Ntuples and histos

int StartRun(int runNo)
{
  int static CurrentRun = -1;
  if (CurrentRun != runNo) {
     vertex_brun(runNo);
     CurrentRun = runNo;
  }
  return (0);
}


int DropList(int DropFlag)
{
  /* Mask off banks according to DropFlag*/

  if (DropFlag & DROP_RAW) bankList(&bcs_, (char*)&"E-", (char*)&"R");
  if (DropFlag & DROP_DC0) bankList(&bcs_, (char*)&"E-", (char*)&"DC0 ");
  if (DropFlag & DROP_DC1) bankList(&bcs_, (char*)&"E-", (char*)&"DC1 ");
  if (DropFlag & DROP_HBLA) bankList(&bcs_, (char*)&"E-", (char*)&"HBLA");
  if (DropFlag & DROP_TBLA) bankList(&bcs_, (char*)&"E-", (char*)&"TBLA");
  if (DropFlag & DROP_HBTB) bankList(&bcs_, (char*)&"E-", (char*)&"HBTB");
  if (DropFlag & DROP_SC) bankList(&bcs_, (char*)&"E-", (char*)&SC_BANKS);
  if (DropFlag & DROP_EC) bankList(&bcs_, (char*)&"E-", (char*)&EC_BANKS);
  if (DropFlag & DROP_HBID) bankList(&bcs_, (char*)&"E-", (char*)&"HBID");
  if (DropFlag & DROP_CL01) bankList(&bcs_, (char*)&"E-", (char*)&"CL01");
  if (DropFlag & DROP_SEB) bankList(&bcs_, (char*)&"E-", (char*)&SEB_BANKS);
  if (DropFlag & DROP_TBID) bankList(&bcs_, (char*)&"E-", (char*)&"TBIDPARTTBERTBTR");
  if (DropFlag & DROP_HDPL) bankList(&bcs_, (char*)&"E-", (char*)&"HDPL");
  if (DropFlag & DROP_LAC) bankList(&bcs_, (char*)&"E-", (char*)&"EC1R");
  if (DropFlag & DROP_CC) bankList(&bcs_, (char*)&"E-", (char*)&CC_BANKS);
  if (DropFlag & DROP_ST) bankList(&bcs_, (char*)&"E-", (char*)&ST_BANKS);
  if (DropFlag & DROP_DHCL) bankList(&bcs_, (char*)&"E-", (char*)&"DHCL");
  if (DropFlag & DROP_TAGR) bankList(&bcs_, (char*)&"E-", (char*)&TAGGER_BANKS);

  return(0);
}


void PrintUsage(char *processName)
{
  cerr << processName << " <options> <filename>\n";
  cerr << "\toptions are:\n";
  cerr << "\t-t#\ttrigger mask.\n";
  cerr << "\t-D#\tDrop flag.\n";
  cerr << "\t-o<filename>\tROOT output file.\n";
  cerr << "\t-M#\tprocess maximum # of events.\n";
  cerr << "\t-i\tQuiet mode (no counter).\n";
  cerr << "\t-h\tprint the above" << endl;
}



// ------------------------------------------------
// ------------------ Main program ----------------
// ---------- To be modified to fit need ----------
// ------------------------------------------------

int main(int argc,char **argv)
{

  int max = 0;
  int verbose = 0;
  int ret = 1;
  int Nevents = 0;
  int Nproc = 0;
  int Nwrite = 0;
  char *argptr;
  int Dispatch = 0;
  unsigned int triggerMask = 0;
  int writeFlag = 0;

  // bos stuff
  char *szRootFilename =(char*)& "test.root";
  int MaxBanks = 1000;
  char  out[300];
  char Elist[5 * MaxBanks];

  int DropFlag = 0x0;
  bool bBatchMode = false;    // events counter is on by default

  for (int i = 1; i < argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {

      case 't':
    triggerMask = strtoul(++argptr,NULL,0);
    break;

      case 'D':
    DropFlag = strtoul(++argptr,NULL,0);
    break;

      case 'o':
    if (argptr++) {
      szRootFilename = argptr;
    }
    break;

      case 'M':
    max = atoi(++argptr);
    break;

      case 'i':
        bBatchMode = true;
        break;

      case 'h':
    PrintUsage(argv[0]);
    exit(0);
    break;

      default:
    cerr << "Unrecognized argument: " << argptr << endl;;
    break;
      }
    }
  }

  // Initialize BOS
  bnames_(&MaxBanks);
  initbos();
  configure_banks(stderr,0);

  // configure ROOT
  fileROOT = TFile::Open(szRootFilename, "RECREATE");
  if (! fileROOT->IsOpen()) {
    cerr << "Couldn't open ROOT file " << szRootFilename << endl;
    exit(8);
  }

  TNtuple nt("nt","Gamma checking","id:beta:ecbeta:sec");
  TH1F h1("h1", "#beta for neutrals", 100, 0.7, 1.3);
  h1.GetXaxis()->SetTitle("#beta");
  h1.GetYaxis()->SetTitle("hits");
  h1.GetYaxis()->CenterTitle();

  // *********************
  // initialize ROOT stuff
  // *********************

  for (int i = 1; i < argc; ++i) {
    argptr = argv[i];
    // process all arguments on command line.
    if (*argptr != '-') {
      // we have a file to process
      clasEvent event(argptr, &bcs_, partbank0, 0);

      cerr << "initialize:\t" << argptr << endl;

      Dispatch = isDispatcher(argptr);
      if (event.status()) {
    ret = 1;
    while ((max ? Nevents < max : 1) && ret) {
      // process every event in the file
      ret = event.read(partbank0);
      if (ret == DISIO_DATA) {

        Nevents++;
        if (!bBatchMode && ((Nevents % 100) == 0)){
              cerr << Nevents << "\r";
            }

        if (event.status()) {
          if (triggerMask ? (triggerMask & event.trig()) : 1) {
        int runno = event.run();
        ConfigEvent(runno,0);
        /* Initialize the TOF geometry.  This is needed regardless of whether you remake the SC reconstruction banks or not.  However, for the regeneration case, this is done in ConfigEvent. */
        StartRun(runno);

        writeFlag = ProcessEvent(event, nt, h1) ;

        dropAllBanks(&bcs_,(char*)&"E");
        cleanBanks(&bcs_);
        Nproc++;
          }
        }
      }
      else if (ret == DISIO_COMMAND) {
        cerr << "Message from Giant Head: " << getBuffer() << endl;;
          }

    }
    cerr << "\nTotal number of itape records read:\t" << Nevents << endl;
    cerr << "Total number of events processed:\t" << Nproc << endl;
    cerr << "\tTotal number of records written:\t" << Nwrite << endl;
      }
      else {
    cerr << "Unable to open " << argptr << endl;
      }
    }
  }

  if (Dispatch) {
    disIO_command("FINISHED");
  }

  cerr << "\nTotal number of itape records:\t" << Nevents << endl;

  fileROOT->Write();
  fileROOT->Close();

  return (0);
}


// ------------------------------------------------
// ---------------- ProcessEvent ------------------
// ------------ Make all analysis here ------------
// ------------------------------------------------

int ProcessEvent(clasEvent &event, TNtuple& nt, TH1F& h1)
{
  int nReturnValue = 0;

  clasPART_t *PART = (clasPART_t*) getBank(&bcs_, "PART");
  clasTBID_t *TBID = (clasTBID_t*) getBank(&bcs_, "TBID");
  clasECHB_t *ECHB = (clasECHB_t*) getBank(&bcs_, "ECHB");

  if (PART && TBID && ECHB) {
    // analysis starts here
    int nHits = PART->bank.nrow;
    for (int i = 0; i < nHits; i++) {
      part_t* part = &PART->part[i];
      tbid_t* tbid = &TBID->tbid[part->trkid - 1];

      TCalibGamma gam;
      switch ((Particle_t) part->pid){
      case Neutron:
      case Gamma:
    h1.Fill(tbid->ec_beta);
    nt.Fill(part->pid, tbid->beta, tbid->ec_beta, tbid->sec);
    break;
      default:
    break;
      }
    }
  }

  return(nReturnValue);
}













