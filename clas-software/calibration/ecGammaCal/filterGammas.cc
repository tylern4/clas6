#include <iostream>
#include <fstream>
#include <cstdio>
#include <string>

extern "C" {

#include <ntypes.h>
#include <bostypes.h>
#include <utility.h>
#include <itape.h>
#include <errno.h>
#include <unistd.h>
#include <ec.h>

}

#include <TVector3.h>
#include <clasEvent.h>
#include "TCalibGamma.h"

using namespace std;


// ------------------------------------------------
// ------------------ Drop flags  -----------------
// ------------------------------------------------
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

//#define CC_BANKS "CCRCCC01"
//#define SEB_BANKS "HEVTEVNTDCPBSCPBCCPBUNUSEVHBTRKSSTPBTGPBLCPB"
//#define TAGGER_BANKS "TAGRTAGIPSO "
//#define SC_BANKS "SC1 SCR SCRC"
//#define EC_BANKS "EC01ECHBECPIECPCECPBEC  EC1 "
//#define ST_BANKS "ST1 STR "
//#define REGION1_BANKS "RGLK"

// ------------------------------------------------
// ------------------  Prototypes  ----------------
// ------------------------------------------------
void PrintUsage(const char *szProcessName);
int StartRun(int nRunNo);
int  DropList(int DropFlag);
extern "C" {
  void bnames_(int *);
}
void remakeNeutrals(int,double);
int ProcessEvent (double dMinGammaEnergy, int nPartGroupIndex);


// ------------------------------------------------
// ------------------  Utilities   ----------------
// ------------------------------------------------
int StartRun(int nRunNo)
{
  int static nCurrentRun = -1;
  if (nCurrentRun != nRunNo) {
     vertex_brun(nRunNo);
     nCurrentRun = nRunNo;
  }

  return (0);
}


int DropList(int DropFlag)
{

  /* Mask off banks according to DropFlag*/

  if (DropFlag & DROP_RAW) bankList(&bcs_, (char*)&"E-", (char*)&"R");
  if (DropFlag & DROP_DC0) bankList(&bcs_, (char*)&"E-", (char*)&"DC0 (char*)&");
  if (DropFlag & DROP_DC1) bankList(&bcs_, (char*)&"E-", (char*)&"DC1 (char*)&");
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

}


void PrintUsage(const char* szProcessName)
{
  cerr << "\nUsage: " << szProcessName << " <options>\n";
  cerr << "\tSelect events that will be used for EC calibration\n";
  cerr << "\toptions are:\n";
  cerr << "\t-f<filename>\tname of stub file containing list of files to process.\n";
  cerr << "\t-d<dir>\tdirectory name for BOS filterde files (default is \"Skimmed\").\n";
  cerr << "\t-D#\tDrop flag (default: 0x1601E)\n";
  cerr << "\t-e#\tphoton minimum energy (in GeV) for calibration (default is 0.1).\n";
  cerr << "\t-g#\tPART bank group number (default is 1).\n";
  cerr << "\t-M#\tmaximum # of events to process in a file (default is 0, process all events)\n";
  cerr << "\t-i\tquiet mode (no counter).\n";
  cerr << "\t-h\tprint the above" << endl;

  exit(0);
}


/*******************************************************
 *******************************************************
Name: remakeNeutrals
Date: 05/01/2001
Author: Matthieu Guillo

Description:
This function rebuild neutrals (photons, neutrons) from PART bank using vertex position, hit position in the calorimeter. Beta is left unchanged since calibration is not done at that time.

Input:
  . PART bank number (varible nPartGroupIndex)
Output:
  . none, modifies PART id, vertex position and 4-momentum for neutrals.
 *******************************************************
*******************************************************/
void remakeNeutrals(int nPartGroupIndex)
{

  clasPART_t* PART = (clasPART_t *) getGroup(&bcs_, "PART", nPartGroupIndex); // PART ID bank
  clasMVRT_t* MVRT = (clasMVRT_t *) getBank(&bcs_, "MVRT");  // vertex Result bank
  clasTBID_t* TBID = (clasTBID_t *) getBank(&bcs_, "TBID");  // Time Based particle ID bank
  clasECHB_t* ECHB = (clasECHB_t *) getBank(&bcs_, "ECHB");  // Forward calorimeter result bank

  if (TBID && MVRT && PART && ECHB) {
    vector3_t vertex = MVRT->mvrt[0].vert; // vertex position (c structrure)
    for (int i = 0; i < PART->bank.nrow; i++) {
      // loop over all particle, looking for neutral
      part_t* part = &PART->part[i];
      tbid_t* tbid = &TBID->tbid[part->trkid - 1];
      echb_t* echb = &ECHB->echb[tbid->ec_id - 1]; // get corresponding echb bank for neutral
      switch ((Particle_t) part->pid) {
      case Gamma:
      case Neutron:
    if (tbid->ec_id) {
      TVector3 v3R(echb->x_hit - vertex.x, echb->y_hit - vertex.y, echb->z_hit - vertex.z); // to get direction of momentum
      float fEnergy = EC_MAGIC_NUMBER * echb->e__hit; // gets energy from energy deposit in EC
      TVector3  v3Momentum = fEnergy * v3R.Unit();
      // fill part bank with new data
      part->pid = Gamma;
      part->p.t = fEnergy;
      part->p.space.x = v3Momentum.X();
      part->p.space.y = v3Momentum.Y();
      part->p.space.z = v3Momentum.Z();
      part->vert.x = vertex.x;
      part->vert.y = vertex.y;
      part->vert.z = vertex.z;
    }
    break;
      default:
    break;
      }
    }
  }
}


/*******************************************************
 *******************************************************
Name: ProcessEvent
Date: 05/01/2001
Author: Matthieu Guillo

Description:
Tests if the event can be used or not for calibration. A good event for calibration should contains one proton, at least one gamma that can be used for calibration and at least 2 charged tracks (for better vertex reconstruction). There is also a test on the vertex time for pathologic events.

Inputs:
  . photon minimum energy (in GeV) for use for calibration (variable dMinGammaEnergy)
  . group number of PART bank (variable nPartGroupIndex).
Output:
  . true of false according if the event can be used for calibration.
 *******************************************************
*******************************************************/
int ProcessEvent (double dMinGammaEnergy, int nPartGroupIndex)
{

  clasPART_t *PART = (clasPART_t*) getGroup(&bcs_, "PART", nPartGroupIndex); // get PART bank
  int nProtons = 0, nGoodGammas = 0, nChargedTracks = 0;
  double dVertexTime = 0.;
  TCalibGamma gam;

  if (PART) {
    clasTBID_t *TBID = (clasTBID_t*) getBank(&bcs_, "TBID");
    if (TBID) {
      dVertexTime = TBID->tbid[0].vtime;  // gets vertex time from first track
    }
    for (int nPartIndex = 0; nPartIndex < PART->bank.nrow; nPartIndex++) {
      part_t* part = &PART->part[nPartIndex]; // point to next particle
      if (part->q) {
    nChargedTracks++;
      }
      switch ((Particle_t) part->pid) {
      case Proton:
    nProtons++;
    break;
      case Gamma:
    gam.Set(&bcs_, nPartIndex, dMinGammaEnergy, nPartGroupIndex);
    if (gam.IsGoodPhoton()) { // check is the photon is good for EC calibration
      nGoodGammas++;
    }
    break;
      default:
    break;
      }
    }
  }

  bool bGoodEvent = (nProtons == 1) && (nChargedTracks > 1) && (nGoodGammas);   // look for a proton, a good photon and at least 2 charged particles

  return(bGoodEvent? nGoodGammas : 0);

}


// ------------------------------------------------
// ------------------ Main program ----------------
// ------------------------------------------------
int main(int argc, char **argv)
{

  int nDispatch = 0;

  //
  char* szStubFilename = NULL;
  string stDirBosOutput = "Skimmed";  // directory to output filtered BOS bank
  double dMinGammaEnergy = 0.1; // 100 MeV minimum to get ride of the background.
  int nPartGroupIndex = 1;
  int nMaxEventsToRead = 0;   // if set to 0, process all events in the file
  bool bBatchMode = false;    // events counter is on by default

  // BOS variables
  char szFortranStyleOutput[300];  // some arcane fortran formatting string
  const int c_nOutputUnitNo = 9;   // Fortran output unit number

  /*
     drop some banks:
     LAC: 2000, DHCL: 10000, CC: 4000, TBLA: 10, HBLA: 8, DC0: 2, DC1:4
     total is: 0x1601E
  */
  unsigned long lDropFlag = 0x1601E;

  if (argc == 1){
    PrintUsage(argv[0]);
  }
  for (int i = 1; i < argc; i++) {
    char* argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {

      case 'f':
    if (*(++argptr)) {
      szStubFilename =  argptr;
    }
    break;

      case 'd':
    if(*(++argptr)) {
      stDirBosOutput = argptr;
    }
    break;

      case 'D':
    if (*(++argptr)) {
      lDropFlag = strtoul(++argptr,NULL,0);
    }
    break;

      case 'e':
    if (*(++argptr)) {
      dMinGammaEnergy = atof(argptr);
    }
    break;

      case 'g':
    if (*(++argptr)) {
      nPartGroupIndex = atoi(argptr);
    }
    break;

      case 'M':
    if (*(++argptr)) {
      nMaxEventsToRead = atoi(argptr);
    }
    break;

      case 'i':
    // batch mode (no counter)
    bBatchMode = true;
    break;

      case 'h':
    PrintUsage(argv[0]);
    break;

      default:
    cerr << "Unrecognized argument: " << argptr << endl;
    exit(0);
    break;

      }
    }
  }

  // Initialize BOS
  int c_nMaxBosBanks = 1000;
  bnames_(&c_nMaxBosBanks);
  initbos();
  configure_banks(stderr, 0);

  // try to open stub file
  ifstream fsStubFile(szStubFilename);
  if (!fsStubFile) {
    cerr << "Couldn't read stub file " << szStubFilename << "\n!";
    exit(8);
  }

  // iniializes different counters
  int nTotalEventsRead = 0, nTotalEventsProcessed = 0, nTotalEventsWritten = 0, nTotalGammasFound = 0;

  string stBosFilename;  // filename read from the stub file
  while (fsStubFile >> stBosFilename) {
    // we have a file to process

    // Open BOS file and initialize it
    int nDasphPosition = stBosFilename.find_last_of("/"); // finds position of last /
    if (nDasphPosition == string::npos) nDasphPosition = -1; // cases no / found
    string stBosOutputFilename = stDirBosOutput + "/" + stBosFilename.substr(nDasphPosition + 1, stBosFilename.size()); // create ouput file string
    unlink(stBosOutputFilename.c_str());  // delete previous BOS with same name
    sprintf(szFortranStyleOutput, "OPEN BOSOUTPUT UNIT=%d FILE=\"%s\" WRITE  STATUS=NEW RECL=3600", c_nOutputUnitNo, stBosOutputFilename.c_str());
    if (!fparm_c(szFortranStyleOutput)) {
      cerr << argv[0] << ": Unable to open file: " << stBosOutputFilename << " " <<  strerror(errno) << endl;
      continue;
    }
    cerr << "\nOutput file: " << stBosOutputFilename << endl;

    char *xxx = (char *) stBosFilename.c_str();

    clasEvent event(xxx, &bcs_, nPartGroupIndex, 0);
    cerr << "Initialize:\t" << stBosFilename << endl;
    nDispatch = isDispatcher(xxx);

    if (event.status()) {
      int nEventsRead = 0, nEventsProcessed = 0, nEventsWritten = 0, nGammasFound = 0, nStillEvents = 1; // initializes variables for new file
      while ((nMaxEventsToRead ? nEventsRead < nMaxEventsToRead : 1) && nStillEvents) {
    // process every event in the file
    nStillEvents = event.read(nPartGroupIndex);
    if (nStillEvents == DISIO_DATA) {
      nEventsRead++;
      if (!bBatchMode && ((nEventsRead % 100) == 0)){
        cerr << nEventsRead << "\r";
      }

      if (event.status()) {
        int nRunNo = event.run();
        ConfigEvent(nRunNo, 0);
        // Initialize the TOF geometry. This is needed regardless of whether you remake the SC reconstruction banks or not. However,for the regeneration case, this is done in ConfigEvent.
        StartRun(nRunNo);

        // remake vertex
        bankList(&bcs_,(char*)&"E+",(char*)&"MVRT");
        make_mvrt();
        // remake neutrals
        remakeNeutrals(nPartGroupIndex);
        int nGoodPhotons = ProcessEvent(dMinGammaEnergy, nPartGroupIndex); // good photons in the event
        nGammasFound += nGoodPhotons; // good photons in the file
        if (nGoodPhotons) {
          if (lDropFlag) {
        DropList(lDropFlag);
          }
          putBOS(&bcs_, c_nOutputUnitNo, (char*)&"E");
          nEventsWritten++;
        }
        dropAllBanks(&bcs_, (char*)&"E");
        cleanBanks(&bcs_);
        nEventsProcessed++;
      }
    }
    else if (nStillEvents == DISIO_COMMAND) {
      cerr << "Message from Giant Head: " << getBuffer() << endl;;
    }
      }

      // sum over all files
      nTotalEventsRead += nEventsRead;
      nTotalEventsProcessed  += nEventsProcessed;
      nTotalGammasFound += nGammasFound;
      nTotalEventsWritten += nEventsWritten;

      cerr << "\nnumber of events read in " << stBosFilename << ":\t" << nEventsRead << endl;
      cerr << "number of events processed in " << stBosFilename << ":\t" << nEventsProcessed << endl;
      cerr << "number of events written in " << stBosOutputFilename << ":\t" << nEventsWritten << endl;
      cerr << "number of good gammas found in " << stBosFilename << ":\t" << nGammasFound << endl;

    }
    else {
      cerr << "Unable to open " << stBosFilename << endl;
    }

    // close BOS output file
    putBOS(&bcs_, c_nOutputUnitNo, (char*)&"0");
    sprintf(szFortranStyleOutput, (char*)&"CLOSE BOSOUTPUT UNIT=%d", c_nOutputUnitNo);
    fparm_c(szFortranStyleOutput);
  }

  if (nDispatch) {
    disIO_command((char*)&"FINISHED");
  }

  cerr << "\nTotal number of events read:\t" << nTotalEventsRead << endl;
  cerr << "Total number of events processed:\t" << nTotalEventsProcessed << endl;
  cerr << "Total number of events written:\t" << nTotalEventsWritten << endl;
  cerr << "Total number of gammas found for calibration:\t" << nTotalGammasFound << endl;

  fsStubFile.close();

  return (0);
}






