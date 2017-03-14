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

}

#include <clasEvent.h>
#include <TCalibECReject.h>

using namespace std;


// ------------------------------------------------
// ------------------  Prototypes  ----------------
// ------------------------------------------------
int StartRun(int nRunNo);
void PrintUsage(const char *processName);
extern "C" {
  void bnames_(int *);
}


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


void PrintUsage(const char *szProcessName)
{
  cerr << "\nUsage: " << szProcessName << " <options>\n";
  cerr << "\tFlags for rejection gammas that give a bad fitting.\n";
  cerr << "\toptions are:\n";

  cerr << "\t-f<filename>\tname of stub file containing list of files to process.\n";
  cerr << "\t-c<filename>\tcalibration constants input file (default is calibconsts.dat).\n";
  cerr << "\t-m<filename>\tfit constants input file (default is fitconsts.dat).\n";
  cerr << "\t-a#\tMinimum ADC value in a tube for calibration (default is 100.).\n";
  cerr << "\t-d<dir>\tdirectory name for BOS output file (default is \"Rejected\").\n";
  cerr << "\t-n#\tnumber of sigmas from centroid for rejecting gammas (default is 2).\n";
  cerr << "\t-e#\tphoton minimum energy (in GeV) for calibration (default is 0.1).\n";
  cerr << "\t-g#\tPART bank group number (default is 1).\n";
  cerr << "\t-M#\tmaximum # of events to process in a file (default is 0, process all events)\n";
  cerr << "\t-i\tquiet mode (no counter).\n";
  cerr << "\t-h\tprint the above" << endl;

  exit(0);
}



// ------------------------------------------------
// ------------------ Main program ----------------
// ------------------------------------------------
int main(int argc, char **argv)
{

  int nDispatch = 0;

  //
  char* szStubFilename = NULL;
  char* szCalibConstsFilename = (char*)&"calibconsts.dat"; // calibration constants input
  char* szFitConstsFilename = (char*)&"fitconsts.dat";     // fit constants input.
  string stDirBosOutput = (char*)&"Rejected";  // directory to write new BOS file (with flags gammas)
  int  nNumberSigmas = 2;
  double dMinGammaEnergy = 0.1; // 100 MeV minimum to get ride of the background.
  double dMinAdcValue = 100.;
  int nPartGroupIndex = 1;
  int nMaxEventsToRead = 0;   // if set to 0, process all events in the file
  bool bBatchMode = false;    // events counter is on by default

  // BOS variables
  char szFortranStyleOutput[300];  // some arcane fortran formatting string
  const int c_nOutputUnitNo = 9;   // Fortran output unit number

  if (argc == 1){
    PrintUsage(argv[0]);
    exit(0);
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

      case 'c':
    if (*(++argptr)) {
      szCalibConstsFilename =  argptr;
    }
    break;

      case 'm':
    if (*(++argptr)) {
      szFitConstsFilename = argptr;
    }
    break;

      case 'a':
    if (*(++argptr)) {
      dMinAdcValue = atof(argptr);
    }
    break;

      case 'd':
    if(*(++argptr)) {
      stDirBosOutput = argptr;
    }
    break;

      case 'n':
    if (*(++argptr)) {
      nNumberSigmas = atoi(argptr);
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
    cerr << "Couldn't read stub file " << szStubFilename << "!\n";
    exit(8);
  }

  TCalibECReject rejectEC(szCalibConstsFilename, szFitConstsFilename, dMinAdcValue, nNumberSigmas); // initializes object
  // iniializes different counters
  int nTotalEventsRead = 0, nTotalEventsProcessed = 0, nTotalGammasFound = 0, nTotalEventsWritten = 0;

  string stBosFilename; // filename read from the stub file
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
    char *filein = (char *)stBosFilename.c_str();
    clasEvent event(filein, &bcs_, nPartGroupIndex, 0);
    cerr << "Initialize:\t" << stBosFilename << endl;
    nDispatch = isDispatcher(filein);

    if (event.status()) {
      int nEventsRead = 0, nEventsProcessed = 0, nGammasFound = 0, nEventsWritten = 0, nStillEvents = 1; // initializes variables for new file
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
        /* Initialize the TOF geometry.  This is needed regardless of whether you remake the SC reconstruction banks or not.  However, for the regeneration case, this is done in ConfigEvent. */
        StartRun(nRunNo);
        nGammasFound += rejectEC.FindGammas(&bcs_, dMinGammaEnergy, nPartGroupIndex);  // finds gammas in the event and reject them if they are not good for the fit.

        // write event and clean BOS array
        nEventsWritten++;
        putBOS(&bcs_, c_nOutputUnitNo, (char*)&"E");
        dropAllBanks(&bcs_, (char*)&"E");
        cleanBanks(&bcs_);
        nEventsProcessed++;
      }
    }
    else if (nStillEvents == DISIO_COMMAND) {
      cerr << "Message from Giant Head: " << getBuffer() << endl;
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
    sprintf(szFortranStyleOutput, "CLOSE BOSOUTPUT UNIT=%d", c_nOutputUnitNo);
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






