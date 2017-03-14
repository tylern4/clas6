#include <cstdlib>
#include <iostream>
#include <fstream>
#include <cstdio>
#include "TCalibEC.h"

extern "C" {
#include <map_manager.h>
}

using namespace std;

// ------------------------------------------------
// ------------------  Prototypes  ----------------
// ------------------------------------------------
void PrintUsage(const char *szProcessName);


// ------------------------------------------------
// ------------------  Utilities   ----------------
// ------------------------------------------------
void PrintUsage(const char *szProcessName)
{
  cerr << "\nUsage: " << szProcessName << " -c<file> [-m<mapfile>] -t<run#>\n";
  cerr << "\tReads EC calibration constants from a file and put them into a EC map OR remove entries from a EC map\n";
  cerr << "Options are:\n";
  cerr << "\t-r\t remove entries mode (exclusive to -c flag).\n";
  cerr << "\t-c<file>\tcalibration constants file (default is calibconsts.dat).\n";
  cerr << "\t[-m<mapfile>]\tEC map file. (by default, looks for EC_CALIB.map in current directory).\n";
  cerr << "\t-t<time/run number>\ttime/run number.\n";
  cerr << "\nExamples:\n"
       << "write constants: " << szProcessName << " -cecConstants -mMaps/EC_CALIB.map -t19543\n";
  cerr << "delete entries form a run: " << szProcessName << " -r -mMaps/EC_CALIB.map -t19543\n";
  exit(0);

}


// ------------------------------------------------
// ------------------ Main program ----------------
// ------------------------------------------------
int main(int argc, char** argv)
{

  int nTime = -1;
  char *szECMap = (char*)&"EC_CALIB.map";                  // EC map file
  char* szCalibConstsIn = (char*)&"calibconsts.dat"; // calibration constants input
  bool bDeleteEntryMode = false;

  if (argc == 1){
    PrintUsage(argv[0]);
  }
  for(int i = 1; i < argc; ++i){
    char* argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch(*argptr) {

      case 'r':
       bDeleteEntryMode = true;
       break;

      case 'c':
    if (*(++argptr)) {
      szCalibConstsIn = argptr;
    }
    break;

      case 'm':
    if (*(++argptr)) {
      szECMap = argptr;
    }
    break;

      case 't':
    if (*(++argptr)) {
      nTime = atoi(argptr);
    }
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

  ifstream fsCalibConstsIn(szCalibConstsIn);
  if (!fsCalibConstsIn && !bDeleteEntryMode) {
    cerr << "Couldn't read calibration constants file " << szCalibConstsIn << "!\n";
    exit(8);
  }

  if (nTime == -1) {
    PrintUsage(argv[0]);
  }


  // specific to EC_CALIB map
  const int c_nItems = TCalibEC::m_c_nLayers * TCalibEC::m_c_nViews;
  char *aszItem[c_nItems] = {
        (char*)&"InnerU",
        (char*)&"InnerV",
        (char*)&"InnerW",
        (char*)&"OuterU",
        (char*)&"OuterV",
        (char*)&"OuterW"};
  const int c_nSubSystems = TCalibEC::m_c_nCalibConsts;
  char *aszSubSystem[c_nSubSystems] = {
        (char*)&"EC_To",
        (char*)&"EC_Tch",
        (char*)&"EC_Tadc",
        (char*)&"EC_dT1",
        (char*)&"EC_dT2"};

  if (bDeleteEntryMode) {
    for (int nSubSystem = 0; nSubSystem < c_nSubSystems;  nSubSystem++) { // parameters
      for (int nItem = 0; nItem < c_nItems; nItem++){ // layer and view
    map_rem_arr(szECMap, aszSubSystem[nSubSystem], aszItem[nItem], nTime);
      }
    }

  }
  else {
    const int c_nMapEntries = TCalibEC::m_c_nSectors * TCalibEC::m_c_nTubes; // number of entries in the Map
    float afMapValues[c_nSubSystems][c_nMapEntries];

    for (int nItem = 0; nItem < c_nItems; nItem++) { // layer and view
      for (int nMapEntry = 0; nMapEntry <  c_nMapEntries; nMapEntry++) {
    int nCounts;
    fsCalibConstsIn >> nCounts; // reads and discards the number of hits
    for (int nSubSystem = 0; nSubSystem < c_nSubSystems;  nSubSystem++) { // parameters
      // read from file and fill afMapValues array
      double dCalibConst, dErrorCalibConst;
      fsCalibConstsIn >> dCalibConst >> dErrorCalibConst;
      if (dCalibConst == TCalibEC::m_c_dError) {
        dCalibConst  = 0.0; // different convention, for the map a 0 value is an error
      }
      afMapValues[nSubSystem][nMapEntry] = static_cast<float>(dCalibConst);
    }
      }

      float afValues[c_nMapEntries];
      for (int nSubSystem = 0; nSubSystem < c_nSubSystems;  nSubSystem++) {
    for (int nMapEntry = 0; nMapEntry < c_nMapEntries; nMapEntry++) {
      afValues[nMapEntry] = afMapValues[nSubSystem][nMapEntry];
    }
    map_put_float(szECMap, aszSubSystem[nSubSystem], aszItem[nItem], c_nMapEntries, afValues, nTime);
      }
    }
  }

  return (0);
}






