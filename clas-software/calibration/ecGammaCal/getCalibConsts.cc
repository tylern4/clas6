#include <cstdlib>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include "TCalibEC.h"

using namespace std;

// ------------------------------------------------
// ------------------  Prototypes  ----------------
// ------------------------------------------------
void PrintUsage(const char *processName);


// ------------------------------------------------
// ------------------  Utilities   ----------------
// ------------------------------------------------
void PrintUsage(const char *szProcessName)
{
  cerr << "\nUsage: " << szProcessName << " -ofilename file1 ... filen\n";
  cerr << "\tFrom a set of calibration constants files, finds the \"best\" values and writes out the result.\n";
  cerr << "\toptions are:\n";

  cerr << "\t-o<filename>\toutput file containing best calibration constants (default is calibconsts.dat).\n";
  cerr << "\tfile1...filen\t set of calibration constants files.\n";
  cerr << "\t-h\tprint the above" << endl;
  cerr << "\nExample: " << szProcessName << " -ofinalcalibration.dat calibconsts*.dat\n";

  exit(0);
}


// ------------------------------------------------
// ------------------ Main program ----------------
// ------------------------------------------------
int main(int argc, char** argv)
{
  char* szCalibConstsOut = (char*)&"calibconsts.dat";

  // process command line
  if (argc == 1){
    PrintUsage(argv[0]);
  }
  for (int i = 1; i < argc; i++) {
    char* argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {
      case 'o':
    if (*(++argptr)) {
      szCalibConstsOut = argptr;
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

  // open output calibration file
  ofstream fsCalibConstsOut(szCalibConstsOut);
  if (!fsCalibConstsOut) {
    cerr << "Couldn't create file szCalibConstsOut !\n";
    exit(8);
  }

  // find all files to process
  vector<ifstream*> fsCalibFiles;
  for (int i = 1; i < argc; i++) {
    char* argptr = argv[i];
    if (*argptr != '-') {
      char* szCalibFilename = argv[i];
      ifstream* p_fsFile = new ifstream(szCalibFilename); // try to open the file
      if (p_fsFile->good()) {
    // could successfully open file
    fsCalibFiles.push_back(p_fsFile);
      }
      else {
    cerr << "Couldn't open " << szCalibFilename << endl;
      }
    }
  }
  int nNumberOfFiles = fsCalibFiles.size();

  // values to be saved
  int nMinCounts;
  double adGoodCalibConsts[TCalibEC::m_c_nCalibConsts], adGoodErrorCalibConsts[TCalibEC::m_c_nCalibConsts];
  // values read
  int nCounts;
  double adCalibConsts[TCalibEC::m_c_nCalibConsts], adErrorCalibConsts[TCalibEC::m_c_nCalibConsts];

  // for each tube, find the best calibration constants
  for (int nTubeIndex = 0; nTubeIndex < TCalibEC::m_c_nTotalTubes; nTubeIndex++) {
    // initialisation
    nMinCounts = -1;
    for (int nParam = 0; nParam < TCalibEC::m_c_nCalibConsts; nParam++) {
      adGoodCalibConsts[nParam] = adGoodErrorCalibConsts[nParam] = TCalibEC::m_c_dError;
    }

    // now process all files to find best calibration constants
    for (int nFile = 0; nFile < nNumberOfFiles; nFile++) {
      // reads line
      *(fsCalibFiles[nFile]) >> nCounts;
      for (int nParam = 0; nParam < TCalibEC::m_c_nCalibConsts; nParam++) {
    *(fsCalibFiles[nFile]) >> adCalibConsts[nParam] >> adErrorCalibConsts[nParam];
      }

      // is it the first file?
      if (nMinCounts == -1) {
    nMinCounts = nCounts;
      }

      if ((adCalibConsts[0] != TCalibEC::m_c_dError) && (nCounts <= nMinCounts)) {
    // found a file with supposely better calibration constants
    nMinCounts = nCounts;
    for (int nParam = 0; nParam < TCalibEC::m_c_nCalibConsts; nParam++) {
      adGoodCalibConsts[nParam] = adCalibConsts[nParam];
      adGoodErrorCalibConsts[nParam] = adErrorCalibConsts[nParam];
    }
      }
    }

    // write the good calibration constants out
    fsCalibConstsOut << nMinCounts;
    for (int nParam = 0; nParam < TCalibEC::m_c_nCalibConsts; nParam++) {
      fsCalibConstsOut << " " << adGoodCalibConsts[nParam] << " " << adGoodErrorCalibConsts[nParam];
    }
    fsCalibConstsOut << endl;

  }

  // free memory
  for (int i = 0; i < nNumberOfFiles; i++) {
    fsCalibFiles[i]->close(); // close the file
    delete fsCalibFiles[i];   // free memory
  }


  return(0);
}
