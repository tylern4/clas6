#include <iostream>
#include <string>
#include <vector>
#include <stdlib.h>
#include <ntypes.h>
#include <errno.h>
#include "Expect.h"
#include "Spectra.h"
#include "JBosFile.h"
#include "BankDescription.h"
#include "JEventCount.h"
#include "signal.h"
#include <map_manager.h>

TRint* theApp;
bool lookforSync = true;
bool useXwindows = true;
int loopSync = 175;
int eventCount = 0;
int eventMax = 0;
bool loopfiles = true;
bool gamecockMode = false;
int SC_Version_Flag = 1;
//every channel, every fit
//vector<vector<TH1D*> > tdcGaussFitHistos;
//vector<vector<TF1*> > tdcGaussFits;

void PrintUsage(char* progname) {
  cout << endl << "Usage:\n\t" << progname 
       << " [-options] <inputfile1> [<inputfile2> ...]\n" << endl
       << "Options:           Comment:                              [Default]\n" << endl
       << "  -n <number>      process only <number> of events       [all events]\n" << endl
       << "  -o <file>        results (output) to <root-file>       [tdc_calib.root]\n" << endl
    //       << "  -d <directory>   write calibration constants to <dir>  [current]\n" << endl
       << "  -l <number>      new loop after <number> of syncs      [175]\n" << endl
       << "  -A               average to determine pulser peak      [gauss fit]\n"
       << "  -X               don't open X-graphics window          [open X]\n" 
       << "  -G               output optimized for gamecock         [terminal]\n"
       << endl
       << "  -h               show commandline options (this output)\n" << endl
       << "Info/Comments/BugReports:\n" << endl
       << "\tlanghei@physics.sc.edu\n" << endl;
}

void handle_break(int signal) {
  cout << "Signal terminates loop" << endl;
  loopfiles = false;
}

void process (JBosEvent* be, Expect* expct, Spectra* spec) {
  if (getBank(&bcs_, "+SYN")) {
//cout << "sync event" << endl;
    spec->GaussFit (expct->getT());
    (*expct)++;
  }
  else {
    expct->eventcount++;
    spec->Fill(be,expct);
  }
}

void complain (char* message, char* progname) {
  cerr << "*** " << message << " ***" << endl;
  PrintUsage(progname);
  exit(2);
}

int main(int argc,char **argv)
{
  int runNumber   = -1;
  int eventNumber =  0;

  vector<string> inputFiles;
  char* rootFileName = NULL;
  char* constOutDir  = NULL;
  char* maxEventNo   = NULL;
  int iarg = 0;

  gFileCount = NULL;
  char  c_newline= '\r';

  while (++iarg < argc) {
    if (argv [iarg] [0] != '-') 
      inputFiles.push_back ( (string) argv [iarg]);
    else {
      switch ( argv[iarg][1] ) {  
      case 'd':                         // write constants in this dir
	if (strlen(argv[iarg]) > 2)     // -dfilename
	  constOutDir = argv[iarg] + 2;
	else {                          // -d filename
	  if (++iarg >= argc)  complain("Option -d without argument", argv[0]);
	  constOutDir = argv[iarg];
	}
	break;

	/*
      case 's':                         // workaround for DAQ bug:
	lookforSync = true;             // look for +SYN rather than eventtype=2
	break;
	*/

      case 'X':                         // text terminal only, don't show any plots
	useXwindows = false;
	break;

      case 'G':
	gamecockMode = true;
	break;

      case 'A':
	TdcHistogram::useGaussian = false;
	break;

      case 'l':
	if (strlen(argv[iarg]) > 2)     // -lnumber
	  loopSync = atoi(argv[iarg] + 2);
	else {                          // -l number
	  if (++iarg >= argc) complain("Option -r without argument", argv[0]);
	  loopSync = atoi(argv[iarg]);
	}
	
	break;

      case 'o':                         // write histogram into root file
	if (strlen(argv[iarg]) > 2)     // -rfilename
	  rootFileName = argv[iarg] + 2;
	else {                          // -r filename
	  if (++iarg >= argc) complain("Option -r without argument", argv[0]);
	  rootFileName = argv[iarg];
	}
	break;


      case 'n':                         // write histogram into root file
	if (strlen(argv[iarg]) > 2)     // -rfilename
	  maxEventNo = argv[iarg] + 2;
	else {                          // -r filename
	  if (++iarg >= argc) complain("Option -r without argument", argv[0]);
	  maxEventNo = argv[iarg];
	}
	break;


      case 'h':
	PrintUsage(argv[0]);
	exit(0);
	break;
    
      default:
	char message[80] = "Unknown option ";
	strcat (message, argv [iarg]);
	complain (message, argv[0]);
	break;
      }
    }
  }

  if (! inputFiles.size()) complain("No input files", argv[0]) ;

  if (useXwindows) {
    theApp = new TRint("Interactive", 0, 0, 0, 0, 0 ); 
  }

  initbos();


  Expect *expct = NULL;
  Spectra *spec = NULL;
  
  if (maxEventNo) eventMax = atoi(maxEventNo);
  
  // end loop and create histograms based on current number of events
  signal (2, handle_break);
  signal (3, handle_break);

  if (gamecockMode) {
    gFileCount = new JFileCount(&inputFiles);
    c_newline = '\n';
  }

  for (unsigned int ifile=0; ifile< inputFiles.size(); ifile++) {
    
    if (!loopfiles) break;
    
    cout << "open file <" << inputFiles[ifile] << ">" << endl;
    JBosFile bf(inputFiles[ifile]);
    
    while (bf.good () && loopfiles) {
      JBosEvent be;
      if (be.good()) {

	clasHEAD_t* HEAD = NULL;
	if ((HEAD = (clasHEAD_t*) getBank(&bcs_, "HEAD"))) {
	  if (runNumber<=0) {	  
	    runNumber = HEAD->head->nrun;
         if(runNumber >= 55357){
           SC_Version_Flag = 2;
//           tdcGaussFitHistos.resize(342);
//           tdcGaussFits.resize(342);
         }else{
//           tdcGaussFitHistos.resize(288);
//           tdcGaussFits.resize(288);
         }

cout << "runnumber, version flag = " << runNumber << ", " << SC_Version_Flag << endl;
         read_tdc_calib_banks();
         expct = new Expect(loopSync, 2.139);
         spec = new Spectra;

	    //if (runNumber > 0) {
	    //	    initialize_tof(runNumber); 
	    //}	if (!(++eventCount % 200)) { 
	  }
	  eventNumber = HEAD->head->nevent;
	}

	if (gamecockMode && gFileCount) {
	  cout << "Processed:  " 
	       << gFileCount->GetPercent(ifile, eventNumber)
	       << " %" << endl;
	}
	else {
	  cout.width(7);
	  cout << eventCount << c_newline; 
	  cout.flush(); 
	}
      
	if (eventMax && eventCount >= eventMax) loopfiles = false;
	
	process (&be, expct, spec); //gaussian fit to tdc spectrum of each paddle for all events between sync events!!! (occurs every 1k-2k events???)
      } //ends if be.good
      else { 
	bf.setEof();
      }
    } //ends bf.good
  } //ends file loop


  spec->PolynomialFit ();
  spec->AverageT0 (); //currently does nothing
  
  spec->WriteHist (rootFileName);

  if (useXwindows) theApp->Run();
  //  spec.DumpValues ();


/*  JMainMenu* mm = new JMainMenu(runNumber,  constOutDir);
  spec->HandOverResults (mm);
  if (rootFileName) mm->WriteHisto(rootFileName);

  if (useXwindows) {
    mm->StartMenu();
    theApp->Run();
  } */

}
