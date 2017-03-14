/** @file BosSkimOmega.cc
 *  @brief Skims \f$\omega\f$ meson events from BOS files and produces an
 *         output ROOT Event file.
 *
 */
//_____________________________________________________________________________
// Headers:
// Standard C++ Headers:
#include <fstream>
#include <cmath>
#include <iostream>
#include <unistd.h>
// ROOT Headers:
#include "TFile.h"
#include "TTree.h"
#include "TH1.h"
// Local Headers:
#include "ClasEvent.h"
#include "Bos.h"
//_____________________________________________________________________________

using namespace std;

void PrintUsage(); // prints usage of the program to the screen
//_____________________________________________________________________________

int main(int __argc,char *__argv[]){

  /*___________________________Variable Declaration__________________________*/

  Bos *bos = new Bos(); // handles data input
  ClasEvent clasevent("p:pi+:pi-"); // gamma p --> p pi+ pi- (X)
  clasevent.PrintConfig();
  
  Event *event = new Event(); // object to be written to the output tree

  // the next 3 are for parsing the command line
  int c;
  extern char* optarg;
  extern int optind;

  char *outFileName = "default.root"; // name of output ROOT file
  bool is_mc = false; // is this monte carlo?
  int events_processed = 0; // total number of events processed
  int max = (int)1e9; // maximum number of events to process
  double mm,mmp; // total missing mass and missing mass off the proton

  /*____________________________Parse Command Line___________________________*/
  while((c = getopt(__argc,__argv,"m:o:h")) != -1){
     switch(c){
     case 'h': // help option
       PrintUsage();
       return 0;
       break;
     case 'm': // set maximum number of events to process
       max = atoi(optarg);
       cout << "Maximum events to process: " << max << endl;
       break;
     case 'o': // set output file name
       outFileName = optarg;
       cout << "Output file: " << outFileName << endl;
       break;
     default:
       // not an option
       break;
     }
  }

  /*_______________________________ROOT Set Up_______________________________*/
  TROOT troot();
  TFile outFile(outFileName,"recreate"); // output file

  TTree *outTree = new TTree("T","omega skim"); // output tree
  outTree->Branch("event","Event",&event,64000,0);
  
  /*__________________________Loop Over Input Files__________________________*/
  for(int n_arg = optind; n_arg < __argc; n_arg++){

    /*______________________________File Set Up______________________________*/
    cout << "Processing file: " << __argv[n_arg] << endl;
    if(!(bos->InFile(__argv[n_arg]))){ // open the file
      cout << "Unable to open BOS file: " << __argv[n_arg] << endl;
      continue; // skip the file
     }

    while(events_processed < max && bos->GetBOS()){

      if(events_processed == 0){ // 1st event check if this is mc
	if(((clasHEAD_t*)bos->GetBank("HEAD"))->head->type < 0) is_mc = true;
	else is_mc = false;
	clasevent.SetIsMC(is_mc); // let clasevent know if this is monte carlo
	if(is_mc) cout << "This is a monte carlo file." << endl;
      }

      events_processed++;

      clasevent.SetClasEvent(bos);  // init ClasEvent for this event

      while(clasevent.GetEvent(bos)){ // loop over all g p -> p pi+ pi- combos

	if(!is_mc){ // this is data...apply tagger/momentum corrections
	  clasevent.CorrectTagger();
	  clasevent.CorrectMomenta();
	}
	clasevent.ElossCor(); // energy loss corrections

	mm = clasevent.MissingMass(); // get total missing mass
	if(abs(mm - 0.13498) < 0.3){ // very loose pi0 mass cut (to save time)
	  
	  // kinematic fit to gamma p --> p pi+ pi- (pi0)
	  clasevent.KinematicFit("pi0"); 
	  
	  mmp = clasevent.MissingMassOff("p"); // missing mass off the proton
	  
	  if(abs(mmp - 0.78256) < 0.15){ // loose omega mass cut for the skim
	    
	    *event = (Event)clasevent; // set event
	    if(clasevent.Prob() > 0.1) // confidence level cut
	      outTree->Fill();
	    
	  }
	}
      }
      bos->CleanBOS(); // drop banks...get ready for next event
    }
    bos->CloseInFile(); // close input file
  }/* end loop over input files */

  /*________________________Write Output to ROOT File________________________*/

  cout << "Events Processed: " << events_processed << endl;

  outTree->Print(); // print the tree to the screen

  outFile.Write(); // write to the output file
  outFile.Close(); // close the output file
  
  return 0;
}
//_____________________________________________________________________________

void PrintUsage(){

  cout << "Usage: BosSkimOmega -o <file> inFile1 inFile2 ..." << endl;
  cout << endl;
  cout << "Options:" << endl;
  cout << "\t-o <file>   Output ROOT file name " << endl;
  cout << "\t-h          Print this message " << endl;
  cout << endl;
  cout << "Example Usage:" << endl;
  cout << "[thebride@kbill]$ BosSkimOmega -o 43582_omega.root "
       << "/raid4/g11a/data/run_43582* " << endl;
  cout << endl;
}
//_____________________________________________________________________________
