/** @file SkimOmega.cc
 *  @brief Skims \f$\omega\f$ meson events from compressed ROOT files and 
 *         produces an output ROOT Event file.
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
#include "CLASdata.h"
//_____________________________________________________________________________

using namespace std;

void PrintUsage(); // prints usage of the program to the screen
//_____________________________________________________________________________

int main(int __argc,char *__argv[]){

  /*___________________________Variable Declaration__________________________*/

  CLASdata *data = new CLASdata(); // handles data input
  ClasEvent clasevent("p:pi+:pi-"); // gamma p --> p pi+ pi- (X)
  clasevent.PrintConfig();
  
  Event *event = new Event(); // object to be written to the output tree

  // the next 3 are for parsing the command line
  int c;
  extern char* optarg;
  extern int optind;

  char *outFileName = "default.root"; // name of output ROOT file
  bool is_mc = false; // is this monte carlo?
  int entry = 0; // loop variable within a file
  int num_entries = 0; // number of entries stored in each file
  int events_processed = 0; // total number of events processed
  int max = (int)1e9; // maximum number of events to process
  double mm,mmp; // total missing mass and missing mass off the proton
  bool cut_trip = false; // cut out events in bad trip intervals?

  TFile *inFile = 0; // input file
  TTree *tree = 0; // tree in input file

  /*____________________________Parse Command Line___________________________*/
  while((c = getopt(__argc,__argv,"m:o:ht")) != -1){
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
     case 't': // cut bad trip intervals
       cut_trip = true;
       cout << "Cutting out bad trip intervals" << endl;
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

  // gflux histo...bin i's content is paddle i's flux with flux error
  TH1D *hFlux = new TH1D("hFlux","Flux in E-Counter bins",767,0.5,767.5);

  /*__________________________Loop Over Input Files__________________________*/
  for(int n_arg = optind; n_arg < __argc; n_arg++){

    /*______________________________File Set Up______________________________*/
    cout << "Processing file: " << __argv[n_arg] << endl;
    inFile = new TFile(__argv[n_arg]); // open the file

    if(!inFile->Get("Data")) { // file corrupted...skip it
      cout << "No Data tree found...skipping file..." << endl;
      continue; 
    }

    tree = (TTree*)inFile->Get("Data"); // get the data tree
    data->Init(tree); // connect the CLASdata object to the tree

    num_entries = (int)tree->GetEntries(); // number of entries in the tree
    if(num_entries == 0){ // tree is empty...skip the file
      cout << "No entries in the Data tree...skipping file..." << endl;
      continue;
    }

    // Check to see if this is a monte carlo file
    data->GetEntry(0);
    if(data->Type() == 1) is_mc = false; // type = 1 is data
    else is_mc = true;
    clasevent.SetIsMC(is_mc); // let clasevent know if this is monte carlo

    if(is_mc) cout << "This is a monte carlo file." << endl;

    // if data, require the flux histo to be in the file
    if(!is_mc && !inFile->Get("hFlux")){
      cout << "No flux histogram in data file...skipping file..." << endl;
      continue;
    }
    // add this file's flux to the total
    if(!is_mc) hFlux->Add((TH1D*)inFile->Get("hFlux")); 

    entry = 0; // reset entry number for this file

    /*____________________Loop Over Events in Current File___________________*/
    while(events_processed < max && entry < num_entries){

      events_processed++;
      data->GetEntry(entry); // read next entry into memory

      if(cut_trip){
	// if data file and event is NOT in a good trip interval...skip it   
	if(!is_mc && !data->IsFluxEvent()) {
	  entry++; // increment it since we won't get to the bottom
	  continue; 
	}
      }
      clasevent.SetClasEvent(data); // init ClasEvent for this event

      while(clasevent.GetEvent(data)){ // loop over all g p -> p pi+ pi- combos

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
      entry++; // increment the entry counter
    }
    delete inFile; inFile = 0; // close the input file
    tree = 0;
  } /* end loop over input files */

  /*________________________Write Output to ROOT File________________________*/

  cout << "Events Processed: " << events_processed << endl;

  outTree->Print(); // print the tree to the screen

  outFile.Write(); // write to the output file
  outFile.Close(); // close the output file
  
  return 0;
}
//_____________________________________________________________________________

void PrintUsage(){

  cout << "Usage: SkimOmega -o <file> inFile1 inFile2 ..." << endl;
  cout << endl;
  cout << "Options:" << endl;
  cout << "\t-o <file>   Output ROOT file name " << endl;
  cout << "\t-m <###>    Maximum number of events to process" << endl;
  cout << "\t-t          Cut out events in bad trip intervals" << endl;
  cout << "\t-h          Print this message " << endl;
  cout << endl;
  cout << "Example Usage:" << endl;
  cout << "[thebride@kbill]$ SkimOmega -o 43582_omega.root "
       << "/raid4/g11a/data/43582*.root " << endl;
  cout << endl;
}
//_____________________________________________________________________________
