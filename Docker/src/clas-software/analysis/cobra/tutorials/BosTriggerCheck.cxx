/** @file BosTriggerCheck.cxx
 *  @brief Grabs info straight from the banks in a BOS file.
 */
//_____________________________________________________________________________
// Includes:

// Standard C++ Headers:
#include <iostream>
#include <string>
#include <bitset>
#include <unistd.h>

// ROOT headers:
#include "TROOT.h"
#include "TFile.h"
#include "TTree.h"

// Bos class header:
#include "Bos.h"
//_____________________________________________________________________________

using namespace std;

void PrintUsage();
//_____________________________________________________________________________

int main(int __argc, char *__argv[]){

  Bos *bos = new Bos(); // this handles all interfacing with the BOS file
  string rootFile_name = "default_BosTriggerBits.root"; // default outfile
  // command line args handlers
  extern char* optarg;
  extern int optind;
  int c;

  // Parse the command line
  while((c = getopt(__argc,__argv,"ho:")) != -1){
    switch(c){
    case 'h': // help option
      PrintUsage();
      return 0;
      break;
    case 'o':
      rootFile_name = optarg;
      cout << "Root file: " << rootFile_name << endl;       
      break;
    default:
      // not an option
      break;
    }
  }

  float current; // beam current (from 2C21A measurement in epics scalers)
  float trigTot,trigAcc; // total/accepted triggers
  float clockRg,clockBg; // run/busy gated clocks
  float fcupRg,fcupBg; // run/busy gated faraday cup measurements
  float randomRg,randomBg; // run/busy gated random pulser
  float clockRg_prev = 0,clockBg_prev = 0;
  float trigTot_prev = 0,trigAcc_prev = 0;
  float fcupRg_prev = 0,fcupBg_prev = 0;
  float randomRg_prev = 0,randomBg_prev = 0;
  float goodTrig,badTrig; // events where trigger word is good/bad
  float l1acc,l1acc_prev = 0; // Level 1 accepted triggers

  // Set up the ROOT enviornment and open the output file
  TROOT troot("","");
  TFile outFile(rootFile_name.c_str(),"recreate");
  
  // Set up the output tree
  TTree *tree = new TTree("T","");
  tree->Branch("current",&current,"current");
  tree->Branch("trigTot",&trigTot,"trigTot");
  tree->Branch("trigAcc",&trigAcc,"trigAcc");
  tree->Branch("clockRg",&clockRg,"clockRg");
  tree->Branch("clockBg",&clockBg,"clockBg");
  tree->Branch("fcupRg",&fcupRg,"fcupRg");
  tree->Branch("fcupBg",&fcupBg,"fcupBg");
  tree->Branch("randomRg",&randomRg,"randomRg");
  tree->Branch("randomBg",&randomBg,"randomBg");
  tree->Branch("goodTrig",&goodTrig,"goodTrig");
  tree->Branch("badTrig",&badTrig,"badTrig");
  tree->Branch("l1acc",&l1acc,"l1acc");

  // bank pointers we'll need
  clasEPIC_t *epic = 0;
  clasTRGS_t *trgs = 0;
  clasS1ST_t *s1st = 0;
  clasHEAD_t *head = 0;
  int scaler_interval = 0,num_epics_entries,num_bits_set;
  bool has_epics = false;
  bitset<32> *trigBits = 0;

  for(int n_arg = optind; n_arg < __argc; n_arg++){ // loop over bos files

    cout << "Processing File: " << __argv[n_arg] << endl;
    if(!(bos->InFile(__argv[n_arg]))){ // open bos file
      cout << "Unable to open file." << endl;
      continue; // skip it
    }

    while(bos->GetBOS()){ // gets the next BOS event
      
      head = (clasHEAD_t*)bos->GetBank("HEAD");
      if(!head) continue; // no HEAD bank for this event...skip it
      if(head->head->type == 10){ // scaler
	trgs = (clasTRGS_t*)bos->GetBank("TRGS");
	s1st = (clasS1ST_t*)bos->GetBank("S1ST");

	if(scaler_interval > 0){ 
	  // skip the 1st interval, since we're looking between scalers
	  
	  if(trgs){
	    trigTot = trgs->trgs->trig_or_ug - trigTot_prev;
	    clockRg = trgs->trgs->clock_g1 - clockRg_prev;
	    clockBg = trgs->trgs->clock_g2 - clockBg_prev;
	    fcupRg = trgs->trgs->fcup_g1 - fcupRg_prev;
	    fcupBg = trgs->trgs->fcup_g2 - fcupBg_prev;
	    randomRg = trgs->trgs->random_g1 - randomRg_prev;
	    randomBg = trgs->trgs->random_g2 - randomBg_prev;
	    l1acc = trgs->trgs->l1accept - l1acc_prev;
	  }
	  if(s1st) trigAcc = s1st->s1st->event_count - trigAcc_prev;

	  if(has_epics && trgs && s1st){ 
	    // between last/this scaler there was epics written
	    tree->Fill();
	    cout << "Interval: " << scaler_interval << " I: " << current 
		 << " LiveC: " << clockBg/clockRg << " LiveF: " 
		 << fcupBg/fcupRg << " LiveR: " << randomBg/randomRg 
		 << " Trigger Ratio: " << badTrig/(goodTrig + badTrig) << endl;
	  }
	}
	scaler_interval++; 
	trigTot_prev = trgs->trgs->trig_or_ug;
	clockRg_prev = trgs->trgs->clock_g1;
	clockBg_prev = trgs->trgs->clock_g2;
	fcupRg_prev = trgs->trgs->fcup_g1;
	fcupBg_prev = trgs->trgs->fcup_g2;
	randomRg_prev = trgs->trgs->random_g1;
	randomBg_prev = trgs->trgs->random_g2;
	trigAcc_prev = s1st->s1st->event_count;
	l1acc_prev = trgs->trgs->l1accept;
	goodTrig = 0.;
	badTrig = 0.;
	has_epics = false;
      }
      else { // NOT a scaler event
	// check the trigger bits
	trigBits = new bitset<32>::bitset(head->head->trigbits);
	num_bits_set = 0;
	for(int i = 0; i < 6; i++) if(trigBits->test(i)) num_bits_set++;
	if(num_bits_set >= 2) goodTrig++;
	else badTrig++;
	delete trigBits; trigBits = 0;

	// see if there's an epics write out
	epic = (clasEPIC_t*)bos->GetBank("EPIC");
	if(epic){
	  num_epics_entries = epic->bank.nrow;
	  if(num_epics_entries >= 69){
	    current = epic->epic[68].value;
	    if(current > 0.) has_epics = true;
	  }
	}
      }
      bos->CleanBOS(); // drop the banks, get ready for next event
    }      
    bos->CloseInFile(); // close the current bos input file
  }
  
  outFile.Write(); // write tree to output root file
  outFile.Close(); // close output file

  return 0;
}
//_____________________________________________________________________________

void PrintUsage(){

  cout << "Usage:  BosTriggerCheck -o<file> bosFile1 bosFile2 ..." << endl;
  cout << endl;
  cout << "\t-o <file>  Output ROOT file name " << endl;
  cout << "\t-h         Print this message." << endl;
  cout << endl;
}
//_____________________________________________________________________________
