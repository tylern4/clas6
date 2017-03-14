/* CLASdatadump.cc
 * dumps a class data event
 */
// standard C++ header files 
#include <unistd.h>
#include <iostream>
#include <fstream>

// ROOT header files
#include "TROOT.h"
#include "TFile.h"
#include "CLASdata.h"
//#include "ClasEvent.h"

void PrintOptions();

using namespace std;

int main(int __argc,char *__argv[]) {
  int c;
  extern char* optarg; //this is a dummy that will be converted into an integer max
  double max = 1e9; //maximum number of events
  TFile *inFile = 0; // input file
  CLASdata *data = new CLASdata();
  extern int optind;
  TTree *tree = 0; // tree in input file
  int num_entries = 0; // number of entries stored in each file
  int entry = 0; // loop variable within a file
  int events_processed = 0; // total number of events processed
  int eventNum = 0; //what number do you want to see?
  int eventStart = 0; //what number do you want to start at?
  int eventFinish = 0; //what event to end at?
  int EventNumber = 0; //the event number
  if(__argc == 1) //checking to see if user knows what they are doing
  {
    PrintOptions(); //user doesn't know what to do!
    return 0;
  }
  else{ //user does know what they are doing it looks like
    while((c = getopt(__argc,__argv,"h:M:e:n:s:f:")) != -1){ //figuring out which argument the "smart" user chose
      switch(c){ 
      case 'h': //they want help
	PrintOptions(); //give it to them
	exit(1); //exitting....
	break;
      case 'M': //setting the max number of events
	max = atoi(optarg); //converting to integer
	cerr << "Max Events: " << max << endl; //telling them their choice
	break;
      case 'e': //they're noisy, they want to know what's going on
	eventNum = atoi(optarg);
	cerr << "Looking for event number " << eventNum << endl;
	break;
      case 'n': //go to an event # and start there
	eventStart = atoi(optarg);
	break;
      case 's': //event # to start at
	eventNum = atoi(optarg);
	break;
      case 'f': //event # to end at
	eventFinish = atoi(optarg);
	break;
      default:
	PrintOptions(); //oops, they didn't know what they were doing
	exit(1);
	break;
      }
    }
  }
  TROOT troot();
  for(int n_arg = optind; n_arg < __argc; n_arg++){
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
     entry = 0; // reset entry number for this file
     while(events_processed < max+eventStart && entry < num_entries){
       data->GetEntry(entry);
       EventNumber = data->EventNumber();
       if(eventNum == 0) {
	 if(eventStart == 0) {
	   data->Show(events_processed);
	 } else if(eventStart <= events_processed) {
	   data->Show(events_processed);
	 } else { //nothing else, just skip this
	 }
       } else if(EventNumber == eventNum && eventFinish == 0) {
	 data->Show(events_processed);
       } else if(EventNumber >= eventNum && EventNumber <= eventFinish) { 
	 data->Show(events_processed);
       } else { //nothing else, just skipping things
       }
       events_processed++;
       entry++;
     }
     delete inFile; inFile = 0; // close the input file
     tree = 0;
  }
}

//_____________________________________________________________________________
/* PrintOptions will give help for those who need to know syntax or wish to change some of the parameters */
void PrintOptions() 
{
  cerr << "Usage:" << endl;
  cerr << "CLASdatadump inFile1 inFile 2....  " << endl;
  cerr << "\t-M[#] Max number of events to be processed" << endl;
  cerr << "\t-n Start at the nth event in file" << endl;
  cerr << "\t-e  Event Number in File" << endl;
  cerr << "\t-s Event number to start at." << endl;
  cerr << "\t-f Event number to stop at" << endl;
  cerr << endl;
}
