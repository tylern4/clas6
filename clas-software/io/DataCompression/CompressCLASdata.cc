// Author: Mike Williams

// standard C++ header files 
#include <iostream>
#include <fstream>
#include <vector>
#include <unistd.h>

// ROOT header files
#include "TROOT.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TTree.h"
#include "TRandom.h"

// My header files
#include "BOS.h"
#include "SetCLASdata.h"

// subroutine to display usage 
void PrintOptions();
void SetIsFluxEvent(EventHeader*,const vector<int>&,const vector<int>&);

using namespace std;

int main(int argc, char *argv[]) {

  int remake = 0,mcflag = 1,Nevents,i,evMin,evMax,dummy,c,tripflag;
  char *rootfile = "default.root";
  char *fluxfile = NULL;
  char *tripfile = NULL;
  int max = (int)1E9,Nrequired = -1;
  float flux,error,targOffset = -10.;

  BOS *bos = new BOS();

  int verbose = 0;

  Photons *tag = new Photons();
  EventHeader *head = new EventHeader();
  Tracks *tracks = new Tracks();
  Charged *charged = new Charged();

  ifstream *fluxFile = NULL;
  ifstream *tripFile = NULL;

  vector<string> filenames;

  vector<int> tripMin,tripMax;

  // Check command line arguments
  if(argc == 1){
    PrintOptions();
    return 0;
  }
  else{   
    while((c = getopt(argc,argv,"hM:R:F:T:B:N:rvst:V")) != -1){
      switch(c){
        case 'h':
          PrintOptions();
          exit(0);
          break;
        case 'M':
          max = atoi(optarg);
          cout << "Max Events = " << max << endl;
          break;
        case 'R': 
          rootfile = optarg;
          cout << "Root Output File Name: " << rootfile << endl;
          break;
        case 'V':
          verbose = 1;
          break;
        case 'r':
          remake = 1;
          cout << "BOS banks will be remade." << endl;
          break;
        case 't':
          targOffset = atof(optarg);
          cout << "Target Offset: " << targOffset << endl;
          break;
        case 'v':
          remake = 2;
          cout << "MVRT will be built." << endl;
          break;
        case 's':
          mcflag = 0;
          cout << "This is a Monte Carlo File." << endl;
          break;
        case 'F':
          fluxfile = optarg;
          cout << "gFlux file is: " << fluxfile << endl;
          fluxFile = new ifstream(fluxfile);
          break;
        case 'T':
          tripfile = optarg;
          cout << "Trip file is: " << tripfile << endl;
          tripFile = new ifstream(tripfile);
          break;
        case 'B':
          cout << "Processing BOS file: " << optarg << endl;
          //if(!(bos->InFile(optarg))){
          // cout << "Unable to open BOS file." << endl;
          filenames.push_back(optarg);
       //   return 0;
      //}
      break;
        case 'N':
      Nrequired = atoi(optarg);
      cout << "Requiring at least " << Nrequired << " charged tracks." << endl;
      break;
        default:
      // not an option of bos2root
      cout << "Warning!!!! Unknown option: " << c << endl;
      break;
      }
    } 
  } 
  // Tree Declaration
  TROOT simple("simple","");
  TFile rfile(rootfile,"RECREATE");
  rfile.SetCompressionLevel(9);

  TTree *tree = new TTree("Data","analysis"); 
  tree->Branch("photons","Photons",&tag,32000,0);
  tree->Branch("header","EventHeader",&head,32000,99);
  tree->Branch("tracks","Tracks",&tracks,32000,0); 
  tree->Branch("charged","Charged",&charged,32000,0);

  TH1D *hFlux = new TH1D("hFlux","gFlux in E counter bins",767,0.5,767.5);
  // Get Flux and error for each E-counter bin from Flux file
  if(fluxfile){
    i = 0;
    while(fluxFile->peek() != EOF){
      i++;
      *fluxFile >> flux >> error;
      hFlux->SetBinContent(i,flux);
      hFlux->SetBinError(i,error);
    }
    cout << "Flux read for " << i << " E-counter bins." << endl;
  }
  // Get bad flux event numbers from the trip file
  if(tripfile){
    while(tripFile->peek() != EOF){
      *tripFile >> dummy >> dummy >> tripflag >> evMin >> evMax;
      tripFile->ignore(200,'\n');
      if(tripflag != 0){
        tripMin.push_back(evMin);
        tripMax.push_back(evMax);
      }
    }
    cout << tripMax.size() << " trip intervals defined." << endl;
  }

  int numfiles = filenames.size();
  cerr << "List of " << filenames.size() << " files." << endl;

  for(int i=0;i<numfiles;i++)
  {
    if((bos->InFile(filenames[i].c_str()))){
    cerr << "Opening " << filenames[i] << endl;
      Nevents = 0;
      // Loop over events in the BOS file
      while((Nevents < max) && (bos->GetBOS(remake,1,mcflag))){
        Nevents++;
        if(Nevents%1000 == 0) cerr << Nevents << "\r";
        // Get the required banks
        if(bos->GetBanks()){
          if(verbose) cerr << "Got banks....." << endl;
          SetPhotons(bos,tag,targOffset);
          SetEventHeader(bos,head);
          SetTracks(bos,tracks);
          SetCharged(bos,charged);
          SetIsFluxEvent(head,tripMin,tripMax);
          if(bos->HEADbank()->head[0].type < 0) head->SetTrigBits(GetTrigBits(bos));
          if(charged->N() >= Nrequired){
            tree->Fill();
          }   
        }
        // Drop the banks
        bos->CleanBOS();
      }
      bos->CloseInFile();
      cout << "Events Processed: " << Nevents << "." << endl;
    }
  }

  // Write out the Tree to the ROOT output file
  rfile.Write();
  rfile.Close();

  return 0;
}
//_____________________________________________________________________________

void SetIsFluxEvent(EventHeader *head,const vector<int> &Min,const vector<int> &Max){
  // Sets EventHeader::_IsFluxEvent
  int nevent = head->EventNumber();
  bool IsGood = true;
  for(int i = 0; i < (int)Min.size(); i++){
    if(nevent > Min[i] && nevent < Max[i]){
      IsGood = false;
      i = (int)Min.size();
    }
  }
  head->SetIsFluxEvent(IsGood);  
}
//_____________________________________________________________________________

void PrintOptions(){
  //  PrintOptions prints to the screen how to enter commmand line options 
  cout << endl;
  cout << "Command Line: CompressCLASdata -[Option 1] -[Option 2] ..." << endl << endl;
  cout << "Options:"  << endl;
  cout << "\t-N[required number of tracks]" << endl << endl;
  cout << "\t-B[BOS input file]" << endl << endl;
  cout << "\t-M[max number of events]" << endl << endl;
  cout << "\t-R[root file for output]" << endl << endl;
  cout << "\t-r(remake banks)" << endl << endl;
  cout << "\t-v(remake the MVRT bank)" << endl << endl;
  cout << "\t-s(this is a monte carlo file)" << endl << endl;
  cout << "\t-t[#] Target offset (default is -10. for g11a) " << endl << endl;
  cout << "\t-F[gFlux file]" << endl << endl;
  cout << "\t-T[trip file]" << endl << endl;
  cout << "\t-h(Print Options)"        << endl << endl << endl;

}
