#include "CLASdata.h"
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// CLASdata is the main class used to interface with the ROOT compressed CLAS
// data files. Once connected to a TTree filled with CompressCLASdata.cxx, a
// CLASdata object provides access to all info that was kept from the BOS file.
//
// Trees filled with CompressCLASdata.cxx have 4 branches: an EventHeader 
// branch, a Photons branch, a Charged branch and a Tracks branch. Each of 
// these branches stores a ROOT compatible class that stores a certain type
// of CLAS data (for more information see the documentation for these classes).
// This class has been provided for easy access to all of this info, thus 
// CLASdata reads into memory all 4 branches when GetEntry(i) is used. 
// 
// CLASdata Return Functions:
//   GetParticle(i)......Returns the ith AParticle object stored.
//   GetPhoton(i)........Returns the ith TAGRentry object stored.
//   GetSEBphoton(i).....Returns the TAGRentry object id'd by SEB as the 
//                         correct photon in the TGPB bank.
//   GetTrack(i).........Returns the ith ATrack object stored.
//   Ncharged()..........Returns the number of particles stored in Charged.
//   Nphotons()..........Returns the number of photons stored in Photons.
//   Ntracks()...........Returns the number of entries in Tracks.
//   RFtime1()...........RF time 1 (ns) (rf1 in CL01 bank)
//   RFtime2()...........RF time 2 (ns) (rf2 in CL01 bank)
//   RFtime()............Good RF time (ns) (rf in CL01/rf1 in HEVT)
//   SEBtime()...........Event start time as determined by SEB (ns),(stt in 
//                         HEVT bank) (same as STT())
//   RunNumber().........Returns run number (nrun in HEAD/HEVT)
//   EventNumber().......Returns the event number (nevent in HEAD/HEVT banks)
//   TimeProcessed().....Returns time event was processed, UNIX time or seconds
//                         as of Jan. 1,1970 (time in HEAD/ptime in HEVT)
//   Type()..............data = 1, monte carlo = 0 (type in HEAD/HEVT banks)
//   EvtClass()..........DAQ classification (evtclass in HEAD bank)
//   Vertex()............Returns the MVRT vertex as a TVector3.
//   Vtime(group)........Returns the TBID vtime (group=BOS bank sector 1 or 2)
//
// The GetXXXX(i) function calls can then be combined with the functions of the
// storage classes to obtain any info that's needed. For example, to get the 
// energy of the 2nd photon stored in CLASdata data for this event, write:
//   data.GetPhoton(1).E();
// and similarly for any TAGRentry return function. For charged particle and
// tracking info, follow a similar process with GetParticle(i) or GetTrack(i)
// using AParticle or ATrack return functions.
// 
// Below is an example of a program that reads compressed CLAS data files
// (the data is stored in a TTree named "Data") using the CLASdata class:
//
// // File: RunOverROOTfiles.cxx
// #include "CLASdata.h"
// #include ...
//
// int main(int argc, char *argv[]) {
//     TFile *inFile = NULL;
//     TTree *inTree = NULL;
//     CLASdata *Data = new CLASdata();
//     ...declare needed variables, decifer command line options, etc...
//     
//     for(n = 1; n < argc; n++){
//      if(argv[n][0] != '-'){
//       cout << "Processing Compressed ROOT file: " << argv[n] << endl;
//       inFile = new TFile(argv[n]); // Open the ROOT data file
//       inTree = (TTree*)inFile->Get("Data");
//       Data->Init(inTree); // Link the CLASdata object to the correct TTree
//       i = 0;
//       // Loop over events in inFile
//       while(i < inTree->GetEntries()){
//         Data->GetEntry(i); // Load the Current Entry into memory
//
//         ...Process the event, fill histos or trees...
//
//         i++;
//       }
//      delete inTree;
//      inTree = NULL;
//      delete inFile;
//      inFile = NULL;
//      }
//     }
//  ...Write new ROOT objects to an output file, etc...
//  return 0;
//  }
//
//  This could then be run over multiple compressed CLAS data files,
//  $ RunOverROOTfiles -...options... file1.root file2.root ... filen.root
//
//  Interactively, the compressed ROOT files can be accessed with CLASdata as 
//  well. Add gSystem.Load("...path.../libCLASdata.so") to your rootlogon.C,
//  then in ROOT you can type:
//  root [0] TFile f("file.root");
//  root [1] TTree *T = (TTree*)f.Get("Data");
//  root [2] CLASdata data(T);
//  root [3] data.GetEntry(1);
//  root [4] data.Show();
//
//  This will show the 2nd event (obviously something more involved could be 
//  done using a macro).
// 
///////////////////////////////////////////////////////////////////////////////

ClassImp(CLASdata)

//_____________________________________________________________________________

CLASdata::CLASdata(TTree *tree){
  // **********Constructor**********
  //
  // Calls Init(tree). Default value for tree is NULL.
  Init(tree);
}
//_____________________________________________________________________________

CLASdata::~CLASdata(){
  // **********Destructor**********
}
//_____________________________________________________________________________

Int_t CLASdata::GetEntry(Int_t entry){
  // Get entry from the current TTree. If !_Tree returns 0, otherwise it 
  // returns TTree::GetEntry(entry).
  if(!_Tree) return 0;
  if(_Photons) _Photons->Reset();
  if(_Charged) _Charged->Reset();
  if(_Tracks) _Tracks->Reset();
  return _Tree->GetEntry(entry);
}
//_____________________________________________________________________________

void CLASdata::Init(TTree *tree){
  // Connect tree to this CLASdata object. Branch addresses are set to the 
  // appropriate data members ("photons" branch address is set to _Photons,
  // etc...). CLASdata::GetBranches() is also called.
  _Header = NULL;
  _Photons = NULL;
  _Charged = NULL;
  _Tracks = NULL;
  //  _Neutrals = NULL;

  if(tree == NULL) return;
  _Tree = tree;

  // Set Branch Addresses
  _Tree->SetBranchAddress("photons",&_Photons);
  _Tree->SetBranchAddress("header",&_Header);
  _Tree->SetBranchAddress("tracks",&_Tracks);
  _Tree->SetBranchAddress("charged",&_Charged);
  // _Tree->SetBranchAddress("neutrals",&_Neutrals);

  this->GetBranches();
}
//_____________________________________________________________________________

Bool_t CLASdata::GetBranches(){
  // Get Branch pointers. Branch data members are set to point to the correct
  // branch in _Tree (_PhotonsBranch is set to point to "photons", etc...).
  // If all branch pointers are valid it returns true, otherwise it returns
  // false.
  _PhotonsBranch = _Tree->GetBranch("photons");
  _HeaderBranch = _Tree->GetBranch("header");
  _TracksBranch = _Tree->GetBranch("tracks");
  //  _NeutralsBranch = _Tree->GetBranch("neutrals");
  _ChargedBranch = _Tree->GetBranch("charged");

  return (_PhotonsBranch && _HeaderBranch && _TracksBranch && _ChargedBranch);
}
//_____________________________________________________________________________

void CLASdata::Show(Int_t entry){
  // Show entry. Defaults to current entry. Contents of entry are displayed on
  // the screen using the Print() functions of the classes stored in the 
  // branches.
  if(entry != -1) this->GetEntry(entry);
  cout << "**********Event: " << _Tree->GetReadEntry() << endl;
  _Header->Print();
  _Photons->Print();
  _Charged->Print();
  _Tracks->Print();
}
//_____________________________________________________________________________

Float_t CLASdata::CalcMass(Int_t n,const char *idscheme,Int_t group) const {
  // Returns the calculated mass of particle n, using the path length and sc 
  // time specified by "idscheme" (for "part" group is the sector, 1 or 2).
  Float_t beta,gamma,mass;
  AParticle part = this->GetParticle(n);
  if(!strcmp(idscheme,"seb")){
    beta = ((part.SCpath("seb"))/(part.SCtime("seb") - this->STT()))/29.99792;
    gamma = 1./sqrt(1.-beta*beta);
    mass = part.P()/(beta*gamma);
    return mass;
  }
  else if(!strcmp(idscheme,"part")){
    beta = ((part.SCpath("part"))/(part.SCtime("part") - this->Vtime(group)))/29.99792;
    gamma = 1./sqrt(1.-beta*beta);
    mass = part.P()/(beta*gamma);
    return mass;
  }
  else{
    cout << "Warning <CLASdata::CalcMass> Unknown id scheme " << idscheme << endl;
    return -1.;
  }
}
