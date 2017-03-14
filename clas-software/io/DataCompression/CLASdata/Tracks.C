#include "Tracks.h"
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// The Tracks class stores each charged particle's tracking info in a 
// TClonesArray of ATrack objects. For documentation on what is stored for 
// each track see ATrack.
//
// Tracks Return Functions:
//   GetTrack(i)........Returns the i'th ATrack object stored
//   N()................Returns the number of ATrack objects stored
//
// The operators () and [] have been overloaded so that Tracks trks, 
// trks.GetTrack(i), trk[i] and trk(i) are all equivalent.
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

ClassImp(Tracks)

//_____________________________________________________________________________

Tracks::Tracks():TObject(){
  // *********Constructor**********
  _Tracks = new TClonesArray("ATrack",0);
}  
//_____________________________________________________________________________

Tracks::Tracks(const Tracks &trks):TObject(){
  // **********Copy Constructor**********
  _Tracks = new TClonesArray("ATrack",0);
  for(Int_t i = 0; i < trks.N(); i++){
    this->AddTrack(trks[i]);
  }
}
//_____________________________________________________________________________

Tracks::~Tracks(){
  // *********Destructor**********
  _Tracks->Clear();
} 
//_____________________________________________________________________________

Tracks& Tracks::operator=(const Tracks &trks) {
  // Assignment operator
  if(!_Tracks) _Tracks = new TClonesArray("ATrack",0);
  else _Tracks->Clear();
  for(Int_t i = 0; i < trks.N(); i++){
    this->AddTrack(trks[i]);
  }
  return *this;
}
//_____________________________________________________________________________

void Tracks::AddTrack(Float_t *cov,const TVector3 &scinter,const TVector3 &scdir,Float_t chi2){
  // Add a track to _Tracks. If needed, the array will dynamically expand to 
  // store the new ATrack object.
  new((*_Tracks)[this->N()]) ATrack(cov,scinter,scdir,chi2);
}
//_____________________________________________________________________________

void Tracks::AddTrack(const ATrack &trk){
  // Same as above, except ATrack object passed.
  new((*_Tracks)[this->N()]) ATrack(trk);
}
//_____________________________________________________________________________

void Tracks::Print(std::ostream &os) const{
  // Dump to os the contents of this Tracks object. os defaults to std::cout,
  // thus Print() prints the info to the screen.
  os << "Tracks(" << this->N() << "):" << endl;
  for(Int_t i = 0; i < this->N(); i++){
    this->GetTrack(i).Print();
  }
}
//_____________________________________________________________________________

std::ostream& operator<<(std::ostream &os,const Tracks &trks){
  // overloaded std::ostream::operator<< for Tracks
  trks.Print(os);
  return os;
}
