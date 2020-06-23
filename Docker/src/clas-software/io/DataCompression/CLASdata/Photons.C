#include "Photons.h"
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// The Photons class stores all information pertaining to tagged photons. 
// Each photon in the event with a good status flag (7 or 15) is added to the 
// TClonesArray _Phots as a TAGRentry object. For further documentation on what
// info is kept for each photon see TAGRentry. Photons also keeps a pointer to
// which entry in _Phots is the SEB photon and what the SEB dt was.
//
// Photons Return Functions:
//   GetPhoton(i).......Returns the i'th TAGRentry object stored for this event
//   GetSEBphoton().....Returns the TAGRentry coresponding to photon flagged
//                      in TGPB as the SEB photon.
//   SEBphot()..........Returns a C-index reference to which _Phots entry  was 
//                      identified by TGPB as the SEB photon.
//   SEBdt()............Returns ST-TAG time difference (ns) (dt in TGPB bank)
//   N()................Returns the number of photons in this Photons object
//
// The operators [] and () have also been overloaded so that for Photons phots,
// phots.GetPhoton(i), phots[i] and phots(i) are all equivalent.
//
// Note: Photons::Reset() must be called before filling a Photons object if it 
//       has previously been filled, otherwise elements not overwritten in the
//       new event will be carried over (ie. if the last event had 3 photons, 
//       and this event only has 2, if Reset isn't called then this event will
//       store 3 photons where the 3rd is the 3rd entry from the previous
//       event).
//
///////////////////////////////////////////////////////////////////////////////

ClassImp(Photons)
//_____________________________________________________________________________

Photons::Photons():TObject(){
  //**********Constructor**********
  _SEBphot = -1;
  _SEBdt = -100.0;
  _Phots = new TClonesArray("TAGRentry",0);
}
//_____________________________________________________________________________

Photons::Photons(const Photons &phot):TObject(){
  //**********Copy Constructor**********
  _SEBphot = phot._SEBphot;
  _SEBdt = phot._SEBdt;
  _Phots = new TClonesArray("TAGRentry",0);
  for(Int_t i = 0; i < phot.N(); i++){
    this->AddPhoton(phot[i]);
  }
}
//_____________________________________________________________________________

Photons::~Photons(){
  //**********Destructor**********
  _Phots->Clear();
}
//_____________________________________________________________________________

Photons& Photons::operator=(const Photons &phot){
  // Assignment operator
  _SEBphot = phot._SEBphot;
  _SEBdt = phot._SEBdt;  
  if(!_Phots) _Phots = new TClonesArray("TAGRentry",0);
  else _Phots->Clear();
  for(Int_t i = 0; i < phot.N(); i++){
    this->AddPhoton(phot[i]);
  }
  return *this;
}
//_____________________________________________________________________________

void Photons::AddPhoton(Float_t e,Float_t ttag,Float_t tpho,Int_t tid,Int_t eid){
  // Add a TAGRentry object with energy e, tagger time ttag, rf corrected time
  // tpho and paddle id's tid and eid.
  //
  // If needed, the TClonesArray of TAGRentry objects (_Phots) will dynamically
  // expand to add this photon.
  //
  // Similar functions are AddPhoton(const TAGRentry&) which adds the TAGRentry
  // object passed to _Phots, and AddTAGRentry which is just a wrapper to this
  // function.
  new((*_Phots)[this->N()]) TAGRentry(e,ttag,tpho,tid,eid);
}
//_____________________________________________________________________________

void Photons::Reset(){
  // Reset the Photons object for the next event. The TClonesArray _Phots is 
  // cleared using TClonesArray::Clear.
  _Phots->Clear();
  _SEBphot = -1;
  _SEBdt = -100.0;
}
//_____________________________________________________________________________

void Photons::Print(std::ostream &os) const {
  // Output the contents of this Photons object using std::ostream os.
  // Default is cout, thus dumping the info to the screen.
  Int_t i;
  TAGRentry tag;
  os << "Photons(" << this->N() << ":" << endl;
  os << "SEB photon index: " << this->SEBphot() << " SEB dt: " << this->SEBdt() << endl;
  for(i = 0; i < this->N(); i++){
    tag = this->GetPhoton(i);
    tag.Print(os);
  }
}
//_____________________________________________________________________________

std::ostream& operator<<(std::ostream &os,const Photons &phot){
  // Overload of std::ostream::operator<< for Photons.
  phot.Print(os);
  return os;
}
