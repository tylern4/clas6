#include "Photons.h"
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// The TAGRentry class stores a single photon's info from the TAGR bank.     //
//
// TAGRentry Return Functions:
//     E()..............Energy (GeV) (erg in TAGR bank)
//     Ttag()...........Photon's tagger time (ns) (ttag in TAGR bank)
//     Tpho()...........Photon's RF corrected time (ns) (tpho in TAGR bank)
//     Tid()............Photon's T counter id (t_id in TAGR bank)
//     Eid()............Photon's E counter id (e_id in TAGR bank)
//
// Note: TAGR variable stat is NOT stored since only photons with stat's of
//       7 or 15 are kept in Photons.
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

ClassImp(TAGRentry)

//_____________________________________________________________________________

TAGRentry::TAGRentry():TObject(){
  //**********Constructor**********
}
//_____________________________________________________________________________

TAGRentry::TAGRentry(Float_t e,Float_t ttag,Float_t tpho,Int_t tid,Int_t eid):TObject(){
  //**********Constructor**********
  //
  // Creates a TAGRentry object with energy e, tagger time ttag, RF corrected 
  // time tpho, and paddle id's tid and eid.
  _E = e;
  _Ttag = ttag;
  _Tpho = tpho;
  _PaddleIDs = 1000*tid + eid;
}
//_____________________________________________________________________________

TAGRentry::TAGRentry(const TAGRentry &tag):TObject(){
  //**********Copy Constructor**********
  _E = tag._E;
  _Ttag = tag._Ttag;
  _Tpho = tag._Tpho;
  _PaddleIDs = tag._PaddleIDs;
}
//_____________________________________________________________________________

TAGRentry::~TAGRentry(){
  //**********Destructor**********
}
//_____________________________________________________________________________

TAGRentry& TAGRentry::operator=(const TAGRentry &tag){
  // Assignment operator
  _E = tag._E;
  _Ttag = tag._Ttag;
  _Tpho = tag._Tpho;
  _PaddleIDs = tag._PaddleIDs;
  return *this;
}
//_____________________________________________________________________________

void TAGRentry::Print(std::ostream &os) const{
  // Dump top os the contents of this TAGRentry object.
  // Default is std::cout, which outputs the info to the screen.
  os << "\tE: " << this->E() << " Ttag " << this->Ttag() << " Tpho: " << this->Tpho() << " Tid: " << this->Tid() << " Eid: " << this->Eid() << std::endl;
}
//_____________________________________________________________________________

std::ostream& operator<<(std::ostream &os,const TAGRentry &tag){
  // Overload of std::ostream::operator<< for TAGRentry.
  tag.Print(os);
  return os;
}

