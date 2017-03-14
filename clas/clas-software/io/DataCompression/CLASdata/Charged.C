#include "Charged.h"
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// Charged stores charged particle info. Each charged track in the event is  
// added to the TClonesArray _Particles as a AParticle object. For further 
// documentation on what info is kept for each charged particle see AParticle.
// Charged also stores the MVRT vertex as a TVector3 object and the TBID vertex
// times for the sector 1 and 2 TBID banks. 
//
// Cuts for keeping particles....
//
// Note: Drift chamber info (from DCPB/TDPL) along with tracking covariance 
//       matrix elements (from TBER) are stored in the Tracks class.
//
// Charged Return Functions:
//   GetParticle(i)......Returns the ith AParticle object for this event.
//   Vertex()............Returns the MVRT vertex as a TVector3
//   Vtime(group)........Returns the TBID vertex time from the group TBID bank,
//                         group = 1,2.
//   N().................Returns the number of particles in this Charged object
//
// The operators () and [] have also been overloaded so that for Charged ch,
// ch.GetParticle(i), ch[i] and ch(i) are equivalent.
//
// Note: Charged::Reset() must be called before filling a Charged object if it
//       has previously been filled, otherwise elements not overwritten by the
//       new event will be carried over from the old event.
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

ClassImp(Charged)

//_____________________________________________________________________________

Charged::Charged() : TObject() {
  // **********Constructor**********
  _Particles = new TClonesArray("AParticle",0);
  _Vtime[0] = 0.0;
  _Vtime[1] = 0.0;
}
//_____________________________________________________________________________

Charged::Charged(const Charged &ch):TObject() {
  // **********Copy Constructor**********
  _Vertex = ch._Vertex;
  _Vtime[0] = ch._Vtime[0];
  _Vtime[1] = ch._Vtime[1];
  _Particles = new TClonesArray("AParticle",0);
  for(Int_t i = 0; i < ch.N(); i++){
    this->AddParticle(ch[i]);
  }
}
//_____________________________________________________________________________

Charged::~Charged() {
  // **********Destructor**********
  _Particles->Clear();
}
//_____________________________________________________________________________

Charged& Charged::operator=(const Charged &ch) {
  // Assignment operator
  _Vertex = ch._Vertex;
  _Vtime[0] = ch._Vtime[0];
  _Vtime[1] = ch._Vtime[1];
  if(!_Particles) _Particles = new TClonesArray("AParticle",0);
  else _Particles->Clear();
  for(Int_t i = 0; i < ch.N(); i++){
    this->AddParticle(ch[i]);
  }
  
  return *this;
}
//_____________________________________________________________________________
  
void Charged::AddParticle(const AParticle &part) {
  // Add a particle to the TClonesArray _Particles. If needed, the TClonesArray
  // will dynamically expand in memory to store the additional AParticle object
  new((*_Particles)[this->N()]) AParticle(part);
}
//_____________________________________________________________________________

void Charged::Reset() {
  // Reset the Charged object. This clears _Particles using TClonesArray::Clear
  // and zeros the MVRT vertex and vertex times.
  _Particles->Clear();
  _Vtime[0] = 0.0;
  _Vtime[1] = 0.0;
  _Vertex.SetXYZ(0.,0.,0.);
}
//_____________________________________________________________________________

void Charged::Print(std::ostream &os) const {
  // Dump the Charged object's info to std::ostream os. The default value of 
  // os is cout, thus Print() prints the info to the screen.
  os << "Charged(" << this->N() << "):" << endl;
  os << "   MVRT vertex: (" << _Vertex.X() << "," << _Vertex.Y() << "," << _Vertex.Z() << ") Vtimes: (1): " << _Vtime[0] << " (2): " << _Vtime[1] << endl;
  for(Int_t i = 0; i < this->N(); i++){
    os << "   ";
    this->GetParticle(i).Print();
  }
}
//_____________________________________________________________________________

std::ostream& operator<<(std::ostream &os,const Charged &ch){
  // Overload of std::ostream::operator<< for Charged.
  ch.Print(os);
  return os;
}
