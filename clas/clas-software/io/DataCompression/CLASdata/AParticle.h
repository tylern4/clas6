#ifndef ROOT_AParticle
#define ROOT_AParticle

#include "TObject.h"
#include "TClonesArray.h"
#include "TVector3.h"
#include "TLorentzVector.h"
#include <iostream>

using namespace std;

int geant2pdg(int);

class AParticle : public TObject {

 private:

  Float_t _P; //   Momentum Magnitude (GeV)
  Float_t _Lambda; //   Tracking angle Lambda (rad)
  Float_t _Phi; //   Tracking angle Phi (rad)
  Float_t _D0; //   DOCA (cm)
  Float_t _Z0; //   Tracking parameter z0 (cm)
  Int_t _Info; //   Bit packed sector, charge, SEB id and PART id's
  Int_t _StatFlags; //   Bit packed status flags from EVNT
  UInt_t _SCinfo1; //   Bit packed SC paddle id, chi2 and deposited energy
  Int_t _SCinfo2; //   Bit packed SC status and path length
  Int_t _SCpartInfo1; //   Bit packed part sc time and id
  Int_t _SCpartInfo2; //   Bit packed part sc edep and path
  Float_t _SCtime; //   SC time 
  Int_t _STinfo; //   Bit packed ST time and vertex time

  Float_t IDtoMass(Int_t) const; // converts a PDG id to a mass

 public:

  // Constructors/Destructors
  AParticle();
  AParticle(const AParticle&);
  virtual ~AParticle();

  // Setters
  inline void SetP(Float_t p) {_P = p;};
  inline void SetLambda(Float_t lam) {_Lambda = lam;};
  inline void SetPhi(Float_t phi) {_Phi = phi;};
  inline void SetD0(Float_t d0) {_D0 = d0;};
  inline void SetZ0(Float_t z0) {_Z0 = z0;};  
  inline void SetSector(Int_t);
  inline void SetQ(Int_t);
  inline void SetCharge(Int_t q) {this->SetQ(q);};
  inline void SetSEBid(Int_t);
  inline void SetPARTids(Int_t,Int_t);
  void SetStatFlags(Int_t,Int_t,Int_t,Int_t,Int_t,Int_t,Int_t);
  void SetSCinfo(Int_t,Float_t,Float_t,Int_t,Float_t,Float_t);
  void SetSCpartInfo(Int_t,Float_t,Float_t,Float_t);
  void SetSTinfo(Float_t,Float_t);

  // Getters
  inline Float_t P() const {return _P;}; 
  inline Float_t Phi() const {return _Phi;};
  inline Float_t Lambda() const {return _Lambda;};
  inline Float_t D0() const {return _D0;};
  inline Float_t Z0() const {return _Z0;};  
  inline Int_t Sector() const {return abs(_Info-this->Q()*10000000)/1000000;}; 
  inline Int_t Q() const {return _Info/10000000;};
  inline Int_t Charge() const {return this->Q();};
  inline Int_t SEBid() const;
  inline Int_t PARTid(Int_t group = 1) const;

  inline Int_t Status() const {return _StatFlags/1000000;};
  inline Int_t DCstat() const;
  inline Int_t CCstat() const;
  inline Int_t SCstat() const;
  inline Int_t ECstat() const;
  inline Int_t LCstat() const;
  inline Int_t STstat() const;
  
  Int_t SCstatus(const char*) const;
  Int_t SCid(const char*) const;
  Float_t SCedep(const char*) const; 
  Float_t SCpath(const char*) const;
  inline Float_t SCchi2(const char *id = "seb") const {return ((_SCinfo1-this->SCid("seb")*1000000)/1000)/10.;};
  Float_t SCtime(const char*) const;
   
  inline Float_t STtime() const;
  inline Float_t STvtime() const;

  // Getters for basic quantities calculated from data members
  TLorentzVector P4(const char* IDscheme,Int_t group = 1) const;
  inline TVector3 P3() const {return TVector3(this->Px(),this->Py(),this->Pz());};
  inline Float_t Px() const {return _P*(cos(_Lambda)*sin(_Phi)*cos(this->Alpha())-sin(_Lambda)*sin(this->Alpha()));};
  inline Float_t Py() const {return _P*(cos(_Lambda)*sin(_Phi)*sin(this->Alpha())+sin(_Lambda)*cos(this->Alpha()));};
  inline Float_t Pz() const {return _P*(cos(_Lambda)*cos(_Phi));};
  inline Float_t Alpha() const {return (3.14159/3.)*(this->Sector() - 1);};
  inline Float_t Vx() const {return -(_Z0*sin(this->Alpha()));};
  inline Float_t Vy() const {return _Z0*cos(this->Alpha());};
  inline Float_t Vz() const {return -_D0/sin(_Phi);};

  // operators
  AParticle& operator=(const AParticle&);

  // Functions
  void Print(std::ostream &os = std::cout) const;
  void Zero();
  inline Float_t Mass(const char *IDscheme,Int_t group = 1) const;

  ClassDef(AParticle,1) // contains a single charged particle's info
};
//_____________________________________________________________________________

// inline member functions of class AParticle

inline void AParticle::SetSector(Int_t sec){
  // Set the particle's sector number. If sec !in [1,6], set sec = 0
  _Info = _Info - (_Info/abs(_Info))*(this->Sector()*1000000);
  if(sec > 6 || sec < 1) sec = 0;
  _Info = _Info + (_Info/abs(_Info))*sec*1000000;
}

inline void AParticle::SetQ(Int_t q){
  // Set the particle's charge to be q.
  _Info -= this->Q()*10000000;
  _Info += abs(q)*10000000;
  if(q < 0) _Info *= -1;
}

inline void AParticle::SetSEBid(Int_t id){
  // Set the particle's seb id (PDG id scheme)
  _Info = _Info - (_Info/abs(_Info))*(this->SEBid()*10000);
  if(id > 99 || id < 0){
    // assume that if pid > 99 or < 0 that geant id was passed
    cout << "<AParticle::SetSEBid> Warning!!!! Id passed seems to be Geant ID when PDG ID was expected, ID will be converted using geant2pdg." << endl;
    id = geant2pdg(id);
  }
  _Info = _Info + (_Info/abs(_Info))*id*10000;
}

inline void AParticle::SetPARTids(Int_t id1,Int_t id2){
  // Set the particle's part ids (id1=sector1,id2=sector2)
  if(id1 > 99 || id1 < 0) id1 = 0;
  if(id2 > 99 || id2 < 0) id2 = 0;
  _Info -= (_Info/abs(_Info))*(this->PARTid(1)*100 + this->PARTid(2));
  _Info += (_Info/abs(_Info))*(id1*100 + id2);
}

inline Int_t AParticle::DCstat() const {
  return (abs(_StatFlags) - abs(this->Status())*1000000)/100000;
}

inline Int_t AParticle::CCstat() const {
  return (abs(_StatFlags) - abs(this->Status())*1000000 - this->DCstat()*100000)/10000;
}

inline Int_t AParticle::SCstat() const {
  return (abs(_StatFlags) - abs(this->Status())*1000000 - this->DCstat()*100000 - this->CCstat()*10000)/1000;
}

inline Int_t AParticle::ECstat() const {
  return (abs(_StatFlags) - abs(this->Status())*1000000 - this->DCstat()*100000 - this->CCstat()*10000 - this->SCstat()*1000)/100;
}

inline Int_t AParticle::LCstat() const {
  return (abs(_StatFlags) - abs(this->Status())*1000000 - this->DCstat()*100000 - this->CCstat()*10000 - this->SCstat()*1000 - this->ECstat()*100)/10;
}

inline Int_t AParticle::STstat() const {
  return (abs(_StatFlags) - abs(this->Status())*1000000 - this->DCstat()*100000 - this->CCstat()*10000 - this->SCstat()*1000 - this->ECstat()*100 - this->LCstat()*10);
}

inline Int_t AParticle::SEBid() const {
  // Return SEB id (orginally stored in the EVNT bank)
  return (abs(_Info) - abs(this->Q())*10000000-this->Sector()*1000000)/10000;
}

inline Int_t AParticle::PARTid(Int_t group) const {
  // Return PID id (orginally stored in PART bank (sector = group))
  Int_t pid1 = (abs(_Info)-abs(this->Q())*10000000-this->Sector()*1000000-this->SEBid()*10000)/100;
  if(group == 1) return pid1;
  return (abs(_Info)-abs(this->Q())*10000000-this->Sector()*1000000-this->SEBid()*10000 - pid1*100);
}

inline Float_t AParticle::STtime() const {
  // Return TBID variable st_time for this particle
  Int_t pckd = abs(_STinfo);
  if(this->STvtime() < 0.0) pckd -= 1000000000;
  pckd -= (pckd/100000)*100000;
  if(_STinfo < 0) return -(pckd/1000.);
  return (pckd/1000.);
}

inline Float_t AParticle::STvtime() const {
  // Return TBID variable st_vtime for this particle
  Int_t flag = abs(_STinfo)/1000000000;
  Int_t pckd = abs(_STinfo) - flag*1000000000;
  if(flag == 1) return -(pckd/100000)/100.;
  return (pckd/100000)/100.;
}

inline Float_t AParticle::Mass(const char *IDscheme,Int_t group) const {
  // Return the mass of the particle using "IDscheme" particle id.
  Float_t mass = -1.0;
  if(!strcmp(IDscheme,"seb")) mass = IDtoMass(this->SEBid());
  if(!strcmp(IDscheme,"pid")) mass = IDtoMass(this->PARTid(group));
  return mass;
}
//_____________________________________________________________________________

// non-member functions for AParticle

std::ostream& operator<<(std::ostream &os,const AParticle& part);

#endif
