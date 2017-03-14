#ifndef ROOT_Photons
#define ROOT_Photons

#include "TObject.h"
#include "TClonesArray.h"
#include <iostream>

using namespace std;

class TAGRentry : public TObject {

 private:

  Float_t _E;  //   Energy (GeV)
  Float_t _Ttag; //   Tagger time (ns)
  Float_t _Tpho; //   RF corrected time (ns)
  Int_t _PaddleIDs; //   Bit packed paddle id's
  
 public:
  
  // Constructors/Destructors
  TAGRentry();
  TAGRentry(const TAGRentry&);
  TAGRentry(Float_t,Float_t,Float_t,Int_t,Int_t);
  virtual ~TAGRentry();

  // Getters
  inline Float_t E() const {return _E;};
  inline Float_t Ttag() const {return _Ttag;};
  inline Float_t Tpho() const {return _Tpho;};
  inline Int_t Tid() const {return _PaddleIDs/1000;};
  inline Int_t Eid() const {return (_PaddleIDs - (_PaddleIDs/1000)*1000);};

  // Functions
  void Print(std::ostream &os = std::cout) const;

  // Operators
  TAGRentry& operator=(const TAGRentry&);

  ClassDef(TAGRentry,1) // Contains a single photon's info from the TAGR bank
};
//_____________________________________________________________________________

std::ostream& operator<<(std::ostream &os,const TAGRentry&);

//_____________________________________________________________________________

class Photons : public TObject {

 private:

  TClonesArray *_Phots; //-> TClonesArray of TAGRentry objects
  Short_t _SEBphot; //   Pointer to the SEB photon in _Phots
  Float_t _SEBdt; //   SEB starttime_ST - starttime_TAG (ns)

 public:

  // Constructors/Destructors
  Photons();
  Photons(const Photons&);
  virtual ~Photons();
  
  // Setters
  void AddPhoton(Float_t e,Float_t ttag,Float_t tpho,Int_t tid,Int_t eid);
  inline void AddPhoton(const TAGRentry &tag){new((*_Phots)[this->N()]) TAGRentry(tag);};
  inline void AddTAGRentry(Float_t e,Float_t ttag,Float_t tpho,Int_t tid,Int_t eid) {this->AddPhoton(e,ttag,tpho,tid,eid);};
  void SetSEBphot(Short_t sebphot) {_SEBphot = sebphot;};
  void SetSEBdt(Float_t sebdt) {_SEBdt = sebdt;};
  
  // Getters
  inline const TAGRentry& GetPhoton(Int_t i) const {return *(TAGRentry*)(*_Phots)[i];};
  inline const TAGRentry& operator[](Int_t i) const {return this->GetPhoton(i);};
  inline const TAGRentry& operator()(Int_t i) const {return this->GetPhoton(i);};
  inline Short_t SEBphot() const {return _SEBphot;};
  inline Float_t SEBdt() const {return _SEBdt;};
  inline const TAGRentry& GetSEBphoton() const {return this->GetPhoton(this->SEBphot());};
  inline Int_t N() const {return _Phots->GetEntries();};

  // Functions
  void Reset();
  void Print(std::ostream &os = std::cout) const;

  // Operators
  Photons& operator=(const Photons&);

  ClassDef(Photons,1) // Stores all info kept on tagged photons
};
//_____________________________________________________________________________

std::ostream& operator<<(std::ostream &os,const Photons&);

#endif
