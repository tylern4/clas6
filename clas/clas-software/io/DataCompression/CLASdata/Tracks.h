#ifndef ROOT_Tracks
#define ROOT_Tracks

#include "TObject.h"
#include "TClonesArray.h"
#include "TVector3.h"
#include <iostream>
#include "ATrack.h"

using namespace std;

class Tracks : public TObject {

 private:

  TClonesArray *_Tracks; //-> TClonesArray of ATrack objects

 public:

  // Constructors/Destructors
  Tracks();  
  Tracks(const Tracks&);
  virtual ~Tracks();

  // Setters
  void AddTrack(Float_t *cov,const TVector3 &scinter,const TVector3 &scdir,Float_t chi2);
  void AddTrack(const ATrack &trk);

  // Getters
  inline Int_t N() const {return _Tracks->GetEntries();};  
  inline const ATrack& GetTrack(Int_t i)const{return *(ATrack*)(*_Tracks)[i];};
  inline const ATrack& operator[](Int_t i) const {return this->GetTrack(i);};
  inline const ATrack& operator()(Int_t i) const {return this->GetTrack(i);};

  // Functions
  inline void Reset() {_Tracks->Clear();};
  void Print(std::ostream &os = std::cout) const;

  // Operators
  Tracks& operator=(const Tracks&);

  ClassDef(Tracks,1) // Contains tracking info for all charged particles
};
//_____________________________________________________________________________

std::ostream& operator<<(std::ostream&,const Tracks&);

#endif
