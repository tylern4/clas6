#ifndef ROOT_Charged
#define ROOT_Charged

#include "TObject.h"
#include "TClonesArray.h"
#include "TVector3.h"
#include "AParticle.h"
#include <iostream>

using namespace std;

class Charged : public TObject {

 private:

  TClonesArray *_Particles; //-> TClonesArray of AParticle objects
  TVector3 _Vertex;         //   MVRT vertex
  Float_t _Vtime[2];        //   TBID vtime's (from the 2 TBID banks) 
  
 public:
  
  // Constructors/Destructors
  Charged();
  Charged(const Charged&);
  virtual ~Charged();

  // Setters
  void AddParticle(const AParticle&);
  inline void SetVertex(const TVector3 &v) {_Vertex = v;};
  inline void SetVtimes(Float_t,Float_t);

  // Getters
  inline Int_t N() const {return _Particles->GetEntries();};
  inline const AParticle& GetParticle(Int_t i)const{return *(AParticle*)(*_Particles)[i];};
  inline const AParticle& operator[](Int_t i) const {return this->GetParticle(i);};
  inline const AParticle& operator()(Int_t i) const {return this->GetParticle(i);};

  inline const TVector3& Vertex() const {return _Vertex;};
  inline Float_t Vtime(Int_t i = 1) const {return _Vtime[i-1];};

  // Functions
  void Reset();
  void Print(std::ostream &os = std::cout) const;
  
  // Operatros
  Charged& operator=(const Charged&);

  ClassDef(Charged,1) // Contains info on charged particles
};
//_____________________________________________________________________________

// inline member functions of class Charged

inline void Charged::SetVtimes(Float_t v1,Float_t v2){
  _Vtime[0] = v1;
  _Vtime[1] = v2;
}

//_____________________________________________________________________________

std::ostream& operator<<(std::ostream &os,const Charged &ch);


#endif
