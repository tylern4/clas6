#ifndef ROOT_ATrack
#define ROOT_ATrack

#include <cstdlib>
#include <cmath>

#include "TObject.h"
#include "TClonesArray.h"
#include "TVector3.h"
#include <iostream>

using namespace std;

class ATrack : public TObject {

 private:

  // TBER info
  Int_t _Rho151234; // Bit packed p-z0,p-lambda,phi-d0 corelations
  Int_t _Rho242325; // Bit packed lambda-d0,phi-lambda,lambda-z0 corelations
  Int_t _Rho354514; // Bit packed phi-z0,z0-d0,p-d0 corelations
  Int_t _C11C22; // Bit packed p,lambda tracking errors
  Int_t _C33C44; // Bit packed phi,d0 tracking errors
  Int_t _Rho13C55; // Bit packed z0 tracking error and p-lambda corelation
  Int_t _Layinfo1; // layinfo1 in TBER bank

  // DCPB info
  Float_t _SCintersect[3]; // Track intersection with SC plane
  Float_t _SCdir[3]; // Direction at SC plane
  Float_t _Chi2; // Tracking chi^2

  // private encoding/decoding functions
  Int_t Encode3Rhos(Float_t r1,Float_t r2,Float_t r3);
  Int_t Encode2Cs(Float_t c1,Float_t c2);
  Int_t Encode1Rho1C(Float_t r13,Float_t c55);
  Float_t Decode3Rhos(Int_t encoded,Int_t n) const;
  Float_t Decode2Cs(Int_t encoded, Int_t n) const;
  
 public:

  // Constructors/Destructors
  ATrack();
  ATrack(const ATrack&);
  ATrack(Float_t*,const TVector3&,const TVector3&,Float_t);
  virtual ~ATrack();
  
  // Setters
  void SetCovariance(Float_t c11, Float_t c12, Float_t c13, Float_t c14, Float_t c15, Float_t c22, Float_t c23, Float_t c24, Float_t c25, Float_t c33, Float_t c34, Float_t c35, Float_t c44, Float_t c45, Float_t c55);
  inline void SetLayInfo1(Int_t layinfo) {_Layinfo1 = layinfo;};
  inline void SetSCintersect(const TVector3& inter);
  inline void SetSCdir(const TVector3& dir);
  inline void SetChi2(Float_t chi2) {_Chi2 = chi2;};

  // Getters
  inline Int_t LayInfo1() const {return _Layinfo1;};
  inline TVector3 SCintersect() const {return TVector3(_SCintersect[0],_SCintersect[1],_SCintersect[2]);};
  inline TVector3 SCdirection() const {return TVector3(_SCdir[0],_SCdir[1],_SCdir[2]);};
  inline Float_t Chi2() const {return _Chi2;};

  // return covariance matrix elements
  inline Float_t C55() const;  
  inline Float_t C11() const {return this->Decode2Cs(_C11C22, 1);};
  inline Float_t C22() const {return this->Decode2Cs(_C11C22, 2);};
  inline Float_t C33() const {return this->Decode2Cs(_C33C44, 1);};
  inline Float_t C44() const {return this->Decode2Cs(_C33C44, 2);};
  inline Float_t C12() const {return this->Decode3Rhos(_Rho151234,2)*sqrt(this->C11()*this->C22());};
  inline Float_t C15() const {return this->Decode3Rhos(_Rho151234,1)*sqrt(this->C11()*this->C55());};
  inline Float_t C34() const {return -1.*(this->Decode3Rhos(_Rho151234,3)*sqrt(this->C33()*this->C44()));};
  inline Float_t C23() const {return this->Decode3Rhos(_Rho242325,2)*sqrt(this->C22()*this->C33());};
  inline Float_t C24() const {return this->Decode3Rhos(_Rho242325,1)*sqrt(this->C22()*this->C44());};
  inline Float_t C25() const {return -1.*(this->Decode3Rhos(_Rho242325,3)*sqrt(this->C22()*this->C55()));};
  inline Float_t C35() const {return this->Decode3Rhos(_Rho354514,1)*sqrt(this->C33()*this->C55());};
  inline Float_t C45() const {return this->Decode3Rhos(_Rho354514,2)*sqrt(this->C44()*this->C55());};
  inline Float_t C14() const {return this->Decode3Rhos(_Rho354514,3)*sqrt(this->C11()*this->C44());};
  inline Float_t C13() const {return ((_Rho13C55/1000000)/1000.)*sqrt(this->C11()*this->C33());};

  // Functions
  void Print(std::ostream &os = std::cout) const;

  // Operators
  ATrack& operator=(const ATrack&);

  ClassDef(ATrack,1) // Contains tracking covariance matrix elements along with drift chamber info from DCPB/TDPL.
};
//__________________________________________________________________________

inline Float_t ATrack::C55() const {
  // Return the c55 element of the tracking covariance matrix
  int flag = (abs(_Rho13C55)-(abs(_Rho13C55)/1000000)*1000000)/10000;
  int val = (abs(_Rho13C55)-(abs(_Rho13C55)/10000)*10000);
  if (flag == 2) return 9.999;
  if (flag == 1) return val/1000.;
  return val/10000.;
}

inline void ATrack::SetSCintersect(const TVector3 &v){
  _SCintersect[0] = v.X();
  _SCintersect[1] = v.Y();
  _SCintersect[2] = v.Z();
}

inline void ATrack::SetSCdir(const TVector3 &v){
  _SCdir[0] = v.X();
  _SCdir[1] = v.Y();
  _SCdir[2] = v.Z();
}
//__________________________________________________________________________

std::ostream& operator<<(std::ostream&,const ATrack&);

#endif
