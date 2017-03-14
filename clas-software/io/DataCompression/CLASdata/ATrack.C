#include "ATrack.h"
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// ATrack stores the tracking info for a charged particle in CLAS. It stores
// the SC plane intersection and direction from the SCPB/TDPL banks, the 
// tracking chi^2 and the tracking covariance matrix elements from TBER.
//
// ATrack Return Functions:
//   C11()...............Returns sigma(q/p)^2 (c11 in TBER)
//   C22()...............Returns sigma(lambda)^2 (c22 in TBER)
//   C33()...............Returns sigma(phi)^2 (c33 in TBER)
//   C44()...............Returns sigma(d0)^2 (c44 in TBER)
//   C55()...............Returns sigma(z0)^2 (c55 in TBER)
//   C12()...............Returns C(q/p,lambda) (c12 in TBER)
//   C13()...............Returns C(q/p,phi) (c13 in TBER)
//   C14()...............Returns C(q/p,d0) (c14 in TBER)
//   C15()...............Returns C(q/p,z0) (c15 in TBER)
//   C23()...............Returns C(lambda,phi) (c23 in TBER)
//   C24()...............Returns C(lambda,d0) (c24 in TBER)
//   C25()...............Returns C(lambda,z0) (c25 in TBER)
//   C34()...............Returns C(phi,d0) (c34 in TBER)
//   C35()...............Returns C(phi,z0) (c35 in TBER)
//   C45()...............Returns C(z0,d0) (c45 in TBER)
//   Chi2()..............Returns tracking chi^2 (chi2 in DCPB bank)
//   LayInfo1()..........Returns layinfo1 from TBER bank
//   SCdirection().......Returns Direction at SC plane as TVector3, this is
//                         (cx_sc,cy_sc,cz_sc) in DCPB bank.
//   SCintersect().......Returns intersection with SC plane, (x_sc,y_sc,z_sc)
//                         in the DCPB bank, as a TVector3
//
// To save disk space, the precision of the covariance matrix elements has been
// reduced. Diagnol elements are stored with 4 sig digs and off-diagnol 
// elements are stored with 3 (these are stored in terms of the corelations,
// see Encode3Rhos for info).
//
///////////////////////////////////////////////////////////////////////////////

ClassImp(ATrack)

//_____________________________________________________________________________

ATrack::ATrack():TObject(){
  // *********Constructor**********
}
//_____________________________________________________________________________

ATrack::ATrack(const ATrack &track):TObject(){
  // **********Copy Constructor**********
  _Rho151234 = track._Rho151234;
  _Rho242325 = track._Rho242325;
  _Rho354514 = track._Rho354514;
  _C11C22 = track._C11C22;
  _C33C44 = track._C33C44;
  _Rho13C55 = track._Rho13C55;
  _Layinfo1 = track._Layinfo1;
  for(Int_t i = 0; i < 3; i++){
    _SCintersect[i] = track._SCintersect[i];
    _SCdir[i] = track._SCdir[i];
  }
  _Chi2 = track._Chi2;
}
//_____________________________________________________________________________

ATrack::ATrack(Float_t *cov,const TVector3 &scinter,const TVector3 &scdir,Float_t chi2){
  // *********Constructor**********
  this->SetCovariance(cov[0],cov[1],cov[2],cov[3],cov[4],cov[5],cov[6],cov[7],cov[8],cov[9],cov[10],cov[11],cov[12],cov[13],cov[14]);
  this->SetSCintersect(scinter);
  this->SetSCdir(scdir);
  this->SetChi2(chi2);
}
//_____________________________________________________________________________

ATrack::~ATrack(){
  // *********Destructor**********
}
//_____________________________________________________________________________

ATrack& ATrack::operator=(const ATrack &track){
  // Assignment operator
  _Rho151234 = track._Rho151234;
  _Rho242325 = track._Rho242325;
  _Rho354514 = track._Rho354514;
  _C11C22 = track._C11C22;
  _C33C44 = track._C33C44;
  _Rho13C55 = track._Rho13C55;
  _Layinfo1 = track._Layinfo1;
  for(Int_t i = 0; i < 3; i++){
    _SCintersect[i] = track._SCintersect[i];
    _SCdir[i] = track._SCdir[i];
  }
  _Chi2 = track._Chi2;
  return *this;
}
//_____________________________________________________________________________

void ATrack::SetCovariance(Float_t c11, Float_t c12, Float_t c13, Float_t c14, Float_t c15, Float_t c22, Float_t c23, Float_t c24, Float_t c25, Float_t c33, Float_t c34, Float_t c35, Float_t c44, Float_t c45, Float_t c55){
  // Set the TBER covariance matrix elements for one particle.
  Float_t r12=0.,r13=0.,r14=0.,r15=0.,r23=0.,r24=0.,r25=0.,r34=0.,r35=0.,r45=0.;
  if(c11 != 0 && c22 != 0) r12 = c12/(sqrt(c11)*sqrt(c22));
  if(c11 != 0 && c33 != 0) r13 = c13/(sqrt(c11)*sqrt(c33));
  if(c11 != 0 && c44 != 0) r14 = c14/(sqrt(c11)*sqrt(c44));
  if(c11 != 0 && c55 != 0) r15 = c15/(sqrt(c11)*sqrt(c55));
  if(c22 != 0 && c33 != 0) r23 = c23/(sqrt(c22)*sqrt(c33));
  if(c22 != 0 && c44 != 0) r24 = c24/(sqrt(c22)*sqrt(c44));
  if(c22 != 0 && c55 != 0) r25 = c25/(sqrt(c22)*sqrt(c55));
  if(c33 != 0 && c44 != 0) r34 = c34/(sqrt(c33)*sqrt(c44));
  if(c33 != 0 && c55 != 0) r35 = c35/(sqrt(c33)*sqrt(c55));
  if(c44 != 0 && c55 != 0) r45 = c45/(sqrt(c44)*sqrt(c55));

  _Rho151234 = this->Encode3Rhos(r15,r12,r34);
  _Rho242325 = this->Encode3Rhos(r24,r23,r25);
  _Rho354514 = this->Encode3Rhos(r35,r45,r14);
  
  _C11C22 = this->Encode2Cs(sqrt(c11),sqrt(c22));
  _C33C44 = this->Encode2Cs(sqrt(c33),sqrt(c44));
  _Rho13C55 = this->Encode1Rho1C(r13,c55);
}
//_____________________________________________________________________________

Int_t ATrack::Encode3Rhos(Float_t r1,Float_t r2,Float_t r3){
  // Encode 3 rhos into 1 integer. Rho is the corelation between 2 of the 
  // tracking quantities, ex.) Rho(p,phi) = c13/sqrt(c11*c33). Thus, rho must
  // be between -1 and 1. Each rho is stored as 0.XXX, r3's sign is NOT stored
  // and thus must be known (several of the corelations have the same sign for
  // every track in CLAS, these are stored as r3), r1 and r2's signs are stored
  // r1's is the sign of the integer and r2's is a flag in the 10^10 place.
  Int_t n1,n2,n3,value;
  
  n1 = (int)(r1*1000.);
  n2 = (int)(r2*1000.);
  n3 = (int)(r3*1000.);

  value = abs(n1)*1000000 + abs(n2)*1000 + abs(n3);

  if(n2 < 0) value += 1000000000;
  if(n1 < 0) value *= -1;
  
  return value;
}
//_____________________________________________________________________________

Int_t ATrack::Encode2Cs(Float_t c1,Float_t c2){
  // Encode 2 c's into 1 integer. The c's are square roots of diagnol elements 
  // of the tracking covariance matrix (thus they're always positive and we 
  // don't have to store their signs). Each c is stored as 4 significant digits
  // along with a flag in the 10^9 place that stores where the 2 decimals 
  // belong.
  Int_t value,n1,n2, flag;

  n1 = (int)(c1*1000000);
  if ((c1*1000000 - n1) > .5) n1 += 1;
  n2 = (int) (c2*1000000);
  if ((c2*1000000 - n2) > .5) n2 += 1;

  flag = 0;
  if (n1 > 9999){
    flag += 1;
    n1 = (int)(c1*100000);
    if ((c1*100000 - n1) > .5) n1 += 1;
  }
  else if (n1 < 1000){
    flag += 2;
    n1 = (int)(c1*10000000);
    if ((c1*1000000 - n1) > .5) n1 += 1;
  }
  
  if (n2 > 99999){
    flag += 6;
    n2 = (int) (c2*10000);
    if ((c2*10000 - n1) > .5) n2 += 1;
  }
  else if (n2 > 9999){
    flag += 3;
    n2 = (int) (c2*100000);
  if ((c2*10000 - n1) > .5) n2 += 1;
  }

  value = 100000000*flag + 10000*n1 + n2;
  return value;
}
//_____________________________________________________________________________

Float_t ATrack::Decode2Cs(Int_t encoded, Int_t n) const {
  // encoded must be in format described in Encode2Cs. This function decodes
  // the diagnol covariance matrix elements encoded by Encode2Cs and returns
  // n'th one (n can only be equal to 1 or 2).
  Float_t val;
  int flag = encoded/100000000;
  int n1 = (encoded - (encoded/100000000)*100000000)/10000;
  int n2 = (encoded - (encoded/10000)*10000);
  if (n == 1){
    if (flag == 1 || flag == 4 || flag == 7)  val = pow(n1/100000.,2);
    else if (flag == 2 || flag == 5 || flag == 8) val = pow(n1/10000000., 2);
    else val = pow(n1/1000000., 2);
  }
  else {
    if (flag == 3 || flag == 4 || flag == 5) val = pow(n2/100000., 2);
    else if (flag == 6 || flag == 7 || flag == 8) val = pow(n2/10000., 2);
    else val = pow(n2/1000000., 2);
  }

  return val;
}

//_____________________________________________________________________________

Int_t ATrack::Encode1Rho1C(Float_t r,Float_t c){
  // Encode 1 c an 1 rho into 1 integer. c is c55 (not sqrt(c55), as with the 
  // other diagnol elements). Rho is the same as above (see Encode3Rhos).
  // c55 is kept to 4 digits.
  Int_t value,n1,n2, flag;
  //store c55 directly
  if (c > 9.999){
    flag = 2;
    n1 = 9999;
  }
  else if (c > .9999){
    flag = 1;
    n1 = (int)(c*1000.);
    if (c*1000. - n1 >=.5) n1 += 1;
  }
  else{
    flag = 0;
    n1 = (int)(c*10000.);
    if (c*10000. - n1 >=.5) n1 += 1;
  }

  n2 = (int)(r*1000.);
  value = n1 + flag*10000 + abs(n2)*1000000;
  if(n2 < 0) value *= -1;
  return value;
}
//_____________________________________________________________________________

Float_t ATrack::Decode3Rhos(Int_t encoded,Int_t n) const {
  // Decode the 3 rhos in encoded and return the nth one as a Float_t. This 
  // function is the inverse of Encode3Rhos.
  Float_t r1 = 1.,r2 = 1.,r3 = 1.;

  if(encoded < 0){
    r1 = -1.;
    encoded *= -1;
  }
  if(abs(encoded)/1000000000 >= 1){
    r2 = -1.;
    encoded -= 1000000000;
  }
  r1 *= (encoded/1000000)/1000.;
  encoded -= (encoded/1000000)*1000000;
  r2 *= (encoded/1000)/1000.;
  encoded -= (encoded/1000)*1000;
  r3 = encoded/1000.;

  if(n == 1) return r1;
  if(n == 2) return r2;
  if(n == 3) return r3;
  return 0.0;
}
//_____________________________________________________________________________

void ATrack::Print(std::ostream &os) const{
  // Dump to os the contents of this ATrack object. os defaults to std::cout,
  // thus Print() prints the info to the screen.
  os << "   Covariance Matrix: c11: " << this->C11() << " c12: " << this->C12() << " c13: " << this->C13() << endl;
  os << "\tc14: " << this->C14() << " c15: " << this->C15() << " c22: " << this->C22() << " c23: " << this->C23() << endl;
  os << "\tc24: " << this->C24() << " c25: " << this->C25() << " c33: " << this->C33() << " c34: " << this->C34() << endl;
  os << "\tc35: " << this->C35() << " c44: " << this->C44() << " c45: " << this->C45() << " c55: " << this->C55() << endl;
  os << "   SC plane info: Intersection: (" << this->SCintersect().X() << "," << this->SCintersect().Y() << "," << this->SCintersect().Z() << ") DC Chi2 = " << this->Chi2() << endl; 
  os << "   Direction: (" << this->SCdirection().X() << "," << this->SCdirection().Y() << "," << this->SCdirection().Z() << ")" << endl;
}
//_____________________________________________________________________________

std::ostream& operator<<(std::ostream &os,const ATrack &track){
  track.Print(os);
  return os;
}
