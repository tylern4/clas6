#include "AParticle.h"
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// The class AParticle stores the relevant info for a single charged particle.
// 
// AParticle Return Functions:
//     P()..............Momentum Magnitude (GeV) (pmom in EVNT bank)
//     Phi()............Tracking angle phi (rad) (phi in TBER bank)
//     Lambda().........Tracking angle lambda (rad) (lambda in TBER bank)
//     D0().............Tracking parameter d0 (cm) (d0 in TBER bank)
//     Z0().............Tracking parameter z0 (cm) (z0 in TBER bank)
//     Sector().........Sector number (sec in TBID, encoded in DCPB/TBER)
//     Q()..............Charge (charge in EVNT/q in PART/TBTR) (also Charge())
//     SEBid()..........PDG SEB id number (id in EVNT converted to PDG scheme)
//     PARTid(group)....PDG PID id number (pid in PART bank) (group=which PART
//                        bank is used, 1 or 2)
//     Status().........Overall SEB status flag (status in EVNT bank)
//     DCstat().........SEB DC status flag (dcstat in EVNT bank)
//     CCstat().........SEB CC status flag (ccstat in EVNT bank)
//     SCstat().........SEB SC status flag (scstat in EVNT bank)
//     ECstat().........SEB EC status flag (ecstat in EVNT bank)
//     LCstat().........SEB LC status flag (lcstat in EVNT bank)
//     STstat().........SEB ST status flag (ststat in EVNT bank)
//     SCstatus().......SC status flag (status in SCPB bank)
//     SCid()...........SC paddle id (encoded in SCPB/id in SCRC bank)
//     SCedep().........Energy deposited in SC (edep in SCPB/energy in SCRC)
//     SCpath().........Path Length (cm) (path in SCPB bank)
//     SCchi2().........SC chi^2 (chi2sc in SCPB)
//     SCtime().........Cluster time (ns) (time in SCPB/SCRC banks)
//     STtime().........Start counter time (ns) (st_time in TBID bank)
//     STvtime()........Start counter vertex time (ns) (st_vtime in TBID)
//     P4(scheme,grp)...Returns the particle's 4-momentum as a TLorentzVector,
//                        see P4() for details (p in PART bank)
//     P3().............3-momentum as a TVector3 (GeV)(p in TBTR bank)
//     Px().............Momentum in x-direction
//     Py().............Momentum in y-direction
//     Pz().............Momentum in z-direction
//     Alpha()..........Tracking angle (center of sector, pi/3(sec-1))
//     Vx().............Vertex x-coordinate (cm) (vert.x in PART)
//     Vy().............Vertex y-coordinate (cm) (vert.y in PART)
//     Vz().............Vertex z-coordinate (cm) (vert.z in PART/TBTR/EVNT)
//
// Many of these quantities are also stored elsewhere in a different form, for
// example the 3-momentum is stored in EVNT via pmom(magnitude) and cx,cy,cx
// (direction cosines),etc.
//
// Note: Some quantities that are bit packed have been reduced in precision, 
//       this has almost exclusively been done with calculated values where 
//       the precision kept is still greater than the accuracy of the 
//       calculation (start counter vertex time for example), and for values 
//       where physics analysis doesn't require precision (the SC chi2 is kept
//       as XX.X, which is enough to judge the quality of the SC fit). Some 
//       quantities are also forced to be in a given range (the ranges chosen
//       contain all meaningful values a quantity can take on).
//       Info on this can be found in the documentation for SetStatFlags,
//       SetSCinfo and SetSTinfo.
//
// Note: DC info (from DCPB/TDPL) and tracking covariance matrix elements 
//       (from TBER) are stored in the Tracks class.
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

ClassImp(AParticle)
//_____________________________________________________________________________

AParticle::AParticle(){
  // **********Constructor**********
  this->Zero();
}
//_____________________________________________________________________________

AParticle::AParticle(const AParticle &part){
  // **********Copy Constructor**********
  *this = part;
}
//_____________________________________________________________________________

AParticle::~AParticle(){
  // **********Destructor**********
}
//_____________________________________________________________________________

AParticle& AParticle::operator=(const AParticle &part){
  // Assignment operator
  _P = part._P;
  _Lambda = part._Lambda;
  _Phi = part._Phi;
  _D0 = part._D0;
  _Z0 = part._Z0;
  _Info = part._Info;
  _StatFlags = part._StatFlags;
  _SCinfo1 = part._SCinfo1;
  _SCinfo2 = part._SCinfo2;
  _STinfo = part._STinfo;
  _SCtime = part._SCtime;
  _SCpartInfo1 = part._SCpartInfo1;
  _SCpartInfo2 = part._SCpartInfo2;
  return *this;
}
//_____________________________________________________________________________

void AParticle::SetStatFlags(Int_t status,Int_t dcstat,Int_t ccstat,Int_t scstat,Int_t ecstat,Int_t lcstat,Int_t ststat){
  // Set the particle's "Stat Flags". These flags are the same as those found
  // in the EVNT bank (dcstat is dcstat from EVNT for this particle, etc.).
  //
  // Note: Since these values no longer represent actual pointers, all that 
  //       matters is whether they're 0 or greater than 0. To save space, any 
  //       status flag > 9 is set to be 9. Also, these flags should all be 
  //       positive, if for some reason a negative flag is encountered it will
  //       be set to 0 (status can be < 0) 
  if(status > 9) status = 9;
  if(dcstat > 9) dcstat = 9;
  if(ccstat > 9) ccstat = 9;
  if(scstat > 9) scstat = 9;
  if(ecstat > 9) ecstat = 9;
  if(lcstat > 9) lcstat = 9;
  if(ststat > 9) ststat = 9;
  if(dcstat < 0) dcstat = 0;
  if(ccstat < 0) ccstat = 0;
  if(scstat < 0) scstat = 0;
  if(ecstat < 0) ecstat = 0;
  if(lcstat < 0) lcstat = 0;
  if(ststat < 0) ststat = 0;
  _StatFlags = 1000000*abs(status)+100000*dcstat+10000*ccstat+1000*scstat+100*ecstat+10*lcstat+ststat;
  if(status < 0) _StatFlags *= -1;
}
//_____________________________________________________________________________

void AParticle::SetSTinfo(Float_t st_time,Float_t st_vtime){
  // Set the particle's ST information (acquired from the TBID bank), st_time
  // and st_vtime are stored.
  //
  // Precision Stored: st_time XX.XXX  and  st_vtime XX.XX
  //
  // If abs(time) >= 100. then time is stored as +-99.9, depending on the sign
  //   of time.
  if(TMath::Abs(st_time) >= 100.) st_time = (st_time/TMath::Abs(st_time))*99.9;
  if(TMath::Abs(st_vtime) >= 100.) st_vtime = (st_vtime/TMath::Abs(st_vtime))*99.9;
  _STinfo = ((int)(TMath::Abs(st_vtime)*100.))*100000 + (int)(TMath::Abs(st_time)*1000);
  if(st_vtime < 0.0) _STinfo += 1000000000;
  if(st_time < 0.0) _STinfo *= -1;
}
//_____________________________________________________________________________

void AParticle::SetSCinfo(Int_t status,Float_t chi2,Float_t time,Int_t scid,Float_t path,Float_t edep){
  // Set the particle's SC information (acquired from the SCPB bank), the 
  // following are stored:
  //
  // ***Quantity***    ***SCPB bank variable*** 
  //   SC status                status
  //    SC chi^2                chi2sc
  //    SC time                  time
  //   Path Length               path
  // Deposited Energy            edep
  //   SC paddle id          scpdht(encoded)
  //
  //
  // Precision: chi2 XX.X : path XXX.XXX : sc id XX : edep XX.X : time (full)
  //            status XXXX
  //
  // Note: If chi2 or edep >= 100., they're set to 99.9. If path >= 1000., it's
  //       set to 999.9 (this would be outside the detector anyway) or if it is
  //       negative (??) it's set to 0. If abs(time) >= 200., then time is set 
  //       to +-200. (+- depends on the sign of time) to accomidate the bit 
  //       packing. If scid < 0 (??), it is set to 0. If abs(status) > 2000,
  //       it's set to +- 2000 (status should be a 2 digit number), again to 
  //       accomidate the bit packing.
  if(chi2 >= 100. || chi2 < 0) chi2 = 99.9; 
  if(path >= 1000.) path = 999.9;
  else if(path < 0) path = 0.;
  if(edep >= 100.) edep = 99.9;
  else if(edep < 0.) edep = 0.;
  if(scid < 0) scid = 0;
  _SCinfo1 = scid*1000000 + ((int)(chi2*10.))*1000 + (int)(edep*10.);
  if(TMath::Abs(status) > 2000) status = (status/TMath::Abs(status))*2000;
  _SCinfo2 = TMath::Abs(status)*1000000 + (int)(path*1000.);
  if(status < 0) _SCinfo2 *= -1;
  _SCtime = time;
}
//_____________________________________________________________________________

void AParticle::SetSCpartInfo(Int_t id,Float_t time,Float_t path,Float_t edep){
  // Set SC paddle id, time and path length given which sc hit the PART bank
  // assigns to this particle (unfortunately, part and seb don't always agree
  // on which sc hit goes with a track).
  //
  // Since about 90% of the time part and seb agree on which is the correct SC
  // hit, storing the differences means storing a bit packed integer 0 most of 
  // the time which aids in compression.
  _SCpartInfo1 = 0;
  if(TMath::Abs(time - _SCtime) > 0.001){
    if(time < 0 || time >= 1000.) time = 999.;
    _SCpartInfo1 = (int)(time*10000.);
  }
  if(id != this->SCid("seb")) _SCpartInfo1 += id*10000000;
  _SCpartInfo2 = 0;
  if(path == 0.) _SCpartInfo2 = 1;
  else if(TMath::Abs(path - this->SCpath("seb")) > 0.01){
    if(path < 0. || path >= 1000.) path = 999.;
    _SCpartInfo2 += (int)(path*1000.);
  }
  if(TMath::Abs(edep - this->SCedep("seb")) >= 0.1){
    if(edep >= 100.) edep = 99.;
    _SCpartInfo2 += (int)(edep*10.)*1000000;
  } 
}
//_____________________________________________________________________________

TLorentzVector AParticle::P4(const char* IDscheme,Int_t group) const {
  // Return the particle's 4-momentum. The mass is determined from the particle
  // ID assigned to this particle by "IDscheme" (for the PART id, group is 
  // which PART bank sector's id is to be used 1 or 2, default is group = 1).
  TVector3 p3 = this->P3();
  Float_t mass = this->Mass(IDscheme,group);
  TLorentzVector p4(p3.X(),p3.Y(),p3.Z(),sqrt(p3.Mag2()+mass*mass));
  return p4;
}
//_____________________________________________________________________________

void AParticle::Zero() {
  // Zero ALL of the AParticle data members
  _P = 0.0;
  _Lambda = 0.;
  _Phi = 0.;
  _D0 = 0.;
  _Z0 = 0.;
  _Info = 0;
  _StatFlags = 0;
  _SCinfo1 = 0;
  _SCinfo2 = 0;
  _SCtime = 0.;
  _STinfo = 0;
  _SCpartInfo1 = 0;
  _SCpartInfo2 = 0;
}
//_____________________________________________________________________________

Int_t AParticle::SCstatus(const char *idscheme) const {
  //
  if(!strcmp(idscheme,"seb")) return _SCinfo2/1000000;
  if(!strcmp(idscheme,"part")){
    Int_t tmp = _SCpartInfo2 - (_SCpartInfo2/1000000)*1000000;
    if(tmp == 1) return 0;
    else if(tmp == 0 && _SCinfo2/1000000 == 0) return 0;
    else return 2;
  }
  cout << "Warning! <AParticle::SCstatus> Unknown id scheme " << idscheme << endl;
  return -1;
}
//_____________________________________________________________________________

Int_t AParticle::SCid(const char *idscheme) const {
  //
  if(!strcmp(idscheme,"seb")) return _SCinfo1/1000000;
  if(!strcmp(idscheme,"part")){
    Int_t tmp = _SCpartInfo1/10000000;
    if(tmp == 0) return _SCinfo1/1000000;
    else return tmp;
  }
  cout << "Warning! <AParticle::SCid> Unknown id scheme " << idscheme << endl;
  return 0;
}
//_____________________________________________________________________________

Float_t AParticle::SCedep(const char *idscheme) const {
  // 
  if(!strcmp(idscheme,"seb")){
    Int_t pckd = _SCinfo1-this->SCid("seb")*1000000;
    pckd -= (pckd/1000)*1000;
    return pckd/10.;
  }
  if(!strcmp(idscheme,"part")){
    Int_t tmp = _SCpartInfo2/1000000;
    if(tmp == 0) return this->SCedep("seb");
    else return tmp/10.;
  }
  cout << "Warning! <AParticle::SCedep> Unknown id scheme " << idscheme << endl;
  return 0.;
}
//_____________________________________________________________________________

Float_t AParticle::SCpath(const char *idscheme) const {
  //
  if(!strcmp(idscheme,"seb")) return (TMath::Abs(_SCinfo2)-TMath::Abs(this->SCstatus("seb"))*1000000)/1000.;
  if(!strcmp(idscheme,"part")){
    Int_t tmp = _SCpartInfo2 - (_SCpartInfo2/1000000)*1000000;
    if(tmp == 1) return 0.0;
    if(tmp == 0) return this->SCpath("seb");
    else return tmp/1000.;
  }
  cout << "Warning! <AParticle::SCpath> Unknown id scheme " << idscheme << endl;
  return -1.;
}
//_____________________________________________________________________________

Float_t AParticle::SCtime(const char *idscheme) const {
  //
  if(!strcmp(idscheme,"seb")) return _SCtime;
  if(!strcmp(idscheme,"part")){
    Int_t tmp = _SCpartInfo1 - (_SCpartInfo1/10000000)*10000000;
    if(tmp == 0) return _SCtime;
    else return tmp/10000.;
  }
  cout << "Warning! <AParticle::SCtime> Unknown id scheme " << idscheme << endl;
  return -1.;
}
//_____________________________________________________________________________

Float_t AParticle::IDtoMass(Int_t id) const {
  // Convert a PDG id number to a mass.
  Float_t mass = -1.0;

  if(id == 8 || id == 9) mass = 0.13957;
  else if(id == 11 || id == 12) mass = 0.49368;
  else if(id == 14 || id == 15) mass = 0.93827;
  else if(id == 2 || id == 3) mass = 0.00051100;
  else cout << "<AParticle::IDtoMass> Warning, unknown pdg id flag: " << id << endl;

  return mass;
}
//_____________________________________________________________________________

void AParticle::Print(std::ostream &os) const {
  // Output the contents of the AParticle object using std::ostream os.
  // Default is cout, which dumps the info to the screen.
  os << "Particle IDs: SEB: " << this->SEBid() << " PART (1): " << this->PARTid(1) << " PART(2): " << this->PARTid(2) << endl;
  os << "     Q: " << this->Q() << " P: (" << this->Px() << "," << this->Py() << "," << this->Pz() << ") Sector: " << this->Sector() << endl;
  os << "     Vertex: (" << this->Vx() << "," << this->Vy() << "," << this->Vz() << ") ST: time: " << this->STtime() << " vtime: " << this->STvtime() << endl;
  os << "     SC: status: " << this->SCstatus("seb") << "," << this->SCstatus("part") << " paddle id: " << this->SCid("seb") << "," << this->SCid("part") << " edep: " << this->SCedep("seb") << "," << this->SCedep("part") << " path: " << this->SCpath("seb") << "," << this->SCpath("part") << " time: " << this->SCtime("seb") << "," << this->SCtime("part") << " chi2: " << this->SCchi2() << endl;
  os << "     EVNT flags: status: " << this->Status() << " DC: " << this->DCstat() << " CC: " << this->CCstat() << " SC: " << this->SCstat() << " EC: " << this->ECstat() << " LC: " << this->LCstat() << " ST: " << this->STstat() << endl;
}
//_____________________________________________________________________________

std::ostream& operator<<(std::ostream &os,const AParticle& part){
  // Overload of std::ostream::operator<< for AParticle
  part.Print(os);
  return os;
}
//_____________________________________________________________________________

int geant2pdg(int pid) {
  // converts a geant id to a pdg id
  int result=0;

  if (pid == 22) {          /* photon */
    result=1;
  } else if (pid == -11) {  /* positron */
    result=2;
  }else if (pid == 11) {    /* electron */
    result=3;
  } else if(pid == 12) {    /* neutrino */
    result=4;
  } else if(pid == 14) {    /* neutrino */
    result=4;
  } else if(pid == 16) {    /* neutrino */
    result=4;
  } else if(pid == 13) {    /* mu+ */
    result=5;
  } else if(pid == -13) {   /* mu- */
    result=6;
  } else if (pid == 111) {  /* pi 0 */
    result=7;
  } else if (pid == 211) {  /* pi + */
    result=8;
  } else if (pid == -211) { /* pi - */
    result=9;
  } else if(pid == 130) {   /* K0 long */
    result=10;
  } else if(pid == 321) {   /* K+ */
    result=11;
  } else if(pid == -321) {  /* K- */
    result=12;
  } else if(pid == 2112) {  /* neutron */
    result=13;
  } else if (pid == 2212) { /* proton */
    result=14;
  } else if (pid == -2212) { /* anti-proton */
    result=15;
  } else if (pid == 310) {  /* anti-proton */
    result=16;
  } else if(pid == 40221) { /* eta */
    result=17;
  } else if(pid == 3122) {  /* lambda */
    result=18;
  }
  return(result);
}
