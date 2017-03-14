#include "stpp_SThit.h"
#include "stpp_JCalibration.h"
extern JCalibration* gCalib;

SThit::SThit(const STdata& t, const STposition& p): 
  STdata(t), STposition(p) {};

inline double SQR(double x) { return x*x; }

double SThit::GetHitTime() {
  /// leg
  if (IsLeg()) 
    return time - GetPosition() / gCalib->GetFloat(stn_veff_leg,index);

  /// nose
  return time 
    - GetLegLength()    / gCalib->GetFloat(stn_veff_leg,index)
    - GetNosePosition() / gCalib->GetFloat(stn_veff_nose,index) 
    - gCalib->GetFloat(stn_veff_nose1,index)
    - gCalib->GetFloat(stn_veff_nose2,index) * SQR(GetNosePosition());
}

void SThit::FetchToBos(str_t* strptr) {
  strptr->id      = GetIndexDigits() + (IsNose() ? 2 : 1);
  strptr->trk_no  = GetTrackIndex()+1;
  strptr->st_time = GetHitTime();
  strptr->st_l    = GetTrackLength();
  strptr->st_pos  = GetPosition();	
  strptr->status  = STRstatus();
}

void SThit::FetchToBos(stre_t* streptr) {
  streptr->id      = GetIndexDigits() + (IsNose() ? 2 : 1);
  streptr->trk_no  = GetTrackIndex()+1;
  streptr->st_time = GetHitTime();
  streptr->st_l    = GetTrackLength();
  streptr->st_pos  = GetPosition();	
  streptr->st_edep = GetEnergyDeposit();
  streptr->st_corr = GetGeoScale();
  streptr->status  = STRstatus();
}

double SThit::GetEnergyDeposit() {
  double pos = GetPosition();
  double zr1 = gCalib->GetFloat(stn_adc2edep_reg1limit,index);
  double zr2 = z1 - z0;
  double posCorrection = 0;

  /// region 1 of leg
  if (pos < zr1) 
    posCorrection = 
      gCalib->GetFloat(stn_adc2edep_reg1scale,index) 
      * exp(pos * gCalib->GetFloat(stn_adc2edep_reg1exp,index) )
      + gCalib->GetFloat(stn_adc2edep_reg1const,index);
  
  /// region 3 (nose)
  else if (pos > zr2)
    posCorrection = 
      gCalib->GetFloat(stn_adc2edep_reg3par0,index) +
      gCalib->GetFloat(stn_adc2edep_reg3par1,index) * pos +
      gCalib->GetFloat(stn_adc2edep_reg3par2,index) * pos * pos;

  /// region 2 of leg
  else {
    double z = pos - zr1;
    posCorrection =
      gCalib->GetFloat(stn_adc2edep_reg1scale,index) 
      * exp(zr1 * gCalib->GetFloat(stn_adc2edep_reg1exp,index) )
      + gCalib->GetFloat(stn_adc2edep_reg1const,index)
      + gCalib->GetFloat(stn_adc2edep_reg2par1,index) * z
      + gCalib->GetFloat(stn_adc2edep_reg2par2,index) * z * z;
  }

  if (posCorrection == 0.) return -1.;
  double edep = adc * gCalib->GetFloat(stn_adc2edep_mip_energy, 0) / posCorrection;
  if (edep < 0) return 0.;
  return edep;
}
