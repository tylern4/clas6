#include "stpp_STdata.h"
#include "stpp_JCalibration.h"

extern JCalibration* gCalib;

STdata::STdata(int inx_, int rawADC, int tdc_) : index(inx_), tdc(tdc_) {
  adc = rawADC - gCalib->GetFloat(stn_pedestals_value, index);
  time =
    - gCalib->GetFloat(stn_delta_T_st2tof, 0)
    - gCalib->GetFloat(stn_delta_T_s2s, index/4)
    + gCalib->GetFloat(stn_delta_T_pd2pd, index)
    + gCalib->GetFloat(stn_T0_value, index)
    - gCalib->GetFloat(stn_T1_value, index) * tdc
    - GetTimewalk();
}

int STdata::GetIndexDigits() {
  int sector = index / 4 + 1;
  int segmnt = index % 4 + 1;
  return sector * 100 + segmnt * 10;
}

double STdata::GetTimewalk() {
  const double thresh=50;
  double adc_max = gCalib->GetFloat(stn_adc_max_value, index);
  double w0 = gCalib->GetFloat(stn_W0_value, index);
  double w1 = gCalib->GetFloat(stn_W1_value, index);
  double w2 = gCalib->GetFloat(stn_W2_value, index);

  if (adc>adc_max)
    return -(w0+w1/(adc_max-w2));

  if (adc>thresh)
    return -(w0+w1/(adc-w2));

  if (adc>0)
    return -(w0+w1/(thresh-w2));

  return 0;
}

void STdata::FetchToBos(st1_t* st1ptr) {
  st1ptr->id      = GetIndexDigits();
  st1ptr->status  = 2;
  st1ptr->time_1  = GetTime();
  st1ptr->adc_1   = adc;
  st1ptr->time_2  = 0.;
  st1ptr->adc_2   = 0.;
}

