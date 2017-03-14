#ifndef __ST_DATA_H__
#define __ST_DATA_H__

extern "C" {
#include "ntypes.h"
#include "bostypes.h"
}

const int ST_MAX=24;

/// intermediate results for ST
/// apply TDC and timewalk calibration on TDC,
/// offset subtraction on ADC
class STdata {
protected:
  int index;
  double adc;
  int tdc;
  double time;
public:
  /// empty constructor makes compiler happy
  STdata(): index(-1), adc(0.), tdc(0), time(0.) {}

  /// parameters for constructor: index 0..23, raw ADC, raw TDC
  STdata(int inx_, int rawADC, int tdc_);

  /// returns timewalk correction
  double GetTimewalk();

  /// returns index as sector*100 + paddle*10
  int GetIndexDigits();

  /// returns index 0..23
  int GetIndex() const   { return index; }

  /// returns time[ns] after TDC calibration and timewalk correction
  double GetTime() const { return time; }

  /// returns ADC after offset subtraction
  double GetADC() const  { return adc; }

  /// returns raw TDC
  double GetTDC() const  { return tdc; }

  /// copy information to ST1 bos bank
  void FetchToBos(st1_t* st1ptr);

};

#endif
