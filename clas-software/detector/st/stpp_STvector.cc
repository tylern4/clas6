#include <iostream>

#include "stpp_STvector.h"

extern "C" {
#include "bostypes.h"
#include <string.h>
}
#include "stpp_JBosBank.h"
#include "stpp_JCalibration.h"

extern JCalibration* gCalib;

STvector::STvector() {
  Jstn0 stn0;        /// get ST TDC raw bank
  Jstn1 stn1;        /// get ST ADC raw bank

  /// check whether both banks are available
  if (stn0.Found() && stn1.Found()) {

    /// store raw ADC in array, clear arry first
    int stadc[ST_MAX];
    memset (stadc, 0, sizeof(stadc));
    for (int istn1=0; istn1<stn1.GetNrows(); istn1++) {
      stadc[stn1[istn1].id-1] = stn1[istn1].adc;
    }

    /// store TDC in vector, allowing multiple entries
    for (int istn0=0; istn0<stn0.GetNrows(); istn0++) {
      int inx = stn0[istn0].id - 1;
      double adc = stadc[inx];
      int tdc = stn0[istn0].tdc;

      if (InWindow(adc, tdc)) {
        vstdata[inx].push_back(STdata(inx, adc, tdc));
      }
    }
  }
}

int STvector::size() {
  int sum = 0;
  for (int j=0; j<ST_MAX; j++) {
    sum+=vstdata[j].size();
  }
  return sum;
}

int STvector::BestSegment(int sector, int segment8) {

  /// the search order for a segment[0..7] is a row in segment_order
  static const int segment_order[8][4] = {
    { 0, 1, 2, 3},
    { 0, 1, 2, 3},
    { 1, 0, 2, 3},
    { 1, 2, 0, 3},
    { 2, 1, 3, 0},
    { 2, 3, 1, 0},
    { 3, 2, 1, 0},
    { 3, 2, 1, 0},
  };

  /// neg return value => not found anything
  int bestsegment = -1;

  for (int i=0; i<4; i++) {
    int j = (sector-1) * 4               /// calculate index to ST: sector[1..6]...
      + segment_order[segment8][i];      /// ... and segment4 [0..3]
    if (vstdata[j].size()) {             /// any data entries for given index ?
      bestsegment = j;                   /// yes: store index ...
      break;                             ///      ... and don't look any further
    }
  }
  return bestsegment;
}

bool STvector::InWindow(double adc, int tdc) {
  if (adc < 0) return false;
  if (tdc < gCalib->GetFloat(stn_TDC_min_value, 0)) return false;
  if (tdc > gCalib->GetFloat(stn_TDC_max_value, 0)) return false;
  return true;
}
