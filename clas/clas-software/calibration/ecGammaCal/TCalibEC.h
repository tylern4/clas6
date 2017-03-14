#ifndef TCALIBEC_H
#define TCALIBEC_H

#include "TCalibGamma.h"

class TCalibEC
{
  
 public:
  TCalibEC(double dMinAdcValue = 100.);
  int  FindGammas(BOSbank* pBcs, double dMinGammaEnergy = 0.1, int nGroupIndex = 1); // find gammas in the event for calibration
  bool IsGoodEvent(const clasPART_t *PART) const;  // checks if event has everything for calibration
  virtual void UseGamma(double dAdc, double dTdc, double dVertex2Centroid, double dEcSurface2Edge, double dVertexTime, int nLayer, int nView) = 0;

  // hardware constants
  static const int m_c_nSectors = 6;  // 6 sectors in CLAS
  static const int m_c_nViews = 3;    // 3 views in calorimeter (U, V and W)
  static const int m_c_nLayers = 2;   // 2 layers in calorimeter ("in" and "out")
  static const int m_c_nTubes = 36;   // 36 tubes per layer, view and sector
  static const int m_c_nTotalTubes = 1296;   // Total number of tubes (6 * 3 * 2 * 36)
  static const double m_c_dEffectiveSpeedInPlastic = 18.1; // speed of light in the strip (in cm/s) according to Will Brooks.

  // const error code
  static const double m_c_dError = -1000.;
  static const int m_c_nError = -1000;
  static const int m_c_nOk = -1;
  
  // const for fitting
  static const int m_c_nCalibConsts = 5; // 5 parameters to fit the time
  static const double a0_offset = 40.0; // kludge to add in offset for a0

 protected:
  const double m_c_dMinAdcValue;   // Minimum ADC value in a tube for calibration 
  BOSbank*  m_bcs;   // pointer on the bcs BOS array
  double m_adCalibConsts[m_c_nTotalTubes][m_c_nCalibConsts]; // contains calibration constants for each tube
  double m_adErrorCalibConsts[m_c_nTotalTubes][m_c_nCalibConsts]; // contains statistical errors on calibration constants for each tube
  
  TCalibGamma* m_CurrentGamma;  // current gamma in the event

};


#endif
