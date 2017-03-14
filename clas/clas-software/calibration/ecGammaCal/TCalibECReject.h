#ifndef TCALIBECREJECT_H
#define TCALIBECREJECT_H

#include "TCalibEC.h"

class TCalibECReject : public TCalibEC
{
 
 public:
  TCalibECReject(const char* szCalibConstIn = "calibconsts.dat", const char* szFitConstIn = "fitconsts.dat", double dMinAdcvalue = 100., int nSigmaNumber = 2);
  ~TCalibECReject() { ; }

  void UseGamma(double dAdc, double dTdc, double dVertex2Centroid, double dEcSurface2Edge, double dVertexTime, int nLayer, int nView);

 private:
  double m_adCalibConsts[m_c_nTotalTubes][m_c_nCalibConsts]; 
  double m_adFitCentroid[m_c_nTotalTubes]; // contains centroid of Gaussian for each tube
  double m_adFitSigma[m_c_nTotalTubes];    // contains sigma of Gaussian for each tube
  int m_nNumberSigmas; // number of sigmas from centroid to keep gamma

};

#endif

