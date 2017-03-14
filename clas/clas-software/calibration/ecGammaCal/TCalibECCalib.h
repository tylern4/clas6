#ifndef TCALIBECCALIB_H
#define TCALIBECCALIB_H


#include <vector>
#include <lorentz.h>
#include "TCalibEC.h"


class TCalibECCalib : public TCalibEC
{
 public:
  TCalibECCalib(int nMinHitsForCalib = 50, double dMinAdcValue = 100.);
  ~TCalibECCalib() { ; }

  void UseGamma(double dAdc, double dTdc, double dVertex2Centroid, double dEcSurface2Edge, double dVertexTime, int nLayer, int nView);  // fills M and V matrices for computing calibration constants
  void ComputeCalibConstants(char* szCalibConstOut = "calibconsts.dat"); // computes and writes out  calibrations constants
  void PrintTubesCounts(const char* szFilename = "tubescounts.dat") const; // prints number of hits in the tubes

 private:
  
  // --------------------------------------------
  // matrices for computing calibration constants
  // --------------------------------------------
  vector< matrix<double> > m_mtxM;  // M-Matrix
  vector< matrix<double> > m_mtxV;  // V-Matrix

  int m_anCounts[m_c_nTotalTubes]; // number of hits in a tube

  // ---------------------
  // const for calibration
  // ---------------------
  const int m_c_nMinHitsForCalib;  // minimum hits in a tube for calibration
 
  //ClassDef (TCalibECCalib, 0)

};

#endif





