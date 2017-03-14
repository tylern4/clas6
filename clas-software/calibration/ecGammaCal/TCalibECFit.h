#ifndef TCALIBECFIT_H
#define TCALIBECFIT_H

#include <TFile.h>
#include <TTree.h>
#include <TH1.h>
#include "TCalibEC.h"

class TCalibECFit : public TCalibEC
{
 
 public:
  TCalibECFit(const char* szCalibConstIn = "calibconsts.dat", const char* szRootOut = "calib.root", double dMinAdcValue = 100., int nMinHitsForFit = 50);
  ~TCalibECFit();

  void UseGamma(double dAdc, double dTdc, double dVertex2Centroid, double dEcSurface2Edge, double dVertexTime, int nLayer, int nView);
  int FitTimeDifference(const char* szFitConstOut = "fitconsts.dat") const;
  bool WriteHistos();

 private:

  // ------------------------
  // Private member functions
  // ------------------------
  void m_InitTree();     // defines tree, contains adc, tdc, ...
  void m_InitHistos();   // defines histograms for Texpected - Tfit
  
  // ---------------------------------------
  // array for storing calibration constants
  // ---------------------------------------
  double m_adCalibConsts[m_c_nTotalTubes][m_c_nCalibConsts];
  
  // -----------------
  // const for fitting
  // -----------------
  const int m_c_nMinHitsForFit;  // minimum hits in a tube for calibration (default is 50)
  
  // ------------------------------------------
  // ROOT variables (file, tree and histograms)
  // ------------------------------------------
  TFile m_fileROOT;
  TTree m_tree;
  TH1F* m_h1Fit[m_c_nTotalTubes]; // contain Texpect - Tfit to check fit quality
  
  // -----------------------------------
  // Variables for filling the ROOT Tree
  // -----------------------------------
  
  struct {
    float fVTime;  // time at vertex;
    float fAdc;  
    float fTdc;
    float fVertex2Centroid; // distance (in cm) from vertex to centroid of shower in the EC
    float fHit2View; // distance from hit position on surface of calorimeter to a given view
    float fTimeExpected; // time computed and expected for a photon
    float fTimeModel;    // time calculated from the model
    int nSector; // sector where hit occured
    int nLayer;  // inner or outer
    int nView;   // U, V or W
    int nTubeId;   // tube ID (1 to 36)
    int nTubeIndex; // tube index (1 to 1296)
  } m_brGamma; // this structure will fill the branch "gamma" in the tree

  float m_afModelTerms[m_c_nCalibConsts];  // contains differents terms for Tmodel

};

#endif


