#ifndef __TShowCalib__
#define __TShowCalib__ 

#include "c_stds.h"
#include "TH1.h"
#include "TH2.h"
#include "TF1.h"
#include "TChain.h"
#include "TText.h"
#include "TStyle.h"
#include "TFrame.h" 
#include "TPaveLabel.h"
#include "TRandom.h" 
#include "TGraphErrors.h"
#include "TPad.h"
#include "TCanvas.h"
#include "TControlBar.h"
#include "TFile.h"
#include "TROOT.h"

#include "NumStrips.h"

#define N_SECTS 6			// Number of Sectors
#define N_MAX_STRIPS 57
#define N_MAX_TOTAL_STRIPS 342

//int N_STRIPS = 48;
//int N_TOTAL_STRIPS = 288;
const int MC_RUN_NUMBER = 1;

class TShowCalib
{
 public:

  TShowCalib( const char* RootFileName = " " );
  //  TShowCalib( TFile* RootFile );
  ~TShowCalib();
  void ShowMassAlignment( int level );
  void ShowMassSpec() ;
  void ShowBvsP();
  void ShowBvsP( int Sector ) ;
  void ShowFine( int iSect ) ;
  void ShowGoodRF() ; 
  void ShowConstants() ; 
  void ShowResolutions() ; 

 private:

  char fRootFileName[256] ;
  TFile* fRootFile ;
  TControlBar* fTOF_Bar ;
  TCanvas* fMainCanvas;
  TCanvas* fCanvM[2] ;
  
} ;

#endif






