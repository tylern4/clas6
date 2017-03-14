#ifndef __TDCHISTOGRAM_H
#define __TDCHISTOGRAM_H

using namespace std;

#include <stdio.h>
#include <iostream>
#include <string.h>
#include <map>
#include <vector>
#include <string>
#include <sstream>
#include <cmath>
#include <TROOT.h>
#include <TCanvas.h>
#include <TVector.h>
#include <TGraph.h>
#include <TMath.h>
#include <TGraphErrors.h>
#include <TControlBar.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <TStyle.h>
#include <TFile.h>
#include <TRint.h>
#include "BankDescription.h"

#define HIGHEST_CHANNEL 4090
#define HIGHEST_CHANNEL_PIPELINE 4500

class GaussFitResult {
public:
  double  t;
  double  mean;
  double  sigma;
  double  area;
  double  errMean;
  double  errSigma;
  double  errArea;
  double  chiSquare;
  int     sum;
  GaussFitResult (double* par, double* errpar, double chisq, double t_, int sum_);
  GaussFitResult (double mean_, double rms, double t_, int sum_);
  bool bad (bool pipeline);
};

class TdcHistogram {
  int chan;              // detector channel
  BankDescription* bd;    // bank info
  TGraphErrors* rootFit; // contain fit
  TH1F* rootHist;        // raw data histogram
  double expect;
  map<int,int> entry; //1st index (or key) is tdc value, 2nd index (or value) is num hits
  vector<GaussFitResult> fitresult;
  double polypar [4];
  double polyerr [3];
public:
  static bool useGaussian;
  TdcHistogram (int chan_, BankDescription* bd_);
  void   Fill  (int tdc, bool pipeline);
  void   Write ();
  void   PolynomialFit (bool pipeline);
  void   DrawFit(bool onlyT2);
  double GetT0 ()          {return polypar [0];}
  double GetT1 ()          {return polypar [1];}
  double GetT2 ()          {return polypar [2];}
  double GetT0Error ()          {return polyerr [0];}
  double GetT1Error ()          {return polyerr [1];}
  double GetT2Error ()          {return polyerr [2];}
  void   SubtractAverageT0 (double average) {polypar [0]-= (polypar [3]=average); }
  double* GetPar()    {return polypar;}
  double* GetErr()    {return polyerr;}
  string  GetName(string stub);
  TH1F*   GetHisto()  {return rootHist;} 
  TGraphErrors*   GetGraph() { return rootFit;}
  GaussFitResult* GaussFit (double texp, int locChannel);
  GaussFitResult* GetAverage (double texp);
  GaussFitResult* leaveGaussFit (GaussFitResult* result, double* x, double* y);
  friend ostream& operator << (ostream&, const TdcHistogram&);
};

#endif
