#ifndef _MAIN_MENU_H
#define _MAIN_MENU_H

#include <stdio.h>
#include <iostream>
#include <fstream>
#include <TROOT.h>
#include <TCanvas.h>
#include <TVector.h>
#include <TGraph.h>
#include <TGraphErrors.h>
#include <TControlBar.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <TStyle.h>
#include <TFile.h>
#include <TRint.h>
#include <TLatex.h>
#include <TPaveLabel.h>
#include <TDialogCanvas.h>
#include <TButton.h>
#include <TSystem.h>
#include <TLine.h>

extern "C" {
  int sc_index(int sec, int stripe);
}

enum write_t { w_hist, w_fitl, w_poly };

class JMainMenu {
  TCanvas* C;
  char    constOutDir[80];
  char    constOutMask[80];
  int  runNumber;
  int  showWhat;
  bool showRight;
  int  showSector;
  int  showStripe;
  int  showParam;
  class ShowChannel {
    TGraphErrors* g1;
    TGraphErrors* g2;
    TF1*          p2;
    TH1F*       hist;
  public:
    double parVal[4];
    double parErr[3];
    ShowChannel(TGraphErrors* g1_, TH1F* hist_, double* parVal_, double* parErr_);
    void ModifyTitleSwapped(int newIndex);
    void DrawFit(int soWhat);
    void Write(write_t what);
  };
    
  ShowChannel* sc[342] [2];
  TGraphErrors* sp[3] [2];
  TH2F* gs [6];
  TDialogCanvas* td;
  char commentWhat[3] [80];
public:
  JMainMenu();
  JMainMenu(int runNumber_, char* constOutDir_);
  ~JMainMenu();
  void SetCanvas(TCanvas* C_) {C=C_;}
  void SetGaussHisto(TH2F** gs_) {for(int i=0;i<6;i++) gs[i]=gs_[i];}
  void SetChannel(int ichan, int noswap, int lr, TGraphErrors* g1_, TH1F* hist_,
		  double* parVal_, double* parErr_);
  void SetOverview();
  void StartMenu();
  void CloseChannelMenu();
  void ChannelMenu();
  void DrawChannel(int sector, int stripe);  // stripe == 0 -> whole sector
  void DrawOverview(int deg);
  void DrawGaussHisto(int i);
  void InitCanvas(const char* title);
  void ToggleLeftRight ();
  void TogglePresentation (int newValue);
  int  WriteResult();
  void WriteDatabase();
  void WriteHisto(char* rootFileName);
};

#endif
