#include <stream.h>
#include <fstream.h>
#include <stdlib.h>
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
#include <TFrame.h>
#include <TRint.h>
#include <TLatex.h>
#include <TPaveLabel.h>
#include <TDialogCanvas.h>
#include <TButton.h>
#include <TSystem.h>
#include <TDirectory.h>
#include <TLine.h>
#include <TMarker.h>
#include <TPad.h>
#include <TVirtualPS.h>
#include <TPadView3D.h>
#include <THistPainter.h>
#include <TView.h>
#include <Hoption.h>
#include <Hparam.h>

#include "JRefit.h"
#include "JSurvey.h"
#include "jdefine.h"

using namespace std;

TRint*   theApp;
JRefit*  todo[N_CHANNEL];
TH1F*    hcheck[4];
TDirectory* gdir;
TCanvas* Cr;

int compare(const void* a_, const void* b_) {
  double a = *((double*)a_);
  double b = *((double*)b_);
  if (a==b) return 0;
  return (a<b ? -1 : 1);
}

double median(double* vec, int n) {
  qsort (vec, n, sizeof(double), compare);
  int i=0;
  while (i<n && vec[i]==0.) i++;
  int j= (n-i)/2;
  return vec[i+j];
}

void updateSurvey(int i) {
  if (todo[i]->TrustFit()) {
    hcheck[0]->  SetBinContent(i+1, todo[i]->GetGaussMean());
    hcheck[1]->  SetBinContent(i+1, todo[i]->GetGaussSigma());
    hcheck[2]->  SetBinContent(i+1, todo[i]->GetGaussArea());
    hcheck[3]->  SetBinContent(i+1, todo[i]->GetChisquare());
  }
}

int main () {
// initializer for GUI needed for interactive interface
  extern void  InitGui();
  VoidFuncPtr_t initfuncs[] = { InitGui, 0 };

  gStyle->SetPalette(1);
  gStyle->SetOptStat(0);
  TROOT analysis("ANALYSIS","PHYSICS ANALYSIS PROGRAM", initfuncs );
  cout << "ROOT Framework initialization done..\n" << endl;
  theApp = new TRint("Interactive", 0, 0, 0, 0, 0 );
  

  //----------------------- edepfit --------------------

  gdir = new TDirectory("gdir", "memory top directory");
  Cr = new TCanvas ("Cr", "manual refit", 700, 700);
  TH1F* hedep [N_CHANNEL];
  TH2F* hetau [N_CHANNEL];


  hcheck[0] =  new TH1F("hvmean",  "Gauss Mean",  N_CHANNEL, -0.5, float(N_CHANNEL) - 0.5);
  hcheck[1] =  new TH1F("hvsigma", "Gauss Sigma", N_CHANNEL, -0.5, float(N_CHANNEL) - 0.5);
  hcheck[2] =  new TH1F("hvarea",  "Gauss Area",  N_CHANNEL, -0.5, float(N_CHANNEL) - 0.5);
  hcheck[3] =  new TH1F("hvchisq", "Chisquare",   N_CHANNEL, -0.5, float(N_CHANNEL) - 0.5);

  //  hcheck[0]->Draw();
  char* filename = "/home/langhei/root/edeptau.root";
  //  char* filename1 = "/home/langhei/root/dep2/gmean1d.root";
  //  char* filename2 = "/home/langhei/root/dep2/gmean2d.root";

  TFile* fi = new TFile (filename);
  for (int i=0; i<N_CHANNEL; i++) {
    char name [80];
    int isec    = i/N_SECTOR + 1;
    int istripe = i%N_SECTOR + 1;
    //    sprintf (name, "h%03d", isec*100+istripe);
    sprintf (name, "hedep%03d", i);
    if (! (hedep[i] = (TH1F*) fi->Get(name))) {
      cerr << "Histogram <" << name << "> not found in " << filename << endl;
      exit(1);
    }
    sprintf (name, "hetau%03d", i);
    if (! (hetau[i] = (TH2F*) fi->Get(name))) {
      cerr << "Histogram <" << name << "> not found in " << filename << endl;
      exit(1);
    }
  }
//   TFile* fi2 = new TFile (filename2);
//   for (int i=0; i<N_CHANNEL; i++) {
//     char name [80];
//     int isec    = i/N_SECTOR + 1;
//     int istripe = i%N_SECTOR + 1;
//     sprintf (name, "h%03d", isec*100+istripe);
//     if (! (hetau[i] = (TH2F*) fi2->Get(name))) {
//       cerr << "Histogram <" << name << "> not found in " << filename2 << endl;
//       exit(1);
//     }
//   }

  gdir->cd();
  double sum;
  double vecSigma [N_CHANNEL];
  memset (vecSigma, 0, sizeof(vecSigma));

  for (int i=0; i<N_CHANNEL; i++) {
    cout << i << "\r";
    cout.flush();
    todo[i] = new JRefit(hedep[i], hetau[i], i, NULL);
    updateSurvey(i);
    if (todo[i]->TrustFit()) {
      vecSigma[i] = todo[i]->GetGaussSigma(); 
    }
      //      Cr->Modified();
      //      Cr->Update();
  }

  double medSigma = median(vecSigma, N_CHANNEL);
  cout << "median Sigma: " << medSigma << ", going for 3 sigma refit" << endl;

  for (int i=0; i<N_CHANNEL; i++) {
    cout << i << "\r";
    cout.flush();
    if (todo[i]->GetGaussSigma() > medSigma) {
      todo[i]->Refit3Sigma(medSigma);
      updateSurvey(i);
    }
  }

  JSurvey* js = new JSurvey(todo, hcheck);  

  theApp->Run();
}
