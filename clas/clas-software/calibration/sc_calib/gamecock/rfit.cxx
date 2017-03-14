#include "ROOT.h"

void SetGausParameters(TF1 *f, TH1D* h) {
  double par[3];
  par[0] = h->GetMaximum()*2./3.;
  par[1] = h->GetBinCenter(h->GetMaximumBin());
  par[2] = h->GetRMS()/2.;
  f->SetParameters(par);
};

void TH2::FitSlicesY(TF1 *f1, Int_t binmin, Int_t binmax, Int_t cut, Option_t *option)
{
// Project slices along Y in case of a 2-D histogram, then fit each slice
// with function f1 and make a histogram for each fit parameter
// Only bins along X between binmin and binmax are considered.
// if f1=0, a gaussian is assumed
// Before invoking this function, one can set a subrange to be fitted along Y
// via f1->SetRange(ymin,ymax)
// The argument option (default="QNR") can be used to change the fit options.
//     "Q" means Quiet mode
//     "N" means do not show the result of the fit
//     "R" means fit the function in the specified function range
//
// Note that the generated histograms are added to the list of objects
// in the current directory. It is the user's responsability to delete
// these histograms.
//
//  Example: Assume a 2-d histogram h2
//   Root > h2->FitSlicesY(); produces 4 TH1D histograms
//          with h2_0 containing parameter 0(Constant) for a Gaus fit
//                    of each bin in X projected along Y
//          with h2_1 containing parameter 1(Mean) for a gaus fit
//          with h2_2 containing parameter 2(RMS)  for a gaus fit
//          with h2_chi2 containing the chisquare/number of degrees of freedom for a gaus fit
//
//   Root > h2->FitSlicesY(0,15,22,10);
//          same as above, but only for bins 15 to 22 along X
//          and only for bins in X for which the corresponding projection
//          along Y has more than cut bins filled.
//
// A complete example of this function is given in  tutorial:fitslicesy.C 
// with the following output:
//
/*
 <http://ific.uv.es/informatica/manuales/root_000723/html/src/gif/fitslicesy.gif> 
*/
//

  bool isGaus = false;

Int_t nbins  = fXaxis.GetNbins();
   if (binmin < 1) binmin = 1;
   if (binmax > nbins) binmax = nbins;
   if (binmax < binmin) {binmin = 1; binmax = nbins;}

//default is to fit with a gaussian
if (f1 == 0) {
  isGaus = true;
      f1 = (TF1*)gROOT->GetFunction("gaus");
      if (f1 == 0) f1 = new TF1("gaus","gaus",fYaxis.GetXmin(),fYaxis.GetXmax());
      else         f1->SetRange(fYaxis.GetXmin(),fYaxis.GetXmax());
   }
   const char *fname = f1->GetName();
   Int_t npar = f1->GetNpar();
   Double_t *parsave = new Double_t[npar];
   f1->GetParameters(parsave);

//Create one histogram for each function parameter
Int_t ipar;
   char name[80], title[80];
   TH1D *hlist[25];
   TArrayD *bins = (TArrayD*) fXaxis.GetXbins();
   for (ipar=0;ipar<npar;ipar++) {
      sprintf(name,"%s_%d",GetName(),ipar);
      sprintf(title,"Fitted value of par[%d]=%s",ipar,f1->GetParName(ipar));
      if (bins->fN == 0) {
         hlist[ipar] = new TH1D(name,title, nbins, fXaxis.GetXmin(), fXaxis.GetXmax());
      } else {
         hlist[ipar] = new TH1D(name,title, nbins,bins->fArray);
      }
      hlist[ipar]->GetXaxis()->SetTitle(fXaxis.GetTitle());
   }
   sprintf(name,"%s_chi2",GetName());
   TH1D *hchi2 = new TH1D(name,"chisquare", nbins, fXaxis.GetXmin(), fXaxis.GetXmax());
   hchi2->GetXaxis()->SetTitle(fXaxis.GetTitle());

//Loop on all bins in X, generate a projection along Y
Int_t bin;
   Int_t nentries;
   for (bin=binmin;bin<=binmax;bin++) {
     //      TH1D *hpy = ProjectionY("_temp",bin,bin,"e");
      TH1D *hpy = ProjectionY("_temp",bin,bin);
      if (hpy == 0) continue;
      nentries = Int_t(hpy->GetEntries());
      if (nentries == 0 || nentries < cut) {delete hpy; continue;}
      if (isGaus) {
	SetGausParameters (f1, hpy);
      }
      else
	f1->SetParameters(parsave);
      cout << "**Par: " << f1->GetParameter(1) << endl;
      hpy->Fit(fname,option);
      gPad->Modified();
      gPad->Update();
      Int_t npfits = f1->GetNumberFitPoints();
      if (npfits > npar && npfits >= cut) {
         for (ipar=0;ipar<npar;ipar++) {
            hlist[ipar]->Fill(fXaxis.GetBinCenter(bin),f1->GetParameter(ipar));
            hlist[ipar]->SetBinError(bin,f1->GetParError(ipar));
         }
         hchi2->Fill(fXaxis.GetBinCenter(bin),f1->GetChisquare()/(npfits-npar));
      }
      delete hpy;
   }
   delete [] parsave;
}


TRint*   theApp;


void main (int argc, char* argv[]) {
// initializer for GUI needed for interactive interface
  extern void  InitGui();
  VoidFuncPtr_t initfuncs[] = { InitGui, 0 };

  TROOT analysis("ANALYSIS","PHYSICS ANALYSIS PROGRAM", initfuncs );
  cout << "ROOT Framework initialization done..\n" << endl;
  theApp = new TRint("Interactive", 0, 0, 0, 0, 0 );
  
  //-----------------------insert user program here...

  gStyle->SetPalette(1);
  TFile* f = new TFile ("veff.root");
  TCanvas* C = new TCanvas("C","fit slices", 800, 800);
  TH2F* h333 = (TH2F*) f->Get("h333");
  h333->FitSlicesY(0,1,100,0,"");
  
  TH1D* h0 = (TH1D*) gROOT->FindObject("h333_0");
  TH1D* h1 = (TH1D*) gROOT->FindObject("h333_1");
  TH1D* h2 = (TH1D*) gROOT->FindObject("h333_2");
  C->Divide(2,2);
  C->cd(1);
  h0->Draw();
  C->cd(2);
  h1->Draw();
  C->cd(3);
  h2->Draw();
  C->cd(4);
  h333->Draw("colz");

  //-----------------------end user program...........

  theApp->Run();
}
