#include "ROOT.h"
#include "jglobal.h"

double f_tdc(Double_t *x, Double_t *par) {
  return par[0]+par[1]*(*x)+par[2]*(*x)*(*x);
}

double f_tdcp(Double_t *x, Double_t *par) {
  return par[0]+par[1]*(*x);
}

TCanvas* showhisto(SingleStripe* sstr);

void compose_fname (char* s, const char* stub, const int index,
		    int style = 0);

TF1* FunctionTdc(int index, int ii=0) {
  char funname[80];
  compose_fname(funname, "f_tdc", index);
  TF1* fret;
  if(gCalib->GetType() == c_tdcp)
    fret = new TF1(funname, f_tdcp, 0., 8192., 3);
  else{fret = new TF1(funname, f_tdc, 0., 4096., 3);}
  fret->SetParameters (gConst[ii]->GetParameters(index));
  fret->SetFillColor(3);
  fret->SetFillStyle(0);
  return fret;
}

int fittdc(int index, SingleStripe* sstr) {
  double fitpar[3];
  if (! gGraph[0][index]) return -1;

  /// prepare fitfunction
  TF1* ftdc;
  /// parameter set manually ?
  if (sstr && sstr->IsParSet() && gFitFn[0][index])
    ftdc = gFitFn[0][index];
  else{
    if(gCalib->GetType() == c_tdcp)
      ftdc = new TF1("ftdc",f_tdcp,0.,8192.,3);
    else{ftdc = new TF1("ftdc",f_tdc,0.,4096.,3);}
    gCalib->SetDefaultPar(ftdc);
  }
  gCalib->SetFitLimits(ftdc);

  TGraphErrors* gcopy = 
    (TGraphErrors*) gGraph[0][index]->Clone();

  /// is range set?
  double x, y;
  if (sstr && sstr->IsLimits() ) {
    for (int i=gcopy->GetN()-1; i>=0; i--) {
      gcopy->GetPoint(i,x,y);
      if (! sstr->GetCut()->IsInside(x,y) ) gcopy->RemovePoint(i);
    }
  }

/*  double locAverageTime = 0.0;
  //make sure y's aren't out of range...
  for (int i=gcopy->GetN()-1; i>=0; i--) {
    gcopy->GetPoint(i,x,y);
    locAverageTime += y;
  }
  if(gcopy->GetN() != 0)
    locAverageTime /= gcopy->GetN();
  for (int i=gcopy->GetN()-1; i>=0; i--) {
    gcopy->GetPoint(i,x,y);  
    if((y < 0) || (y > 3.0*locAverageTime)) gcopy->RemovePoint(i);
  } */

  int retvalue;
  if ((retvalue = gcopy->Fit(ftdc,"0q"))) return retvalue;

  if (gFitFn[0][index]) 
    gFitFn[0][index]->SetParameters(ftdc->GetParameters());
  else {
    gFitFn[0][index]=ftdc;
    char fname[80];
    sprintf (fname, "ftdc%03d", index);
    gFitFn[0][index]->SetName(fname);
  } 

  gConst[0]->SetParameters(index, ftdc);

  /// create second graph
  ftdc->GetParameters(fitpar);

  TGraphErrors* gpoly = 
    (TGraphErrors*) gGraph[0][index]->Clone();
  if(gCalib->GetType() != c_tdcp){
    for (int i=0; i<gpoly->GetN(); i++) {
      double x, y;
      gpoly->GetPoint(i,x,y);
      y -= fitpar[0] + fitpar[1] * x;
      gpoly->SetPoint(i,x,y);
    }
    TF1* fpoly = new TF1("fpoly", "[0]*x*x", 0., 4096.);
    fpoly->SetParameter(0,fitpar[2]);
    fpoly->SetLineColor(2);
    gpoly->GetListOfFunctions()->Add(fpoly);
  }
  gGraph[1][index] = gpoly;
  
  if (sstr) {
    showhisto (sstr);
    //    TCanvas* Cfit = showhisto (sstr);
  }
  // delete gcopy;
  return 0;
}
