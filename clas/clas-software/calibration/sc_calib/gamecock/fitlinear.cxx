#include "ROOT.h"
#include "jglobal.h"
#include "Regression.h"

double * xp;
double * yp;
double * np;
double * erry;

double QUAD(double x) {return x*x;} ;

void compose_fname (char* s, const char* stub, const int index,
		    int style = 0);

double distance (double *x, double *par) {
  int i = (int) *x;
  double b0 = par[0];
  double m0 = par[1];
  double a0 = (yp[i] - m0*xp[i] - b0) / sqrt (m0*m0+1.);
  return  a0 ;
}

double f_veff (double *x, double *par) {
  if (par[1] == 0){
    cout << "f_veff: slope 0" << endl;
    return 0.0;
  }
  return par[0] + 2./par[1]*x[0];
}

double f_atten (double *x, double *par) {
  if (par[1] == 0){
    cout << "f_atten: slope 0" << endl;
    return 0.0;
  }
  return par[0] - 2./par[1]*x[0];
}

TF1* FunctionAtten(int index, int ii=0) {
//   double xbins  = gHisto[0][index]->GetXaxis()->GetNbins();
//   double ybins  = gHisto[0][index]->GetYaxis()->GetNbins();
  if(gHisto[0][index] == NULL)
    return NULL;

  double x0  = gHisto[0][index]->GetXaxis()->GetXmin();
  double x1  = gHisto[0][index]->GetXaxis()->GetXmax();
//   double y0  = gHisto[0][index]->GetYaxis()->GetXmin();
//   double y1  = gHisto[0][index]->GetYaxis()->GetXmax();

  char funname[80];
  compose_fname(funname, "f_atten", index);

  TF1* fret = new TF1(funname, f_atten, x0, x1, 2);
  fret->SetParameters (gConst[ii]->GetParameters(index));
  fret->SetFillColor(3);
  fret->SetFillStyle(0);
  return fret;
}

TF1* FunctionVeff(int index, int ii=0) {
  if(gHisto[0][index] == NULL)
    return NULL;

//   double xbins  = gHisto[0][index]->GetXaxis()->GetNbins();
//   double ybins  = gHisto[0][index]->GetYaxis()->GetNbins();
  double x0  = gHisto[0][index]->GetXaxis()->GetXmin();
  double x1  = gHisto[0][index]->GetXaxis()->GetXmax();
//   double y0  = gHisto[0][index]->GetYaxis()->GetXmin();
//   double y1  = gHisto[0][index]->GetYaxis()->GetXmax();

  char funname[80];
  compose_fname(funname, "f_veff", index);
  TF1* fret = new TF1(funname, f_veff, x0, x1, 2);
  fret->SetParameters (gConst[ii]->GetParameters(index));
  fret->SetFillColor(3);
  fret->SetFillStyle(0);
  //  cout << "FunctionVeff " << fret->GetParameter(0) << " " << fret->GetParameter(1) << endl;;
  return fret;
}

/// generate veff/atten function, b and m given in histogram parameters
TF1* FunctionLinear(double b, double m, int index) {
  if(gHisto[0][index] == NULL)
    return NULL;

  double xbins  = gHisto[0][index]->GetXaxis()->GetNbins();
  double ybins  = gHisto[0][index]->GetYaxis()->GetNbins();
  double x0  = gHisto[0][index]->GetXaxis()->GetXmin();
  double x1  = gHisto[0][index]->GetXaxis()->GetXmax();
  double y0  = gHisto[0][index]->GetYaxis()->GetXmin();
  double y1  = gHisto[0][index]->GetYaxis()->GetXmax();

  if (m == 0.) throw "FunctionLinear: slope 0";
  double mpar = 2/m * ybins/xbins * (x1-x0)/(y1-y0);
  double bpar = -(m * xbins/ybins * (y1-y0)/(x1-x0) *x0
		  - b * (y1-y0)/ybins - y0) ; 
  switch (gCalib->GetType()) {
  case c_veff:     
    gConst[0]->SetParameters(index, bpar, mpar);
    return FunctionVeff(index);
  case c_atten:    
    gConst[0]->SetParameters(index, bpar, -mpar);
    return FunctionAtten(index);
  default:
    cerr << "FunctionLinear: not supported type " << *gCalib << endl;
    throw "FunctionLinear: calibration type not supported";
  }
}

TF1* FunctionLinear(TF1* fx, int index) {
  if(gHisto[0][index] == NULL)
    return NULL;

  double xbins  = gHisto[0][index]->GetXaxis()->GetNbins();
  double ybins  = gHisto[0][index]->GetYaxis()->GetNbins();
  double x0  = gHisto[0][index]->GetXaxis()->GetXmin();
  double x1  = gHisto[0][index]->GetXaxis()->GetXmax();
  double y0  = gHisto[0][index]->GetYaxis()->GetXmin();
  double y1  = gHisto[0][index]->GetYaxis()->GetXmax();

  TF1* fret = FunctionLinear(fx->GetParameter(0), fx->GetParameter(1), index);
  double relErr_m = fx->GetParError(1)/fx->GetParameter(1);
  fret->SetParError(1, fret->GetParameter(1)*relErr_m);

  double relErr_b =  (fx->GetParError(0) * (y1-y0)/ybins
    + y0 - fx->GetParError(1) * xbins/ybins * (y1-y0)/(x1-x0) *x0) / fret->GetParameter(0);
  fret->SetParError(0, fret->GetParameter(0)*relErr_b);
  fret->SetChisquare(      fx->GetChisquare() );
  return fret;
}

void style(int index) {
  int funccolor[5] = { 2, 4, 6, 3, 33};
  //  int funccolor[5] = { 2, 1, 4, 6, 3 };
  int funcstyle[5] = { 1, 1, 1, 1, 1 };
  //  int funcstyle[5] = { 1, 3, 2, 3, 3 };
  int histfillcolor = 34;

  if (gCalib->IsTwoDim()) gStyle->SetPalette(1);
  else {
    if (gHisto[0][index])
      gHisto[0][index]->SetFillColor(histfillcolor);
  }

  for (int ifun = 0; ifun<5; ifun++) {
    if (gFitFn[ifun][index]) {
      gFitFn[ifun][index]->SetFillStyle(0);
      gFitFn[ifun][index]->SetLineWidth(2);
      gFitFn[ifun][index]->SetLineColor(funccolor[ifun]);
      gFitFn[ifun][index]->SetLineStyle(funcstyle[ifun]);
    }
  }
}

TCanvas* showhisto(SingleStripe* sstr) {
  if (!sstr) throw "showhisto called without pointer to SingleStripe window";

  TObject* first, *second;
  char option [5] = "";
  char opt2   [5] = "";
  int index = sstr->GetIndex();

  if (gCalib->IsGraph()) {
    first  = gGraph[0][index];
    second = gGraph[1][index];
    strcpy (option, "AP");
    strcpy (opt2, "AP");
  }
  else {
    first  = gHisto[0][index];
    second = gHisto[1][index];
    if (gCalib->IsTwoDim()) strcpy (option, "COLZ"); 
  }

  TCanvas* Cfit = sstr->GetCanvas();
  Cfit->Clear();
  Cfit->cd();


  style(index);

  if (!first) return NULL; /// nothing to draw, leave canvas empty

  if (second) {
    Cfit->Divide(1,2);
    Cfit->cd(1);
  }

  first->Draw(option);

  /// add functions (if any)
  for (int ifun = 0; ifun<5; ifun++) {
    if (gFitFn[ifun][index]) {
      //      cout << "index=" << index << "\t ifun=" << ifun << "\t" 
      //   << gFitFn[ifun][index]->GetParameter(0) << endl;
      gFitFn[ifun][index]->Draw("same");
    }
  }

  gPad->Modified();

  if (second) {
    Cfit->cd(2);
    second->Draw(opt2);
    gPad->Modified();
  }

  Cfit->Update();
  return Cfit;
}



bool outsideCut(int index, SingleStripe* sstr, int ix, int iy) {
  if (!sstr) return false;
  if (!sstr->IsLimits()) return false;

  double x = gHisto[0][index]->GetXaxis()->GetBinCenter(ix);
  double y = gHisto[0][index]->GetYaxis()->GetBinCenter(iy);
  return  ! sstr->GetCut()->IsInside(x,y);
}


int fitlinear(int index, SingleStripe* sstr=NULL) {
  int nentry = 0;
  if (! gHisto[0][index] ) { 
    cerr << "fitlinear cannot access gHisto[0][" << index << "]\n";
    return -1;
  }
  int xbins  = gHisto[0][index]->GetXaxis()->GetNbins();
  int ybins  = gHisto[0][index]->GetYaxis()->GetNbins();
//   double x0  = gHisto[0][index]->GetXaxis()->GetXmin();
//   double x1  = gHisto[0][index]->GetXaxis()->GetXmax();
//   double y0  = gHisto[0][index]->GetYaxis()->GetXmin();
//   double y1  = gHisto[0][index]->GetYaxis()->GetXmax();
  double threshold = gHisto[0][index]->GetMaximum()*0.02;
  for (int i=0; i<xbins; i++) 
    for (int j=0; j<ybins; j++) {
      if ( outsideCut(index, sstr, i, j) ) continue; 
      if ( gHisto[0][index]->GetBinContent(i+1,j+1) > threshold) nentry++;
    }

  if (! nentry) return -1;
  Regression reg;

  xp   = new double [nentry];
  yp   = new double [nentry];
  np   = new double [nentry];
  erry = new double [nentry];
  double* errx = new double [nentry];
  double* xf   = new double [nentry];
  double* yf   = new double [nentry];

  TF1* gg0, *gg1, *fff;
  TH2F* fr = (TH2F*) gROOT->FindObject("fr");
  if (fr) delete fr;
  fr = new TH2F ("fr", "frame", xbins, 0., xbins, ybins, 0., ybins);

  bool newdistribution = false;
  if (!gHisto[1][index]) {
    newdistribution = true;
    char hstname[80];
    compose_fname (hstname, "distribution", index);
    gHisto[1][index] =
      new TH1F (hstname, hstname, 50, 0., gHisto[0][index]->GetMaximum()*1.1);;
  }  

  int k=0;

  for (int i=0; i<xbins; i++) 
    for (int j=0; j<ybins; j++) { 
      double z =  gHisto[0][index]->GetBinContent(i+1,j+1);
      //      fr->SetBinContent(i+1,j+1,z);
      if ( outsideCut(index, sstr, i, j)) continue; 
      if ( z > 0 && newdistribution ) gHisto[1][index]->Fill(z);
      if ( z > threshold) {
	reg.Fill(i+0.5, j+0.5, z);
	xp[k] = i+0.5;
	yp[k] = j+0.5;
	np[k] = z;
	xf[k] = k;
	yf[k] = 0.;
	errx[k]= 0.0001;
	erry[k]= 10./sqrt(z);
	k++;
      }
    }

  for (int i=0; i<nentry; i++) {
    fr->Fill(xp[i], yp[i], np[i]);
  }

//   double mreg_show = mreg * xbins/ybins * (y1-y0)/(x1-x0);
//   double breg_show = breg * (y1-y0)/ybins
//     + y0 -           mreg * xbins/ybins * (y1-y0)/(x1-x0) *x0;

  // fr->Draw("colz");
  if (k != nentry) throw "fitlinear: error in vector, k and nentry should be equal";
  if (reg.VariationXX()==0.) return -2;

  //  if (sstr && sstr->IsParSet() && gFitFn[0][index])
  fff = new TF1("fff", distance, 0., k-1., 2);
  fff ->SetParameters (reg.B(), reg.M());
  TGraphErrors *g = new TGraphErrors (k, xf, yf, errx, erry);
  g->Fit(fff,"0q");
  //  g->Draw("APL"):

  //  double mxy = fff->GetParameter(1);
  //  double bxy = fff->GetParameter(0);

  gg0 = FunctionLinear(fff, index);
  if (gFitFn[0][index]) {
    gFitFn[0][index]->SetParameters(gg0->GetParameters());
  }
  else {
    gFitFn[0][index] = gg0;
    char fname[80];
    sprintf (fname, "flinear%03d", index);
    gFitFn[0][index]->SetName(fname);
  }
    
  if (!gConst[0]) throw "fitlinear: function parameter not initialized";
  gConst[0]->SetParameters(index, gg0);

  
  gg1 = FunctionLinear(reg.B(), reg.M(), index);

  if (sstr) {
    TCanvas* chist = showhisto(sstr);
    chist->cd(2);
    //    gPad->SetLogy();
    VLine* lthresh = new VLine(threshold);
    lthresh->SetLineColor(4);
    lthresh->Draw();
    gPad->Modified();
    gPad->Update();
  }

  return 0;
}
