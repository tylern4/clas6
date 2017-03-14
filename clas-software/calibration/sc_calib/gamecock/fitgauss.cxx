#include "ROOT.h"
#include "jglobal.h"
#include "VLine.h"

TCanvas* showhisto(SingleStripe* sstr);
void compose_fname (char* s, const char* stub, const int index,
		    int style = 0);

double f_mass(Double_t *x, Double_t *par) {
  double x0 = (*x - 0.88035 - par[1]) / par[2];
  return par[0]*exp(-0.5*x0*x0);
}

double f_p2pdelay(Double_t *x, Double_t *par) {
  double x0 = (*x - par[1]) / par[2];
  return par[0]*exp(-0.5*x0*x0);
}

TCanvas* showmass(SingleStripe* sstr) {
  //  int index = sstr->GetIndex();
  TCanvas* Cfit = showhisto(sstr);

  double pimass_square = 0.01948;
  Cfit->cd();
  Cfit->SetLogy();
  VLine *l = new VLine( pimass_square);
  l->SetLineColor(2);
  l->SetLineStyle(3);
  l->Draw();
  Cfit->Modified();
  Cfit->Update();
  return Cfit;
}

int fitgauss(int index, SingleStripe* sstr) {
  TH1F* hdummy = (TH1F*) gHisto[0][index]->Clone();
  TF1* fgaus;

  if (gCalib->GetType()==c_mass) {
    for (int i=0; i<hdummy->GetNbinsX(); i++) {
      double xi = hdummy->GetBinCenter(i+1);
      if (xi < 0.6) hdummy->SetBinContent(i+1,0.);
      if (xi > 1.2) hdummy->SetBinContent(i+1,0.);
    }
    fgaus = new TF1("fgaus",f_mass,0.6,1.2,3);
  }
  else {
    fgaus = new TF1("fgaus",f_p2pdelay,-2.,2.,3);
  }

  if (hdummy->GetSum() < 5.) return -1;

  fgaus->SetParameters(hdummy->GetMaximum()/3.*2., 0.0001, 
		      hdummy->GetRMS()/2.);
  fgaus->SetParLimits(1, -1.5, 1.5);
  fgaus->SetParLimits(2, -0.5, 0.5);

  gHisto[0][index]->SetFillColor(34);
  int retvalue;
  if ((retvalue = gHisto[0][index]->Fit(fgaus,"0qr"))) return retvalue;

  if (gFitFn[0][index])
    gFitFn[0][index]->SetParameters(fgaus->GetParameters());
  else {
    gFitFn[0][index]=fgaus;
    char fname[80];
    sprintf (fname, "fgaus%03d", index);
    gFitFn[0][index]->SetName(fname);
  }

  gConst[0]->SetParameters(index, fgaus);
  if (sstr) {
    if (gCalib->GetType()==c_mass) showmass(sstr);
    else                          showhisto(sstr);
  }

  return 0;
 
}

TF1* FunctionGauss(int index, int ii=0) {
  if(gHisto[0][index] == NULL)
    return NULL;
  double x0  = gHisto[0][index]->GetXaxis()->GetXmin();
  double x1  = gHisto[0][index]->GetXaxis()->GetXmax();

  char funname[80];
  compose_fname(funname, "fgaus", index);
  TF1* fret;

  if(gCalib->GetType()==c_mass)
    fret = new TF1(funname, f_mass, x0, x1, 3);
  else{fret = new TF1(funname, f_p2pdelay, x0, x1, 3);}

  fret->SetParameters (gConst[ii]->GetParameters(index));
  fret->SetFillColor(3);
  fret->SetFillStyle(0);
  return fret;
}
