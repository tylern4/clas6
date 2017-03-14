#include "ROOT.h"
#include "jglobal.h"

double f_landau(Double_t *x,Double_t *par)
{
  return par[0] * TMath::Landau(x[0],par[1], par[2])
					   + x[0]*par[4] + par[3];
}

TCanvas* showhisto   (SingleStripe* sstr);

TF1* FunctionGmean2(int index, int ii) {
  if (!ii) throw "FunctionGmean2: use for secondary function only";
  TF1* flan2 = new TF1("flan2",f_landau,0,2500,5);
  if (gFitFn[0][index]) 
    flan2->SetParameters(gFitFn[0][index]->GetParameters());
  else
    gCalib->SetDefaultPar(flan2);
  
  gConst[ii]->Mipadc2Gmean(index);

  /// move peak maximum
  flan2->SetParameter(1, gConst[ii]->GetParameter(index,1));

  return flan2;
}
 
int fitlandau( int index, SingleStripe* sstr=NULL) {
  TF1* flan;

  if(gHisto[0][index] == NULL)
    return -1;

  if (gHisto[0][index]->GetSum() < 2.) return -1;
  double y0    = ( gHisto[0][index]->GetBinContent(1)
		  +gHisto[0][index]->GetBinContent(2))/2.;
  double rms   = gHisto[0][index]->GetRMS();
  double xmean = gHisto[0][index]->GetBinCenter(gHisto[0][index]->
						GetMaximumBin());
  double ymax  = gHisto[0][index]->GetMaximum();

  /// parameter moved by slider
  if (sstr && gFitFn[0][index] && sstr->IsParSet()) {
    flan = gFitFn[0][index];
  }
  else {
    flan = new TF1("flan",f_landau,0,2500,5);
    flan->SetParameters( ymax*5. , xmean, rms/5., y0, -y0/2000.);
  }
  
  int retvalue;
  if (sstr && sstr->IsLimits()) {
    if ((retvalue = gHisto[0][index]->Fit(flan,"0q","",
	sstr->GetLowerLimit(), sstr->GetUpperLimit()) )) return retvalue;
  }
  else {
    if ((retvalue = gHisto[0][index]->Fit(flan,"0q") )) return retvalue;
  }

  /// store results in global function an constants
  if (gFitFn[0][index])
    gFitFn[0][index]->SetParameters(flan->GetParameters());
  else {
    gFitFn[0][index]=flan;
    char fname[80];
    sprintf (fname, "flandau%03d", index);
    gFitFn[0][index]->SetName(fname);
  }
  gConst[0]->SetParameters(index, flan);

  if (sstr) showhisto( sstr );
  return 0;
}

