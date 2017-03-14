#include <stdio.h>
#include "ROOT.h"
#include "jglobal.h"
#include "Regression.h"

double QUAD(double x);
double f_inv(double *x, double *par);

TCanvas* showhisto(SingleStripe* sstr);

void compose_fname (char* s, const char* stub, const int index,
		    int style = 0);

double f_timewalk (double *x, double *par) {
  double d = (par[3]<=0.) ? 1E-12 : par[3];
  double xnorm = *x / 35.;
  if (xnorm < d)
    return par[1]/pow(xnorm,par[2]) + par[0];
  double m = par[1]*par[2]/pow(d,par[2]+1.);
  double b = par[1]*(1.+par[2])/pow(d,par[2]) + par[0];
  return b - m * xnorm;
}

double f_inv (double *x, double *par) {
  double xnorm = *x / 35.;
  return par[1]/pow(xnorm,par[2]) + par[0];
}

double f_timewalkmirror (double* x, double* par) {
  double xnorm;
  double d = (par[3]<=0.) ? 1E-12 : par[3];
  double c = (par[2]<=0.) ? 1E-12 : par[2];
  double b = (par[1]<=0.) ? 1E-12 : par[1];

  if (*x > 0) {
    xnorm = *x / 35.;
    if (xnorm < d) {
      return b/pow(xnorm,c) + par[0];
    }
    double m = -b*c/pow(d,c+1.);
    double n = b*(1.+c)/pow(d,c) + par[0];
    return n + m * xnorm;
  }

  // mirror part
  double y = - *x;
  if (y == par[0]) {
    return y*1E12;
  }

  xnorm = pow (b/(y-par[0]),1./c);
  if (xnorm < d) {
    return xnorm * 35.;
  }
  double m = -b*c/pow(d,c+1.);
  double n = b*(1.+c)/pow(d,c) + par[0];
  if (m == 0) return y*1E12;
  return (y - n) / m * 35.;
}

TF1* FunctionTimewalk(int index, int ii=0) {
//   double xbins  = gHisto[0][index]->GetXaxis()->GetNbins();
//   double ybins  = gHisto[0][index]->GetYaxis()->GetNbins();
  if(gHisto[0][index] == NULL)
    return NULL;
  double x0  = gHisto[0][index]->GetXaxis()->GetXmin();
  double x1  = gHisto[0][index]->GetXaxis()->GetXmax();
//   double y0  = gHisto[0][index]->GetYaxis()->GetXmin();
//   double y1  = gHisto[0][index]->GetYaxis()->GetXmax();

  char funname[80];
  compose_fname(funname, "f_timewalk", index);
  TF1* fret = new TF1(funname, f_timewalk, x0, x1, 4);
  fret->SetParameters (gConst[ii]->GetParameters(index));
  fret->SetFillColor(3);
  fret->SetFillStyle(0);
  return fret;
}

/// fitfunction given except constant offset
int fitoffset(int index, int ii) {
  if (! gFitFn[ii][index]){
   cout << "fitoffset: function " << ii << " not defined.  ";
   if(index%57 >= 48)
     cout << "if runno < 55357 then don't worry about it.";
   cout << endl;
   return -1;
  }

  gFitFn[ii][index]->SetParameter(0,0.);
  //  if (!index) cout << "2,145: " << gHisto[0][index]->GetBinContent(2,145) << endl;
  //  if (!index) cout << "2,285: " << gHisto[0][index]->GetBinContent(2,285) << endl;
  bool usegraph = gCalib->IsGraph(); 

  if (usegraph) {
    if (!gGraph[0][index]) return -1;
  }
  else {
    if (!gHisto[0][index]) return -1;
  }

  double entries = 0.;
  double average = 0.;
  double varianz = 0.;
  int jmax = usegraph ? gGraph[0][index]->GetN() : 
    gHisto[0][index]->GetXaxis()->GetNbins();
  int imax = usegraph ? 1 : 
    gHisto[0][index]->GetYaxis()->GetNbins();
  
  for (int j=0; j<jmax; j++) {
    double x;
    double y;
    if (usegraph) {
      gGraph[0][index]->GetPoint(j, x, y);
      double func = gFitFn[ii][index]->Eval(x,0.,0.);
      double dy = y-func;
      entries += 1;
      average += dy;
      varianz += dy*dy;
    }
    else {
      x = gHisto[0][index]->GetXaxis()->GetBinCenter(j+1); 
      double func = gFitFn[ii][index]->Eval(x,0.,0.);
      for (int i=0; i<imax; i++) {
	double y = gHisto[0][index]->GetYaxis()->GetBinCenter(i+1); 
	double dy = y-func;
	double z = gHisto[0][index]->GetBinContent(j+1,i+1);
	entries += z;
	average += z*dy;
	varianz += z*dy*dy;
	//	if (!index && z > 10) {
	//	  printf ("%3d %3d %12.4f(x) %12.4f(y) %12.4f(f) %12.4f(z)\n", 
	//		  i+1, j+1, x, y, func, z);
      }
    }
  }
  if (entries == 0) { 
    cerr << "fitoffset: no entries in " << index << endl; 
    return -2;
  }
  average /= entries;
  varianz /= entries;
  varianz -= average*average;
  gConst[ii]->SetParameter(index,0,average);
  gFitFn[ii][index]->SetParameter(0,average);
//  if (!index) cout << gHisto[0][index]->GetName() << "\t" << average << endl;
  return 0;
}
//--------------- end fitoffset ----------------------

TCanvas* showtimewalk(SingleStripe* sstr) {
  int index = sstr->GetIndex();
  //  gHisto[0][index]->GetXaxis()->SetRangeUser(0.,2000.);
  cout << "showtimewalk " << index << endl;

  TCanvas* Cfit = showhisto(sstr);
  return Cfit;
}

int fittimewalk (int index, SingleStripe* sstr) {
  //this function fits the time-walk corrections.  the input hists are time vs ADC-P.  
  //the time-offset doesn't matter, only the time dependence (delta-t)
  //NOTE: this means T0 of the laser reference photo-diode PMT (CALL_CALIB->T0->value) doesn't matter, only T1.  

  //The Time-Walk function looks complicated, but it is really just a simple piecewise function:
  //It is simply either 1/(x^const) (shifted and scaled) or linear, but forced to be continuous, hence the weird offset term in the linear part

  //This function first creates a TGraphErrors by calculating the mean and variance of y for each bin of x, then creating a "1D" plot with the TGraphErrors.
  //To fit it, this function first manually gets a "first guess" for the 1/(x^const) part.  It then fits this part of the histogram.
  //It then fits the overall function by constraining the results of the 1/(x^const) part semi-tightly.  This part basically just finds the piecewise junction (par[3])

  //In the final fit, parameters (indices) 1->3 represent WALK1, WALK2, and WALK_A0

  if(!gHisto[0][index])
    return -7;

  double cut = 5.;
  int kx = 0;

  int xbins     = gHisto[0][index]->GetXaxis()->GetNbins();
  int ybins     = gHisto[0][index]->GetYaxis()->GetNbins();
  //  double xmin   = gHisto[0][index]->GetXaxis()->GetXmin();
  double xmax   = gHisto[0][index]->GetXaxis()->GetXmax();
  //  double ymin   = gHisto[0][index]->GetYaxis()->GetXmin();
  //  double ymax   = gHisto[0][index]->GetYaxis()->GetXmax();
  
  double* ymean = new double[xbins]; //mean time for different (ADC-P)
  double* ysum  = new double[xbins]; //number of entries for different (ADC-P)
  double* yrms  = new double[xbins]; //rms time for different (ADC-P)
  double* xx    = new double[xbins+ybins]; //(ADC-P) values
  double* ex    = new double[xbins+ybins]; //1.E-8

  double* xmean = new double[ybins]; //mean (ADC-P) for different times
  double* xsum  = new double[ybins]; //number of entries for different times
  double* xrms  = new double[ybins]; //rms (ADC-P) for different times
  double* yy    = new double[xbins+ybins]; //mean time for different (ADC-P)
  double* ey    = new double[xbins+ybins]; //rms time for different (ADC-P)

  memset (xsum, 0, ybins*sizeof(double));
  memset (ysum, 0, xbins*sizeof(double));
  memset (xrms, 0, ybins*sizeof(double));
  memset (yrms, 0, xbins*sizeof(double));
  memset (xmean, 0, ybins*sizeof(double));
  memset (ymean, 0, xbins*sizeof(double));
  for (int i=0; i<xbins; i++) {
    double x = gHisto[0][index]->GetXaxis()->GetBinCenter(i+1);
    for (int j=0; j<ybins; j++) {
      double y = gHisto[0][index]->GetYaxis()->GetBinCenter(j+1);
      double z = gHisto[0][index]->GetBinContent(i+1, j+1);
      
      ysum[i]  += z;
      ymean[i] += z * y;
      yrms[i]  += z * y * y;

      xsum[j]  += z;
      xmean[j] += z * x;
      xrms[j]  += z * x * x;
    }
  }


  for (int i=0; i<xbins; i++) {
    if (ysum [i] > cut) {
      double x = gHisto[0][index]->GetXaxis()->GetBinCenter(i+1);
      ymean [i] /= ysum [i];
      yrms [i] /= ysum [i];
      yrms [i] = sqrt (yrms[i] - ymean [i] * ymean[i]);
      xx [kx] = x;
      ex [kx] = 1.E-8;
      yy [kx] = ymean[i];
      ey [kx] = yrms[i];
      kx++;
    }
  }

  //means, variances calced for different bins, quit if no good data
  if (kx < 5) return -5;

  Regression reg;
  TGraph* glog= NULL;
  TF1* flog=NULL;
  double par1, par2;
  if (kx >= 6) {
    glog = new TGraph(30);
    for (int i=1; i<kx && i < 30; i++) {
      double dx = (xx[i] - xx[i-1])/35.; //bin size of (ADC-P) div 35
      double dy = yy[i] - yy[i-1]; //difference of mean times btw adjacent (ADC-P) //delta_t
      if (dy < 0) { //what it should be, it's decreasing
	double logx = log((xx[i]+xx[i-1])/2./35.); //log((ADC-P)/35) //log(energy) //close enough... ~energy so keep for clarity
	double logy = log(-dy/dx); //log(-delta_t/ADC_binsize) //log(-slope)
	glog->SetPoint(i, logx, logy);
	reg.Fill( logx, logy, 1.);
      }
    }
    par2 = -reg.M()-1.; //reg.M() = avg(logx*logy) - (avg(logx)*avg(logy))/(avg(logx*logx)- avg(logx)*avg(logx))
    //whatever, par2 is a first guess for [2] in f_timewalk, or w3 in the clas_note
    if (par2<=0 || par2>=1.) par2 = 0.5;
    par1 = exp(reg.B())/par2; //par1 is a first guess for [1] in f_timewalk, or w2 in the clas_note
    flog = new TF1("flog", "[0]+x*[1]", 0., 20.);
    flog->SetParameters(reg.GetParameters());
    flog->SetLineColor(2);
    glog->SetMarkerStyle(29);
  }
  else {
    par2 = 0.5;
    par1 = 25.;
  } 

  double locInitOffset = gHisto[0][index]->GetYaxis()->GetBinCenter(gHisto[0][index]->GetYaxis()->GetNbins()/2);

  //INITIAL VALUES NOW FOUND, fit 1/x^const part of time-walk
  TGraphErrors* g = new TGraphErrors (kx, xx, yy, ex, ey); //"1D" graph of mean time vs (ADC-P), points have error bars
  TF1* invfit = new TF1 ("invfit", f_inv, 1.0, 800.0, 3);
  invfit->SetParameters(locInitOffset, par1, par2);
  g->Fit(invfit, "0QR");
  double locInvOffset = invfit->GetParameter(0);
  double locInvSlope = invfit->GetParameter(1);
  double locInvPower = invfit->GetParameter(2);
  double locInvCutoff = 25.0;
  g->GetListOfFunctions()->Clear();
  if(invfit != NULL)
    delete invfit;
//  cout << " init par0 -> 3 = " << locInvOffset << ", " << locInvSlope << ", " << locInvPower << ", " << locInvCutoff << endl;

  //FIT THE WHOLE DISTRIBUTION
  TF1* fit = new TF1 ("fit", f_timewalk, 1., xmax, 4);
  gConst[0]->SetParameters(index, locInvOffset, locInvSlope, locInvPower, locInvCutoff);
  fit->SetParameters(locInvOffset, locInvSlope, locInvPower, locInvCutoff);
  gFitFn[0][index] = fit;
  char fname[80];
  sprintf (fname, "ftimewalk%03d", index);
  gFitFn[0][index]->SetName(fname);
  fit->SetParLimits(0, locInvOffset - 100.0,locInvOffset + 100.0);
  fit->SetParLimits(1, locInvSlope - 25.0, locInvSlope + 25.0);
  fit->SetParLimits(2, locInvPower - 0.2, locInvPower + 0.2);
  fit->SetParLimits(3, 5.0, 200.0);
  int retvalue;
  if ((retvalue = g->Fit(fit, "0QR"))) return -retvalue;

cout << "index, fit par0->3 (offset, WALK1, WALK2, and WALK_A0) = " << index << ", " << fit->GetParameter(0) << ", " << fit->GetParameter(1) << ", " << fit->GetParameter(2) << ", " << fit->GetParameter(3) << endl;
cout << "index, fit errors par0->3 (offset, WALK1, WALK2, and WALK_A0) = " << index << ", " << fit->GetParError(0) << ", " << fit->GetParError(1) << ", " << fit->GetParError(2) << ", " << fit->GetParError(3) << endl;
  
  if (gConst[0]->SetParameters(index, fit) <0) return -6;
  
  gFitFn[0][index] = fit;

  if (sstr) {
    //cout << "Draw " << index << " ..." << endl;
    showtimewalk (sstr);
    //    TCanvas* Cfit = showtimewalk (sstr);
    //cout << "Draw done!" << endl;
  }
  return 0;
}

int fittimewalkold (int index, bool nodraw) {
  int nentry = 0;
  if (! gHisto[0][index] ) { 
    cerr << "fitlinear cannot access gHisto[0][" << index << "]\n"; exit(1); 
  }
  int xbins  = gHisto[0][index]->GetXaxis()->GetNbins();
  int ybins  = gHisto[0][index]->GetYaxis()->GetNbins();
  //  double x0  = gHisto[0][index]->GetXaxis()->GetXmin();
  //  double x1  = gHisto[0][index]->GetXaxis()->GetXmax();
  //  double y0  = gHisto[0][index]->GetYaxis()->GetXmin();
  //  double y1  = gHisto[0][index]->GetYaxis()->GetXmax();
  double cut = gHisto[0][index]->GetMaximum()*0.02;
  for (int i=0; i<xbins; i++) 
    for (int j=0; j<ybins; j++) {
      if ( gHisto[0][index]->GetBinContent(i+1,j+1) > cut) nentry++;
    }

  //  double xave = 0.;
  //  double yave = 0.;
  //  double xx   = 0.;
  //  double yy   = 0.;
  //  double xy   = 0.;
  //  double sum  = 0.;

  return 0;
}
