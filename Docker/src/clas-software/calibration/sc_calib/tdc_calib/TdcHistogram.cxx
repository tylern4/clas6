#include "TdcHistogram.h"

TGraph*       gr;
bool TdcHistogram::useGaussian = true;
extern int SC_Version_Flag;
//extern vector<vector<TH1D*> > tdcGaussFitHistos;

inline double SQR(double x) { return x*x; }

GaussFitResult::GaussFitResult (double* par, double* errpar, 
				double chisq, double t_, int sum_) :
  t(t_), mean(par[1]), sigma(par[2]), errMean(errpar[1]), errSigma(errpar[2]),
  chiSquare(chisq), sum(sum_)
{
  area  = sqrt (2 * M_PI) *  par[0] * sigma;
  errArea = area * sqrt ( SQR (errSigma/sigma) + SQR (errpar[0]/par[0]));
}

GaussFitResult::GaussFitResult(double mean_, double rms, double t_, int sum_) :
  t(t_), mean(mean_), sigma(rms), area(0), errMean(0), errSigma(0), errArea(0), 
  chiSquare(0), sum(sum_) {
}

bool GaussFitResult::bad (bool pipeline) {
  if (mean < 0)               return true;
  if ((pipeline && (mean >= HIGHEST_CHANNEL_PIPELINE)) || (!pipeline && (mean >= HIGHEST_CHANNEL))) return true;
  if (!pipeline && mean > HIGHEST_CHANNEL) return true;
  if (fabs(sigma) > 15)       return true;
  if (!TdcHistogram::useGaussian) return false;
  if (area > 1200.)           return true;
  if (area < 800.)            return true;
  return false;
}

string TdcHistogram::GetName (string stub) {
  stub +=  string("_") + bd->GetHistogramName (chan);
  return stub;
}

TdcHistogram::TdcHistogram (int chan_, BankDescription* bd_) :
  chan(chan_), bd(bd_), rootFit(NULL), rootHist(NULL), 
  expect (0) 
{
  memset (polypar, 0, sizeof (polypar));
  memset (polyerr, 0, sizeof (polyerr));

  int chan = (bd->GetModuleType()==1872?4096:10000);
//  double hmin = (bd->GetModuleType()==1872?-0.5:1999.5);
  double hmin = -0.5;

  string name = GetName ("raw");

  rootHist = new TH1F (name.c_str(), name.c_str(), chan, hmin, hmin+chan);
}

// fill a single tdc value into the histogram for a given channel
void TdcHistogram::Fill(int tdc, bool pipeline) {
  if (!tdc) return;                         // avoid zeros
  if((pipeline && (tdc >= HIGHEST_CHANNEL_PIPELINE)) || (!pipeline && (tdc >= HIGHEST_CHANNEL))) return;       // avoid overflow values, pulser reference signal (~5000)
  rootHist -> Fill ((float) tdc);           // fill histogram for surveillance
  entry[tdc] ++;
}

void TdcHistogram::PolynomialFit (bool pipeline) {

  double* x, *y, *ex, *ey;
  double  chisq, locAverageTime = 0.0;
  int n = 0;
//cout << "fitresult size = " << fitresult.size() << endl;
  if ((n = fitresult.size()) < 12) return;

  x = new double[n]; //mean tdc
  y = new double[n]; //time
  ex = new double[n]; //0.01
  ey = new double[n]; //errors on means * 0.05

  n = 0;
  for (vector<GaussFitResult>::iterator it=fitresult.begin(); it != fitresult.end(); it++) 
    if (!it->bad (bd->IsPipeline())) {
      x[n]  = it->mean;     y[n]    = it->t;
      ex[n] = 0.01;         ey[n++] = it->errMean * 0.05;
      locAverageTime += it->t;
    }
  if (n < 12) return;
  locAverageTime /= n;

  string name = GetName("tdc");
  //  sprintf (name, "%s%d.%02d", (lr ? "R" : "L"), chan/48+1, chan%48+1);
  rootFit = new TGraphErrors (n, x, y, ex, ey);
  rootFit-> SetNameTitle(name.c_str(), name.c_str());
  double loc_x, loc_y;
  for (int i=rootFit->GetN()-1; i>=0; i--) {
    rootFit->GetPoint(i,loc_x,loc_y);  
//    if((loc_y < 0) || (loc_y > 3.0*locAverageTime))
    if((loc_y < (locAverageTime/5.0)) || (loc_y > 3.0*locAverageTime))
      rootFit->RemovePoint(i);
  }

  TF1 *fit;
  if(pipeline){
    rootFit-> Fit("pol1","q");
    fit = (TF1*) rootFit->GetFunction("pol1");
  }else{
    rootFit-> Fit("pol2","q");
    fit = (TF1*) rootFit->GetFunction("pol2");
  }
  fit-> SetLineColor(2);

  chisq   = fit->GetChisquare();

  for (int i = 0; i < 3; i++) {
    if(pipeline && (i == 2)){
      polypar[i] = 0.0;
      polyerr[i] = 0.0;
    }else{
      polypar[i] = fit->GetParameter(i);
      polyerr[i] = fit->GetParError(i);
    }
  }
  
  delete[] x;  delete[] ex;
  delete[] y;  delete[] ey;
} 

GaussFitResult* TdcHistogram::leaveGaussFit (GaussFitResult* result, double* x, double* y) {
  delete [] x;
  delete [] y;
  entry.clear(); 
  return result;
}

GaussFitResult* TdcHistogram::GetAverage(double texp) {
  int    n    = 0;
  int    wsum = 0;
  int    entr = 0;
  double ave = 0.;
  double rms = 0.;

  if (!(n=entry.size())) return NULL;

  //get maximum  
  double maxval = -1.;
  for (map<int,int>::iterator it = entry.begin(); it != entry.end(); it++) {
    if (it->second > maxval) maxval = it->second;
  }
  double cutoff = 0.1 * maxval;

  // calculate sum and average of distribution
  for (map<int,int>::iterator it = entry.begin(); it != entry.end(); it++) {
    int weight = it->second;  // number of entries
    entr += weight;

    if (weight > cutoff) {
    //    cout << "<" << it->first << "," << it->second << "> ";
      n++;
      double x   = it->first;
      wsum    += weight;
      ave    += weight * x;
      rms    += weight * x * x;
    }
  }
  if (entr < 900 || wsum < 200) { entry.clear(); return NULL; }
  ave /= wsum;
  rms = sqrt ( rms/wsum - ave*ave );

  GaussFitResult* res = new GaussFitResult(ave, rms, texp, wsum);
  fitresult.push_back(*res);
  //  cout << rootHist->GetName() << " " << ( res->bad(bd->IsPipeline()) ? "--" : "++" ) 
  //       << " " << res->mean << " " << res->sigma << endl;
  entry.clear();
  return res;
}

GaussFitResult* TdcHistogram::GaussFit (double texp, int locChannel) {
//cout << "start gauss fit" << endl;
  if (!useGaussian) return GetAverage(texp);
//cout << "perform gauss fit" << endl;
  double* x, *y;
  double* par,* errpar, chisq;
  int    n = 0; 
  int    sum=0; //total num entries
  double ave=0.; //average tdc value

  // vector entries for TGraph
  if (!(n=entry.size())) return NULL;
//cout << "NOT zero entries" << endl;
  x = new double[n];
  y = new double[n];

  // calculate sum and average of distribution
  for (map<int,int>::iterator it = entry.begin(); it != entry.end(); it++) {
    n++;
    sum    += it->second;
    ave    += it->first * it->second;
  }
  if (sum < 900) return leaveGaussFit (NULL, x, y); 
  ave /= sum;

  n = 0;
  int locMin = 99999999; //minimum tdc value
  for (map<int,int>::iterator it = entry.begin(); it != entry.end(); it++) {
 // accept entries close to average
    if (fabs (ave-it->first) < 100) {  
      x[n]   = it->first;
      y[n++] = it->second;
      if(it->first < locMin)
        locMin = it->first;
    } 
    else {
      // large number of events off, dont use entry for fit
      if (it->second > 10) return leaveGaussFit (NULL, x, y); 
    }
  }
  
  if (!n) return leaveGaussFit (NULL, x, y);   // all entries rejected
//cout << "stuff ok, fit it" << endl;
  //  if (!C) {  C = new TCanvas ("C", "gauss fit", 1200, 800);}
  if (gr) delete gr;
  gr = new TGraph (n, x, y);
  gr->Fit("gaus","q");
  gr->SetMarkerStyle(20);

  TF1* fit = (TF1*) gr->GetFunction("gaus");
  par     = fit->GetParameters();
  errpar  = fit->GetParErrors();
  chisq   = fit->GetChisquare();
  fit-> SetLineColor(2);
  GaussFitResult* result = new GaussFitResult
    ( fit->GetParameters(), fit->GetParErrors(), fit->GetChisquare(), texp, sum);
  fitresult.push_back(*result);
//cout << "fit height, mean, sigma = " << fit->GetParameter(0) << ", " << fit->GetParameter(1) << ", " << fit->GetParameter(2) << endl;

  //save stuff in local histo...
/*  int locSector, locPaddle, locNumPaddles_Sector = 48;
  if(SC_Version_Flag == 2)
    locNumPaddles_Sector = 57;
  locSector = locChannel/locNumPaddles_Sector + 1;
  locPaddle = locChannel%locNumPaddles_Sector + 1;
  int locRangeMin = locMin - 100;
  int locRangeMax = 2*int(ave) - locRangeMin;
  int locNumBins = locRangeMax - locRangeMin;
  char *locName = new char[200];
  sprintf(locName, "Sector %d, Paddle %d TDC Gauss Spectrum %d", locSector, locPaddle, tdcGaussFitHistos[locChannel].size() + 1);
  TH1D *locHist = new TH1D(locName, locName, locNumBins, locRangeMin, locRangeMax);
  delete locName;

  for (map<int,int>::iterator it = entry.begin(); it != entry.end(); it++){
    if(((it->first) >= locRangeMin) && ((it->first) < locRangeMax))
      locHist->SetBinContent(locBin = locHist->GetXaxis()->FindBin(), it->second);
  }
  tdcGaussFitHistos[locChannel].push_back(locHist);

  //save fit
  TF1 *locFit = new TF1();
  *locFit = *fit;
  locName = new char[200];
  sprintf(locName, "Sector %d, Paddle %d TDC Gauss Fit %d", locSector, locPaddle, tdcGaussFits[locChannel].size() + 1);
  locFit->SetName(locName);
  tdcGaussFits[locChannel].push_back(locFit);
  delete locName; */

  return leaveGaussFit (result, x, y); 
}

void TdcHistogram::Write() {
  if (rootHist)  rootHist -> Write();
  if (rootFit)   rootFit  -> Write();
}

ostream& operator << (ostream& os, const TdcHistogram& h) {
  os.width (3); os << h.chan;
  //  os << (h.lr ? "R" : "L");
  for (int i=0; i<3; i++) {
    os << "\t" << h.polypar[i] << " (" << h.polyerr[i] << ")"; 
  }
  os << "\t" << h.polypar[3]; 
  return os;
}
