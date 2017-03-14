//$Id: tagslope.C,v 1.8 2008/09/25 19:23:05 fklein Exp $
//
#ifdef __CINT__
const double M_PI = 3.141592654;
#else
#include "ROOT.h"
#endif

// subdirectory where to keep the output files
char* subdirectory="data";

// T-counter reference spectra shown after fit 
//  (change interactively by clicking on Tcounter bin in "Fit" plot)
int TAGTrefid=22;

//---------------------------------------------------------------------
// set the constant below to false to get a coarse calibration
// set it to true 
//   1.) if things seem to be stable
//   2.) to get the very precice calibration of the slope using 
//       the RF period time of 2.004008 ns
//   3.) you will have to adjust the other parameters afterwards.
//       Check whether the iteration converges.
const bool want_to_do_the_hihgly_experimental_slope_correction = false;
//---------------------------------------------------------------------

const int NWEED = 4;    // remove max. peak in lrxxx histogram (set max. 5 bins to zero)
const int NRMEAN = 60;
//const int NRMEAN = 11;
const int TCNT_MAX =  61;
const double empty = 5;
const double fullRF = 2.004008;
const double halfRF = fullRF / 2.;

enum result_t { r_slope, r_lr_a0, r_lr_a1, r_lrchisq,
		r_rfarea, r_rfmean, r_rfsigma, r_rfchisq, r_max };
int gResult = r_slope;

TH1D* hresult[r_max];

TH1F* hrunmean[TCNT_MAX];
TH1F* htimedif[TCNT_MAX];
TH1D* hrfslice[TCNT_MAX];
TGraph* gSlope[TCNT_MAX];
TProfile* hprof[TCNT_MAX];
double maxx0[TCNT_MAX];
double maxy0[TCNT_MAX];

TLine* cursor = NULL;
TCanvas* C  = NULL;
TCanvas* CT = NULL;
TDirectory* mem = NULL;
bool busy = false;
int runno = 0;

double sethalfmax ( double drf) {
  while (drf < - halfRF) drf += fullRF;
  while (drf >   halfRF) drf -= fullRF;
  return drf;
}

double myrffun ( double*x, double*par) {
  double dx = sethalfmax(*x - par[1]);
  double arg = dx / par[2];
  return par[0] * exp (-0.5 * arg * arg) + par[3];
}

// make sure par[1] > 0
double myslopefun ( double*x, double*par) {
  int n = (int) floor (*x / par[1]) ;
  double z = *x - n * par[1];
  double parh = par[1] / 2.;
  while (z > parh) z -= par[1];
  while (z < parh) z += par[1];
  return z + par[0];
}

TObject* locateDirObject(char* name, char *dirname=NULL) {
  /// search memory
  TObject* obj = gROOT->FindObject(name);
  if (obj) return obj;

  /// search files
  TIter nextfile(gROOT->GetListOfFiles());
  TFile *f;
  while (f = (TFile*) nextfile() ) {
    f->UseCurrentStyle();
    if( dirname ) {
      TDirectory* dir = (TDirectory*) f->Get(dirname);
      if ((obj = dir->Get(name))) break;
    }
    else
      if ((obj = f->Get(name))) break;
  }
  return obj;
}

void switch_histo(int ihist) {
  C->cd();
  gResult = ihist;
  hresult[gResult]->Draw();
  TExec* ex = new TExec("ex", "interactive()");
  ex->Draw("same");
  gSystem->ProcessEvents();
  C->Modified();
  C->Update();  
}

void show_tcounter(int chan) {
  char canvastitle[80];
  sprintf (canvastitle, "T-counter %d", chan+1);
  if (CT==NULL) 
    CT = new TCanvas("CT", canvastitle, 700, 700);
  else {
    CT->cd();
    CT->Clear();
    CT->SetTitle(canvastitle);
  }
  CT->Divide(2,2);

  mem->cd();

  cout << "\nresults for T-counter " << chan+1 << ":" << endl;

  CT->cd(1);
  if (hrfslice[chan]) {
    hrfslice[chan]->Draw();
    cout << "RF peak    (mean, sigma)    "  
	 << hresult[r_rfmean]->GetBinContent(chan+1) << " \t " 
	 << hresult[r_rfsigma]->GetBinContent(chan+1) << endl;
  }

  CT->cd(2);
  if (gSlope[chan]) {
    gSlope[chan]->Draw("ap");
    cout << "peak to peak     (slope)    " 
	   <<  hresult[r_slope]->GetBinContent(chan+1)  << endl;
  }

  CT->cd(3);
  if (hprof[chan]) {
    hprof[chan]->Draw();
    cout << "LR diff  (slope, offset)    " 
	 << hresult[r_lr_a1]->GetBinContent(chan+1) << " \t " 
	 << hresult[r_lr_a0]->GetBinContent(chan+1) << endl;
  }
  cout << "==============================\n" << endl;

  CT->cd(4);
  if (htimedif[chan]) {
    htimedif[chan]->Draw();
    if (hrunmean[chan]) {
      hrunmean[chan]->Draw("same");
    }
  }

  gSystem->ProcessEvents();
  CT->Update();
  C->cd();
}

void interactive() {
  if (busy) return;
  int px = gPad->GetEventX();
  int py = gPad->GetEventY();
  int ix = (int) floor ((gPad->AbsPixeltoX(px) - 0.5));
  int iy = (int) floor (gPad->AbsPixeltoY(py)*100);
  //  cout << gPad->GetEvent() << ",\tx = " << ix << ",\ty = " << iy << endl;
  if (gPad->GetEvent() != 11) return;
  if (ix < 0 || ix >= TCNT_MAX) return;

  busy = true;
  if (cursor) delete cursor;
  cursor = new TLine(ix+1., C->GetUymin(), ix+1., C->GetUymax());
  cursor->SetLineColor(4);
  cursor->Draw();
  C->Modified();
  C->Update();
  show_tcounter(ix);
  busy = false;
}

void calculate_const() {

  TH1F* hconst_t0l = (TH1F*) locateDirObject("const_t0l");
  TH1F* hconst_t0r = (TH1F*) locateDirObject("const_t0r");
  TH1F* hconst_t1l = (TH1F*) locateDirObject("const_t1l");
  TH1F* hconst_t1r = (TH1F*) locateDirObject("const_t1r");
  TH1F* hconst_run = (TH1F*) locateDirObject("const_run");

  if (!hconst_t0l || !hconst_t0r || !hconst_t1l || !hconst_t1r || !hconst_run) {
    cout << "histogram with calibration constants not found" << endl;
    return;
  }
  runno = hconst_run->GetBinContent(1);

  char *dir=gSystem->OpenDirectory(subdirectory);
  if( !dir ) {
    if( gSystem->MakeDirectory(subdirectory) )   // 0 for success, -1 for fail
      strcpy(subdirectory,".");
  }
  char filename[255];
  sprintf(filename,"%s/slope_left.%d.dat",subdirectory,runno);
  ofstream f_t1l(filename);
  if (!f_t1l.good()) return;

  sprintf(filename,"%s/slope_right.%d.dat",subdirectory,runno);
  ofstream f_t1r(filename);
  if (!f_t1r.good()) return;

  sprintf(filename,"%s/dt_left.%d.dat",subdirectory,runno);
  ofstream f_t0l(filename);
  if (!f_t0l.good()) return;

  sprintf(filename,"%s/dt_right.%d.dat",subdirectory,runno);
  ofstream f_t0r(filename);
  if (!f_t0r.good()) return;

  f_t1l.flags(ios::fixed);
  f_t1r.flags(ios::fixed);
  f_t0l.flags(ios::fixed);
  f_t0r.flags(ios::fixed);

  f_t1l.precision(6);
  f_t1r.precision(6);
  f_t0l.precision(6);
  f_t0r.precision(6);

  TH2F* httag = (TH2F*) locateDirObject("ttag");
  if(httag) httag_prof = httag->ProfileX();

  for (int i=0; i<TCNT_MAX; i++) {
    double xslope = hresult[r_slope]->GetBinContent(i+1);
    double xlr_a0 = hresult[r_lr_a0]->GetBinContent(i+1);
    double xlr_a1 = hresult[r_lr_a1]->GetBinContent(i+1);
    double xrfpos = hresult[r_rfmean]->GetBinContent(i+1);
    double cur_t0l  = hconst_t0l->GetBinContent(i+1);
    double cur_t0r  = hconst_t0r->GetBinContent(i+1);
    double cur_t1l  = hconst_t1l->GetBinContent(i+1);
    double cur_t1r  = hconst_t1r->GetBinContent(i+1);
    

    double tdcl = (maxy0[i] - cur_t0l + cur_t0r + cur_t1r * maxx0[i])
      / ( cur_t1l + cur_t1r );
    double tdcr = maxx0[i] - tdcl;
    cout << tdcl << "\t" << tdcr << endl;

    double d_t1l =   xlr_a1*1000.; 
    double d_t1r = - xlr_a1*1000.; 
    double d_t0l =   xlr_a0 + xrfpos;
    double d_t0r = - xlr_a0 + xrfpos;
    double new_t1l = cur_t1l + d_t1l;
    double new_t1r = cur_t1r + d_t1r;
    double new_t0l = cur_t0l + d_t0l;
    double new_t0r = cur_t0r + d_t0r;

    double myoff = httag ? httag_prof->GetBinContent(i+1) : 0.0;
    int n = (int) floor((fabs(myoff)+0.1)/halfRF);
    cout <<"Tcntr "<<i+1<<" ttag "<<myoff <<" "<< n <<endl;
    if(n) {
      double mycorr = (myoff>0)? -n*halfRF : n*halfRF;
      new_t0l += mycorr;  
      new_t0r += mycorr;
    }  
    if (xslope != 0 &&
	want_to_do_the_hihgly_experimental_slope_correction
	) {
      double stretch = fullRF / xslope;
      double deltat0 = (stretch - 1.) * new_t1l * tdcl - (stretch - 1.) * new_t1r * tdcr;
      new_t1l *= stretch;
      new_t1r *= stretch;
      new_t0l -= deltat0/2.;
      new_t0r += deltat0/2.;
    }
    //    cout << i+1 << " \t " <<  d_t1l << " \t " <<  d_t1r
    //	 << " \t " <<  d_t0l << " \t " <<  d_t0r <<  endl;

    f_t1l << new_t1l << endl;
    f_t1r << new_t1r << endl;
    f_t0l << new_t0l << endl;
    f_t0r << new_t0r << endl;
  }
}

void tagrffit() {
  /// name of the histogram for T-counter L-R difference
  char hstname[80];
  sprintf (hstname, "rftag");

  /// try to locate the histogram
  TH2F* h = (TH2F*) locateDirObject(hstname);

  /// unsuccessful
  if (!h) {
    cout << "histogram not found! <" << hstname << ">" << endl; return; }

  mem->cd();
  TH2F* hrf = (TH2F*) h->Clone();
  hrf->SetOption("colz");

  TF1* myrf = new TF1("myrf", myrffun, -1., 1., 4);
  myrf->SetLineColor(2);

  cout << "T-counter   Mean  \t    Sigma" << endl;

  for (int i=0; i<TCNT_MAX; i++) {
    char hstname[80];
    sprintf (hstname, "rf%03d", i+1);
    hrfslice[i] = hrf->ProjectionY(hstname, i*2+1, i*2+1);
    sprintf (hstname, "RF - tagger (T-counter %d)", i+1);
    hrfslice[i]->SetTitle(hstname);

    double maxy = hrfslice[i]->GetMaximum();
    double maxx = hrfslice[i]->GetBinCenter(hrfslice[i]->GetMaximumBin());

    myrf->SetParameters(maxy/2., maxx, 0.05, maxy/2.);
    myrf->SetParLimits(0, 0., 1.5*maxy);
    myrf->SetParLimits(1, -halfRF, halfRF);
    myrf->SetParLimits(2, 0.05, fullRF);
    
    myrf->SetFillColor(5);
    if (hrfslice[i]->Fit(myrf, "wq", "", -1., 1.) ) {
      cout << "fit failed for T-counter " << i+1 << endl;
      continue;
    }
    
    double rfConst = myrf->GetParameter(0);
    double rfMeang = myrf->GetParameter(1);
    double rfSigma = myrf->GetParameter(2);
    double rfChisq = myrf->GetChisquare();

    if (rfSigma > 0.05 && rfSigma < 0.5) {
      hresult[r_rfarea]->SetBinContent(i+1, rfConst*rfSigma*sqrt(2*M_PI) );
      hresult[r_rfmean]->SetBinContent(i+1, rfMeang);
      hresult[r_rfsigma]->SetBinContent(i+1, rfSigma);
      hresult[r_rfchisq]->SetBinContent(i+1, rfChisq);
      cout << i+1 << " \t " << rfMeang << " \t " << rfSigma << endl;
    }
    else {
      cout << i+1 << " \t " << rfMeang << " \t " << rfSigma << " (out of range)" <<endl;
      continue;
    }

    /*
    if (C) {
      gSystem->ProcessEvents();
      C->Modified();
      C->Update();
    }
    */
  }	
  
}

void tagslopechan(int chan) {
  /// name of the histogram for T-counter L-R difference
  char hstdifft[80], hstname[80];
  sprintf (hstdifft, "difft%03d", chan+1);

  /// try to locate the histogram
  TH1F* h = (TH1F*) locateDirObject(hstdifft,"lr");

  /// unsuccessful
  if (!h) {
    cout << "histogram not found! <" << hstdifft << ">" << endl; return; }

  int    n  = h->GetNbinsX();
  double x0 = h->GetBinLowEdge(1);
  double x1 = h->GetBinLowEdge(n) + h->GetBinWidth(n);
  double xbinwidth = (x1-x0)/n;

  mem->cd();
  sprintf (hstname, "sidepeak%03d", chan+1);
  htimedif[chan] = (TH1F*) h->Clone(hstname);
  sprintf (hstname, "time left-right (T-counter %d)", chan+1);
  htimedif[chan]->SetTitle(hstname);

  /// eliminate huge peak at dt=0
  if (htimedif[chan]->GetMaximum() < empty) {
    cout << "T"<<chan+1<<": histogram <" << hstdifft << "> maximum less than " << empty << endl; return; }

  int imaximum = htimedif[chan]->GetMaximumBin();
  double rfSigma = hresult[r_rfsigma]->GetBinContent(chan+1);
  int irange0 = rfSigma>0.15 ? 25 : 15;
  for (int i = -irange0; i<=irange0; i++) htimedif[chan]->SetBinContent(imaximum+i, 0);
  double threshold = 0.05 * htimedif[chan]->GetMaximum();

  /// calculate the running mean
  double carry[NRMEAN];
  double sum = 0;

  for (int i=0; i<NRMEAN; i++) {
    sum+= (carry[i] = htimedif[chan]->GetBinContent(i+1));
  }

  sprintf(hstname, "runmean%03d", chan+1);
  hrunmean[chan] = new TH1F(hstname, hstname, n, x0, x1);

  //  htimedif[chan]->Draw();
  hrunmean[chan]->SetLineColor(4);
  //  hrunmean[chan]->Draw("same");

  for (int i=NRMEAN; i<n; i++) {
    int j = i%NRMEAN;
    sum -= carry[j];       ///
    sum += (carry[j] = htimedif[chan]->GetBinContent(i+1));
    hrunmean[chan]->SetBinContent(i+1, sum/NRMEAN);
  }

  if (hrunmean[chan]->GetMaximum() < empty) {
    cout << "histogram (T"<<chan+1<<") running mean maximum less than " << empty << endl; return; }

  cout<<" Tcnt "<<chan+1<<": imax "<<imaximum<<", threshold "<<threshold<<endl;
  double* peakpos = new double [n];
  double* peak_no = new double [n];
  double* p_zeros = new double [n];
  double weightsum = 0;
  double wposition = 0;
  int ipeak = 0;
  int firstpositive = -1;
  for (int i=0; i<n-1; i++) {
    double runningMean = hrunmean[chan]->GetBinContent(i+1);

    if (runningMean > threshold) {   /// over threshold 
      double rf = htimedif[chan]->GetBinContent(i+1);

      if (rf > runningMean ) {  /// top of the peak
	weightsum += rf;
	wposition  += rf * htimedif[chan]->GetBinCenter(i+1);
      }
      else { /// gap between peaks

	if (weightsum > 0) { /// calculate mean for last peak
	  peakpos[ipeak] = wposition / weightsum;
	  peak_no[ipeak] = ipeak;
	  if (i > imaximum && firstpositive < 0) {
	    firstpositive = ipeak;
	  }
	  ipeak++;
	  weightsum = 0;   wposition  = 0;
	}
      }
      //      cout << "runmean "<<runningMean<<" rf "<<rf<<", weight sum "<<weightsum<<", pos "<<wposition<<", ipeak "<<ipeak<<endl;
    }
    else {
      weightsum = 0;   wposition  = 0;
    }
  }
  cout<<"Tcnt"<<chan+1<<" firstpositive "<<firstpositive<<"  peaks "<<ipeak<<endl;
  //  for(int i=0; i<ipeak;i++) cout <<" "<<peakpos[i];
  //  cout<<endl;
  if (firstpositive<0) return;

  int ilim0[2]={6,firstpositive+6};
  int ilim1[2]={firstpositive-5,ipeak-5};
  for (int irg=0; irg<2; irg++) {
    int nvdif[6] = {0,0,0,0,0,0};
    int ivdif1[6] = {0,0,0,0,0,0};
    int ivdif2[6] = {0,0,0,0,0,0};
    for (int i=ilim0[irg]; i<ilim1[irg]; i++) {
      double mydif1=peakpos[i]-peakpos[i-1];
      double mydif2=peakpos[i]-peakpos[i-2];
      if(mydif1 > 1.7 && mydif1 < 2.3) {
	int ibin = floor(mydif1*10-17);
	nvdif[ibin]++;
	if( irg==0 || ivdif1[ibin]==0 ) ivdif1[ibin]=i;
      }
      if(mydif2 > 1.7 && mydif2 < 2.3) {
	int ibin = floor(mydif2*10-17);
	nvdif[ibin]++;
	if( irg==0 || ivdif2[ibin]==0 ) ivdif2[ibin]=i;
      }
    }
    int imax=0, nmax=0;
    for (int i=0; i<6; i++) 
      if (nvdif[i] > nmax) {
	nmax=nvdif[i]; imax=i;
      }
    if (imax<2) imax=2; 
    if (imax>3) imax=3; 
    int j1, j2;
    j1 = j2 = ivdif1[imax] ? ivdif1[imax] : ivdif2[imax];
    while ( j1>ilim0[irg]+1 ) {
      if( peakpos[j1] !=0.0 ) {
	if( fabs(peakpos[j1]-peakpos[j1-2]-fullRF) < 0.3 ) {
	  peakpos[j1-1] = 0.;
	  j1--;
	}
	else if( fabs(peakpos[j1]-peakpos[j1-1]-fullRF) > 0.3 ) 
	  peakpos[j1-1] = 0.;
      }
      j1--;
    }
    while ( j2<ilim1[irg]-1 ) {
      if( peakpos[j2]!=0.0 ) {
	if( fabs(peakpos[j2+2]-peakpos[j2]-fullRF) < 0.3 ) {
	  peakpos[j2+1] = 0.;
	  j2++;
	}
	else if( fabs(peakpos[j2+1]-peakpos[j2]-fullRF) > 0.3 ) 
	  peakpos[j2+1] = 0.;
      }
      j2++;
    }
  }
  double* mypeakpos = new double [n];
  double* mypeak_no = new double [n];
  int mypeak=0;
  int myfirstpositive = -1;
  for (int i=0; i<ipeak; i++) {
    if (i==firstpositive) 
      myfirstpositive = mypeak;
    if (peakpos[i] !=0.0) {
      mypeakpos[mypeak] = peakpos[i];
      mypeak_no[mypeak] = mypeak;
      mypeak++;
    }
  }
  //  cout<<" mypeak "<<mypeak;
  //  for(int i=0; i<mypeak; i++) cout<<" "<<mypeakpos[i];
  //  cout<<endl;
  char graphname[80];
  char graphtitle[80];
  sprintf (graphname, "slope%03d", chan+1);
  sprintf (graphtitle, "fit peak time vs peak no (T-counter %d)", chan+1);
  //  gSlope[chan] = new TGraph (ipeak, peak_no, peakpos);
  gSlope[chan] = new TGraph (mypeak, mypeak_no, mypeakpos);
  gSlope[chan]->SetNameTitle(graphname, graphtitle);
  //  TGraph* gHelp = new TGraph (ipeak, peakpos, p_zeros);
  TGraph* gHelp = new TGraph (mypeak, mypeakpos, p_zeros);

  //  TF1* fitslope = new TF1("fitslope", myslopefun, peakpos[0], peakpos[ipeak-1], 2);
  //  TF1* fa = new TF1("fa", "[0]+x*[1]", 5., firstpositive-5. );
  //  TF1* fb = new TF1("fb", "[0]+x*[1]", firstpositive+5., ipeak-5.);
  TF1* fitslope = new TF1("fitslope", myslopefun, mypeakpos[0], mypeakpos[mypeak-1], 2);
  TF1* fa = new TF1("fa", "[0]+x*[1]", 5., myfirstpositive-5. );
  TF1* fb = new TF1("fb", "[0]+x*[1]", myfirstpositive+5., mypeak-5.);
  fitslope->SetParameters(0., fullRF);
  fitslope->SetParLimits(0, -2*fullRF, 2*fullRF);
  fitslope->SetParLimits(1, 1.94, 2.06);


  //  if (gHelp->Fit(fitslope,"0q","", peakpos[5], peakpos[firstpositive-5])) {
  if (gHelp->Fit(fitslope,"0q","", mypeakpos[5], mypeakpos[myfirstpositive-5])) {
    cout << "fitslope (below peak) failed for T-counter " << chan+1 << endl;
    //    return;
  }
  else 
    cout << "fitslope (below peak) for T" << chan+1 <<": " << fitslope->GetParameter(1) <<endl;
  //  fa->SetParameters(0., fitslope->GetParameter(1));
  //  fa->FixParameter(1, fitslope->GetParameter(1));
  fa->SetParameters(0., fullRF);
  gSlope[chan]->Fit(fa, "0q", "", 5., myfirstpositive-5.);
  //  gSlope[chan]->Fit(fa, "0q", "", 5., firstpositive-5.);

  fitslope->SetParameters(0., fullRF);
  fitslope->SetParLimits(0, -2*fullRF, 2*fullRF);
  fitslope->SetParLimits(1, 1.94, 2.06);

  //  if (gHelp->Fit(fitslope,"0q","", peakpos[firstpositive+5], peakpos[ipeak-5] )) {
  if (gHelp->Fit(fitslope,"0q","", mypeakpos[myfirstpositive+5], mypeakpos[mypeak-5] )) {
    cout << "fitslope (above peak) failed for T-counter " << chan+1 << endl;
    //    return;
  }
  else
    cout << "fitslope (above peak) for T" << chan+1 <<": " << fitslope->GetParameter(1) <<endl;
  //  fb->SetParameters(0., fitslope->GetParameter(1));
  //  fb->FixParameter(1, fitslope->GetParameter(1));
  fb->SetParameters(0., fullRF);
  gSlope[chan]->Fit(fb, "0q", "", myfirstpositive+5., mypeak-5.);
  //  gSlope[chan]->Fit(fb, "0q", "", firstpositive+5., ipeak-5.);
  
  fb->SetLineColor(2);
  fa->SetLineColor(4);
  gSlope[chan]->GetListOfFunctions()->Add(fa);
  gSlope[chan]->GetListOfFunctions()->Add(fb);
  //  fa->Draw("same");

  double ma = fa->GetParameter(1);
  double mb = fb->GetParameter(1);
  double mm = 0.;
  if (ma > 1.94 && ma < 2.06) {
    mm = ma;
    if (mb > 1.94 && mb < 2.06) 
      mm = (ma + mb)/2.;
  }
  else if (mb > 1.94 && mb < 2.06) 
    mm = mb;
  //  double mm = (fa->GetParameter(1) + fb->GetParameter(1)) /2. ;
  //  double bb = (fb->GetParameter(0) - fa->GetParameter(0)) / mm;
  double bb = mm>0.0 ? (fb->GetParameter(0) - fa->GetParameter(0)) / mm : 0.0;
  cout << chan+1 << " \t " << mm <<" : ("<< ma <<" + "<< mb <<")/2 \t " << bb << endl;

  /*
  if (C) {
    gSystem->ProcessEvents();
    C->Modified();
    C->Update();
    char giffilename[80];
    sprintf (giffilename, "gif/slope_fit%03d.gif", chan);
    //    C->Print(giffilename);
  }
  */

  delete [] peakpos;
  delete [] peak_no;
  hresult[r_slope]->SetBinContent(chan+1, mm);
}

void extract_maximum(int chan, TH2F* h) {
  int ix, iy, iz;
  h->GetMaximumBin(ix, iy, iz);
  maxx0[chan] = h->GetXaxis()->GetBinCenter(ix);
  maxy0[chan] = h->GetYaxis()->GetBinCenter(iy);
}

void tagleftright(int chan) {
  /// name of the histogram for T-counter L-R difference vs TDC sum
  char hstname[80];
  sprintf (hstname, "lr%03d", chan+1);

  /// try to locate the histogram
  TH2F* h = (TH2F*) locateDirObject(hstname,"lr");

  /// unsuccessful
  if (!h) {
    cout << "histogram not found! <" << hstname << ">" << endl; return; }

  int    nx = h->GetNbinsX();
  int    ny = h->GetNbinsY();
  double hmax = 0;

  mem->cd();
  sprintf (hstname, "lrclean%03d", chan+1);
  TH2F* hsp = (TH2F*) h->Clone(hstname);
  hsp->SetOption("colz");

  extract_maximum(chan, hsp);

  /// clean up histogram
  if (hsp->GetMaximum() < empty) {
    cout << "T" << chan+1 << ": histogram maximum less than " << empty << endl; return; }

  /// X-projection helps finding the plateau
  TH1D *hproj = hsp->ProjectionX();

  /// weed out peak
  
  int iweed[NWEED];
  for (int i=0; i<NWEED; i++) {
    int imaxbin = hproj->GetMaximumBin();
    iweed[i] = imaxbin - 1;  // remember ROOT bins count 1..n
    hproj->SetBinContent(imaxbin, 0.);
  }

  hmax =  0.7 * hproj->GetMaximum();

  for (int i=0; i<nx; i++) {
    bool weedbin = false;
    for (int j=0; j<NWEED; j++) 
      if( i==iweed[j] ) weedbin = true;
    if ( !weedbin && hproj->GetBinContent(i+1) < hmax ) {
      for (int j=0; j<ny; j++) 
	hsp->SetBinContent(i+1, j+1, 0.);
    }
  }

  /// create profile
  hprof[chan] = hsp->ProfileX();

  if (hprof[chan]->Fit("pol1", "q")) {
    cout << "fit failed for T-counter " << chan+1 << endl;
    return;
  }

  TF1* fitlr = (TF1*) hprof[chan]->GetFunction("pol1");
  fitlr->SetLineColor(2);

  double mm = fitlr->GetParameter(1);
  double bb = fitlr->GetParameter(0);
  double cs = fitlr->GetChisquare();

  cout << chan+1 << " \t " << mm << " \t " << bb << endl;

  /*
  if (C) {
    gSystem->ProcessEvents();
    C->Modified();
    C->Update();
    char giffilename[80];
    sprintf (giffilename, "gif/lr_a1_fit%03d.gif", chan);
    //    C->Print(giffilename);
  }
  */

  hresult[r_lr_a1]->   SetBinContent(chan+1, mm);
  hresult[r_lr_a0]->   SetBinContent(chan+1, bb);
  hresult[r_lrchisq]-> SetBinContent(chan+1, cs);
}

void checkin() {
  char command[255];
  sprintf(command,"./checkin.tcl %d %s",runno,subdirectory);
  gSystem->Exec(command);
}

void tagslope() {
  gStyle->SetStatX(0.40);
  gStyle->SetStatY(0.85);
  gStyle->SetStatW(0.15);
  gStyle->SetStatH(0.15);
  gStyle->SetOptStat(0);

  gROOT->cd();
  mem = new TDirectory("mem", "mem");
  gROOT->GetListOfBrowsables()->Add(mem,"mem");
  mem->cd();

  char* res_name[r_max] = {
    "slope_fit", "lr_a0_fit", "lr_a1_fit", "lr_chisq", 
    "RF_area", "RF_meang", "RF_sigma", "RF_chisq"
  };

  for (int i=r_slope; i<r_max; i++) {
    hresult[i] =  new TH1D(res_name[i], res_name[i], 
			   TCNT_MAX, 0.5, TCNT_MAX+0.5);
    hresult[i]->SetFillColor(5);
  }

  memset(hrfslice, 0, sizeof(hrfslice));
  memset(gSlope, 0, sizeof(gSlope));
  memset(hprof, 0, sizeof(hprof));

  TH1F* hconst_run = (TH1F*) locateDirObject("const_run");
  if (hconst_run) {
    char canvtitle[80];
    sprintf(canvtitle, "tagslope fit  (run %d)", hconst_run->GetBinContent(1));
    C = new TCanvas("C", canvtitle, 900, 650);
  }
  else
    C = new TCanvas("C", "Fit", 900, 650);

  cout << "fit RF position <RFused-tag> " << endl; 
  tagrffit();

  cout << "\n================================\n\n"
       << "fit left-right time vs. left+right TDC" << endl; 
  cout << "T-counter    slope  \t  offset " << endl;
  for (int i=0; i<TCNT_MAX; i++) {
    //    C->Clear();
    tagleftright(i);
  }

  cout << "\n================================\n\n"
       << "fit left-right peaks (expected difference: RF cycle)" << endl; 
  for (int i=0; i<TCNT_MAX; i++) {
    //    C->Clear();
    tagslopechan(i);
  }

  calculate_const();

  gStyle->SetOptStat(0);
  mem->UseCurrentStyle();

  switch_histo(gResult);

  TControlBar* menu = new TControlBar("vertical","Select", 10, 480 );
  char command[80];

  for (int i=r_slope; i < r_max; i++) {
    sprintf(command, "switch_histo(%d)", i);
    menu-> AddButton (hresult[i]->GetName(), command, hresult[i]->GetTitle());
  }

  menu->AddSeparator();
  menu->AddButton ("checkin", "checkin()", "check constants into database");
  menu->AddButton ("exit", ".q", "terminate the program");
  menu->Show();

  mem->UseCurrentStyle();
  
  cout<<"\n Click on counter bin entry in upper plot (with yellow border) or"<<endl;
  cout<<" or right-click menu list in any pad to get plot for single T-counter bin"<<endl;

  show_tcounter(TAGTrefid);
}
