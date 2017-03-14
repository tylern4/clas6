//$Id: test_stn_veff_fit.C,v 1.2 2008/03/27 14:22:50 pasyuk Exp $
#ifndef __CINT__
#include "ROOT.h"
#include<iostream>
#include<fstream>

using namespace std;
#else
const double M_PI = TMath::Pi();
#endif

char* histogram_to_fit    = "dt_dist%02d_pion_0";
char* histogram_directory = "veff";
char* histogram_title     = "t_{ST} - t_{TAG} vs ST position";
double min_entries_for_fit = 15.;
int min_bins_for_calibration = 15;

//******************
const char BEEP='\a';


const double SQRT_2PI = sqrt(2 * M_PI);
const double z1 = 52.3;   /// leg length
const int newfit = 24;    /// index to store new fit results in global array

int gIndex;               /// the current channel to fit

/// calibration table used
char* caldb_table[] = {"delta_T_pd2pd",
		       "veff_leg",
		       "veff_nose2",
		       "veff_nose1",
		       "veff_nose",
		       NULL };
enum caldb_table_t {cdb_deltaT, cdb_veffLeg, cdb_veffNose2, 
		    cdb_veffNose1, cdb_veffNose, cdb_max};

/// constants from last good fit
double gLastFit[cdb_max];


/*********START***********/
/************************/
int count1 = 0;
int count2 = 0;

ofstream out_delta("STN_CALIB_delta_T_pd2pd.dat",ios::ate);
out_delta.flags(ios::fixed);
out_delta.precision(4);

ofstream out_veff_leg("STN_CALIB_veff_leg.dat",ios::ate);
out_veff_leg.flags(ios::fixed);
out_veff_leg.precision(4);

ofstream out_veff_nose("STN_CALIB_veff_nose.dat",ios::ate);
out_veff_nose.flags(ios::fixed);
out_veff_nose.precision(4);

ofstream out_veff_nose1("STN_CALIB_veff_nose1.dat",ios::ate);
out_veff_nose1.flags(ios::fixed);
out_veff_nose1.precision(4);

ofstream out_veff_nose2("STN_CALIB_veff_nose2.dat",ios::ate);
out_veff_nose2.flags(ios::fixed);
out_veff_nose2.precision(4);

/***************************/
/***************************/




/// rightmost slice successful fitted
double zMax; 
double vMax;

TDialogCanvas* C = NULL;
TPad* C1 = NULL;
TDirectory* mem = NULL;
TH1F* hcaldb[2][cdb_max];
TH1F* hchisq;

bool ready_for_checkin = false;
bool exit_program      = false;

char* user_action[] = {"fit OK", "refuse fit", "check in", "quit", NULL };
enum user_action_t {act_ok, act_refuse, act_checkin, act_quit };
TButton* gBut[4];

void st_fit_next_channel();
void handle_user_action(int choice);

#ifndef __CINT__
class JButton : public TButton {
  bool active;
  int selection;
  int lastevent;
public:
  JButton(int selection_, Double_t x1, Double_t y1,Double_t x2, Double_t y2) :
    TButton(user_action[selection_], "", x1, y1, x2, y2), 
    active(true), selection(selection_), lastevent(0) {}
  void ExecuteEvent(Int_t event, Int_t px, Int_t py) {
    //    cout << "<" << event << "," << px << "," << py << "> "; cout.flush();
    if (!active) return;
    TButton::ExecuteEvent(event, px, py);
    if (exit_program) return;

    if (event==11 && lastevent==1) {
      //      cout << "calling handle_user_action(" << selection << ")" << endl;
      handle_user_action(selection);
    }
    lastevent = event;
  }

  void SetActive(bool active_) {
    active = active_;
    if (active) SetTextColor(1);
    else        SetTextColor(22);
    Modified();
  }
};
#endif

const bool debug_fit = false;

/// calculate veff_nose for current set of par so that function hits cornerpoint (zMax,vMax)
double veff_nose(double* par) {
  double dz = zMax - z1;
  double arg = vMax - par[0] - z1/par[1] - par[2]*dz*dz - par[3];
  if (arg==0.) return 1.E9;
  return dz/arg;
}

/// veff correction function
double veff_function(double*x, double* par) {
  if (*x < z1) 
    return par[0] + (*x)/par[1];

  double z = *x - z1;
  return par[0] + z1/par[1] + z/par[4] + par[2]*z*z + par[3]; 
}

/// as above, but use fixed point zMax,vMax to restrain function
double veff_function_restrained(double*x, double* par) {
  if (*x < z1) 
    return par[0] + (*x)/par[1];

  double z = *x - z1;
  return par[0] + z1/par[1] + z/veff_nose(par) + par[2]*z*z + par[3]; 
}


double IntegralAround(TH1D* h, double x0, double dx) {
  int i0 = h->FindBin(x0 - dx);
  int i1 = h->FindBin(x0 + dx);

  double integral = 0;
  for (int i=i0; i<=i1; i++) {
    integral+= h->GetBinContent(i);
  }
  return integral;
}

void show_final_plots() {
  C1->Clear();
  C1->Divide(2,3);
  
  gStyle->SetOptStat(0);
  mem->UseCurrentStyle();
  hcaldb[1][cdb_deltaT]->Scale(-1.);
  hcaldb[1][cdb_deltaT]->Add(hcaldb[0][cdb_deltaT]);

  for (int i=0; i<cdb_max; i++) {
    char outfilename[80];
    sprintf (outfilename, "/home/julian/caldb/STN_CALIB_%s.dat",caldb_table[i]);
    ofstream of(outfilename);
    for (int j=0; j<24; j++) {
      of << hcaldb[1][i]->GetBinContent(j+1) << endl;
    }
    C1->cd(i+1);
    hcaldb[1][i]->SetFillColor(5);
    hcaldb[1][i]->Draw();
    hcaldb[0][i]->SetLineColor(34);
    hcaldb[0][i]->Draw("same");
  }
  
  C1->cd(cdb_max+1);
  hchisq->SetFillColor(5);
  hchisq->Draw();
  C1->Modified();
  C1->Update();

}

void handle_user_action(int choice) {
  switch (choice) {
  case act_ok: 
    if (ready_for_checkin) return;
    for (int i=0; i<cdb_max; i++) {
      hcaldb[1][i]->SetBinContent(gIndex+1, gLastFit[i]);
    }

    // no break here, go to next channel  
  case act_refuse:
    if (ready_for_checkin) return;
    gIndex++;
    if (gIndex < newfit) {
      st_fit_next_channel();
    }
    else {
      ready_for_checkin = true;
      show_final_plots();
#ifndef __CINT__
      ((JButton*) gBut[act_ok]     )->SetActive(false);
      ((JButton*) gBut[act_refuse] )->SetActive(false);
      ((JButton*) gBut[act_checkin])->SetActive(true);
#endif
      C->Modified();
      C->Update();
      C->Print("gif/comparison.gif");
    }
    break;
  case act_checkin:
    if (!ready_for_checkin) return;
    gSystem->Exec("./checkin.rc");
    break;
  case act_quit:
    cout << "good bye" << endl;
    exit_program = true;
    exit(0);
  }
}

/*********************************************************************/
/********     ST_FIT_BY_POINTER **************************************/
/********     st_fit_by_pointer **************************************/
/*********************************************************************/

void st_fit_by_pointer(TH2F* h) {
  
  int nx = h->GetXaxis()->GetNbins();
  int ny = h->GetYaxis()->GetNbins();

  double* gauspeak = new double[nx];
  double* gausigma = new double[nx];
  double* gausarea = new double[nx];
  double* gauserry = new double[nx];
  double* gauserrx = new double[nx];
  double* gausbinx = new double[nx];
  int nfit = 0;
  
  bool fitOK = false;
  mem->cd();

  /// gauss fit for slices
  for (int i=0; i<nx; i++) {
    gSystem->ProcessEvents();
    char slicename[80];
    sprintf (slicename, "%s_yslice%02d", h->GetName(), i);
    TH1D* hsliy = h->ProjectionY(slicename, i+1, i+1);
    double par0 = hsliy->GetMaximum() * 0.7;
    double par1 = hsliy->GetBinCenter(hsliy->GetMaximumBin());

    double nentries = IntegralAround(hsliy, par1, 2.);

    /// region around maximum has enough entries
    if (nentries > min_entries_for_fit) {
      sprintf (slicename, "%s_fit", slicename);
      TF1* fgaus = new TF1(slicename, "[0]*exp(-0.5*pow((x-[1])/[2],2))", par1-1.2, par1+1.2);
      fgaus->SetParameters(par0, par1, 0.25);

      TF1* before = (TF1*) fgaus->Clone();
      before->SetParameters(par0, par1, 0.25);
      before->SetLineColor(4);
      if (hsliy->Fit(fgaus, "0qwr") == 0) {

	if (debug_fit) {
	  hsliy->Draw();
	  fgaus->SetLineColor(2);
	  fgaus->Draw("same");
	  before->Draw("same");
	  C1->Modified();
	  C1->Update();
	  TMarker* m = (TMarker*) C1->WaitPrimitive("TMarker", "Marker");
	  m->Draw();
	}
	gausbinx[nfit] = h->GetXaxis()->GetBinCenter(i+1);
	gauserrx[nfit] = 1.E-9; /// center of X bin well known
	gauserry[nfit] = fgaus->GetParError(1);
	gausigma[nfit] = fgaus->GetParameter(2);
	gauspeak[nfit] = fgaus->GetParameter(1);
	gausarea[nfit] = fgaus->GetParameter(0) * gausigma[nfit] * SQRT_2PI 
	  / h->GetYaxis()->GetBinWidth(i+1);

	if (0.1 < gausigma[nfit] && gausigma[nfit] < 1. 
	    && h->GetYaxis()->GetXmin() < gauspeak[nfit]
	    && h->GetYaxis()->GetXmax() > gauspeak[nfit]
	    && nentries    < 2. * gausarea[nfit]
	    && 2.*nentries >      gausarea[nfit] ) {  
	  nfit++;
	  fgaus->SetLineColor(2);
	  hsliy->SetFillColor(5);
	  hsliy->GetListOfFunctions()->Add(fgaus);
	}
      }
      
      //      delete fgaus;
    }
  }

  /// normalize histogram
  for (int i=0; i<nx; i++) {
    double max = 0.;
    double z;
    for (int j=0; j<ny; j++) {
      if ( (z=h->GetBinContent(i+1, j+1)) > max) max = z;
    }
    if (max > 0) {
      for (int j=0; j<ny; j++) {
	z=h->GetBinContent(i+1, j+1)/max;
	if (z < 0.1) z = 0;
	h->SetBinContent(i+1, j+1, z);
      }
    }
  }

  /// set corner
  if (nfit) {
    zMax = gausbinx[nfit-1];
    vMax = gauspeak[nfit-1];
  }

  char funname[80];
  sprintf (funname, "%s_fit_new", h->GetName());
  TF1* fitfun = new TF1(funname, veff_function_restrained, 0, 80, 4);
  fitfun->SetParameters(0.1, 
			hcaldb[0][cdb_veffLeg]->GetBinContent(gIndex+1), 
			-0.0001, -0.01);
  TGraphErrors* ge = NULL;

  if (nfit >= min_bins_for_calibration) {
    ge = new TGraphErrors(nfit, gausbinx, gauspeak, gauserrx, gauserry);
    char funname[80];
    sprintf (funname, "%s_fit", h->GetName());
    fitfun->SetParLimits(1, 5., 33.);
    ge->Fit(fitfun, "0q", "", 5, 48); 

    double par0 = fitfun->GetParameter(0);
    double par1 = fitfun->GetParameter(1);
    fitfun->SetParameters(par0, par1, -0.01, 0.05);
    fitfun->SetParLimits(0, par0, par0);
    fitfun->SetParLimits(1, par1, par1);
    fitfun->SetParLimits(2, -0.1, 0.);
    fitfun->SetParLimits(3, -0.2, 0.2);
    fitfun->SetLineColor(1);
    fitfun->SetRange(5, zMax+1.);

    if (ge->Fit(fitfun, "0q", "", 5, zMax+1.) == 0) {
      cout << "calibration constants successfully extracted, please check fit quality" << endl;
      
      /**********************/
      cout << fitfun->GetParameter(0) << "\n";
      cout << fitfun->GetParameter(1) << "\n";
      cout << fitfun->GetParameter(2) << "\n";
      cout << fitfun->GetParameter(3) << "\n";
      cout << fitfun->GetParameter(4) << "\n";
      /*********************/


      fitOK = true;
    }
    else {
      cout << "fit was not successful, calibration constants will remain unchanged" << endl;
    }
  }
  else {
    cout << "only " << nfit 
	 << " y-bins were fitted successfully, (at least " 
	 << min_bins_for_calibration << " needed) for calibration"
	 << endl;
  }


  delete[] gauspeak;
  delete[] gausigma;
  delete[] gausarea;
  delete[] gauserrx;
  delete[] gauserry;
  delete[] gausbinx;


  C1->Clear();
  C1->cd();
  h->Draw("colz");

  if (ge) {
    ge->SetMarkerStyle(7);
    ge->Draw("P");
  }


  C1->Modified();
  C1->Update();


  double y0 = h->GetYaxis()->GetXmin();
  double y1 = h->GetYaxis()->GetXmax();

  /// draw function with current parameter
  sprintf (funname, "%s_fit_old", h->GetName());
  TF1* fitold = new TF1(funname, veff_function, 0, zMax+2., 5);
  fitold->SetParameter(0, 0.);
  for (int i=1; i<5; i++) {
    fitold->SetParameter(i, hcaldb[0][i]->GetBinContent(gIndex+1));
    
    /***********************/
    cout << "old " << hcaldb[0][i]->GetBinContent(gIndex+1) << "\n";
    /*************************/
  
  }

  /*******************/
  fitold->SetLineColor(2);  // it is  (2)red into the plot and a fit from old constants 
  //  fitold->SetLineStyle(3);
  fitold->Draw("same");
  
  //draw new function
  fitfun->SetLineColor(3);   // it is (3) green into the plot and a fit from new constants 
  fitfun->Draw("same");
  /*********************/


  TLine* lp = new TLine (z1, y0, z1, y1);
  lp->SetLineColor(22);
  lp->Draw();


  TLine* lt = new TLine (0, y0, 0, y1);
  lt->SetLineColor(22);
  lt->SetLineStyle(3);
  lt->Draw();

  TLine* lx = new TLine (zMax, y0, zMax, y1);
  lx->SetLineColor(22);
  lx->Draw();

  fitfun->GetParameters(gLastFit);
  gLastFit[4] = veff_nose(gLastFit);
  hchisq->SetBinContent(gIndex+1, fitfun->GetChisquare());

  char* cname[5] = { "#Delta t_{P2P}", "veff_{LEG}", "C2_{NOSE}", "C0_{NOSE}", "veff_{NOSE}" };

  for (int ipar=0; ipar<5; ipar++) {
    double y0 = h->GetYaxis()->GetXmax() - 1.2;
    char cpar[80];
    sprintf (cpar, "%s ...", cname[ipar]);

    cout << cpar ;

    TLatex* tl = new TLatex (-17, y0-1.2*ipar, cpar);
    tl->SetTextSize(0.025);
    tl->Draw();
    sprintf (cpar, "%9.5f", gLastFit[ipar]);
    
    cout << " " <<  gLastFit[ipar] << "\n";

    TText* tt = new TLatex (-1., y0-1.2*ipar, cpar);
    tt->SetTextSize(0.04);
    tt->Draw();

    
    /***************START*********************/
    /************************************/
   
    if(ipar==0)
      out_delta << gLastFit[ipar] << endl;
    
    if(ipar==1)
      out_veff_leg << gLastFit[ipar] << endl;
    
    if(ipar==2)
      out_veff_nose2 << gLastFit[ipar] << endl;
    
    if(ipar==3)
      out_veff_nose1 << gLastFit[ipar] << endl;
    
    if(ipar==4)
      out_veff_nose << gLastFit[ipar] << endl;
    

    
    /**************************************/
    /*****************END****************/
    

  }
  
  C1->Modified();
  C1->Update();
 
  /****************/
  char c1_name[30];
  sprintf(c1_name,"gif/ts%d%d.gif",gIndex/4+1,gIndex%4+1);
  C1->Print(c1_name);
  /*****************/
 
  cout << "========================= (click button to continue) =======" << endl;
  cout << BEEP << "hurry.. push the botton" << "\n";
  cout << BEEP << "hurry.. push the botton" << "\n";
  cout << BEEP << "hurry.. push the botton" << "\n";
  cout << BEEP << "hurry.. push the botton" << "\n";

}

TH1* find_histogram(char*dirname,  char* hstname, bool verbose=true) {
  TH2F* h = (TH2F*) gROOT->FindObject(hstname);

  /// try memory first
  if (h) {
    if (verbose)
      cout << "\t histogram found in ROOT memory ..." << endl;
    return (h);
  }

  /// try all open files
  gROOT->GetListOfFiles();
  TIter nextfile(gROOT->GetListOfFiles());
  TFile *f;
  while ((f = (TFile*) nextfile() )) {
    f->UseCurrentStyle();
    if (verbose)
      cout << "\t file: <" << f->GetName() << "> ... " << endl; 
    TDirectory* dir = (TDirectory*) f->Get(dirname);
    if (dir) {
      if (verbose) cout << "\t directory found ... " << endl;
      if ((h = (TH2F*) dir->Get(hstname))) { 
	if (verbose) cout << "\t histogram found ..." << endl;
	return(h);
      }
      if (verbose) cout << "\t histogram not found ..." << endl;
    }
    else {
      if (verbose) cout << "\t directory not found ..." << endl;
    }
  }
  return NULL;
}


void st_fit_next_channel() {
  if (gIndex < 0 || gIndex >= 24) return;
  char hstname[80];
  sprintf (hstname, histogram_to_fit, gIndex);
  cout << "looking for histogram " << hstname 
       <<" in directory " << histogram_directory << endl;

  TH2F* h = (TH2F*) find_histogram(histogram_directory, hstname, true);

  if (!h) {
    cout << "search for <" << hstname << "> unsuccessful" << endl; return; }

  sprintf (hstname, "%s, sector %d, segment %d", histogram_title, gIndex/4+1, gIndex%4+1);
  h->SetTitle(hstname);
  st_fit_by_pointer(h);
}

void load_old_constants() {
  cout << "loading current calibration constants (system=STN_CALIB)" << endl;
  mem->cd();

  for (int i=0; i<cdb_max; i++) {
    char hstname[80];
    sprintf (hstname, "caldb_stn_%s", caldb_table[i]);
    hcaldb[0][i] = (TH1F*) find_histogram("caldb", hstname, false);
    if (hcaldb[0][i]) {
      cout << "\t table " << caldb_table[i] << "loaded" << endl;
      hcaldb[1][i] = (TH1F*) hcaldb[0][i]->Clone();
    }
    else {
      cout << "\t histogram " << hstname << " not found" << endl;
      exit(-1);
    }
  }
  for (int i=0; i<24; i++) {
    hcaldb[1][cdb_deltaT]->SetBinContent(i,0.);
  }
  hchisq = new TH1F("chisq", "Chi-square from fit", 24, -0.5, 23.5);
}

void draw_buttons() {
  /// count number of actions first

  C->cd();
  double nbut = 0;
  for (int i=0; user_action[i] != NULL; i++) nbut+= 1.;


  for (int i=0; user_action[i] != NULL; i++) {
#ifdef __CINT__
    char callhandle[80];
    sprintf (callhandle, "handle_user_action(%d)", i);
    cout << callhandle << endl;
    gBut[i] = new TButton(user_action[i], callhandle, 
			  i/nbut+0.005, 0.01, (i+1.)/nbut-0.005, 0.07);
    gBut[i]->Draw();
#else
    JButton* but = new JButton(i, i/nbut+0.005, 0.01, (i+1.)/nbut-0.005, 0.07);
    but->SetActive(i != act_checkin);     /// all buttons but check-in active
    but->Draw();
    gBut[i] = but;
#endif
  }

  C->Modified();
  C->Update();
}

void test_stn_veff_fit() {
  gStyle->SetOptFit(0);
  gStyle->SetOptStat(0);
  gStyle->SetPadBottomMargin(0.13);
  gStyle->SetPadRightMargin(0.13);
  gStyle->SetPadLeftMargin(0.05);
  gStyle->SetStatX(0.865);
  gStyle->SetStatW(0.14);
  gStyle->SetStatY(0.895);
  gStyle->SetStatH(0.14);
  gROOT->UseCurrentStyle();

  mem = new TDirectory("mem", "mem");
  gROOT->GetListOfBrowsables()->Add(mem,"mem");

  load_old_constants();

  gStyle->SetOptStat(111110);

  C = new TDialogCanvas ("C", "C",400, 400);
  C->SetFillColor(42);
  C1 = new TPad("C1", "C1", 0.005, 0.08, 0.995, 0.995, 3, 1);
  C1->SetFillColor(41);
  C1->Draw();
  
  
  
  /// new constants
  ofstream ffconst("/tmp/stn_calib_const.dat");
  ofstream ffvleg("/tmp/stn_calib_veff_leg.dat");
  ofstream ffvnose("/tmp/stn_calib_veff_nose.dat");
  ofstream ffvnose0("/tmp/stn_calib_veff_nose1.dat");
  ofstream ffvnose2("/tmp/stn_calib_veff_nose2.dat");
  

  ffconst.flags(ios::fixed);
  ffvleg.flags(ios::fixed);
  ffvnose.flags(ios::fixed);
  ffvnose0.flags(ios::fixed);
  ffvnose2.flags(ios::fixed);

  ffconst.precision(4);
  ffvleg.precision(4);
  ffvnose.precision(4);
  ffvnose0.precision(4);
  ffvnose2.precision(4);

  gIndex = 0;

  st_fit_next_channel();
    /*
    if (!fvleg.good()) { cout << "error reading veff leg" << endl; break; }
    fvleg >> gVeffLeg;
    if (!fvnose.good()) { cout << "error reading veff nose" << endl; break; }
    fvnose >> gVeffNose;
    if (!fvnose0.good()) { cout << "error reading veff nose0" << endl; break; }
    fvnose0 >> gVeffNose0;
    if (!fvnose2.good()) { cout << "error reading veff nose2" << endl; break; }
    fvnose2 >> gVeffNose2;
    */
    
  draw_buttons();

}

#ifndef __CINT__
int main(int argc, char** argv) {
  if (argc < 2) {
    cerr << "Usage: " << argv[0] << " <root-file>" << endl;
    exit(0);
  }

  TRint*   theApp  = new TRint("Interactive", 0, 0, 0, 0, 0 );  
  TFile* ff = new TFile(argv[1]);
  if (!ff) {
    cerr << "Can't open file " << argv[1] << endl;
    exit (-1);
  }
	
  
  test_stn_veff_fit();

  theApp->Run();
  
}
#endif
