//$Id: new_veff_fit.C,v 1.2 2008/03/27 14:22:50 pasyuk Exp $

/////////////////////////////// new_veff_fit.C /////////////////////////////////////////////////
//                                                                                            //
//                                                                                            //
//    Macro to fit a linear + polynomial function to a plot of (the difference between the    //
//  tagger and ST timing) vs.(the length along the paddle), for each of the 24 paddles        //
//  of the start counter.                                                                     //
//                                                                                            //
//  Must be run on the root file produced by running the stn_calib executable on data files.  //
//                                                                                            //
//  The macro produces a fit, paddle by paddle, then writes the constants from the fit into   //
//  a set of four .dat files, three with the new paddle constants which can then be checked   //
//  into the calibration database, and one "_old.dat" for the timing offsets, which gets      //
//  fed into the next macro, ProjectionFit.C.                                                 //  
//                                                                                            //
//  You need to have a gif/ directory in the one you're running this in, into which  .gif     //
//  versions of the paddle fits are saved and a ps/ dir for the .ps plots.                    //
//                                                                                            // 
//  The constants extracted from the fit are the gradient of the linear section (veff_leg),   //
//  and the x and x^2 coefficients from the quadratic section in the nose of the paddle,      //
//  (veff_nose and veff_nose2 respectively).                                                  //
//                                                                                            //
//  The fit is first done without constraints on parameters, apart from the x coefficient in  //
//  the nose section, which is set to 0 - 1000. About half the paddles will have a nonsense   //
//  fit in the "nose" part. You will mostly need to adjust the x^2 parameter which you can do //
//  by raising or lowering its lower limit with the buttons. This will re-fit the entire      //
//  paddle, most likely setting the x^2 parameter to the new lower limit.                     //
//  If there are still problems, constrain the parameters still further in the code           //
//  (signposted).                                                                             //
//                                                                                            //
//  The plots will only be saved and .dat files created if you've clicked through the entire  //
//  code to the final summary screen at the end.                                              //
//  New fit is in red, function with old constants in black. If you like the new fit, press   //
//  "Fit OK", if you like the old fit press "Keep Old", if you don't like either, adjust the  // 
//  new fit with the buttons, the press "fit OK". If you just went through the whole lot and  //
//  adjusted the fitting but needed to re-do one or two paddles, press "use last", which      //
//  will extract the fit parameters from the .dat files you created last and plot the fit     //
//  function with those. You then need to keep pressing "Fit OK" if you want to keep them.    //
//  If you don't like any of them, you can adjust the parameters again and then press         //
//  "Fit OK".                                                                                 //
//  Quitting the macro at any stage (either killing the window or pressing "quit") will not   //
//  modify .dat files or save plots, unless you had reached the final screen.                 //
//                                                                                            //
//  Generally, run by typing in root (after opening your desired .root file):                 //
//  [root] .x new_veff_fit.C                                                                  //
//  You can also view a single paddle:                                                        //
//  [root] .L new_veff_fit.C                                                                  //
//  [root] new_veff_fit(paddle_number)                                                        //
//  From here you can continue to the end of the paddles, but if you started in the middle,   //
//  the old constants will be kept for all the paddles up to there.                           // 
//                                                                                            //
//  The timing (y-axis) offset (called pd2pd) for each paddle can also be extracted from the  //
//  fit, but a more accurate calculation should be done by running the ProjectionFit.C macro  //
//  on the same root file (in a new root session) afterwards.                                 //
//                                                                                            //
//  ProjectionFit.C adjusts the old offset for each paddle by the required amount, and        //
//  therefore needs the old constants to make changes to. The file                            //
//  STN_CALIB_delta_T_pd2pd_old.dat produced by this macro contains these constants.          // 
//  Running ProjectionFit.C on the root file with the "_old.dat" file in the same directory   //
//  produce a new file STN_CALIB_delta_T_pd2pd.dat with the new constants.                    //                                                              
//                                                                                            //
//  That is the last stage in the calibration, now you can check in all four new              // 
//  STN_CALIB_*_.dat files into the database.                                                 //
//                                                                                            //
//  Modifications made in the g13 era and the above note:                                     //
//                 Daria Sokhan, University of Edinburgh, Nov 2007, Feb 2008                  //
//                                                                                            //
////////////////////////////////////////////////////////////////////////////////////////////////      



#ifndef __CINT__
#include "ROOT.h"
#include<iostream>
#include<fstream>
using namespace std;

#else
const double M_PI = TMath::Pi();
#endif




// Declarations ***********************************************************************************//

char* histogram_to_fit    = "dt_dist%02d_pion_0";
char* histogram_directory = "veff";
char* histogram_title     = "t_{ST} - t_{TAG} vs ST position";
double min_entries_for_fit = 15.;
int min_bins_for_calibration = 15;

const char BEEP='\a';

double xlmax;
int paddle = 999;
double increment = 0.;         // For manually adjusting the quadratic fit parameter 
int fitting_flag = 0;          // For reading a previous set of fit constants
int nlines = 0;                // For reading in previous set of fit constants
int par2_flag = 1;             // So parameter 2 (veff_nose) is always positive


ifstream infile;


double par0;
double par1;
double par2;
double par3;

double par3_last = 0.;

double new_par[5];             // Needs to be one more than number of constants because of how array is filled.
double fixed_par[24][3];       // Previous set of fit constants 

TH2F* h_copy;

const double SQRT_2PI = sqrt(2 * M_PI);
const double z1 = 52.3;   /// leg length
const int newfit = 24;    /// index to store new fit results in global array


int gIndex;               /// the current channel to fit

/// calibration table used
char* caldb_table[] = {"delta_T_pd2pd",
		       "veff_leg",
		       "veff_nose",
		       "veff_nose2",
		       NULL };
enum caldb_table_t {cdb_deltaT, cdb_veffLeg, cdb_veffNose, cdb_veffNose2, cdb_max};


/// constants from last good fit

double gLastFit[cdb_max];

double zMax; 
double vMax;

TDialogCanvas* C = NULL;
TPad* C1 = NULL;
TDirectory* mem = NULL;
TH1F* hcaldb[2][cdb_max];
TH1F* hchisq;

bool ready_for_checkin = false;
bool exit_program      = false;

char* user_action[] = {"fit OK", "keep old", "+ 0.005", "- 0.005", "+ 0.001", "- 0.001", "+ 0.0002", "- 0.0002", "use last", "quit", NULL };
enum user_action_t {act_ok, act_refuse, act_par3up_big, act_par3down_big, act_par3up_small, act_par3down_small, act_par3up_tiny, act_par3down_tiny, act_uselast, act_quit };
TButton* gBut[10];

void st_fit_next_channel();
void handle_user_action(int choice);
void read_last_fitting();


// Create new colours (a fix for old versions of root having the hideous mustard retro palette and nothing else):

TColor *yellow = new TColor(1050,1.,1.,.8,"yellow"); // kYellow-10, 390
TColor *green = new TColor(1051,.2,.8,.2,"green"); // kGreen-3, 413
TColor *pale_blue = new TColor(1052,.6,1.,1.,"pale_blue"); // kCyan-9, 423
TColor *pale_green = new TColor(1053,.6,1.,.6,"pale_green"); // kGreen-9, 407
TColor *pale_red = new TColor(1054,1.,.8,.8,"pale_red"); // kRed-10, 622
TColor *red = new TColor(1055,1.,.2,.2,"red"); // kRed-4, 628
TColor *dark_blue = new TColor(1056,.2,.6,1.,"dark_blue"); // kAzure+1, 861
TColor *blue = new TColor(1057,.2,.8,1.,"blue"); // kAzure+8, 868
TColor *violet = new TColor(1058,.2,.0,.4,"violet"); // kViolet+3, 883


// Button handling:

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




/************************************** FITTING FUNCTIONS *******************************************/



/********* Below, commented out, are old fitting functions, no longer used ***************************/

// /// calculate veff_nose for current set of par so that function hits cornerpoint (zMax,vMax)
// double veff_nose(double* par) {
//   double dz = zMax - z1;
//   // printf("zMax=%d, dz=%d, vMax=%d\n",zMax,dz,vMax);
//   double arg = vMax - par[0] - z1/par[1] - par[2]*dz*dz - par[3];
//   // printf("arg=%d, par[0]=%d, par[1]=%d, par[2]=%d, par[3]=%d",arg,par[0],par[1],par[2],par[3]);
//   //  if (arg==0.) return 1.E9;
//   return arg/dz;
// }

// /// as above, but use fixed point zMax,vMax to restrain function
// double veff_function_restrained(double*x, double* par) {
//   if (*x < z1) 
//     return par[0] + (*x)/par[1];

//   double z = *x - z1;
//   return par[0] + z1/par[1] + z/veff_nose(par) + par[2]*z*z + par[3]; 
// }

/*************************** End of old fitting functions *********************************************/


// veff correction function

double veff_function(double*x, double* par) {
  if (*x <= xlmax) 
    return par[0] + (*x)/par[1];
  else {
    double z = *x - xlmax;
    return par[0] + xlmax/par[1] + z/par[2] + par[3]*z*z; 
  }
}

// quadratic function:

double quad_function(double*x, double* par) {
  double z = *x - xlmax;
  return par[0] + par[1]*z + par[2]*z*z; 
}



// ****************************************** end of fitting function ************************************ //




// *******************************************  MAIN FUNCTION  ******************************************** //


void new_veff_fit(int paddle_dummy = 999) {

  if (paddle_dummy != 999) paddle = paddle_dummy;

  if (paddle == 999) cout << "going through the paddles one by one... " << endl;
  else cout << "paddle no is: " << paddle << endl;
  

  // Appearance of canvas:

  gStyle->SetOptFit(0);                            // No fit parameters printed
  gStyle->SetOptStat(10);                          // Just print the number of entries
  gStyle->SetPadBottomMargin(0.13);
  gStyle->SetPadRightMargin(0.13);
  gStyle->SetPadLeftMargin(0.05);
  gStyle->SetStatX(0.865);                         // X pos or top right corner of stat box
  gStyle->SetStatW(0.14);                          // width of stat box
  gStyle->SetStatY(0.895);
  gStyle->SetStatH(0.14);                          // height of stat box
  gStyle->SetPalette(100,0);                       // Beautiful deep sea palette :)

  gROOT->UseCurrentStyle();

  mem = new TDirectory("mem", "mem");
  gROOT->GetListOfBrowsables()->Add(mem,"mem");



  for (int j=0; j<24; j++){                        // Reset the previous constants to 0, in case the "use last" function is called.
    for (int i=0; i<3; i++) {
      fixed_par[j][i] = 0.;
    }
  }

  load_old_constants();

  C = new TDialogCanvas ("C", "C",1200, 900); 
  C->SetFillColor(1050);                            // kYellow-10
  C1 = new TPad("C1", "C1", 0.005, 0.08, 0.995, 0.995, 3, 1);
  C1->SetFillColor(0);                             // white (kWhite)
  C1->Draw();
 
  gIndex = 0;
  
  st_fit_next_channel( );
  
  draw_buttons();

}


/*****************************   end of main function  ******************************/



/******************* ALL THE OTHER FUNCTIONS USED IN MAIN  ****************************/


// Read in the constants from the last fit you did:

void read_last_fitting() {

  cout << "\n Reading previous fit constants." << endl;
  
  for (int i=1; i<cdb_max; i++) {
    
    char infilename[80];
    sprintf (infilename,"STN_CALIB_%s.dat",caldb_table[i]);
    infile.open(infilename);
 
    nlines = 0;
    
    if (infile.is_open()){
      cout << "file open!" << endl;
      while ((!infile.eof()) && (nlines < 24)){
	infile >> fixed_par[nlines][i-1];
	if (!infile.good()){
	  cout << "\n error reading " << infilename << endl;
	  break;
	}
	nlines++;
      }
      infile.close();
    }
    
    else {
      cout << "\n file " << infilename << " cannot be opened, sorry for the headache :(" << endl;
      fitting_flag = 0;
    }
  }
}



// Old constants loaded:

void load_old_constants() {
  
  cout << "loading current calibration constants (system=STN_CALIB)" << endl;
  mem->cd();
  
  for (int i=0; i<cdb_max; i++) {
    
    char hstname[80];
    sprintf (hstname, "caldb_stn_%s", caldb_table[i]);
    hcaldb[0][i] = (TH1F*) find_histogram("caldb", hstname, true);
    
    if (hcaldb[0][i]) {
      cout << "\t table " << caldb_table[i] << " loaded" << endl;
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




// Histogram extracted:


TH1* find_histogram(char*dirname,  char* hstname, bool verbose=true) {

  TH2F* h = (TH2F*) gROOT->FindObject(hstname);
  
  
// try memory first

  if (h) {
    if (verbose)
      cout << "\t histogram found in ROOT memory ..." << endl;
    return (h);
  }

// try all open files
    
  gROOT->GetListOfFiles();
  TIter nextfile(gROOT->GetListOfFiles());
  TFile *f;
  while ((f = (TFile*) nextfile() )) {

    f->UseCurrentStyle();

    if (verbose) cout << "\t file: <" << f->GetName() << "> ... " << endl; 

    TDirectory* dir = (TDirectory*) f->Get(dirname);

    if (dir) {
 
      if (verbose) cout << "\t directory found ... " << endl;
      if ((h = (TH2F*) dir->Get(hstname))) { 
// 	delete h;
// 	h = (TH2F*) dir->Get(hstname);
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




// Start fitting:

void st_fit_next_channel() {
  if (gIndex < 0 || gIndex >= 24) return;

  if (paddle != 999) {
    cout << "paddle no is: " << paddle << endl;
    gIndex = paddle-1;
  }
  
  char hstname[80];
  sprintf (hstname, histogram_to_fit, gIndex);
  cout << "looking for histogram " << hstname 
       <<" in directory " << histogram_directory << endl;
 
  TH2F* h = (TH2F*) find_histogram(histogram_directory, hstname, true);
 
  if (!h) {
    cout << "search for <" << hstname << "> unsuccessful" << endl; return; }
  
  sprintf (hstname, "%s, sector %d, segment %d", histogram_title, gIndex/4+1, gIndex%4+1);
  h->SetTitle(hstname);
  h->SetAxisRange(-1.,7.,"Y");       //////////  Y-AXIS LIMITS!!!!!!!! //////////////////////
  h->SetAxisRange(-10.,70.,"X");       //////////  X-AXIS LIMITS!!!!!!!! //////////////////////

  //  TH2F* h_copy = h->Clone("h_copy");

  st_fit_by_pointer(h);
   
}



/////////////////////////////////////////////////////////////////////////////////////
// The monster-fit function:
/////////////////////////////////////////////////////////////////////////////////////


void st_fit_by_pointer(TH2F* h) {

  (TH2F*) h_copy = h->Clone("h_copy");
  
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
  

  // Gaussian fit to each projection in y (of every bin in x along the paddle length):
  
  for (int i=0; i<nx; i++) {
    gSystem->ProcessEvents();
    char slicename[80];
    sprintf (slicename, "%s_yslice%02d", h->GetName(), i);
    TH1D* hsliy = h->ProjectionY(slicename, i+1, i+1);
    TH1D* hsliy = h->ProjectionY(slicename, i+1, i+1);
    double par0 = hsliy->GetMaximum() * 0.7;
    double par1 = hsliy->GetBinCenter(hsliy->GetMaximumBin()); 
   
    double nentries = IntegralAround(hsliy, par1, 2.);                   //Unsure of this integral thing!
    
       
    // Determine whether enough entries for fit in region round max of projection and set up fit function:
    
    if (nentries > min_entries_for_fit) {

      sprintf (slicename, "%s_fit", slicename);
      TF1* fgaus = new TF1(slicename, "[0]*exp(-0.5*pow((x-[1])/[2],2))", par1-1.2, par1+1.2);
      fgaus->SetParameters(par0, par1, 0.25);
      
      
      // Do the fitting and determine whether fit is "good" or "bad":
      
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
	gauserrx[nfit] = 1.E-9;                                         // center of X bin well known
	gauserry[nfit] = fgaus->GetParError(1);
	gausigma[nfit] = fgaus->GetParameter(2);
	gauspeak[nfit] = fgaus->GetParameter(1);
	gausarea[nfit] = fgaus->GetParameter(0) * gausigma[nfit] * SQRT_2PI / (h->GetYaxis()->GetBinWidth(i+1));
	
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
  
  
  
  // "Normalise" histogram h to a height of 1:
  
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
  

  cout << "so far so good! Got past projection fitting and normalising!" << endl;



  // set corner
  
  if (nfit) {
    zMax = gausbinx[nfit-1];
    vMax = gauspeak[nfit-1];
  }
  
  
  // Do the fitting to the paddle:
  
  TGraphErrors* ge = NULL;
  
  double fit1_min = 6.;
  double fit1_max = z1;
  double fit2_max = zMax;

  xlmax = fit1_max;
  
  char fitname[80];
  sprintf (fitname, "%s_quad_fit_new", h->GetName());
  
  char fitname2[80];
  sprintf (fitname2, "%s_fit_new", h->GetName());

  TF1 *polyfit1 = new TF1("polyfit1","pol1",fit1_min,fit1_max);
  TF1 *polyfit2 = new TF1(fitname,quad_function,fit1_max,fit2_max,3);
  TF1 *polyfit3 = new TF1(fitname2,veff_function,fit1_min,fit2_max,4);
  
//   cout << "old par0 (delta_T_pd2pd) = " << hcaldb[0][cdb_deltaT]->GetBinContent(gIndex+1) << endl;
//   cout << "old par1 (veff_leg) = " << hcaldb[0][cdb_veffLeg]->GetBinContent(gIndex+1) << endl;
  
  if (nfit >= min_bins_for_calibration) {

    ge = new TGraphErrors(nfit, gausbinx, gauspeak, gauserrx, gauserry);  // This is the "ridge" of the histogram.
    
    ge->Fit(polyfit1,"ROq");       //* Plots in the range specified in constructor (R), doesn't plot the fit (O), min printing (q) */ 
    polyfit1->GetParameters(&new_par[0]);
    
    par0=new_par[0];
    par1=1/new_par[1];

    ge->Fit(polyfit2,"ROq");
    polyfit2->GetParameters(&new_par[2]);

    par2=1/new_par[3];
    par3=new_par[4];

    cout << "\n Parameters from the separate linear and quadratic fits:" << endl;   
    
    cout << "\t new par0 = " << par0 << endl;
    cout << "\t new par1 = " << par1 << endl;
    cout << "\t new par2 = " << par2 << endl;
    cout << "\t new par3 = " << par3 << endl;
    
    if (fitting_flag == 1){
      
      if (fixed_par[gIndex][0] != 0.) par1 = fixed_par[gIndex][0];
      if (fixed_par[gIndex][1] != 0.) par2 = fixed_par[gIndex][1];
      if (fixed_par[gIndex][2] != 0.) par3 = fixed_par[gIndex][2];
      
      cout << "\n Fitting with: " << endl; 
      
      cout << "\t using par1 = " << par1 << endl;
      cout << "\t using par2 = " << par2 << endl;
      cout << "\t using par3 = " << par3 << endl;
    }
    
    if (increment != 0.){
      cout << "\n incrementing by: " << increment << endl;
      par3 = par3_last+increment;
      cout << "using par3 as: " << par3 << endl;
    }
      
    polyfit3->SetParameters(par0,par1,par2,par3);

    polyfit3->SetParLimits(2,0.,1000.);

    if ((fitting_flag == 1) && (increment == 0.)){
      polyfit3->SetParLimits(1,par1,par1);
      polyfit3->SetParLimits(2,par2,par2);
      polyfit3->SetParLimits(3,par3,par3);
    }
    
    if (increment != 0.) polyfit3->SetParLimits(3,par3,0.);
    
    cout << "gIndex is: " << gIndex << endl;

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    ///       ADJUST THE INDIVIDUAL PADDLE FITTING LIMITS FOR PARAMETERS BELOW !!!                   // 
    ///////////////////////////////////////////////////////////////////////////////////////////////////


//     if (gIndex == 08){                            //    paddle 3, sector 1 (the problem one)
//       polyfit3->SetParLimits(2,10.,1000.);
//     }

 //    else if (gIndex == 09){                            //    paddle 3, sector 2
//       polyfit3->SetParLimits(3,par3,par3);
//       cout << "setting limits" << endl;
//     }

    //////////////////// end of adjusting fitting limits //////////////////////////////////////////////


    if (ge->Fit(polyfit3,"ROq") == 0) {
      cout << "\n calibration constants successfully extracted, please check fit quality" << endl;
	
      cout << "\t par0 = " << polyfit3->GetParameter(0) << "  after final fit" << endl;
      cout << "\t par1 = " << polyfit3->GetParameter(1) << "  after final fit" << endl;
      cout << "\t par2 = " << polyfit3->GetParameter(2) << "  after final fit" << endl;
      cout << "\t par3 = " << polyfit3->GetParameter(3) << "  after final fit" << endl;
	
	
      polyfit3->GetParameters(&new_par[0]);

      new_par[4] = 0.;

      par3_last = new_par[3];
      
      fitOK = true;
    }

    else {
      cout << "\n Fit was not successful, calibration constants will remain unchanged" << endl;
    }

  }

  else {
    cout << "\n only " << nfit 
	 << " y-bins were fitted successfully, (at least " 
	 << min_bins_for_calibration << " needed for calibration)"
	 << endl;
  }
  
  
  delete[] gauspeak;
  delete[] gausigma;
  delete[] gausarea;
  delete[] gauserrx;
  delete[] gauserry;
  delete[] gausbinx;

  increment = 0.;
  
  //***************************************************************//
      
      
  C1->Clear();
  C1->cd();
  h->Draw("colz");



  if (ge) {
    ge->SetMarkerStyle(7);
    ge->Draw("P");
  }
  
  
  C1->Modified();
  C1->Update();
  
  
  //  cout << "get past new fit no problems!" << endl;
  
// Plot fit with old constants for comparison:
  
  char funname[80];
  sprintf (funname, "%s_fit_old", h->GetName());
  
  TF1 *fitold = new TF1(funname,veff_function,fit1_min,fit2_max,4);
  fitold->SetParameter(0, 0.);
  
  for (int i=1; i<4; i++) 
    fitold->SetParameter(i, hcaldb[0][i]->GetBinContent(gIndex+1));
  
  
  for (int i=0; i<4; i++) 
    cout << "\t old " << i << " is: " << hcaldb[0][i]->GetBinContent(gIndex+1) << "\n";
 

  /*******************/
  fitold->SetLineColor(1);  // it is  (2)red into the plot and a fit from old constants 
  //  fitold->SetLineStyle(3);
  fitold->Draw("same");
  

  if (ge) {
 //draw new function
    polyfit3->SetLineColor(2);   // it is (3) green into the plot and a fit from new constants 
    polyfit3->Draw("same");
  }

  /*********************/

 
  
  // Draw vertical lines to show regions:
  
  if (ge) {
  
    double y0 = h->GetYaxis()->GetXmin() + 18.9;
    double y1 = h->GetYaxis()->GetXmax() - 3.4;
    
    TLine* lp = new TLine (fit1_max, y0, fit1_max, y1);
    lp->SetLineColor(22);
    lp->Draw();
    
    
    TLine* lt = new TLine (0, y0, 0, y1);
    lt->SetLineColor(22);
    lt->SetLineStyle(3);
    lt->Draw();
    
    TLine* lx = new TLine (fit2_max, y0, fit2_max, y1);
    lx->SetLineColor(22);
    lx->Draw();
    
    double chisq_n = polyfit3->GetChisquare();
    double NDF = (double)polyfit3->GetNDF();
    double chisqdeg = chisq_n/NDF;
    cout << "Chi squared per degree of freedom: " << chisqdeg << endl;
    
    cout << "deg of freedom: " << polyfit3->GetNDF() << endl;
    cout << "chisq: " << polyfit3->GetChisquare() << endl;
    cout << "data points: " << ge->GetN() << endl;
    
    cout << "Chi sq from linear fit: " << (polyfit1->GetChisquare())/(polyfit1->GetNDF()) << endl;
    cout << "Chi sq from quadratic fit: " << (polyfit2->GetChisquare())/(polyfit2->GetNDF()) << endl;
    
    
    hchisq->SetBinContent(gIndex+1, chisqdeg);

  }


  // Draw the fit parameters on the canvas:

  if (ge) {
    char* cname[5] = { "Delta_t_pd2pd", "veff_leg", "veff_nose", "veff_nose2" };
   
    double y0 = h->GetYaxis()->GetXmax() - 3.8;       // Previously set to getXmax() - 1.2
  
    for (int ipar=0; ipar<4; ipar++) {
      
      char cpar[80];
      sprintf (cpar, "%s...", cname[ipar]);
      
      TLatex* tl = new TLatex (15., y0-0.5*ipar, cpar);   // Previously set to (-17, y0-1.2*ipar, cpar)
      tl->SetTextSize(0.025);
      tl->SetTextColor(6);
      tl->Draw();
      sprintf (cpar, "%9.5f", new_par[ipar]);
      
      TText* tt = new TLatex (31., y0-0.5*ipar, cpar);   // Previously set to  (-1., y0-1.2*ipar, cpar) 
      tt->SetTextSize(0.03);
      tt->SetTextColor(6);
      tt->Draw();    
    }
     
    C1->Modified();
    C1->Update();
    
  }
  
  /****************/
  char c1_name[30];
  sprintf(c1_name,"gif/ts%d%d.gif",gIndex/4+1,gIndex%4+1);
  C1->Print(c1_name);
  char c1_name2[30];
  sprintf(c1_name2,"ps/ts%d%d.ps",gIndex/4+1,gIndex%4+1);
  C1->Print(c1_name2);
  /*****************/
  
  cout << "\n ***********  Click a button to continue!!  **********\n" << endl;
//   cout << BEEP << "hurry.. push the botton" << "\n";
//   cout << BEEP << "hurry.. push the botton" << "\n";
//   cout << BEEP << "hurry.. push the botton" << "\n";
//   cout << BEEP << "hurry.. push the botton" << "\n";
  
}


/////////////////////////////////////////////////////////////////////////////////////




// Integral function (why?? why not just integrate??):

double IntegralAround(TH1D* h, double x0, double dx) {
  int i0 = h->FindBin(x0 - dx);
  int i1 = h->FindBin(x0 + dx);
  
  double integral = 0;
  for (int i=i0; i<=i1; i++) {
    integral+= h->GetBinContent(i);
  }
  return integral;
}





// Drawing Buttons on the canvas:

void draw_buttons() {
  /// count number of actions first

  C->cd();
  double nbut = 0;
  for (int i=0; user_action[i] != NULL; i++) nbut+= 1.;


  for (int i=0; user_action[i] != NULL; i++) {
#ifdef __CINT__
    char callhandle[80];
    sprintf (callhandle, "handle_user_action(%d)", i);
    //    cout << callhandle << endl;
    gBut[i] = new TButton(user_action[i], callhandle, 
			  i/nbut+0.005, 0.01, (i+1.)/nbut-0.005, 0.07);

    //gBut[i]->SetBorderMode(0);
    if (i == 0)  gBut[0]->SetFillColor(1051);           // (kGreen-3)
    else if (i == 1) gBut[1]->SetFillColor(1055);       // (kRed-4)
    else if (i == 2) gBut[2]->SetFillColor(1056);       // (kAzure+1)
    else if (i == 3) gBut[3]->SetFillColor(1056);       // (kAzure+1)
    else if (i == 4) gBut[4]->SetFillColor(1057);       // (kAzure+8)
    else if (i == 5) gBut[5]->SetFillColor(1057);       // (kAzure+8)
    else if (i == 6) gBut[6]->SetFillColor(1052);       // (kCyan-9)
    else if (i == 7) gBut[7]->SetFillColor(1052);       // (kCyan-9)
    else if (i == 8) gBut[8]->SetFillColor(1053);       // (kGreen-9)
    else if (i == 9) gBut[9]->SetFillColor(1054);       // (kRed-10)
    gBut[i]->SetTextSize(0.4);
    gBut[i]->Draw(); 



#else
    JButton* but = new JButton(i, i/nbut+0.005, 0.01, (i+1.)/nbut-0.005, 0.07);
    but->SetActive(i);     /// all buttons but check-in active
    but->Draw();
    gBut[i] = but;
#endif
  }

  C->Modified();
  C->Update();
}





// Handling button-pressing:

void handle_user_action(int choice) {
  switch (choice) {

  case act_ok: 
    if (ready_for_checkin) return;
    for (int i=0; i<cdb_max; i++) {
      hcaldb[1][i]->SetBinContent(gIndex+1, new_par[i]);
    }

    // no break here, go to next channel  

  case act_refuse:
    if (ready_for_checkin) return;
    gIndex++;
    if (paddle != 999) gIndex = paddle;
    paddle = 999;
    if (gIndex < newfit) {
      st_fit_next_channel();
    }
    else {
      ready_for_checkin = true;
      show_final_plots();
#ifndef __CINT__
      ((JButton*) gBut[act_ok]     )->SetActive(false);
      ((JButton*) gBut[act_refuse] )->SetActive(false);
      ((JButton*) gBut[act_par3up_big] )->SetActive(false);
      ((JButton*) gBut[act_par3down_big] )->SetActive(false);
      ((JButton*) gBut[act_par3up_small] )->SetActive(false);
      ((JButton*) gBut[act_par3down_small] )->SetActive(false);
      ((JButton*) gBut[act_par3up_tiny] )->SetActive(false);
      ((JButton*) gBut[act_par3down_tiny] )->SetActive(false);
      ((JButton*) gBut[act_uselast] )->SetActive(false);
#endif
      C->Modified();
      C->Update();
      C->Print("gif/comparison.gif");
    }
    break;

  case act_par3up_big:
    if (ready_for_checkin) return;
    increment = 0.005;
    st_fit_by_pointer(h_copy);
    break;

  case act_par3down_big:
    if (ready_for_checkin) return;
    increment = -0.005;
    st_fit_by_pointer(h_copy);
    break;

  case act_par3up_small:
    if (ready_for_checkin) return;
    increment = 0.001;
    st_fit_by_pointer(h_copy);
    break;  
    
  case act_par3down_small:
    if (ready_for_checkin) return;
    increment = -0.001;
    st_fit_by_pointer(h_copy);
    break;

 case act_par3up_tiny:
    if (ready_for_checkin) return;
    increment = 0.0002;
    st_fit_by_pointer(h_copy);
    break;

 case act_par3down_tiny:
    if (ready_for_checkin) return;
    increment = -0.0002;
    st_fit_by_pointer(h_copy);
    break;

  case act_uselast:
    if (ready_for_checkin) return;
    read_last_fitting();
    fitting_flag = 1;
    st_fit_by_pointer(h_copy);
    break;

  case act_quit:
    cout << "good bye" << endl;
    exit_program = true;
    exit(0);
  }
}





// Plot the final summary plots:


void show_final_plots() {
  C1->Clear();
  //  C1->SetFillColor(kGreen-10);
  C1->Divide(2,3);
  
  gStyle->SetOptStat(0);
  mem->UseCurrentStyle();
  hcaldb[1][cdb_deltaT]->Scale(-1.);
  hcaldb[1][cdb_deltaT]->Add(hcaldb[0][cdb_deltaT]);

  for (int i=0; i<cdb_max; i++) {
    
    if (i==0){
      char outfilename[80];
      sprintf (outfilename,"STN_CALIB_%s_old.dat",caldb_table[i]);
      ofstream of(outfilename);
      
      for (int j=0; j<24; j++) {
	of << hcaldb[0][i]->GetBinContent(j+1) << endl;
      }
    }

    else { 
      char outfilename[80];
      sprintf (outfilename,"STN_CALIB_%s.dat",caldb_table[i]);
      ofstream of(outfilename);
      
      for (int j=0; j<24; j++) {
	of << hcaldb[1][i]->GetBinContent(j+1) << endl;
      }
    }
    
    C1->cd(i+1);
    hcaldb[1][i]->SetFillColor(5);
    hcaldb[1][i]->Draw();
    hcaldb[0][i]->SetLineColor(1058);   // (kViolet+3)
    hcaldb[0][i]->Draw("same");
  }
  
  C1->cd(cdb_max+1);
  hchisq->SetFillColor(9);
  hchisq->Draw();
  C1->Modified();
  C1->Update();
  
}






// Final bits:

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
	
  
  new_veff_fit();

  theApp->Run();
  
}
#endif
