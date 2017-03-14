//$Id: tagraw.cxx,v 1.9 2008/09/25 19:23:05 fklein Exp $
///////////////////////////////////////////////////////////////////////////////
// tagraw: RF and tagger calibration (based on raw data)
// 
// Notes: 
//   - Tcounters,Tbins,Ecounters are counted from 1...61 or 1...121 or 1...384 
//
//   - RF has to be checked versus SC and ST and appropriately adjusted:
//     after RFtdc is chosen and slope is checked (using tagraw), the RF timing wrt. ST 
//     and SC times can still be off (change RF_offset, tag2tof, st2tof, TAGT_ci)
//
//   - some (private) changes in the tag/ and st/ libraries allow for quick tests of
//     those general time offsets (if tagraw.cxx and tag/, st/ libraries are compiled
//     with this flag)
//
//   - it looks like RF (and Tagger) timing wrt. CLAS (ST,SC) cannot be solved without
//     reconstructed data since ST timing has to account for the propagation through
//     the long scintillator stripes (at least some pattern recognition to be included
//     in a later version of tagraw.cxx)
//
//   - histograms produced by tagraw can be analyzed using root macros rf_check.C, 
//     tagslope.C, ci_adjust.C, edt_fit.C 
//
/////////////////////////////////////////////////////////////////////////////////
using namespace std;

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <stdlib.h>
#include <errno.h>
#include <math.h>
#include <signal.h>
#include <stdio.h>

extern "C" {
#include "ntypes.h"
#include "bostypes.h"
#include "clas_cern.h"
#include "kinematics.h"
#include "map_manager.h"
#include "sc.h"
#include "pid.h"
#include "utility.h"
#ifdef STTCL
#include "../st/sttcl.h"
#endif
}

#include "ROOT.h"
#include "JBosBank.h"
#include "JTagrFile.h"
//#include "JCalibration.h"
#include "tag_cpp_wrapper.h"
#include "ETmatch.h"

int use_rf = 0;
int TAGTrefid = 48;  //Tcounter# (1-61) for check of time spectrum

bool do_skipCL01 = false;   // don't do any RF corrections
// for stripcharts
bool do_stripcharts = false;
const int NTCNT_CHARTS = 10;  //number of T-counters for which stripcharts are made
int Tcnt_chart[NTCNT_CHARTS]={10, 40, 41, 42, 43, 44, 0,0,0,0}; //Tcounter# (1-61)
const int maxepicbins  = 500;  //max number of epics events (for stripchart)
const int maxeventbins = 50000; // max bins of stripchart
const int maxevents_file = 800000; // assume about 800k events per file

const int EC_MAX  = 1296;
const int CC_MAX  =  216;
const double c_light = 29.9792458;
const int TCNT_MAX =  61;
const int ECNT_MAX = 384;
const int ST_MAX  =   24;
int SC_MAX  =  342;
const int TAG_MAX   =  121;
const double RFcycle = 2.004008;
// (+ 0.556085  0.569766 -2.004)
const int RFprescaled = 80;  //prescaled by 80 RFcycles
int RFdiff[2] = {1642,1642}; //for pipeline TDCs RF3,RF4:
// 1641.9+/-1.1 tdc channel (80 RFcycles between RF tdc entries) 

const double PROTM  = 0.93827;
const double NEUTM  = 0.93957;
const double PIONM  = 0.13957;
const double KAONM  = 0.49368;
const double KZERM  = 0.49767;
const double ELECM  = 0.00051;
const double LAMBM  = 1.11568;
const double DEUTM  = 1.8756; 

const int negat_id   = 3;
const int pionm_id   = 8;
const int proton_id  = 14;
const int kaonp_id   = 11;
const int ambig_id   = 10;
const int pionp_id   = 9;
const int posit_id   = 2;
const int pionlam_id = 7;
const int protlam_id = 13;
const double DEG = 180. / M_PI;

inline double SQR(double x) { return x*x; }

extern "C" {
  void initCL01(int runno);
  void make_CL01_bank();
  void make_cl01_bank_using(int userf);
  float getCL01time(int id, int tdc);
  float rf_correction_center(float time, float rf);
  int get_sc_version_flag(int runno);
  void st_brun_(int* runno);
  void st_bevt_();
  void st_evnt_(int* stlevel);
  void st_set_def_();
}
void stpp_init_geometry();
void fill_epics_charts(int);
void analyze_event(int);
void final_plots();

void PrintUsage(const char *processName) {
  cerr<< "\nUsage: "<<processName<<" [-opt1 ...] <data_file1> <data_file2> ... <data_fileN>" <<endl;
  cerr<< "Options:\t-n[#]   \tProcess only # number of events\n";
  cerr<< "        \t-o[file]\tOutput file <file> (default: \"tagraw.<runno>.root\")\n";
  cerr<< "        \t-X      \tX-windows: do not open histogram canvas\n";
  cerr<< "        \t-W      \tWrite ascii file with TAG data\n";
  cerr<< "        \t-S      \tStripcharts: make event dependent histograms\n";
  cerr<< "        \t-R      \tRead local files with offsets (tagt_dt.dat,st_p2pi.dat,sc_p2p.dat)\n";
  cerr<< "        \t-C      \tdrop CL01: no RF correction\n";
#ifdef STTCL
  cerr<< "        \t-aRF[#] \tAdd additional constant to RF_OFFSETS.rf_offset.value\n";
  cerr<< "        \t-aS2T[#]\tAdd additional constant to STN_CALIB.deltaT.st2tof\n";
  cerr<< "        \t-aT2T[#]\tAdd additional constant to TAG_CALIB.tag2tof.value\n";
  cerr<< "        \t-aT20[#]\tAdd additional constant to TAG_CALIB.tag_t.dt for T20\n";
  cerr<< "        \t-aTCI[#]\tAdd additional (common) constant to TAG_CALIB.tag_t.ci\n";
#endif
  cerr<< "        \t-h      \tPrint this message\n";
  cerr<< endl;
  exit(0);
}

TCanvas*C = NULL;
TFile* froot = NULL;
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,16,0)
TDirectoryFile *dir_evts=NULL, *dir_lr=NULL;
#else
TDirectory *dir_evts=NULL, *dir_lr=NULL;
#endif
//extern JCalibration* gCalib;

bool fileloop   = true;
void handle_break(int signal) {
  cout << "\nSignal terminates loop" << endl;
  fileloop = false;
}

JTagrFile* gTagrFile;

// ----------- JBosFile ---------------------------

class JBosFile {
  bool isGood;
public:
  JBosFile (string fileName);
  ~JBosFile ();
  bool good()         { return isGood;    }
  void setEof()       { isGood = false;   }
};

// constructor opens bos file  or  sets isGood false if not successful
JBosFile::JBosFile (string fileName) : isGood(true) {
  
  string openCommand = 
    string ("OPEN BOSINPUT UNIT=1 FILE=\"" 
	    + fileName + "\" READ" ); 

  if (! fparm_c((char*)openCommand.c_str() ) ) {
    cerr << "Unable to open file " << fileName << ": " << strerror(errno) << endl;
    isGood = false;
  }
} 

// destructor closes bos file
JBosFile::~JBosFile () {
  fparm_c("CLOSE BOSINPUT UNIT=1");
} 


//------------- JBosEvent -------------------------
class JBosEvent {
  bool isGood;
public:
  JBosEvent ();
  ~JBosEvent ();
  bool good () { return isGood; }
};

// constructor reads event  or  sets isGood false if not successful
JBosEvent::JBosEvent () : isGood(true) {
  if (! getBOS(&bcs_,1,"E") ) {
    cerr << "\nError reading next event (might be end of file)" << endl;
    isGood = false;
  }
}

// destructor clears bos banks
JBosEvent::~JBosEvent () {
  dropAllBanks(&bcs_,"E");
  cleanBanks(&bcs_);
}

TRint*   theApp;  // interactive version of ROOT processor

// define some histograms
TH1F* hconst_t1r, *hconst_t1l, *hconst_t0r, *hconst_t0l, *hconst_tci, *hconst_edt, *hconst_rf, *hconst_run;
TH1F* hlcount, *hrcount, *hreftdc, *hecount;
TH2F* heslope, *hecntr, *hecnttagr;
TH1F* hrf, *hrfavail, *hright, *hleft, *hrawcl01[4], *htrig, *hrfraw[4];
TH2F* hrffun[4], *hrfcl01[4], *hrftag, *hrftag2, *hrftpho, *hrftpho2, *hrftcnt;
TH2F* htagtpho, *httag, *htpho;
TH2F* hlr[TCNT_MAX], *hlraw, *hrraw, *heraw;
TH1F* hdiffall, *hdifft[TCNT_MAX];
TH1F *hrfdiff[2], *hrfmod[2], *hrfdif23, *hrfrawdif23;
TH1F *hrfrawdiff[2], *hrfrawmod[2];
TH2F *hbeamx, *hbeamy, *htesc, *hevts_rftag[NTCNT_CHARTS];
TH2F *hst1_time, *hst1_tpho[6];  // *hst1_ttag[6]; 

void book_histo(int runno, char*outfile) {

  if( outfile ) {
    froot = new TFile(outfile, "recreate");
  }
  else {
    ostringstream rootfilename;
    rootfilename << "tagraw." << runno << ".root";
    froot = new TFile(rootfilename.str().c_str(), "recreate");
  }

  //with version 5.16 TDirectory became pure interface: use new class TDirectoyFile
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,16,0)
  if(do_stripcharts) dir_evts = new TDirectoryFile("evts", "evts");
  dir_lr = new TDirectoryFile("lr",   "lr");
#else
  if(do_stripcharts) dir_evts = new TDirectory("evts", "evts");
  dir_lr = new TDirectory("lr",   "lr");
#endif

  char hstname[80], hsttitle[80];
  //  hreftdc = new TH1F ("reftdc", "reftdc", 100, 4950., 5050.);
  if (TAGTrefid > 0) {
    //  hrf = new TH1F ("rf", "rf", 1000, 720., 820.);
    hrf = new TH1F ("rf", "rf", 200, -1.05, 1.05);
    sprintf(hsttitle,"time T%02d left",TAGTrefid);
    hleft  = new TH1F ("left", hsttitle, 400, 0, 60000);
    sprintf(hsttitle,"time T%02d right",TAGTrefid);
    hright = new TH1F ("right", hsttitle, 400, 0, 60000);
  }
  htrig  = new TH1F ("trig", "trigbits", 32, 0.5, 32.5);
  httag   = new TH2F ("ttag", "ttag", TCNT_MAX, 0.5, TCNT_MAX+0.5, 100, -5., 5.);
  htpho   = new TH2F ("tpho", "tpho", TCNT_MAX, 0.5, TCNT_MAX+0.5, 100, -5., 5.);
  htagtpho  = new TH2F ("tagtpho", "Ttime - Tpho (Tbins)", TAG_MAX, 0.5, TAG_MAX+0.5, 100, -1.05, 1.05);
  hecount = new TH1F("ecount", "ecount", ECNT_MAX, 0.5,  ECNT_MAX+0.5);
  hecnttagr= new TH2F("ecnttagr", "Etime - Ttime (TAGR entries)",   ECNT_MAX, 0.5,  ECNT_MAX+0.5, 100, -40., 40.);
  hecntr  = new TH2F("ecntr", "Etime - Ttime (all entries)",   ECNT_MAX, 0.5,  ECNT_MAX+0.5, 100, -40., 40.);
  heraw  = new TH2F ("eraw","Ecntr TDC",  ECNT_MAX, 0.5, ECNT_MAX+0.5, 150, 700., 1600.);
  heslope = new TH2F("eslope", "eslope", 150, 700.,  1600., 100, -10., 10.);

  hrfavail = new TH1F ("rfavail", "rfavail", 5, -0.5, 4.5);
  hrfdif23 = new TH1F ("rfdif23", "diff RFused - RFnotused", 200, -1.05, 1.05);
  hrfrawdif23 = new TH1F ("rfrawdif23", "diff raw RFused - RFnotused", 1000, -3400., 3400.);
  hrftag  = new TH2F ("rftag", "RFused - Ttag (Tbins)",     TAG_MAX, 0.5, TAG_MAX+0.5, 100, -1.05, 1.05);
  hrftag2 = new TH2F ("rftag2", "RFnotused - Ttag (Tbins)", TAG_MAX, 0.5, TAG_MAX+0.5, 100, -1.05, 1.05);
  hrftpho  = new TH2F ("rftpho", "RFused - Tpho (Tbins)",     TAG_MAX, 0.5, TAG_MAX+0.5, 50, -0.5, 0.5);
  hrftpho2 = new TH2F ("rftpho2", "RFnotused - Tpho (Tbins)", TAG_MAX, 0.5, TAG_MAX+0.5, 50, -0.5, 0.5);


  for (int i = 0; i< 2; i++) {
    sprintf (hstname, "rfdiff%d", i);
    hrfdiff[i]  = new TH1F (hstname, hstname, 500, 0., 1200.);
    sprintf (hstname, "rfrawdiff%d", i);
    hrfrawdiff[i]  = new TH1F (hstname, hstname, 100, 1600., 1700.);
    sprintf (hstname, "rfrawmod%d", i);
    hrfrawmod[i]  = new TH1F (hstname, hstname, 855, -1., 1709.);
    sprintf (hstname, "rfmod%d", i);
    hrfmod[i]  = new TH1F (hstname, hstname, 100, -1.05, 1.05);
  }
  // st time (only one smaller paddle per sector)
  hst1_time = new TH2F("st1_time","st1_time", 24,0.5,24.5, 100,0.,40.);
  for (int i=0; i<6; i++) {
    //    sprintf(hstname,  "st1_ttag%d",i);
    //    sprintf(hsttitle, "st1 - tag time Pd.1 in S.%d",i+1);
    //    hst1_ttag[i] = new TH2F(hstname,hsttitle, TCNT_MAX,0.5,TCNT_MAX+0.5, 300, -30.,30.);
    sprintf(hstname,  "st1_tpho%d",i);
    sprintf(hsttitle, "st1 - tpho time Pd.1 in S.%d",i+1);
    hst1_tpho[i] = new TH2F(hstname,hsttitle, TCNT_MAX,0.5,TCNT_MAX+0.5, 300, -30.,30.);
  }

  // subdirectory dir_lr for left-right alignment
  dir_lr->cd();
  for (int i =0; i<TCNT_MAX; i++) {
    sprintf (hstname, "lr%03d", i+1);
    sprintf (hsttitle, "left-right vs. left+right, T counter %d", i+1);
    double yrange= (i<TCNT_MAX-1) ? 5. : 20.;

    hlr[i] = new TH2F(hstname, hsttitle, 100, 25000., 70000., 100, -yrange, yrange);
    sprintf (hstname, "difft%03d", i+1);
    hdifft[i]  = new TH1F (hstname, hstname, 10000, -1000., 1000.);
  }
  hrftcnt  = new TH2F ("rftcnt", "rftcnt", TCNT_MAX, 0.5, TCNT_MAX+0.5, 100, -1.05, 1.05);
  hlraw  = new TH2F ("lraw", "Tcntr TDC left" , TCNT_MAX, 0.5, TCNT_MAX+0.5, 200, 10000, 40000);
  hrraw  = new TH2F ("rraw", "Tcntr TDC right", TCNT_MAX, 0.5, TCNT_MAX+0.5, 200, 10000, 40000);

  // back to main directory
  froot->cd();
  for (int i = 0; i< 4; i++) {
    double max = i < 2? 2200. : 12500.;
    int ibins = i<2 ? 400 : 500;
    sprintf (hstname, "rfraw%d", i);
    hrfraw[i] = new TH1F (hstname, hstname, ibins, 0., max);

    max = i < 2? 1900. : 13000.;
    sprintf (hstname, "rffun%d", i);
    hrffun[i] = new TH2F (hstname, hstname, 300, 200., max, 100, -1.05, 1.05);

    double min = i<2? 200. : 9000.;
    max = i < 2? 3000. : 12500.;
     sprintf (hstname, "rfcl01%d", i);
    hrfcl01[i] = new TH2F (hstname, hstname, 200, min, max, 200, -1.05, 1.05);


    sprintf (hstname, "rawcl01%d", i);
    min = i < 2? 0. : 0.;
    max = i < 2? 300. : 1200.;
    hrawcl01[i]  = new TH1F (hstname, hstname, 300, min, max);

  }

  hlcount = new TH1F ("lcount", "lcount", TCNT_MAX, 0.5, TCNT_MAX+0.5);
  hrcount = new TH1F ("rcount", "rcount", TCNT_MAX, 0.5, TCNT_MAX+0.5);


  hconst_t1l = new TH1F("const_t1l", "const_t1l",
                        TCNT_MAX, 0.5, TCNT_MAX+0.5);
  hconst_t1r = new TH1F("const_t1r", "const_t1r",
                        TCNT_MAX, 0.5, TCNT_MAX+0.5);
  hconst_t0l = new TH1F("const_t0l", "const_t0l",
                        TCNT_MAX, 0.5, TCNT_MAX+0.5);
  hconst_t0r = new TH1F("const_t0r", "const_t0r",
                        TCNT_MAX, 0.5, TCNT_MAX+0.5);
  hconst_tci = new TH1F("const_tci", "const_tci",
                        TAG_MAX, 0.5, TAG_MAX+0.5);
  hconst_edt = new TH1F("const_edt", "const_edt",
                        ECNT_MAX+1, -0.5, ECNT_MAX+0.5);  // 1.bin=slope
  hconst_rf  = new TH1F("const_rf", "const_rf",32,-0.5,31.5);
  hconst_run = new TH1F("const_run","const_run",60,-0.5,59.5);

}

void book_stripcharts(int first_event=1, int nevents=0) {
  // subdirectory dir_evts for stripcharts (quantities vs. event no.)
  if (!do_stripcharts) return;
  dir_evts->cd();

  int last_event = nevents? first_event+nevents : first_event+maxevents_file;

  int diffevts = last_event-first_event+1;
  while (diffevts > maxeventbins) diffevts /=4;

  char hstname[80], hsttitle[80];
  int ncnts[61];
  memset(ncnts, 0, 61*sizeof(int));

  for (int i =0; i<NTCNT_CHARTS; i++) {

    Tcnt_chart[i]--;  //change to C++ index
    if (Tcnt_chart[i] >= 0 && Tcnt_chart[i] < TCNT_MAX) {
      if ((++ncnts[Tcnt_chart[i]]) > 1) {
	Tcnt_chart[i]=-1;
	continue;
      }
      sprintf(hstname, "rftag_T%02d",Tcnt_chart[i]+1);
      sprintf(hsttitle, "RFused-Ttag for T%02d",Tcnt_chart[i]+1);
      hevts_rftag[i] = new TH2F(hstname, hsttitle, 
			    diffevts, first_event,last_event, 50, -0.5,0.5);
    }
  }
  hbeamx = new TH2F("beamx", "Beam X-pos",
		      maxepicbins,first_event,last_event, 50, -0.5, 0.5);
  hbeamy = new TH2F("beamy", "Beam Y-pos",
		    maxepicbins,first_event,last_event, 50, -0.5, 0.5);
  htesc  = new TH2F("tesc", "E-counter scalers",
		    maxepicbins,first_event,last_event, ECNT_MAX, 0.5,ECNT_MAX+0.5);
  froot->cd();
}

double tag_corr[TAG_MAX];
double sc_corr[SC_NPADDLES_TOTAL];
double st_corr[ST_MAX];

// read local files with correction offsets 
//   ascii file format: 
//     <channel> <value>
//     <channel> <value> ...
void read_corr(bool readcorr) {
  memset (tag_corr, 0, sizeof(tag_corr));
  if (readcorr) {
    ifstream ftag("tagt_dt.dat");
    while (ftag.good()) {
      int index;
      double value;
      ftag >> index >> value;
      if (0 < index && index <= TAG_MAX) tag_corr[index-1] = value;
    }
  }

  memset (sc_corr, 0, sizeof(sc_corr));
  if (readcorr) {
    ifstream fsc("sc_p2p.dat");
    while (fsc.good()) {
      int index;
      double value;
      fsc >> index >> value;
      if (0 < index && index <= SC_MAX) sc_corr[index-1] = value;
    }
  }

  memset (st_corr, 0, sizeof(st_corr));
  if (readcorr) {
    ifstream fst("st_p2p.dat");
    while (fst.good()) {
      int index;
      double value;
      fst >> index >> value;
      if (0 < index && index <= ST_MAX) st_corr[index-1] = value;
    }
  }
}

float tag_t_dt_left[TCNT_MAX];
float tag_t_dt_right[TCNT_MAX];
float tag_t_slope_left[TCNT_MAX];
float tag_t_slope_right[TCNT_MAX];
float tag_t_ci[TAG_MAX];
float tag2tof_value[1];
float tag_e_dt[ECNT_MAX];
float tag_e_slope[1];
float call_T0_value[10];  // offset should be multiple of RFcycle (e.g.750)
float call_T1_value[10];
float call_T2_value[10];
float RF_offset;

void get_calibration_constants(int runno) {
  char map_tag[255];
  char map_call[255];
  char map_rf[255];
  int firsttime;
  sprintf(map_tag, "%s/Maps/TAG_CALIB.map", getenv("CLAS_PARMS"));
  sprintf(map_call,"%s/Maps/CALL_CALIB.map",getenv("CLAS_PARMS"));
  sprintf(map_rf,  "%s/Maps/RF_OFFSETS.map",getenv("CLAS_PARMS"));

  cout<< "get calibration constants for run " << runno <<endl;

  if (map_get_float(map_tag, "tag_t", "dt_left", TCNT_MAX, tag_t_dt_left, runno, &firsttime)<0) throw -11;
  cout<< "got "<< TCNT_MAX <<" entries for TAG_CALIB.tag_t.dt_left  \t (maprun=" << firsttime << ")" <<endl;

  if (map_get_float(map_tag, "tag_t", "dt_right", TCNT_MAX, tag_t_dt_right, runno, &firsttime)<0) throw -12;
  cout<< "got "<< TCNT_MAX <<" entries for TAG_CALIB.tag_t.dt_right \t (maprun=" << firsttime << ")" <<endl;

  if (map_get_float(map_tag, "tag_t", "slope_left", TCNT_MAX, tag_t_slope_left, runno, &firsttime)<0) throw -13;
  cout<< "got "<< TCNT_MAX <<" entries for TAG_CALIB.tag_t.slope_left\t (maprun=" << firsttime << ")" <<endl;

  if (map_get_float(map_tag, "tag_t", "slope_right", TCNT_MAX, tag_t_slope_right, runno, &firsttime)<0) throw -14;
  cout<< "got "<< TCNT_MAX <<" entries for TAG_CALIB.tag_t.slope_right\t (maprun=" << firsttime << ")" <<endl;

  if (map_get_float(map_tag, "tag_t", "ci", TAG_MAX, tag_t_ci, runno, &firsttime)<0) throw -15;
  cout<< "got "<< TAG_MAX <<" entries for TAG_CALIB.tag_t.ci       \t (maprun=" << firsttime << ")" <<endl;

  if (map_get_float(map_tag, "tag2tof", "value", 1, tag2tof_value, runno, &firsttime)<0) throw -16;
  cout<< "got tag2tof="<< tag2tof_value[0] <<" from TAG_CALIB.tag2tof.value (maprun=" << firsttime << ")" <<endl;

  if (map_get_float(map_tag, "tag_e", "dt", ECNT_MAX, tag_e_dt, runno, &firsttime)<0) throw -17;
  cout<< "got "<< ECNT_MAX <<" entries for TAG_CALIB.tag_e.dt  \t (maprun=" << firsttime << ")" <<endl;

  if (map_get_float(map_tag, "tag_e", "slope", 1, tag_e_slope, runno, &firsttime)<0) throw -18;
  cout<< "got tage_slope="<< tag_e_slope[0] <<" from TAG_CALIB.tag_e.slope\t (maprun=" << firsttime << ")" <<endl;

  if (map_get_float(map_call, "T0", "value", 10, call_T0_value, runno, &firsttime)<0) throw -20;
  cout<< "got 10 entries for CALL_CALIB.T0.value  \t (maprun=" << firsttime << ")" <<endl;
  cout.width(8);
  cout<< "    for RF3: T0="<< call_T0_value[8] <<"; for RF4: T0="<< call_T0_value[9] <<endl;
  if (map_get_float(map_call, "T1", "value", 10, call_T1_value, runno, &firsttime)<0) throw -21;
  cout<< "got 10 entries for CALL_CALIB.T1.value  \t (maprun=" << firsttime << ")" <<endl;
  cout.width(10);
  cout<< "    for RF3: T1="<< call_T1_value[8] <<"; for RF4: T1="<< call_T1_value[9] <<endl;

  if (map_get_float(map_call, "T2", "value", 10, call_T2_value, runno, &firsttime)<0) throw -22;
  cout<< "got 10 entries for CALL_CALIB.T2.value  \t (maprun=" << firsttime << ")" <<endl;
  cout.width(10);
  cout<< "    for RF3: T2="<< call_T2_value[8] <<"; for RF4: T2="<< call_T2_value[9] <<endl;

  if (use_rf==0) {
    if (map_get_int(map_rf, "status", "value", 1, &use_rf, runno, &firsttime)<0) throw -23;
    cout<< "**** got use_rf="<< use_rf <<" from RF_OFFSETS.status.value\t (maprun=" << firsttime << ")" <<endl;
  }
  else 
    cout<< "**** use_rf="<< use_rf <<" (chosen by user)" <<endl;

  if (map_get_float(map_rf, "offset", "value", 1, &RF_offset, runno, &firsttime)<0) throw -24;
  cout<< "got RF_offset="<< RF_offset <<" from RF_OFFSETS.offset.value  (maprun=" << firsttime << ")" <<endl;

  if (call_T1_value[8] != 0.0)
    RFdiff[0]=(int)(0.5-RFprescaled*RFcycle/call_T1_value[8]);
  if (call_T1_value[9] != 0.0)
    RFdiff[1]=(int)(0.5-RFprescaled*RFcycle/call_T1_value[9]);
  cout<< "Expected TDC channel difference for RF3: "<< RFdiff[0] <<", for RF4: "<< RFdiff[1] <<endl;

#ifdef STTCL
  // apply offsets given in sttcl_real_ (command line)
  cout << "sttcl: t2t "<<sttcl_real_.tag2tof_add_offset 
       << " ;  s2t "<<sttcl_real_.st2tof_add_offset 
       << " ;  tci "<<sttcl_real_.tci_add_offset 
       << " ;  t20 "<<sttcl_real_.tagt20_add_offset 
       << " ;  rf "<<sttcl_real_.rf_add_offset << endl;
 
  if( sttcl_real_.tag2tof_add_offset !=0.0 ) {
    tag2tof_value[0] += sttcl_real_.tag2tof_add_offset;
    cout << "Applied additional offset to tag2tof: " << sttcl_real_.tag2tof_add_offset << endl;
  }
  if( sttcl_real_.tagt20_add_offset !=0.0 ) {
    tag_t_dt_left[19]  += sttcl_real_.tagt20_add_offset;
    tag_t_dt_right[19] += sttcl_real_.tagt20_add_offset;
    cout << "Applied additional offset to tagt.dt for T20: " << sttcl_real_.tagt20_add_offset << endl;
  }
  if( sttcl_real_.rf_add_offset !=0.0 ) {
    RF_offset += sttcl_real_.rf_add_offset;
    cout << "Applied additional offset to RF_offset: " << sttcl_real_.rf_add_offset << endl;
  }
  if( sttcl_real_.tci_add_offset < -999.) {
    for (int i=0; i<TAG_MAX; i++)
      tag_t_ci[i] = 0.0;
    cout << "Set all TAG_CALIB.tagt.ci to zero" << endl;
  }
  else if( sttcl_real_.tci_add_offset !=0.0 ) {
    for (int i=0; i<TAG_MAX; i++)
      tag_t_ci[i] += sttcl_real_.tci_add_offset;
    cout << "Applied additional common offset to tagt.ci: " << sttcl_real_.tci_add_offset << endl;
  }
#endif
  if (get_sc_version_flag(runno)==1) SC_MAX = 288;
}

void const2histo() {
  for (int i=0; i<TCNT_MAX; i++) {
    hconst_t0l->SetBinContent(i+1, tag_t_dt_left[i]);
    hconst_t0r->SetBinContent(i+1, tag_t_dt_right[i]);
    hconst_t1l->SetBinContent(i+1, tag_t_slope_left[i]);
    hconst_t1r->SetBinContent(i+1, tag_t_slope_right[i]);
  }
  for (int i=0; i<TAG_MAX; i++) {
    hconst_tci->SetBinContent(i+1, tag_t_ci[i]);
  }
  for (int i=0; i<ECNT_MAX; i++) {
    hconst_edt->SetBinContent(i+1, tag_e_dt[i]);
  }
  hconst_edt->SetBinContent(ECNT_MAX+1, tag_e_slope[0]);

  hconst_rf->SetBinContent(1,use_rf);
  hconst_rf->SetBinContent(2,RF_offset);
  for (int i=0; i<10; i++) {
    hconst_rf->SetBinContent(i+3,  call_T0_value[i]);
    hconst_rf->SetBinContent(i+13, call_T1_value[i]);
    hconst_rf->SetBinContent(i+23, call_T2_value[i]);
  }
}

bool isPion(int id) {
  switch (id) {
  case pionm_id: return true;
  case pionp_id: return true;
  case pionlam_id: return true;
  case negat_id: return true;
  }
  return false;
}

int ecIndex(int sector, int id) {
  int layer = id >> 8;
  if (layer <= 0 || layer > 6) {
    cout << "Invalid layer " << layer << endl;
    throw -1;
  }
  int index = id & 0xFF;
  if (index <=0 || index > 36) {
    cout << "invalid index " << index << endl;
    throw -2;
  }
  return (sector - 1) * 6 * 36 + (layer - 1) * 36 + (index - 1);
}

int scIndex(int sector, int id, int side) {
  if (side < 0) {
    int highbyte = id >> 8;
    side = highbyte;
  }

  if (side < 0 || side > 1) {
    cout << "Invalid side " << side << endl;
    throw -1;
  }
 
  int index = id & 0xFF;
  if (index <=0 || index > 48) {
    cout << "invalid index " << index << endl;
    throw -2;
  }
  return side * SC_MAX + (sector- 1) * 48 + (index - 1);
}

int ccIndex(int sector, int id) {
  return (sector - 1) *36 + (id-1);
}

/// find run number: number follows last underscore in string 
bool extractRunNumber(int& runno, int& ifile, string a) {
  unsigned int len = 0;
  unsigned int i = a.rfind('_');
  if (i == string::npos) return false;
  i++;
  while ( i+len < a.length() && isdigit(a[i+len]) ) {
    len++;
  }
  if (!len) return false;
  runno = atoi(a.substr(i,len).c_str());

  // now look for Axx number
  i= i+len;
  len = 0;
  while ( i < a.length() && ! isdigit(a[i]) )  i++;
  if (i == a.length()) return false;

  while ( i+len < a.length() && isdigit(a[i+len]) ) {
    len++;
  }
  if (!len) return false;
  ifile = atoi(a.substr(i,len).c_str());

  return true;
}
int no_tgtlr=0;
int no_tage =0;
int no_tagr =0;
int no_cl01 =0;

int main (int argc, char* argv[]) {

  bool do_xdisplay  = true;
  bool do_readcorr = false;
  bool do_writetagr= false;
  char *cRootOutFile = NULL;
  vector<string> bosfile;
  int eventMax = 0;
  float t2t_add_offset =0.;
  float s2t_add_offset =0.;
  float t20_add_offset =0.;
  float tci_add_offset =0.;
  float rf_add_offset  =0.;

  // check command line options
  int iarg =1;
  while (iarg<argc) {
    char *argptr = argv[iarg];
    if (*(argptr++) == '-') {
      switch (*(argptr++)) {
      case 'n':
        if (*argptr)            eventMax = atoi(argptr);
        else if (++iarg < argc) eventMax = atoi(argv[iarg]);
        else {
          cerr << "Option -n given without max event number\n" << endl;
          PrintUsage(argv[0]);
        }
        break;
      case 'o':
        if (*argptr)            cRootOutFile = argptr;
        else if (++iarg < argc) cRootOutFile = argv[iarg];
        else {
          cerr << "Option -o given without ROOT file name\n" << endl;
          PrintUsage(argv[0]);
        }
        break;
      case 'W':
        do_writetagr = true;
        break;
      case 'X':
        do_xdisplay = false;
        break;
      case 'S':
        do_stripcharts = true;
        break;
      case 'R':
        do_readcorr = true;
        break;
      case 'C':
        do_skipCL01 = true;
        break;
#ifdef STTCL
      case 'a':
	if (*argptr) {
	  if( !strncmp(argptr,"RF",2) ) {
	    argptr+=2;
	    if (*argptr)          rf_add_offset = atof(argptr);
	    else if (++iarg<argc) rf_add_offset = atof(argv[iarg]);
	    else {
	      cerr << "Option -aRF given without value\n" << endl;
	      PrintUsage(argv[0]);
	    }
	  }
	  else if( !strncmp(argptr,"T2T",3) ) {
	    argptr+=3;
	    if (*argptr)          t2t_add_offset = atof(argptr);
	    else if (++iarg<argc) t2t_add_offset = atof(argv[iarg]);
	    else {
	      cerr << "Option -aT2T given without value\n" << endl;
	      PrintUsage(argv[0]);
	    }
	  }
	  else if( !strncmp(argptr,"T20",3) ) {
	    argptr+=3;
	    if (*argptr)          t20_add_offset = atof(argptr);
	    else if (++iarg<argc) t20_add_offset = atof(argv[iarg]);
	    else {
	      cerr << "Option -aT20 given without value\n" << endl;
	      PrintUsage(argv[0]);
	    }
	  }
	  else if( !strncmp(argptr,"S2T",3) ) {
	    argptr+=3;
	    if (*argptr)          s2t_add_offset = atof(argptr);
	    else if (++iarg<argc) s2t_add_offset = atof(argv[iarg]);
	    else {
	      cerr << "Option -aS2T given without value\n" << endl;
	      PrintUsage(argv[0]);
	    }
	  }
	  else if( !strncmp(argptr,"TCI",3) ) {
	    argptr+=3;
	    if (*argptr)          tci_add_offset = atof(argptr);
	    else if (++iarg<argc) tci_add_offset = atof(argv[iarg]);
	    else {
	      cerr << "Option -aTCI given without value\n" << endl;
	      PrintUsage(argv[0]);
	    }
	  }
	  break;
	}
	PrintUsage(argv[0]);
#endif
      case 'h':
        PrintUsage(argv[0]);
        break;
      default:
        cerr << "Unknown Option <" << argv[iarg] << ">\n"<< endl;
        PrintUsage(argv[0]);
      }
    }
    else {
      bosfile.push_back(string(argv[iarg]));
    }
    iarg++;
  }

  if (!bosfile.size()) {
    cerr << "No input file specified \n" << endl;
    PrintUsage(argv[0]);
  }

  read_corr(do_readcorr);
	
  if (do_xdisplay) {
    theApp = new TRint("Interactive", 0, 0, 0, 0, 0 );  // activate interactive ROOT processor
    C = new TCanvas ("C", "C", 800, 600);      // open a new window to display histog    
  }


  initbos();                                          // initialize bos library
  st_set_def_();
#ifdef STTCL
  sttcl_real_.tag2tof_add_offset = t2t_add_offset;
  sttcl_real_.st2tof_add_offset  = s2t_add_offset;
  sttcl_real_.tagt20_add_offset  = t20_add_offset;
  sttcl_real_.tci_add_offset     = tci_add_offset;
  sttcl_real_.rf_add_offset      = rf_add_offset;
#endif
  tagtcl_set_def_();
  tag_init_();

  signal (2, handle_break);
  signal (3, handle_break);

  int eventCount = 0;                                 // event count
  int errorCount = 0;
  int epicCount = 0;
  int fileCount = 0;
  int runNumber = 0;
  int eventNumber = 0;

  // loop over file-list:  bosformat file1 file2 ... filen

  for (vector<string>::iterator ibos = bosfile.begin(); ibos!=bosfile.end(); ibos++) {

    if (! fileloop) break;
    JBosFile bf( ibos->c_str() ); // open file

    if (bf.good()) {       // file is good (e.g. existing, readable..)

      fileCount++;
      int name2runno =0;
      int name2fileno=0;

      if ( !extractRunNumber(name2runno, name2fileno, ibos->c_str()) ) {
	cerr<< "can't extract run/file number from filename" <<endl;
      }
      else
	cout<<"extracted run number: "<<name2runno<<", file "<<name2fileno<<endl;

      if (do_writetagr) {
	char *subdir="extract";
	char extrname [255];
	char *dir=(char*)gSystem->OpenDirectory(subdir);
	if( !dir ) {
	  if( gSystem->MakeDirectory(subdir) )   // 0 for success, -1 for fail
	    strcpy(subdir,".");
	}
	sprintf(extrname, "%s/tagr%05d.A%02d.dat", subdir, name2runno, name2fileno);
	gTagrFile = new JTagrFile(extrname, ios::out);
      }

      if (! froot) {
	book_histo(name2runno,cRootOutFile);

	if (do_xdisplay) {
	  gStyle->SetPalette(1);            // use rainbow colors for histogram
	  gStyle->SetOptStat(11);
	  C->Divide(1,2);
	  C->cd(1);
	  hrftag->Draw("colz");
	  C->cd(2);
	  hrftag2->Draw("colz");
	  //      gPad->SetLogz(1);
	  ///
	  //hgammakine->Draw();
	  //hgammafound->SetFillColor(5);
	  //hgammafound->Draw("same");
	  //hec_old_new0->GetXaxis()->SetRangeUser(TCNT_MAX-0.5,160.5);
	  //hec_old_new0->GetYaxis()->SetRangeUser(TCNT_MAX-0.5,160.5);
	}

      }
      if (do_xdisplay && name2runno) {
        char canvtitle[80];
        sprintf(canvtitle,"tagraw run %05d.A%02d", name2runno, name2fileno);
        C->SetTitle(canvtitle);
      }

      bool loop = true;   
      while (fileloop && loop) {       // loop over events in file
 	JBosEvent be;      // get a single event
	
	// we check for be.good(). The variable loop stores the ...
	if ( ( loop = be.good()) ) { // ... result of this check

          Jhead head;
          if (!head.Found()) continue;

          if( head[0].evtclass > 15 ) continue; //sync,prestart,go,pause,end events
          if( head[0].evtclass == 0 ) {         //special event (scaler,database,epics)
	    bool epic_evnt=false;
            for (int i=0; i<2; i++) {
              Jepic epic(i); 
              Jtesc tesc(i);
              if( epic.Found() || tesc.Found() ) epic_evnt=true;
            }
            if( epic_evnt ) {
	      epicCount++;
	      fill_epics_charts(eventNumber);
	    }
	    //	    else if( head[0].type==10 ) {  // scaler event
	    continue;
          }

	  eventCount++;
          eventNumber = head[0].nevent;

	  if (head[0].nrun != runNumber) {
	    runNumber =  head[0].nrun;
	    if (runNumber>0) {
	      cout << "runNumber = " << runNumber 
		   << " found in event " << eventNumber  
		   << " (" << eventCount <<". event in file)" <<endl;
	      //  initialize_tof(runNumber);
	      initCL01(runNumber);
	      tag_brun_(&runNumber);
              st_brun_(&runNumber);
              stpp_init_geometry();
	      
	      //	      gCalib = new JCalibration(runNumber);
	      get_calibration_constants(runNumber);
	      const2histo();
	      book_stripcharts(eventNumber,eventMax);

	      hconst_run->SetBinContent(1,runNumber);
	    }
	  }

	  try {
	    analyze_event(eventNumber);
	  }

	  catch (int e) {
            errorCount++;
	  }

	  if ( !(eventCount %100)) {
	    cout.width(10);
	    cout<< eventCount;
	    cout.width(5);
	    cout <<" evts; "<< epicCount <<" epic; "<< no_tagr <<" no TAGR; "<< no_cl01 <<" no CL01; "
		 << no_tage <<" no TAGE; "<< no_tgtlr <<" no TGTL/R\r";
	    cout.flush();
	    if (do_xdisplay) {
	      gSystem->ProcessEvents();
	    }
	  }

	  // refresh screen all 10000 events
	  if ( !(eventCount %10000)) {
	    if (do_xdisplay) {
	      for (int i=1; i<=2; i++) {
		C->cd(i);
		gPad->Modified();
	      }
	      C->Update();
	      //             char gifname[80];
	      //	      sprintf (gifname, "snapshot/doublepeak%04d.gif", eventCount/1000);
	      //	      C->Print(gifname);
	    }
	  } 
	}                  // end branch "event is good"
	if(fileloop) {
	  fileloop = eventMax==0 || eventCount<eventMax;
	  if(!fileloop) cout<< "\nstop processing: " << eventMax << " events done" <<endl;
	}
      }                    // end loop over events in file

      int myfilecount = (fileCount<6) ? fileCount-1 : 5;
      //start with bin 4
      hconst_run->SetBinContent(myfilecount*10+2,name2fileno);
      hconst_run->SetBinContent(myfilecount*10+3,eventNumber);
      hconst_run->SetBinContent(myfilecount*10+4,epicCount);
      hconst_run->SetBinContent(myfilecount*10+5,errorCount);
      hconst_run->SetBinContent(myfilecount*10+6,no_tagr);
      hconst_run->SetBinContent(myfilecount*10+7,no_cl01);
      hconst_run->SetBinContent(myfilecount*10+8,no_tage);
      hconst_run->SetBinContent(myfilecount*10+9,no_tgtlr);

    }                      // end branch "file is good"
    if (gTagrFile) {
      delete gTagrFile;
      gTagrFile = NULL;
    }
  }                        // end loop over file-list  bosformat file1 file2 ... filen
  
  final_plots();

  froot->Write();
  froot->Flush();

  if (do_xdisplay) {
    gPad->Modified();
    gPad->Update();

    theApp->Run();           // interactive ROOT started (stops when user enters .q)
  }

  return 0;
  
}

void final_plots() {
  
}

void fill_epics_charts(int nevent) {

  if(!do_stripcharts || !nevent) return;

  float xevent = (float)nevent;
  Jhead head;
  int epicNumber = head[0].nevent;
  Jepic epic(1);
  Jtesc tesc(0);

  if( epic.Found() ) {  //epic (record#1) about every 2sec
    char *beam_xpos={"IPM2C21A.XPOS"};
    char *beam_ypos={"IPM2C21A.YPOS"};
    char *coh_edge ={"coh_edge"};
    
    for(int i=0; i<epic.GetNrows(); i++) {
      // structure epic_t is shit ...
      char *s; s=(char*)&epic[i].char1;
      //		  for(int j=0; j<strlen(beam_xpos); j++) {
      if( !strncmp(beam_xpos,s,strlen(beam_xpos)) ) {
	float xpos = epic[i].value;
	hbeamx->Fill(xevent,xpos);
      }
      if( !strncmp(beam_ypos,s,strlen(beam_ypos)) ) {
	float ypos = epic[i].value;
	hbeamy->Fill(xevent,ypos);
      }
      //      if( !strncmp(coh_edge,s,strlen(coh_edge)) ) {
      //	float edge = epic[i].value;
      //	hcohedge->Fill(xevent,edge);
      //      }
    }
  }
  if( tesc.Found() ) {
    for(int i=0; i<tesc.GetNrows(); i++)         //TESC seems to be screwed up for g9a? 
      htesc->Fill(xevent,i+1.0,tesc[i].value);
  }
}

class DetectorResponse {
public:
  int index;
  double t0;
  double time;
  double path;
  DetectorResponse() : index(-1), t0(0), time(0), path(0) {}
};

void analyze_event(int eventno) {

  // tag_filter_out.F: Eid_time=-ECchwidth*tdc/1000.-posEpeak
  //                   tl_time=-TCchwidthL(id)*tdc/1000.-posTLpeak; same for tr_time
  //     t_mean=(tl_time+tr+time)/2;   t_time=rf_corr_time_counter(id,t_mean)
  //tagi: idt=Tid;ide=Eid;timel;timer;timemean;trf=t_time-t_mean;timee=Eid_time;
  //      nexttime=t_mean-t_mean_neighbour for overlap=even Tid
  //tagr: t_id;e_id;stat;erg;ttag=t_mean+tag2tof; tpho=t_time+tag2tof;

  Jtgtl tgtl;
  Jtgtr tgtr;
  Jtage tage;

  Jhead head;
  Jcall call;
  Jrft  rft;

  if( !rft.Found() && !call.Found())  
    cout<< "Neither RFT nor CALL bank in event "<< eventno <<"              \n"<<endl;
  if( !tgtl.Found() || !tgtr.Found() ) no_tgtlr++; 
  if( !tage.Found() ) no_tage++;

  // CL01 info for all RF tdcs (old (1,2): FASTBUS single hit; new (3,4): VME pipeline)
  double rfcl01[4] = { 0, 0, 0, 0 };
  for (int i=0; i<4; i++) {
    dropAllBanks(&bcs_, "CL01");
    make_cl01_bank_using(i+1);
    Jcl01 cl01;
    if( cl01.Found() ) {
      hrawcl01[i]->Fill(cl01[0].rf);
      hrfavail->Fill(i+1);
      if(!do_skipCL01) rfcl01[i] = cl01[0].rf - 10000 * RFcycle;
    }
    else
      hrfavail->Fill(0);
  }

  dropAllBanks(&bcs_, "CL01");
  if( !do_skipCL01 ) {
    make_CL01_bank();
  }
  Jcl01 cl01;
  if( !cl01.Found() ) no_cl01++;

  dropAllBanks(&bcs_,"TAGITAGR");
  tag_evnt_();

  Jtagr tagr;
  if (gTagrFile) gTagrFile->write();
  if( !tagr.Found() ) no_tagr++;

  //  check whether VME reference signal is constant(=5000)
  //  Jref ref;
  //  static int mytest=0;
  //  int ind_ref=-1;
  //  if( ref.Found() ){
  //    for(int i=0; i<ref.GetNrows(); i++) {
  //     if( (ref[i].id&0xff00)>>8 == 22 ) ind_ref=i;
  //     if(mytest<2) cout<<" REF "<< (int)((ref[i].id&0xff00)>>8) <<" "<< (int)(ref[i].id&0xff) <<" : "<<(int)(ref[i].tdc) <<endl;
  //   }
  //   if(ind_ref>=0) hreftdc->Fill((float)(ref[ind_ref].tdc));
  //  }
  //  if(mytest++<2) cout<<" reference signal for ROC 22 index= "<<ind_ref<<endl;

  // trigger info
  //  bool trigbit2, trigbit6;   //for g8b
  bool trigbit1to6 = false;
  if (head.Found()) {
    for (int i=0; i<31; i++) {
      bool bitset = head[0].trigbits & (1<<i);
      if (bitset) {
	htrig->Fill (i+1);
	if (i>=0 && i<6) trigbit1to6 = true;
      }
      //      if (i+1==2) trigbit2 = bitset;
      //      if (i+1==6) trigbit6 = bitset;
    }
  }

  // raw RF info for all 4 possible
  vector<int> rfraw[4];
  if (call.Found()) {
    for (int i=0; i<call.GetNrows(); i++) {
      switch (call[i].id) {
      case 6: rfraw[0].push_back(call[i].tdc); break;
      case 7: rfraw[1].push_back(call[i].tdc); break;
      }
    }
  }
  if (rft.Found()) {
    for (int i=0; i<rft.GetNrows(); i++) {
      switch (rft[i].id) {
      case 6: rfraw[2].push_back(rft[i].tdc); break;
      case 7: rfraw[3].push_back(rft[i].tdc); break;
      }
    }
  }

  // rftime for pipeline tdcs
  vector<double> rftime[2];
  for (int i=0; i<4; i++) {
    double val0=0.0;
    for (int j=0; j<rfraw[i].size(); j++) {
      hrfraw[i]->Fill(rfraw[i][j]);
      if(i>1){
        double xtime=getCL01time(7+i,rfraw[i][j]);
        rftime[i-2].push_back(xtime);
        if( val0!=0.0 ) {
          double xdiff = fabs(val0-rfraw[i][j]);
          if(xdiff<1700) hrfrawdiff[i-2]->Fill(xdiff);
          hrfrawmod[i-2]->Fill(fmod(xdiff+10.0*RFdiff[i-2],RFdiff[i-2]*1.0));
        }
      }
      val0=rfraw[i][j];
    }
  }

  // RF time difference for all hits
  for(int i=0; i<2; i++) {
    if(rftime[i].size() >1){
      for (int j1=0; j1<rftime[i].size()-1; j1++) {
        for (int j2=j1+1; j2<rftime[i].size(); j2++) {
          hrfdiff[i]->Fill(rftime[i][j2]-rftime[i][j1]);
          double xdiff=rf_correction_center(rftime[i][j1],rftime[i][j2]);
          hrfmod[i]->Fill(xdiff);
        }
      }
   }
  }

  // RF raw and time difference between RF3 and RF4 tdc
  for (int j0=0; j0<rftime[0].size(); j0++) {
    for (int j1=0; j1<rftime[1].size(); j1++) {
      double xdiff=fmod(rftime[0][j0]-rftime[1][j1]+10000.5*RFcycle,RFcycle)-0.5*RFcycle;
      hrfdif23->Fill(xdiff);
    }
  }
  for (int j0=0; j0<rfraw[2].size(); j0++) {
    for (int j1=0; j1<rfraw[3].size(); j1++) {
      hrfrawdif23->Fill(rfraw[2][j0]-rfraw[3][j1]);
    }
  }

  if (tagr.Found()) {

    for (int j=0; j<tagr.GetNrows(); j++) {
      int    index   = tagr[j].t_id - 1;
      double tagtime = tagr[j].ttag + tag_corr[index] - tag2tof_value[0];
      double tagtpho = tagr[j].tpho + tag_corr[index] - tag2tof_value[0];
      htagtpho->Fill(index+1, tagtime - tagtpho);

      for (int k=0; k<4; k++) {
	if (rfcl01[k] != 0) {
	  double rfttag = fmod (tagtime - rfcl01[k], RFcycle) - 0.5*RFcycle;
	  double rftpho = fmod (tagtpho - rfcl01[k], RFcycle) - 0.5*RFcycle;
	  if (k== (use_rf-1) ) {
	    hrftag->Fill(index+1, rfttag);
	    hrftpho->Fill(index+1, rftpho);
	    if (do_stripcharts) {
	      for (int n=0; n<NTCNT_CHARTS; n++) {
		if (Tcnt_chart[n]==index/2+1) 
		  hevts_rftag[n]->Fill(eventno, rfttag);
	      }
	    }
	  }
          else {
	    hrftag2->Fill(index+1, rfttag);
	    hrftpho2->Fill(index+1, rftpho);
	  }
	  hrfcl01[k]->Fill(rfraw[k][0], rfttag ); 
	}
      }

      int cci[4] = { 5, 6, 8, 9};  /// call calibration constants index
      for (int k=0; k<4; k++) {
	for (int l=0; l<rfraw[k].size(); l++) {
	  double xrft =
	    call_T0_value[cci[k]]                 +
	    call_T1_value[cci[k]]*    rfraw[k][l] +
	    call_T2_value[cci[k]]*SQR(rfraw[k][l])- 
	    10000. * RFcycle;
	  hrffun[k]->
	    Fill(rfraw[k][l], fmod (tagtime - xrft, RFcycle) - 0.5*RFcycle);
	}
      }
    }
  }

  // tagger T-counter entries 
  vector<int> lval[TCNT_MAX];
  vector<int> rval[TCNT_MAX];

  if (tgtl.Found() && tgtr.Found()) {
    for (int i=0; i<tgtl.GetNrows(); i++) {
      int index = tgtl[i].id-1;
      hlcount->Fill(index+1);
      if (index < TCNT_MAX) {
	lval[index].push_back (tgtl[i].tdc);
      }
      else {
	cout << "Tcntr "<< index << " left not found in event "<< eventno << endl;
      }
    }

    for (int i=0; i<tgtr.GetNrows(); i++) {
      int index = tgtr[i].id-1;
      hrcount->Fill(index+1);
      if (index < TCNT_MAX) {
	rval[index].push_back (tgtr[i].tdc);
      }
      else {
	cout << "Tcntr "<< index << " right not found in event "<< eventno << endl;
      }
    }
  
    if (TAGTrefid > 0) {
      for (vector<int>::iterator il = lval[TAGTrefid-1].begin(); 
	   il!= lval[TAGTrefid-1].end(); il++) {
	for (vector<int>::iterator ir = rval[TAGTrefid-1].begin(); 
	     ir!= rval[TAGTrefid-1].end(); ir++) {
	  double ltime = *il;
	  double rtime = *ir;
	//	  if (28920 < ltime && ltime < 29000 &&
	//	      27370 < rtime && rtime < 27450 && 
	//	      trigbit6  && ! trigbit2) {
	  if (trigbit1to6) {
	    hleft ->Fill(ltime);
	    hright->Fill(rtime);
	  //	  double rf = -tag_t_slope_left[TAGTrefid-1] * ltime/1000. - rfcl01[use_rf-1];
	  //	  hrf->Fill(rf);
	    hrf->Fill(fmod(-rfcl01[use_rf-1],RFcycle)-0.5*RFcycle);
	  }
	}
      }
    }
    
    /// all Tcounters: left-right alignment
    for (int indTcnt = 0; indTcnt < TCNT_MAX; indTcnt++) {
      for (vector<int>::iterator tdcl = lval[indTcnt].begin(); 
	   tdcl!= lval[indTcnt].end(); tdcl++) {
	for (vector<int>::iterator tdcr = rval[indTcnt].begin(); 
	     tdcr!= rval[indTcnt].end(); tdcr++) {
	  double lt = 
	    - tag_t_dt_left[indTcnt]
	    - tag_t_slope_left[indTcnt] **tdcl/1000.;
	  
	  double rt = 
	    - tag_t_dt_right[indTcnt]
	    - tag_t_slope_right[indTcnt] **tdcr/1000.;
	  
	  hlr[indTcnt]->Fill(*tdcl+*tdcr, lt-rt);
	  hdifft[indTcnt]->Fill(lt-rt);
	  
	  if (fabs(lt-rt)<1.) {
	    double tagtime = (lt + rt)/2. + tag_corr[indTcnt*2] - tag2tof_value[0];
	    double rf2ttag = fmod (tagtime - rfcl01[use_rf-1], RFcycle) - 0.5*RFcycle;
	    hrftcnt->Fill(indTcnt+1, rf2ttag);
	  }
	  hlraw ->Fill(indTcnt+1,*tdcl);
	  hrraw ->Fill(indTcnt+1,*tdcr);
	}
      }
    }

    if (tage.Found()) {
      for (int i=0; i<tage.GetNrows(); i++) {
	int indEcnt = tage[i].id - 1;
	double et = - tag_e_dt[indEcnt] - tag_e_slope[0]/1000. * tage[i].tdc;
        heraw->Fill(indEcnt+1, tage[i].tdc);
	hecount->Fill(indEcnt+1);
	if (indEcnt >= 0 && indEcnt < 384 && tage[i].tdc > 10) {
	  for (int indTcnt = ETmatch[indEcnt][0]-1; 
	       indTcnt<=ETmatch[indEcnt][1]-1; indTcnt++) {
	    for (vector<int>::iterator tdcl = lval[indTcnt].begin(); 
		 tdcl!= lval[indTcnt].end(); tdcl++) {
	      for (vector<int>::iterator tdcr = rval[indTcnt].begin(); 
		   tdcr!= rval[indTcnt].end(); tdcr++) {
		double lt = -tag_t_dt_left[indTcnt] 
		  - tag_t_slope_left[indTcnt]/1000. * (*tdcl);      
		double rt = - tag_t_dt_right[indTcnt]
		  - tag_t_slope_right[indTcnt]/1000. * (*tdcr);      
		double tt = (lt+rt)/2. + tag_corr[indTcnt*2];
		
		hecntr->Fill(indEcnt+1, et - tt);
		//	if(et-tt<-15.1 || et-tt>-1.40) 
		heslope->Fill(tage[i].tdc, et - tt); 
	      }
	    }
	  }
	}
      }
    
    // same for found matches in TAGR
      if (tagr.Found()) {
        for (int i=0; i<tagr.GetNrows(); i++) {
          if (tagr[i].stat==7 || tagr[i].stat==15) {
            int indTcnt = (tagr[i].t_id-1)/2;
            int indEcnt0 = tagr[i].e_id>1 ? tagr[i].e_id/2 : 1;
            int indEcnt1 = (tagr[i].e_id+1)/2;
            for (int j=0; j<tage.GetNrows(); j++) {
              if( tage[j].id==indEcnt0 || tage[j].id==indEcnt1 ) {
                int indEcnt = tage[j].id-1;
                double et = -tag_e_dt[indEcnt] 
                  - tag_e_slope[0]/1000. * tage[j].tdc;
                hecnttagr->Fill(indEcnt+1, et + tag2tof_value[0] - tagr[i].tpho);
              }
            }
          }
        }
      }
    }

    /// comparison of Tcounter time if entry in TAGR
     if (tagr.Found()) {
      for (int i=0; i<tagr.GetNrows(); i++) {
	if (tagr[i].t_id & 1) {
	  int indTagr = tagr[i].t_id-1;
	  int indTcnt = (tagr[i].t_id - 1) / 2;
	  for (vector<int>::iterator tdcl = lval[indTcnt].begin(); 
	       tdcl!= lval[indTcnt].end(); tdcl++) {
	    for (vector<int>::iterator tdcr = rval[indTcnt].begin(); 
		 tdcr!= rval[indTcnt].end(); tdcr++) {
	      double lt = 
		- tag_t_dt_left[indTcnt]
		- tag_t_slope_left[indTcnt] *(*tdcl)/1000.;      

	      double rt = 
		- tag_t_dt_right[indTcnt]
		- tag_t_slope_right[indTcnt] *(*tdcr)/1000.;      
	      
	      double ttag = (lt + rt) / 2. + tag2tof_value[0] - tag_t_ci[indTagr];

	      httag->Fill (indTcnt+1, ttag-tagr[i].ttag);
	      htpho->Fill (indTcnt+1, ttag-tagr[i].tpho);
	    }
	  }
	}
      }
    }
  }
  // check with ST paddle 1 (not too long to have more than 2nsec distribution)
  int stlevel = 1;
  dropAllBanks(&bcs_, "ST1 ");
  st_bevt_();
  st_evnt_(&stlevel);
  Jst1 st1;
  if( st1.Found() ) {
    for(int i=0; i<st1.GetNrows(); i++) {
      int sector=st1[i].id/100;
      int paddle=st1[i].id/10-sector*10;
      if(st1[i].adc_1>0 && sector>0 && sector<7) {
	hst1_time->Fill((sector-1)*4+paddle,st1[i].time_1);
	if (tagr.Found() && paddle==1) {
	  for (int j=0; j<tagr.GetNrows(); i++) {
            int indTcnt = (tagr[i].t_id+1)/2;
	    //	    hst1_ttag[sector-1]->Fill(indTcnt,st1[i].time_1-tagr[j].ttag);
	    hst1_tpho[sector-1]->Fill(indTcnt,st1[i].time_1-tagr[j].tpho);
	  }
	}
      }
    }
  }
}

