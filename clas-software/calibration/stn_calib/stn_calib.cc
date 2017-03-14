//$Id: stn_calib.cc,v 1.6 2008/04/08 15:18:37 fklein Exp $
using namespace std;

#include <iostream>
#include <string>
#include <sstream>
#include <vector>
#include <map>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include <signal.h>

extern "C" {
#include "ntypes.h"
#include "bostypes.h"
#include "clas_cern.h"
#include "kinematics.h"
#include "map_manager.h"
#include "sc.h"
#include "pid.h"
#include "utility.h"
}

#include "ROOT.h"
#include "DOCA.h"
#include "stpp_JBosBank.h"
#include "stpp_JCalibration.h"
#include "stpp_STposition.h"
#include "stpp_STdata.h"
#include "stpp_SThit.h"
#include "stpp_STvector.h"

/// switch on to recook STR banks (which are not needed in the current version of this code)
bool remakeST = false;

const int EC_MAX  = 1296;
const int CC_MAX  =  216;
const double c_light = 29.9792458;
const int TAG_MAX =   64;
const int SC_MAX  =  288;
const int T_MAX   =  121;
const double RFcycle = 2.004;

const double PROTM  = 0.93827;
const double NEUTM  = 0.93957;
const double PIONM  = 0.13957;
const double KAONM  = 0.49368;
const double KZERM  = 0.49767;
const double ELECM  = 0.00051;
const double LAMBM  = 1.11568;
const double DEUTM  = 1.8756; 

const int pion_id   = 20;
const int kaon_id   = 21;
const int prot_id   = 22;
const int deut_id   = 23;


/// values can be overwritten by commandline options
bool xdisplay = true;
char* gRawDirectory = NULL;
char* gRootOutFile  = NULL;
double dt_tagcutS = 1.2;
double dt_tagcutM = 1.2;

extern "C" {
  void st_brun_(int* runno);
  void st_bevt_();
  void st_evnt_(int* stlevel);
  void st_set_def_();
}

TFile* froot = NULL;
extern JCalibration* gCalib;
//------------- JBosFile -------------------------

bool fileloop   = true;
void handle_break(int signal) {
  cout << "Signal terminates loop" << endl;
  fileloop = false;
}

inline double SQR(double x) { return x*x; }

void PrintUsage(const char *processName) {
  cerr << "\nUsage: " << processName << " [-opt1 ... -optn] file1 [file2] ... [fileN]\n\n";
  cerr << "Options:\n";
  cerr << "\t-h      \tPrint this message.\n\n";
  cerr << "\t-n[#]   \tProcess only # number of events\n\n";
  cerr << "\t-o[file]\tOutput <filename>\n\n";
  cerr << "\t-r[dir] \tRaw data input from (not BOS) file <dir>/stnraw_<runno>_a<no>\n\n";
  cerr << "\t-t[#.#] \tTag/ToF time match in #.#ns window for single-track events\n\n";
  cerr << "\t-T[#.#] \tTag/ToF time match in #.#ns window for multi-track ambiguities\n\n";
  cerr << "\t-X      \tX-windows: dont open histogram canvas window\n";
  cerr << endl;
  exit(0);
}


int strId(int id) {
  int sec = id / 100 -1;
  id %= 100;
  return sec*4 + id/10 - 1;
}

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
    cerr << "Error reading next event (might be end of file)" << endl;
    isGood = false;
  }
}

int analyze_event(istream* rawStream);

void final_plots();

// destructor clears bos banks
JBosEvent::~JBosEvent () {
  dropAllBanks(&bcs_,"E");
  cleanBanks(&bcs_);
}

TRint*   theApp;  // interactive version of ROOT processor

// define some histograms
TH1F* hreject_event;
TH1F* hvieh_deg, *hsectormissmatch;
TH2F* hvieh_stplane;
TH2F* hdt_adc[ST_MAX][4][3], *hdt_dist[ST_MAX][4][3];
TH2F* hsctag[4], *htagsc[4], *hsttag[4], *hstsc[4];
TH2F* htau_pos, *htau_neg, *hpose, *hnege;
TH2F* hhits4segment;
TH1F* hmassq_pos[2], *hmassq_neg[2], *hmult_tagr;
TH1F* hsttime[4], *hinfo_available;
TH1F* hmultstr, *hmult_stn0, *hmult_stn1, *hmultdata, *hstr_tbtr;
TH1F* hmypart;
TH2F* hxz_stplane[4], *hcompare_stpb[24], *hdt_dist_nose[24];
TH2F* hadc_neg[24], *hadc_pos[24], *hpartdedx[24][4], *hadc_corr[24];
TH1F* ht0_phypothesis[4];
TH1F* ht0_tdiff_2part, *ht0_tdiff_1part_tag, *hdt_average;
TH1F* ht0_tbest_mpart[4], *ht0_tbest_1part[4];

int tagcount = 0;
int tdplcount = 0;
int stn0count = 0;
int stn1count = 0;

void book_histo(int runno) {
  ostringstream rootfilename;
  
  if (gRootOutFile) 
    rootfilename << gRootOutFile;
  else
    rootfilename << "stn_calib.r" << runno << ".root";
  
  froot = new TFile(rootfilename.str().c_str(), "recreate");

  //with version 5.16 TDirectory became pure interface: use new class TDirectoyFile
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,16,0)
  TDirectoryFile* dir_tw   = new TDirectoryFile("tw",   "tw");
  TDirectoryFile* dir_veff = new TDirectoryFile("veff", "veff");
  TDirectoryFile* dir_gain = new TDirectoryFile("gain", "gain");
  TDirectoryFile* dir_caldb= new TDirectoryFile("caldb","caldb");
#else
  TDirectory* dir_tw   = new TDirectory("tw",   "tw");
  TDirectory* dir_veff = new TDirectory("veff", "veff");
  TDirectory* dir_gain = new TDirectory("gain", "gain");
  TDirectory* dir_caldb= new TDirectory("caldb","caldb");
#endif

  hmultstr = new TH1F ("multstr", "multstr", 50, -0.5, 49.5);
  hmult_stn0 = new TH1F ("mult_stn0", "mult_stn0", 50, -0.5, 49.5);
  hmult_stn1 = new TH1F ("mult_stn1", "mult_stn1", 50, -0.5, 49.5);
  hmultdata = new TH1F ("multdata", "multdata", 50, -0.5, 49.5);
  hvieh_deg = new TH1F("vieh_deg", "vieh_deg", 180, 0., 360.);
  hvieh_stplane = new TH2F ("vieh_stplane", "vieh_stplane", 24, -0.5, 23.5, 100, 0., 360.);

  hsectormissmatch = new TH1F("sectormissmatch", "sectormissmatch",
			      180, -180., 180);

  hmypart = new TH1F("mypart", "mypart", 50, -0.5, 49.5);
  ht0_tdiff_2part  = new TH1F("t0_tdiff_2part", "t0_tdiff_2part", 401, -20., 20.);
  ht0_tdiff_1part_tag = new TH1F("t0_tdiff_1part_tag", "t0_tdiff_1part_tag", 401, -20., 20.);
  hdt_average  = new TH1F("dt_average", "dt_average", 401, -20., 20.);

  hstr_tbtr = new TH1F ("str_tbtr",  "str_tbtr",  200, -0.01, 5.);

  hpose = new TH2F ("pose", "pose", 100, 0., 2.5, 100, 0., 90.);
  hnege = new TH2F ("nege", "nege", 100, 0., 2.5, 100, 0., 90.);


  hmult_tagr = new TH1F("mult_tagr", "mult_tagr", 50, -0.5, 49.5);
  

  hhits4segment = new TH2F("hits4segment", "hits4segment", 48, -0.5, 47.5, 24, -0.5, 23.5);
  hhits4segment ->SetOption("colz");

  hinfo_available     = new TH1F("info_available", "info_available", 51, -1.5, 49.5);

  hinfo_available->GetXaxis()->SetBinLabel(49, "STcalib");
  hinfo_available->GetXaxis()->SetBinLabel(44, "STPB");
  hinfo_available->GetXaxis()->SetBinLabel(39, "SCPB");
  hinfo_available->GetXaxis()->SetBinLabel(34, "DCPB");
  hinfo_available->GetXaxis()->SetBinLabel(29, "ECPB");
  hinfo_available->GetXaxis()->SetBinLabel(25, "TDPL");
  hinfo_available->GetXaxis()->SetBinLabel(20, "STNx");

  hreject_event = new TH1F("reject_event", "reject_event", 15, -0.5, 14.5);
  hreject_event->GetXaxis()->SetBinLabel( 1, "OK");
  hreject_event->GetXaxis()->SetBinLabel( 2, "no_head");
  hreject_event->GetXaxis()->SetBinLabel( 3, "no_tagr");
  hreject_event->GetXaxis()->SetBinLabel( 4, "raw_read");
  hreject_event->GetXaxis()->SetBinLabel( 5, "raw_sync");
  hreject_event->GetXaxis()->SetBinLabel( 6, "no_track");
  hreject_event->GetXaxis()->SetBinLabel( 7, "noTmatch");
  hreject_event->GetXaxis()->SetBinLabel( 8, "t0_bad_1");
  hreject_event->GetXaxis()->SetBinLabel( 9, "t0_ambig");
  hreject_event->GetXaxis()->SetBinLabel(10, "no_STdat");
  hreject_event->GetXaxis()->SetBinLabel(13, "c_except");
  hreject_event->GetXaxis()->SetBinLabel(14, "i_except");
  hreject_event->GetXaxis()->SetBinLabel(15, "u_except");

  hnege     ->SetOption("colz");
  hpose     ->SetOption("colz");
  hvieh_stplane ->SetOption("colz");
  
  char* cpart[] = { "pion", "kaon", "prot", "deut" }; 

    /*
    //    hst1st[i] = new TH2F (hstname, hstname, 100, 2000., 9000., 100, 700., 1600.);
    hst1st[i] = new TH2F (hstname, hstname, 100, 2000., 9000., 100, -600., 400.);
    //    hst1st[i] = new TH2F (hstname, hstname, 100, 0., 4000., 100, 0., 1000.);
  }
  */

  htau_pos = new TH2F ("tau_pos", "tau_pos", 100, 0., 3., 100, 0.8, 2.5);
  htau_pos ->SetOption("colz");
  htau_neg = new TH2F ("tau_neg", "tau_neg", 100, 0., 3., 100, 0.8, 2.5);
  htau_neg ->SetOption("colz");

  
  for (int i=0; i<24; i++) {
    char hstname[80];

    sprintf (hstname, "compare_stpb%02d", i);
    hcompare_stpb[i] = new TH2F (hstname, hstname, 120, -20., 80., 121, -4., 4.); 
    hcompare_stpb[i]->SetOption("colz");

    for (int j=0; j<4; j++) {
      sprintf (hstname, "partdedx%02d_%s", i, cpart[j]);
      hpartdedx[i][j] = new TH2F (hstname, hstname, 120, 0., 3., 120, 0., 800);
      hpartdedx[i][j]->SetOption("colz");
    }
  }

  for (int i=0; i<2; i++) {
    char hstname[80];
    double massq_min[2] = { -0.2, -0.4 };
    double massq_max[2] = {  1.2,  5.4 };
    sprintf (hstname, "massq_pos%d", i);
    hmassq_pos[i] = new TH1F (hstname, hstname, 400, massq_min[i], massq_max[i]);
    sprintf (hstname, "massq_neg%d", i);
    hmassq_neg[i] = new TH1F (hstname, hstname, 400, massq_min[i], massq_max[i]);
  }


  for (int i = 0; i< 4; i++) {
    char hstname[80];
    sprintf (hstname, "sctag_%s", cpart[i]);
    hsctag[i] = new TH2F (hstname, hstname, SC_MAX, -0.5, SC_MAX-0.5, 199, -20., 20.);
    hsctag[i]->SetOption("colz");

    sprintf (hstname, "tagsc_%s", cpart[i]);
    htagsc[i] = new TH2F (hstname, hstname, T_MAX, -0.5, T_MAX-0.5, 199, -20., 20.);
    htagsc[i]->SetOption("colz");

    sprintf (hstname, "xz_stplane%d", i);
    hxz_stplane[i] = new TH2F (hstname, hstname, 120, -100., 20., 120, -60, 60);
    hxz_stplane[i]->SetOption("colz");

    sprintf (hstname, "sttag_%s", cpart[i]);
    hsttag[i] = new TH2F (hstname, hstname, 24, -0.5, 23.5, 799, -10., 10.);
    hsttag[i]->SetOption("colz");

    sprintf (hstname, "stsc_%s", cpart[i]);
    hstsc[i] = new TH2F (hstname, hstname, 24, -0.5, 23.5, 199, -20., 20.);
    hstsc[i]->SetOption("colz");

    sprintf (hstname, "sttime_%s",  cpart[i]);
    hsttime[i]  = new TH1F(hstname, hstname, 100, -5., 5.);

    sprintf (hstname, "t0_tbest_mpart_%s", cpart[i]);
    ht0_tbest_mpart[i] = new TH1F (hstname, hstname, 401, -20., 20.);

    sprintf (hstname, "t0_tbest_1part_%s", cpart[i]);
    ht0_tbest_1part[i] = new TH1F (hstname, hstname, 401, -20., 20.);

    sprintf (hstname, "t0_phypothesis_%s", cpart[i]);
    ht0_phypothesis[i] = new TH1F (hstname, hstname, 401, -20., 20.);
  }

  for (int i = 0 ; i< ST_MAX; i++) {
    char hstname[80];

    sprintf (hstname, "dt_dist_nose%02d", i);
    hdt_dist_nose[i] = new TH2F (hstname, hstname, 120, 45., 70., 121, -2., 2.); 
    hdt_dist_nose[i]->SetOption("colz");

    for (int j=0; j<4; j++) {
      for (int k=0; k<3; k++) {

	dir_tw->cd();
	sprintf (hstname, "dt_adc%02d_%s_%d", i, cpart[j], k);
	hdt_adc[i][j][k] = new TH2F(hstname, hstname, 100, 0., 1200., 100, -2., 5.);
	hdt_adc[i][j][k]->SetOption("colz");

	dir_veff->cd();
	sprintf (hstname, "dt_dist%02d_%s_%d", i, cpart[j], k);
	hdt_dist[i][j][k] = new TH2F(hstname, hstname, 100, -20., 80., 200, -20., 10.);
	hdt_dist[i][j][k]->SetOption("colz");

      }
    }
  }

  for (int i=0; i<24; i++) {
    char hstname[80];
    dir_gain->cd();
    sprintf (hstname, "adc_neg%02d", i);
    hadc_neg[i] = new TH2F (hstname, hstname, 120, -20., 80, 120, 0., 500);
    hadc_neg[i]->SetOption("colz");

    sprintf (hstname, "adc_corr%02d", i);
    hadc_corr[i] = new TH2F (hstname, hstname, 120, -20., 80, 120, 0., 500);
    hadc_corr[i]->SetOption("colz");

    sprintf (hstname, "adc_pos%02d", i);
    hadc_pos[i] = new TH2F (hstname, hstname, 120, -20., 80, 120, 0., 500);
    hadc_pos[i]->SetOption("colz");
  };

  if (gCalib) {
    dir_caldb->cd();
    TH1F* hcaldb_stn_veff_leg = 
      new TH1F("caldb_stn_veff_leg", "caldb_stn_veff_leg", 24, -0.5, 23.5); 
    TH1F* hcaldb_stn_veff_nose = 
      new TH1F("caldb_stn_veff_nose", "caldb_stn_veff_nose", 24, -0.5, 23.5); 
    TH1F* hcaldb_stn_veff_nose1 = 
      new TH1F("caldb_stn_veff_nose1", "caldb_stn_veff_nose1", 24, -0.5, 23.5); 
    TH1F* hcaldb_stn_veff_nose2 = 
      new TH1F("caldb_stn_veff_nose2", "caldb_stn_veff_nose2", 24, -0.5, 23.5); 
    TH1F* hcaldb_stn_delta_T_pd2pd = 
      new TH1F("caldb_stn_delta_T_pd2pd", "caldb_stn_delta_T_pd2pd", 24, -0.5, 23.5); 
    TH1F* hcaldb_stn_W0_value = 
      new TH1F("caldb_stn_W0_value", "caldb_stn_W0_value", 24, -0.5, 23.5); 
    TH1F* hcaldb_stn_W1_value = 
      new TH1F("caldb_stn_W1_value", "caldb_stn_W1_value", 24, -0.5, 23.5); 
    TH1F* hcaldb_stn_W2_value = 
      new TH1F("caldb_stn_W2_value", "caldb_stn_W2_value", 24, -0.5, 23.5); 

    for (int i=0; i<24; i++) {
      hcaldb_stn_veff_leg->SetBinContent(i+1, gCalib->GetFloat(stn_veff_leg, i));
      hcaldb_stn_veff_nose->SetBinContent(i+1, gCalib->GetFloat(stn_veff_nose, i));
      hcaldb_stn_veff_nose1->SetBinContent(i+1, gCalib->GetFloat(stn_veff_nose1, i));
      hcaldb_stn_veff_nose2->SetBinContent(i+1, gCalib->GetFloat(stn_veff_nose2, i));
      hcaldb_stn_delta_T_pd2pd->SetBinContent(i+1, gCalib->GetFloat(stn_delta_T_pd2pd, i));
      hcaldb_stn_W0_value->SetBinContent(i+1, gCalib->GetFloat(stn_W0_value, i));
      hcaldb_stn_W1_value->SetBinContent(i+1, gCalib->GetFloat(stn_W1_value, i));
      hcaldb_stn_W2_value->SetBinContent(i+1, gCalib->GetFloat(stn_W2_value, i));
    }
  }
    
  froot->cd();
  
  gStyle->SetPalette(1);                              // use rainbow colors for histogram


  if (xdisplay) {
    hreject_event->SetFillColor(5);
    hreject_event->Draw();
  }

}

///  construct vector from stream
class STvectorFromStream: public STvector {
  int eventNo;
public:
  STvectorFromStream(): STvector(), eventNo(0) {}
  STvectorFromStream(istream* input);
  int GetEventNo() { return eventNo; };
};

STvectorFromStream::STvectorFromStream(istream* input) :
  STvector(), eventNo(-1)
{
  /// unless istream defined: use BOS bank based constructor of STvector
  if (input == NULL)  return;
  if (! input->good()) return;

  /// clear vector and read from input
  for (int i=0; i<ST_MAX; i++) vstdata[i].clear();
  unsigned short int  bhead[6];
  unsigned short int* badc = NULL;
  unsigned short int* btdc = NULL;

  input->read((char*) bhead, sizeof(bhead));

  if (! input->good()) return;

  if (bhead[0] != 0xabcd) 
    throw "STvectorFromStream::STvectorFromStream: not a valid input file";
  int nrun = bhead[1];
  eventNo  = bhead[2] + ( ((int) bhead[3]) << 16) ;
  int ntdc = bhead[4];
  int nadc = bhead[5];

  if (ntdc) {
    btdc = new unsigned short int[ntdc*2];
    input->read((char*) btdc, sizeof(unsigned short int) * ntdc * 2);
  }

  if (nadc) {
    badc = new unsigned short int[nadc*2];
    input->read((char*) badc, sizeof(unsigned short int) * nadc * 2);
  }

  if (nadc && ntdc) {
    
    int stadc[ST_MAX];
    memset (stadc, 0, sizeof(stadc));    
    for (int i=0; i<nadc; i++) {
      int index = badc[i*2];
      if (! (index & 0x1000)) throw "STvectorFromStream::STvectorFromStream: invalid ADC index";
      index &= 0xFFF;
      index--;
      if (0 <= index && index < ST_MAX) 
	stadc[index] = badc[i*2+1];
      else
	throw "STvectorFromStream::STvectorFromStream: ADC index out of range";
    }
    
    for (int i=0; i<ntdc; i++) {
      int index = btdc[i*2];
      if ((index & 0x1000)) throw "STvectorFromStream::STvectorFromStream: invalid TDC index";
      index--;
      if (0 <= index && index < ST_MAX) 
	vstdata[index].push_back(STdata(index, stadc[index], btdc[i*2+1]));
      else
	throw "STvectorFromStream::STvectorFromStream: TDC index out of range";
    }
  }

  if (ntdc) delete[] btdc;
  if (nadc) delete[] badc;
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

void dump_str(vector<SThit>& myhit) {
  Jstre stre(1);
  cout.flags(ios::fixed);
  cout.precision(2);
  cout << "C++ " << myhit.size() <<  "    time      pos       path      track     status" << endl;
  for (vector<SThit>::iterator it = myhit.begin(); it != myhit.end(); it++) {
    cout << it->GetIndexDigits() / 10  << (it->IsNose() ? 2 : 1);
    cout.width(10);
    cout << it->GetHitTime();
    cout.width(10);
    cout << it->GetPosition();
    cout.width(10);
    cout << it->GetTrackLength();
    cout.width(10);
    cout << it->GetTrackIndex()+1;
    cout.width(10);
    cout << it->GetEnergyDeposit();
    cout.width(10);
    cout << it->GetGeoScale();
    cout.width(10);
    cout << it->STRstatus();
    cout << endl;
  }
  
  if (stre.Found()) {
    cout << "Fortran " << stre.GetNrows() 
	 << " ....................................." << endl;
    for (int istre = 0; istre < stre.GetNrows(); istre++) {
      cout << stre[istre].id;
      cout.width(10);
      cout << stre[istre].st_time;
      cout.width(10);
      cout << stre[istre].st_pos;
      cout.width(10);
      cout << stre[istre].st_l;
      cout.width(10);
      cout << stre[istre].trk_no;
      cout.width(10);
      cout << stre[istre].st_edep;
      cout.width(10);
      cout << stre[istre].st_corr;
      cout.width(10);
      cout << stre[istre].status;
      cout << endl;
    }
  }
  cout << "=============================================================\n";
}

int main (int argc, char* argv[]) {

  vector<string> v_bosfile;

  int eventCount = 0;     /// event count
  int eventMax   = 0;     /// process eventMax events, 0=unlimited 

  int iarg = 1;
  while (iarg<argc) {
    char* argptr = argv[iarg];
    if (*(argptr++) == '-') {
      switch (* (argptr++)) {
      case 'h':
	PrintUsage(argv[0]);
	break;
      case 'n':
	if (*argptr)            eventMax = atoi(argptr);
	else if (++iarg < argc) eventMax = atoi(argv[iarg]);
	else {
	  cerr << "Option -n given without max number\n" << endl;
	  PrintUsage(argv[0]);
	}
	break;
      case 'o':
	if (*argptr)            gRootOutFile = argptr;
	else if (++iarg < argc) gRootOutFile = argv[iarg];
	else {
	  cerr << "Option -o given without file name\n" << endl;
	  PrintUsage(argv[0]);
	}
	break;
      case 'r':
	if (*argptr)            gRawDirectory = argptr;
	else if (++iarg < argc) gRawDirectory = argv[iarg];
	else {
	  cerr << "Option -r given without directory name\n" << endl;
	  PrintUsage(argv[0]);
	}
	break;
      case 't':
	if (*argptr)            dt_tagcutS = atof(argptr);
	else if (++iarg < argc) dt_tagcutS = atof(argv[iarg]);
	else {
	  cerr << "Option -t given without number (time window in ns)\n" << endl;
	  PrintUsage(argv[0]);
	}
	break;
      case 'T':
	if (*argptr)            dt_tagcutM = atof(argptr);
	else if (++iarg < argc) dt_tagcutM = atof(argv[iarg]);
	else {
	  cerr << "Option -t given without number (time window in ns)\n" << endl;
	  PrintUsage(argv[0]);
	}
	break;
      case 'X':
	xdisplay = false;
	break;
      default:
	cerr << "Unknown Option <" << argv[iarg] << ">\n"<< endl;
	PrintUsage(argv[0]);
      }
    }
    else {
      v_bosfile.push_back(string(argv[iarg]));
    }
    iarg++;
  }

  if (!v_bosfile.size()) {
    cerr << "No input file specified \n" << endl;
    PrintUsage(argv[0]);
  }

  if (xdisplay) {
    theApp = new TRint("Interactive", 0, 0, 0, 0, 0 );  // activate interactive ROOT processor
    TCanvas *C = new TCanvas ("C", "C", 600, 600);      // open a new window to display histog    
  }
  
  initbos();                                          // initialize bos library

  st_set_def_();    //initialize st package

  signal (2, handle_break);
  signal (3, handle_break);


  // loop over file-list:  bosformat file1 file2 ... filen
  for (vector<string>::iterator itbos = v_bosfile.begin(); 
       itbos!=v_bosfile.end(); itbos++) {
    if (! fileloop) break;
    JBosFile bf ( itbos->c_str() ); // open file

    cout << "running " << *itbos << endl;
    int runNumber = -1;

    if (bf.good()) {       // file is good (e.g. existing, readable..)

      int runNo=0;
      int fileNo=0;
      int eventFile  = 0;
      tagcount   = 0;
      tdplcount  = 0;
      stn0count  = 0;
      stn1count  = 0;

      extractRunNumber(runNo, fileNo, *itbos);

      ifstream* ffraw=NULL;
      
      if (gRawDirectory) {
	char ifilename[80];
	sprintf (ifilename, "%s/stnraw_%05d_a%02d.dat", gRawDirectory, runNo, fileNo);
	ffraw = new ifstream(ifilename);
	if (!ffraw->good()) {
	  cerr << "can't open raw data file " << ifilename << endl;
	  exit (-1);
	}
      }

      bool loop = true;   
      while (fileloop && loop) {       // loop over events in file
 	JBosEvent be;      // get a single event
	
	// we check for be.good(). The variable loop stores the ...
	if ( ( loop = be.good()) ) { // ... result of this check

	  if (runNumber<=0) {
	    Jhead head;
	    runNumber =  head[0].nrun;
	    if (runNumber>0) {
	      cout << "runNumber = " << runNumber << endl;
	      gCalib = new JCalibration(runNumber);

	      if (! froot) {
		book_histo(runNumber);
	      }

	      st_brun_(&runNumber);
	      stpp_init_geometry();
	    }
	  }

	  try {
	    /// fill return code from analyze event in histogram, 0 if everything OK
	    hreject_event->Fill(analyze_event(ffraw));
	  }
	  catch (const char* e) {
	    cout << "exception <" << e << "> cought" << endl;
	    hreject_event->Fill(12);
	  }
	  catch (int e) {
	    cout << "exception " << e << " cought" << endl;
	    hreject_event->Fill(13);
	  }
	  catch (...) {
	    cout << "unknown exception cought" << endl;
	    hreject_event->Fill(14);
	  }
	  eventCount++;
	  eventFile++;

	  if ( !(eventCount %100)) {
	    cout.width(12);
	    cout << tagcount;
	    cout << " TAG";
	    cout.width(12);
	    cout << tdplcount;
	    cout << " TDPL";
	    cout.width(12);
	    cout << stn0count;
	    cout << " STN0";
	    cout.width(12);
	    cout << eventFile;
	    cout << " Events file";
	    cout.width(12);
	    cout << eventCount;
	    cout << " Events total";
	    cout << "\r";
	    cout.flush();
	    
	    if (xdisplay) {
	      gSystem->ProcessEvents();
	    }
	  }
	  // refresh screen all 1000 events
	  

	  if ( !(eventCount %1000)) {
	    if (xdisplay) {
	      gPad->Modified();
	      gPad->Update();
	    }
	  } 
	  if (eventMax && eventCount >= eventMax) {
	    loop=false;
	    fileloop = false;
	  } 
	}                  // end branch "event is good"
      }                    // end loop over events in file

      if (ffraw) { delete ffraw; ffraw = NULL; }
    }                      // end branch "file is good"
  }                        // end loop over file-list  bosformat file1 file2 ... filen
  
  final_plots();

  froot->Write();
  froot->Flush();

  if (xdisplay) {
    gPad->Modified();
    gPad->Update();

    theApp->Run();           // interactive ROOT started (stops when user enters .q)
  }

  return 0;
  
}

void final_plots() {
  
}

class DetectorResponse {
public:
  int index;
  double t0;
  double time;
  double path;
  double erg;
  DetectorResponse() : index(-1), t0(0), time(0), path(0), erg(0) {}
  bool operator< (const DetectorResponse dr) const {return dr.index < index ; }
};


/// pick the best photon for SC_t + particle hypothesis
class StartTime {
public:
  int hypothesis;
  int tagrIndex;
  double tpho;
  double t_sc;
  StartTime(int h, double t);
  bool good() { return tagrIndex >= 0; }
};

StartTime::StartTime(int h, double t) : 
  hypothesis(h), tagrIndex(-1), tpho(0), t_sc(t) {
  Jtagr tagr;
  if (!tagr.Found() || tagr.GetNrows() <= 0) return;
  double dtbest = 1.E9;
  for (int itagr = 0; itagr < tagr.GetNrows(); itagr++) {
    double dt = t_sc - tagr[itagr].tpho;
    if (fabs (dt) < fabs(dtbest)) {
      dtbest    = dt;
      tagrIndex = itagr;
      tpho      = tagr[itagr].tpho;
    }
  }
}

class MyPart {
public:
  TLorentzVector p;
  PartTrack trk;
  int charge;
  int id;
  int idcpb;
  int sector;
  double t_st_expect;
  vector<StartTime> vt0;
  vector<SThit> vsthit;
  DetectorResponse sc;
  DetectorResponse ec;
  DetectorResponse st;
  MyPart(TVector3 mom, TVector3 vert, double mass, int q);
  double DVertZ();
  double CalculateT0 ( DetectorResponse* det);
  void ReCalculate ();
  friend ostream& operator<< (ostream&, const MyPart);
};

MyPart::MyPart(TVector3 mom, TVector3 vert, double mass, int q) :
  trk(vert, mom), charge(q), id(0), idcpb(0), sector(0) {
  p.SetVectM(mom, mass);
}

double MyPart::DVertZ() {
  if (!gCalib) throw "calibration not initialized";
  return trk.VertZ() - gCalib->GetFloat(geo_target_position,2);
}

double MyPart::CalculateT0 ( DetectorResponse* det) {
  return  det->time - det->path / p.Beta() / c_light - DVertZ() / c_light; 
}

void MyPart::ReCalculate() {
  if (sc.index >= 0) sc.t0 = CalculateT0 (&sc);
  if (st.index >= 0) st.t0 = CalculateT0 (&st);
}
    
ostream& operator<< (ostream& os, const MyPart mp) {
  os.width(5);
  os << mp.id;
  os.width(3);
  os << mp.charge;
  os.flags(ios::fixed);
  os.precision(4);
  os.width(8);
  os << mp.p.X();
  os.width(8);
  os << mp.p.Y();
  os.width(8);
  os << mp.p.Z();
  os.width(8);
  os << mp.p.E();
  return os;
}


typedef vector<StartTime>::iterator startiter_t; 
typedef vector<SThit>::iterator     hititer_t;
typedef vector<MyPart>::iterator    partiter_t;

/// TVector3 to JVector3, this is really nasty and only needed to maintain a ROOT free cooking code
class T2JVector3: public JVector3 {
public:
  T2JVector3(const TVector3& v) { x=v.X(); y=v.Y(); z=v.Z(); }
};

int last[2];

/// histogram: ST data available for segment hit 
void fill_match_histo(const STvector& data, const STposition& pos) {
  int pos2index = pos.GetSector() * 8 + pos.GetSegment8();
  for (int i=0; i<24; i++) {
    hhits4segment->Fill(pos2index, data.size(i));
  }
}


int analyze_event(istream* rawStream) {

  /// access to BOS banks
  Jstn0 stn0;
  Jstn1 stn1;
  Jhead head;

  Jevnt evnt;
  Jscpb scpb;
  Jdcpb dcpb;
  Jecpb ecpb;
  Jstpb stpb;
  Jtbtr tbtr;

  if (!head.Found()) return 1;

  if (remakeST) {
    dropAllBanks(&bcs_, "STR ST1 STH ");
    int stlevel = 1;
    st_bevt_();
    st_evnt_(&stlevel);
  }

  Jstr  str(1);
  Jst1  st1;

  /// STR / TBTR ratio
  if (str.Found()) {
    hmultstr->Fill(str.GetNrows());
    if (tbtr.Found() && tbtr.GetNrows() > 0) {
      hstr_tbtr->Fill ((double) str.GetNrows() / (double) tbtr.GetNrows() );
    }
  }

  /// count events which have STN0/TDPL/TAGR BOS banks

  /// STN0
  if (stn0.Found()) {
    hmult_stn0->Fill(stn0.GetNrows());
    stn0count++;
  }

  /// STN1
  if (stn1.Found()) {
    hmult_stn1->Fill(stn1.GetNrows());
    stn1count++;
  }

  /// TDPL
  for (int i=1; i<=6; i++) {
    Jtdpl tdpl(i);
    if (tdpl.Found()) {
      tdplcount++;
      break;
    }
  }

  /// TAGR
  Jtagr tagr;
  if (tagr.Found()) {
    hmult_tagr->Fill(tagr.GetNrows());
    tagcount++;
  }
  else {
    return 2;
  }

  int search_event = head[0].nevent;

  STvectorFromStream data(rawStream);
  int adjust = 0;
  while (rawStream && data.GetEventNo() >= 0 && data.GetEventNo() < search_event) {
    data = STvectorFromStream(rawStream);
    adjust++;
  }
  if (adjust > 2000) cerr << "Steps of adjustment: " << adjust << endl;

  if (rawStream && data.GetEventNo() < 0) {
    cerr << "error reading input stream, event " << search_event << " " << data.GetEventNo() << endl;
    return 3;
  }

  vector<MyPart> mypart;

  if (rawStream && search_event != data.GetEventNo()) {
    cerr << endl;
    cerr << "Different eventnumber " << search_event << " and " << data.GetEventNo() << endl;
    cerr << " last event was " << last[0] << " and " << last[1] << endl;
    cerr << "....................................................." << endl;
    return 4;
  }
  hmultdata->Fill(data.size());

  last[0] = search_event;
  last[1] = data.GetEventNo();

  // loop over all TB particles in EVNT bank, assume pion
  if (evnt.Found()) {
    for (int i=0; i<evnt.GetNrows(); i++) {
      if (evnt[i].charge && evnt[i].status > 0) {
	TVector3 unit(evnt[i].dir_cos.x,
		      evnt[i].dir_cos.y,
		      evnt[i].dir_cos.z);
	TVector3 vert(evnt[i].vert.x,
		      evnt[i].vert.y,
		      evnt[i].vert.z);
	MyPart mp(evnt[i].pmom*unit, vert, PIONM, evnt[i].charge);

	int sectorSCPB = 0;
	int sectorSTPB = 0;
	int sectorDCPB = 0;
	int sectorECPB = 0;

	/// time of flight match from SCPB bank
	int iscpb = evnt[i].scstat - 1;
	if (scpb.Found() && iscpb >= 0) {
	  hinfo_available->Fill(39);
	  sectorSCPB = scpb[iscpb].scpdht / 10000;
	  int stripe = (scpb[iscpb].scpdht % 10000) / 100; 
	  double sc_index = (sectorSCPB-1) * 48 + stripe - 1;
	  double sc_edep = scpb[iscpb].edep;
	  double sc_time = scpb[iscpb].time;
	  double sc_path = scpb[iscpb].path;
	  bool sc_good = true;

	  mp.sc.index = (sectorSCPB-1) * 48 + stripe - 1;
	  mp.sc.time  = sc_time;
	  mp.sc.path  = sc_path;
	  mp.sc.erg   = sc_edep;
	  mp.sc.t0    =  mp.CalculateT0( & mp.sc );
	  if (mp.charge > 0) {
	    hpose->Fill(mp.p.P(), sc_edep);
	  }
	  else {
	    hnege->Fill(mp.p.P(), sc_edep);
	  }
	}
	else {
	  hinfo_available->Fill(38);
	}

	/// start counter match from STPB bank
	int istpb = evnt[i].ststat -1;
	if (stpb.Found() && istpb >= 0) {
	  hinfo_available->Fill(44);
	  sectorSTPB = stpb[istpb].sthid / 100;
	  int stripe = (stpb[istpb].sthid % 100) / 10;
	  int st_index = (sectorSTPB-1) * 4 + stripe - 1;
	  double st_time = stpb[istpb].time;
	  double st_path = stpb[istpb].path;

	  mp.st.index = st_index;
	  mp.st.time  = st_time;
	  mp.st.path  = st_path;
	  mp.st.erg   = 0.;
	  mp.st.t0    = mp.CalculateT0( & mp.st );
	}
	else {
	  hinfo_available->Fill(43);
	}

	/// drift chamber match from DCPB bank
	int idcpb = evnt[i].dcstat - 1;
	if (dcpb.Found() && idcpb >= 0) {
	  mp.idcpb = idcpb;
	  sectorDCPB = dcpb[idcpb].sctr / 100;
	  mp.sector = sectorDCPB;
	  hinfo_available->Fill(34);
	}
	else {
	  mp.idcpb = -1;
	  hinfo_available->Fill(33);
	}

	/// forward calorimeter match from ECPB bank
	int iecpb = evnt[i].ecstat - 1;
	if (ecpb.Found() && iecpb >= 0) {
	  sectorECPB = ecpb[iecpb].scht / 100;
	  hinfo_available->Fill(29);
	}
	else {
	  hinfo_available->Fill(28);
	}

	if ( (sectorSTPB  && sectorSCPB && sectorSTPB != sectorSCPB) ||
	     (sectorDCPB  && sectorSCPB && sectorDCPB != sectorSCPB) ||
	     (sectorECPB  && sectorSCPB && sectorECPB != sectorSCPB)
	     ) {
	  hsectormissmatch->Fill(unit.Phi() * DEG);
	}

	if (stn0.Found() && stn1.Found()) hinfo_available->Fill(22);
	else if (stn1.Found())            hinfo_available->Fill(21);
	else if (stn0.Found())            hinfo_available->Fill(20);
	else                              hinfo_available->Fill(19);
	

	mypart.push_back(mp);
      }                   // if (charged && status >0)
    }                     // loop  EVNT bank
  }                       // if (evnt.Found)

  for (partiter_t itx = mypart.begin(); itx != mypart.end(); itx++) {
    /// pion hypothesis
    StartTime t0pion(pion_id, itx->sc.t0);
    if (t0pion.good()) {
      ht0_phypothesis[0]->Fill(t0pion.t_sc - t0pion.tpho);
      itx->vt0.push_back(t0pion);
    }

    /// kaon hypothesis
    MyPart kaon = *itx;
    kaon.p.SetVectM(kaon.p.Vect(), KAONM);
    StartTime t0kaon(kaon_id, kaon.CalculateT0( &kaon.sc ));
    if (t0kaon.good()) {
      ht0_phypothesis[1]->Fill(t0kaon.t_sc - t0kaon.tpho);
      itx->vt0.push_back(t0kaon);
    }

    if (itx->charge > 0) {
      /// proton hypothesis
      MyPart prot = *itx;
      prot.p.SetVectM(prot.p.Vect(), PROTM);
      StartTime t0prot(prot_id, prot.CalculateT0( &prot.sc ));
      if (t0prot.good()) {
	ht0_phypothesis[2]->Fill(t0prot.t_sc - t0prot.tpho);
	itx->vt0.push_back(t0prot);
      }
      
      /// deuteron hypthesis
      MyPart deut = *itx;
      deut.p.SetVectM(deut.p.Vect(), DEUTM);
      StartTime t0deut(deut_id, deut.CalculateT0( &deut.sc ));
      if (t0deut.good()) {
	ht0_phypothesis[3]->Fill(t0deut.t_sc - t0deut.tpho);
	itx->vt0.push_back(t0deut);
      }
    }
  }      

  
  double bestdiff = 1.E9;
  startiter_t bestA;
  startiter_t bestB;

  bool starttime_found = false;

  hmypart->Fill(mypart.size());

  switch (mypart.size()) {

    /// nothing to do without charged tracks
  case 0:
    return 5;
    
    /// one charged track: which hypotheses matches best with tagger ?
  case 1:
    for (startiter_t itA = mypart[0].vt0.begin(); itA != mypart[0].vt0.end(); itA++) {
      double dt = fabs (itA->t_sc - itA->tpho);
      if (dt < bestdiff) {
	bestdiff = dt;
	ht0_tdiff_1part_tag->Fill(dt);
	bestA = itA;
	starttime_found = true;
      }
    }    
    break;

    /// two or more charged tracks: which pair of two is a good match
  default:
    for (partiter_t iterA = mypart.begin()+1; iterA != mypart.end(); iterA++) {
      for (startiter_t itA = iterA->vt0.begin(); itA != iterA->vt0.end(); itA++) {
	if (fabs (itA->t_sc - itA->tpho) < dt_tagcutM) {
	  for (partiter_t iterB = mypart.begin(); iterB != iterA; iterB++) {
	    for (startiter_t itB = iterB->vt0.begin(); itB != iterB->vt0.end(); itB++) {
	      if (fabs (itB->t_sc - itB->tpho) < 2.) {
		double dt = fabs(itB->t_sc - itA->t_sc);
		ht0_tdiff_2part->Fill(dt);
		ht0_tdiff_2part->Fill(-dt);
		if (dt < bestdiff) {
		  bestdiff = dt;
		  bestA = itA;
		  bestB = itB;
		  starttime_found = true;;
		}
	      }
	    }
	  }
	}
      }
    } 
  }

  if (!starttime_found) return 6;
  StartTime starttime = *bestA;

  switch (mypart.size()) {

    /// no tracks at this point should never happen due to return in previous statement
  case 0:
    return 5;

    /// one track: start time match with tagged photon good enough
  case 1:
    ht0_tbest_1part[bestA->hypothesis-20]->Fill(bestdiff);
    if (fabs(bestdiff) > dt_tagcutS) return 7;
    break;

    /// two or more tracks:
  case 2:
    ht0_tbest_mpart[bestA->hypothesis-20]->Fill(bestdiff);
    ht0_tbest_mpart[bestA->hypothesis-20]->Fill(-bestdiff);
    ht0_tbest_mpart[bestB->hypothesis-20]->Fill(bestdiff);
    ht0_tbest_mpart[bestB->hypothesis-20]->Fill(-bestdiff);
    
    starttime = *bestA;

    /// photon ambiguity
    if (bestA->tagrIndex != bestB->tagrIndex) {
      double average = (bestA->t_sc + bestB->t_sc) / 2.;
      starttime = StartTime(0, average);
      double dt = average - starttime.tpho;
      hdt_average->Fill(dt);
      if (fabs(dt) > dt_tagcutM) return 8;
    }
  }

  for (partiter_t mpi = mypart.begin(); mpi != mypart.end(); mpi++) {
    double tau = (mpi->sc.time - starttime.tpho - 
		  mpi->DVertZ()/c_light) / mpi->sc.path * c_light;
    double massq = mpi->p.Vect().Mag2() * (SQR(tau) - 1.);

    /// positive
    if (mpi->charge > 0) {
      htau_pos->Fill(mpi->p.P(), tau);
      hmassq_pos[0]->Fill(massq);
      hmassq_pos[1]->Fill(massq);
      if      (massq < 0.17) mpi->id = pion_id;
      else if (massq < 0.32) mpi->id = kaon_id;
      else if (massq < 3.  ) mpi->id = prot_id;
      else                   mpi->id = deut_id;
    }
    /// negative
    else {
      htau_neg->Fill(mpi->p.P(), tau);
      hmassq_neg[0]->Fill(massq);
      hmassq_neg[1]->Fill(massq);
      if (massq > 0.17 && massq < 0.32) mpi->id = kaon_id;
      else mpi->id = pion_id;
    }


    switch (mpi->id) {
    pion_id:
      break;
    kaon_id:
      mpi->p.SetVectM(mpi->p.Vect(), KAONM);
      mpi->ReCalculate();
      break;
    prot_id:
      mpi->p.SetVectM(mpi->p.Vect(), PROTM);
      mpi->ReCalculate();
      break;
    deut_id:
      mpi->p.SetVectM(mpi->p.Vect(), DEUTM);
      mpi->ReCalculate();
      break;
    }
  }

  
  /// ST++ reconstruction as in cooking code
  /// ... except we use a different constructor for STposition
  for (partiter_t mpi = mypart.begin(); mpi != mypart.end(); mpi++) {    

    if (mpi->idcpb >= 0) {                     /// particle with DCPB

      //      STposition pos(mpi->idcpb);

      STposition pos(mpi->sector,              /// construct ST plane
		     T2JVector3(mpi->trk.Vertex()), 
		     T2JVector3(mpi->trk.Direction()));

      if (pos.Found()) {                          /// plane good

	mpi->t_st_expect = starttime.tpho              /// time when particle hits ST
	  + pos.GetTrackLength() / mpi->p.Beta() / c_light 
	  + mpi->DVertZ() / c_light; 

	fill_match_histo(data, pos);             /// histogram: ST data available for segment hit 

	                                         /// search best ST hit
	int inx = data.BestSegment(pos.GetSector(), pos.GetSegment8() ); 

	if (inx >= 0 ) {                         /// found a hit (several?)

	                                           /// loop over hits
	  for (dataiter_t i=data.begin(inx); i!= data.end(inx); i++) {

	    mpi->vsthit.push_back(SThit(*i, pos));    /// store hit in container
	  }
	}
	hinfo_available->Fill(mpi->vsthit.size());
      }
      else {
	hinfo_available->Fill(-1);
      }
    }
  }


  /// count number of ST hits matched by particle track
  int countSTdata = 0;
  for (vector<MyPart>::iterator mpi = mypart.begin(); mpi != mypart.end(); mpi++) {
    countSTdata += mpi->vsthit.size();
  }


  if (countSTdata) {
    hinfo_available->Fill(49);
  }
  else {
    hinfo_available->Fill(48);
    return 9;
  }

  for (vector<MyPart>::iterator mpi = mypart.begin(); /// loop particles
     mpi != mypart.end(); mpi++) {

    if (mpi->vsthit.size()) {
      for (hititer_t ht=mpi->vsthit.begin(); ht!=mpi->vsthit.end(); ht++) {
	int pid    = mpi->id - pion_id;   /// 0=pion  1=kaon  2=prot   3=deut
	int ii     = ht->GetIndex();
	double dt  = ht->GetHitTime() - mpi->t_st_expect;
	double pos = ht->GetPosition();
	double t_to_st = mpi->t_st_expect - starttime.tpho;

	hxz_stplane[ht->GoodPlane()]->Fill(ht->Z(), ht->X());

	double phideg = atan2(ht->Y(), ht->X()) * DEG + 30.;
	hvieh_deg->Fill(phideg);
	hvieh_stplane->Fill(ii, phideg); 

	hsttime[pid]        ->Fill(dt);
	hsttag[pid]         ->Fill(ii, dt);
	hpartdedx[ii][pid]  ->Fill(mpi->p.P(), ht->GetEnergyDeposit() * ht->GetGeoScale()); 
	hdt_adc[ii][pid][0] ->Fill(ht->GetADC(), dt+ht->GetTimewalk());
	hdt_adc[ii][pid][1] ->Fill(ht->GetADC(), dt);
	hdt_dist[ii][pid][0]->Fill(pos, ht->GetTime() - mpi->t_st_expect);
	hdt_dist[ii][pid][1]->Fill(pos, dt);

	/// ToF entry
	if (mpi->sc.index >= 0) {
	  double dtsc = ht->GetTime() - t_to_st - mpi->sc.t0;
	  hstsc[pid]          ->Fill(ii, dtsc);
	  hdt_adc[ii][pid][2] ->Fill(ht->GetADC(), dtsc);
	  hdt_dist[ii][pid][2]->Fill(pos, dtsc);
	  hsctag[pid]         ->Fill(mpi->sc.index, mpi->sc.t0 - starttime.tpho);
	  htagsc[pid]         ->Fill(tagr[starttime.tagrIndex].t_id-1., 
				     mpi->sc.t0 - starttime.tpho);
	}

	/// hits in the nose 
	if (ht->IsNose()) {
	  hdt_dist_nose[ii] ->Fill(pos, dt);
	}

	if (!pid) {
	  if (mpi->charge < 0) 
	    hadc_neg[ii]    ->Fill(pos, ht->GetADC() * ht->GetGeoScale());
	  else
	    hadc_pos[ii]    ->Fill(pos, ht->GetADC() * ht->GetGeoScale());
	  hadc_corr[ii]     ->Fill(pos, ht->GetEnergyDeposit() * ht->GetGeoScale());
	}

	/// compare time from STPB bank with time ST++ package
	if (mpi->st.index >= 0) {
	  hcompare_stpb[ii] ->Fill(pos, ht->GetHitTime() - mpi->st.time);
	}
      }    /// loop ST hits
    }      /// if ST hits
  }        /// loop particle

  return 0;
}          

