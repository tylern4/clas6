//Class authored by Hovanes Egiyan, updated and added to clas packages by Paul Mattione
//
// This class is designed to help p2p calibrations with photon 
// beam using start time from SEB. The start time is the RF corrected 
// time from tagger propagated to the event vertex. Typically the more 
// tracks there are in the event, the better start time determination is 
// done in SEB.
//
//


#ifndef _TPhotTiming_
#define _TPhotTiming_

#include <iostream>
#include <map>
#include <cmath>
using namespace std;

#include "TObject.h"
#include "TList.h"
#include "TClonesArray.h"
#include "TH1.h"
#include "TH2.h"
#include "TF1.h"
#include "TChain.h"
#include "TFile.h"
#include "TString.h"
#include "TGraphErrors.h"
#include "TSystem.h"
#include "TStyle.h"
#include "TPad.h"
#include "TCanvas.h"
#include "TLine.h"
#include "TDatabasePDG.h"

#include "THEADERClass.h"
#include "TEVNTClass.h"
#include "TTAGRClass.h"
#include "TDCPBClass.h"
#include "TSCPBClass.h"
#include "TSTPBClass.h"
#include "TECPBClass.h"
#include "TCCPBClass.h"
#include "TDSTReader.h"
#include "TClasTool.h"
#include "TMapUtils.h"

#define N_SEC  (6)           // Number of sectors in CLAS
#define N_STRP_MAX (57)          // Number of TOF paddles in each sector
#define N_TC   (121)          // Number of tagger t-counters to look at

typedef Double_t (*DblArrPtr)[N_STRP_MAX]  ;  // define a pointer to an array of floats
typedef Int_t (*IntArrPtr)[N_STRP_MAX]  ;     // define a pointer to an array of integers

class TPhotTiming {

 private:
  Int_t    kSC_VERSION_FLAG;
  Int_t    kN_STRP;

  Int_t    kReadOnly;                  // Flag to define if this object is read from a file 
  Int_t    kObjNum;                    // Number the objects in the memory
  Int_t    kFitDone;                   // Flg toindicate if fit is done or not
  Int_t    kRunNum;                    // Run Number being analized and calibrated

  TString  fRunIndex;                  // Name of the RunIndex (ie calib.RunIndex)
  TString  fOutFileName;               // Name of the ROOT file where the histos will be saved
  TString  fTxtFileName;               // Name of the text file where the delays willbe written
  TFile*   fOutFile;                   // ROOT file where the histos reside
  TH2F*    h_dt_vs_pd[N_SEC];          // 2d histos for dt vs paddle number
  TH1F*    h_dt[N_SEC][N_STRP_MAX];        // 1d histos for dt   
  TH2F*    h_rf_vs_pd;                 // 2D histo for RF versus T-counter id
  TH1F*    h_rf;                       // 1D histo for RF offset in the tagger
  TH2F*    h_tof_tag_tc;               // 2D histo to show time differences between TOF and Tagger
  TH1F*    h_tof_tag[N_TC];            // 1D histos to show time differences between TOF and Tagger
  TH1F*    h_com_offset;               // 1d histogram for all TOF counters 
  TH1F*    h_tg[N_TC];                 // 1d histogram to T-counters

  Double_t fDelay[2][N_SEC][N_STRP_MAX];   // Delay and error values from fit [0] - values, [1] - error
  Double_t fWidth[2][N_SEC][N_STRP_MAX];   // Width and error values from fit [0] - values, [1] - error
  Double_t fMapDelay[N_SEC][N_STRP_MAX];   // Delays already in CalDB
  Double_t fDelay4Map[N_SEC][N_STRP_MAX];  // Delays calculated for entry  into CalDB 
  Int_t    kBadChan[N_SEC][N_STRP_MAX] ;   // Bad channel flags

  Double_t fTagDelay[2][N_TC];   // Delay and error values from fit [0] - values, [1] - error
  Double_t fTagWidth[2][N_TC];   // Width and error values from fit [0] - values, [1] - error
  Double_t fTagMapDelay[N_TC];   // Delays already in CalDB
  Double_t fTagDelay4Map[N_TC];  // Delays calculated for entry  into CalDB 
  Int_t    kTagBadChan[N_TC] ;   // Bad channel flags

  Double_t fRfOffset;                  // RF Offset in tagger
  Double_t fRfWidth;                   // Width of the RF offset plot
  Double_t fTOFOffset;                 // Overall offset of TOF system wrt event start time
  Double_t fTOFWidth;                  // Width of the overall offset plot

  TGraphErrors*  gDelay[N_SEC];        // Graph to show the results of the fit for TOF paddles
  TGraphErrors*  gTagDelay;            // Graph to show the results of the fit for tagger paddles

  TDSTReader fReader;                  // Reader inteface for ClasTool 
  TMapUtils  fCalDB;                   // CalDB interface 

  void   BookHistos();                 // Function which books the histos
  Int_t  ReadHistos();                 // Function which reads the histos when constructed from file
  void   InitConstants();              // Initilize constants
  void   InitCalDB();                  // Initilize CalDB interface
  Int_t  PionDeDxCut( Double_t P, Double_t E ); // dE/dX cut to ID pions w/o timing
  Int_t  GetMapValues( const Char_t* opt = "tagtof");               // Obtain calibration constants from CalDB
  Int_t  GetNewMapConstants( const Char_t* opt = "tagtof" );         // Calculates  new p2p constants

 public:
  static Char_t* DBHost;               // MySQL server name for CALDB
  static Char_t* UserName;             // user name for MySQL 
  static Char_t* DataBase;             // Database name for CalDB

  static TDatabasePDG DbPDG;           // PDG database for accessing data on particles

  static TString PionIdMethod;         // Method to be used for Pion ID ( "time" or/and "energy" )

  static Double_t cLight;              // Speed of light in cm/ns
  static Double_t RfConst;             // RF time period 
  static Double_t ConstShift;          // Positive constant shift  to avoid negative numbers
  static Int_t kObjectNumber;          // Number of this object
  static Int_t MinTrkNum;              // Minimum track number for the event to be used 

  TPhotTiming( const Char_t* FileName, const Char_t* OutFileName = "photon_timing.root", 
	       const Char_t* runIndex ="calib.RunIndex", int runno = -1);
  TPhotTiming( TFile* OutFile,  const Char_t* runIndex = "calib.RunIndex", int runno = -1);
  ~TPhotTiming();

  Int_t  Process( Long_t nEvts = -999 );      // Loop through th events and fill histos
  void   Plot2D( Double_t minY = -5, Double_t maxY = +5 );  // Plot 2D histos for TOF
  void   Plot1D( Int_t iSec = 0 );                     // Plot 1D histos for TOF
  void   PlotTagger();                        // Plot 1D and 2D histos for tagger
  void   PlotTagger1D();                      // Plot 1D histos for tagger
  void   PlotTagger2D( Double_t minY = -6, Double_t maxY = +6 );       // Plot 2D histos for tagger
  Int_t  Fit( Int_t Sec = 0, Int_t Pd = 0 );  // Performs fit for the tagger and TOF paddles 
  Int_t  FitTagger();                         // Perform tagger fit
  Int_t  CloseFile();                         // Saves the histos and closes the ROOT file
  Int_t  WriteConstants( const Char_t* FileName = "tof_timing.dat", 
			 const Char_t* FileName = "tagger_timing.dat"   );  // Write p2p constatnts into file
  Int_t  WriteTOFConstants( const Char_t* FileName = "tof_timing.dat" );    // Write TOF p2p constatnt into file
  Int_t  WriteTagConstants( const Char_t* FileName = "tagger_timing.dat" ); // Write TAG p2p constatnt into file
  

  // Inline functions are below

  inline void       Add( const Char_t* fileName ) { fReader.Add( fileName ) ; }
  inline void       SetTxtFileName( const Char_t* FileName ) { fTxtFileName = FileName ; }
  inline void       SetOutFileName( const Char_t* FileName ) { fOutFileName = FileName ; }
  inline void       SetRunNumber( const Int_t RunNum ) { kRunNum = RunNum ; }

  inline TGraphErrors* GetDelGraph( Int_t Sec ) const { return gDelay[Sec-1] ; }

  inline TMapUtils* GetCalDB() { return &fCalDB; }

  inline Int_t      GetObjNum() const { return kObjNum; } 
  inline Int_t      GetRunNum() const { return kRunNum; }
  inline TString    GetRunIndex() { return fRunIndex ; }
  inline TString    GetTxtFileName()  { return fTxtFileName; } 
  inline TString    GetOutFileName()  { return fOutFileName; } 
  inline Double_t   GetRfOffset() const { return fRfOffset; }
  inline Double_t   GetRfWidth() const { return fRfWidth; }
  inline Double_t   GetTOFOffset() const { return fTOFOffset; }
  inline Double_t   GetTOFWidth() const { return fTOFWidth; }
  inline DblArrPtr  GetDelays( )  { return  fDelay[0]; }
  inline Double_t   GetDelay( Int_t Sec, Int_t Pd ) const {
    return  fDelay[0][Sec-1][Pd-1] ; }
  inline DblArrPtr  GetDelErrs()  { return fDelay[1]; }
  inline Double_t   GetDelErr( Int_t Sec, Int_t Pd ) const {
    return  fDelay[1][Sec-1][Pd-1] ; }
  inline DblArrPtr  GetWidths( )  { return fWidth[0]; }
  inline Double_t   GetWidth( Int_t Sec, Int_t Pd ) const {
    return  fWidth[0][Sec-1][Pd-1] ; }
  inline DblArrPtr  GetMapDelays()  { return fMapDelay; }
  inline Double_t   GetMapDelay( Int_t Sec, Int_t Pd)  const { 
    return fMapDelay[Sec-1][Pd-1] ; }
  inline DblArrPtr  GetDelays4Map() { return fDelay4Map; }
  inline Double_t   GetDelay4Map( Int_t Sec, Int_t Pd ) const { 
    return  fDelay4Map[Sec-1][Pd-1] ; }
  inline IntArrPtr  GetBadChans()  { return kBadChan; }
  inline Int_t      GetChanStat( Int_t Sec, Int_t Pd ) const { 
    return kBadChan[Sec-1][Pd-1] ; }
};


#endif
