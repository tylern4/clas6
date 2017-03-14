#ifndef __TTofCalib__
#define __TTofCalib__


#include "c_stds.h"
#include "RootF.h"
#include "RunControl.h" 
#include "TReaction.h"
#include "call.h"
#include "NumStrips.h"

const float c_light = 29.9792458 ;	// Speed of Light
const float RF_Period = 2.004 ;		// RF period in ns
const float CalConst_RF = 1. ;		// Correction constsnt for RF TDC 
const float CalConst_TOF = 1. ;		// Correction constant for TOF TDCs
const float Sigma_el = 0.200 ;		// Sigma for el timing, ns
const float Sigma_pi = 0.250 ;		// Sigma for pion timing
//      float n_strips_for_el  ;          // Number of strips starting from #1 to be
//      float BeamEnergy       ;
const int   n_bins = 100 ;		// Number of bins in Histograms

class TTofCalib : public TReaction	// t0 calibration class
{

  friend class TFineTune;
  friend class TCrudeTune;
 
public:
  	
  TTofCalib();	
  ~TTofCalib();			// Calculates Constants
  void		Process( physEVNT_c* EVENT );	// Fill Histos etc.
  		
  	 
private:
 	
  void		SaveConstants( float Const[][N_MAX_STRIPS], const char* filename );			      
 
  	
  ///-------------------------- class TFineTune -------------------------------------
  class   TFineTune {		// Class for fine tuning with RF
    
  public:
    TFineTune();
    ~TFineTune();

    float		GetT0( int iSect, int iStrip );
    float		GetErrT0( int iSect, int iStrip );
    float		GetSigma( int iSect, int iStrip );
    float		GetErrSigma( int iSect, int iStrip );
    int 		GetStatus( int iSect, int iStrip );
    void		Process();			// Fill Histos etc.
    void		Do();				// Do the Calibration
    int		        Done();
    void		Write();			// Write out Histos
    
  private:
    
    int 		DoneFlag;
    
    struct Strip_t		// Class containing info for a sc. strip.
    {
      
      Strip_t( int Sect, int Strip );
      ~Strip_t();
      int 		Rearrange();
      void		SetT0( float t );
      void		SetErrT0( float err );
      void		SetSigma( float sig );
      void		SetErrSigma( float err );
      float		GetT0();
      float		GetErrT0();
      float		GetSigma();
      float		GetErrSigma();
      int		IsOK();
      void		SetBad();
      void		SetGood();
      
      int 		iSect;
      int 		iStrip;
      float 		t0;
      float           err_t0 ;
      float 		sigma;
      float           err_sigma ;
      int		OKstatus;
      TH1F* 		hist;
    } ;
  	  
    Strip_t* 	Strip[N_SECTS][N_MAX_STRIPS] ;	// Array of pointers 
  	  	 						// to SC Strips in CLAS
    TH1F*     histBankAvail;
  	  
  }; 
  	
  TFineTune* 	FineTune;		// Pointer of Fine Tuning Class
  ///---------------------- end class TFineTune -------------------------------------
  	
  class   TCrudeTune {		// Class for Crude tuning
  public:
    TCrudeTune();
    ~TCrudeTune();
    int		GetNBunches( int iSect, int iStrip );
    void		Process();			// Fill Histos etc.
    void		Do( float TuneConst[][N_MAX_STRIPS] );	// Do the Calibration
    int		Done();
    void		Write();			// Write out Histos
    
  private:
    
    void 		DoFirst10( float TuneConst[][N_MAX_STRIPS] );
    void		DoSect2Sect( float TuneConst[][N_MAX_STRIPS] );
    void		DoLast38( float TuneConst[][N_MAX_STRIPS] );
    void 		DoFirst10Again( float TuneConst[][N_MAX_STRIPS] );
    int 		PiIsOpposite( int iSect_e, int iStrip_e, 
		                              int iSect_pi, int iStrip_pi )  ;
    
    int 		DoneFlag;
    
    struct Strip_t		// Class containing info for a sc. strip.
    {
      
      Strip_t( int Sect, int Strip );
      ~Strip_t();
  	   		float		GetMax();		
      void		SetNBunches( int n_bunches );
      int		GetNBunches();
      
      int 		iSect;
      int 		iStrip;
      int 		n_bunch;
      TH1F* 		hist;
    } ;
    
    Strip_t* 	Strip[N_SECTS][N_MAX_STRIPS] ;	// Array of pointers 
    // to SC Strips in CLAS
    struct Sector_t	// Class containing info for a sect. of strip.
    {
      
      Sector_t( int Sect );
      ~Sector_t();
      float		GetMax();		
      void		SetNBunches( int n_bunches );
      int		GetNBunches();
      
      int 		iSect;
      int 		n_bunch;
      TH1F* 		hist;
    } ;
    
    Sector_t* 	Sector[N_SECTS];
    
    TNtuple* 	NTuple;					
    TH1F*       histBankAvail;
  	  
  };
  	
  TCrudeTune*	CrudeTune; 		// Pointer of Crude Tuning Class
  ///---------------------- end class TCrudeTune -------------------------------------
  
  
  class 	TCalibTest        {
  public:
    TCalibTest();
    ~TCalibTest();
    void		Process();			// Fill Ntuple etc.
    void		Do( float Const[][N_MAX_STRIPS],
			    int Status[][N_MAX_STRIPS] );	// Do test
    
  private:
    
    
    TH2F*	        fRF_Test ;
    TH2F*	        fRF_Test_sngl ;
    TH1F*          PhotStat ;
    TH2F* 		M2vsStrip[2][N_SECTS];
    TH2F*		BetavsP[2];
    TH1F*		M_Spec[2];  	   	  	 
    
    TNtuple* 	NTuple;					 
    TH1F*       histBankAvail;
   
  };
  
  
  TCalibTest* 	CalibTest;
  ///---------------------- end class TCalibTest  -------------------------------------
  
  TFile* 		RootFile;		// Pointer to the root file 	
  TDirectory*	FineDir;		// Pointers to the Fine and
  TDirectory*	CrudeDir;		// Crude Tune Directories
  TDirectory* 	TestDir;		// Test Directory
  
  TH1F*           ConstHisto[N_SECTS] ;   // Histo for constants
  TH1F*           SigmaHisto[N_SECTS] ;   // Histo for sigmas
};

int 	PionDeDxCut( float P, float E );	// Pion DE_DX Cut
int	RoundOff( float x );	

#endif

