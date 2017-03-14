#ifndef TPECal_CALIB_H
#define TPECal_CALIB_H

// %%%%%%%%%%%%%
// ROOT headers
// %%%%%%%%%%%%%
#include "TGraphErrors.h"
#include "TH1.h"
#include "TF1.h"
#include "TTree.h"

// %%%%%%%%%%%%%%%%
// Cerenkov headers
// %%%%%%%%%%%%%%%%
#include "usage.h"

// TF1 functions
double backgroundf(double *x, double *par);
double gaussianf(  double *x, double *par);
double gaussianff(  double x, double *par);
double fitf(       double *x, double *par);
double poissonf(   double *x, double *par);
double poissonff(   double x, double *par);

class tpecal_calib
{
public:
	tpecal_calib(tpecal_opts*);
  ~tpecal_calib(){;}
	
	tpecal_opts *tpecalOpt;
	
	// %%%%%%%%%%%%%%%%%%%%%
	// Calibration constants
	// %%%%%%%%%%%%%%%%%%%%%
	int runno;              ///< run number
	int min_runno;          ///< database min run number
	int max_runno;          ///< database max run number
	int fit_all;            ///< set to 1 during SPE pass
	
	int DEAD[30];
	double MEAN[30];
	double RMS[30];
	double mean[30];
	double emean[30];

	// PED Analysis		
	double ped_value[30];
	double ped_old[30];
	double ped_err[30];
	double ped_sigma[30];
	double ped_parameters[30][3];
	TF1  *PedSignal;                          ///< PED Fit function
	TGraphErrors *PMean, *PSigma, *PMeanO;    ///< PED Graphs from data, old pedestal from DB
	
		
	// MIP Analysis
	TH1F *TPECal_ADC[30];          ///< All ADcs
	TH1F *TPECal_ADC_Single[30];   ///< ADC when none of the neighbours fired	
	TH1F *TPECal_TDC[30];          ///< All TDCs


	TTree *TPET;


	double mip_value[30];                  ///< MIP values as determined from fit/single fit/manual
	double mip_old[30];                    ///< MIP values from last calibration (from DB)
	TGraphErrors *Mean,  *PoissMu;         ///< MIP Graphs from data
	TGraphErrors *Meanu, *MeanO;           ///< Graphs with user mods (if any), Old SPE
	TGraphErrors *MeanD, *MeanM;           ///< Dead, Modified channels
	double parameters[30][5];
	
	TF1  *BG, *GS, *PS;                            ///< MIP Display functions
	TF1  *Background, *Gaussian, *Poisson, *Fitf;  ///< MIP Fit functions	
	TGraphErrors *HVMeanO, *HVMeanN, *HVMeanD;     ///< HV Graphs from data: Old, New, Diff
		
	
	// HV Analysis
	double TPECal_HV_OLD[30];
	double TPECal_HV_NEW[30];
	double TPECal_HV_CORR[30];
	
		
	string rfile;                                          ///< ROOT file name
	string datfile;
	vector<string> burt_header;
	
	bool open_tpecal_b,    ///< is it a bos file
	open_mip_b,            ///< is it a spe file
	open_ped_b,            ///< is it a pedestal file
	processed_mip_b,       ///< is the MIP file already processed
	processed_ped_b,       ///< is the PED file already processed
	hv_b;                  ///< is it HV data
	
	void Set_ParLimits();          ///< Set Fit functions Parameter Limits
	int get_adcs();                ///< Gets ADC from bos data
	int open_root();               ///< Opens ROOT file
	int getADCs(int);              ///< Get ADCs from bos file(s). 0: don't subract pedestals. 1: subtract pedestals
	void reset_pars();             ///< Reset fit parameters
	int jrow(int ch){return ch/6;}
	int jcol(int ch){return ch%6;}
	vector<int> neighbors(int);       ///< Return the vector of neighbours scintillators
	
	// MIP
	void fit_spe(int);          ///< Fits one channel. Argument: channel index
	void fit_all_spe();         ///< Fits all channels
	void spe_graphs(int);       ///< Builds(arg=1)/Delete(arg=0) SPE TGraphErrors from data
	void ped_graphs(int);       ///< Builds(arg=1)/Delete(arg=0) PED TGraphErrors from data
	void hv_graphs(int);        ///< Builds(arg=1)/Delete(arg=0) HV TGraphErrors from data
	void write_pars();          ///< Write parameters to ROOT file, means to .dat file.
	void write_ped_pars();      ///< Write pedestal parameters to ROOT file, means to .dat file.
	
	// Pedestals
	void fit_ped(int);          ///< Fits pedestals. Argument: channel
	void fit_all_ped();         ///< Fits all ped channels
	
	// Database
	void init_db();                      ///< Initialize Database
	void change_run_number(int, int);    ///< Changes Run Number
	void read_hv_db();                   ///< Reads HV from Database
	void write_hv_db();                  ///< Writes HV to Database
	void read_ped_db();                  ///< Reads pedestals from Database
	void read_mip_db();                  ///< Reads old MIP from Database
	void read_hv_snap(string);           ///< Reads HV voltages from snap file
	void write_hv_snap();                ///< Writes HV voltages to snap file
	void write_mip_db();                 ///< Writes MIP constant into DB
	void write_ped_db();                 ///< Writes PED constant into DB
	
	
};

#endif



