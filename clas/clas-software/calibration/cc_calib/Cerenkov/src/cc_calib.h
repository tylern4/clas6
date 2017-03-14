#ifndef CC_CALIB_H
#define CC_CALIB_H

// Translation Table for CC:
// ch      L          R
//  1      1          2
//  2      3          4
//  3      5          6
//
// Notice: channels are stored differently in the internal HV array

// %%%%%%%%%%%%%
// ROOT headers
// %%%%%%%%%%%%%
#include "TGraphErrors.h"
#include "TH1.h"
#include "TF1.h"

// %%%%%%%%%%%%%%%%
// Cerenkov headers
// %%%%%%%%%%%%%%%%
#include "usage.h"

// TF1 functions
double backgroundf(double *x, double *par);
double gaussianf(double *x, double *par);
double gaussianff(double x, double *par);
double fitf(double *x, double *par);
double poissonf(double *x, double *par);
double poissonff(double x, double *par);

class cc_calib
{
 public:
   cc_calib(cc_opts*);
  ~cc_calib(){;}

   cc_opts *ccOpt;

   // %%%%%%%%%%%%%%%%%%%%%
   // Calibration constants
   // %%%%%%%%%%%%%%%%%%%%%
   int runno;              ///< run number
   int min_runno;          ///< database min run number
   int max_runno;          ///< database max run number
   int fit_all;            ///< set to 1 during SPE pass

   int DEAD[6][36];
   double MEAN[6][36];
   double RMS[6][36];
   double mean[6][36];
   double emean[6][36];
   double poissMu[6][36];
   double poissP[6][36];

   double spe_value[6][36];          ///< SPE values as determined from fit/single fit/manual
   double spe_old[6][36];            ///< Old (DB) SPE values
   double parameters[6][36][5];
   double ped_parameters[6][36][3];

   double ped_value[6][36];
   double ped_old[6][36];
   double ped_err[6][36];
   double ped_sigma[6][36];
   double chi2df[6][36];             ///< chi2/degrees of freedom

   double CC_HV_OLD[6][36];
   double CC_HV_NEW[6][36];
   double CC_HV_CORR[6][36];

   // %%%%%%%%%%%%
   // ROOT objects
   // %%%%%%%%%%%%
   TH1F         *CC_ADC[6][36];
   TF1          *BG, *GS, *PS;                            ///< SPE Display functions
   TF1          *Background, *Gaussian, *Poisson, *Fitf;  ///< SPE Fit functions
   TF1          *PedSignal;                               ///< PED Fit function
   TGraphErrors *Mean[6],  *PoissMu[6];                   ///< SPE Graphs from data
   TGraphErrors *Meanu[6], *MeanO[6];                     ///< Graphs with user mods (if any), Old SPE
   TGraphErrors *MeanD[6], *MeanM[6];                     ///< Dead, Modified channels
   TGraphErrors *PMean[6], *PSigma[6], *PMeanO[6];        ///< PED Graphs from data, old pedestal from DB
   TGraphErrors *HVMeanO[6], *HVMeanN[6], *HVMeanD[6];    ///< HV Graphs from data: Old, New, Diff
   string rfile;                                          ///< ROOT file name
   string datfile;
   vector<string> burt_header;
	
   bool open_cc_b,             ///< is it a bos file
        open_spe_b,            ///< is it a spe file
        open_ped_b,            ///< is it a pedestal file
        processed_spe_b,       ///< is the SPE file already processed
        processed_ped_b,       ///< is the PED file already processed
        hv_b;                  ///< is it HV data

   void Set_ParLimits();          ///< Set Fit functions Parameter Limits
   int get_adcs();                ///< Gets ADC from bos data
   int open_root();               ///< Opens ROOT file
   int getADCs(int);              ///< Get ADCs from bos file(s). 0: don't subract pedestals. 1: subtract pedestals
   string cc_nomenclature(int);   ///< Translate 1-36 into 1-18 L/R
   void reset_pars();             ///< Reset fit parameters

   // SPE
   void fit_spe(int, int);     ///< Fits one spe channel. Argument: sector, channel
   void fit_all_spe();         ///< Fits all spe channels
   void spe_graphs(int);       ///< Builds(arg=1)/Delete(arg=0) SPE TGraphErrors from data
   void ped_graphs(int);       ///< Builds(arg=1)/Delete(arg=0) PED TGraphErrors from data
   void hv_graphs(int);        ///< Builds(arg=1)/Delete(arg=0) HV TGraphErrors from data
   void write_pars();          ///< Write parameters to ROOT file, means to .dat file.
   void write_ped_pars();      ///< Write pedestal parameters to ROOT file, means to .dat file.

   // Pedestals
   void fit_ped(int, int);     ///< Fits pedestals. Argument: sector, channel
   void fit_all_ped();         ///< Fits all ped channels

   // Database
   void init_db();                      ///< Initialize Database
   void change_run_index(string);       ///< Changes Run Index
   void change_run_number(int, int);    ///< Changes Run Number
   void read_hv_db();                   ///< Reads HV from Database
	void write_hv_db();                  ///< Writes HV to Database
	void read_ped_db();                  ///< Reads pedestals from Database
   void read_spe_db();                  ///< Reads old SPE from Database
   void read_hv_snap(string);           ///< Reads HV voltages from snap file
   void write_hv_snap();                ///< Writes HV voltages to snap file
   void write_spe_db();                 ///< Writes SPE constant into DB
   void write_ped_db();                 ///< Writes PED constant into DB


};

#endif



