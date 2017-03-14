#ifndef GSIMKO_H
#define GSIMKO_H

// PAW DEFINES ---------------------------------------------------------------------------
#define MEMH 400000
#define LREC 1024      // record length of hbook direct access file in WORDS
#define LUN  3         // logical unit number of hbook file

// M A C R O S ---------------------------------------------------------------------------
#define CC_CHANNELS      216
#define MV_2_ADC_EC      23.0   // Threshold in adc is 23 channels per mV discriminator
#define EC_SAMPLING_FRAC 0.275  // Sampling fraction for Calorimeters
#define MV_2_ADC_CC      2      // Threshold in adc is 2 channels per mV discriminator
#define MIN_CC_TRIG_BIT  0      // don't consider trigger bits less than this (should be zero)
#define N_CC_TRIG_BITS   8      // there are this many cc trigger bits
#define NOTB1_MIN_CC_BIT 1      // This is to ignore trig bit 1 for testing
#define GOOD_PHOTON      600
#define BAD_PHOTON       700
#define GAUSSIAN_MODE     1

// Define Bitwise Processing Flags -------------------------------------------------------

#define EC_IN_TRIG          BIT(0)
#define ECINNER_IN_TRIG     BIT(1)
#define CC_IN_TRIG          BIT(2)
#define CC_BIT1_NOT_IN_TRIG BIT(3)
#define INCLUDE_DC          BIT(0)
#define SMEAR_DC	          BIT(1)
#define INCLUDE_SC	        BIT(2)
#define SMEAR_SC	          BIT(3)
#define INCLUDE_RF	        BIT(4)
#define SMEAR_TAGR          BIT(5)
#define REMOVE_BAD_TAGR     BIT(6)
#define INCLUDE_IC	        BIT(7)
#define SMEAR_IC	          BIT(8)

#ifdef Darwin
	typedef unsigned short ushort;
#endif

#include <cstdio>
#include <iostream>
#include <string>
#include <iomanip>
#include <cmath>
#include <cstdlib>
using namespace std;


extern "C" {
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <string.h>
#include <ntypes.h>
#include <bostypes.h>
#include <ec.h>
#include <clas_cern.h>
#include <bosddl.h>
#include <particleType.h>
#include <utility.h>
#include <makebanks.h>
#include <status.h>
#include <map_manager.h>
#include <kinematics.h>
#include <sc.h>
#include <time.h>
#include <call.h>
#include <bitflags.h>
#include <pid.h>
#include <tagcal_cmn.h>

typedef struct
{
 int sec;
 int adc;
} pmtHit_t;

int   ProcessEvent(int RunNo, int verbose);
int   ProcessDC(int verbose);
int   ProcessSC();
int   ProcessIC();
int   ProcessRF();
int   ProcessPART(int partflag);
int   ProcessTAGR(int runno);
void  PrintUsage(char *processName);
void  bnames_(int *);
void  bosta_(int []);
void  initRun(int RunNo,int verbose);

float SmearFnc(int mode,float value,float smear);
float get_resol(int region, float DOCA);
void  load_map_vars(char *map);
float Uniform(float Low, float High);
float ran2(long *idum);

int   TrigSect(int *retTrig);
int   sort_pmtHit(const void *pmtHit1, const void *pmtHit2);
int   cc_id2TrigBit(int id, int * CCBit1, int * CCBit2);
int   ec_strip(int);
int   ec_layer(int);
int   ec_orientation(int);
int   cc_index(int,int);
  // void  ec_read_map(int, float[6][EC_LAYERS][EC_ORIENTATION][EC_STRIPS], char *);
void  get_cc_pedestals_(char*,int,float[],float[]);

void  tag_read_bounds_(int *, int *, float *);
void  tagcal_read_map_(int *, int *, int *);
int   tagr_setup(int runno);

void  book_histos();
void  hini();
void  hrend_(char *, int);
void  hldir_(char *, char *, int, int);
void  hlimit_(int *);
void  hropen_(int *, char *, char *, char *, int *, int *, int, int, int);

// hbook common --------------------------------------------------------------------------
float pawc_[MEMH];
float hcfitd_[25];
int   quest_[100];

// bos common ------------------------------------------------------------------------
BOSbank bcs_;
BOSbank wcs_;

}

using namespace std;

int IncludeDC     = 0;
int IncludeSC     = 0;
int IncludeIC     = 0;
int IncludeRF     = 0;
int SmearDC       = 0;
int SmearSC       = 0;
int SmearIC       = 0;
int SmearTAGR     = 0;
int RemoveBadTAGR = 0;
int AddTAGR       = 0;
int ProcessFlag   = 0x1f;

long seed = -((long)time(0)); // initialize random number generator

const int NumBars_total = 57;     // number of bars in counter
const int NumParams  = 11;     // number of parameters for DOCA fit
const int NumICPar  = 529;     // number of parameters for IC constants
int NumBars = NumBars_total;

float ScaleFactor = 1.0;      // scaling factor for tdcl and tdcr SC smear
float Reg1Scale   = 1.0;      // scaling factors for DOCA smear in Region 1
float Reg2Scale   = 1.0;
float Reg3Scale   = 1.0;

float SC_length[NumBars_total];
float Resolution[NumBars_total];

float Reg1_Params[NumParams];
float Reg2_Params[NumParams];
float Reg3_Params[NumParams];
float Reg1_Means[NumParams];
float Reg2_Means[NumParams];
float Reg3_Means[NumParams];

float Eff[6][6912];
float gppTargetCenter[3];
int   MEANS = 0;
int   EFF   = 0;
int   TAGRSync = 0;

call_const_t call_calib;
int ic_stat[NumICPar];
float ic_gain[NumICPar];
float ic_pedestal[NumICPar];
float ic_tch[NumICPar];
//float ic_noise=0; 
//float ic_noiseRMS=0;
float ic_timeRMS=0;
float ic_apar=0; 
float ic_bpar=0; 
float ic_cpar=0; 

clasHEAD_t *HEAD;

float EC_thresh      = 0.0;
float CC_thresh      = 0.0;
float ECInner_thresh = 0.0;
int ectrig           = 0;                // Flag for EC in the trigger
int cctrig           = 0;                // Flag for CC in trigger
int ecinnertrig      = 0;                // Flag for EC Inner in trigger
int notb1trig        = 0;                // Flag for CC Bit 1 not to be in trigger
int min_cc_bit       = MIN_CC_TRIG_BIT;
int TrigFlag         = 0x0;

float tpho_offset    = 0.0;              // TAGR related stuff
float tpho_sigma     = 0.0;
float BeamEnergy     = 0.0;


float SmearFnc(int mode,float value,float smear)
{
 float ret;
 switch (mode) 
 {
  default:
   ret = gauss(value,smear);
  break;
 }
 return(ret);
}


float get_resol(int reg, float x) 
{
 // reg = region   x = doca

 float p[NumParams];
 float sigma      = 0;
 float scale      = 0;
 const int factor = 10000;
    
 // make sure x is in the proper range for the function fit
 x = fabs(x);  
 x /= factor;

 // load the coefficients for the polynomial
 switch (reg) 
 {
  case 1:
   for(int i=0;i<NumParams;++i) p[i] = Reg1_Params[i];
   scale = Reg1Scale;
  break;
  case 2:
   for(int i=0;i<NumParams;++i) p[i] = Reg2_Params[i]; 
   scale = Reg2Scale;
  break;
  case 3:
   for(int i=0;i<NumParams;++i) p[i] = Reg3_Params[i];
   scale = Reg3Scale;
  break;
  default:
   cout << "Invalid value for reg. Must be in range [1-3]" << endl;
   return 0;
  break;
 }
  
 // use functional fit to calculate the resolution
 for(int i=0;i<NumParams;++i) sigma += p[i]*pow(x,i);
  
 sigma *= factor;
 sigma *= scale;
 return sigma;
}


float get_mean(int reg, float x)
{
 // reg = region
 // x = doca

 float p[NumParams];
 float mean       = 0;
 const int factor = 10000;

 // make sure x is in the proper range for the function fit
 x = fabs(x);
 x /= factor;

 // load the coefficients for the polynomial
 switch (reg) 
 {
  case 1:
   for(int i=0;i<NumParams;++i) p[i] = Reg1_Means[i];
  break;
  case 2:
   for(int i=0;i<NumParams;++i) p[i] = Reg2_Means[i];
  break;
  case 3:
   for(int i=0;i<NumParams;++i) p[i] = Reg3_Means[i];
  break;
  default:
   cout << "Invalid value for reg. Must be in range [1-3]" << endl;
   return 0;
  break;
 }

 // use functional fit to calculate the resolution
 for(int i=0;i<NumParams;++i) mean += p[i]*pow(x,i);

 mean *= factor;
 return mean;
}



float Uniform(float Low, float High) 
{
 //  cout << seed << endl;
 if(Low > High) 
 {
  cerr << "Illegal range passed to Uniform()" << endl;
  return(1);
 }
 else 
 {
  float IntervalSize = High - Low;
  float RandomOffset = ran2(&seed) * IntervalSize;
  return (Low + RandomOffset);
 }
}


void load_map_vars(int runno) 
{
 int firsttime;
 char *dir, icmap[300], rfmap[300], gppmap[300], geomap[300];


 dir=getenv("CLAS_PARMS");
 sprintf(gppmap,"%s/Maps/GPP.map",dir);

 dir=getenv("CLAS_PARMS");
 sprintf(geomap,"%s/Maps/GEOMETRY.map",dir);

 if( NumBars==NumBars_total ) {
   map_get_float(gppmap, "SC",      "Sigma57",     NumBars,      Resolution,    runno, &firsttime);
 }
 else { 
   map_get_float(gppmap, "SC",      "Length",    NumBars,      SC_length,     runno, &firsttime);
   map_get_float(gppmap, "SC",      "Sigma",     NumBars,      Resolution,    runno, &firsttime);
 }
 cout << endl << endl << "Getting parameters from database (map)..." << endl;
 cout << "  SC length and sigma..." << endl; 
 

 map_get_float(gppmap, "DC",      "Reg1",      NumParams,    Reg1_Params,   runno, &firsttime);
 map_get_float(gppmap, "DC",      "Reg2",      NumParams,    Reg2_Params,   runno, &firsttime);
 map_get_float(gppmap, "DC",      "Reg3",      NumParams,    Reg3_Params,   runno, &firsttime);
 cout << "    DC doca sigmas..." << endl; 

 map_get_float(gppmap, "DC",      "Reg1_mean", NumParams,    Reg1_Means,    runno, &firsttime); 
 map_get_float(gppmap, "DC",      "Reg2_mean", NumParams,    Reg2_Means,    runno, &firsttime); 
 map_get_float(gppmap, "DC",      "Reg3_mean", NumParams,    Reg3_Means,    runno, &firsttime); 
 cout << "      DC doca means..." << endl; 
 
 if(!IncludeDC && EFF==1)
 {
  map_get_float(gppmap, "DC_WIRE", "sector1",   DC_WIRES_SEC, Eff[0],        runno, &firsttime);
  map_get_float(gppmap, "DC_WIRE", "sector2",   DC_WIRES_SEC, Eff[1],        runno, &firsttime);
  map_get_float(gppmap, "DC_WIRE", "sector3",   DC_WIRES_SEC, Eff[2],        runno, &firsttime);
  map_get_float(gppmap, "DC_WIRE", "sector4",   DC_WIRES_SEC, Eff[3],        runno, &firsttime);
  map_get_float(gppmap, "DC_WIRE", "sector5",   DC_WIRES_SEC, Eff[4],        runno, &firsttime);
  map_get_float(gppmap, "DC_WIRE", "sector6",   DC_WIRES_SEC, Eff[5],        runno, &firsttime);
  cout << "        DC wire efficiencies..." << endl; 
 }
 cout << endl << endl;
 // for(int i=0 ; i<6912; i++) cout << Eff[0][i] << endl;
 
 sprintf(rfmap,"%s/Maps/CALL_CALIB.map",dir);
 map_get_float(rfmap,  "T1", "value",  CALL_IDS,  call_calib.t1, runno, &firsttime); 

 sprintf(icmap,"%s/Maps/IC.map",dir);
 map_get_int(icmap,  "status", "status", NumICPar ,  ic_stat, runno, &firsttime); 
 int irun=1;//only GSIM run values are needed
 map_get_float(icmap,  "Energy", "Gain0", NumICPar ,  ic_gain, irun, &firsttime); 
 map_get_float(icmap,  "Energy", "Pedestals", NumICPar ,  ic_pedestal, irun, &firsttime); 
 map_get_float(icmap,  "Timing", "Tch", NumICPar ,  ic_tch, irun, &firsttime); 
 map_get_float(gppmap, "IC", "ic_a", 1, &ic_apar, runno, &firsttime); 
 map_get_float(gppmap, "IC", "ic_b", 1, &ic_bpar, runno, &firsttime);
 map_get_float(gppmap, "IC", "ic_c", 1, &ic_cpar, runno, &firsttime); 
 cout<<"parameters:"<<ic_apar<<endl;
 map_get_float(gppmap, "IC", "timeRMS", 1, &ic_timeRMS, runno, &firsttime); 
 cout<<"parameters:"<<ic_apar<<" "<<ic_timeRMS<<endl;

 map_get_float(geomap, "target", "position", 3, gppTargetCenter, runno, &firsttime);
 cout << "targcenter from map = " << gppTargetCenter[0] << ", " << gppTargetCenter[1] << ", " << gppTargetCenter[2] << endl;
}

int tagr_setup(int runno) 
{
 char *dir;
 char RUNcontrolmapfile[256];
 char GPPmapfile[256];
 int ok         = 0;
 int ttranslate = 0;
 int where      = 0;
 int firsttime  = 0;
 int status     = -1;
  
 dir=getenv("CLAS_PARMS");
 tagcal_read_map_(&ok, &runno, &ttranslate);

 if(!ok) 
 {
  fprintf(stderr,"tagr_setup -F- problem with reading from Tagger maps\n");
  return(-1);
 }

 sprintf(RUNcontrolmapfile,"%s/Maps/RUN_CONTROL.map",dir);
 status = map_get_float(RUNcontrolmapfile, "beam", "energy", 1, &BeamEnergy, runno, &firsttime); 

 if(status != 0) 
 {
  fprintf(stderr, "tagr_setup -F- could not get Beam Energy from %s\n",          RUNcontrolmapfile);
  return(-1); 
 }

 fprintf( stderr, "tagr_setup -I- got BeamEnergy from RUN_CONTROL for run %d\n", firsttime);

 BeamEnergy = BeamEnergy/1000.;

 tag_read_bounds_(&ok, &where, &BeamEnergy);
 if(!ok) 
 {
  fprintf(stderr,"tagr_setup -F- problem with reading Tagger boundaries\n");
  return(-1);
 }
 sprintf(GPPmapfile,"%s/Maps/GPP.map",dir);
 if(AddTAGR) 
 {
  status = map_get_float(GPPmapfile, "tagr", "offset", 1, &tpho_offset, runno, &firsttime); 
  if(status != 0) 
  {
   fprintf(stderr, "tagr_setup -F- could not get tpho_offset from %s\n", GPPmapfile);
   return(-1); 
  }
 }
 
 if(SmearTAGR) 
 {
  status = map_get_float(GPPmapfile, "tagr", "sigma", 1,	 &tpho_sigma, runno, &firsttime); 
  if(status != 0) 
  {
   fprintf(stderr, "tagr_setup -F- could not get tpho_sigma from %s\n", GPPmapfile); 
   return -1; 
  } 
 }
       
 return(0);
}

int sort_pmtHit(const void *pmtHit1, const void *pmtHit2)
{
 pmtHit_t *hit1 = (pmtHit_t *) pmtHit1;
 pmtHit_t *hit2 = (pmtHit_t *) pmtHit2;

 if (hit1->adc < hit2->adc)  return -1;
 if (hit1->adc == hit2->adc) return  0;
 if (hit1->adc > hit2->adc)  return  1;

 return 99; // shouldn't get here
}

int cc_id2TrigBit(int id, int * Bit1, int * Bit2)
{
 // input : id        --  a cc pmt tube id
 // output: Bit1 Bit2 --  the cc trigger bits that tube id goes into.

 *Bit1 = id<33 ? 2*( (int)((id-1)/8) + 1) - 1 : 0; // odd numbered bits 
 *Bit2 = id>4  ? 2*( (int)((id-5)/8) + 1)     : 0; // even numbered bits are offset by 4 
  
 return 1;
}

#endif
