


#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <ctype.h>

#ifdef SunOS
#include <ieeefp.h> // needed for finite() on SunOS
#endif

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE
#endif

#include <unistd.h>
#include <errno.h>
#include <sys/time.h>
#include <time.h>
#include <signal.h>


/* For SunOS */
#undef SEC

#ifdef __cplusplus

#ifdef linux
extern "C" {
// This unfortunate kludge is to avoid lots of
// implicit declaration warnings on my home PC
extern void _IO_flockfile __P ((_IO_FILE *));
extern void _IO_funlockfile __P ((_IO_FILE *));
}

// for pthreads
#define __REENTRANT
#define __POSIX_SOURCE

#endif /* linux */


#include <iostream>
//#include <strstream.h>
#include <iomanip>
using namespace std;
#include <pthread.h>

#endif /* __cplusplus__ */


#include "c_cern.h"

#ifdef __cplusplus
extern "C" {
#endif
#include <dc.h>

#include<tcl.h>
#include<tk.h>

#ifdef __cplusplus
}
#endif

#define DEBUG() cerr<<__FILE__<<":"<<__LINE__<<"\n"

#define ansi_escape		((char)0x1b)
#define ansi_bold 		ansi_escape<<"[1m"
#define ansi_black		ansi_escape<<"[30m"
#define ansi_red			ansi_escape<<"[31m"
#define ansi_green		ansi_escape<<"[32m"
#define ansi_blue			ansi_escape<<"[34m"
#define ansi_normal		ansi_escape<<"[0m"
#define ansi_up(A)		ansi_escape<<"["<<A<<"A"
#define ansi_down(A)		ansi_escape<<"["<<A<<"B"
#define ansi_forward(A)	ansi_escape<<"["<<A<<"C"
#define ansi_back(A)		ansi_escape<<"["<<A<<"D"

#define DEG2RAD (PI/180.0)

#define MAX_COMMENT_SIZE 1024

/* Global Variables */
extern float pawc_[]; /* point to fortran PAWC variable */
extern int *gcbank_;
extern int quest_[100];
extern Tcl_Interp* interp;
extern int program_is_dc_calib_check;
extern int EXCLUDED[7];
extern char INPUTFILE[256];
extern char PROGRESS_MESSAGE[256];
extern char VERSIONSTR[];
extern char DC3_LOGO[];
extern int DC_DCH_VERSION_MAJOR;
extern int DC_DCH_VERSION_MINOR;


/* Tcl/Tk Globals */
extern int RUNNUMBER;
extern int NOTEBOOK_PAGE;
extern int SEC,SUP;
extern int EXPERT;
extern int FAST_STARTUP;
extern int TLIM[7],TLIMT[7],TLIM_LOW;
extern int TLIM_AUTO_LOW[7],TLIM_AUTO_HI[7];
extern int AUTOCUT_T;
extern int STD_TCUT;
extern float STD_TCUT_LOW[7][7],STD_TCUT_HI[7][7];
extern double AUTOCUT_T_FRAC;
extern char *TZERO_FRAC,*TMAX_FRAC,*TZERO_AUTOFRAC,*TMAX_AUTOFRAC;
extern char *TMAX_REGION1_AUTOFRAC, *TMAX_REGION2_AUTOFRAC, *TMAX_REGION3_AUTOFRAC;
extern char *TMAX_REGION1_AUTOFRAC_INIT;
extern char *TMAX_REGION2_AUTOFRAC_INIT;
extern char *TMAX_REGION3_AUTOFRAC_INIT;
extern char *MAPFILE,*MAPCOMMENT;
extern char *USERCOMMENT;
extern char *CALDB_HOST, *CALDB_RUNINDEX, *CALDB_USER, *CALDB_PASSWORD;
extern int MINRUN, MAXRUN;
extern int SHOW_RESIDUAL,SHOWPLOTS,SHOW_SCATTER,SHOW_PROFILE;
extern int SHOW_FUNCTION,SHOW_PREDICTED_RESI,SHOW_ZERO_LINE;
extern char *FILENAME;
extern int RECALC_RESIDUALS;
extern int RECALC_METHOD;
extern int REPLOT;
extern int HOLD_PAR1 ,HOLD_PAR2 ,HOLD_PAR3 ,HOLD_PAR4;
extern int HOLD_TMAX ,HOLD_TZERO ,HOLD_FF;
extern int SEEK_FIRST;
extern int CANCEL;
extern int USE_CORRECTED_TIME;
extern int TW_TYPE ,XVST_TYPE ,EXPERT_CONTROLS;
extern int TW_TYPE_COMMAND_LINE;
extern int MDBI_RESULT;
extern int TZERO_PLOT_MIN,TZERO_PLOT_MAX;
extern int TZERO_FIT_MIN,TZERO_FIT_MAX;
extern int REFILL_XVST_TABLE_FOR_DOCA_PLOT;

/* for storing info about fields in the Ntuple */
typedef struct{
   char name[32];
   int pos;
   float min;
   float max;
}nt_field_t;
#define MAX_NTUPLE_FIELDS 64
extern nt_field_t NTUPLE_FIELD[MAX_NTUPLE_FIELDS];
extern int NUM_NTUPLE_FIELDS;
extern char NTUPLE_FIELD_NAMES[][32];



/* function declarations */

void PrintVersion(void);
void Usage(void);

/* dc3_file.cc */
void Get_Ntuple(int NID);



/* Structure for 24 paramter parms array from map */
typedef struct {
   float locang;
   float tzero;
   float tmax;
   float B;
   float ff;
   float p1;
   float p2;
   float p3;
   float p4;
   float layer;
   float tau_p1;
   float tau_p2;
   float tau_p3;
   float spare1;
   float tmax_correction;
   float B_p1;
   float B_p2;
   float B_p3;
   float B_p4;
   float spare2;
   float tmax_B_p1;
   float tmax_B_p2;
   float tmax_B_p3;
   float xvst_functiontype;
}map_parms_t;


#ifdef __cplusplus

class ntuple_row_t {
   public:
      float sector;
      float layer;
      float wire;
      float time;
      float doca;
      float resi;
      float B;
      float B1;
      float B2;
      float beta;
      float phi;
      float locangle;
      float P;
      float q;
      float chisq;
      float hit;
      float DC1;
      float calcdoca;
      float ctime;
};
extern ntuple_row_t AVG[7][7];
extern ntuple_row_t ISCUT,CUTMAX,CUTMIN;

/* Structures for filter cuts */
typedef struct{
   int pos;
   float min,max;
}cut_t;

class filter_t{

   public:
      int N;
      int sec,sup;
      cut_t cut[MAX_NTUPLE_FIELDS];
   
      // This flag just keeps track of whether the residuals
      // used were re-calculated or not.
      int recalculated_residuals;
      int recalculate_method;

      // This flag will force a not-equal result in a filter equality test
      int stale;

		// This is the fraction of the ntuple read so far in filling histos
		float progress;

		// Was a standard time cut used
		int std_tcut;
		int autocut_t;
		float autocut_t_frac;

		// Histograms plotted vs. corrected time axis
		int used_corrected_time;
};
extern filter_t DATA_FILTER[7];

int operator==(filter_t f1,filter_t f2);
int operator!=(filter_t f1,filter_t f2);
int operator*(ntuple_row_t n,filter_t f);

// Parms Class definition
class dc_parms_t {

   public:
      union {float par[24]; map_parms_t p;} parms;
      float tmax[36];
      float chisq;
      int   DOF;
      int   POINTS_IN_FIT;

      int xvst_runno;
      int tmax_runno;
      int tw_runno[5];
   
      float tw_parm_r1[11];
      float tw_parm_r2[11];
      float tw_parm_r3[11];
      float tw_fact[4];
      float tw_tau[4];
		float tw_betaslope[7];
		int   tw_functiontype;

      // Above here should be aligned exactly with the xvst_params_t data structure
      // defined in dc.h. The first field after this must be "filter" since it is
      // used to check this in dc3.cc
      
      filter_t filter;
      int expert_mode;
      int sector,superlayer;


      // calulate XvsT function using these parameters
      float xvst_fct(ntuple_row_t *);
      float Chisq(void);
      float Chisq(float *data, float *err);
      void MakeFunctionHist(int FID);
      void PrintComparison(dc_parms_t *d,char *mess);
		void ShowParameters(char *path,int recalc_chisq);
		void GetExpertParameters(int get_timewalk, int get_xvst);
};

extern dc_parms_t FILE_PARMS[7][7];
extern dc_parms_t MAP_PARMS[7][7];
extern dc_parms_t USER_PARMS[7][7];

filter_t GetFilter(void);
void Read_Ntuple(filter_t filter);
void CopyParmsToDC(dc_parms_t* dc_parms,int sec,int sup);
void CopyParmsFromDC(dc_parms_t* dc_parms,int sec,int sup);
void CopyTimewalkParmsToAll(dc_parms_t tp);
void FindResidualWidths(int HID, float *sigma_narrow,float *sigma_wide);

void FillTmaxLayers(dc_parms_t *dc_parms);
int CompareXvsTParms(dc_parms_t *a, xvst_parms_t *b);
int CompareTimewalkParms(dc_parms_t *a, xvst_parms_t *b);

#endif /* __cplusplus */


/* Routine declarations */
void USLEEP(long microseconds);
void Tcl_Evaluate(char* cmd);
void Get_Ntuple(int NID);
void* PlotThread(void* arg);
void* dc_xvst_init_thread(void *arg);
void* dc_fill_t2x_table_thread(void *arg);
Tcl_VarTraceProc NewSelection;
Tcl_VarTraceProc NewTzeroTmax;
Tcl_VarTraceProc NewNotebookPage;
void* ProgressBar(void *arg);
void* ProgressClock(void *arg);
void* Fill_DOCA_From_Ntuple_thread(void *arg);
void ProgressClockStart(void);
void ProgressClockUpdate(void);
void ProgressClockStop(void);
void Book_Histograms(void);
void ListHistos(void);
float TzeroFromLinearFit(int hid, int plot);

void ShowLogo(void);
void RemoveLogo(void);

void PlotResidual(void);
void PlotXvsT(void);
void PlotDriftTime(void);
void PlotResidualProjection(void);
void MakeFunctionHist(int sec,int sup);

void FillTmaxLayers(int sec,int sup);
float FindTFractionFromHID(float t_frac,int TID);

void FindResidualWidths(int HID
   ,float *sigma_narrow
   ,float *sigma_wide
   ,float *mean
   ,float *amplitude_ratrio);

void FindHitsPerTBT(float *hpt);
void FindAvgTrackingChisq(float *chisq);
float TotalWidth(float R,float sigma_n,float sigma_w);

int ConnectToDatabase(int disconnect_after_check);
char* GetDCDefault(char *key);
float GetDCDefaultFloat(char *key);


Tcl_CmdProc TclThreadTcl;
Tcl_CmdProc OpenFile;
Tcl_CmdProc QualityCheck;
Tcl_CmdProc FindTFraction;
Tcl_CmdProc PlotThreadTcl;
Tcl_CmdProc CaldbWrite;
Tcl_CmdProc MapWrite;
Tcl_CmdProc CreateMapFile;
Tcl_CmdProc ResetTValue;
Tcl_CmdProc ResetTValues;
Tcl_CmdProc FitOne;
Tcl_CmdProc FitAll;
Tcl_CmdProc FitSuperlayer;
Tcl_CmdProc SetCutButton;
Tcl_CmdProc SetCuts;
Tcl_CmdProc ResetUserParms;
Tcl_CmdProc ChisqVector;

Tcl_CmdProc TzeroTable;
Tcl_CmdProc TmaxTable;

Tcl_CmdProc ChisqTable;
Tcl_CmdProc ApplyExpertParameters;
Tcl_CmdProc SetParametersToDefault;
Tcl_CmdProc CopyUserParms;
Tcl_CmdProc PlotCalcDOCA;
Tcl_CmdProc SetTzeroFromLinearFit;



