/* call.h - Taylor, Manak --  Jan, 1998 */
#ifndef CALL_
#define CALL_

#ifdef __cplusplus
/* make this header file useabble for c++ compiler */
extern "C" {
#endif

/* number of elements in CAllCall array*/
#define  CALL_IDS 10
/* index into CALL_IDS array for CAllCall structure (ind_cl01)*/
#define AC60HZ  1
#define FWD_DIODE 2
#define N_CLMSH_DIODE 3
#define S_CLMSH_DIODE 4
#define SPCFM_DIODE 5
#define RF1 6
#define RF2 7

#define RF_STRUCTURE 2.004


typedef struct{
	float t0[CALL_IDS];
	float t1[CALL_IDS];
	float t2[CALL_IDS];
	float ped[CALL_IDS];
}call_const_t;

typedef struct{
  float low;
  float high;
  float p0;
  float p1;
  float p2;
  float p3;
} F_t;

#define NO_CL01_HIT -9999
int make_cl01_bank_();
int initcl01_(int *runno);
int make_CL01_bank();
int CL01_begin_run(int runno); /* initialize for mc and data */
int initCL01(int runno);
float getCL01time(int ind_cl01, int tdc);
int getCL01adc(int ind_cl01, int adc);
int get_diode_id(int id,int sec);
float get_diode_time(cl01_t cl01,int id,int sec);
float rf_correction(float time, float rf);
float rf_corr_time(float time); /*get rf corrected time*/
float rf_corr_time_(float *time); /*Fortran-callable version of this routine*/
void rf_numbers_(float *rf1, float *rf2, float *good_rf, float *rf_corr);

#ifdef __cplusplus
/* make this header file useabble for C++ compiler */
}
#endif

#endif
