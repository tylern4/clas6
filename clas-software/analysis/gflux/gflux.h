/* gflux.h */

/* hbook */

#define LREC 1024 /* record length of hbook direct access file in WORDS */
#define LUN 3 /* logical unit number of hbook file */
#define MEMH 5000000

/* gflux */

#define BID_BANKS "CC01SC1 SCR SCRCECHBECPITAGITAGRST1 STR PSO "
#define EC_CHANNELS 2048

#define ERG_MAX 6.0
#define ERG_MIN 1.0
#define ERG2_MAX 6.0
#define ERG2_MIN 1.0

/* #define ERG_MAX 2.9875 */
/* #define ERG_MIN 0.4875 */
/* #define ERG2_MAX 3.0 */
/* #define ERG2_MIN 0.5 */

#define ET_T_DIFF_MIN -20.
#define ET_T_DIFF_MAX 20.
#define INTERRUPT_TIME_FREQ 1.e6
#define NUM_EB 767
#define NUM_ERG 100
#define NUM_ERG2 50
#define NUM_EC 384
#define NUM_ET_T_DIFF 80
#define NUM_ST 3
#define NUM_TC 61
#define NUM_TDC 260
#define NUM_TB 121
#define OVERFLOW_32BIT 0xFFFF
#define TAG_BANKS "CL01TAGRTAGITACOPCO PSO "
#define TARGET_Z_MAX 11.
#define TC_CHANNELS 4096
#define TDC_MAX 220.
#define TDC_MIN -40.

/*--------------------------- Function prototypes ---------------------------*/

float binomialU(float p, float n);
float sq(float x);

void bookHistos();
void ctrlCHandle(int);
void cleanUp();
void fillHistos();
void fillScalerHistos();
void finalCal();
void fluxCal();
void getMapInfo();
void getRate(float *rate, float *rateFU, float *NoutOfTime, float *Ntrials,
             int numBins);
void getRateST();
void getTime();
void hini(char *file);
void normYield(int numBins, int hIndex, int pidType);
void photonsTC2EB(float *NphoTC, float *NphoTCFU, float *NphoEB,
                  float *NphoEBU, int numBins, int hIndex);
void taggingRatio();

/*---------------------------------------------------------------------------*/
