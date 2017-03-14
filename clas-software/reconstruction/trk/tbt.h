/*
Header file for time based tracking structures and prototypes
*/


#define MAX_FITS 64

/******************STRUCTURES*********************/

/*structure for performing linear regression fits*/
typedef struct{
  float a;
  float b;
  float chi2;
  float x;
  float y;
  float x2;
  float xy;
  float one;
}fit_results;

/*structure to store results of good fits*/
typedef struct{
  float a[64];
  float chi2[64];
  float LR[64][6];
  int num_fit;
  int flag;
}line_params;

/*structure to store relevant fit information of good fits*/
typedef struct{
  float chi2[64];
  float doca[6][64];
  float slopeD[64];
  int counter;
  int flag;
}lramb;

/*structure to store wire, drift time, doca, and sigma for a hit*/
typedef struct{
  int wire;
  float dtime;
  float doca;
  float sigma;
}hit_info;

/*structure which contains corrected hit information for all successful fits*/
typedef struct{
  int nfits;
  hit_info fit[6][MAX_FITS];
  float chi2[MAX_FITS];
  int known_amb[6][MAX_FITS*5];
  int nknown[6];
}fit_info;

/*structure of the corrected hit information for one successful fit in a segment*/
typedef struct{
  int flag;
  hit_info layer[6];
}sort_sls;

/*structure of (x,y) coordinates and sigma(y) for fitting*/
typedef struct{
  float x;
  float y[2];
  float sigma;
}fit_pts;

/*****************PROTOTYPES***********************/
line_params line_fit(fit_pts[], int, int, float, float);
lramb resolve_lramb(int, int, hit_info[], float, float);
fit_results calc_line(float, float, float, int *, fit_results);
sort_sls get_lramb_info(int, int, int);
fit_pts get_lramb_pts(hit_info, dcgm_t, float, int *);
int sort_by_doca(const void *, const void *);
int sort_by_x(const void *, const void *);
void weed_lramb(int, int);
void weed_known_amb(int, int);
/**************GLOBAL VARIABLES*******************/
extern fit_info seg[MAX_TRACKS][6];
