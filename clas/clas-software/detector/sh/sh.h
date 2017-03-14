#ifndef SH_H
#define SH_H

#include <ntypes.h>
#include <bostypes.h>

#define SH_CHANNELS      72
#define SH_PIXELS        54
#define SH_MAX_ADCHITS   72
#define SH_MAX_TDCHITS 1152


// ------------------------ sh_cailb -----------------/
typedef struct {
  int adcstat[SH_CHANNELS];
  int tdcstat[SH_CHANNELS];
  
  float pedestal[SH_CHANNELS];
  float gain[SH_CHANNELS];  
  
  float t2channel[SH_CHANNELS];
  float t0[SH_CHANNELS]; 
  float tcoi_m[SH_CHANNELS];
  float tcoi_s[SH_CHANNELS];

  float x0[3];          // position of hodoscope from center of CLAS    
  float alpha[3];
  float pixelsize[1];
} sh_calib_t;
// ------------------------END  sh_cailb -----------------/

// ------------------------ shraw -----------------/
typedef struct {
  int id;     // ID  /
  int val;    // Raw value (adc or tdc) /  
} shraw_t;
// --------------------- END shraw -----------------/

// ------------------------ shraw2et -----------------/
typedef struct {
  int   pix;    // ID /
  float val;    // Converted value /  
  int   type;   // Hit type /
} sh2et_t;
// --------------------- END shraw2et -----------------/


void  sh_brun_(int *runno);
int   sh_brun(int  runno);
int   sh_maputil(char *shmap,sh_calib_t *shcalib,int runno);
void  sh_coordinates(float *x0, float *ang,float *pixsz);

void  sh_evnt_();
void  sh_evnt();
int   sh_read_adc();
int   sh_read_tdc();
int   sh_read_adc_g12();
int   sh_read_tdc_g12();
int   sh_adc2_nphe(int adch);
int   sh_tdc2_nsec(int tdch);

int   get_chan_id(int raw_id);
int   get_chan_id_g12(int raw_id);
int   get_db_ind(int raw_id);
int   coin_check(int pixid);

void  sh_hb_pb(int adch, int tdch);
float get_nphe(int adc, float pedestal, float gain);
float get_time(int tdc, float t2ch, float t0);


#endif
