#define length_ic 529

typedef struct {
  float deep;
  float ehitmin;
  float egrplmin;
  float egrpmin;
  float w0;
  float xstep;
  float ystep;
  float zt0;
  float encorr[16][3];
  float dcorr[16][3];
} recon_t;

extern recon_t recon_;

