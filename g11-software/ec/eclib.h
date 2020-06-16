
/* eclib.h */

#define NSTRIP   36
#define NLAYER    6
#define NSECTOR   6
#define NHIT     30
#define NSHOWER  30

#ifndef PI
#define PI   3.14159265358979323846
#endif

typedef struct ecdata *ECDataPtr;
typedef struct ecdata
{
#ifdef Linux
  unsigned char  strip;     /* layer number */
  unsigned char  layer;     /* strip number */
#else
  unsigned char  layer;
  unsigned char  strip;
#endif
  unsigned short tdc;		/* tdc */
  unsigned short adc;		/* adc */
} ECData;


typedef struct ecpar *ECParPtr;
typedef struct ecpar
{
  float r;          /*                    */
  float a;          /* distance and angle */
  float v;          /*                    */
  float d;          /* depth */
  float H1;
  float the;        /* theta angle */
  float phi;        /* phi angle, 0 for first sector */
  float edge[3];    /* length of a side ( U, V and W layers ) */

  float h;
  float h1;
  float h2;

} ECPar;


typedef struct ecstrip *ECStripPtr;
typedef struct ecstrip
{
  float energy;       /* strip energy */
  float time;         /* strip time */
} ECStrip;


typedef struct ecpeak *ECPeakPtr;
typedef struct ecpeak
{
  float energy;       /* peak energy */
  float time;         /* peak time */
  float coord;        /* peak coordinate */
  float width;        /* peak width */
  int   strip1;       /* first strip in peak */
  int   stripn;       /* the number of strips in peak */
  float tmp;
} ECPeak;


typedef struct echit *ECHitPtr;
typedef struct echit
{
  short sector;
  short layer;
  float energy;       /* hit energy */
  float denergy;      /* hit energy error */
  float time;         /* hit time */
  float dtime;        /* hit time error */
  float i;            /* hit i-coordinate   |          */
  float j;            /* hit j-coordinate   | detector */
  float di;           /* hit di-coordinate  | system   */
  float dj;           /* hit dj-coordinate  |          */
  float x;            /* hit x-coordinate  |        */
  float y;            /* hit y-coordinate  |        */
  float z;            /* hit z-coordinate  |  CLAS  */
  float dx;           /* hit dx-coordinate | system */
  float dy;           /* hit dy-coordinate |        */
  float dz;           /* hit dz-coordinate |        */

  float uvw2[3];      /* second moment of uvw */
  float uvw3[3];      /* third moment of uvw */
  float i2;           /* second moment of i-coordinate */
  float j2;           /* second moment of j-coordinate */
  float i3;           /* third moment of i-coordinate */
  float j3;           /* third moment of j-coordinate */

  float width;        /* hit width */
  int   peak1[3];     /* first peak in hit in each layer */
  int   peakn[3];     /* the number of peaks in hit in eack layer */

  float k;

} ECHit;


typedef struct ecshower *ECShowerPtr;
typedef struct ecshower
{
  float e_in;         /* energy found for the inner layer */
  float e_out;        /* energy found for the outer layer */
  float dE_in;        /* error on the energy found for the inner layer */
  float dE_out;       /* error on the energy found for the outer layer */
  float t_in;         /* time found for the inner layer */
  float t_out;        /* time found for the outer layer */
  float dt_in;        /* error on the time found for the inner layer */
  float dt_out;       /* error on the time found for the outer layer */
  float i_in;         /* sector rectangular coordinate for the inner layer */
  float j_in;         /* sector rectangular coordinate for the inner layer */
  float i_out;        /* sector rectangular coordinate for the outer layer */
  float j_out;        /* sector rectangular coordinate for the outer layer */
  float di_in;        /* sector rectangular coordinate error, inner layer */
  float dj_in;        /* sector rectangular coordinate error, inner layer */
  float di_out;       /* sector rectangular coordinate error, outer layer */
  float dj_out;       /* sector rectangular coordinate error, outer layer */
  float x_in;         /* lab coordinate, inner layer */
  float y_in;         /* lab coordinate, inner layer */
  float z_in;         /* lab coordinate, inner layer */
  float x_out;        /* lab coordinate, outer layer */
  float y_out;        /* lab coordinate, outer layer */
  float z_out;        /* lab coordinate, outer layer */
  float dx_in;        /* lab coordinate error, inner layer */
  float dy_in;        /* lab coordinate error, inner layer */
  float dz_in;        /* lab coordinate error, inner layer */
  float dx_out;       /* lab coordinate error, outer layer */
  float dy_out;       /* lab coordinate error, outer layer */
  float dz_out;       /* lab coordinate error, outer layer */
  float u2_in;        /* second moment of u inner hit pattern */
  float v2_in;        /* second moment of v inner hit pattern */
  float w2_in;        /* second moment of w inner hit pattern */
  float u2_out;       /* second moment of u outer hit pattern */
  float v2_out;       /* second moment of v outer hit pattern */
  float w2_out;       /* second moment of w outer hit pattern */
  float u3_in;        /* third moment of u inner hit pattern */
  float v3_in;        /* third moment of v inner hit pattern */
  float w3_in;        /* third moment of w inner hit pattern */
  float u3_out;       /* third moment of u outer hit pattern */
  float v3_out;       /* third moment of v outer hit pattern */
  float w3_out;       /* third moment of w outer hit pattern */
  float i2;           /* second moment of overall shower, sector coordinates */
  float j2;           /* second moment of overall shower, sector coordinates */
  float i3;           /* third moment of overall shower, sector coordinates */
  float j3;           /* third moment of overall shower, sector coordinates */
  float spare1;       /* spare */
  float spare2;       /* spare */
  float spare3;       /* spare */
  float spare4;       /* spare */
  int   istat;        /* status word */

} ECShower;

/* functions */

int ecstrips(int *jw, float threshold, int def_adc, int def_tdc, int sector, ECStrip strip[NLAYER][NSTRIP]);
int ecstrips_(int *jw, float *threshold, int *def_adc, int *def_tdc, int *sector, ECStrip strip[NLAYER][NSTRIP]);

int ecpeak(float threshold, ECStrip strip[36], ECPeak peak[18]);
int ecpeak_(float *threshold, ECStrip strip[36], ECPeak peak[18]);

int echit(int io, int sector, int npeak[3], ECPeak peak[3][18], ECHit hit[NHIT]);
int echit_(int *io, int *sector, int npeak[3], ECPeak peak[3][18], ECHit hit[NHIT]);

int eccorr(float th, int def_atten, int io, int sector, int npeak[3], ECPeak peak[3][18], int npsble, ECHit hit[NHIT]);
int eccorr_(float *th, int *def_atten, int *io, int *sector, int npeak[3], ECPeak peak[3][18], int *npsble, ECHit hit[NHIT]);

int ecshower(int nhits, ECHitPtr hit);
int ecshower_(int *nhits, ECHitPtr hit);

int ecbos(int nhits, ECHitPtr hit);
int ecbos_(int *nhits, ECHitPtr hit);

int ecpath(ECParPtr geom, float di, float dj, float path[3]);
int ecxyz(ECParPtr geom, float i, float j, float k, float xyz[3]);
