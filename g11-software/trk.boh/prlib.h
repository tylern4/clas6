#ifndef _PRLIB_

#ifdef	__cplusplus
extern "C" {
#endif


#undef SL5

/* prlib.h - header file for prlib package */

#define NLAY     6        /* the number of layers in the superlayer */
#define NTRACK   500      /* the maximum number of tracks in ... */
#define NFOUND   200      /* the maximum number of segments in ... */
#define NS2      142
#define MAX_NPAT 200
#define NLISTAX  256*64*256  /* 22 bits */
#define NLISTST  256*128*128  /* 22 bits */
#define NO_HIT   0

/* definitions for SDA related functions - have to be consistent with SDA !!! */

#define nsgmx  60
#define nsgcmx 30
#define nclmx  100
#define ntrmx  500
#define nlnkvect 16
#define nlnkec   12
#define NVEC 11

/* accuracy */

#define VECT1 10.         /* x -  1 mm */
#define VECT2 10.         /* y -  1 mm */
#define VECT3 1.          /* z -  1 cm */
#define VECT4 100.        /* dx - 1 % */
#define VECT5 100.        /* dy - 1 % */
#define VECT6 100.        /* dz - 1 % */
#define VECT7 1000.       /* p -  1 MeV/c */

#define P0    0.02        /* GeV/c */
#define DP    0.025       /* 2.5 % (acctual PR resolution about 8-11 %) */

/* segment structures */

typedef struct segmdict
{
  int   ipat[6];
} SEGMdict;

typedef struct
{
  unsigned nwhit    : 2;
  unsigned sign0    : 1;
  unsigned iw0      : 4;
  unsigned sign1    : 1;
  unsigned iw1      : 4;
  unsigned sign2    : 1;
  unsigned iw2      : 4;
  unsigned sign3    : 1;
  unsigned iw3      : 4;
  unsigned sign4    : 1;
  unsigned iw4      : 4;
  unsigned sign5    : 1;
  unsigned iw5      : 4;
} DCsegm;

/* could we use that ???
typedef struct
{
  signed nwhit : 2;
  signed iw0   : 5;
  signed iw1   : 5;
  signed iw2   : 5;
  signed iw3   : 5;
  signed iw4   : 5;
  signed iw5   : 5;
} DCsgm;
typedef struct
{
  signed nwhit : 2;
  signed iw[6] : 5;
} DCsgm;
*/


/* road structures */

#define MAXNPAT 10

typedef struct linksegm
{
  char  npat[6];
  char  ipat[6][MAXNPAT];
} LINKsegm;





typedef struct
{
  unsigned path : 24;
  unsigned p    : 8; /* for reconstruction only */

  union
  {
    struct
    {
      unsigned p  : 16; /* for simulation only */
      unsigned np : 14;
      signed charge : 2;
    } s;
    struct
    {
      unsigned gr0  : 5;
      unsigned gr1  : 5;
      unsigned gr2  : 5;
      unsigned gr3  : 5;
      unsigned gr4  : 5;
      unsigned gr5  : 5;
      signed charge : 2;
    } r;
  } u;

#ifdef ONLINE
  unsigned ntof : 6;
  unsigned nu   : 7;
  unsigned nv   : 7;
  unsigned nw   : 7;
  unsigned res  : 5;

  short x;
  short y;

  char  z;
  char  dx;
  char  dy;
  char  dz;
#endif

#ifdef SIM
  char  npat[6];
  char  ipat[6][MAXNPAT];
#endif

} DCroad;




typedef struct
{
  int           nroads;
  unsigned char min[4];
  unsigned char max[4];
  DCroad        *road;
} DCstereo;
typedef DCstereo DCaxial;






/* track structures */

#define MINHITSEGM 3

typedef struct eccluster
{
  float u;
  float v;
  float w;
  float e;
} ECCLUSTER;

typedef struct prsegment
{
  int   nw;
  short iw[6];
  short tdc[6];
} PRSEGMENT;

typedef struct prcluster
{
  int       nsegment;
  int       iwmin;
  int       iwmax;
  PRSEGMENT segment[MAX_NPAT];
} PRCLUSTER;

typedef struct prtrack
{
  int       sector;
  PRCLUSTER cluster[6];
  int       ntrk;
  float     vect[6];
  float     p;
  int       charge;
  int       ntof;
  int       nu;
  int       nv;
  int       nw;
  ECCLUSTER ec[3];
} PRTRACK;


/* info about tracks in current sector - for run time only */
typedef struct sectrk
{
  int ntrack_firstinsector;
} SECTRK;




void prinit(char *filename, int lanal1, int lanal2, int lanal5);
int prlib(int *iw, PRTRACK *track);
int eclib(int *iw, const float threshold[3], int *ntrk, PRTRACK *trk);
int prbos(int *iw, int *ntrk, PRTRACK *trk);







#ifdef	__cplusplus
}
#endif

#define _PRLIB_
#endif







