#ifndef __BOSH
#define __BOSH

#include <stdio.h>

#define  NDIM  50000

#ifndef __DEFINEDBOOLEAN
#define __DEFINEDBOOLEAN
typedef enum {False, True} Boolean;
#endif

typedef struct bosbank {
  int    pad[5];
  int    iw[NDIM];
} BOSBank;

/* structure for holding DC hits */

typedef struct dcdata  *DCDataPtr;

typedef struct dcdata
{
  unsigned short id;
  unsigned short tdc;
  unsigned short adc;
} DCData;

/* structure for holding DC0 hits */

typedef struct dc0data  *DC0DataPtr;

typedef struct dc0data
{
  unsigned short id;
  unsigned short tdc;
} DC0Data;

/* structure for holding EC hits.
   The layer is defined as

   1-6 (uvw inner, uvw outer) for FORWARD ANGLE 
   1-4 (xy inner, xy outer) for LARGE ANGLE */

typedef struct ecdata  *ECDataPtr;

typedef struct ecdata
{
/*
#ifdef __ultrix
  unsigned char  strip;
  unsigned char  layer;
#else
  unsigned char  layer;
  unsigned char  strip;
#endif
*/
  unsigned short id;
  unsigned short tdc;
  unsigned short adc;
} ECData;

typedef struct ec1data  *EC1DataPtr;

typedef struct ec1data
{
#ifdef __ultrix
  unsigned char  strip;
  unsigned char  layer;
#else
  unsigned char  layer;
  unsigned char  strip;
#endif
  unsigned short tdcleft;
  unsigned short adcleft;
  unsigned short tdcright;
  unsigned short adcright;
} EC1Data;

/* analyzed EC "pixel" banks */

typedef struct pixdata *PixDataPtr;

typedef struct pixdata
{
  int      id;
  int     Uid;
  int     Vid;
  int     Wid;
  float   E;    /*GeV*/
  float   T;    /*ns*/
  float   X;    /*GeV*/
  float   Y;    /*cm*/
  float   Z;    /*cm*/
} PixData;


/* data structure for any component with the standard id, tdc, adc format */

typedef struct normaldata  *NormalDataPtr;

typedef struct normaldata
{
  unsigned short id;
  unsigned short tdc;
  unsigned short adc;
} NormalData;

/* data structure for scintillator data */

typedef struct scdata  *SCDataPtr;

typedef struct scdata
{
  unsigned short id;
  unsigned short tdcleft;
  unsigned short adcleft;
  unsigned short tdcright;
  unsigned short adcright;
} SCData;

/* data structure for HEAD data bank */

typedef struct headdata  *HEADDataPtr;

typedef struct headdata
{
  int        VersionNumber;
  int        RunNumber;
  int        EventNumber;
  int        EventTime;
  int        EventType;
  int        ROCStatus;
  int        EventClass;
  int        EventWeight;
} HEADData;

/* data structure for MC input data bank */

typedef struct mcindata  *MCINDataPtr;

typedef struct mcindata
{
  float        Xo;
  float        Yo;
  float        Zo;
  float        Px;    /* direction cosine, i.e. Px/P */
  float        Py;    /* direction cosine, i.e. Py/P */
  float        Pz;    /* direction cosine, i.e. Pz/P */
  float        P;
  float        M;
  float        Q;
  int          LundID; 
} MCINData;

/* data structure for MCVX input data bank */

typedef struct mcvxdata *MCVXDataPtr;

typedef struct mcvxdata 
{
  float        X;
  float        Y;
  float        Z;
  float        tof;
  int          vtx_flag;
} MCVXData ;

/* data structure for holding the random number starting seeds */

typedef struct mcevdata *MCEVDataPtr;

typedef struct mcevdata {
  int          seed1;
  int          seed2;
} MCEVData;

/* data structure for MCPA */

typedef struct mcpadata *MCPADataPtr;

typedef struct mcpadata {
  float        dum1;
  float        dum2;
} MCPAData;

/* data structure for MCTK input data bank */

typedef struct mctkdata *MCTKDataPtr;

typedef struct mctkdata
{
  float        Cx ;
  float        Cy ;
  float        Cz ;
  float        P ;
  float        M ;
  float        Q ;
  int          lund_id ;
  int          track_flag ;
  int          beg_vtx ;
  int          end_vtx ;
  int          parent ;
} MCTKData ;

/* structure for cc */

typedef struct ccdata *CCDataPtr;

typedef struct ccdata {
  float        Xi;   /* init point in CC (x) */
  float        Yi;   /* init point in CC (y) */
  float        Zi;   /* init point in CC (z) */
  float        Xf;   /* final point in CC (x) */
  float        Yf;   /* final point in CC (y) */
  float        Zf;   /* final point in CC (z) */
  float        P;    /* paricle momentum */
  int          num;  /* particle number */
  int          trk;  /* track number (1000*istak + itra) */
  int          cseg; /* CC segment number (or 0 if there are no hits) */
  int          itdc; /* in channels (or 0 if no hits) */
  int          hflg; /* 1 for a hit; 0 otherwise */
} CCData ;     

/* structure for cc1 */

typedef struct cc1data *CC1DataPtr;

typedef struct cc1data {
  float        Xi;   /* init point in CC (x) */
  float        Yi;   /* init point in CC (y) */
  float        Zi;   /* init point in CC (z) */
  float        Xf;   /* final point in CC (x) */
  float        Yf;   /* final point in CC (y) */
  float        Zf;   /* final point in CC (z) */
  float        P;    /* paricle momentum */
  int          num;  /* particle number */
  int          trk;  /* track number (1000*istak + itra) */
  int          cseg; /* CC segment number (or 0 if there are no hits) */
  int          itdc; /* in channels (or 0 if no hits) */
  int          hflg; /* 1 for a hit; 0 otherwise */
} CC1Data ;     


/* structure for GEANT CC hits  */

typedef struct cchdata  *CCHDataPtr;

typedef struct cchdata {
  float        Xi;   /* init point in CC (x) */
  float        Yi;   /* init point in CC (y) */
  float        Zi;   /* init point in CC (z) */
  float        Xf;   /* final point in CC (x) */
  float        Yf;   /* final point in CC (y) */
  float        Zf;   /* final point in CC (z) */
  float        P;    /* paricle momentum */
  int          num;  /* particle number */
  int          trk;  /* track number (1000*istak + itra) */
  int          cseg; /* CC segment number (or 0 if there are no hits) */
  int          itdc; /* in channels (or 0 if no hits) */
  int          hflg; /* 1 for a hit; 0 otherwise */
} CCHData ;

/*  structures for GEANT EC hits */

typedef struct echdata *ECHDataPtr;

typedef struct echdata {
  float        X;    /* coord of hit (x) */
  float        Y;    /* coord of hit (y) */
  float        Z;    /* coord of hit (z) */
  float        cx;   /* */
  float        cy;
  float        cz;
  float        P;    /* momentum */
  int          trkNum; /* track num = 1000*istak+itrak */
  int          pdg_id; /* ??????? */
} ECHData ;

/* struct for GEANT Start Counter Hits */
  
typedef struct sthdata *STHDataPtr;

typedef struct sthdata {
  float        X;    /* coord of hit (x) */
  float        Y;    /* coord of hit (y) */
  float        Z;    /* coord of hit (z) */
  float        cx;   /* */
  float        cy;
  float        cz;
  float        P;    /* momentum */
  int          trkNum; /* track num = 1000*istak+itrak */
  int          pdg_id; /* ??????? */
} STHData ;

typedef struct schdata *SCHDataPtr;

typedef struct schdata {
  float        X;    /* coord of hit (x) */
  float        Y;    /* coord of hit (y) */
  float        Z;    /* coord of hit (z) */
  float        cx;   /* */
  float        cy;
  float        cz;
  float        P;    /* momentum */
  int          trkNum; /* track num = 1000*istak+itrak */
  int          pdg_id; /* ??????? */
} SCHData ;

typedef struct dchdata *DCHDataPtr;

typedef struct dchdata {
  float        X;    /* coord of hit (x) */
  float        Y;    /* coord of hit (y) */
  float        Z;    /* coord of hit (z) */
  float        step;
  float        dedx;
  float        ptot;
  float        tofg;
  float        cx;   /* */
  float        cy;
  float        cz;
  int          trkNum; /* track num = 1000*istak+itrak */
  int          pdg_id; /* ??????? */
  int          wlayer;  /* wire layer, 1-36 */
} DCHData ;


/*------ global variables ------*/

extern   int     bosNumEvents;
extern   char   *bosFileName;
extern   char   *bosDataName;
extern   int     bosNdim;
extern   int     bosUnit;

extern   BOSBank  bcs_;    /* main bos common */  unsigned char  layer;

/*------ local prototypes ----- */


/*------ bos_utilities.c ------*/


#endif











