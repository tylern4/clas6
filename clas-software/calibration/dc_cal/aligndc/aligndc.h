/* aligndc.h   whatever is needed by all in aligndc
 *    created 8-Jun-98
 *    Rob Feuerbach
 */


typedef struct hit_stx {     /* hit type: what is kept for each hit */
  short int layer;         /* input from TBLA banks */
  short int wire;
  float tdoca;
  float sigma;
  float trsigma;
} hit_t;

typedef struct track_stx {  /* track type: contains all the hits and parameters */
  int event; /* event number the track came from (to get back to) */
  float chi2;        /* chi2 from the original cooking program (user_align), from TBER/TBTR */
  char number;        /* track number */
  char sector;        /* track sector */
  float weight;      /* weight applied to the track in chi2 */
  vector3_t dir;     /* unit vector in direction of the track */
  vector3_t pos;     /* position at initial point (from TBER) */
  float X[5][3];     /* partial derivative of Xi wrt Qj [i][j] */
  float V[5][3];     /* partial derivative of Vi wrt Qj [i][j] */
  float sig[5][5];   /* covariance matrix from TBER bank */
  hit_t hits[34];    /* array of hits */
  struct track_stx *next;
} track_t;

/* global control variables */
extern int Sector[];     /* sector list to align up coherently
			  */

extern int RegMove[3];   /* region to adjust to align and to calc chi2 from
			  */

extern int MaxHoles[6];  /* max number of "fake" hits to permit in SL
			  */

extern int ndim;         /* number of dimension to fit to
			  */

extern int dim[6];       /* array of a list of the dimensions to fit
			  */

extern int layertype;    /* type of layers used in minimization
			  * (1=AX,2=ST,3=AX+ST)
			  */

extern int nevent;       /* number of events to read in from the file
			  */

extern int jumpevnt;     /* number of events to skip before processing nevent's
			  */

extern float weightmult[3]; /* times larger the sigma is than it should be
			     */

extern int WeightOn;     /* whether to use dynamic weighting by theta or not
			  */
extern int debug;        /* debugging output on
			  */


/* other global variables */
extern char **dimlabel;  /* array of pointers to labels */
extern float xpos[3][18];    /* initial reference offsets */
extern float sdir[3][18];
extern float xt[3],st[3];    /* initial torus offsets */
extern track_t *toptrack;    /* topmost track in the linked list */
extern char *command;
