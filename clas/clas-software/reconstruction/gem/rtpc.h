#define TRUE 1
#define FALSE 0
#define GEMDEBUG FALSE
#define pawc_1 pawc_
#define PAWmem 4000000

/* Global Variables  */
#define LINLMAX  1400 /* maximum number characters per input line */

/* The following (six) lines must be set properly for each group of data files
   that have the same trigger timing 
   For cos4.dat and friends values are 200, 115, 238, 268, 10.0, 30 */

#define NLINE 7  /* Number of ALTRO points at each end of time window to be 
		    used in fitting a straight line for baseline subtraction 
		 CHANGED FROM 10 TO 5 4/14/04 HCF TO ANALYZE PULSER DATA*/
#define BASE_START 5 //starting time bin for calculating baselines

#define NS_PER_TIC 114 /* nanoseconds per ALTRO sample */
#define NAL_CHAN 3328 /* Number of channels of ALTRO data */
#define NUM_PADS 3200 /* Number of actual pads in the RTPC */
#define NAL_SAMP 100 /* Number of time samples from ALTRO */
#define NAL_KEEP NAL_SAMP /* highest  time bin to record*/
#define TPC_TMIN 16 /* earliest TPC track data */
                    //10/21/05 hcf changed from 12 to 15 based on histo 2041
#define TPC_TMAX 68 /* latest TPC track data */
		    //10/21/05 hcf changed from 60 to 68 based on histo 2041
#define SMIN 25.0  /* what a hit's radius gets set to if it is late */ 
#define SMAX 65.0  /* what a hit's radius gets set to if it is early */

#define GLITCH_MAX_CHAN 12
#define GLITCH_THRESH 30  /* more than GLITCH_MAX_CHAN with this charge
			      at one time = discharge event */

/* The status bits for hits in the hitlists */
#define HUNUSED 0  /* unused */
#define HISUSED 1  /* - (was 1) used */
#define HREMOVD 2  /* - (was 2)removed for high residual to helix */
#define HDISEAR 4  /* - discarded (was -1) t<MTPC_TMIN */
#define HDISLAT 8  /* - discarded (was -2) t>TPC_TMAX */
#define HEARLST 16 /* - this hit is has earliest time for this pad on this chain */
#define HABSORB 32 /* - hit absorbed in routine weightData */
#define HSMALLQ 64 /* - (was -3) charge < THR_Q_LINK */
#define HITTEMP 128 /* for routine-specific status marks. */

static int first_call_to_pad_neighbors = 1;
/* HBOOK Common Block Declarations */
static int c_b2 = PAWmem;

struct paw_struct {
  double hmemor[PAWmem];
};

extern struct paw_struct pawc_ ;

  /* The channel mapping within the two types of connectors (even/odd): 
     This turns a pin location within a connector (the index) into an 
     altro channel number (the array element value).
             pcb_map[even_odd][conn_side][conn_col] */
  static int pcb_map[2][2][8] ={ 7, 8,12, 3, 0,15,11, 4, 
				13, 6, 2, 9,10, 1,14, 5, 
				15, 0, 4,11, 8, 7, 3,12,
			 	 5,14,10, 1, 2, 9, 6,13};

  /* The inverse channel map within the two types of connectors (even/odd):
     This turns an altro channel number (the index) into a pad location (value)
     specified by the row/column of that pad within this connector's group of 16.
  */

static int seq_col[2][16] = { 4, 5, 2, 3, 7, 7, 1, 0, 1, 3, 4, 6, 2, 0, 6, 5, /*even*/
                              1, 3, 4, 6, 2, 0, 6, 5, 4, 5, 2, 3, 7, 7, 1, 0};/*odd */
static int seq_row[2][16] = { 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, /*even*/
			      0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0};/*odd */


/* The angular (phi) locations of the centers of the first row of pads on the
   left and right rtpc modules, respectively: */
static float phi_start[2]= { 5.02700, 1.88542 };

/* Rows of pads may be shifted along Z... starting with bottom row on left rtpc
   (top row on right rtpc) and proceeding in increasing phi:

     1st row   no shift in z
     2nd row   shifted by +1.0 mm in z (1mm downstream)
     3rd row   no shift
     4th row   shifted by -2 mm in z (2mm upstream)
     repeat

   So the pad centers of the first pads on each row are at these positions:*/
static float zoff[4]= { -97.85, -96.85, -97.85, -99.85};
 

typedef struct {
  unsigned raw;
  int tim;
  int edg;
  int chn;
} tdc_type;

typedef char linetype[LINLMAX];
typedef char id_name_t[11];
typedef struct {
  int pad;
  int t;
  float r;
  float phi;
  float z;
  float q;
  float del; //distance away from the helix
  int status;
} hitlist; /* all hits in RTPC are stored in this structure */

typedef struct {
  float phi;
  float z;
  float s;
  float charge;
} cylindrical; //this is the raw data type

typedef struct {
  float x;
  float y;
  float z;
  float charge;
} cartesian; //this array is needed to measure the residuals


/* Function Prototypes */
void box3d();
void keystone3d();
void draw_3d_event();
//void correlate();
int readkey();
//int pad_row(float);
//int pad_locs(int, int);
///////5-03-06 added this routine to replace pad_locs.c/////////
void generate_pad_locations();
/////////////////////////////////////////////////////////////////
int baseline2(int);
int redraw_3d_event();
void mark_time();
int real_pad(int);
int scope_plot(int);
int get_bonus_bits();
int make_movie(float,float,float,float[]);
int print_stats();
int interactive_event(int,int,int,int,int,int,float,id_name_t[],float[],float[],float[]);
float dca_to_helix(float,float,float,float,float,float,float,hitlist,cylindrical*,float*);
void helix_fit(int,int,int);
int circle_int(float,float,float,float,float,float,float*,float*,float*,float*);
int inside_rtpc(float, float, float);
float track_charge(int,float*,float*,float*);
int findwall(int, int, float*, float*);
float sumtrackq(int, float*, float*);
int bit_print(int, int);
int bin2deci(int *, int, int);
int read_line(char *string);
int read_line2(char *);
int unpack();
#ifdef __cplusplus
extern "C" int read_BOS();
#else
extern int read_BOS();
#endif
int event_id(float, float, float, float, int);
int chain_track();
int clean_hits();
int removeRes(int, int);
int countPads(int, int);
int weightData(int);
void readMagField();
float bofz(float);
void track_qpad(int,float,float);
void chain_qpad(int);
void sig2xyz(int, float, cartesian*);
void linefit(int, int);
float calcChi(int,int);
int mark_earliest_hits(int chainnum);
//a single pad length in z (mm)
#define DEL_Z 5.0

//the angular coverage of a single pad (rad)
#define DEL_PHI 0.064493

extern int fit_track_to_beamline;

typedef struct {
  int row;
  int col;
  float phi; //in radians!
  float z; //mm
} coordinates;

#define NODATA -1000 //no signal in time sample marker
#define RESCUT 4 //throw away data w/ res over this number
#define MIN_PROT_DQDT_CUT 50
#define MAX_PROT_DQDT_CUT 999999
#define THRESHA 30 //threshold that determines which data from the altro are put into the raw data to fit array
#define THR_Q_LINK 2 // minimum pulse height for possible inclusion in a chain
#define PI 3.1415927
#define GEM_RAD 60 //this is the radius where the first GEM plane is located (in mm)
#define CAT_RAD 30 //radius of cathode (mm)
#define ZEND 100.0 // half-length of rtpc volume (mm)
#define FBANG 74.0*PI/180.0 /* half-angle of phi acceptance of rtpc: 74 degrees */
#define NUM_XTRA    NAL_CHAN-NUM_PADS

#define EVT_TRKS 50
///////////////////////////////////////////////////////////////////////////

///////////////////////////FOR THE CHAIN-LINK TRACKER////////////////////////////////////

#define MAX_NUM_CHAINS 40
#define MAX_HITS_ON_CHAIN 200
#define HH_MAX_NUM_HITS 10000 /* the raw hit structure has this much space 
			       nkb increased 1000 -> 10000 for min-I analysis*/
#define MIN_LINKS_ON_TRACK 15 /* at least this many links to try a track */
#define MAX_LINK_SEP 9.0  /* hits must be this close to be together on a chain (mm)*/
#define WW_PHI_CUT 0.07 //if a given data point is within this phi range of another link
#define WW_RAD_CUT 0.5  //and within this r range, it will be combined
#define WW_Z_CUT 7.1  //and within this z range, it will be combined
#define DCA_CUT 20    //a track must come within this close to the beam line to refit including
                      //the beamline 
hitlist hh_hitlist[HH_MAX_NUM_HITS];/* structure of all hits in the RTPCs and their status */
#define HH_MAX_NUM_XTRS 200 /* the raw xtra structure has this much space */
hitlist hh_xtrlist[HH_MAX_NUM_XTRS];/* structure of all hits in the extra ALTRO channels */
int hh_num_hits; /* count number of hit-chains */
int hh_num_xtrs; /* count number of hits in extra altro channels */

int chain_hits[MAX_NUM_CHAINS][MAX_HITS_ON_CHAIN];
int num_chains, num_hits_this_chain[MAX_NUM_CHAINS];
int track_chain[EVT_TRKS]; /* gives the chain number associated with the indexed track number */

//these variables are for the true proton position determination
//so that we can visualize tracks and fit circles to the real
//positions - taking Lorentz angle into account


  //nkb added on 11-30-05 to keep track of track start & end locations
  int leftCand, rightCand;
  int strt_bfore_bound;
  int strt_aftr_bound;
  int end_bfore_bound;
  int end_aftr_bound;
  int siei;//start inside, end inside
  int soei;//start outside, end inside
  int sieo;//start inside, end outside
  int soeo;//start outside, end outside
int tmax, tmin;

/////these are temporary variables and constants to use the real solenoid field map 
/////to determine the B-field at the tracks avg z position

#define NUM_Z1 21
#define NUM_S1 15
#define NUM_VAL1 315

float z_exit;  //the z position of the the track exit point
float sol_current; //the current of the solenoid in Amperes

//I'm defining these here cause I'm lazy
float sVals1[NUM_S1]; //the array of the s values from the field map
float zVals1[NUM_Z1]; //the array of the z values from the field map
float int_Bs[NUM_S1][NUM_Z1]; //the array of the Br values from the field map
float int_Bz[NUM_S1][NUM_Z1]; //the array of the Bz values from the field map

//////////////////////////////////////////////////////////////////////////////


int n_bb_pulser, n_tb_pulser, n_either_pulser, n_both_pulser;
int nal_samp;/* VARIABLE number corresponding to the above */
int nal_keep;
int num_bad_altro_data;
int ntype[30]; /* counters of event types */
char singleChar[2];
char statfilename[200];
float pad_dat[NAL_SAMP][NUM_PADS], xtra_dat[NAL_SAMP][NUM_XTRA];
int  got_data[NAL_CHAN];
float pad_ped[NAL_SAMP][NAL_CHAN];
int good_altro_data;
char filename[200];
char ev_date[30];
char ev_time[9];
int ev_num;
int quiet; /* whether to print out messages */
int indicate_dead_channels; /* whether to indicate hot/non-reporting channels on SED */
linetype textline;
int holder;

coordinates padLocs[NUM_PADS];
float dzds[EVT_TRKS], del3d[MAX_HITS_ON_CHAIN];
float track_theta, track_phi, track_vertex;

int ntracks;
float x_0[EVT_TRKS], y_0[EVT_TRKS],r_0[EVT_TRKS], phi0[EVT_TRKS];
float x_close[EVT_TRKS], y_close[EVT_TRKS], z_close[EVT_TRKS];
float dca[EVT_TRKS]; /* distance of closest approach of track to beamline */
float chi2[EVT_TRKS]; /* total chi**2 of track fit */
float p_perp, p_para, p_x, p_y, p_tot;
cartesian point_entry, point_exit, point_appear, point_disappear;
int doFits;

float totIntQ; //total integrated charge
float totIntQ_L, totIntQ_R; //left and right of chamber

cylindrical ePath[NAL_SAMP];
int n_pathpts;

int num_events, num_read;
float qtot_early, qtot_intime, qtot_late;
cylindrical rawCYL[NAL_SAMP][NUM_PADS]; /*holds the signal data for the first linefit
	                      a NODATA in charge indicates there is no data*/
cartesian rawXYZ[NAL_SAMP][NUM_PADS]; /*holds the signal data for the first linefit
	                      a NODATA in charge indicates there is no data*/

int hotPads[NAL_CHAN], num_hot_pads, processPad, done;
  int lookatPad[NUM_PADS];
  int num_pads_hit; //stands for number of pads hit
int printPads;
int fit_track_to_beamline;
int helix_error;//to tell us if the track curves too much
float m_y[EVT_TRKS], b_y[EVT_TRKS], lineChi2[EVT_TRKS];//for those tracks which we think are straight

///////3-29-06 nate added these variables and functions to replace read_path.c///////

void setPadTimes(float, float*, int, float*);
int init_paths();
float getS(float, float, int);
float getPhi(int, float, int, float);
void calcVolt(float, float, float, float);
float Lvolt, Rvolt;
float he;

