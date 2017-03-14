typedef struct {
	int version;			//  Version Number //
	int nrun;			//  Run Number (monotonically increasing) //
	int nevent;			//  Event Number (starting with 1 at run begin, //
	int time;			//  Event Time (UNIX time = seconds as of January 1,1970) //
	int type;			//  Event Type (Defined by on-line system or MC run: //

	int roc;			//  32 bit readout controller status //
	int evtclass;			//  Event Classification from DAQ: //
	int trigbits;			//  Trigger Latch Bits //
} head_t;

typedef struct {
//	bankHeader_t bank;
	head_t head[1];
} clasHEAD_t;
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

typedef struct {
	float cx;			//  x dir cosine at track origin
	float cy;			//  y dir cosine
	float cz;			//  z dir cosine
	float pmom;			//  momentum 
	float mass;			//  mass 
	float charge;			//  charge 
	int id;				//  track Particle Data Group id 
	int flag;			//  track flag 
	int beg_vtx;			//  beginning vertex number 
	int end_vtx;			//  ending vertex number
	int parent;			//  parent track 
} mctk_t;

typedef struct {
//	bankHeader_t bank;
	mctk_t mctk[1];
} clasMCTK_t;
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

typedef struct {
	float x;			//  x of vertex
	float y;			//  y 
	float z;			//  z 
	float tof;			//  secs of flight 
	int flag;			//  vertex flag 
} mcvx_t;

typedef struct {
//	bankHeader_t bank;
	mcvx_t mcvx[1];
} clasMCVX_t;
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

typedef struct {
	int pid;
	float x;
	float y;
	float z;
	float E;
	float px;
	float py;
	float pz;
	float q;
	int trkid;
	float qpid;
	float qtrk;
	int flags;
} part_t;

typedef struct {
	part_t part[1];
} clasPART_t;
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

typedef struct {
	float ERG;
	float TTAG;
	float TPHO;
	int STAT;
	int T_id;
	int E_id;
} tagr_t;

typedef struct {
	tagr_t tagr[1];
} clasTAGR_t;
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

typedef struct {
	int NRUN;
	int NEVENT;
	int TIME;
	int TYPE;

	int REACTYPE;
	float WEIGHT;
	float W;
	float Q2;
	float E_PHOT;
	float PX_PHOT;
	float PY_PHOT;
	float PZ_PHOT;
	float E_TARG;
	float PX_TARG;
	float PY_TARG;
	float PZ_TARG;
} mchd_t;

typedef struct {
	mchd_t mchd[1];
} clasMCHD_t;
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

typedef struct {
	int v_id;
	float ntrk;
	float x;
	float y;
	float z;
	float chi2;
	float cxx;
	float cxy;
	float cxz;
	float cyy;
	float cyz;
	float czz;
	int stat;
} mvrt_t;

typedef struct {
	mvrt_t mvrt[1];
} clasMVRT_t;
