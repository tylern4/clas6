/*----------------Constants----------------------*/
#define MEMH 1000000
#define NUMBINS 500
#define SUPERLAYERS 6
#define LAYERS 36
#define MAXPEAKS 15
#define SECTORS 6
#define REGIONS 3
#define WIRES 192

/*---------------- typedefs ----------------*/

typedef struct {
  int Min;
  int Max; 
} Llimits_t;

/* ----------- Function prototypes ---------------- */
int Welcome();

int ProcessEvent();

void ctrlCHandle(int);

void PrintUsage(char *processName);

void book_histos();

void hini(char *out);

int InitWireLimits(Llimits_t *);

int CountHits(int OutHits[SECTORS][LAYERS-2][WIRES]);

int FlagBadWires(int Hits[SECTORS][LAYERS-2][WIRES], int WireFlag[SECTORS][LAYERS][WIRES]);

int WeightAve(int Hits[SECTORS][LAYERS-2][WIRES], int NomOcc[SECTORS][SUPERLAYERS]);

void PrntMessage(char *Mess, FILE *fp, int BannR);

int get_run_number();

int ReadMapStatus(int WireFlag[SECTORS][LAYERS][WIRES], int runno );

int ReportChange(int WireFlag[SECTORS][LAYERS][WIRES], 
		 int PreviousWireFlag[SECTORS][LAYERS][WIRES], FILE *);
