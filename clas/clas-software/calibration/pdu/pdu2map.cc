#include <iostream.h>

int Nout = 0;

/* ----------- Function prototypes ---------------- */

extern "C" {

#include <status.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <bosddl.h>
#include <particleType.h>
#include <map_manager.h>

typedef struct {
  int Min;
  int Max; 
} Llimits_t;

/*--------------- function prototypes ----------------*/

int ProcessEvent(int RunNo,int verbose);
void PrintUsage(char *processName);
void initbos();
int fparm_c(char *mess);
int getBOS(BOSbank *bcs, int lun, char *list);
void cleanBanks(BOSbank *bcs);
void dropAllBanks(BOSbank *bcs, char *list);
void *getBank(BOSbank *,const char *);
int InitWireLimits(Llimits_t *);

extern BOSbank bcs_;

}


/* --------------------------------------------------- */

typedef enum {DCpdu,SCpdu} programMode_t;

void ProgramEnd()
{

  ;
}

main(int argc,char **argv)
{
  int Index;
  FILE *fp = NULL;
  int i,j,k,a,b,c,d;
  char *argptr;
  int Nevents = 0;
  int verbose = 0;
  float pbeam;
  int RunNo = 1;
  int status[NSECTOR *  CHANNELS];
  char *dir,def_map[100],Sector[100];
  int sector,layer,wire;
  int pathology;
  int padSTAT[NSECTOR][NPADDLES][ATS][LR];
  int adcl,adcr,tdcl,tdcr,isec,id,aot,lor;   
  programMode_t mode;
  int force = 0;
  char *parms = "CLAS_PARMS";
  Llimits_t WireLimits[36];

  if(argc == 1) {
    PrintUsage(argv[0]);
    exit(0);
  }
  for (i=1; i<argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {
      case 'h':
	PrintUsage(argv[0]);
	break;
      case 'v':
	verbose = 1;
	break;
      case 'R':
	RunNo = atoi(++argptr);
	break;
      case 'D':
	mode = DCpdu;
	break;
      case 'S':
	mode = SCpdu;
	break;
      case 'F':
	force = 1;
	break;
      default:
	fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
	PrintUsage(argv[0]);
	break;
      }
    }
  }
  switch(mode){
  case DCpdu:
    InitWireLimits(WireLimits);
    for (i = 0; i < NSECTOR * CHANNELS; ++i)
      status[i] = DC_GOOD_WIRE;

    /* flag the wires that Do Not Exist */
    for (i = 0 ; i < NSECTOR; ++i){
      for(j = 0; j < 36;  ++j){                  /* loop over layers */
	for(k = 0; k < WireLimits[j].Min-1; ++k){   /* loop over wires  */
	  Index = i * CHANNELS + j * WIRES_PER_LAYER + k;
	  status[Index] = DC_DNE;
	}
	for(k = WireLimits[j].Max; k < 192; ++k){
	  Index = i * CHANNELS + j * WIRES_PER_LAYER + k;
	  status[Index] = DC_DNE;
	}
      }
      /* R1 phantom layers : */
      for (j = 4 ; j < 6 ; ++j){
	for (k = 0; k < WIRES_PER_LAYER ; ++k){
	  Index = i * CHANNELS + j * WIRES_PER_LAYER + k;
	  status[Index] = DC_DNE;
	}
      }
    }
    
    /* read the pdu bad wire list */
    while (cin >> sector >> layer >> wire >> pathology) {
      Nevents++;
      switch (pathology) {
      case DC_DEAD_WIRE:
	Index = (sector - 1) * CHANNELS + (layer -1) * WIRES_PER_LAYER + wire - 1;
	if (Index > NSECTOR * CHANNELS)
	  fprintf(stderr,"YIKES!!!! %d %d %d\n",sector,layer,wire);
	else
	  status[Index] = DC_DEAD_WIRE;
	break;
      }
    }
    //}
    dir = getenv(parms); 
    sprintf(def_map,"%s/Maps/DC_STATUS.map",dir); 
    for (sector = 0; sector  < NSECTOR; ++sector) {
      sprintf(Sector, "sector%1d", sector + 1);
      map_put_int(def_map, Sector, "status", CHANNELS, &status[sector * CHANNELS], 
		  force ? -RunNo : RunNo);
    }
    break;

  case SCpdu:
    for (a=0;a<LR;++a){
      for (b=0;b<ATS;++b){
	for (c=0;c<NPADDLES;++c){
	  for (d=0;d<NSECTOR;++d){
	    padSTAT[d][c][b][a]=GOOD_SC;
	  }
	}
      }
    }

 
    while (cin >> sector >> id >> adcl >> adcr >> tdcl >> tdcr) {
      if (adcl==0)
	padSTAT[sector-1][id][ADC][L]=BAD_SC;
      if (adcr==0)
	padSTAT[sector-1][id][ADC][R]=BAD_SC;
      if (tdcl==0)
	padSTAT[sector-1][id][TDC][L]=BAD_SC;
      if (tdcr==0)
	padSTAT[sector-1][id][TDC][R]=BAD_SC;
    }
    dir = getenv("CLAS_PARMS"); 
    sprintf(def_map,"%s/Maps/SC_STATUS.map",dir); 
    for (i = 0; i  < NSECTOR; ++i) {
      sprintf(Sector,"sector%1d",i  + 1);
      map_put_int(def_map,Sector,"status", (4*NPADDLES),&padSTAT[i][0][0][0], force ? -RunNo : RunNo);
    }
    break;
  }
}
void PrintUsage(char *processName)
{
  cerr << "Usage: " << processName << " [options] < [faulty elements file name]\n\n";
  cerr << "\toptions:\n";
  cerr << "\t-R#\tRun number # (default, # = 1) configuration, to implement bad elements\n";
  cerr << "\t-D\tadjust DC status map to include bad elements\n";
  cerr << "\t-S\tadjust SC status map to include bad elements\n";
  cerr << "\t-F\tForce overwrite of status map\n";
  cerr << "\t-h\tPrint this message.\n";
  exit(0);
}


