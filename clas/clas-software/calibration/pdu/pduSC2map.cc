#include <iostream.h>

#define NSECTOR 6
#define CHANNELS 6912
#define WIRES_PER_LAYER 192

#define BAD_STATUS 1
#define GOOD_STATUS 888

#define BAD_SC 1
#define GOOD_SC 0
#define NPADDLES 48
#define LR 2
#define ATS 2
#define ADC 0
#define TDC 1
#define L 0
#define R 1

int Nout = 0;

/* ----------- Function prototypes ---------------- */

extern "C" {

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

/*--------------- function prototypes ----------------*/

int ProcessEvent(int RunNo,int verbose);
void PrintUsage(char *processName);
void initbos();
int fparm_c(char *mess);
int getBOS(BOSbank *bcs, int lun, char *list);
void cleanBanks(BOSbank *bcs);
void dropAllBanks(BOSbank *bcs, char *list);
void *getBank(BOSbank *,const char *);

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
  int i,j,a,b,c,d;
  char *argptr;
  int Nevents = 0;
  int verbose = 0;
  float pbeam;
  int RunNo = 1;
  int status[NSECTOR *  CHANNELS];
  char *dir,def_map[100],Sector[100];
  int sector,layer,wire;
  char pathology;
  int padSTAT[NSECTOR][NPADDLES][ATS][LR];
  int adcl,adcr,tdcl,tdcr,isec,id,aot,lor;   
  programMode_t mode;
  int force = 0;

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
    for (i = 0; i < NSECTOR * CHANNELS; ++i)
      status[i] = GOOD_STATUS;


    while (cin >> sector >> layer >> wire >> pathology) {
      Nevents++;
      switch (pathology) {
      case 'd':
	Index = (sector - 1) * CHANNELS + (layer -1) * WIRES_PER_LAYER + wire - 1;
	status[Index] = BAD_STATUS;
	break;
      }
    }
    dir = getenv("CLAS_PARMS"); 
    sprintf(def_map,"%s/Maps/DC_STATUS.map",dir); 
    for (i = 0; i  < NSECTOR; ++i) {
      sprintf(Sector,"sector%1d",i  + 1);
      map_put_int(def_map,Sector,"status", CHANNELS, &status[i * CHANNELS], RunNo);
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
	padSTAT[sector-1][id -1][ADC][L]=BAD_SC;
      if (adcr==0)
	padSTAT[sector-1][id - 1][ADC][R]=BAD_SC;
      if (tdcl==0)
	padSTAT[sector-1][id - 1][TDC][L]=BAD_SC;
      if (tdcr==0)
	padSTAT[sector-1][id - 1][TDC][R]=BAD_SC;
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
  cerr << "Usage: " << processName << " < [elements file name]\n\n";
  cerr << "\t-R\tRun number configuration to implement bad elements\n";
  cerr << "\t-D\tadjust DC status map to include bad elements\n";
  cerr << "\t-S\tadjust SC status map to include bad elements\n";
  cerr << "\t-F\tForce overwrite of status map\n";
  exit(0);
}


