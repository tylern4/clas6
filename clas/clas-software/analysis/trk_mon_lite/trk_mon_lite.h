
#include <iostream>
#include <iomanip>
using namespace std;

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>

extern "C" {
#include <ntypes.h>
#include <bosddl.h>
#include <utility.h>
#include <ntypes.h>
#include <bostypes.h>
#include <makebanks.h>
#include <clas_cern.h>
#include <dc.h>
#include <itape.h>
#include <map_manager.h>
}

extern int DONE;
extern float LOCANGLE_CUT_HIGH[4],LOCANGLE_CUT_LOW[4];
extern int RUNNUMBER;
extern char OUTPUTFILE[256];
extern int LOCAL_ANGLE_HISTOS_ONLY, AUTOFIND_LOCAL_ANGLE_CUTS;
extern int UPDATE_QUALITY_DATABASE;
extern int TOTAL_NTUPLE_ROWS[7];
extern int MAX_NTUPLE_ROWS;
extern int PROTON_CUT;
extern int SINGLE_SECTOR;
extern float VERTEX_CUT_LOW;
extern float VERTEX_CUT_HIGH;

/*********************************************************/
/* Data structure to hold info on cuts.                  */
/*                                                       */
/* The test will be:                                     */
/*                                                       */
/*   val[field]*factor<limit                             */
/*                                                       */
/* So if the desired test is a "less than" test, then    */
/* factor=+1.0 and limit is just the limit passed on     */
/* the command line.                                     */
/*                                                       */
/* If the desired test is a "greater than" test, then    */
/* factor=-1.0 and limit is the negative of what was     */
/* passed on the command line. This way, the test really */
/* just requires one extra multiplication at event time  */
/* making the program faster.                            */
/*********************************************************/
typedef struct {
   int field;
   float limit;
   float factor;
}tm_ntuple_cut_t;

typedef struct {
  float lower;
  float upper;
}tm_mass_cut_t;


// Routines
int hist_book(int runnumber);
int hist_fill(void);
int hist_end(void);
void tm_set_ntuple_cut(char *arg);
int tm_pass_ntuple_cut(float *vals);
void tm_set_mass_upper(float upper);
void tm_set_mass_lower(float lower);
void tm_use_mass_cuts(void);
int UpdateQualityDB(void);

// Other CLAS routines which need declaring (why are there header files for these?!)
extern "C" {
	void bnames_(int *maxnames);
	int initFile(char *filename);
	int getData(BOSbank *bcs,char *list);
	
	// CERNLIB
	void rzpurg_(int*);
}
