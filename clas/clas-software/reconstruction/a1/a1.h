/*
 * a1.h
 *
 * Written by Joe Manak, April 1997
 *
 * Produce analyzed groups
 *
*/

/* -=======================================================- *
$Id: a1.h,v 1.68 2008/05/06 16:35:24 goetz Exp $
$Author: goetz $
$Revision: 1.68 $
$Date: 2008/05/06 16:35:24 $
* -=======================================================- */

 

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <clasmdl.h>
#include <dc_tcl.h>
#include <trk_run_control.h>
#include <evnt_par.h>
#include <utility.h>

/* FORTRAN COMMON BLOCKS - YUK!!! */
extern clasmdl_t clasmdl_; /*still need this for seb*/

/*  defines */

/*bitwise processing flags*/
#define PROCESS_HBTR  BIT(0)
#define PROCESS_SC      BIT(1)
#define PROCESS_HBID  BIT(2)
#define PROCESS_EC      BIT(3)
#define PROCESS_CC     BIT(4)
#define PROCESS_TBTR BIT(5)
#define PROCESS_CL01 BIT(6)
#define PROCESS_TBID BIT(7)
#define PROCESS_SEB BIT(8)
#define PROCESS_LAC BIT(9)
#define PROCESS_TAGGER BIT(10)
#define PROCESS_ST BIT(11)
#define PROCESS_TRKS_HBID BIT(12)
#define PROCESS_TRKS_TBID BIT(13)
#define PROCESS_REGION1 BIT(14)
#define PROCESS_DCDW BIT(15)
#define PROCESS_TAGM BIT(16)
#define PROCESS_GPID BIT(17)
#define PROCESS_TBID_NOST BIT(18)

/*bitwise drop flags*/
#define DROP_RAW BIT(0)
#define DROP_DC0 BIT(1)
#define DROP_DC1 BIT(2)
#define DROP_HBLA BIT(3)
#define DROP_TBLA BIT(4)
#define DROP_HBTB BIT(5)
#define DROP_SC BIT(6)
#define DROP_EC BIT(7)
#define DROP_HBID BIT(8)
#define DROP_CL01 BIT(9)
#define DROP_SEB BIT(10)
#define DROP_TBID BIT(11)
#define DROP_HDPL BIT(12)
#define DROP_LAC BIT(13)
#define DROP_CC BIT(14)
#define DROP_ST BIT(15)
#define DROP_DHCL BIT(16)
#define DROP_BANKS BIT(17)
#define DROP_HISS BIT(18)
#define DROP_TDPL BIT(19)

#define CC_BANKS "CCRCCC01"
#define SEB_BANKS "HEVTEVNTDCPBSCPBCCPBUNUSEVHBTRKSSTPBTGPBLCPBECPB"
#define TAGGER_BANKS "TAGRTAGIPSO "
#define SC_BANKS "SC1 SCR SCRC"
#define EC_BANKS "EC01ECHBECPIECPC"
#define ST_BANKS "ST1 STR "
#define REGION1_BANKS "RGLK"
#define DROP_LIST "TDPLHBTRDCLADTRKHBERSC1 SCR EC01ECPITAGIST1 ECPCTRL1"
#define HISS_BANKS "HISSHISIHISF"

/* extra options bits */

#define TRKS_OPTION BIT(0)

/* -------------Prototypes------------------------------- */

void options(char *processName);
int ProcessEvent(int ProcessFlag, int OptionsFlag, int PIDnumber);
int initRun(int runNo, int ProcessFlag);
int initialize_analyzer(int verbose);
int fill_clasmdl();
int WriteEvent(char *select);
void bosta();
void dumpBuffer(int *a,int n);
int ConfigureEvent(int ProcessFlag,int DefaultRun);
void PrintUsage(char *processName);
static void signalINT(int isig);
static void signalSEGV(int isig);
int ScalerEvent(clasHEAD_t *hdr);
char display_char(int c);
int make_T_list(int DropFlag, char *eliminate_list);
char *FileName(char *input,char *path,char *base,int nfile);
int CompressDC0(); 
int BosWriteFail(void *ptr,size_t size, size_t nitems, FILE *stream);
int rernev_(int *runno, int *recordno, int *nevent); /* to blot out rernev in recutl */
int install_fault_handlers();
void parseTargetPosition(char *str);
char *runType();

