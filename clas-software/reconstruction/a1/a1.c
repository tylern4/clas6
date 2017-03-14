/*
 * a1.c
 *
 * Written by Joe Manak, April 1997
 *
 * Produce analyzed groups
 *
*/

/* -=======================================================-
_begin_doc
RCS ID string
$Id: a1.c,v 1.167 2008/06/02 19:42:43 goetz Exp $
$Author: goetz $
$Revision: 1.167 $
$Date: 2008/06/02 19:42:43 $
_end_doc
* -=======================================================- */
#define USE(var) static void * use_##var = (void *) &var
  static char crcsid[] = "$Id: a1.c,v 1.105"
                         " 1997/04/07 15:27:44 manak Exp $";
USE(crcsid);   /* make sure it is not optimized away */
  static char crname[] = "a1.c";
  static char crauth[] = "Joe Manak";
/*-------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <a1.h>
#include <itape.h>
#include <bitflags.h>
#include <makebanks.h>
#include <utility.h>
#include <unistd.h>
#include <pid.h>

#include <mysql.h>

typedef struct {
  int nsyst;
  int names;
  int nprim;
  int idnam;
  int idptr;
  int idfmt;
  int ndumm;
  int nresr;
  int nlplm;
  int narr;
  int iarr[10];
  int iefmt;
  int tleft;
  int lepio;
  int nami;
  int indi;
  int indj;
  int inta[200];
} sysbos_t;


sysbos_t sysbos_;

#define MAXFILELEN 2000000
int send2Dispatcher(BOSbank *bcs,char *);

void PrintUsage(char *processName)
{
  fprintf(stderr,"%s version: %s - %d\n",processName,crcsid,VersionNo());
  fprintf(stderr,"\nUsage: %s -sX  -o<outfile> file1 file2... \n", processName);
  fprintf(stderr,"Options are:\n");
  fprintf(stderr,"\t-M[#]\tOnly analyze # number of events\n");
  fprintf(stderr,"\t[-o<outfile>]\tOutput b-tape\n");
  fprintf(stderr,"\t[-a<outfile>]\tAppend output to outfile\n");
  fprintf(stderr,"\t[-b<outfile>]\tOutput unselected events b-tape\n");
  fprintf(stderr,"\t[-A<prlink>]\tSpecify the larger Avenue sized prlink file you like\n");
  fprintf(stderr,"\t[-G<runindex>]\tSpecify the run index to be used. Will override the CLAS_CALDB_RUNINDEX environment variable.\n");
  fprintf(stderr,"\t[-sX]\tEvent selection: choices are:  (default = a)\n");
  fprintf(stderr,"\t\t\ta:\tAll events\n\t\t\tt[#]:\tOnly events with at least one track, # = opt. # of tracks\n");
  fprintf(stderr,"\t\t\te: Events with at least one negative HBTR track\n");
  fprintf(stderr,"\t[-z#]\tSet target position to # (default = %f,%f,%f)\n",trktcl_.dpar_TargetPos[0],trktcl_.dpar_TargetPos[1],trktcl_.dpar_TargetPos[2]);
  fprintf(stderr,"\t\t\tExample: -z50 sets target position to 0,0,50\n");
  fprintf(stderr,"\t\t\tExample: -z5,0,50 sets target position to 5,0,50\n");

  fprintf(stderr,"\t[-cX]\tset currents: choices are:\n");
  fprintf(stderr,"\t\t\tt[#]:\ttorus current (max:4000)\n\t\t\tm[#]:\tminitorus current(max:8000)\n\t\t\tp[#]:\tpolarized target value\n");
  fprintf(stderr,"\t[-K]\tUse Kossov's Radial magnetic grid\n");
  fprintf(stderr, "\t[-X#]\tuse a different x vs. t function in tracking (def = 2, MC = 0)\n");
  fprintf(stderr, "\t[-T#]\tset the beam Type \n"
      "\t\t0 = electron\n"
      "\t\t1 = gamma, old Start Counter\n"
      "\t\t2 = gamma, g7a run, no ST\n"
      "\t\t3 = gamma, g2b run, no ST\n"
      "\t\t4 = gamma. New Start Counter\n");
  fprintf(stderr, "\t[-E#]\tset the beam Energy (MeV)\n");
  fprintf(stderr,"\t[-R#]\tinitalize with a different run number. This overrides and sets the run number in the head bank.\n");
  fprintf(stderr,"\t[-x#]\tNumber of events to skip between events\n");
  fprintf(stderr,"\t[-v]\tverbose mode\n");
  fprintf(stderr,"\t[-F]\tinstall fault handlers\n");
  fprintf(stderr,"\t[-B]\tdump out bos information\n");
  fprintf(stderr,"\t[-I#]\tignore the first # events\n");
  fprintf(stderr,"\t[-O]\tAutomatic output file generation\n");
  fprintf(stderr,"\t[-jpathname]\tAt end of each output file, jputremote file to pathname (Automatic output file generation only)\n");
  fprintf(stderr,"\t[-m#]\tMaximum output file length size in kbytes, automatic file generation only (default = 2000000)\n");
  fprintf(stderr,"\t[-Nstring]\tBasename to add to output file names, automatic file generation only (default = 'cooked')\n");
  fprintf(stderr,"\t[-pstring]\tPath to write output files, automatic file generation only (default = current directory) - do not add trailing '/'\n");
  fprintf(stderr,"\t[-C]\tCompress DC0 bank to include only hit based track (default = 'No Compression')\n");
  fprintf(stderr,"\t[-d#]\tMake the TBID and PART banks with a alternate number (def = 0)\n");
  fprintf(stderr,"\t[-y<file>]\tsynch event mode (skip events)\n");
  fprintf(stderr,"\t[-D#]\tBitwise Drop flags, as defined below:\n");
  fprintf(stderr,"\t\t\t0x%x\tDrop RC00->16 Banks\n", DROP_RAW);
  fprintf(stderr,"\t\t\t0x%x\tDrop DC0 Bank\n", DROP_DC0);
  fprintf(stderr,"\t\t\t0x%x\tDrop DC1 Bank\n", DROP_DC1);
  fprintf(stderr,"\t\t\t0x%x\tDrop HBLA Bank\n", DROP_HBLA);
  fprintf(stderr,"\t\t\t0x%x\tDrop TBLA Bank\n", DROP_TBLA);
  fprintf(stderr,"\t\t\t0x%x\tDrop HBTB Bank\n", DROP_HBTB);
  fprintf(stderr,"\t\t\t0x%x\tDrop SC Banks\n", DROP_SC);
  fprintf(stderr,"\t\t\t0x%x\tDrop EC Banks\n", DROP_EC);
  fprintf(stderr,"\t\t\t0x%x\tDrop HBID Bank\n", DROP_HBID);
  fprintf(stderr,"\t\t\t0x%x\tDrop CL01 Bank\n", DROP_CL01);
  fprintf(stderr,"\t\t\t0x%x\tDrop SEB Banks\n", DROP_SEB);
  fprintf(stderr,"\t\t\t0x%x\tDrop TBID,PART Bank\n", DROP_TBID);
  fprintf(stderr,"\t\t\t0x%x\tDrop HDPL Bank\n", DROP_HDPL);
  fprintf(stderr,"\t\t\t0x%x\tDrop LAC Banks\n", DROP_LAC);
  fprintf(stderr,"\t\t\t0x%x\tDrop CC Banks\n", DROP_CC);
  fprintf(stderr,"\t\t\t0x%x\tDrop ST Banks\n", DROP_ST);
  fprintf(stderr,"\t\t\t0x%x\tDrop DHCL Banks\n", DROP_DHCL);
  fprintf(stderr,"\t\t\t0x%x\tDrop ROC Histogram (HISS) Banks\n", DROP_HISS);
  fprintf(stderr,"\t\t\t0x%x\tDrop TDPL Bank\n", DROP_TDPL);
  fprintf(stderr,"\t\t\tFor example: RC0-16 + DC0 + DC1 + HBLA: -D0x%x\n", DROP_RAW + DROP_DC0 + DROP_DC1 + DROP_HBLA);
  fprintf(stderr,"\t\t\tDefault: 0x0\n");
  fprintf(stderr,"\t[-r<string>]\teliminate an arbitrary string of banks, a la bankfilter\n");
  fprintf(stderr,"\t[-P#]\tBitwise Process flags, as defined below:\n");
  fprintf(stderr,"\t\t\t0x%x\tHit-based tracking (HBTR)\n", PROCESS_HBTR);
  fprintf(stderr,"\t\t\t0x%x\tTime-of-Flight  (SC) \n", PROCESS_SC);
  fprintf(stderr,"\t\t\t0x%x\tHit-based particle id (HBID)\n", PROCESS_HBID);
  fprintf(stderr,"\t\t\t0x%x\tElectromagnetic Calorimeter (EC)\n", PROCESS_EC);
  fprintf(stderr,"\t\t\t0x%x\tCerenkov Counter (CC)\n", PROCESS_CC);
  fprintf(stderr,"\t\t\t0x%x\tTime-based tracking (TBTR)\n", PROCESS_TBTR);
  fprintf(stderr,"\t\t\t0x%x\tCALL bank analysis (CL01)\n", PROCESS_CL01);
  fprintf(stderr,"\t\t\t0x%x\tTime-based particle id (TBID, PART)\n", PROCESS_TBID);
  fprintf(stderr,"\t\t\t0x%x\tSimple Event Builder analysis (SEB)\n", PROCESS_SEB);
  fprintf(stderr,"\t\t\t0x%x\tLAC analysis\n", PROCESS_LAC);
  fprintf(stderr,"\t\t\t0x%x\tTagger analysis\n", PROCESS_TAGGER);
  fprintf(stderr,"\t\t\t0x%x\tStart Counter analysis\n", PROCESS_ST);
  fprintf(stderr,"\t\t\t0x%x\tConstruct TRKS from HBID\n", PROCESS_TRKS_HBID);
  fprintf(stderr,"\t\t\t0x%x\tConstruct TRKS from TBID (re-does TB)\n", PROCESS_TRKS_TBID);
  fprintf(stderr,"\t\t\t0x%x\tConstruct RGLK bank(build region 1 track bank)\n", PROCESS_REGION1);
  fprintf(stderr,"\t\t\t0x%x\tKnock out dead wires accordin to DC_STATUS\n", PROCESS_DCDW);
  fprintf(stderr,"\t\t\t0x%x\tTagger Multihit tdc analysis\n", PROCESS_TAGM);
  fprintf(stderr,"\t\t\t0x%x\tMake GPID\n", PROCESS_GPID);
  fprintf(stderr,"\t\t\t0x%x\tMake make PART/TBID no ST, group=2 \n", PROCESS_TBID_NOST);

  fprintf(stderr,"\t\t\tFor example: HBTR + HBID + TBTR: -P0x%x\n", PROCESS_HBTR + PROCESS_HBID + PROCESS_TBTR);
  fprintf(stderr,"\t\t\tDefault: 0xFFFF\n");
  fprintf(stderr,"\t[-t#]\tTrigger Mask eg, -t0x5 fill process only trigger bits 1 and 3\n");

  fprintf(stderr,"\t[-f]\tFilter out scaler events and tagger banks \n");
  fprintf(stderr,"\t\t if this is photon run\n");

  fprintf(stderr,"\t[-n#]\t Do SEB analysis for # neutrals\n");
  fprintf(stderr,"\t\t also include these events in output even if they\n");
  fprintf(stderr,"\t\t failed  -sX test\n");

  fprintf(stderr,"\t[-i]\tBatch mode(no counter)\n");
  fprintf(stderr,"\t[-W<file>]\tUse alternate magnetic field map <file>\n");

  exit(1);
}


/* for debugging purposes only */
int *deb;
clasHEAD_t *hdr;

/* global: accessible for fault handlers */
int AutoOutputUnitNo = 0;
int OutputUnitNo    = 0;
int AppendOutUnitNo = 0;
int ScalerOutUnitNo = 0;
int BadBosOutUnitNo = 0;
/* ------------ global sums ----------------------- */
int Nevents = 0;
int nwrite = 0;
int nscaler = 0;
int ntagger = 0;
double TotalCharge = 0.0; /* in microcoulombs */
/* ---------------------------------------------- */



/* Fault handlers */
static void signalINT(int isig)
{
  char mess[100];
  static int count = 0;

  count++;
  fprintf(stderr,"signalINT: Caught signal %d %d\n",isig,count);
  fprintf(stderr,"Run: %d Event %d\n",hdr->head[0].nrun,hdr->head[0].nevent);
  if (count < 4){
    signal(isig,signalINT);
  } else {
    dropAllBanks(&bcs_,"E"); /*drop the banks we read in*/
    dropAllBanks(&bcs_,"C"); /*drop the cooked banks we made*/
    cleanBanks(&bcs_);
    fprintf(stderr, "a1 aborted by interrupt signal, closing files\n");
    fprintf(stderr,"\n\n# of events:\t%d\n",Nevents);
    fprintf(stderr,"# of events written:\t%d\n",nwrite);
    fprintf(stderr,"total current %f microcoulombs\n",TotalCharge);
    fprintf(stderr,"# of scaler events written:\t%d\n",nscaler);
    fprintf(stderr,"# of tagger events written:\t%d\n",ntagger);
    /*close file*/
    if (AutoOutputUnitNo) {
      putBOS(&bcs_,AutoOutputUnitNo , "0");
      fparm_c("CLOSE AUTOOUTPUT");
    }
    if (AppendOutUnitNo) {
      putBOS(&bcs_,AppendOutUnitNo , "0");
      fparm_c("CLOSE BOSOUT7");
    }
    if (OutputUnitNo) {
      putBOS(&bcs_, OutputUnitNo, "0");
      sprintf(mess,"CLOSE BOSOUT9");
      fparm_c(mess);
    }
    if (BadBosOutUnitNo) {
      putBOS(&bcs_, BadBosOutUnitNo, "0");
      sprintf(mess,"CLOSE BADBOS");
      fparm_c(mess);
    }
    if (ScalerOutUnitNo) {
      putBOS(&bcs_, ScalerOutUnitNo, "0");
      sprintf(mess,"CLOSE SCALERBOS UNIT=%d", ScalerOutUnitNo);
      fparm_c(mess);
    }
    exit(0);
  }
}

static void signalPIPE(int isig)
{
  char mess[100];
  static int count = 0;

  count++;
  fprintf(stderr,"signalPIPE: Caught signal %d %d\n",isig,count);
  fprintf(stderr,"Assumming lost contact with the Dispatcher\n");
    fprintf(stderr, "a1 aborted by lost pipe, closing files\n");
    fprintf(stderr,"\n\n# of events:\t%d\n",Nevents);
    fprintf(stderr,"# of events written:\t%d\n",nwrite);
    fprintf(stderr,"total current %f microcoulombs\n",TotalCharge);
    /*close file*/
    sprintf(mess,"CLOSE BOSOUT7");
    fparm_c(mess);
    exit(0);

}
static void signalSEGV(int isig)
{
  static int icount = 0;

  fprintf(stderr,"signalSEGV: Caught signal %d\tcount:\t%d\n",isig,icount++);
  fprintf(stderr,"Run: %d Event %d\n",hdr->head[0].nrun,hdr->head[0].nevent);
  if (icount < 3)
    signal(isig,signalSEGV);
  else {
    fprintf(stderr,"Exit on:%d %d\n",hdr->head[0].nrun,hdr->head[0].nevent);
    fprintf(stderr,"# of events: %d\n# of events written: %d\n",Nevents,nwrite);
    fprintf(stderr,"total current %f microcoulombs\n",TotalCharge);
    exit(1);
  }


}

/* -------End of fault handlers ------------*/
int ScalerEvent(clasHEAD_t *hdr)
{
  clasTRGS_t *TRGS=NULL;
  clasTGS_t *TGS = NULL;

  if (hdr){
    if((TRGS = getBank(&bcs_, "TRGS")) || (TGS = getBank(&bcs_, "TGS ")) || (hdr->head[0].evtclass == 17) || hdr->head[0].type == 10 || hdr->head[0].type == 100 ) return (1);
  }
  return(0);

}

main(int argc,char*argv[])
{
  int ret;
  int ValidInput = 0;
  int nFileEvt = 0,nFileWrite = 0;
  char *server = NULL;
  int i=0, j, k;
  char def_map[100];
  int runno = 0;
  char *outfile = NULL;
  char *boutfile = NULL;
  char *roadfile = NULL;
  char *runindex = NULL;
  char *map = NULL;
  char *dir;
  char out[500], mess[500], scalout[500];
  FILE *fdump = NULL;
  FILE *fp = NULL;
  char *argptr;
  int Ndump = -1;
  int nbadout = 0;
  int nbad = 0;
  int nskipbad = 0;
  int max = 0;
  char *select = "a";
  int verbose = 0;
  int bverbose = 0;
  int skip = 1;
  int batch = 0;
  int faultFlag = 0;
  int DropFlag = 0x0; /*don't drop anything*/
  int ProcessFlag = 0xffff - PROCESS_DCDW; /*process everything; by default bad wire not removed*/
  int OptionsFlag = 0x0;  /* no options */
  int PIDnumber = 1;
  int ignore = 0;
  char *base = "cooked";
  char *jput = NULL;
  char *path = NULL;
  int Compress = 0;
  clasRUNC_t *RUNC = NULL;
  char *syncfile = NULL;
  FILE *syncptr = NULL;
  int sync_hi = 0, sync_low= 0;
  char *eliminate_list = NULL;
  int CurrentRun = -1;
  int DispatcherMode = 0;
  unsigned int trigMask = 0xffff;
  int trigflag = 0;
  float tmp;
  int filter = 0; /* scalaer filter flag */
  char *scaler_suffix = "Scaler";
  char scalerfile[500];
  char *tf;
  char torus_file[128];
/*    char *scalout = NULL; */

  /* -------------------------- */

  int nFilesOut = 0;
  int maxFileLength = MAXFILELEN;

  int DefaultRun = 0;

  int version = VersionNo();
  int F_false = 0, F_true = -1;

  char *cmd; /* For Dispatcher Commands */
  int nDispatcher = 0;
  int BadHeaderCount=0;

  static int nneut = 0; /* default number of neutrals */

  if(argc == 1) {
    PrintUsage(argv[0]);
    exit(0);
  }

  /* I need to set the defaults set before I over-ride them
     from the command line - yuk */
  trk_set_def_();

  /**
   * to enhance low-momentum backward tracks,
   * and to increase the likelyhood of finding
   * tracks in the forward direction
   **/
  trktcl_.ipar_trk_prlink_param=50;

  dc_set_def_();
  evnt_set_def_();
  set_level_(&F_false,&F_true,&F_true,&F_true,&F_true);

  for(i=1; i < argc; ++i){
    argptr = argv[i];
    if( *(argptr = argv[i])  == '-') {

      switch(*(++argptr)) {
      case 'h':
        PrintUsage(argv[0]);
        break;
      case 'R':
    DefaultRun = atoi(++argptr);
    break;
      case 'x':
    skip = atoi(++argptr);
    skip = skip ? skip : 1;
    break;
      case 'v':
    verbose = 1;
    set_level_(&F_true,&F_true,&F_true,&F_true,&F_true);
    break;
      case 'V':
    SetVerbose(1);
    break;
      case 'l':
    /* laconic mode */
    set_level_(&F_false,&F_false,&F_false,&F_true,&F_true);
    break;
      case 'B':
    bverbose = 1;
    break;
      case 'y':
    syncfile = ++argptr;
    syncptr = fopen(syncfile, "r");
    fprintf(stderr, "opening sync file: %s\n", syncfile);
    break;
      case 'a':
    outfile =  *(++argptr) ? argptr : "/dev/fd/1";
    fprintf(stderr,"Output file: %s\n",outfile);
    sprintf(out, "OPEN BOSOUT7 UNIT=7 FILE=\"%s\" ACTION=READWRITE FORM=BINARY STATUS=OLD ACCESS=SEQ SPLITMB=2047 RECL=32768", outfile);
    if (!fparm_c(out)) {
      fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],out,strerror(errno));
      exit(1);
    }
    else {
      AppendOutUnitNo = 7;
    }
    break;
      case 'o':
    outfile =  *(++argptr) ? argptr : "/dev/fd/1";
    fprintf(stderr,"Output file: %s\n",outfile);
    unlink(outfile);
    sprintf(out, "OPEN BOSOUT9 UNIT=9 FILE=\"%s\" WRITE STATUS=NEW SPLITMB=2047 RECL=32768", outfile);
    if (!fparm_c(out)) {
      fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],out,strerror(errno));
      exit(1);
    }
    else {
      OutputUnitNo = 9;
    }
    break;

      case 'O':
    AutoOutputUnitNo = 9;
    break;
      case 'm':
    maxFileLength = atoi(++argptr);
    break;
      case 'r':
    eliminate_list = ++argptr;
    fprintf(stderr, "%s will eliminate the following banks: %s\n", argv[0], eliminate_list);
    break;
      case 'N':
    base = ++argptr;
    break;
      case 'p':
    path = ++argptr;
    break;
      case 'b':
    boutfile =  *(++argptr) ? argptr : "/dev/fd/1";
    fprintf(stderr,"Bad Output file: %s\n",boutfile);
    unlink(boutfile);
    sprintf(out, "OPEN BADBOS UNIT=8 FILE=\"%s\" WRITE STATUS=NEW RECL=32768", boutfile);
    if (!fparm_c(out)) {
      fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],out,strerror(errno));
      exit(1);
    }
    else {
      BadBosOutUnitNo = 8;
    }
    break;
      case 'A':
    roadfile = *(++argptr) ? argptr : "/dev/fd/1";
    fprintf(stderr, "Going to use prlink file: %s\n", roadfile);
    sprintf(trktcl_.spar_prlink_name, roadfile);
    break;
      case 'G':
    runindex = *(++argptr) ? argptr : "/dev/fd/1";
    char runindex_putenv[256] = "CLAS_CALDB_RUNINDEX=";
    strcat(runindex_putenv, runindex);
    int runindex_stat = putenv(runindex_putenv);
    if(!runindex_stat) {
        fprintf(stderr, "Using run index: CLAS_CALDB_RUNINDEX = %s\n", runindex);
    } else {
        fprintf(stderr, "Problem in a1.c setting environment variable (runindex)\n");
    }
    break;
      case 'M':
    max = atoi(++argptr);
    break;
      case 'D':
    DropFlag = strtoul(++argptr,NULL,0);
    break;
      case 's':
    select = (++argptr);
    break;
      case 'F':
    faultFlag = 1;
    break;
      case 'i':
    batch = 1;
    break;
      case 'P':
    ProcessFlag = strtoul(++argptr,NULL,0);
    break;
      case 'I':
    ignore = atoi(++argptr);
    break;
      case 'j':
    jput = ++argptr;
    break;
      case 'C':
        Compress = 1;
        break;
      case 'K':
    trktcl_.ipar_trk_bgridtyp = 2;
    break;
      case 'z':
    parseTargetPosition(++argptr);
    break;
      case 'c':
    switch (*(++argptr)){
    case 't':
      trktcl_.ipar_torus_current = atoi(++argptr);
      fprintf(stderr, "torus current set to %d Amps\n", trktcl_.ipar_torus_current);
      break;
    case 'm':
      trktcl_.ipar_minitorus_current = atoi(++argptr);
      fprintf(stderr, "minitorus current set to %d Amps\n", trktcl_.ipar_minitorus_current);
      break;
        case 'p':
          trktcl_.ipar_poltarget_current = atoi(++argptr);
      fprintf(stderr, "polarized target value set to %d \n", trktcl_.ipar_poltarget_current);
          break;
    default:
      PrintUsage(argv[0]);
      break;
    }
    break;
      case 'n':
    nneut = atoi(++argptr);
    evnt_par_.neut_trigg_photon = nneut;
    fprintf(stderr, "will process %d neutral trigger\n",nneut);
    break;
      case 'X':
    dc_tcl_.dc_xvst_choice = atoi(++argptr);
    fprintf(stderr, "x vs. t choice set to: %d\n", dc_tcl_.dc_xvst_choice);
    break;
      case 'T':
    set_beam_type(atoi(++argptr));
    fprintf(stderr, "beam type set to %d\n", atoi(argptr));
    break;
      case 'f':
    filter = 1;
    ScalerOutUnitNo = 10;
    break;
      case 'E':
    tmp = atof(++argptr);
    set_beam_energy(tmp); /* MeV */
    fprintf(stderr, "beam energy set to %5.3f MeV\n", tmp);
    break;
      case 'S':
    server = ++argptr;
    break;
      case 't':
    trigflag = 1;
    trigMask = strtoul(++argptr,NULL,0);
    break;
      case 'd':
    PIDnumber = atoi(++argptr);
    break;
      case 'W':
    tf = ++argptr;
    sprintf(torus_file,"%s/%s",getenv("CLAS_PARMS"),tf);
    if(access(torus_file,F_OK)){ /*access returns 0 if file exists*/
      fprintf(stderr,"Could not find field map %s\n",torus_file);
      fprintf(stderr,"The default field map will be used\n");
    } else{
      sprintf(trktcl_.spar_torus_bfield_name,tf);
      fprintf(stderr,
          "Magnetic field map %s will be used.\n",trktcl_.spar_torus_bfield_name);
    }
    break;
      default:
    PrintUsage(argv[0]);
    break;
      }
    }
    else if ( *(argptr = argv[i])  == '+') {
      argptr = argv[i];
      switch(*(++argptr)) {
      case 'O':
    /* undocumented private options */
    OptionsFlag = strtoul(++argptr,NULL,0);
    break;
      }
    }
  }

  fprintf(stderr, "%s compiled on %s %s\n",__FILE__,__DATE__,__TIME__);
  fprintf(stderr,"Process Flag: 0x%x\n", ProcessFlag);
  fprintf(stderr, "Drop Flag: 0x%x\n", DropFlag);

  /*debugging information - so you can see the BOS common
    anywhere in the code in the debugger - do not use for
    anything else!*/
  deb = bcs_.iw;

  /*set default values*/
  if (outfile==NULL){
    fdump = stderr;
  }
  else if (filter){
    /* open file for scalers output */
    sprintf(scalerfile, "%s.%s", scaler_suffix, outfile);
    fprintf(stderr,"Scaler output file: %s\n",scalerfile);
    unlink(scalerfile);
    sprintf(out, "OPEN SCALERBOS UNIT=%d FILE=\"%s\" WRITE STATUS=NEW RECL=32768", ScalerOutUnitNo, scalerfile);

    if (!fparm_c(out)) {
      fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],out,strerror(errno));
      exit(1);
    }
  }


  initialize_analyzer(verbose);

  if (server) {
    initDispatcher(server,10);
    DispatcherMode = 1;
  }

  /* Set the output banks */

  bankList(&bcs_, "C=","TDPLHDPLHEADHBTRTRKSDCLATBLADTRKTBTRHBERTBERHBLATBLAHBTBSC1 SCR SCRCHBIDDC1 CC01CCRCEC01ECPIECHBEVNTEVHBDCPBECPBSCPBCCPBSTPBTGPBUNUSHEVTCL01TBIDPARTEC1RTAGRTAGITAGMST1 STR RF  ECPCLCPBRGLKDHCLTRL1SYNCMVRTGPID");  /*Cooked list*/

  /*bankList(&bcs_, "C=","TDPLHEADTRKSDCLATBLADTRKTBTRTBERSC  EC  EC1 EVNTDCPBECPBSCPBSTPBTGPBUNUSCL01TBIDPARTTAGRTAGTTAGEST  RF  LCPBRGLK+SYNCALLCL  DC0 DSPSFBPMHEVTHLS HLS+RCSTS1STSC  STS SCS ECS TGS TGBITLV1TRGSTRGTDC1 HBTRHBERHLS SYNC");  /*Cooked list, Luminita Todor's*/

  bankList(&bcs_, "R=","RC01RC02RC03RC04RC05RC06RC07RC08RC09RC10RC11RC12RC13RC14RC15RC16"); /*raw list*/

  /*event loop */
  for (i = 1; (max ? Nevents < max : 1) && ((i < argc) || server) ; ++i) {
    argptr = argv[i];
    if (server) {
      ValidInput = 1;
      fprintf(stderr,"Input(Dispatcher): %s\n",server);
    }
    else {
      if (*argptr != '-') {
    if (!server) {
      ValidInput = initFile(argptr);
      fprintf(stderr,"Input(file): %s\n",argptr);
    }

      }
    }


    if (ValidInput) {


      if (AutoOutputUnitNo) {
    if (!server) {
      outfile =  FileName(argptr,path,base,nFilesOut);
      fprintf(stderr,"Output file: %s\n",outfile);
      unlink(outfile);
      sprintf(out, "OPEN AUTOOUTPUT UNIT=%d FILE=\"%s\" WRITE STATUS=NEW RECL=32768", AutoOutputUnitNo, outfile);
      if (!fparm_c(out)) {
        fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],out,strerror(errno));
        exit(1);
      }

      if (filter){
        /* open file for scalers output */
        sprintf(scalerfile, "%s.%s", outfile, scaler_suffix);
        unlink(scalerfile);
        fprintf(stderr,"Scaler output file: %s\n",scalerfile);
        sprintf(out, "OPEN SCALERBOS UNIT=%d FILE=\"%s\" WRITE STATUS=NEW RECL=32768", ScalerOutUnitNo, scalerfile);
        if (!fparm_c(out)) {
          fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],out,strerror(errno));
          exit(1);
        }
      }
    }
      }

      nFileEvt = 0;
      nFileWrite = 0;


      if (faultFlag) install_fault_handlers();
      /*event loop */

      int disio_err = 0;

      while ((max ? Nevents < max : 1) && ValidInput && ((ret = getData(&bcs_,"E")))) {

    switch (ret) {
    case DISIO_DATA:
      hdr = getBank(&bcs_, "HEAD");
      if(!hdr){
        fprintf(stderr,"ERROR - no HEAD bank event #%d (%d)\n",Nevents,BadHeaderCount);
        BadHeaderCount++;
        continue; /* Get next event in while loop */
      }
      hdr->head[0].version = version;

      if (DefaultRun)
      {
          hdr->head[0].nrun = DefaultRun;
      }

      if (syncptr && hdr->head[0].nevent >= sync_hi){
        if (fscanf(syncptr, "%d %d\n", &sync_low, &sync_hi) != EOF){
          fprintf(stderr, "will skip events from %d to %d\n", sync_low, sync_hi);
        } else {
          fclose(syncptr);
          syncptr = NULL;
        }
      }

      if (bverbose) {
        bosSystem(bcs_.iw,stderr);
      }

      Nevents++;
      nFileEvt++;

      if (Nevents % 100 == 0) {
        if (!batch) {
          fprintf(stderr,"%d\t \t%d\t%d\t%d\t%d\t%d\r",Nevents,nwrite,nFileEvt,nFileWrite,nscaler,ntagger);
          fflush(stderr);
        }
      }

      /*general mdl common (contains the run number - and
        number of events for rernev - I would love to get rid of this*/
      fill_clasmdl();

      /* performs initialization if run numb. changes */


      ConfigureEvent(ProcessFlag,DefaultRun);

      if (ScalerEvent(hdr) || (hdr->head[0].type > 100)) {
        if (RUNC = getBank(&wcs_, "RUNC")){
          switch (RUNC->runc.beam.type.val.i[0]) {
          case 1: /* photon beam */
          case 4: /* photon beam with new ST*/
        if(ProcessFlag & PROCESS_TAGGER) tag_scaler_();
        break;
          case 0:
        if (PROCESS_SEB & ProcessFlag) bm_read_trig_();  /*scaler information for seb*/
        break;
          }
        }
        if (AutoOutputUnitNo)
          putBOS(&bcs_,AutoOutputUnitNo,"E");
        if (OutputUnitNo)
          putBOS(&bcs_,OutputUnitNo,"E");
        if (ScalerOutUnitNo){
          putBOS(&bcs_,ScalerOutUnitNo,"E");
          nscaler++;
        }
        nwrite++;
        nFileWrite++;
      }
      else {
        if (ignore > 0)
          ignore--;
        else {
          if (!(Nevents % skip) && (syncptr ? hdr->head[0].nevent < sync_low: 1)) {
        if (RUNC = getBank(&wcs_, "RUNC")){
          if (filter && RUNC->runc.beam.type.val.i[0]){
            /* write tagger related banks */
            putBOS(&bcs_, ScalerOutUnitNo, "HEADCALLTAGTTAGEDSPSFBPM");
            ntagger++;
          }
        }
        if (!trigflag ? 1 : hdr->head[0].trigbits & trigMask) {
          ProcessEvent(ProcessFlag,OptionsFlag, PIDnumber);

          if (Compress){
            CompressDC0();
          }

          /*the T list contains whatever we choose to write out*/
          make_T_list(DropFlag, eliminate_list);

          /* Write out events */
          if (server) {
            if (WriteEvent(select) || NeutralEvent(nneut)) {
              if (verbose)
            fprintf(stderr,"Sending data to Dispatcher...\n");
              send2Dispatcher(&bcs_,"T");
              nDispatcher++;
            }
          }


          if (outfile || boutfile) {
            if (WriteEvent(select) || NeutralEvent(nneut)) {
/*                if(NeutralEvent(nneut)) fprintf(stderr, "2n event %d\n",WriteEvent(select)); */
              if (AutoOutputUnitNo)
            putBOS(&bcs_, AutoOutputUnitNo, "T");
              if (OutputUnitNo)
            putBOS(&bcs_, OutputUnitNo, "T");
              nwrite++;
              nFileWrite++;
              if (AutoOutputUnitNo && fileLength("AUTOOUTPUT") > maxFileLength) {
            /*close file*/
            putBOS(&bcs_, AutoOutputUnitNo, "0");
            sprintf(mess,"CLOSE AUTOOUTPUT UNIT=%d", AutoOutputUnitNo);
            fparm_c(mess);
            /* if -j option was present, issue PutFile command */
            if (jput) {
              sprintf(mess,"PutFile %s %s &",outfile,jput);
              system(mess);
            }
            outfile =  FileName(argptr,path,base,++nFilesOut);
            fprintf(stderr,"Output file: %s\n",outfile);
            unlink(outfile);
            sprintf(out, "OPEN AUTOOUTPUT UNIT=%d FILE=\"%s\" WRITE STATUS=NEW RECL=32768", AutoOutputUnitNo, outfile);
            if (!fparm_c(out)) {
              fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],out,strerror(errno));
              exit(1);
            }
              }
            }
            else if (boutfile) {
              putBOS(&bcs_, BadBosOutUnitNo, "T");
              nbadout++;

            }
          }

        }
          }

        }
      }
      dropAllBanks(&bcs_,"E"); /*drop the banks we read in*/
      dropAllBanks(&bcs_,"C"); /*drop the cooked banks we made*/
      cleanBanks(&bcs_);

      break;
    case DISIO_COMMAND:
      if (cmd = getBuffer()) {
        fprintf(stderr,"Command from Dispatcher: %s\n",cmd);
        if (strcmp(cmd,"EXIT") == 0) {
          server = NULL;
          ValidInput = 0;
        }
      }
      else
        fprintf(stderr,"NULL Command from Dispatcher\n");
      break;
    case DISIO_ERROR:
      /* Assume broken pipe for time being, and remove ValidInput flag after 10 tries */
      if (disio_err++ > 10)
      {
        ValidInput = 0;
      }
      break;
    default:
      break;
    }



    /// close any mysql connection that may be open.
    /// This is done every 500 events
    /// There is a negligable penalty for calling this
    /// function when the mysql connection is already closed.
    if ( !(Nevents % 500) )
    {
        calib_mysql_disconnect();
    }

      }

      if (DispatcherMode && ValidInput) {
    disIO_command("FINISHED");
    if (verbose)
      fprintf(stderr,"sent FINISHED to Dispatcher\n");
      }

      fprintf(stderr,"\n\n# of events:\t%d\n",Nevents);
      fprintf(stderr,"# of events written:\t%d\n",nwrite);
      fprintf(stderr,"\n\n# of events (file):\t%d\n",nFileEvt);
      fprintf(stderr,"# of events written (file):\t%d\n",nFileWrite);
      fprintf(stderr,"# of events written:\t%d\n",nwrite);
      fprintf(stderr,"# of scaler events written:\t%d\n",nscaler);
      fprintf(stderr,"# of tagger events written:\t%d\n",ntagger);


      if (DispatcherMode)
    fprintf(stderr,"# of events sent to Dispatcher:\t%d\n",nDispatcher);


    }

  }


  /*close file if open*/
  if (AutoOutputUnitNo) {
    putBOS(&bcs_,AutoOutputUnitNo , "0");
    sprintf(mess,"CLOSE AUTOOUTPUT UNIT=%d", AutoOutputUnitNo);
    fparm_c(mess);
  }
  if (OutputUnitNo) {
    putBOS(&bcs_, OutputUnitNo, "0");
    sprintf(mess,"CLOSE BOSOUT9");
    fparm_c(mess);
  }
  if (AppendOutUnitNo) {
    putBOS(&bcs_,AppendOutUnitNo , "0");
    fparm_c("CLOSE BOSOUT7");
  }
  if (BadBosOutUnitNo) {
    putBOS(&bcs_, BadBosOutUnitNo, "0");
    sprintf(mess,"CLOSE BADBOS");
    fparm_c(mess);
  }
  if (ScalerOutUnitNo) {
    putBOS(&bcs_, ScalerOutUnitNo, "0");
    sprintf(mess,"CLOSE SCALERBOS UNIT=%d", ScalerOutUnitNo);
    fparm_c(mess);
  }
  /* all done, exit now */
  exit(0);
}

int install_fault_handlers(){
  signal(SIGINT,signalINT);
  signal(SIGSEGV,signalSEGV);
  signal(SIGABRT,signalSEGV);
  signal(SIGBUS,signalSEGV);
  signal(SIGPIPE,signalPIPE);
  fprintf(stderr,"Fault handlers installed\n");
}


int printPhotonScalers(FILE *fp){
  clasRTSL_t *RTSL = getBank(&wcs_, "RTSL");
  clasG1SL_t *G1SL = getBank(&wcs_, "G1SL");
  clasG2SL_t *G2SL = getBank(&wcs_, "G2SL");
  clasG3SL_t *G3SL = getBank(&wcs_, "G3SL");
  clasG3SL_t *G4SL = getBank(&wcs_, "G4SL");

  /*regurgitate banks for placement into offline database*/
  if (RTSL) printRTSLbank(fp, RTSL);
  if (G1SL) printG1SLbank(fp, G1SL);
  if (G2SL) printG1SLbank(fp, (clasG1SL_t *)G2SL);
  if (G3SL) printG1SLbank(fp, (clasG1SL_t *)G3SL);
  if (G4SL) printG1SLbank(fp, (clasG1SL_t *)G4SL);

}

int initialize_analyzer(int verbose){
  /* int memh = MEMH;*/
  int i, max = 1000;
  char banklist[100];

  /*set up common blocks for fpack and cern and anything else you
    have to do only once*/
  bnames_(&max);
  initbos();
  clasmdl_.trec = 0; /* for rernev routine - do not remove - JM */
  configure_banks(stderr, verbose);
  /*hlimit_(&memh);
  fprintf(stderr, "hbook initialized\n");*/
}

int ConfigureEvent(int ProcessFlag,int DefaultRun)
{
  clasHEAD_t *hdr;
  static int CurrentRun = -1;
  char *dir;
  int iret = 0;

  if (hdr = getBank(&bcs_,"HEAD")) {
    if (hdr->head[0].nrun != CurrentRun && hdr->head[0].nrun != 0) {
      fprintf(stderr, "Initializing analyzer for run: %d\n", hdr->head[0].nrun);
      fprintf(stderr,"CurrentRun: %d\n", CurrentRun);
      if (DefaultRun) {
    initRun(DefaultRun,ProcessFlag);
      }
      else {
    initRun(hdr->head[0].nrun, ProcessFlag);
      }
      CurrentRun = hdr->head[0].nrun;
      fprintf(stderr,"CurrentRun: %d, nrun: %d\n", CurrentRun, hdr->head[0].nrun);
    }
  }

  return(1);
}


int NeutralEvent(int nneut)
{
  int nnnn = 0;
  int ret = 0;
  int i;
 clasEVNT_t *EVNT = getBank(&bcs_, "EVNT");
  if(nneut == 0) {
    ret = 0;
  }
  else {
    if (EVNT) {
      for (i = 0; i < EVNT->bank.nrow; ++i) {
    if (EVNT->evnt[i].charge == 0) nnnn++;
      }
    }
    ret = 0;
    if ( nnnn >= nneut) ret = 1;
  }
  return(ret);
}

int WriteEvent(char *select)
{
  clasHBTR_t *HBTR = getBank(&bcs_, "HBTR");
  int ret = 0;
  int i;
  int ne = 0;

  switch (select[0]) {
  case 'a':
    ret = 1;
    break;
  case 't':
    if (strlen(select) == 1){
      ret = (int) HBTR;
    }
    else if(strlen(select) > 1) {
      if (HBTR) {
        if (HBTR->bank.nrow >= atoi(&select[1])) ret = 1;
      }
    }
    break;
  case 'e':
    /* at least one negative HBTR bank */
    if (HBTR) {
      for (i = 0; i < HBTR->bank.nrow; ++i) {
    if (HBTR->hbtr[i].q < 0.0)
      ne++;
      }
      ret = ne ;
    }
    break;
  default:
    ret = 1;
    break;
  }

  return(ret);
}

void dumpBuffer(int *a,int n)
{

#define MAXLINE 80
#define MAXSYM 16
#define MASK

    int line_ctr;
    int LineNo = 0;
    char line[MAXLINE];
    int ctr = 0;
    char display_char();
    char c[4];
    int l;

    line_ctr = 0;
    while (ctr < n) {
        c[0] = (*a & 0xff000000) >> 24;
        c[1] = (*a & 0x00ff0000) >> 16;
        c[2] = (*a & 0x0000ff00) >>  8;
        c[3] = *a & 0x000000ff;
        for (l=0; l < 4; ++l) {
          ctr++;
          fprintf(stderr,"%2x ",c[l]);
          line[line_ctr++] = display_char(c[l]);
          if (line_ctr >= MAXSYM){
        line[line_ctr] = '\0';
        fprintf(stderr,"%5d  %s\n",LineNo++,line);
        line_ctr = 0;
          }
        }
        a++;
    }
    line[line_ctr] = '\0';
    line_ctr = MAXSYM - line_ctr;
    while(line_ctr--)
        fprintf(stderr,"   ");
    fprintf(stderr,"%5d   %s\n",LineNo++,line);
}




char display_char(int c)
{
  if(c >= ' ' && c < '~')
    return((char) c);
  else if (c == '\n')
    return('*');
  else
    return('-');
}

int make_T_list(int DropFlag, char *eliminate_list){

  /*  bankList(&bcs_,"T=","0"); */ /*set list T equal to the empty list*/
  /*T list initially consists of all concievable banks*/
  bankList(&bcs_,"T=","E");
  bankList(&bcs_,"T+","C");
  bankList(&bcs_,"T+","R");
  if (eliminate_list) bankList(&bcs_,"T-",eliminate_list);

  /* Mask off banks according to DropFlag*/
  if (DropFlag & DROP_RAW) bankList(&bcs_, "T-", "R");
  if (DropFlag & DROP_DC0) bankList(&bcs_, "T-", "DC0 ");
  if (DropFlag & DROP_DC1) bankList(&bcs_, "T-", "DC1 ");
  if (DropFlag & DROP_HBLA) bankList(&bcs_, "T-", "HBLA");
  if (DropFlag & DROP_TBLA) bankList(&bcs_, "T-", "TBLA");
  if (DropFlag & DROP_HBTB) bankList(&bcs_, "T-", "HBTB");
  if (DropFlag & DROP_SC) bankList(&bcs_, "T-", SC_BANKS);
  if (DropFlag & DROP_EC) bankList(&bcs_, "T-", EC_BANKS);
  if (DropFlag & DROP_HBID) bankList(&bcs_, "T-", "HBID");
  if (DropFlag & DROP_CL01) bankList(&bcs_, "T-", "CL01");
  if (DropFlag & DROP_SEB) bankList(&bcs_, "T-", SEB_BANKS);
  if (DropFlag & DROP_TBID) bankList(&bcs_, "T-", "TBIDPART");
  if (DropFlag & DROP_HDPL) bankList(&bcs_, "T-", "HDPL");
  if (DropFlag & DROP_LAC) bankList(&bcs_, "T-", "EC1R");
  if (DropFlag & DROP_CC) bankList(&bcs_, "T-", CC_BANKS);
  if (DropFlag & DROP_ST) bankList(&bcs_, "T-", ST_BANKS);
  if (DropFlag & DROP_DHCL) bankList(&bcs_, "T-", "DHCL");
  if (DropFlag & DROP_BANKS) bankList(&bcs_, "T-", DROP_LIST);
  if (DropFlag & DROP_HISS) bankList(&bcs_, "T-", HISS_BANKS);
  if (DropFlag & DROP_TDPL) bankList(&bcs_, "T-", "TDPL");
}

char *FileName(char *input,char *path,char *base,int nfile)
{
  char temp[1000];
  char run[100];
  int ir;
  char *ptr,*r = "",*ext = "";
  static char ret[1000];
  char *word,*last = NULL;

  /* get the file name */
  strcpy(temp,input);
  word = last = strtok(temp,"/");
  while (word) {
    word = strtok(NULL,"/");
    last = word ? word : last;
  }

  /* loop through and find a run number */

  ptr = last;
  ir = 0;
  while (*ptr) {    if (*ptr >= '0' && *ptr <= '9') {
      r = ptr;
      run[ir++] = *r;
      while (*ptr >= '0' && *ptr <= '9') {
    ptr++;
    run[ir++] = *ptr;
      }
      run[--ir] = (char) NULL;
       break;
    }
    ptr++;
  }

  /* get the file extension */

  word = ext = strtok(last,".");
  while (word) {
    word = strtok(NULL,".");
    ext = word ? word : ext;
  }

  r = run;
  /* remove leading zeros */
  if (r) {
  while (*r == '0')
    r++;
  }
  if (path)
  sprintf(ret,"%s/run%s_%s.%s.%2.2d",path,r,base,ext,nfile);
  else
    sprintf(ret,"run%s_%s.%s.%2.2d",r,base,ext,nfile);
  return(ret);
}

int CompressDC0(){ /*dangerous, bos banks are lost and remade here*/
  clasHBTR_t *HBTR=getBank(&bcs_, "HBTR");
  clasDC1_t *DC1= getBank(&bcs_, "DC1 ");
  clasHBLA_t *HBLA=getBank(&bcs_, "HBLA");
  int i;

  if ((DC1) && (HBLA) && (HBTR)){
    dropAllBanks(&bcs_, "DC0 ");
    for (i=0; i< HBTR->bank.nrow; i++){
      hbtr2dc0(&(HBTR->hbtr[i]));
    }
  }
}

BosWriteFail(void *ptr,size_t size, size_t nitems, FILE *stream)
{
  if (hdr)
   fprintf(stderr,"Exit on:%d %d\n",hdr->head[0].nrun,hdr->head[0].nevent);
  fprintf(stderr,"# of events: %d\n# of events written: %d\n",Nevents,nwrite);
}


int VersionNo()
{
  int ret = 0;
  float r;
  char local[1000];
  char *word;

  strcpy(local,crcsid);
  word = strtok(local,"v");
  if (word) {
     word = strtok(NULL," ");
     r = atof(word);
    ret = r * 1000;
  }
  return(ret);
}

void parseTargetPosition(char *str)
{
  double x = 0,y=0,z=0;
  char *word;
  if (nchar(str,',') == 2) {
    /* user has specified a 3-vector */
    word = strtok(str,",");
    x = atof(word);
    word = strtok(NULL,",");
    y = atof(word);
    word = strtok(NULL,",");
    z = atof(word);
  }
  else if (nchar(str,',') == 0) {
    z = atof(str);
  }
  else {
    fprintf(stderr,"error parsing target position: %s\n",str);
  }
  fprintf(stderr,"Target position set to: %f %f %f\n",x,y,z);
  trktcl_.dpar_TargetPos[0] = x;
  trktcl_.dpar_TargetPos[1] = y;
  trktcl_.dpar_TargetPos[2] = z;
}


