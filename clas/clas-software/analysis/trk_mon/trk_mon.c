/* trk_mon.c  check out the tracking
 */

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <clas_cern.h>
#include <kinematics.h>
#include <map_manager.h>
#include <sc.h>
#include <call.h>
#include <ec.h>
#include <pid.h>
#include <utility.h>
#include <PartUtil.h>
#include <particleType.h>
#include <vertex.h>
#include <clasmdl.h>
#include <dc.h>
#include <trk_run_control.h>
#include <trk.h>
#include <itape.h>
#ifdef CSQL
#include "csql.h"
#endif

/* prototype... */
void set_level_(int *linfo, int *lwarn, int *ldebug, int *lerror, int *lfatal);
void bnames_(int *namax);
int initDispatcherDisplay(char *server,int dispatcherPipelinePrefill);
int initFile(char *filename);
int getData(BOSbank *bcs,char *list);

/*------------ PAW DEFINES ----------------- */
#define MEMH 7000000
#define LREC 1024 /* record length of hbook direct access file in WORDS */
#define LUN 3 /* logical unit number of hbook file */

#define MASS_HISTOGRAM(NUMBER,TITLE)  hbook1(NUMBER,TITLE, 2000, 1.2, 2.7,0)

#define RAD2DEG (180.0/3.14159)

/* re-declare the bos common */
BOSbank bcs_;
BOSbank wcs_;

/* declare the hbook common */
float pawc_[MEMH];
float hcfitd_[25];
int quest_[100];

/* csql globals */
int csql_flag=0;

int max = 0;
int debug = 0;
int pickwire = 0;

int quit_program=0;

int no_histos = 0;

float targetZposition = 0.0;

int ntbla = 0;
static int nhbt=0,ntbt=0,n3tbt=0,nevnt=0,nhits=0,ressqrd=0; /*total diagnostics*/
static int snhbt[6],sntbt[6],snhits[6]  ; /*diagnostics by sector*/
static float iressqrd[6][6];  /*diagnostics by individual drift chamber*/
static int inhits[6][6];      /*diagnostics by individual drift chamber*/
int mkntuple[7][7]; /*flags to make specific Sector/Superlayer Ntup */
static int makentuple=0;
static int event_counter=0;
extern float LOCANGLE_CUT_HIGH[4],LOCANGLE_CUT_LOW[4];

/* ----------- Function prototypes ---------------- */
void PrintUsage(char *processName);
int ProcessEvent(int *);
void ctrlCHandle(int);
void book_histos(int runno);
void hini(char *out,int runno);
int ConfigGeom(int runno);
int get_sector(vector4_t *part);
float get_phi(vector4_t *part);
float get_theta(vector4_t *part);
int clean_up(int runno, int nevents);
void user_book_hist(int runno);
int user_process_event(int runno);
int printPHM_report(FILE *fp, int RunNo, int Nevents);
int fill_clasmdl();
int generate_trk_mon_report(int runno, int Nevents);

void tm_book_ntuples(int runno);
int tm_fill_ntuples(void);
void tm_end_ntuples(void);
void tm_set_ntuple_cut(char *arg);
void tm_set_mass_upper(float upper);
void tm_set_mass_lower(float lower);
void tm_use_mass_cuts(void);
void csql_setup(int runno, int runext);

float mass_upper,mass_lower;

/* --------------------------------------------------- */

void PrintUsage(char *processName)
{
  fprintf(stderr,"Usage: %s [-M] [-T#]  -o<outfile> file1 [file2] etc....\n\n",processName);
  fprintf(stderr,"  Options:\n");
  fprintf(stderr,"\t-M[#]\t\t Process only # number of events\n");
  fprintf(stderr,"\t-o<outfile>\t Output file name\n");
  fprintf(stderr,"\t-GBANK\t\t Print BANK (ala bosdump) for each event just prior to ProcessEvent\n");
  fprintf(stderr,"\t[-X#]\tuse a different x vs. t function in tracking (def = 2, MC = 0)\n");
  fprintf(stderr,"\t-r[#]\t\t Use run number # for initializations etc..\n");
  fprintf(stderr,"\t-R\t\t Rebuild TBLA bank - NEEDS DC0, TBER and TRKS\n");
  fprintf(stderr,"\t-D\t\t Rebuild TDPL bank - needs TBER and TRKS\n");
  fprintf(stderr,"\t-L\t\t Rebuild TBTR bank - needs TBER \n");
  fprintf(stderr,"\t-d\t\t debug mode\n");
  fprintf(stderr,"\t-N[sup[sec]]\t make calibration Ntuples\n");
  fprintf(stderr,"\t-cX\t\t set currents: choices are:\n");
  fprintf(stderr,"\t\t\t\tt[#]:\ttorus current (max:4000)\n\t\t\t\tm[#]:\tminitorus current(max:8000)\n\t\t\t\tp[#]:\tpolarized target value\n");
  fprintf(stderr,"\t-v\t\t Show event counter while processing.\n");
  fprintf(stderr,"\t-C\t\t Set cut on Ntuple events (e.g. -Cbeta.lt.0.9).\n");
  fprintf(stderr,"\t-A\t\t Set cut on local angle(in degrees) by region. e.g.\n");
  fprintf(stderr,"\t  \t\t -A2u12.4  sets the upper limit cut on region 2 to 12.4\n");
  fprintf(stderr,"\t  \t\t the first character after A is the region, the second \n");
  fprintf(stderr,"\t  \t\t is either\"u\" for upper or \"l\" for lower followed\n");
  fprintf(stderr,"\t  \t\t by the value.\n");
  fprintf(stderr,"\t-t[#]\t\t 0 wire at hit, -1 wire with smaller TDC, +1 larger TDC.\n");
  fprintf(stderr,"\t     \t\t -2 wire with smaller wire id, +2 larger wire id.\n");
  fprintf(stderr,"\t-Shost:handle\t Use Dispatcher on host with handle\n");
  fprintf(stderr,"\t-T#\t\t Use # as a trigger mask, eg, -T0x3 masks trigger bits 1 and 3\n");
  fprintf(stderr,"\t-PROTON\t\t Cut on proton mass (don't use -u or -l with this!)\n");
  fprintf(stderr,"\t-P<prfile>\t Specify prlink file to use.\n");
  fprintf(stderr,"\t-T[#]\t\t Specify trigger mask to use.\n");
  fprintf(stderr,"\t-u\t\t Set upper mass cut in GeV\n");
  fprintf(stderr,"\t-l\t\t Set lower mass cut in GeV\n");
  fprintf(stderr,"\t-H\t\t Do not book or fill histograms\n");
  fprintf(stderr,"\t-Z\t\t Target z-position (cm). Histogram will be +/- 50 cm this value.\n");
  fprintf(stderr,"\t-B<file>\t Use alternate magnetic field map <file>\n");
  fprintf(stderr,"\t        \t The user's CLAS_PARMS area will be searched for <file> \n");
  fprintf(stderr,"\t        \t If <file> is not found, the default will be used\n");
#ifdef CSQL
  fprintf(stderr,"\t-z\t\t Fill MySql database with monitoring information\n");
#endif
  fprintf(stderr,"\t-h\t\t Print this message.\n");
  fprintf(stderr,"\n");


#ifdef DC_DCH_VERSION_MAJOR_h
  fprintf(stderr,"DC Versions:\n");
  fprintf(stderr,"\t dclib: %d.%d\n",DC_DCLIB_VERSION_MAJOR,DC_DCLIB_VERSION_MINOR);
  fprintf(stderr,"\t dc.h : %d.%d(dclib)    %d.%d(trk_mon)\n"
      ,DC_DCH_VERSION_MAJOR,DC_DCH_VERSION_MINOR
      ,DC_DCH_VERSION_MAJOR_h,DC_DCH_VERSION_MINOR_h);
#else
  fprintf(stderr,"DC Versions: Not available\n");
#endif

  exit(0);
}

int main(int argc,char **argv)
{
  FILE *fp = NULL;
  int i,j,nmax=200;
  char *argptr, *outfile = NULL;
  char *roadfile=NULL;
  char *tf= NULL;
  char *ext_loc, ext[2] = "A";
  char torus_file[128];
  int Nevents = 0;
  int Npassed = 0;
  char mess[200];
  int icycle, ret;
  int id = 99;
  int time_based = 0;
  int runno = 0,regen_bla=0,regen_dpl=0,makepart=0,regen_tbtr=0,runext=0;
  int runno_substitute=-1;
  int sector,superlayer;
  int sec,reg;
  int batch_mode=0;
  clasHEAD_t *HEAD = NULL;
  unsigned int triggerMask = 0xffffff;
  int F_false = 0, F_true = -1;
  char *groups[1000];
  int ngroup = 0;
  char *xgroups[1000];
  int nxgroup = 0;
  float upper = 0;
  float lower = 0;
  int pass_first=0;
  float beta_t,sec_t,sup_t,time_t,timec_t;
  char c;
  float f;

  char bankl[20];

  /* Dispatcher stuff */
  char *server = NULL;
  int valid_input_stream = 0;

  bankl[0]='\0';

  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);

  set_level_(&F_false,&F_false,&F_false,&F_false,&F_false);

  /* need to put tracking initialization up here so -ct and -cm command line options work */
  dc_set_def_();
  trk_set_def_();

  /* Initialize local angle cuts */
  for(reg=1;reg<=3;reg++){
    LOCANGLE_CUT_HIGH[reg]=60.0;
    LOCANGLE_CUT_LOW[reg]=-60.0;
  }

  for (i=1; i<argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {
        case 'G':
          groups[ngroup++] = ++argptr;
          break;
        case 'h':
          PrintUsage(argv[0]);
          break;
        case 'S':
          server = ++argptr;
          break;
        case 'o':
          outfile = ++argptr;
          break;
        case 'M':
          max = atoi(++argptr);
          break;
        case 'P':
          if(!strcmp(argptr,"PROTON")){
            tm_use_mass_cuts();
            mass_upper = 0.938+0.200; /* GeV */
            tm_set_mass_upper(mass_upper);
            fprintf(stderr,"setting upper mass cut at %f GeV\n",mass_upper);
            mass_lower = 0.938-0.200; /* GeV */
            tm_set_mass_lower(mass_lower);
            fprintf(stderr,"setting lower mass cut at %f GeV\n",mass_lower);
            tm_set_ntuple_cut("Cbeta.lt.1.0");
          }else{
            roadfile = (++argptr);
            sprintf(trktcl_.spar_prlink_name, roadfile);
            fprintf(stderr, "Going to use prlink file: %s\n",trktcl_.spar_prlink_name);

          }
          break;
        case 'X':
          dc_tcl_.dc_xvst_choice = atoi(++argptr);
          fprintf(stderr, "x vs. t choice set to: %d\n", dc_tcl_.dc_xvst_choice);
          break;
        case 'r':
          runno_substitute =atoi(++argptr);
          break;
        case 'R':
          regen_bla =1;
          strcat(bankl,"TBLA");
          break;
        case 'D':
          regen_dpl =1;
          strcat(bankl,"TDPL");
          break;
        case 'L':
          regen_tbtr =1;
          break;
        case 'N':
          makentuple = 1;
          sector=superlayer=0;
          argptr++;
          if((*argptr)!=0){ /* superlayer specified */
            superlayer=((int)*argptr)-48;
            argptr++;
            if((*argptr)!=0){   /* sector specified */
              sector=((int)*argptr)-48;
              mkntuple[sector][superlayer]=1;
            }else{             /* sector not specified */
              for(sector=1;sector<=6;sector++)mkntuple[sector][superlayer]=1;
            }
          }else{          /* superlayer not specified */
            for(superlayer=1;superlayer<=6;superlayer++)
              for(sector=1;sector<=6;sector++)mkntuple[sector][superlayer]=1;
          }
          break;
        case 'd':
          debug =1;
          break;
        case 'c':
          switch (*(++argptr)){
            case 't':
              trktcl_.ipar_torus_current = atoi(++argptr);
              fprintf(stderr, "torus current set to %d Amps\n",
                  trktcl_.ipar_torus_current);
              break;
            case 'm':
              trktcl_.ipar_minitorus_current = atoi(++argptr);
              fprintf(stderr, "minitorus current set to %d Amps\n",
                  trktcl_.ipar_minitorus_current);
              break;
            case 'p':
              trktcl_.ipar_poltarget_current = atoi(++argptr);
              fprintf(stderr, "polarized target value set to %d \n",
                  trktcl_.ipar_poltarget_current);
              break;
            default:
              PrintUsage(argv[0]);
              break;
          }
          break;
        case 'v':
          event_counter =1;
          break;

        case 'C':
          tm_set_ntuple_cut(argptr);
          break;
        case 'A':
          reg=atoi(&argptr[1]);
          if(reg>=1 && reg<=3){
            if(argptr[2]!='l' && argptr[2]!='u'){
              fprintf(stderr,"\n\nArgument must be -A#l# or -A#u# .\n");
              exit(-1);
            }
            f = atof(&argptr[3]);
            if(argptr[2]=='l') {
              LOCANGLE_CUT_LOW[reg]=f;
              fprintf(stderr,"Local Angle cut:  reg[%d] L.A. Low: %f:\n",reg,f);
            }

            else
            {
              LOCANGLE_CUT_HIGH[reg]=f;
              fprintf(stderr,"Local Angle cut:  reg[%d] L.A. High: %f:\n",reg,f);
            }
          }
          break;
        case 'u':
          tm_use_mass_cuts();
          mass_upper = atof(++argptr);
          tm_set_mass_upper(mass_upper);
          fprintf(stderr,"setting upper mass cut at %f GeV\n",mass_upper);
          break;
        case 'l':
          tm_use_mass_cuts();
          mass_lower = atof(++argptr);
          tm_set_mass_lower(mass_lower);
          fprintf(stderr,"setting lower mass cut at %f GeV\n",mass_lower);
          break;
        case 't':
          pickwire=atoi(++argptr);
          break;
        case 'T':
          /* trigger mask */
          triggerMask = strtoul(++argptr,NULL,0);
          break;
        case 'Z':
          targetZposition = atof(++argptr);
          printf("Using z-target position: %f\n",targetZposition);
          break;
        case 'H':
          no_histos = 1;
          break;
        case 'B':
          tf = ++argptr;
          sprintf(torus_file,"%s/%s",getenv("CLAS_PARMS"),tf);
          if (access(torus_file,F_OK)){
            fprintf(stderr,
                "Could not find field map %s\n",torus_file);
            fprintf(stderr,"The default will be used\n");
          } else{
            sprintf(trktcl_.spar_torus_bfield_name,tf);
            fprintf(stderr,
                "Magnetic field map %s will be used.\n",trktcl_.spar_torus_bfield_name);
          }

          break;
#ifdef CSQL
        case 'z':
          csql_flag=1;
          break;
#endif
        default:
          fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
          PrintUsage(argv[0]);
          break;
      }
    }
  }

  /*bosdump stuff*/
  if (ngroup) {
    fprintf(stderr,"Groups:\n");
    for (i = 0; i < ngroup; ++i) {
      fprintf(stderr,"%s ",groups[i]);
      if (!(i + 1)%10)
        fputc('\n',stderr);
    }
    fputc('\n',stderr);
  }

  if (nxgroup) {
    fprintf(stderr,"Exclude Groups:\n");
    for (i = 0; i < nxgroup; ++i) {
      fprintf(stderr,"%s ",xgroups[i]);
      if (!(i + 1)%10)
        fputc('\n',stderr);
    }
    fputc('\n',stderr);
  }
  /*end bosdump stuff*/

  bnames_(&nmax);
  initbos();
  if (server)
    argv[1] = server;

  for (i = 1;i < argc; ++i) {
    argptr = argv[i];
    if (*argptr != '-') {

      if (server) {
        initDispatcherDisplay(server,10);
        valid_input_stream = 1;
      }
      else valid_input_stream = initFile(argptr);

      if (valid_input_stream) {
        fprintf(stderr,"Valid Input stream: ");
        server ? fprintf(stderr,"%s\n",server) : fprintf(stderr,"%s\n",argptr);

        while ((max ? Nevents < max : 1) && (ret = getData(&bcs_,"E"))) {
          switch (ret) {
            case DISIO_DATA:
              Nevents++;
              if(event_counter && (Nevents%100 == 0)) {
                if(makentuple) {
                  fprintf(stderr,"%s: %d events processed\t\t%d events passed.    \r",__FILE__,Nevents,Npassed);
                  fflush(stderr);
                }
                else {
                  fprintf(stderr,"  %d events processed \r",Nevents);
                  fflush(stderr);
                }
              }

              HEAD = getBank(&bcs_, "HEAD");
              if(HEAD) runno=HEAD->head[0].nrun;
              if(runno_substitute>=0)runno=runno_substitute;

              /*************************************************/
              /* I think I need this to initialize dc packages */
              /*
                 if (pass_first==0){
                 dc_xvst_init_(&runno);
                 pass_first=1;
                 }
               */

              /*************************************************/

              if(Nevents == 1) {

                /* Extract the file extension number from first file */
                if(ext_loc=strstr(argv[i],ext)){
                  ext_loc += 1;
                  runext = atoi(ext_loc);
                }
#ifdef CSQL
                /* Initialize the MySql monitoring database with csql commands */
                if(csql_flag) csql_setup(runno,runext);
#endif

                /* This MUST be here for the tracking geometry to be set up period. */
                make_RUNC_bank(runno);

                vertex_brun(runno);
                sc_begin_run(runno);
                dc_begin_run(runno);

                hini(outfile,runno);
              }

              if ( !triggerMask || (triggerMask & HEAD->head[0].trigbits)) {
                ConfigEvent(runno,regen_bla || regen_dpl);
                /*re-make TBLA banks - need TRKS, TBER and DC0 to run*/
                if (regen_bla || regen_dpl) {
                  int sector;
                  dropAllBanks(&bcs_, "DC1 ");
                  bankList(&bcs_, "E+", "DC1 ");
                  for(sector=1; sector<=6; sector++) make_DC1_bank(sector);
                  dropAllBanks(&bcs_, bankl);
                  bankList(&bcs_, "E+", bankl);
                  trk_remake_swim_banks(1,
                      trktcl_.dpar_trk_zangle, trktcl_.dpar_trk_zstep[0],
                      trktcl_.dpar_trk_zstep[1],
                      pickwire, regen_bla, regen_dpl,1);
                }

                if (regen_tbtr){
                  dropAllBanks(&bcs_, "TBTR");
                  make_TBTR();
                }
                make_mvrt();
                if(ngroup){
                  PrintEvent(ngroup,groups,nxgroup,xgroups); /*located in PrintEvent.c*/
                }
                ProcessEvent(&Npassed);
                if (makentuple && checkTBLA())
                  ntbla++;
                if (makentuple && Nevents > 999 && !ntbla) {
                  fprintf(stderr,"Sorry... the -N flag is set, but I have processed 1000 events with NO TBLA banks! \n\tCheck to see that the TBLA bank is in the data stream or that the -R flag is turned on to rebuild the TBLA bank.  exiting....%s:%d\n",__FILE__,__LINE__);
                  exit(-1);
                }
              }
              break;
            case DISIO_COMMAND:
              fprintf(stderr,"Command\n");
              break;
          }


          dropAllBanks(&bcs_,"E"); /*drop everything in the E list*/
          cleanBanks(&bcs_);
          if(quit_program)break;
        }

      }
      else {
        fprintf(stderr,"Invalid input stream: \n");
      }

      if(makentuple)
        fprintf(stderr,"\n #  of events processed: %d.\t\t# of events passed: %d.\n",Nevents,Npassed);
      else
        fprintf(stderr,"\n #  of events processed: %d\n",Nevents);
      /*close file*/
      sprintf(mess,"CLOSE BOSINPUT");
      fparm_c(mess);
    }
  }

  if(!no_histos)generate_trk_mon_report(runno, Nevents);
  clean_up(runno, Nevents);
  return 0;
}

int clean_up(int nevents, int runno){
  int i, icycle;

  char chpath[100];
  char chopt[1]=" ";

  sprintf(chpath,"//esr");
  hcdir_(chpath,chopt,strlen(chpath),1);
  hrout(0,icycle,"T");

  if(makentuple)tm_end_ntuples();

  fprintf(stdout, "\n\n");
  hrend_("esr", 3L);


#ifdef CSQL
  if(csql_flag){
    /* print_all_groups(); */
    fill_table("trk_mon","RUNSDCMN");
  }
#endif
}

void hini(char *out,int runno)
{
  int lun = LUN, lrec = LREC, memh = MEMH;
  int istat;
  char def_out[100];

  if(out == NULL) {
    sprintf(def_out,"trkm%d.hbook", runno);
    out = &def_out[0];
  }
  fprintf(stderr, "Output file name is: %s\n", out);
  quest_[9] = 65000;
  hlimit_(&memh);
  hropen_(&lun, "esr", out , "N", &lrec, &istat, 3L, strlen(out), 1L);

  if (!no_histos){
    book_histos(runno);
    return;
  }

  if(makentuple)
    tm_book_ntuples(runno);
}

void book_histos( int runno){
  int i, bins;
  float min, max, v;
  char title[100];
  char runmes[50];
  int sec,sl,reg,offset;

  sprintf(runmes,"Run %d", runno);

  /* "Generic Histograms for the photon monitor" */

  /* Global histograms */

  if(makentuple)tm_book_ntuples(runno);

  sprintf(title, "number of HB tracks per event, %s", runmes);
  hbook1(10, title, 10, 0.0, 10.0, 0);
  sprintf(title, "charge of HB tracks, %s", runmes);
  hbook1(11, title, 5, -2.5,2.5,0);

  sprintf(title, "number of TB tracks per event, %s", runmes);
  hbook1(20, title, 10, 0.0, 10.0, 0);
  sprintf(title, "charge of TBT tracks, %s", runmes);
  hbook1(21, title, 5, -2.5, 2.5, 0);

  sprintf(title, "HB theta for + particles, %s",runmes);
  hbook1(3000, title,180,0.0,180,0);
  sprintf(title, "HB momentum for + particles, %s",runmes);
  hbook1(3001, title,100,0.0,2.0,0);

  for(i=2;i<10;i++){
    offset=1000+100*i;
    sprintf(title, "X (MVRT vertex for %d tracks), %s",i,runmes);
    hbook1(1+offset,title,100,-2.5,2.5,0);
    sprintf(title, "Y (MVRT vertex for %d tracks), %s",i,runmes);
    hbook1(2+offset,title,100,-2.5,2.5,0);
    sprintf(title, "Z (MVRT vertex for %d tracks), %s",i,runmes);
    hbook1(3+offset,title,100,targetZposition-50,targetZposition+50,0);
    sprintf(title, "X vs Y (MVRT vertex for %d tracks), %s",i,runmes);
    hbook2(4+offset,title,100,-2.5,2.5,100,-2.5,2.5,0);
    sprintf(title, "Y vs Z (MVRT vertex for %d tracks), %s",i,runmes);
    hbook2(5+offset,title,100,-2.5,2.5,300,-15,15,0);
    sprintf(title, "Z vs X (MVRT vertex for %d tracks), %s",i,runmes);
    hbook2(6+offset,title,100,-2.5,2.5,300,-15,15,0);
  }

  for(reg=1;reg<=3;reg++){
    sprintf(title, "Local angle, R%d, %s",reg,runmes);
    hbook1(reg + 4000, title, 100, -70.0, 30.0, 0);
  }

  for(sec = 1; sec<=6; sec++){
    for(sl = 1; sl<=6; sl++){
      offset = 10000 * sec + 1000*sl;
      sprintf(title, "Spacial Residual vs Wire, S%d, SL%d, %s",sec,sl,runmes);
      hbook2(1 + offset, title,200,0.5,200.5,100,-0.5,0.5,0);

      sprintf(title, "Fit DOCA vs DTime, S%d, SL%d, %s",sec,sl,runmes);
      hbook2(2 + offset, title,100,0,3000,100,-((sl+1)/2)*1.0,((sl+1)/2)*1.0,0);

      sprintf(title, "Spacial Residual vs Dtime, S%d, SL%d, %s",sec,sl,runmes);
      hbook2(3 + offset, title,100,0,3000.5,100,-0.5,0.5 ,0);

      sprintf(title, "Fit DOCA, S%d, SL%d, %s",sec,sl,runmes);
      hbook1(4+offset, title, 100, -((sl+1)/2)*1.0, ((sl+1)/2)*1.0, 0);

      sprintf(title, "Time Residual vs Wire, S%d, SL%d, %s",sec,sl,runmes);
      hbook2(5 + offset, title,200,0.5,200.5,100,-0.5,0.5,0);

      sprintf(title, "Time Residual vs Dtime, S%d, SL%d, %s",sec,sl,runmes);
      hbook2(6 + offset, title,100,0,3000.5,100,-0.5,0.5 ,0);

      sprintf(title, "Hits per TBT, S%d, SL%d, %s",sec,sl,runmes);
      hbook1(7 + offset, title,7, 0.0, 7.0, 0);

      sprintf(title, "Time Residual vs DOCA, S%d, SL%d, %s",sec,sl,runmes);
      hbook2(8 + offset, title,100,0,((sl+1)/2)*1.0,100,-0.5,0.5 ,0);

    }

    sprintf(title, "Chisq, S%d, %s",sec,runmes);
    hbook1(sec + 2000, title, 1000, 0.0, 100.0, 0);

  }
}

int checkTBLA()
{
  clasTBLA_t *TBLA = NULL;
  TBLA = getBank(&bcs_, "TBLA");
  return((int)TBLA);
}

int ProcessEvent(int *Npassed) {

  /*----Reconstruction Banks---*/
  clasHBTR_t *HBTR = getBank(&bcs_, "HBTR");
  clasMVRT_t *MVRT = getBank(&bcs_, "MVRT");
  /*  clasTBTR_t *TBTR = getBank(&bcs_, "TBTR");*/
  clasBID_t *TBID = getGroup(&bcs_, "TBID",1);
  clasRUNC_t *RUNC = getBank(&wcs_, "RUNC");
  clasTBLA_t *TBLA = NULL;
  clasTBER_t *TBER = NULL;
  clasTRKS_t *TRKS = NULL;
  clasDC1_t *DC1 = NULL;
  vector4_t zero4v = {0.0, {0.0, 0.0, 0.0} };
  int i, j, sec;
  int offset;
  tber_t *tber;
  int nslhits[7];
  int sl;

  /* Just fill in track numbers.
     Check that a trks bank exists. if not, trigger particle is mising, move along. */

  if(!getBank(&bcs_,"TRKS")){
    return(0);
  }

  nevnt++;

  if(nevnt==1){
    for(i=0;i<6;i++){
      snhbt[i]=0;
      sntbt[i]=0;
      snhits[i]=0;
      for(j=0;j<6;j++){
        inhits[i][j]=0;
        iressqrd[i][j]=0;
      }
    }
  }

  if (makentuple) {
    if (tm_fill_ntuples())
      (*Npassed)++;
  }

  if (no_histos){
    return(1);
  }

  if(MVRT){
    offset=1000+MVRT->mvrt->ntrk*100;
    hfill(offset+1,MVRT->mvrt->vert.x,0,1);
    hfill(offset+2,MVRT->mvrt->vert.y,0,1);
    hfill(offset+3,MVRT->mvrt->vert.z,0,1);
    hfill(offset+4,MVRT->mvrt->vert.x,MVRT->mvrt->vert.y,1);
    hfill(offset+5,MVRT->mvrt->vert.y,MVRT->mvrt->vert.z,1);
    hfill(offset+6,MVRT->mvrt->vert.z,MVRT->mvrt->vert.x,1);
  }

  if(HBTR){
    nhbt+=HBTR->bank.nrow;
    hfill(10, HBTR->bank.nrow,0.0,1.0);
    for(i=0; i< HBTR->bank.nrow; i++){
      float p=v3mag(HBTR->hbtr[i].p);
      float cz=HBTR->hbtr[i].p.z/p;
      hfill(11, HBTR->hbtr[i].q, 0.0, 1.0);
      if(HBTR->hbtr[i].q>0){
        hf1(3000,RAD2DEG*acos(cz),1);
        hf1(3001,p,1);
      }

      snhbt[HBTR->hbtr[i].itr_sec/100 - 1]++; /*hbtracks in sector*/
    }
  }


  /*  if(TBTR){
      ntbt+=TBTR->bank.nrow;
      if(TBTR->bank.nrow==3) {n3tbt++;}
      hfill(20, TBTR->bank.nrow,0.0,1.0);
      for(i=0; i< TBTR->bank.nrow; i++){
      hfill(21, TBTR->tbtr[i].q,0.0,1.0);
      sntbt[TBTR->tbtr[i].itr_sec/100 - 1]++;
      }
      }
   */

  for (sec = 1; sec <= 6; sec++){

    if (debug) {
      clasHEAD_t *HEAD = getBank(&bcs_, "HEAD");
      clasTDPL_t *TDPL = NULL;

      pHEAD(stdout, HEAD);

      if (TBLA = getGroup(&bcs_, "TBLA",sec))
        printTBLAbank(stdout, TBLA);
      if (TDPL = getGroup(&bcs_, "TDPL",sec))
        printTDPLbank(stdout, TDPL);
    }

    if  (TBLA = getGroup(&bcs_, "TBLA",sec)){
      for(i=0;i<TBLA->bank.nrow;i++){
        if(TBLA->tbla[i].stat==0 && TBLA->tbla[i].dc1 > -1){
          int offset;
          float residual, t_residual;
          int sl= ((TBLA->tbla[i].trk_pln)%100 - 4)/6 + 1;
          offset =  10000*sec + 1000*sl;
          residual = TBLA->tbla[i].fitdoca - TBLA->tbla[i].calcdoca;
          t_residual = fabs(TBLA->tbla[i].fitdoca)-fabs(TBLA->tbla[i].calcdoca);
          hf2(1+ offset, TBLA->tbla[i].wire , residual,1);
          hf2(2+ offset, TBLA->tbla[i].dtime , TBLA->tbla[i].fitdoca,1);
          hf2(3+ offset, TBLA->tbla[i].dtime , residual,1);
          hf1(4+ offset, TBLA->tbla[i].fitdoca, 1);
          hf2(5+ offset, TBLA->tbla[i].wire , t_residual,1);
          hf2(6+ offset, TBLA->tbla[i].dtime , t_residual,1);
          hf2(8+ offset, fabs(TBLA->tbla[i].fitdoca) , t_residual,1);
          hf1(4000 + ((sl+1)/2), TBLA->tbla[i].alpha*180.0/3.14159, 1);
          nhits++;
          snhits[sec-1]++;
          inhits[sec-1][sl-1]++;
          ressqrd+= residual*residual;
          iressqrd[sec-1][sl-1]+= residual*residual;
        }
      }
    }
  }

  if (TBER = getBank(&bcs_, "TBER")){
    ntbt+=TBER->bank.nrow;
    if(TBER->bank.nrow==3) {n3tbt++;}
    hfill(20, TBER->bank.nrow,0.0,1.0);
    for(i=0;i<TBER->bank.nrow;i++){
      float qop=TBER->tber[i].q_over_p;
      int q=0;

      tber=&TBER->tber[i];
      sec=tber->layinfo2>>24;

      if (qop>0) q=1;
      if (qop<0) q=-1;

      hfill(21,q,0.0,1.0);
      sntbt[sec - 1]++; /*# of tbt in sector*/

      for(j=1;j<=6;j++)nslhits[j]=0;
      for(j=0;j<30;j++){
        sl=1+(j/6);
        nslhits[sl] += (tber->layinfo1>>j)&0x1;
      }
      for(j=0;j<6;j++){
        sl=6;
        nslhits[sl] += (tber->layinfo2>>j)&0x1;
      }
      for(sl=1;sl<=6;sl++){
        offset =  10000*sec + 1000*sl;
        hf1(7+ offset, nslhits[sl], 1);
      }
      hf1(sec+2000, tber->chi2, 1);
    }
  }
  return(1);
}

int generate_trk_mon_report(int runno, int Nevents){
  int i, j, k=0;
  char format[100], printline[100];
  float chi2[6][6];
  float nbin[3]={100,100,300};
  float beam[3]={0,0,0};
  float beam_sig[3]={0,0,0};
  float beam_chi2[3]={0,0,0}; /*chi2 per degree of freedom*/
  float dcmn_db[50]; /* Initialize array for the MySql monitoring database */

  clasRUNC_t *RUNC = getBank(&wcs_, "RUNC");

  memset((void *)&dcmn_db, 0, 50*sizeof(float));

  /*do MVRT bank gaussian fit of 4-track X,Y&Z positions*/
  for(i=0;i<3;i++)
  {
    int offset=1301+i;
    int minbin=(.05*nbin[i]);
    int maxbin=(.95*nbin[i]);
    float min,max;
    float step[3], par[3], pmin[3], pmax[3], sigpar[3];

    /*fit first to get rough guess*/
    par[0]=n3tbt/5; /*set initial guess of height at (#3track events)/5*/
    par[1]=0;       /*set initial guess of mean at 0*/
    par[2]=1;       /*set initial guess of width at 1*/
    quest_[10] = minbin;
    quest_[11] = maxbin;
    hfithn(offset,"G", "rq", 1, par, step,pmin,pmax,sigpar,&(beam_chi2[i]));
    /*fit again in restricted region*/
    min= par[1]-2*par[2];
    max= par[1]+2*par[2];
    hxi_(&offset,&min,&minbin);
    hxi_(&offset,&max,&maxbin);

    minbin--; /*take into account fortran off by one error*/
    maxbin--; /*take into account fortran off by one error*/

    quest_[10] = minbin;
    quest_[11] = maxbin;
    hfithn(offset,"G", "rq", 1, par, step,pmin,pmax,sigpar,&(beam_chi2[i]));
    beam[i]=par[1];
    beam_sig[i]=par[2];
    beam_chi2[i]=beam_chi2[i];
  }

  fprintf(stdout, "trk_mon report for run: %d\n", runno);
  fprintf(stdout, "Number of events processed: %d.\n", Nevents);
  if (RUNC) pRUNC(stdout, &(RUNC->runc));
  fprintf(stdout, "\n");
  for(i=0;i<6;i++){
    for(j=0;j<6;j++){
      int offset;
      float step[1], par[1], pmin[1], pmax[1], sigpar[1];
      offset =  10000*(i+1) + 1000*(j+1);
      par[0]=ntbt / (100*6);   /*set initial guess at constant level*/
      quest_[10] = 25;
      quest_[11] = 75;
      hfithn(4+offset,"P0", "rq", 1, par, step, pmin, pmax, sigpar, &(chi2[i][j]));
    }
  }

  sprintf(format, "       %%3.3f      %%3.3f      %%3.3f      %%3.3f\n");
  printf("\nsector    hbt/evnt   tbt/evnt   tbt/hbt   Nhits/tbt\n");
  for(i=0;i<6;i++){
    sprintf(printline," %2d %s", i+1, format);
    dcmn_db[k++]=((float)snhbt[i])/nevnt;
    dcmn_db[k++]=((float)sntbt[i])/nevnt;
    printf(printline,((float)snhbt[i])/nevnt,((float)sntbt[i])/nevnt,((float)sntbt[i])/snhbt[i],((float)snhits[i])/sntbt[i]);
  }
  sprintf(printline, " all%s", format);
  dcmn_db[k++]=((float)nhbt)/nevnt;
  dcmn_db[k++]=((float)ntbt)/nevnt;
  printf(printline,((float)nhbt)/nevnt,((float)ntbt)/nevnt,((float)ntbt)/nhbt,((float)nhits)/ntbt);

  printf("\n                         SL_RMS\n");
  printf("sector      1       2       3       4       5       6\n");
  for(i=0;i<6;i++){
    printf("%3d   ",i+1);
    for(j=0;j<6;j++){
      dcmn_db[k++]=sqrt(iressqrd[i][j]/inhits[i][j]);
      fprintf(stdout," %6.4f ",sqrt(iressqrd[i][j]/inhits[i][j]));
    }
    printf("\n");
  }
  printf("\n                     SL_DOCA_FLATNESS\n");
  printf("sector      1       2       3       4       5       6\n");
  for(i=0;i<6;i++){
    printf("%3d   ",i+1);
    for(j=0;j<6;j++){
      fprintf(stdout," %6.2f ",chi2[i][j]);
    }
    printf("\n");
  }

  printf("\nnumber of 3 track events: %d\n",n3tbt);
  printf("                     :    X    ,   Y    ,   Z   \n");
  printf("   beam position     : %7.4f , %7.4f, %7.4f\n",beam[0],beam[1],beam[2]);
  printf("   beam fit sigma    : %7.4f , %7.4f, %7.4f\n",beam_sig[0],beam_sig[1],beam_sig[2]);
  printf("   beam fit chi2/dof : %7.4f , %7.4f, %7.4f\n",beam_chi2[0],beam_chi2[1],beam_chi2[2]);

#ifdef CSQL

  /* Fill the DCMN group in the MySql monitoring database */
  if(csql_flag) set_group("DCMN",(char *) &dcmn_db[0]);
#endif

  return 0;

}

void ctrlCHandle(int x)   /* exit the program gracefully and save ntuple if interrupted with ctrl-c. */
{
  int icycle;

  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  fprintf(stderr,"\n\n\t\t\t***  INTERRUPTED!!!  ***\n\n");
  max = 1;
}


#ifdef CSQL
void csql_setup(int runno, int runext)
{
  char *dir = getenv("CLAS_PACK");
  char ddl[100];

  /* Initialize the csql package in $CLAS_PACK/c_sql */
  init_csql();
  init_clas_def();

  /* add RUNS group to save basic run info */
  add_group("RUNS");
  /*  add_column info for run number, run extension, and time*/
  add_column("RUNS","runno");
  init_column("RUNS","runno","INT",COL_TYPE_INT,COL_CW_TBL);
  set_column_int("RUNS","runno",runno);

  add_column("RUNS","runext");
  init_column("RUNS","runext","INT",COL_TYPE_INT,COL_CW_TBL);
  set_column_int("RUNS","runext",runext);

  add_column("RUNS","time");
  init_column("RUNS","time","TIMESTAMP(14)",20,COL_C_TBL);
  set_column_char("RUNS","time","");

  /* Set defaults for db host, username, password, and db name for package */
  set_database(CSQL_Hostname,CSQL_Username,"",CSQL_Database);

  /* Read in the ddl files with the group definitions */
  sprintf(ddl,"%s/bankdefs/dcmn.ddl",dir);
  read_ddlfile2(ddl); /* clasdb system parameters */

}
#endif
/* end file */










/*


   int crypt()
   {
   ;
   }
 */
