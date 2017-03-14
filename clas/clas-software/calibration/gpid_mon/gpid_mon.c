/* gpid_mon.c 
 *  This utility is to be used for GPID calibration and quality check
 */

/* -=======================================================-
_begin_doc
RCS ID string
$Id: gpid_mon.c,v 1.2 2003/05/06 21:53:39 pasyuk Exp $
$Author: pasyuk $
$Revision: 1.2 $
$Date: 2003/05/06 21:53:39 $
_end_doc
* -=======================================================- */

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
#include <scalers.h>
#include <itape.h>

extern int tag_unbin_(int*); /* in tag package */

/*------------ PAW DEFINES ----------------- */
#define MEMH 4000000
#define LREC 1024 /* record length of hbook direct access file in WORDS */
#define LUN 3 /* logical unit number of hbook file */

#define EB2_LOW 0.4875
#define EB2_HIGH 2.9875
#define NUM_TC 61
#define NUM_TB 121
#define NUM_EC 384
#define NUM_EB 767
#define NUM_EB2 100

#define MASS_HISTOGRAM(NUMBER,TITLE)  hbook1(NUMBER,TITLE, 2000, 1.2, 3.5,0)

#define RAD2DEG (180.0/3.14159)

/* define mass flags fro GPID*/
#define SCTAG_MASS    0 /* mass calculated from SC and TAG */
#define SCST_MASS     1 /* mass claculated from SC and ST */
#define NEUTRAL_MASS -1 /* Neutral particle */
#define PART_MASS     2 /* mass based on PART/TBID */

/* re-declare the bos common */
BOSbank bcs_;
BOSbank wcs_;

/* declare the hbook common */
float pawc_[MEMH];
float hcfitd_[25];
int quest_[100];

int max = 0;


/* ----------- Function prototypes ---------------- */
void PrintUsage(char *processName);
int ProcessEvent(int pid_group);
void ctrlCHandle(int);
void book_histos(int runno);
void hini(char *out,int runno);
int ConfigGeom(int runno);
int clean_up(int runno, int nevents);

/* --------------------------------------------------- */

void PrintUsage(char *processName)
{
  fprintf(stderr,"Usage: %s [options] file1 [file2] etc....\n\n",
	  processName);
  fprintf(stderr,"  Options:\n");
  fprintf(stderr,"\t-o<outfile>\t output hbook file (default=gpid_monXXXXX.hbook) \n");

  fprintf(stderr,"\t-M<#>\t\t Process only # number of events\n");
  fprintf(stderr,"\t-R\t\t Regenerate the TBID/PART and associated banks\n"); 
  fprintf(stderr,"\t-c\t\t Run in calibration mode \n");
  fprintf(stderr,"\t-T<#>\t\t Set trigger bit mask (default=0xffff)\n");
  fprintf(stderr,"\t-h\t\t Print this message.\n\n");
  exit(0);
}

main(int argc,char **argv)
{
  FILE *fp = NULL;
  char *argptr; 
  char *outfile = NULL;
  char mess[200];
  char *server = NULL;

  int Nevents = 0;
  int icycle;
  int ret;
  int id = 99;
  int time_based = 0;
  int runno = 0;
  int regen = 0;
  int sec;
  int pid_group = 0;
  int batch_mode = 0;
  int calib = 0;
  int nmax = 400;

  clasHEAD_t *HEAD = NULL;
  clasGPID_t *GPID = NULL;
  int F_false = 0;
  int F_true = -1;
  int UseThisRunNo = 0;
  int disp_mode = 0;
  int trigmask = 0xffff;
  int i;
  int ii;
  int jj;


  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);

  /*  set_level_(&F_false,&F_false,&F_false,&F_false,&F_false); */
  set_level_(&F_true,&F_true,&F_true,&F_true,&F_true);

  if (argc == 1) PrintUsage(argv[0]);
  for (i=1; i<argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {

      case 'h':
	PrintUsage(argv[0]);
	break;

      case 'c':
	calib=1;
	break;

      case 'M':
	max = atoi(++argptr);
	break;

      case 'R':
	regen =1;
	pid_group=1;
	break;

      case 'T':
	trigmask = strtoul(++argptr,NULL,0);
	break;

      case 'v':
	SetVerbose(1);
	break;

      default:
	fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
	PrintUsage(argv[0]);
	break;
      }
    }
  }

  bnames_(&nmax);
  initbos();
  configure_banks(stderr,0);

  for (i = 1;i < argc ; ++i) {
    argptr = argv[i];
    if (*argptr != '-') {
      sprintf(mess,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ",argptr);
      if(!fparm_c(mess)){
	fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],argptr,strerror(errno));
	exit(0);
      }
      else {
	while ((max ? Nevents < max : 1) && getBOS(&bcs_,1,"E")) {
	  Nevents++;
	  if (HEAD = getBank(&bcs_, "HEAD")) {
	    runno=HEAD->head[0].nrun;
	    if(Nevents == 1){
	      hini(outfile,runno); /* book all histograms */
	      if(!calib) initGPID(runno);	    
	    }
	    ConfigEvent(runno,regen); 
	    /* Initialize the TOF geometry.  This is needed regardless of 
	       whether you remake the SC reconstruction banks or not.  However,
	       for the regeneration case, this is done in ConfigEvent. */
	    if(!regen) ConfigGeom(runno);
	    /*re-make TBID banks*/
	    
	    if (regen) {
	      dropAllBanks(&bcs_, BID_BANKS);
	      bankList(&bcs_, "E+", BID_BANKS); 
	      make_BID_banks(pid_group);
	      dropAllBanks(&bcs_, "PARTMVRT");
	      bankList(&bcs_, "E+", "PARTMVRT");
	      make_mvrt();
	      make_PART_group(pid_group);
	    }
	    
	    if (HEAD->head[0].trigbits & trigmask){		
	      bankList(&bcs_, "E+", "GPID");
	      dropAllBanks(&bcs_, "GPID");
	      clasGPID_t *GPID = makeGPID(pid_group, calib);
	      ProcessEvent(pid_group);
	    }
	  }
	  dropAllBanks(&bcs_,"E"); /*drop everything in the E list*/
	  cleanBanks(&bcs_);
	}
	fprintf(stderr,"\n #  of events processed: %d\n",Nevents);
	/*close file*/
	sprintf(mess,"CLOSE BOSINPUT", argptr);
	fparm_c(mess);
      }
    }
  }
  clean_up(runno, Nevents);
}

int ConfigGeom(int runno){
  static int CurrentRun=-1;
  int sec;
  int ret = 0;
  if (runno!=CurrentRun){
    dropAllBanks(&wcs_,"SCG SCP ");
    make_SCG_banks(runno);
    for(sec=1; sec <= 6; sec++) make_SCP_bank(sec);
    CurrentRun=runno;
    ret = 1;
  }
  return ret;
}



int clean_up(int runno,int nevents){
  int  icycle;
  char chpath[100];
  char chopt[1]=" ";
  char mess[1000];

  sprintf(chpath,"//esr");
  hcdir_(chpath,chopt,strlen(chpath),1);
  hrout(0,icycle,"T");
  /*hldir_(" ", " ", 1L, 1L); don't remove this line */
  hrend_("esr", 3L);
}

void hini(char *out,int runno)
{
  int lun = LUN;
  int lrec = LREC;
  int memh = MEMH;
  int istat;
  char def_out[100];

  if(out == NULL) {
    sprintf(def_out,"gpid_mon%d.hbook", runno);
    out = &def_out[0];
  }
  fprintf(stderr, "Output file name is: %s\n", out);
  quest_[9] = 65000;
  hlimit_(&memh);
  hropen_(&lun, "esr", out , "N", &lrec, &istat, 3L, strlen(out), 1L);
  book_histos(runno);
}

void book_histos( int runno){
  int i, bins;
  float min, max, v;
  char title[100];
  char runmes[50];
  int sec;
  float lbounds[NUM_E_BINS + 2];
  int NID;
  float *lbounds_ptr;
  int zero = 0;
  int BINS = 767;

  sprintf(runmes,"Run %d", runno);

  sprintf(title, "T_left raw TDC", runmes);
  hbook2(12, title, 61, 0.5, 61.5, 512, -0.5, 4095.5, 0);
  sprintf(title, "T_right raw TDC", runmes);
  hbook2(13, title, 61, 0.5, 61.5, 512, -0.5, 4095.5, 0);
  sprintf(title, "E-counter raw TDC", runmes);
  hbook2(14, title, 384, 0.5, 384.5, 512, -0.5, 2047.5, 0);

  sprintf(title, "GPID vs. PID, %s", runmes);
  hbook2(30, title, 50, -0.5, 49.5, 50, -0.5, 49.5, 0);

  sprintf(title, "number of photon for particle vs massref, %s", runmes);
  hbook2(31, title, 6, -1.5, 4.5,6, -1.5, 4.5, 0);

  sprintf(title, "Good RF, %s",runmes);
  hbook1(80, title,400,0.0,200,0);
  sprintf(title, "RF1-RF2, RF1.gt.RF2, %s",runmes);
  hbook1(81, title,80,40,44.0,0);
  sprintf(title, "RF1-RF2, RF1.lt.RF2, %s",runmes);
  hbook1(82, title,80,-40,-36.0,0);
 
  /* Tagger Informational Histograms */

  sprintf(title, "Time of photons according to Tagger, %s",runmes);
  hbook1(50, title,500,-25.0,25.0,0);

  sprintf(title, "Time of photons vs rf1, %s",runmes);
  hbook2(51, title,200,0,100,100,15.,25.0,0);  
  sprintf(title, "Time of photons vs rf2, %s",runmes);
  hbook2(52, title,200,0,100,100,15.0,25.0,0); 
  sprintf(title, "Time of photons vs goodrf, %s",runmes);
  hbook2(53, title,200,0,100,100,15.0,25.0,0);
  sprintf(title, "Time of photons vs T_id, %s",runmes);
  hbook2(54, title,121,0.5,121.5,400,0.0,200.0,0);

  sprintf(title, "Number of Entries in TAGI per event, %s",runmes);
  hbook1(55, title,20,0.0,20.0,0);
  sprintf(title, "E-counter ID seen in TAGI, %s",runmes);
  hbook1(60, title,767,0.5,767.5,0);

  sprintf(title, "T time - E time vs. E?id!, %s",runmes);
  hbook2(61, title,767,0.5,767.5,200,-20,20,0);
  sprintf(title, "T time - RF time vs. T?id!, %s",runmes);
  hbook2(62, title,121,0.5,121.5,84,-2.03,2.03,0);

  sprintf(title, "Photon energy, %s",runmes);
  hbook1(65, title,800,0.5,6.0,0);  
  sprintf(title, "Number of photon energies in TAGR per event, %s",runmes);
  hbook1(90, title,10,0.0,10.0,0);

  sprintf(title, "T counter TDC slope balance L/R, %s",runmes);
  hbook2(70, title,160,0.,1600.,160,0.,32.,0);
  sprintf(title, "T counter TDC slope balance T/RF, %s",runmes);
  hbook2(71, title,160,0.,1600.,160,0.,16.,0);
  sprintf(title, "T tag - RF time vs RF1, %s",runmes);
  hbook2(72, title,200,0.,100.,84,-1.03,1.03,0);
  sprintf(title, "T tag - RF time vs RF2, %s",runmes);
  hbook2(73, title,200,0.,100.,84,-1.03,1.03,0);
  sprintf(title, "T tag - RF time vs good RF, %s",runmes);
  hbook2(74, title,200,0.,100.,84,-1.03,1.03,0);

  /* Start Counter   */

  sprintf(title, " St1 mean - T time vs T id, %s",runmes);
  hbook2(103, title,121,.5,121.5,400,-50.0, 50.0,0);
  sprintf(title, " St1 mean vs T id, %s",runmes);
  hbook2(104, title,121,.5,121.5,400,-50.0, 50.0,0);

  sprintf(title, "Start Counter Sector Number Hit, %s",runmes);
  hbook1(100,title,6,0.5,6.5,0);
  sprintf(title, "ST1 mean - Tag, %s",runmes);
  hbook1(101,title,400,-50.0, 50.0,0);
  sprintf(title, "Photon E counter (ST1-TAG lt. 5ns), %s",runmes);
  hbook1(102, title,1000,0.5,1000.5,0); 
  
  sprintf(title, "ST vert. time - TOF vertex time for pions, %s",runmes);
  hbook1(106,title,300,-30.0,30.0,0);  
  sprintf(title, "LAC vert. time - TOF vertex time for pions, %s",runmes);
  hbook1(107,title,300,-200.0,200.0,0); 
  sprintf(title, "Start Counter vertex time, %s",runmes);
  hbook1(105,title,300,0.0,30.0,0);  
  sprintf(title, "Tagger time v. (ST Vertex Time - tagger time), %s",runmes);
  hbook2(108,title,200,-10.0,10.0,200,0.,40.,0);
  sprintf(title, "tagger time - pion vertex time, %s",runmes);
  hbook1(109,title,300,-15.0,15.0,0);
  
  sprintf(title, "Start Counter 1 pair time, %s",runmes);
  hbook2(14001,title,100,-40.,80.,100,-10.,10.,0);  
  sprintf(title, "Start Counter 2 pair time, %s",runmes);
  hbook2(14002,title,100,-40.,80.,100,-10.,10.,0);  
  sprintf(title, "Start Counter 3 pair time, %s",runmes);
  hbook2(14003,title,100,-40.,80.,100,-10.,10.,0);  

  sprintf(title, "Start Counter 1 time vs. PH, %s",runmes);
  hbook2(15001,title,100,0.,1000.,40,-40.,40.,0);  
  sprintf(title, "Start Counter 2 time vs. PH, %s",runmes);
  hbook2(15002,title,100,0.,1000.,40,-40.,40.,0);  
  sprintf(title, "Start Counter 3 time vs. PH, %s",runmes);
  hbook2(15003,title,100,0.,1000.,40,-40.,40.,0);  
  sprintf(title, "Start Counter 4 time vs. PH, %s",runmes);
  hbook2(15004,title,100,0.,1000.,40,-40.,40.,0);  
  sprintf(title, "Start Counter 5 time vs. PH, %s",runmes);
  hbook2(15005,title,100,0.,1000.,40,-40.,40.,0);  
  sprintf(title, "Start Counter 6 time vs. PH, %s",runmes);
  hbook2(15006,title,100,0.,1000.,40,-40.,40.,0);  

  /* Mass from ToF (PART)*/

  sprintf(title, "Mass from ToF for (+) particles(PART), %s",runmes);
  hbook1(110,title,250,0.0,2.5,0);
  sprintf(title, "Mass from ToF for (-) particles(PART), %s",runmes);
  hbook1(120,title,250,0.0,2.5,0);

  /* beta spectrum (PART)*/

  sprintf(title,"Beta vs. momentum +, %s",runmes);
  hbook2(165,title,100,0.0,2.0,100,0.0,1.2,0);
  sprintf(title,"Beta vs. momentum -, %s",runmes);
  hbook2(166,title,100,0.0,2.0,100,0.0,1.2,0);
  sprintf(title,"Mass vs. momentum +, %s",runmes);
  hbook2(167,title,100,0.0,2.0,210,0.0,2.1,0);
  sprintf(title,"Mass vs. momentum -, %s",runmes);
  hbook2(168,title,100,0.0,2.0,210,0.0,2.1,0);

  /* GPID stuff */

  sprintf(title,"Beta vs. momentum (+) ng>0 (GPID), %s",runmes);
  hbook2(6165,title,100,0.0,2.0,100,0.0,1.2,0);
  sprintf(title,"Beta vs. momentum (-) ng>0 (GPID), %s",runmes);
  hbook2(6166,title,100,0.0,2.0,100,0.0,1.2,0);
  sprintf(title,"Mass vs. momentum (+) ng>0 (GPID), %s",runmes);
  hbook2(6167,title,100,0.0,2.0,210,0.0,2.1,0);
  sprintf(title,"Mass vs. momentum (-) ng>0 (GPID), %s",runmes);
  hbook2(6168,title,100,0.0,2.0,210,0.0,2.1,0);

  sprintf(title,"Beta vs. momentum (+) ng=1 (GPID), %s",runmes);
  hbook2(61165,title,100,0.0,2.0,100,0.0,1.2,0);
  sprintf(title,"Beta vs. momentum (-) ng=1 (GPID), %s",runmes);
  hbook2(61166,title,100,0.0,2.0,100,0.0,1.2,0);
  sprintf(title,"Mass vs. momentum (+) ng=1 (GPID), %s",runmes);
  hbook2(61167,title,100,0.0,2.0,210,0.0,2.1,0);
  sprintf(title,"Mass vs. momentum (-) ng=1 (GPID), %s",runmes);
  hbook2(61168,title,100,0.0,2.0,210,0.0,2.1,0);

  sprintf(title,"Beta vs. momentum (+) ng=0 (GPID), %s",runmes);
  hbook2(60165,title,100,0.0,2.0,100,0.0,1.2,0);
  sprintf(title,"Beta vs. momentum (-) ng=0 (GPID), %s",runmes);
  hbook2(60166,title,100,0.0,2.0,100,0.0,1.2,0);
  sprintf(title,"Mass vs. momentum (+) ng=0 (GPID), %s",runmes);
  hbook2(60167,title,100,0.0,2.0,210,0.0,2.1,0);
  sprintf(title,"Mass vs. momentum (-) ng=0 (GPID), %s",runmes);
  hbook2(60168,title,100,0.0,2.0,210,0.0,2.1,0);

  sprintf(title,"Beta vs. momentum pi+ (GPID), %s",runmes);
  hbook2(7108,title,100,0.0,2.0,100,0.0,1.2,0);
  sprintf(title,"Beta vs. momentum pi- (GPID), %s",runmes);
  hbook2(7109,title,100,0.0,2.0,100,0.0,1.2,0);
  sprintf(title,"Beta vs. momentum K+ (GPID), %s",runmes);
  hbook2(7111,title,100,0.0,2.0,100,0.0,1.2,0);
  sprintf(title,"Beta vs. momentum K- (GPID), %s",runmes);
  hbook2(7112,title,100,0.0,2.0,100,0.0,1.2,0);
  sprintf(title,"Beta vs. momentum p (GPID), %s",runmes);
  hbook2(7114,title,100,0.0,2.0,100,0.0,1.2,0);
  sprintf(title,"Beta vs. momentum d (GPID), %s",runmes);
  hbook2(7145,title,100,0.0,2.0,100,0.0,1.2,0);

  sprintf(title,"Mass vs. momentum pi+ (GPID), %s",runmes);
  hbook2(8108,title,100,0.0,2.0,210,0.0,2.1,0);
  sprintf(title,"Mass vs. momentum pi- (GPID), %s",runmes);
  hbook2(8109,title,100,0.0,2.0,210,0.0,2.1,0);
  sprintf(title,"Mass vs. momentum K+ (GPID), %s",runmes);
  hbook2(8111,title,100,0.0,2.0,210,0.0,2.1,0);
  sprintf(title,"Mass vs. momentum K- (GPID), %s",runmes);
  hbook2(8112,title,100,0.0,2.0,210,0.0,2.1,0);
  sprintf(title,"Mass vs. momentum p (GPID), %s",runmes);
  hbook2(8114,title,100,0.0,2.0,210,0.0,2.1,0);
  sprintf(title,"Mass vs. momentum d (GPID), %s",runmes);
  hbook2(8145,title,100,0.0,2.0,210,0.0,2.1,0);

  sprintf(title,"dbeta vs. betaSCST pi+ (GPID), %s",runmes);
  hbook2(9108,title,100,0.2,1.2,100,-0.2,0.2,0);
  sprintf(title,"dbeta vs. betaSCST pi- (GPID), %s",runmes);
  hbook2(9109,title,100,0.2,1.2,100,-0.2,0.2,0);
  sprintf(title,"dbeta vs. betaSCST K+ (GPID), %s",runmes);
  hbook2(9111,title,100,0.2,1.2,100,-0.2,0.2,0);
  sprintf(title,"dbeta vs. betaSCST K- (GPID), %s",runmes);
  hbook2(9112,title,100,0.2,1.2,100,-0.2,0.2,0);
  sprintf(title,"dbeta vs. betaSCST p (GPID), %s",runmes);
  hbook2(9114,title,100,0.2,1.2,100,-0.2,0.2,0);
  sprintf(title,"dbeta vs. betaSCST d (GPID), %s",runmes);
  hbook2(9145,title,100,0.2,1.2,100,-0.2,0.2,0);


  sprintf(title, "Beta spectrum for tracks in %s",runmes);
  hbook1(170, title,75,0.0,1.5,0);

  /* Target Histograms */

  sprintf(title, "Calculated Vertex Position (x vs. z) (MVRT), %s",runmes);
  hbook2(190, title,200,-50.0,50.0,200,-50.0,50.0,0);
  sprintf(title, "Calculated Vertex Position (y vs. z) (MVRT), %s",runmes);
  hbook2(200, title,200,-50.0,50.0,200,-50.0,50.0,0);
  
  /* Physics Histograms */
  sprintf(title, "Photon energy at TBID, %s",runmes);
  hbook1(210, title,100,0.0,6.0,0); 
  sprintf(title, "Photon T counter at TBID, %s",runmes);
  hbook1(211, title,150,.5,150.5,0); 
  sprintf(title, "Photon E counter at TBID (ST-TAG lt. 5ns), %s",runmes);
  hbook1(212, title,1000,0.5,1000.5,0); 
  
  hbook2(213, "2[g] mass vs. momentum", 250, 0.0, 1.0, 200, 0.0, 4.0, 0);
  sprintf(title, "Photon energy at PART, %s",runmes);
  hbook1(215, title,100,0.0,6.0,0);  
  sprintf(title, "Photon T counter id at PART, %s",runmes);
  hbook1(216, title,150,0.5,150.5,0);  
  sprintf(title, "Photon E counter id at PART, %s",runmes);
  hbook1(217, title,767,0.5,767.5,0);  

  /* exclusive p pi+ pi- */

  sprintf(title, "p [p]^+! [p]^-! MM2 (GPID) , %s",runmes);
  hbook1(800, title,2000,-0.5,1.5,0);
  sprintf(title, "p [p]^+! [p]^-! MM2 (PART) , %s",runmes);
  hbook1(801, title,2000,-0.5,1.5,0);
  sprintf(title, "p [p]^+! [p]^-! MM2 (PART)+/-1ns , %s",runmes);
  hbook1(802, title,2000,-0.5,1.5,0);

  sprintf(title, "p [p]^+! [p]^-! photon pattern, %s",runmes);
  hbook1(900, title,4,-0.5,3.5,0);

  /*  some single particle MM */ 

  /* from GPID */

  /* 1) gp -> pi- X   */
  sprintf(title, "MM from [g]p -> [p]- X ngrf>0 GPID, %s",runmes);
  hbook1(250, title,200,0.0,2.0,0);
  sprintf(title, "MM from [g]p -> [p]- X ngrf=1 GPID, %s",runmes);
  hbook1(252, title,200,0.0,2.0,0);

  /* 2) gp -> pi+ X   */
  sprintf(title, "MM from [g]p -> [p]+ X ngrf>0 GPID, %s",runmes);
  hbook1(260, title,200,0.0,2.0,0);
  sprintf(title, "MM from [g]p -> [p]+ X ngrf=1 GPID, %s",runmes);
  hbook1(262, title,200,0.0,2.0,0);
  
  /* 3) gp -> K+ X */
  sprintf(title, "MM from [g]p -> K+ X ngrf>0 GPID, %s",runmes);
  hbook1(270, title,200,0.0,2.0,0);
  sprintf(title, "MM from [g]p -> K+ X ngrf=1 GPID, %s",runmes);
  hbook1(272, title,200,0.0,2.0,0);

  
  /* 4) gp -> p X */
  sprintf(title, "MM from [g]p -> p X ngrf>0 GPID, %s",runmes);
  hbook1(280, title,200,0.0,2.0,0);
  sprintf(title, "MM from [g]p -> p X ngrf=1 GPID, %s",runmes);
  hbook1(282, title,200,0.0,2.0,0);

  /* from PART */

  /* 1) gp -> pi- X   */
  sprintf(title, "MM from ([g]p -> [p]- X) PART, %s",runmes);
  hbook1(251, title,200,0.0,2.0,0);

  /* 2) gp -> pi+ X   */
  sprintf(title, "MM from ([g]p -> [p]+ X) PART, %s",runmes);
  hbook1(261, title,200,0.0,2.0,0);

  
  /* 3) gp -> K+ X */
  sprintf(title, "MM from ([g]p -> K+ X) PART, %s",runmes);
  hbook1(271, title,200,0.0,2.0,0);

  /* 4) gp -> p X */
  sprintf(title, "MM from ([g]p -> p X) PART, %s",runmes);
  hbook1(281, title,200,0.0,2.0,0);
  sprintf(title, "MM from ([g]p -> p X) PART, %s",runmes);
  hbook1(291, title,200,0.0,2.0,0);

  /* dvtime vs beta */
  sprintf(title,"pi+ ng=0 GPID %s",runmes);
  hbook2(6000+PiPlus,title, 50, 0., 1., 100, -5., 5., 0);
  sprintf(title,"pi- ng=0 GPID %s",runmes);
  hbook2(6000+PiMinus,title, 50, 0., 1.,100, -5., 5., 0);
  sprintf(title,"K+ ng=0 GPID %s",runmes);
  hbook2(6000+KPlus,title, 50, 0., 1., 100, -5., 5.,0);
  sprintf(title,"K- ng=0 GPID %s",runmes);
  hbook2(6000+KMinus,title, 50, 0., 1., 100, -5., 5.,0);
  sprintf(title,"p ng=0 GPID %s",runmes);
  hbook2(6000+Proton,title, 50, 0., 1., 100, -5., 5.,0);
  sprintf(title,"d ng=0 GPID %s",runmes);
  hbook2(6000+Deuteron,title, 50, 0., 1., 100, -5., 5.,0);
  
  sprintf(title,"pi+ ng=1 GPID %s",runmes);
  hbook2(6100+PiPlus,title, 50, 0., 1., 100, -5., 5.,0);
  sprintf(title,"pi- ng=1 GPID %s",runmes);
  hbook2(6100+PiMinus,title, 50, 0., 1., 100, -5., 5.,0);
  sprintf(title,"K+ ng=1 GPID %s",runmes);
  hbook2(6100+KPlus,title, 50, 0., 1., 100, -5., 5.,0);
  sprintf(title,"K- ng=1 GPID %s",runmes);
  hbook2(6100+KMinus,title, 50, 0., 1., 100, -5., 5.,0);
  sprintf(title,"p ng=1 GPID %s",runmes);
  hbook2(6100+Proton,title, 50, 0., 1., 100, -5., 5.,0);
  sprintf(title,"d ng=1 GPID %s",runmes);
  hbook2(6100+Deuteron,title, 50, 0., 1., 100, -5., 5.,0);
  
  sprintf(title,"pi+ ng>1 GPID %s",runmes);
  hbook2(6200+PiPlus,title, 50, 0., 1., 100, -5., 5.,0);
  sprintf(title,"pi- ng>1 GPID %s",runmes);
  hbook2(6200+PiMinus,title, 50, 0., 1., 100, -5., 5.,0);
  sprintf(title,"K+ ng>1 GPID %s",runmes);
  hbook2(6200+KPlus,title, 50, 0., 1., 100, -5., 5.,0);
  sprintf(title,"K- ng>1 GPID %s",runmes);
  hbook2(6200+KMinus,title, 50, 0., 1., 100, -5., 5.,0);
  sprintf(title,"p ng>1 GPID %s",runmes);
  hbook2(6200+Proton,title, 50, 0., 1., 100, -5., 5.,0);
  sprintf(title,"d ng>1 GPID %s",runmes);
  hbook2(6200+Deuteron,title, 50, 0., 1., 100, -5.,5.,0);
  
  sprintf(title,"pi+ massref=PART %s",runmes);
  hbook2(6300+PiPlus,title, 50, 0., 1., 100, -5., 5.,0);
  sprintf(title,"pi- massref=PART %s",runmes);
  hbook2(6300+PiMinus,title, 50, 0., 1., 100, -5., 5.,0);
  sprintf(title,"K+ massref=PART %s",runmes);
  hbook2(6300+KPlus,title, 50, 0., 1., 100, -5., 5.,0);
  sprintf(title,"K- massref=PART %s",runmes);
  hbook2(6300+KMinus,title, 50, 0., 1., 100, -5., 5.,0);
  sprintf(title,"p massref=PART %s",runmes);
  hbook2(6300+Proton,title, 50, 0., 1., 100, -5., 5.,0);
  sprintf(title,"d massref=PART %s",runmes);
  hbook2(6300+Deuteron,title, 50, 0., 1., 100, -5.,5.,0);

}

int ProcessEvent(int pid_group)
{
  /*----RAW Event Banks------*/
  clasHEAD_t *HEAD = getBank(&bcs_, "HEAD");
  clasTAGT_t *TAGT = getBank(&bcs_, "TAGT");
  clasTAGE_t *TAGE = getBank(&bcs_, "TAGE");
  clasST_t   *ST   = getBank(&bcs_, "ST  ");
  
  /*----Reconstruction Banks---*/
  clasHBTR_t *HBTR = getBank( &bcs_, "HBTR");
  clasHBID_t *HBID = getBank( &bcs_, "HBID");
  clasTBTR_t *TBTR = getBank( &bcs_, "TBTR");
  clasBID_t  *TBID = getGroup(&bcs_, "TBID", pid_group);
  clasSCPB_t *SCPB = NULL;
  clasSCRC_t *SCRC = NULL;
  clasSTR_t  *STR  = getBank( &bcs_, "STR ");
  clasST1_t  *ST1  = getBank( &bcs_, "ST1 ");
  clasTAGI_t *TAGI = getBank( &bcs_, "TAGI");
  clasTAGR_t *TAGR = getBank( &bcs_, "TAGR");
  clasPART_t *PART = getGroup(&bcs_, "PART", pid_group);
  clasRUNC_t *RUNC = getBank( &wcs_, "RUNC");
  clasCL01_t *CL01 = getBank( &bcs_, "CL01");
  clasMVRT_t *MVRT = getBank( &bcs_, "MVRT");
  clasGPID_t *GPID = getGroup(&bcs_, "GPID", pid_group);
  hdpl_t     *hdpl = NULL;
  tdpl_t     *tdpl = NULL;  
  clasTGEO_t *TGEO = (clasTGEO_t *) getBank(&wcs_,"TGEO");
  
  float ztarget = TGEO ? TGEO->tgeo[0].z : 0.0;
  float vtime;
  float dvtime; 
  float tprop;
  float mass;
  float pcm;
  float gamma;
  float beta;
  float beta_pr;
  float photon = 0.0;
  float ctheta;
  float phi;
  float cthetacm;
  float phicm;
  float energy;
  float p;
  float p2;
  float mass_pr;
  float beta_cm;
  float pzcm;
  float residual;
  float missing_p;
  float undetected_masssq;
  float undetectedM;
  float dedx;
  float q;  /* Charge: Should really be an integer! */
  float pion_vertex_time = 0;
  float xoff;
  float yoff; 
  float st_time;
  float st_tag_diff;
  float pion_beta;
  float t_diff;
  float diff;
  float lac_vert_time;
  float tagdiff;
  
  int grow;
  int hid;
  int ii; 
  int k;
  int i;
  int j;
  int sec;
  int trsec;
  int scsec;
  int track;
  int trk_ind;
  int inbox = 0;
  int nprotons = 0;
  int npiplus = 0;
  int npiminus = 0;
  int nkplus = 0;
  int proton_ind = -1;
  int piplus_ind = -1;
  int piminus_ind = -1;
  int kplus_ind = -1;
  int g3part = 0;
  int p3part = 0;
  int g_pattern;
  int btid;
  int tid;
  int row;
  int column;
  
  vector4_t photon_v4;
  vector4_t beam = {0.0, 0.0, 0.0, 0.0};
  vector4_t final_state;
  vector4_t undetected;
  vector4_t init_reaction;
  vector4_t zero4v = {0.0, 0.0, 0.0, 0.0};
  vector4_t final_reaction = {0.0,0.0,0.0,0.0};
  vector3_t pmom = {0.0,0.0,0.0};
  
  sortpart_t part;
  tagr_t *tagr = NULL;
  
  /* Just fill in track numbers */
  
  if (HEAD) hf1(5, HEAD->head[0].evtclass, 1.);
  
  if(HBTR){
    hf1(10, HBTR->bank.nrow,1.);	
    for(i=0; i< HBTR->bank.nrow; i++){
      float p  = v3mag(HBTR->hbtr[i].p);
      float cz = HBTR->hbtr[i].p.z/p;
      hfill(11, HBTR->hbtr[i].q, 0., 1.);
      if(HBTR->hbtr[i].q>0){
	hf1(13000,RAD2DEG*acos(cz),1.);
	hf1(13001,p,1.);	  
      }
    }
  }
  
  if(TBTR){
    hf1(20, TBTR->bank.nrow,1.);
    for(i=0; i< TBTR->bank.nrow; i++){
      hf1(21, TBTR->tbtr[i].q,1.);
    }
  }
  
  /* tagger occupancies from the raw banks TAGT and TAGE */
  if(TAGT){
    for(i=0; i<TAGT->bank.nrow; i++){
      if(TAGT->tagt[i].id >0 && TAGT->tagt[i].id <62){
	if(TAGT->tagt[i].tdcl>0 && TAGT->tagt[i].tdcl<4095) 
	  hf2(12, TAGT->tagt[i].id, TAGT->tagt[i].tdcl,1.);
	if(TAGT->tagt[i].tdcr>0 && TAGT->tagt[i].tdcr<4095) 
	  hf2(13, TAGT->tagt[i].id, TAGT->tagt[i].tdcr,1.);
      }
    }
  }
  if(TAGE){
    for(i=0; i<TAGE->bank.nrow; i++){
      if(TAGE->tage[i].id >0 && 
	 TAGE->tage[i].id <385 && 
	 TAGE->tage[i].tdc>0){
	hf2(14, TAGE->tage[i].id, TAGE->tage[i].tdc,1.);
      }
    }
  }
  
  
  /* Fill Tagger Energy spectrum - RF calibration */
  
  if(TAGR && CL01){
    hf1(90, TAGR->bank.nrow,1.);
    for(i=0;i<TAGR->bank.nrow;i++){
      hf1(50,TAGR->tagr[i].tpho,1.);
      hf2(54,TAGR->tagr[i].t_id,TAGR->tagr[i].tpho,1.);
      hf1(65,TAGR->tagr[i].erg,1.0); 
      hf2(51, CL01->cl01[0].rf1, TAGR->tagr[i].tpho,1.);
      hf2(52, CL01->cl01[0].rf2, TAGR->tagr[i].tpho,1.);
      hf2(53, CL01->cl01[0].rf,  TAGR->tagr[i].tpho,1.);
      hf2(72, CL01->cl01[0].rf1, TAGR->tagr[i].ttag-TAGR->tagr[i].tpho,1.);
      hf2(73, CL01->cl01[0].rf2, TAGR->tagr[i].ttag-TAGR->tagr[i].tpho,1.);
      hf2(74, CL01->cl01[0].rf,  TAGR->tagr[i].ttag-TAGR->tagr[i].tpho,1.);
    }
  }
  
  /* Tagger E counter & T counter calibration */
  
  if (TAGI){
    hf1(55, TAGI->bank.nrow,1.0);
    for (i=0; i < TAGI->bank.nrow; i++) {
      btid = TAGI->tagi[i].idt;
      tid  = tag_unbin_(&btid);
      hf1(60, TAGI->tagi[i].ide,1.0);
      hf2(61,TAGI->tagi[i].ide,
	  (TAGI->tagi[i].timel+TAGI->tagi[i].timer)/2.-TAGI->tagi[i].timee,1);
      hf2(62,TAGI->tagi[i].idt,TAGI->tagi[i].trf,1);
      row    = (tid-1)/8;
      column = (tid-1)%8;
      xoff   = 50. + column * 200.;
      yoff   = 2. + row * 4.;
      hf2(70, xoff+TAGI->tagi[i].timemean,
	  yoff+TAGI->tagi[i].timel-TAGI->tagi[i].timer,1.);
      yoff = 1. + row * 2.;
      hf2(71, xoff+TAGI->tagi[i].timemean, yoff+TAGI->tagi[i].trf,1.);
    }
  }
  
  /* Get some information about the RF times */
  
  if (CL01){
    hf1(80, CL01->cl01[0].rf,1.0);
    if(CL01->cl01[0].rf1 > CL01->cl01[0].rf2){
      hf1(81, CL01->cl01[0].rf1-CL01->cl01[0].rf2,1.);
    } 
    else {
      hf1(82, CL01->cl01[0].rf2-CL01->cl01[0].rf1,1.);
    }
  }
  
  /* Fill Start counter occupancy hits */
  
  if (STR){
    for (i=0; i < STR->bank.nrow; i++) {
      hf1(100, STR->str[i].id,1.0);
    }
  }
  
  /* ST1  TDCs */
  if(ST1){
    for (i=0; i < ST1->bank.nrow; i++) {
      hf2(14000+ST1->st1[i].id,   (ST1->st1[i].time_1+ST1->st1[i].time_2)/2.,
	    ST1->st1[i].time_1-ST1->st1[i].time_2,1.0);
      hf2(15000+2*ST1->st1[i].id-1,ST1->st1[i].adc_1,ST1->st1[i].time_1,1.);
      hf2(15000+2*ST1->st1[i].id,  ST1->st1[i].adc_2,ST1->st1[i].time_2,1.);
    }
  }
  

  if(ST1 && TAGR){
    for(i=0; i < ST1->bank.nrow; i++){
      for(j = 0; j < TAGR->bank.nrow; j++) {
	st_time =  (ST1->st1[i].time_1 + ST1->st1[i].time_2)/2.;
	st_tag_diff = st_time - TAGR->tagr[j].tpho;
	hf1(101, st_tag_diff , 1);
	hf2(103, TAGR->tagr[j].t_id,st_tag_diff,1);
	hf2(104, TAGR->tagr[j].t_id,st_time, 1);
	if(st_tag_diff < 10.0 && st_tag_diff > 0.) 
	  hf1(102, TAGR->tagr[j].e_id, 1.);
      }
    }
  }
  /* Calculate dEdx in the scintillators and compare vertex times between the 
     tof and the start counter/tagger with pions identified this way */

  if (TBTR && TBID){
    for (i=0; i<TBID->bank.nrow;i++) {
      trk_ind = TBID->bid[i].track;
      pion_beta = 0.;
      if(trk_ind){
	p = v3mag(TBTR->tbtr[trk_ind-1].p);
	q = TBTR->tbtr[trk_ind-1].q;
	dedx = tbid2dedx(&TBID->bid[i]); 
	
	if(dedx>4 && dedx<14 && p>0.3 && p<0.7){
	  /* Assume this is a pion*/
	  tdpl = tbtr2tdpl(&(TBTR->tbtr[trk_ind-1]));
	  pion_beta = p/sqrt(p*p+PI_CHARGED_MASS*PI_CHARGED_MASS);
	  pion_vertex_time = TBID->bid[i].sc.time - 
	    pathlen2sc((bid_t *)&TBID->bid[i],(hdpl_t *)tdpl)/
	    SPEED_OF_LIGHT*1e9/pion_beta;
	  
	  if(TBID->bid[i].st.stat){
	    diff = TBID->bid[i].st.vtime-pion_vertex_time;
	    hf1(106,diff,1.);
	  }
	  if(TBID->bid[i].lac.stat){
	    lac_vert_time = TBID->bid[i].lac.time-tdpl[LACPlane].tlen/
	      pion_beta/SPEED_OF_LIGHT*1e9;
	    diff = lac_vert_time  - pion_vertex_time;
	    hf1(107,diff,1.);
	  }
	  if (TAGR && MVRT){
	    tprop = MVRT->mvrt[0].vert.z/SPEED_OF_LIGHT*1e9;
	    for (j=0;j<TAGR->bank.nrow;j++){
	      tagdiff = TAGR->tagr[j].tpho+tprop-pion_vertex_time;
	      hf1(109,tagdiff,1.);
	    }
	  }
	}
      }
    }
  }

  if (TBTR && TBID && TAGR){
    for(i=0;i<TBID->bank.nrow;i++) {
      /* Time difference between start counter and tagger */ 
      if(TBID->bid[i].st.stat>0){
	for(j=0;j<TAGR->bank.nrow;j++){
	  /* Start counter timing histograms */
	  hf1 (105,TBID->bid[i].st.vtime,1.0);
	  tprop = TBTR->tbtr[TBID->bid[i].track-1].vert.z/SPEED_OF_LIGHT*1e9;
	  hf2(108,TBID->bid[i].st.vtime-TAGR->tagr[j].tpho-tprop,
	      TAGR->tagr[j].tpho,1.0);
	}
      }
    }
  }

  /* Look at some reactions (select only those events with TAGR) */
  
  if (TAGR && TBID && TBTR && RUNC && (tagr = get_photon_tagr(TAGR, TBID, TIME_BASED))){

    hf1(211, tagr->t_id, 1.);
    hf1(212, tagr->e_id, 1.);
    
    photon = get_photon_energy(TAGR, TBID);
    hf1(210, photon, 1);
    beam.space.x = beam.space.y = 0.;
    beam.t = beam.space.z = photon;
    /* Fill initial reaction information */
    
    init_reaction = v4add(beam, RUNC->runc.target);
    
    beta_cm = init_reaction.space.z/init_reaction.t;
    for (i=0; i < TBID->bank.nrow; i++){
      trk_ind = TBID->bid[i].track-1;
      final_reaction = zero4v;
      beta = TBID->bid[i].beta;
      
      if(trk_ind>-1){ 
	p = v3mag(TBTR->tbtr[trk_ind].p);
	pmom = TBTR->tbtr[trk_ind].p;
	
	if (beta > 0.){
	  hf1(170, beta, 1.);
	  if (TBTR->tbtr[trk_ind].q > 0.)
	    hf2(165, p, beta, 1.);
	  else
	    hf2(166, p, beta, 1.);
	}

	if (beta < 1. && beta > 0.){
	  energy = p/beta;
	  gamma = beta2gamma(beta);
	  mass = p/gamma/beta;
	  if (TBTR->tbtr[trk_ind].q > 0.) 
	    hf1(110, mass, 1.);
	  else 
	    hf1(120, mass, 1.);
	  if (TBTR->tbtr[trk_ind].q > 0.0)
	    hf2(167,p,mass,1.0);
	  else
	    hf2(168,p,mass,1.0); 
	}
      }
    }
    
    
    if (PART){
      hf1(215, photon, 1);
      hf1(216, tagr->t_id, 1);
      hf1(217, tagr->e_id, 1);
      
      /* look at GPID */
      if(GPID) {
	inbox = 0;
	for(grow=0; grow < GPID->bank.nrow; grow++) {
	  
	  /* count good pions and protons */
	  if ((GPID->gpid[grow].mass_ref == SCTAG_MASS ||
	       GPID->gpid[grow].mass_ref == PART_MASS) &&
	      GPID->gpid[grow].ngrf == 1  &&
	      abs(GPID->gpid[grow].pid) == Proton) {
	    nprotons++;
	    proton_ind = grow;
	  }
	  if ((GPID->gpid[grow].mass_ref == SCTAG_MASS ||
	       GPID->gpid[grow].mass_ref == PART_MASS) &&
	      GPID->gpid[grow].ngrf == 1 &&
	      abs(GPID->gpid[grow].pid) == PiPlus) {
	    npiplus++;
	    piplus_ind = grow;
	  }
	  if ((GPID->gpid[grow].mass_ref == SCTAG_MASS ||
	       GPID->gpid[grow].mass_ref == PART_MASS) &&
	      GPID->gpid[grow].ngrf == 1 &&
	      abs(GPID->gpid[grow].pid) == PiMinus) {
	    npiminus++;
	    piminus_ind = grow;
	  }
	  if ((GPID->gpid[grow].mass_ref == SCTAG_MASS ||
	       GPID->gpid[grow].mass_ref == PART_MASS) &&
	      fabs(GPID->gpid[grow].vert.z)<=11.&&
	      GPID->gpid[grow].ngrf == 1 &&
	      abs(GPID->gpid[grow].pid) == KPlus) {
	    nkplus++;
	    kplus_ind = grow;
	  }
	  
	  /* compare PID and GPID */
	  
	  if (GPID->gpid[grow].q != 0 &&
	      fabs(GPID->gpid[grow].vert.z)<=11.){
	    hf2(31,GPID->gpid[grow].mass_ref,GPID->gpid[grow].ngrf,1.);
	    if(GPID->gpid[grow].mass_ref != SCST_MASS) {
	      hf2(30,GPID->gpid[grow].ppid,GPID->gpid[grow].pid,1.); 
	      p = v3mag(GPID->gpid[grow].p.space);
	      beta = GPID->gpid[grow].betam;
	      mass = GPID->gpid[grow].mass;
	      
	      if (GPID->gpid[grow].ngrf !=0) {
		if (GPID->gpid[grow].q > 0){
		  hf2(6165, p,beta, 1.);
		  hf2(6167, p, GPID->gpid[grow].mass,1.);
		}
		if (GPID->gpid[grow].q < 0){
		  hf2(6166, p,beta, 1.);
		  hf2(6168, p, GPID->gpid[grow].mass,1.);
		}
	      }
	      if (GPID->gpid[grow].ngrf == 1) {
		if (GPID->gpid[grow].q > 0){
		  hf2(61165, p,beta, 1.);
		  hf2(61167, p, GPID->gpid[grow].mass,1.);
		  
		}
		if (GPID->gpid[grow].q < 0){
		  hf2(61166, p,beta, 1.);
		  hf2(61168, p, GPID->gpid[grow].mass,1.);
		}
	      }
	      else if (GPID->gpid[grow].ngrf == 0) {
		if (GPID->gpid[grow].q > 0){
		  hf2(60165, p,beta, 1.);
		  hf2(60167, p, GPID->gpid[grow].mass,1.);
		}
		if (GPID->gpid[grow].q < 0){
		  hf2(60166, p,beta, 1.);
		  hf2(60168, p, GPID->gpid[grow].mass,1.);
		}
	      }
	    }
	  }
	  
	  if(GPID->gpid[grow].q && 
	     fabs(GPID->gpid[grow].vert.z)<=10. &&
	     GPID->gpid[grow].mass_ref != SCST_MASS &&
	     !(GPID->gpid[grow].pid == Electron ||
	       GPID->gpid[grow].pid == Positron) ){
	    /*  e+ and e- excluded too */
	    if (GPID->gpid[grow].ngrf == 1) {
	      p = v3mag(GPID->gpid[grow].p.space);
	      beta = GPID->gpid[grow].betam;
	      mass = GPID->gpid[grow].mass;
	      hid = 7100+GPID->gpid[grow].pid;
	      hf2(hid, p,beta, 1.);
	      hid = 8100+GPID->gpid[grow].pid;
	      hf2(hid, p, GPID->gpid[grow].mass,1.);
	      hid = 9100+GPID->gpid[grow].pid;
	      hf2(hid, beta, beta-GPID->gpid[grow].beta,1.);
	    }
	    
	    tprop = (GPID->gpid[grow].vert.z-ztarget)/LIGHT_SPEED;
	    
	    for(ii=0; ii < TAGR->bank.nrow; ii++) {
	      if (TAGR->tagr[ii].stat == 7 || TAGR->tagr[ii].stat == 15){ 
		vtime = TBID->bid[GPID->gpid[grow].trkid-1].st.vtime;
		dvtime = TAGR->tagr[ii].tpho+tprop - vtime;
		hid=6000+abs(GPID->gpid[grow].pid);
		if(GPID->gpid[grow].ngrf == 0){
		  hf2(hid, GPID->gpid[grow].beta, dvtime, 1.);
		}
		if(GPID->gpid[grow].ngrf == 1){
		  hf2(hid+100, GPID->gpid[grow].beta, dvtime, 1.);
		  vtime = GPID->gpid[grow].sc_time - 
		    GPID->gpid[grow].sc_len/LIGHT_SPEED/GPID->gpid[grow].beta;
		  dvtime = TAGR->tagr[ii].tpho+tprop - vtime;
		  hf2(hid+300, GPID->gpid[grow].beta, dvtime, 1.);
		} 
		if(GPID->gpid[grow].ngrf > 1) {
		  hf2(hid+200, GPID->gpid[grow].beta, dvtime, 1.);
		} 
	      }
	    }
	  }
	}
	/* select pi+pi-p events */
	undetected_masssq = -1.;
	if (nprotons == 1 &&
	    npiplus  == 1 &&
	    npiminus == 1 &&
	    fabs(MVRT->mvrt[0].vert.z) <= 11.) {
	  
	  if (GPID->gpid[proton_ind].tagrid ==
	      GPID->gpid[piplus_ind].tagrid &&
	      GPID->gpid[proton_ind].tagrid ==
	      GPID->gpid[piminus_ind].tagrid) {
	    /* all three particles from the same proton */
	    
	    g_pattern = 0;
	    
	    beam.t = beam.space.z = GPID->gpid[proton_ind].epho;
	    init_reaction = v4add(beam, RUNC->runc.target);
	    final_state = v4add(GPID->gpid[proton_ind].p, 
				GPID->gpid[piplus_ind].p);
	    final_state = v4add(final_state, 
				GPID->gpid[piminus_ind].p);
	    undetected = v4sub (init_reaction, final_state);
	    undetected_masssq = v4dot(undetected,undetected);
	    
	    hf1(800,undetected_masssq,1.);
	    hf1(900,g_pattern,1.);
	    if(fabs(undetected_masssq < 0.005)) g3part = 1;
	  }
	  else {
	    if (GPID->gpid[proton_ind].tagrid ==
		GPID->gpid[piplus_ind].tagrid)
	      g_pattern = 1;
	    if (GPID->gpid[proton_ind].tagrid ==
		GPID->gpid[piminus_ind].tagrid)
	      g_pattern = 2;
	    if (GPID->gpid[piplus_ind].tagrid ==
		GPID->gpid[piminus_ind].tagrid)
	      g_pattern = 3;
	    hf1(900,g_pattern,1.);
	  }
	  /* done with pi+pi-p in GPID */
	}
	
	/* deal with single track events in GPID*/
	
	if (nprotons == 1 &&
	    fabs(MVRT->mvrt[0].vert.z) <= 11.){
	  /* 1 proton */
	  beam.t = beam.space.z = GPID->gpid[proton_ind].epho;
	  init_reaction = v4add(beam, RUNC->runc.target);
	  undetected = v4sub (init_reaction, GPID->gpid[proton_ind].p);
	  undetectedM = v4mass(undetected);
	  hf1(280, undetectedM, 1.);
	  if (TBTR->bank.nrow == 1){
	    hf1(290, undetectedM, 1.);
	    if (GPID->gpid[proton_ind].ngrf == 1) hf1(292,undetectedM,1.);
	  }
	  if (GPID->gpid[proton_ind].ngrf == 1) hf1(282,undetectedM,1.);
	}
	if (npiplus == 1 &&
	    fabs(MVRT->mvrt[0].vert.z) <= 11.){
	  beam.t = beam.space.z = GPID->gpid[piplus_ind].epho;
	  init_reaction = v4add(beam, RUNC->runc.target);
	  undetected = v4sub (init_reaction, GPID->gpid[piplus_ind].p);
	  undetectedM = v4mass(undetected);
	  hf1(260, undetectedM, 1.);
	  if (GPID->gpid[piplus_ind].ngrf == 1) hf1(262,undetectedM,1.);
	}
	if (npiminus == 1 &&
	    fabs(MVRT->mvrt[0].vert.z) <= 11.){
	  beam.t = beam.space.z = GPID->gpid[piminus_ind].epho;
	  init_reaction = v4add(beam, RUNC->runc.target);
	  undetected = v4sub (init_reaction, GPID->gpid[piminus_ind].p);
	  undetectedM = v4mass(undetected);
	  hf1(250, undetectedM, 1.);
	  if (GPID->gpid[piminus_ind].ngrf == 1) hf1(252,undetectedM,1.);
	}
	if (nkplus == 1 &&
	    fabs(MVRT->mvrt[0].vert.z) <= 11.){
	  beam.t = beam.space.z = GPID->gpid[kplus_ind].epho;
	  init_reaction = v4add(beam, RUNC->runc.target);
	  undetected = v4sub (init_reaction, GPID->gpid[kplus_ind].p);
	  undetectedM = v4mass(undetected);
	  hf1(270, undetectedM, 1.);
	  if (GPID->gpid[kplus_ind].ngrf == 1) hf1(272,undetectedM,1.);
	}
	
	/* end single track events with GPID*/
	
      }
      /* end GPID */
      
      /*count photons to see if more than one in the bucket */
      int n_phot = 0;
      tagr = get_photon_tagr(TAGR, TBID, TIME_BASED);
      for (k=0;k<TAGR->bank.nrow;k++){
	if((TAGR->tagr[k].stat == 7 || 
	    TAGR->tagr[k].stat == 15)&& 
	   (fabs(TAGR->tagr[k].tpho-tagr->tpho) <=1.)) n_phot++;
      }
      if(n_phot == 1){
	
	/* now get the same reaction from PART bank */
	undetected_masssq = -1.;
	sortpart(PART,&part);	
	if (part.nPip  == 1 &&
	    part.nPim  == 1 &&
	    part.nprot == 1 &&
	    fabs(MVRT->mvrt[0].vert.z) <= 11.) {
	  photon=get_photon_energy(TAGR,TBID);
	  tagr = get_photon_tagr(TAGR, TBID, TIME_BASED);
	  beam.t = beam.space.z = photon;
	  init_reaction = v4add(beam, RUNC->runc.target);
	  final_state = v4add((part.prot[0]->p), 
			      (part.Pip[0]->p) );
	  final_state = v4add(final_state, 
			      (part.Pim[0]->p));
	  
	  undetected = v4sub (init_reaction, final_state);
	  
	  undetected_masssq = v4dot(undetected,undetected);
	  int pitrkid = part.Pip[0]->trkid -1;
	  hf1(801,undetected_masssq,1.);
	  if(fabs(TBID->bid[pitrkid].sc.vtime-TBID->bid[pitrkid].st.vtime)<=1){
	    if(fabs(undetected_masssq) <0.005) p3part = 1; 
	    hf1(802,undetected_masssq,1.);
	  }
	}
	
	/* single track with PART bank */
	
	if(part.nprot == 1 &&
	   fabs(MVRT->mvrt[0].vert.z)<=10.){
	  photon = get_photon_energy(TAGR,TBID);
	  tagr = get_photon_tagr(TAGR, TBID, TIME_BASED);
	  beam.t = beam.space.z = photon;
	  init_reaction = v4add(beam, RUNC->runc.target);
	  undetected = v4sub (init_reaction, part.prot[0]->p);
	  undetectedM = v4mass(undetected);
	  hf1(281,undetectedM,1.);
	  if (TBTR->bank.nrow == 1) hf1(283, undetectedM, 1.);
	}
	if(part.nPip == 1 &&
	   fabs(MVRT->mvrt[0].vert.z)<=10.){
	  photon= get_photon_energy(TAGR,TBID);
	  tagr = get_photon_tagr(TAGR, TBID, TIME_BASED);
	  beam.t = beam.space.z = photon;
	  init_reaction = v4add(beam, RUNC->runc.target);
	  undetected = v4sub (init_reaction, part.Pip[0]->p);
	  undetectedM = v4mass(undetected);
	  hf1(261,undetectedM,1.);
	}
	if(part.nPim == 1 &&
	   fabs(MVRT->mvrt[0].vert.z)<=10.){
	  photon = get_photon_energy(TAGR,TBID);
	  tagr = get_photon_tagr(TAGR, TBID, TIME_BASED);
	  beam.t = beam.space.z = photon;
	  init_reaction = v4add(beam, RUNC->runc.target);
	  undetected = v4sub (init_reaction, part.Pim[0]->p);
	  undetectedM = v4mass(undetected);
	  hf1(251,undetectedM,1.);
	}
	if(part.nKp == 1 &&
	   fabs(MVRT->mvrt[0].vert.z)<=10.){
	  photon = get_photon_energy(TAGR,TBID);
	  tagr = get_photon_tagr(TAGR, TBID, TIME_BASED);
	  beam.t = beam.space.z = photon;
	  init_reaction = v4add(beam, RUNC->runc.target);
	  undetected = v4sub (init_reaction, part.Kp[0]->p);
	  undetectedM = v4mass(undetected);
	  hf1(271,undetectedM,1.);
	}
	
	/* end of single track evnets in PART */
      }
      
      /*-------------------------------------*/
      for(j=0;j<PART->bank.nrow;j++){     
	if(PART->part[j].pid == 14){ /*Proton*/
	  if(tagr->t_id%2)
	    hf1(2000,(tagr->t_id+1)/2,1.);
	  else
	    hf1(2000,tagr->t_id/2,1.);
	  hf1(3000,tagr->e_id,1.);
	  hf1(4000,tagr->erg,1.);
	  
	}
	if(PART->part[j].pid == 8){ /*Pi+*/
	  
	  if(tagr->t_id%2)
	    hf1(2001,(tagr->t_id+1.)/2,1);
	  else
	    hf1(2001,tagr->t_id/2,1.);
	  
	  hf1(3001,tagr->e_id,1.);
	  hf1(4001,tagr->erg,1.);
	  
	}
	if(PART->part[j].pid == 9){ /*Pi-*/
	  
	  if(tagr->t_id%2)
	    hf1(2002,(tagr->t_id+1)/2,1.);
	  else
	    hf1(2002,tagr->t_id/2,1.);
	  
	  hf1(3002,tagr->e_id,1.);
	  hf1(4002,tagr->erg,1.);
	  
	}
	if(PART->part[j].q!=0){ /*All charged particles*/
	  
	  if(tagr->t_id%2)
	    hf1(2003,(tagr->t_id+1)/2,1.);
	  else
	    hf1(2003,tagr->t_id/2,1.);
	  
	  hf1(3003,tagr->e_id,1.);
	  hf1(4003,tagr->erg,1.);
	  
	}
	if(PART->part[j].pid == 0){ /*Unknown*/
	  
	  if(tagr->t_id%2)
	    hf1(2004,(tagr->t_id+1)/2,1.);
	  else
	    hf1(2004,tagr->t_id/2,1.);
	  
	  hf1(3004,tagr->e_id,1.);
	  hf1(4004,tagr->erg,1.);
	  
	}
      }
    }
  }
  return(1);
}
  
void ctrlCHandle(int x)
{
  int icycle;

  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  fprintf(stderr,"\n\n\t\t\t***  INTERRUPTED!!!  ***\n\n");
  max = 1;
}

/* end file */







