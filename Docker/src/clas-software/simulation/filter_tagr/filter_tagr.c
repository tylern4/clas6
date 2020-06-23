/* filter_tagr.c  filter out TAGR banks for GPP */
/* -=======================================================-
RCS ID string
$Id: filter_tagr.c,v 1.5 2009/09/13 09:25:09 pmatt Exp $
$Author: pmatt $
$Revision: 1.5 $
$Date: 2009/09/13 09:25:09 $
* -=======================================================- */

#define USE(var) static void * use_##var = (void *) &var
static char crcsid[] = "$Id: filter_tagr.c,v 1.5 2009/09/13 09:25:09 pmatt Exp $";
USE(crcsid);   /* make sure it is not optimized away */
  static char crname[] = "a1.c";

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



/*------------ PAW DEFINES ----------------- */
#define MEMH 4000000
#define LREC 1024 /* record length of hbook direct access file in WORDS */
#define LUN 3 /* logical unit number of hbook file */


/* re-declare the bos common */
BOSbank bcs_;
BOSbank wcs_;

/* declare the hbook common */
float pawc_[MEMH];
float hcfitd_[25];
int quest_[100];
int Nevents = 0;
int Nwritten =0;
int max = 0;
int trigmask = 0xfff;
int batch_mode=0;

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
  fprintf(stderr,"\n\nUsage: %s [options] -o<outfile> -b<bosfile> file1 [file2] etc....\n\n",processName);

  fprintf(stderr,"\nPurpose:\tFilters out TAGR bank to the file that could be used\n"
	  "\t\tin GPP to add \"accidental\" hits to GSIM data\n\n");


  fprintf(stderr,"  Options:\n");
  fprintf(stderr,"\t-M[#]\t\t Write only # number of events\n");
  fprintf(stderr,"\t-i\t\t Process file in batch mode (no counter)\n");
  fprintf(stderr,"\t-o<outfile>\t Output histogram file name\n");
  fprintf(stderr,"\t-b<outfile>\t Output bos file name where TAGR goes\n");
  fprintf(stderr,"\t-R\t\t Regenerate the PART/TBID and associated banks\n"); 
  fprintf(stderr,"\t-t# \t\t Select trigger mask (exclusive! exact match)\n"
	  "\t\t\t example: -t0x4 select events with trigger bit 3 only\n"
	  "\t\t\t default is any trigger\n");
  fprintf(stderr,"\t-h\t\t Print this message.\n\n");
  exit(0);
}

main(int argc,char **argv){
  FILE *fp = NULL;
  int i,nmax=200;
  char *argptr; 
  char *outfile = NULL;
  char *bosfile = NULL;

  int icycle, ret;
  int id = 99;
  int time_based = 0;
  int runno = 0;
  int regen=0;
  int makepart=0;
  int sec;
  int pid_group=1;
  clasHEAD_t *HEAD = NULL;
  int F_false = 0, F_true = -1;
  int UseThisRunNo = 0;
  char mess[1000];
  static int firsttime = 1; 
  
  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  set_level_(&F_false,&F_false,&F_false,&F_false,&F_false);
  if (argc==1) PrintUsage(argv[0]);
  for (i=1; i<argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {
	
      case 'h': /* print usage */
	PrintUsage(argv[0]);
	break;
	
      case 'o': /* set hbook file name */
	outfile = ++argptr;
	break;
	
      case 'b': /* name of the output file for TAGR */
	bosfile = ++argptr;
	fprintf(stderr,"\nOutput bos file: %s\n",bosfile);
	unlink(bosfile);
	sprintf(mess, "OPEN TAGRBOS UNIT=9 FILE=\"%s\" WRITE STATUS=NEW RECL=3600", bosfile);
	if (!fparm_c(mess)) {
	  fprintf(stderr,"\n%s: Unable to open file \'%s\': %s\n\n",argv[0],mess,strerror(errno));  
	  exit(1);
	}
	
	break;
      case 'M': /* number of events */
	max = atoi(++argptr);
	fprintf(stderr,"\nRequested to filter out %d\tTAGR tevents\n\n",max);
	break;
	
      case 'R': /* rebuild TBID/PART banks */
	regen = 1;
	fprintf(stderr,"\nRequested to rebuild TBID/PART banks\n\n");
	break;
	
      case 'i': /* batch mode */
	batch_mode = 1;
	break;
	
      case 't': /* trigger mask */
	trigmask = strtoul(++argptr,NULL,0);
	break;

      default:
	fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
	PrintUsage(argv[0]);
	break;
      }
    }
  }
  if(bosfile == NULL ) {
    fprintf(stderr,"\n\n !!!!!!!! Output bos file name must be given!\n\n\n");
    exit(1);
  }
  fprintf(stderr,"\nRequested to select events with trigger mask 0x%X\n",trigmask);
 
  bnames_(&nmax);
  initbos();
  configure_banks(stderr,0);

  for (i = 1;i < argc ; ++i) {
    int valid_input_stream = 0;
    argptr = argv[i];
    if (*argptr != '-') {
      sprintf(mess,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", argptr);
      if (!fparm_c(mess)){
	fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",
		argv[0],argptr,strerror(errno));
      }
      else {
	fprintf(stderr,"\n%s\n\n",mess);
	while ((max ? Nwritten < max : 1) && getBOS(&bcs_,1,"E")) {
	  Nevents++;
	  
	  if (HEAD = getBank(&bcs_, "HEAD")) {
	    if (!UseThisRunNo) runno=HEAD->head[0].nrun;
	    
	    if(Nevents == 1) 	hini(outfile,runno); /* book all histograms */
	    
	    ConfigEvent(runno,regen); 
	    /* Initialize the TOF geometry.  This is needed regardless of 
	       whether you remake the SC reconstruction banks or not.  However,
	       for the regeneration case, this is done in ConfigEvent. */

	    if(!regen) ConfigGeom(runno);

	    if (firsttime) {
	      firsttime=0;
	      fprintf(stderr,"\n\n");
	    }

	    if (regen) {
	      dropAllBanks(&bcs_, BID_BANKS);
	      bankList(&bcs_, "E+", BID_BANKS); 
	      make_BID_banks(regen);
	      dropAllBanks(&bcs_, "PARTMVRT");
	      bankList(&bcs_, "E+", "PARTMVRT");
	      make_mvrt();
	      make_PART_group(regen);
	    }
/* 	    countEvents(0, batch_mode,stderr); */
	    /*		if (HEAD->head[0].trigbits & trigmask){	*/	
	    ProcessEvent(pid_group);
	    /*	    } */
	    dropAllBanks(&bcs_,"E"); /*drop everything in the E list*/
	    cleanBanks(&bcs_);
	  }
	}
	  fprintf(stderr,"\n #  of events processed: %d\n"
		  "\n # of events written: %d\n",Nevents,Nwritten);
	  /*close file*/
	  sprintf(mess,"CLOSE BOSINPUT UNIT=1");
	  fparm_c(mess);
      }
    }
  }
  sprintf(mess,"CLOSE TAGRBOS", argptr);
  fparm_c(mess);
  clean_up(runno, Nevents);
}


int ConfigGeom(int runno){
  static int CurrentRun=-1;
  int sec;
  
  if (runno!=CurrentRun){
    dropAllBanks(&wcs_,"SCG SCP ");
    make_SCG_banks(runno);
    
    for(sec=1; sec <= 6; sec++){
      /*clasSCG_t *SCG=NULL;
	if (SCG=getGroup(&wcs_,"SCG ",sec))printSCGbank(stdout,SCG); */
      make_SCP_bank(sec);
    }
    CurrentRun=runno;
  }
}



int clean_up(int runno,int nevents){
  int i, icycle;
  int j;
  
  char chpath[100];
  char chopt[1]=" ";
  float livetime=1.0;
  int firsttime;
  char *dir,map[100];
  
  
  sprintf(chpath,"//esr");
  hcdir_(chpath,chopt,strlen(chpath),1);
  
  hrout(0,icycle,"T");
  /*hldir_(" ", " ", 1L, 1L); don't remove this line */
  
  fprintf(stdout, "\n\n");
  hrend_("esr", 3L);
}

void hini(char *out,int runno)
{
  int lun = LUN, lrec = LREC, memh = MEMH;
  int istat;
  char def_out[100];
  
  if(out == NULL) {
    sprintf(def_out,"filter_tagr_%d.hbook", runno);
    out = &def_out[0];
  }
  fprintf(stderr, "\nOutput hbook file name is: %s\n\n", out);
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


  sprintf(title, "Time of photons according to Tagger (All), %s",runmes);
  hbook1(500, title,800,-20.0,40.0,0);
  sprintf(title, "Time of photons according to Tagger (stat 7 or 15), %s",runmes);
  hbook1(501, title,800,-20.0,40.0,0);
  sprintf(title, "Time of photons according to Tagger (good), %s",runmes);
  hbook1(502, title,800,-20.0,40.0,0);

  sprintf(title, "Time of photons vs goodrf, %s",runmes);
  hbook2(53, title,200,0,100,100,15.0,25.0,0);
  sprintf(title, "Time of photons vs T_id, %s",runmes);
  hbook2(54, title,121,0.5,121.5,400,0.0,200.0,0);
  
  sprintf(title, "T time - RF time vs. T?id!, %s",runmes);
  hbook2(62, title,121,0.5,121.5,84,-2.03,2.03,0);
  sprintf(title, "T time - RF time, %s",runmes);
  hbook1(63, title,84,-2.03,2.03,0);


  sprintf(title, "Photon energy, %s",runmes);
  hbook1(65, title,800,0.5,6.0,0);  
  sprintf(title, "Number of photon energies in TAGR per event, %s",runmes);
  hbook1(90, title,10,0.0,10.0,0);

  sprintf(title, "T tag - RF time vs good RF, %s",runmes);
  hbook2(74, title,200,0.,100.,84,-1.03,1.03,0);

  sprintf(title, "Photon T counter at TBID, %s",runmes);
  hbook1(211, title,150,.5,150.5,0); 
  sprintf(title, "Photon E counter at TBID (ST-TAG lt. 5ns), %s",runmes);
  hbook1(212, title,1000,0.5,1000.5,0); 
 

}
 
int ProcessEvent(int pid_group){
  clasHEAD_t *HEAD = getBank(&bcs_, "HEAD");
  clasTBTR_t *TBTR = getBank(&bcs_, "TBTR");
  clasTAGR_t *TAGR = getBank(&bcs_, "TAGR");
  clasRUNC_t *RUNC = getBank(&wcs_, "RUNC");
  clasCL01_t *CL01 = getBank(&bcs_, "CL01");
  clasBID_t  *TBID = getGroup(&bcs_, "TBID",pid_group);
  clasPART_t *PART = getGroup(&bcs_, "PART",pid_group);
  
  float q;  /* Charge: Should really be an integer! */
  int i,j,sec,trsec,scsec,track,trk_ind;
  
  float mass,pcm,gamma,beta,beta_pr,photon=0.0;
  float ctheta,phi,cthetacm,phicm,energy,p,p2,mass_pr;
  float beta_cm,pzcm;
  
  vector4_t init_reaction,residual,missing_p;
  vector4_t final_reaction = {0.0,0.0,0.0,0.0};
  
  float pion_vertex_time=0;
  int ncharged =0;
  
  /* Fill Tagger Energy spectrum - RF calibration */

  if(PART) {
    for(i=0; i < PART->bank.nrow; i++){
      if (PART->part[i].q != 0) ncharged++;
    }
  }
  
  if(TAGR && 
     CL01 && 
     TBID &&
     (((HEAD->head[0].trigbits & 0xfff) == trigmask) |
      (trigmask == 0xfff))&&
     ncharged >0) {
    
    hf1(90, TAGR->bank.nrow,1.0);
    for(i=0;i<TAGR->bank.nrow;i++){
      hf1(500, TAGR->tagr[i].tpho,1.0);
      if(TAGR->tagr[i].stat == 7 || TAGR->tagr[i].stat == 15)
	hf1(501, TAGR->tagr[i].tpho,1.0);
      hf2(54,TAGR->tagr[i].t_id,TAGR->tagr[i].tpho,1);
      hf1(65, TAGR->tagr[i].erg,1.0); 
      hf2(53,CL01->cl01[0].rf, TAGR->tagr[i].tpho,1);
      hf2(74,CL01->cl01[0].rf,TAGR->tagr[i].ttag - TAGR->tagr[i].tpho,1);
      hf2(62,TAGR->tagr[i].t_id,TAGR->tagr[i].ttag - TAGR->tagr[i].tpho,1);
      hf1(63,TAGR->tagr[i].ttag - TAGR->tagr[i].tpho,1);
   }
    
    
    /* find "good" photon */
    
    tagr_t *tagr = get_photon_tagr(TAGR, TBID, TIME_BASED);
    if (tagr) {
      /* tag "good" photon (add 600 to status) */
      tagr->stat = tagr->stat+600;
      hf1(502, tagr->tpho,1.0);
      hf1(211, tagr->t_id, 1);
      hf1(212, tagr->e_id, 1);
      putBOS(&bcs_, 9, "HEADTAGR");
      Nwritten++;
    }
  }  
      if(!batch_mode && Nevents%1000 == 0)
	fprintf(stderr," # of events processed:\t%d\t"
		"written:\t%d\r",Nevents,Nwritten);

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







