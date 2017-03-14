/*
 * pair_mon:   program for histogramming counter-to-counter offsets for 
 * 	       G1 or E1 runs using the tagger (=RF) time as reference  
 *
 *  Modificatrion by Kijun Park at Feb/18/1999
 *  Version 2.19
 * pair_mon -M[#] -RTBID -e (or -p) -o[making HBOOK file] [called data file]
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

/*------------ PAW DEFINES ----------------- */
#define MEMH 6000000  /* MEMH is depend on space in memory */
#define LREC 1024 /* record length of hbook direct access file in WORDS */
#define LUN 3 /* logical unit number of hbook file */

#define RAD2DEG (180.0/3.14159)

#define MINIMUM_ENTRIES 50 /* Minimum number of entries in a histogram for
			      performing the fit */

#define MEAN 1 /* For hstati */
#define STD_DEV 2 /* For hstati */

/* re-declare the bos common */
BOSbank bcs_;
BOSbank wcs_;

/* declare the hbook common */
float pawc_[MEMH];
float hcfitd_[25];
int iquest_[100];

typedef struct{
  float mean;
  float sigma;
  float err_mean;
  float err_sigma;
  float chi2;
}fit_t;


float par_[3];



/*globally useful variables*/
int runno = 0;
int regen = 0;

/* ----------- Function prototypes ---------------- */
void PrintUsage(char *processName);
int ProcessEvent(),ProcessEvent_ph_plane3(),ProcessEvent_el_plane3(),ProcessEvent_ph_plane4(),ProcessEvent_el_plane4();
int init_hbook(char *outfile,int sector);
void ctrlCHandle(int);
void book_histos(int runno);
void hini(char *out);
int clean_up();


/* --------------------------------------------------- */

void PrintUsage(char *processName)
{
  fprintf(stderr,"Usage: %s [-M] file1 [file2] etc....\n\n",processName);
  fprintf(stderr,"  Options:\n");
  fprintf(stderr,"\t-M[#]\tProcess only # number of events\n");
  fprintf(stderr,"\t-o[hbook_file]\tHBOOK file name\n");
  fprintf(stderr,"\t-e\tProcess with Electron data\n");
  fprintf(stderr,"\t-p\tProcess with Photon data\n");
  fprintf(stderr,"\t-R\tRegenerate the SC, CC and TBID banks\n"); 
  fprintf(stderr,"\t-h\tPrint this message.\n\n");
  exit(0);
}

main(int argc,char *argv[])
{
  FILE *fp = NULL;
  int i,nmax=200;
  char *argptr, *outfile = NULL;
  int Nevents = 0;
  int max = 0;
  char mess[100], choice = NULL;
  int icycle, ret;
  int id = 99;
  int time_based = 0;
  int runno = 0;
  fit_t tof_values[SC_NPADDLES];
  char def_parm[100];
  clasHEAD_t *HEAD=NULL;
  int F_false = 0, F_true = -1;

  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  set_level_(&F_false,&F_false,&F_false,&F_false,&F_false,&F_false,&F_false);
  if (argc==1) PrintUsage(argv[0]);
  for (i=1; i<argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {
      case 'h':
	PrintUsage(argv[0]);
	break;
      case 'o':
	outfile = ++argptr;
	break;
      case 'M':
	max = atoi(++argptr);
	break;
      case 'e':
	choice = 'e';
	break;
      case 'p':
	choice = 'p';
	break;
      case 'R':
	regen =1;
	break;
      default:
	fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
	PrintUsage(argv[0]);
	break;
      }
    }
  } 

  /* Zero the parameter arrays */
  memset(tof_values,0,SC_NPADDLES*sizeof(fit_t));

  /*BOS initialization */
  bnames_(&nmax);
  initbos();

  /* Open the bos files and loop through the events. */
  for (i = 1;i < argc; ++i) {
    argptr = argv[i];
    if (*argptr != '-') {
      sprintf(mess,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", argptr);
      if (!fparm_c(mess)) {
	fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],argptr,strerror(errno));
      }
      else {
	while ((max ? Nevents < max : 1) && getBOS(&bcs_,1,"E")) {
	  Nevents++;
	  if(Nevents == 1){
	    /* Initialize the book file and the scintillator, st, etc. 
	       packages */
	    hini(outfile);
	    if (HEAD=getBank(&bcs_,"HEAD")) runno=HEAD->head[0].nrun;
	    ConfigEvent(runno,regen);              /* Configuration Events */
	  }
	  if (Nevents % 100 == 0) {
	    fprintf(stderr,"%d\r",Nevents);
	    fflush(stderr);
	  }
	  if ( choice == 'e' ) 
	    {
	      ProcessEvent_el_plane3();
	      ProcessEvent_el_plane4();
	      dropAllBanks(&bcs_,"E");
	      cleanBanks(&bcs_);
	    } 
	  
	  if ( choice == 'p' )
	    {
	      ProcessEvent_ph_plane3();
	      ProcessEvent_ph_plane4();
	      dropAllBanks(&bcs_,"E");
	      cleanBanks(&bcs_);
	    } 
	  if (choice == NULL )
	    {
	      printf ( "\n\n\t\t WARNING !!!: YOU Must put electron or photon option \n\n" );
	      break;
	    }
	}
	fprintf(stderr,"#  of events processed: %d\n",Nevents);
	/*close file*/
	sprintf(mess,"CLOSE BOSINPUT", argptr);
	fparm_c(mess);
      }
    }
  }
  
  /* Close the hbook file cleanly */
  clean_up();
}


int clean_up(){
  int i, icycle,all=0;
  
  hrout(0,icycle," ");
  /*hldir_(" ", " ", 1L, 1L);*/ /* don't remove this line */
  hrend_("esr", 3L);
}

/* Open the HBOOK file for writing the histograms */
void hini(char *out)
{
  int lun = LUN, lrec = LREC, memh = MEMH;
  int istat;
  char def_out[100];
  clasHEAD_t *HEAD = getBank(&bcs_, "HEAD");

  if(out == NULL) {
    if (HEAD){
      sprintf(def_out,"tof%d.hbook", HEAD->head[0].nrun);
    } else {
      sprintf(def_out,"tof.hbook");
    }
    out = &def_out[0];
  }  
  fprintf(stderr, "HBOOK file name is: %s\n", out);
 
  iquest_[9] = 65000;
  hlimit_(&memh);
  hropen_(&lun, "esr", out , "N", &lrec, &istat, 3L, strlen(out), 1L);
  book_histos(HEAD->head[0].nrun);
}


/* Book the histograms */
void book_histos( int runno){
  int i,j,k,l,bins;
  float min, max, v;
  char title[100];
  char runmes[50];
  int sec,all=0;
  sprintf(runmes,"Run %d", runno);

  for(i=1;i<7;i++){                      /* i : number of sectors */
    for(j=40;j<43;j++){                  /* j : number of counters 40,41,42 */
      sprintf(title, "Sec %d, id %d, %s",i,j,runmes);
      hbook2(100*i+j,title,100,-5,5,400,-360,50,0.);
    }
  }
  for(k=1;k<7;k++){                      /* i : number of sectors */
    for(l=43;l<49;l++){                  /* j : number of counters 43~48 */
      sprintf(title, "Sec %d, id %d, %s",k,l,runmes);
      hbook2(1000*k+l,title,100,-5,5,400,-360,50,0.);
    }
  }
}

int ProcessEvent_ph_plane3()
{
  /*----RAW Event Banks------*/
  clasHEAD_t *header = NULL;
  
  /*----Reconstruction Banks---*/
  clasTBTR_t *TBTR = NULL;
  clasBID_t *TBID = NULL;
  clasSCRC_t *SCRC = NULL;
  clasSCR_t *SCR = NULL;
  clasSTR_t *STR = NULL;
  clasSCG_t *SCG = NULL;
  float dedx;
  int q,sc,id;
  int i,j,sec,trk_ind;
  float mass,gamma,beta,p,z;
  float pion_vertex_time=0,pion_beta,t_diff;
  tdpl_t *tdpl=NULL;

  /*re-make TBTR and TBID banks*/

  if (regen) TBID=make_BID_banks(1);
  else TBID = getGroup(&bcs_,"TBID",1);
  TBTR = getBank(&bcs_, "TBTR");
  SCG = getBank(&wcs_, "SCG ");

  /* Calculate dEdx in the scintillators and compare vertex times between the 
     tof and tagger with pions identified for pair counters*/
  if (TBTR && TBID){
    for (i=0; i<TBID->bank.nrow;i++) {
      trk_ind=TBID->bid[i].track;
      if(trk_ind){
	p = v3mag(TBTR->tbtr[trk_ind-1].p);
	q = TBTR->tbtr[trk_ind-1].q;
	dedx=tbid2dedx(&TBID->bid[i]);
	if(dedx>4 && dedx<14 && p>0.3 && p<0.7){
	  tdpl=tbtr2tdpl(&(TBTR->tbtr[trk_ind-1]));
	  pion_beta=p/sqrt(p*p+PI_CHARGED_MASS*PI_CHARGED_MASS);
	  pion_vertex_time=TBID->bid[i].sc.time- 
	    pathlen2sc((bid_t *)&TBID->bid[i],(hdpl_t *)tdpl)/
	    SPEED_OF_LIGHT*1e9/pion_beta;
	  t_diff = pion_vertex_time - TBID->bid[i].vtime;
	  if ( SCG ) {
	    int sc_plane;
	    if(sc_plane = scPlane3 ){
	      z= tdpl[sc_plane].pos.z;
	      if (SCRC=getGroup(&bcs_,"SCRC",TBID->bid[i].sec)){
		scrc_t scrc=SCRC->scrc[TBID->bid[i].sc.id-1];
		if(scrc.status<100){
		  /*hf1(TBID->bid[i].sec*100+scrc.id,t_diff,1.);*/
		  hf2(TBID->bid[i].sec*100+scrc.id,t_diff,z,1.);
		}
	      }  
	    }
	  }
	}
      }
    }
  }
  return(1);
  
}

int ProcessEvent_ph_plane4()
{
  /*----RAW Event Banks------*/
  clasHEAD_t *header = NULL;
  
  /*----Reconstruction Banks---*/
  clasTBTR_t *TBTR = NULL;
  clasBID_t *TBID = NULL;
  clasSCRC_t *SCRC = NULL;
  clasSCR_t *SCR = NULL;
  clasSTR_t *STR = NULL;
  clasSCG_t *SCG = NULL;
  float dedx;
  int q,sc,id;
  int k,l,sec,trk_ind;
  float mass,gamma,beta,p,z;
  float pion_vertex_time=0,pion_beta,t_diff;
  tdpl_t *tdpl=NULL;

  /*re-make TBTR and TBID banks*/

  if (regen) TBID=make_BID_banks(1);
  else TBID = getGroup(&bcs_,"TBID",1);
  TBTR = getBank(&bcs_, "TBTR");
  SCG = getBank(&wcs_, "SCG ");

  /* Calculate dEdx in the scintillators and compare vertex times between the 
     tof and tagger with pions identified for pair counters*/
  if (TBTR && TBID){
    for (k=0; k<TBID->bank.nrow;k++) {
      trk_ind=TBID->bid[k].track;
      if(trk_ind){
	p = v3mag(TBTR->tbtr[trk_ind-1].p);
	q = TBTR->tbtr[trk_ind-1].q;
	dedx=tbid2dedx(&TBID->bid[k]);
	if(dedx>4 && dedx<14 && p>0.3 && p<0.7){
	  tdpl=tbtr2tdpl(&(TBTR->tbtr[trk_ind-1]));
	  pion_beta=p/sqrt(p*p+PI_CHARGED_MASS*PI_CHARGED_MASS);
	  pion_vertex_time=TBID->bid[k].sc.time- 
	    pathlen2sc((bid_t *)&TBID->bid[k],(hdpl_t *)tdpl)/
	    SPEED_OF_LIGHT*1e9/pion_beta;
	  t_diff = pion_vertex_time - TBID->bid[k].vtime;
	  if ( SCG ) {
	    int sc_plane;
	    if(sc_plane = scPlane4 ){
	      z= tdpl[sc_plane].pos.z;
	      if (SCRC=getGroup(&bcs_,"SCRC",TBID->bid[k].sec)){
		scrc_t scrc=SCRC->scrc[TBID->bid[k].sc.id-1];
		if(scrc.status<100){
		  /*hf1(TBID->bid[k].sec*100+scrc.id,t_diff,1.);*/
		  hf2(TBID->bid[k].sec*1000+scrc.id,t_diff,z,1.);
		}
	      }  
	    }
	  }
	}
      }
    }
  }
  return(1);
  
}


int ProcessEvent_el_plane3()
{
  /*----RAW Event Banks------*/
  clasHEAD_t *header = NULL;
  
  /*----Reconstruction Banks---*/
  clasBID_t *TBID = NULL;
  clasTBTR_t *TBTR = NULL;
  clasPART_t *PART = NULL;
  clasSCRC_t *SCRC = NULL;
  clasSCG_t *SCG = NULL;
  vector4_t Particle,Pion;
  int sc,id;
  int i,sec,trkid,part_id,index_tbid,trk_ind;
  float pion_vertex_time=0.,z,Part_beta,Part_mom,charge,pion_beta,t_diff,dedx,Part_mass_sq;
  tdpl_t *tdpl=NULL;
  /*=========================================================================*/
  
  /* Compare vertex times between the tof and tagger with knocked out electron from target identified for pair counters for E1 data*/
  
  /* -------------- remake TBID & PART banks -------------------- */
  if (regen) TBID=make_BID_banks(1);
  else TBID = getGroup(&bcs_,"TBID",1);
  
  TBTR = getBank(&bcs_, "TBTR");
  PART = getGroup(&bcs_, "PART",1);  
  SCG = getBank(&wcs_, "SCG ");
  
  if(PART){
    for (i = 0; i < PART->bank.nrow; i++){
      part_id = PART->part[i].pid;
      Particle = PART->part[i].p;
      charge = PART->part[i].q;
      Part_mom = v3mag(Particle.space); 
      if(charge > 0 && Part_mom>0.3 &&  Part_mom<0.7){
        if (TBTR && TBID){
          for (i=0; i<TBID->bank.nrow;i++) {
            trk_ind=TBID->bid[i].track;
            if(trk_ind){
              dedx=tbid2dedx(&TBID->bid[i]);
              if(dedx>4 && dedx<14){
                tdpl=tbtr2tdpl(&(TBTR->tbtr[trk_ind-1]));
                pion_beta=Part_mom/sqrt(Part_mom*Part_mom+
                                        PI_CHARGED_MASS*PI_CHARGED_MASS);
                pion_vertex_time=TBID->bid[i].sc.time- 
                  pathlen2sc((bid_t *)&TBID->bid[i],(hdpl_t *)tdpl)/
                  SPEED_OF_LIGHT*1e9/pion_beta;
                t_diff = pion_vertex_time - TBID->bid[i].vtime;
                if ( SCG ) {
                  int sc_plane;
                  if(sc_plane = scPlane3){
                    z= tdpl[sc_plane].pos.z;
                    if (SCRC=getGroup(&bcs_,"SCRC",TBID->bid[i].sec)){
                      scrc_t scrc=SCRC->scrc[TBID->bid[i].sc.id-1];
                      if(scrc.status<100){
			/*hf1(TBID->bid[i].sec*100+scrc.id,t_diff,1.);*/
			hf2(TBID->bid[i].sec*100+scrc.id,t_diff,z,1.);
                      }
                    }
                  }
		}
	      }
	    }
	  }
	}
      }
    }
  }
  return(1);  
}

int ProcessEvent_el_plane4()
{
  /*----RAW Event Banks------*/
  clasHEAD_t *header = NULL;
  
  /*----Reconstruction Banks---*/
  clasBID_t *TBID = NULL;
  clasTBTR_t *TBTR = NULL;
  clasPART_t *PART = NULL;
  clasSCRC_t *SCRC = NULL;
  clasSCG_t *SCG = NULL;
  vector4_t Particle,Pion;
  int sc,id;
  int k,sec,trkid,part_id,index_tbid,trk_ind;
  float pion_vertex_time=0.,z,Part_beta,Part_mom,charge,pion_beta,t_diff,dedx,Part_mass_sq;
  tdpl_t *tdpl=NULL;
  /*=========================================================================*/
  
  /* Compare vertex times between the tof and tagger with knocked out electron from target identified for pair counters for E1 data*/
  
  /* -------------- remake TBID & PART banks -------------------- */
  if (regen) TBID=make_BID_banks(1);
  else TBID = getGroup(&bcs_,"TBID",1);
  
  TBTR = getBank(&bcs_, "TBTR");
  PART = getGroup(&bcs_, "PART",1);  
  SCG = getBank(&wcs_, "SCG ");
  
  if(PART){
    for (k = 0; k < PART->bank.nrow; k++){
      part_id = PART->part[k].pid;
      Particle = PART->part[k].p;
      charge = PART->part[k].q;
      Part_mom = v3mag(Particle.space); 
      if(charge > 0 && Part_mom>0.3 &&  Part_mom<0.7){
        if (TBTR && TBID){
          for (k=0; k<TBID->bank.nrow;k++) {
            trk_ind=TBID->bid[k].track;
            if(trk_ind){
              dedx=tbid2dedx(&TBID->bid[k]);
              if(dedx>4 && dedx<14){
                tdpl=tbtr2tdpl(&(TBTR->tbtr[trk_ind-1]));
                pion_beta=Part_mom/sqrt(Part_mom*Part_mom+
                                        PI_CHARGED_MASS*PI_CHARGED_MASS);
                pion_vertex_time=TBID->bid[k].sc.time- 
                  pathlen2sc((bid_t *)&TBID->bid[k],(hdpl_t *)tdpl)/
                  SPEED_OF_LIGHT*1e9/pion_beta;
                t_diff = pion_vertex_time - TBID->bid[k].vtime;
                if ( SCG ) {
                  int sc_plane;
                  if(sc_plane = scPlane4){
                    z= tdpl[sc_plane].pos.z;
                    if (SCRC=getGroup(&bcs_,"SCRC",TBID->bid[k].sec)){
                      scrc_t scrc=SCRC->scrc[TBID->bid[k].sc.id-1];
                      if(scrc.status<100){
			/*hf1(TBID->bid[k].sec*100+scrc.id,t_diff,1.);*/
			hf2(TBID->bid[k].sec*1000+scrc.id,t_diff,z,1.);
                      }
                    }
                  }
		}
	      }
	    }
	  }
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
  fprintf(stderr,"\n\n\t\t\t***  Oh~~~Nooooo Stop it..!!!  ***\n\n");

  /* Close the hbook file cleanly */
  clean_up();

  exit(1);
}

/* end file */





