/*
 * sc_mon.c   SC (time of flight) offline monitoring program
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
#include <scExtern.h>
#include <pid.h>
#include <utility.h>
#include <particleType.h>

void bnames_(int* nmax);

/*----------- SC_MON SCANNING BOUNDERIES ------- */
/*----------- Use to detect bad Channels  ------- */
#define LOWER_SC_SCAN 0.6
#define HIGHER_SC_SCAN 1.5


/*------------ PAW DEFINES ----------------- */
#define MEMH 2000000
#define LREC 1024 /* record length of hbook direct access file in WORDS */
#define LUN 3 /* logical unit number of hbook file */
#define ID 99 /* column-wise-ntuple id */

#define RAD2DEG (180.0/3.14159)
/* declare the bos common */
BOSbank bcs_;

extern sc_const_t sc_pedestals;

/* declare the hbook common */
float pawc_[MEMH];
float hcfitd_[25];
int quest_[100];
float z0=100.0;
float range = 6.0;
float pcut = 12.0;
int tid=24;

/* ----------- Function prototypes ---------------- */
void PrintUsage(char *processName);
int ProcessEvent(int bankflag, int sc_cut);
void ctrlCHandle(int);
void book_histos(int runno, int sc_cut);
void hini(int runno, char *out);
int fill_efficiency_histos(int sec,int sc_cut);
int fill_call_histos();
int fill_rf_histos(int sec);
void get_hardware_status(int runno);
void configure(int runNo,int bankflag);
float getTimePhoton(clasTAGR_t *TAGR, clasBID_t *BID, int tid);
void copy_pipeline_tdc_();
/* --------------------------------------------------- */

void PrintUsage(char *processName)
{
  fprintf(stderr,"Usage: %s [-M] file1 [file2] etc....\n\n",processName);
  fprintf(stderr,"  Options:\n");
  fprintf(stderr,"\t-c[#]\tMaximum absolute difference between predicted scintillator id \n\t\tmatching a track and the actual scintillator that fired.\n\n");
  fprintf(stderr,"\t-r[float#]choosing the timing historgram range (default 6.0ns).\n\n");
  fprintf(stderr,"\t-p[float#]choosing the momentum cut for tof mass historgram range(default 12.0GeV).\n\n");
  fprintf(stderr,"\t-t[float#]set the maximun T counter id for Pion vertex time - sc vtime histograms (h521-h527)(default 24).\n\n");
  fprintf(stderr,"\t-T\tset beam type (1=photon beam, 0=e beam)\n\n");
  /*  the above 4 options added by Lei Guo */


  fprintf(stderr,"\t-M[#]\tProcess only # number of events\n\n");
  fprintf(stderr,"\t-o[outfile]\tWrite histograms to hbook output file <outfile>\n\n");
  fprintf(stderr,"\t-d[outfile]\tProcess bad channel detection. outfile is the output ASCII file\n\n");
  fprintf(stderr,"\t-b\tbatch mode (no counter)\n\n");
  fprintf(stderr,"\t-R\tRemake SC1,SCR,SCRC, and TBID banks\n\n");
  fprintf(stderr,"\t-s\tGenerate text file with SC harware status: -R flag required\n\n");
  fprintf(stderr,"\t-h\tPrint this message.\n\n");
  exit(0);
}


main(int argc,char **argv)
{
  FILE *fp = NULL;
  int i;
  char *argptr, *outfile = NULL;
  char *outfile_scan = NULL;
  int Nevents = 0;
  int max = 0;
  int j;
  char mess[100];
  int icycle, ret;
  int id = 99, flag = 0;
  float beam_energy, Q_live;
  int bankflag = 0;
  int sc_cut=1,runno=0, evtno=0;
  int batch = 0;
  int hardware_stat = 0;
  int F_false = 0, F_true = -1;
  int beam_type = 1;
  int CurrentRun = 0;
  int nmax = 300;

  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  set_level_(&F_false,&F_false,&F_false,&F_false,&F_false);
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


      case 'R':
	bankflag = 1;
	break;
      case 'c':
	sc_cut=atoi(++argptr);
	break;
      case 'b':
	batch = 1;
	break;
      case 's':
	hardware_stat = 1;
	break;
      case 'd':
	outfile_scan = ++argptr;
	break;

/*  	//following options are added by Lei Guo */
      case 'T':
	beam_type = atoi(++argptr);
	set_beam_type(beam_type);
	break;
/*  	//set beam type */
      case 'r':
	range = atof(++argptr) ;
	break;
/*  	// set the historgram range for 501-506, 511-516, 521-526 */
      case 'p':
	pcut = atof(++argptr) ;
	break;
/*  	//set the momentum cut for TOF mass histogram 411-416 */
      case 't':
	tid = atoi(++argptr) ;
	break;
/*  	//set the maximun T counter id for Pion vertex time - sc vtime histograms */



      default:
	fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
	PrintUsage(argv[0]);
	break;
      }
    }
  }

  bnames_(&nmax);
  initbos();
  trk_set_def_();

  for (i = 1;i < argc; ++i) {
    argptr = argv[i];
    if (*argptr != '-') {
      sprintf(mess,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", argptr);
      if (!fparm_c(mess)) {
	fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],argptr,strerror(errno));
      }
      else {
	while ((max ? Nevents < max : 1) && getBOS(&bcs_,1,"E")) {
	  clasHEAD_t *HEAD=getBank(&bcs_,"HEAD");
	  if (HEAD) {
	    runno=HEAD->head[0].nrun;
	    evtno=HEAD->head[0].nevent;
	  }
	  Nevents++;
       if(runno >= 55357)
         copy_pipeline_tdc_();

	  if (runno != CurrentRun) {
	  //printf("RUN NUMBER:%d\n",runno);
	    configure(runno,bankflag);
	    CurrentRun = runno;
         set_sc_version_constants(runno);
	  }
	  if(Nevents == 1){
	    hini(runno,outfile);
	    book_histos(runno,sc_cut);
	  }
	  if ((Nevents % 100 == 0) && !batch) {
	    fprintf(stderr,"%d\r",Nevents);
	    fflush(stderr);
	  }
	  ProcessEvent(bankflag,sc_cut);
	  dropAllBanks(&bcs_,"E");
	  dropBank(&bcs_, "TRKS", 0);
	  cleanBanks(&bcs_);

	}

	if (hardware_stat) {
	  get_hardware_status(runno);
	}


	fprintf(stderr,"#  of events processed: %d\n",Nevents);
	/*close file*/
	sprintf(mess,"CLOSE BOSINPUT", argptr);
	fprintf(stderr, "err = %d\n", fparm_c(mess));
      }

    }

  }

  /* Detection of bad channels :   12/24/1999 */
   if (outfile_scan != NULL) ProcessScanning(outfile_scan,runno);



  /* Divide the actual hits by the projected hits to get the efficiency */
  for (j=1;j<=6;j++) hopera(200+j,"/",210+j,220+j,1.0,1.0);

  hrout(0,icycle," ");
  hrend_("esr", 3L);

}

void configure(int runNo,int bankflag)
{
  initialize_tof(runNo);
  ConfigEvent(runNo,bankflag);
}

void hini(int runno, char *out)
{
  int lun = LUN, lrec = LREC, memh = MEMH;
  int istat, icycle;
  char def_out[256];

  sprintf(def_out,"scm%d.hbk",runno);
  if(out == NULL) out = &def_out[0];
  quest_[9] = 65000;
  hlimit_(&memh);
  hropen_(&lun, "esr", out , "N", &lrec, &istat, 3L, strlen(out), 1L);

  return;
}

void book_histos( int runno, int sc_cut){
  int i, id, bins;
  float min, max, v;
  char title[100];
  char runmes[50];
  int sec;
  float locMaxRange = SC_NPADDLES_SEC + 0.5;

  sprintf(runmes,"Run %d", runno);

  /*Global histos*/
  sprintf(title, "SC hits by category, %s", runmes);
  hbook1(1000, title, 100, 0.0, 100.0, 0);
  sprintf(title, "SC mass sq., %s", runmes);
  hbook1(1001, title, 250, -0.5, 2. ,0);
  sprintf(title,"60 hz monitor, %s",runmes);
  hbook1(10002, title, 100, 0, 9000, 0);
  sprintf(title, "RF1 vs RF2, %s", runmes);
  hbook2(10003, title, 100, 0, 200,100,0,200, 0);

 /* Scintillator Informational Histograms */
  sprintf(title,"pion vertex time - sc vtime %s",runmes);
  hbook1(507,title,400, -10.0, 10.0,0);
  sprintf(title,"pion vertex time - sc vtime, t_id<=%d, %s",tid, runmes);
  hbook1(527,title,400, -10.0, 10.0,0); 
  sprintf(title,"tpho, t_id <=%d, %s",tid, runmes);
  hbook1(508,title,400, -20.0, 20.0,0); 

  
   sprintf(title, "pion vertex time-start counter vertex time %s",
	runmes);
    hbook1(510,title,400,-10,10,0); 
  sprintf(title,"ToF dEDx vs p (+)ve tracks, %s",runmes);
  hbook2(1002,title,100,0.0,2.0,200,0.0,100.0,0); 

  sprintf(title,"ToF dEDx vs p, (-)ve tracks, %s",runmes);
    hbook2(1003,title,100,0.0,2.0,200,0.0,100.0,0); 

 sprintf(title,"beta vs p, all tracks, %s",runmes);
  hbook2(1007,title,100,0.0,4.0,200,0.0,1.2,0);
/*  //histograms 1007 added by Lei Guo */

  for (i=1;i<7;i++){
    sprintf(title, "Mass vs. sc id, sec %d, %s",i,runmes);
    hbook2(400+i,title,SC_NPADDLES_SEC,0.5,locMaxRange,200,0,2,0);
    sprintf(title, "Mass vs. sc id, sec %d, with momentum cut%s",i,runmes);
    hbook2(410+i,title,SC_NPADDLES_SEC,0.5,locMaxRange,200,0,2,0);
/*      //check the TOF mass with a momentum cut, added by Lei Guo */
    
    sprintf(title, "pion vertex time-tagger time vs. sc id, sec %d, %s",i,
	runmes);
    hbook2(500+i,title,SC_NPADDLES_SEC,0.5,locMaxRange,200,-range,range,0);
    sprintf(title, "pion vertex time-tagger time vs. sc id, sec %d, t_id<=%d, %s",i, tid, runmes);
    hbook2(520+i,title,SC_NPADDLES_SEC,0.5,locMaxRange,200,-range,range,0);

    sprintf(title, "pion vertex time-start counter vertex time , sec %d, %s",i,
	runmes);
    hbook1(510+i,title,400,-range,range,0);
  }


  /*by sector*/
  for (sec=1; sec<7; sec++){
    sprintf(title,"occupancy, SC bank sec %d, %s", sec, runmes);
    hbook1(0 + sec, title, SC_NPADDLES_SEC, 0.5, locMaxRange, 0);
    sprintf(title,"Hits, left, sec %d, %s", sec, runmes);
    hbook1(10 + sec, title, SC_NPADDLES_SEC, 0.5, locMaxRange, 0);
    sprintf(title,"Hits, right, sec %d, %s", sec, runmes);
    hbook1(20 + sec, title, SC_NPADDLES_SEC, 0.5, locMaxRange, 0);

 
    sprintf(title,"SCR occupancy, sec %d, %s", sec, runmes);
    hbook1(30 + sec, title, SC_NPADDLES_SEC, 0.5, locMaxRange, 0);
    sprintf(title,"SCR-TDPL diff., sec %d, %s", sec, runmes);
    hbook1(40 + sec, title, 100, 0, 100, 0);
    sprintf(title,"SCR-TDPL x diff., sec %d, %s", sec, runmes);
    hbook1(50 + sec, title, 200, -50, 50, 0);
    sprintf(title,"SCR-TDPL y diff., sec %d, %s", sec, runmes);
    hbook1(60 + sec, title, 200, -50, 50, 0);
    sprintf(title,"SCR-TDPL z diff., sec %d, %s", sec, runmes);
    hbook1(70 + sec, title, 200, -50, 50, 0);
    sprintf(title,"SCR-TDPL diff.(fwd), sec %d, %s", sec, runmes);
    hbook1(80 + sec, title, 100, 0, 100, 0);
    sprintf(title,"SCR-TDPL diff. x(fwd), sec %d, %s", sec, runmes);
    hbook1(90 + sec, title, 200, -50, 50, 0);
    sprintf(title,"SCR-TDPL diff. y(fwd), sec %d, %s", sec, runmes);
    hbook1(100 + sec, title, 200, -50, 50, 0);
    sprintf(title,"SCR-TDPL diff. z(fwd), sec %d, %s", sec, runmes);
    hbook1(110 + sec, title, 200, -50, 50, 0);
    sprintf(title,"SCR, id vs y, sec %d, %s", sec, runmes);
    hbook2(120 + sec, title, 100, -250, 250, SC_NPADDLES_SEC, .5, locMaxRange, 0);
    sprintf(title,"SCR BOTH_HIT occ., sec %d, %s", sec, runmes);
    hbook1(130 + sec, title, SC_NPADDLES_SEC, 0.5, locMaxRange, 0);
    sprintf(title,"SCR BAD_HIT occ., sec %d, %s", sec, runmes);
    hbook1(140 + sec, title, SC_NPADDLES_SEC, 0.5, locMaxRange, 0);
    sprintf(title,"SCR, BOTH_HIT id vs y, sec %d, %s", sec, runmes);
    hbook2(150 + sec, title, 100, -250, 250, SC_NPADDLES_SEC, .5, locMaxRange, 0);
    sprintf(title,"SCR, BAD_HIT id vs y, sec %d, %s", sec, runmes);
    hbook2(160 + sec, title, 100, -250, 250, SC_NPADDLES_SEC, .5, locMaxRange, 0);
    sprintf(title,"TBID beta, sec %d, %s", sec, runmes);
    hbook1(170 + sec, title, 100, 0.0, 2.0, 0);
    sprintf(title,"TBID mass, sec %d, %s", sec, runmes);
    hbook1(180 + sec, title, 250, 0.0, 2.5, 0);
    sprintf(title,"Actual matches, sec %d, %s", sec, runmes);
    hbook1(200 + sec, title, SC_NPADDLES_SEC, .5, locMaxRange, 0);
    sprintf(title,"Projected matches, sec %d, %s", sec, runmes);
    hbook1(210 + sec, title, SC_NPADDLES_SEC, .5, locMaxRange, 0);
    sprintf(title,"ToF efficiency, id cut=%d,sec %d, %s",sc_cut, sec, runmes);
    hbook1(220 + sec, title, SC_NPADDLES_SEC, .5, locMaxRange, 0);
    sprintf(title,"id vs. rf-time at target,sec %d, %s", sec, runmes);
    hbook2(240 + sec, title, 2000,0,100.0, SC_NPADDLES_SEC,0.5,locMaxRange,0);
    sprintf(title,"SC1 hits vs. projected hits,sec %d, %s", sec, runmes);
    hbook2(250 + sec, title, SC_NPADDLES_SEC, .5, locMaxRange, SC_NPADDLES_SEC,0.5,locMaxRange,0);
    sprintf(title,"Energy deposit, sec %d, %s", sec, runmes);
    hbook2(270 + sec, title, SC_NPADDLES_SEC, .5, locMaxRange, 100, 0, 100, 0);
    sprintf(title,"id vs scr.x-TDPL.x, matched,cut=%d, sec %d, %s",sc_cut,
	    sec, runmes);
    hbook2(280 + sec, title, 100,-150,150, SC_NPADDLES_SEC,0.5,locMaxRange,0);
    sprintf(title,"id vs scr.z-TDPL.z, matched,cut=%d,sec %d, %s",sc_cut, 
	    sec, runmes);
    hbook2(290 + sec, title, 100,-150,150, SC_NPADDLES_SEC,0.5,locMaxRange,0);
    sprintf(title,"id vs scr.y-TDPL.y, matched,cut=%d, sec %d, %s",sc_cut, 
	    sec, runmes);
    hbook2(300 + sec, title, 100,-150,150, SC_NPADDLES_SEC,0.5,locMaxRange,0);

    sprintf(title,"adcr vs. filtered id, sec %d, %s", sec, runmes);
    hbook2(310 + sec, title, SC_NPADDLES_SEC + 2, -0.5, locMaxRange + 1.0, 200, 0, 500, 0);
    sprintf(title,"adcl vs. filtered id, sec %d, %s", sec, runmes);
    hbook2(320 + sec, title, SC_NPADDLES_SEC + 2, -0.5, locMaxRange + 1.0, 100, 0, 1000, 0);
    sprintf(title,"trk adcr vs. filtered id, sec %d, %s", sec, runmes);
    hbook2(330 + sec, title, SC_NPADDLES_SEC + 2, -0.5, locMaxRange + 1.0, 100, 0, 1000, 0);
    sprintf(title,"trk adcl vs. filtered id, sec %d, %s", sec, runmes);
    hbook2(340 + sec, title, SC_NPADDLES_SEC + 2, -0.5, locMaxRange + 1.0, 100, 0, 1000, 0);
    sprintf(title,"SCR status vs. id, sec %d, %s", sec, runmes);
    hbook2(350 + sec, title, SC_NPADDLES_SEC + 2, -0.5, locMaxRange + 1.0, 15, .5, 15.5, 0);

    sprintf(title,"ADC Left occupancy, sec %d, %s", sec, runmes);
    hbook1(3000+sec,title, SC_NPADDLES_SEC, 0.5, locMaxRange, 0);
    sprintf(title,"ADC Right occupancy, sec %d, %s", sec, runmes);
    hbook1(3010+sec,title, SC_NPADDLES_SEC, 0.5, locMaxRange, 0);
    sprintf(title,"TDC Left occupancy, sec %d, %s", sec, runmes);
    hbook1(3020+sec,title, SC_NPADDLES_SEC, 0.5, locMaxRange, 0);
    sprintf(title,"TDC Right occupancy, sec %d, %s", sec, runmes);
    hbook1(3030+sec,title, SC_NPADDLES_SEC, 0.5, locMaxRange, 0);

    sprintf(title,"Left-Right alignment, sector %d", sec); 
    hbook2(5000+sec, title, 100, -300.0, 300.0, SC_NPADDLES_SEC, 0, SC_NPADDLES_SEC,0);
  }

}




/* New since 12/24/1999 */
/* Compares each sector's bin in SC1 to the average value of all sectors (left and right) for this bin*/
/* Output is a text file, name: outfile_scan*/
int ProcessScanning(char* outfile_scan,int runno )
 


{

 int i,j,k;

 float mval[57];

 float h1[6][57];  /* 48 left channels and 6 sectors */
 float h2[6][57];  /* 48 right channels and 6 sectors */

 float adc_l[57];
 float adc_r[57];
 float tdc_l[57];
 float tdc_r[57];
 float sum_adc;
 float sum_tdc;

FILE *f;
f=fopen(outfile_scan,"w");

fprintf(f,"SC_MON CHANNEL SCAN, RUN %d \n\n",runno);

 for(i=0;i<6;i++)   /* histograms -->  array */
  {
    hunpak(11+i,h1[i],"",0);
    hunpak(21+i,h2[i],"",0);
  }
for(i=0;i<SC_NPADDLES_SEC;i++) mval[i] = 0; 
for(i=0;i<SC_NPADDLES_SEC;i++) for(j=0;j<6;j++) 
  mval[i] += (h1[j][i]+h2[j][i]);   /* sum all the sectors left and right for this channel */
 for(i=0;i<SC_NPADDLES_SEC;i++) mval[i] = mval[i] / 12.;  /* 2 channels left and right and 6 sectors = 12 */ 


for(j=0;j<6;j++)
  {
    hunpak(3000+j+1,adc_l,"",0);
    hunpak(3010+j+1,adc_r,"",0);
    hunpak(3020+j+1,tdc_l,"",0);
    hunpak(3030+j+1,tdc_r,"",0);
  
    fprintf(f,"SECTOR: %d\n",j+1);
    sum_adc=0; sum_tdc=0;
    for(k=0;k<SC_NPADDLES_SEC;k++)
      {
	sum_adc+= ( adc_l[k] + adc_r[k] );
	sum_tdc+= ( tdc_l[k] + tdc_r[k] );
      }

    fprintf(f,"\t Average ADC occupancy per bin: %d\n", (int)(sum_adc/((float)(2*SC_NPADDLES_SEC))));
    fprintf(f,"\t Average TDC occupancy per bin: %d\n\n", (int)(sum_tdc/((float)(2*SC_NPADDLES_SEC))));
    
    for(i=0;i<SC_NPADDLES_SEC;i++)
      {
	if( ( h1[j][i] / mval[i] ) < LOWER_SC_SCAN )
	  {
	    fprintf(f,"\t Channel %d\t left  is %d%% too LOW.\t ADC: %d   \tTDC: %d   \t"
		   ,i+1,(int) (100*(1.-( h1[j][i] / mval[i] ))),(int)  adc_l[i],(int) tdc_l[i]);
	  if(i!=0 && i<(SC_NPADDLES_SEC - 1)) fprintf(f,"Channel vs Neighbours = %f \n", 2* h1[j][i] / (h1[j][i-1]+h1[j][i+1]));
	  if(i==0)  fprintf(f,"Channel vs Neighbours = %f \n", h1[j][i] / h1[j][i+1]);
	  if(i==(SC_NPADDLES_SEC - 1))  fprintf(f,"Channel vs Neighbours = %f \n", h1[j][i] / h1[j][i-1]);
	  }
         
	if( ( h2[j][i] / mval[i] ) < LOWER_SC_SCAN )
	  {
 	  fprintf(f,"\t Channel %d\t right is %d%% too LOW.\t ADC: %d   \tTDC: %d   \t"
		 ,i+1,(int) (100*(1.-( h2[j][i] / mval[i] ))),(int)  adc_r[i],(int) tdc_r[i]);
	  if(i!=0 && i<(SC_NPADDLES_SEC - 1)) fprintf(f,"Channel vs Neighbours = %f \n", 2* h2[j][i] / (h2[j][i-1]+h2[j][i+1]));
	  if(i==0)  fprintf(f,"Channel vs Neighbours = %f \n", h2[j][i] / h2[j][i+1]);
	  if(i==(SC_NPADDLES_SEC - 1))  fprintf(f,"Channel vs Neighbours = %f \n", h2[j][i] / h2[j][i-1]);
	  }


	if( ( h1[j][i] / mval[i] ) >  HIGHER_SC_SCAN)
	  {
	  fprintf(f,"\t Channel %d\t left  is %d%% too HIGH.\t ADC: %d   \tTDC: %d   \t"
		 ,i+1,(int) (100*(( h1[j][i] / mval[i] )-1.)),(int)  adc_l[i],(int) tdc_l[i]);
	  if(i!=0 && i<(SC_NPADDLES_SEC - 1)) fprintf(f,"Channel vs Neighbours = %f \n", 2* h1[j][i] / (h1[j][i-1]+h1[j][i+1]));
	  if(i==0)  fprintf(f,"Channel vs Neighbours = %f \n", h1[j][i] / h1[j][i+1]);
	  if(i==(SC_NPADDLES_SEC - 1))  fprintf(f,"Channel vs Neighbours = %f \n", h1[j][i] / h1[j][i-1]);
	  }


	if( ( h2[j][i] / mval[i] ) > HIGHER_SC_SCAN) 
	  {
 	  fprintf(f,"\t Channel %d\t right is %d%% too HIGH.\t ADC: %d   \tTDC: %d   \t"
		 ,i+1,(int) (100*(( h2[j][i] / mval[i] )-1.)),(int)  adc_r[i],(int) tdc_r[i]);
	  if(i!=0 && i<(SC_NPADDLES_SEC - 1)) fprintf(f,"Channel vs Neighbours = %f \n", 2* h2[j][i] / (h2[j][i-1]+h2[j][i+1]));
	  if(i==0)  fprintf(f,"Channel vs Neighbours = %f \n", h2[j][i] / h2[j][i+1]);
	  if(i==(SC_NPADDLES_SEC - 1))  fprintf(f,"Channel vs Neighbours = %f \n", h2[j][i] / h2[j][i-1]);
	  }

   }    
 fprintf(f,"\n");


  }
fclose(f);
}






int ProcessEvent(int bankflag,int sc_cut)
{
  /*----RAW Event Banks------*/
  clasHEAD_t *HEAD = NULL;
  clasSC_t *SC = NULL;
  /*----Reconstruction Banks---*/
  clasHBTR_t *HBTR = NULL;
  clasHDPL_t *HDPL = NULL;
  clasTDPL_t *TDPL = NULL;
  clasTBTR_t *TBTR = NULL;
  clasTAGR_t *TAGR = NULL;
  clasSC1_t *SC1 = NULL;
  clasSCR_t *SCR = NULL;
  clasSCRC_t *SCRC=NULL;
  int i,j, sec;
  clasBID_t *TBID=NULL;
  clasSCG_t *SCG=NULL;
  clasHBID_t *HBID=NULL;

  clasTGEO_t *TGEO=NULL;//ADDED BY ME
  float x;

  if (bankflag){
    /* Redo the pid stuff.  We need to do all of this to get the TBID bank */
    dropAllBanks(&bcs_, BID_BANKS);
    bankList(&bcs_, "E+", BID_BANKS);
    make_BID_banks(1);
 }

  /* Get some useful banks... */

  TBID=getGroup(&bcs_,"TBID",1);
  TBTR=getBank(&bcs_,"TBTR");
  TAGR=getBank(&bcs_, "TAGR");

  TGEO=getBank(&wcs_,"TGEO"); //ADDED BY ME

  for(sec=1;sec<=6;sec++)
    make_TRKS_bank(sec,getBank(&bcs_,"HBID"));
  fill_call_histos();
  for(sec=1; sec <=6; sec++){
    if(SC = getGroup(&bcs_,"SC  ", sec)){
//fprintf(stderr,"SC bank grabbed \n");
      for (i=0; i < SC->bank.nrow; i++){
//fprintf(stderr,"validifying \n");
	if (!valid_SC_hit(SC, sec, i)) {
	  float adcr = SC->sc[i].adcr
	    - sc_pedestals.right[sc_index(sec, (SC->sc[i].id)%0xFF)];
	  float adcl = SC->sc[i].adcl
	    - sc_pedestals.left[sc_index(sec, (SC->sc[i].id)%0xFF)];
//if((((SC->sc[i].id)%0xFF) > 47) && (((SC->sc[i].id)%0xFF) < 58))
//  fprintf(stderr, "\n not valid, id %d, adcr = %f, adcl = %f \n", (SC->sc[i].id)%0xFF, adcl, adcr);
//fprintf(stderr, "id %d, right pedestal = %f, left pedestal = %f \n", (SC->sc[i].id)%0xFF, sc_pedestals.right[sc_index(sec, (SC->sc[i].id)%0xFF)], sc_pedestals.left[sc_index(sec, (SC->sc[i].id)%0xFF)]);
	  hf2(310+sec, (SC->sc[i].id)%0xFF, adcr, 1);
	  hf2(320+sec, (SC->sc[i].id)%0xFF, adcl, 1);
	  if (TDPL = getGroup(&bcs_, "TDPL", sec)){
	    int tk_ind;
	    for (tk_ind = 0; tk_ind < (TDPL->bank.nrow/NoOfPlanesPerTrack);
		 tk_ind++){
	      if (abs(dpl2sc_id(sec,(hdpl_t *) &(TDPL->tdpl[tk_ind*NoOfPlanesPerTrack]) ) - (SC->sc[i].id)%0xFF)){
		hf2(330 + sec, (SC->sc[i].id)%0xFF, adcr,1);
		hf2(340 + sec, (SC->sc[i].id)%0xFF, adcl,1);
	      }
	    }
	  }
	} else {           /* Fill ADC & TDC occupancy histograms */
	                   /* to be used for map hardware status  */
//if((((SC->sc[i].id)%0xFF) > 47) && (((SC->sc[i].id)%0xFF) < 58))
//  fprintf(stderr, "\n valid, id %d, adcr = %d, adcl = %d \n", (SC->sc[i].id)%0xFF, SC->sc[i].adcl, SC->sc[i].adcr);
//good status events are here
	  float adcr = SC->sc[i].adcr
	    - sc_pedestals.right[sc_index(sec, (SC->sc[i].id)%0xFF)];
	  float adcl = SC->sc[i].adcl
	    - sc_pedestals.left[sc_index(sec, (SC->sc[i].id)%0xFF)];
	  float tdcr = SC->sc[i].tdcr;
	  float tdcl = SC->sc[i].tdcl;
//if((((SC->sc[i].id)%0xFF) > 47) && (((SC->sc[i].id)%0xFF) < 58))
//  fprintf(stderr, "calcs for id = %d: pedl = %f, pedr = %f, adcr = %f, adcl = %f, tdcr = %f, tdcl = %f \n", (SC->sc[i].id)%0xFF, (float)sc_pedestals.left[sc_index(sec, (SC->sc[i].id)%0xFF)], (float)sc_pedestals.right[sc_index(sec, (SC->sc[i].id)%0xFF)], adcl, adcr, tdcl, tdcr);

	  if ((adcl>0.)&&(adcl<SC_ADC_MAX))
	    hf1(3000+sec,(SC->sc[i].id)%0xFF,1);
	  if ((adcr>0.)&&(adcr<SC_ADC_MAX))
	    hf1(3010+sec,(SC->sc[i].id)%0xFF,1);
  	  if ((tdcl>0.)&&(tdcl<SC_TDC_MAX)){
	    hf1(3020+sec,(SC->sc[i].id)%0xFF,1);
//if((((SC->sc[i].id)%0xFF) > 47) && (((SC->sc[i].id)%0xFF) < 58))
//fprintf(stderr, "tdc ok, hists filled \n");
}
	  if ((tdcr>0.)&&(tdcr<SC_TDC_MAX))
	    hf1(3030+sec,(SC->sc[i].id)%0xFF,1);
	}

	hf1(0+sec, (SC->sc[i].id)%0xFF, 1);





	hf1(1000, 0, 1);
	if (valid_SC_hit(SC, sec, i) == 0) hf1(1000,1,1);
	if ((SC->sc[i].tdcl<=0&&SC->sc[i].tdcr>0&&SC->sc[i].tdcr<SC_TDC_MAX) ||
	    (SC->sc[i].tdcr<=0&&SC->sc[i].tdcl>0&&SC->sc[i].tdcl<SC_TDC_MAX))
	  hf1(1000,10,1);
      }
    }

    if(SC1 = getGroup(&bcs_,"SC1 ", sec)){
//fprintf(stderr,"SC1 grabbed \n");
      for (i=0; i < SC1->bank.nrow; i++){
        x=(SC1->sc1[i].time_l - SC1->sc1[i].time_r)*8.0;
        if(SC1->sc1[i].energy_l > 0 && SC1->sc1[i].time_l > SC_TIME_UNDERFLOW
	      && SC1->sc1[i].time_l > SC_TIME_UNDERFLOW)
	     hf1(10+sec, SC1->sc1[i].id, 1);
	   if(SC1->sc1[i].energy_r > 0 && SC1->sc1[i].time_r > SC_TIME_UNDERFLOW
	      && SC1->sc1[i].time_r > SC_TIME_UNDERFLOW)
	     hf1(20+sec, SC1->sc1[i].id, 1);
	   if (valid_SC1_hit(SC1->sc1[i]) == BOTH_OK) hf1(1000,20,1);
	     hf1(1000,30,1);

        hf2(5000+sec,x , (float)SC1->sc1[i].id - 0.5, 1.0);


	/* new stuff 10/28/99 */
//fprintf(stderr,"new stuff \n");

          if(SC = getGroup(&bcs_,"SC  ", sec)){
          float adcr = SC->sc[i].adcr
	    - sc_pedestals.right[sc_index(sec, (SC->sc[i].id)%0xFF)];
	  float adcl = SC->sc[i].adcl
	    - sc_pedestals.left[sc_index(sec, (SC->sc[i].id)%0xFF)];


	  if (adcr>50 && adcl>50){
	      if(SC1->sc1[i].energy_l > 0 && SC1->sc1[i].time_l > SC_TIME_UNDERFLOW
		 && SC1->sc1[i].time_l > SC_TIME_UNDERFLOW)
		hf1(20010+sec, SC1->sc1[i].id, 1);
	      if(SC1->sc1[i].energy_r > 0 && SC1->sc1[i].time_r > SC_TIME_UNDERFLOW
		 && SC1->sc1[i].time_r > SC_TIME_UNDERFLOW)
		 hf1(20020+sec, SC1->sc1[i].id, 1);
	    }
	  }

	/*end new stuff */



      }
    }


    if(SCR = getGroup(&bcs_,"SCR ", sec)){
      for (i=0; i < SCR->bank.nrow; i++){
	hf2(350+sec, SCR->scr[i].id, SCR->scr[i].status,1);
	hf2(270+sec, SCR->scr[i].id, SCR->scr[i].energy,1);
	hf1(30+sec, SCR->scr[i].id,1);
	hf2(120+sec, SCR->scr[i].pos.y, SCR->scr[i].id, 1);
	hf1(1000,40,1);
	if (SCR->scr[i].status == BOTH_OK){
	  hf1(130+sec, SCR->scr[i].id, 1);
	  hf2(150+sec, SCR->scr[i].pos.y, SCR->scr[i].id, 1);
	} else{
	  hf1(140+sec, SCR->scr[i].id, 1);
	  hf2(160+sec, SCR->scr[i].pos.y, SCR->scr[i].id, 1);
	}
      }

      if((TDPL=getGroup(&bcs_, "TDPL",sec))&&(SCG=getGroup(&wcs_,"SCG ",sec))){
	vector3_t diff, scr, tdpl;

	for(j=0; j< (TDPL->bank.nrow)/NoOfPlanesPerTrack; j++){
	  for(i=0; i< SCR->bank.nrow; i++){
	    int sc_plane;
	    for(sc_plane = scPlane1; sc_plane <= scPlane4; sc_plane++){
	      scr = SCR->scr[i].pos;
	      tdpl = TDPL->tdpl[sc_plane + j*10].pos;
	      if(tdpl.x<1000.0&&sc_plane==SCG->scg[SCR->scr[i].id-1].panel+3){
		diff = v3sub(scr, tdpl);
		/* all counters */
		hf1(sec+40, v3mag(diff), 1);
		hf1(sec+50, diff.x, 1);
		hf1(sec+60, diff.y, 1);
		hf1(sec+70, diff.z, 1);
		/*foreward counters*/
		if(SCR->scr[i].id <=24){
		  hf1(sec+80, v3mag(diff), 1);
		  hf1(sec+90, diff.x, 1);
		  hf1(sec+100, diff.y, 1);
		  hf1(sec+110, diff.z, 1);
		}
		if(abs(dpl2sc_id(sec,(hdpl_t *)&(TDPL->tdpl[j*10]))-SCR->scr[i].id)<=sc_cut){
		  hf2(300+sec,diff.y,SCR->scr[i].id,1);
		  hf2(280+sec,diff.x,SCR->scr[i].id,1);
		  hf2(290+sec,diff.z,SCR->scr[i].id,1);
		}
	      }
	    }
	    
	  }
	}
      }
    }

  fill_efficiency_histos(sec,sc_cut);
  fill_rf_histos(sec);
  }

  if (TBID && TBTR){
    for(i=0; i < TBID->bank.nrow; i++){
      int trk_ind = TBID->bid[i].track;
      int sec=TBID->bid[i].sec;
      float p,q/*!*/,dedx;
      float tpho;

      /* Plot the beta spectrum for all particles */
      hf1(sec + 170, TBID->bid[i].beta, 1);

      if (trk_ind && (SCRC=getGroup(&bcs_,"SCRC",sec))){
	p = v3mag(TBTR->tbtr[trk_ind-1].p);
	q=TBTR->tbtr[trk_ind-1].q;

	/* Plot the energy deposition in the scintillators for charged 
	   particles */
	dedx=tbid2dedx(&TBID->bid[i]); 
	if (q>0) hf2(1002,p,dedx,1);
	if (q<0) hf2(1003,p,dedx,1);
	/* Plot mass information for charged particles */
	if (TBID->bid[i].beta > 0){
	  float mass,mass_sq, gamma, v_time;
	  float beta=TBID->bid[i].beta;
	  hf2(1007, p, beta, 1);
	  
   	  mass_sq = p*p*(1/beta/beta - 1);
	  if (beta<=1) mass=sqrt(mass_sq);
	  else mass=0.0;

	  hf1(sec + 180, mass, 1);
	  hf1(1001, mass_sq, 1);
	  hf2(400+sec,SCRC->scrc[TBID->bid[i].sc.id-1].id,mass,1);
	  if(p<pcut)   hf2(410+sec,SCRC->scrc[TBID->bid[i].sc.id-1].id,mass,1);
	  tpho=0;
	  if(dedx>4 && dedx<13.5 && p>0.3 && p<0.735){
	  /* Assume this is a pion*/
          /*plot tbidvtime-pion vertex time*/
	    float t_diff;
	    float allt_diff;
	    /*also plot tbid.st_vtime-pion vertex time*/
	    float st_tdiff;
	    tdpl_t *tdpl=tbtr2tdpl(&(TBTR->tbtr[trk_ind-1]));
	    float pion_beta=p/sqrt(p*p+PI_CHARGED_MASS*PI_CHARGED_MASS);
	    float pion_vertex_time=TBID->bid[i].sc.time-pathlen2sc((bid_t *)&TBID->bid[i],(hdpl_t *)tdpl)/SPEED_OF_LIGHT*1e9/pion_beta;
	    
	    allt_diff = pion_vertex_time - TBID->bid[i].vtime;
	    if(TAGR){
	      float tagtpho=getTimePhoton(TAGR,TBID, tid);
	      float tprop=(TBTR->tbtr[trk_ind-1].vert.z+z0)/SPEED_OF_LIGHT*1e9;
	      float vtime;
	      vtime=tagtpho+tprop;
	      t_diff = pion_vertex_time-vtime;
	      tpho=tagtpho;
	      hf1(508, tagtpho, 1);
	      hf1(527, t_diff,1);
	    }
	    st_tdiff=pion_vertex_time - TBID->bid[i].st.vtime;
	    if(SCRC->scrc[TBID->bid[i].sc.id-1].status<100){
	      if(abs(tpho)>0)
		{
	      hf2(520+sec,SCRC->scrc[TBID->bid[i].sc.id-1].id,t_diff,1);
		}
	      hf2(500+sec, SCRC->scrc[TBID->bid[i].sc.id-1].id,allt_diff,1);
	      
	    }
	    
	    hf1(510+sec,st_tdiff,1);
	    hf1(510,st_tdiff,1);
            hf1(507, allt_diff,1);
            
	  }


	}
      }
    }
  }




  return(1);
}

float getTimePhoton(clasTAGR_t *TAGR, clasBID_t *BID, int tid)
{
  /* This routine only works for time-based tracks! */
  static const float c_fSpeedOfLight = 29.9792; /*  // cm / ns */

  float best_diff = ST_TAG_COINCIDENCE_WINDOW;
  float tprop = 0.0;
  tagr_t *tagr = NULL;
  clasTBTR_t *TBTR = (clasTBTR_t *) getBank(&bcs_,"TBTR");
  int i, j;

  /* Exit from function if missing the requisite banks... */
  if(!TAGR || !TBTR || !BID) return(0.f);

  for (i = 0; i< BID->bank.nrow; i++){
    int trk_ind = BID->bid[i].track;
    if(trk_ind){
      tprop = (TBTR->tbtr[trk_ind-1].vert.z+z0) / c_fSpeedOfLight;
      if (BID->bid[i].st.stat){
        for(j = 0; j < TAGR->bank.nrow; j++){
	  float diff_sign = BID->bid[i].st.vtime - (TAGR->tagr[j].tpho + tprop);
          float diff = fabs(diff_sign);
          if (diff < ST_TAG_COINCIDENCE_WINDOW && diff < best_diff
              && (TAGR->tagr[j].stat == 7 || TAGR->tagr[j].stat == 15)){
            best_diff = diff;
            tagr = &(TAGR->tagr[j]);
          }
        }
      }
    }
  }

  if(tagr){
    if(tagr->t_id>tid) return 0.0f;
    return(tagr ? tagr->tpho : 0.0f);
  }
}



void ctrlCHandle(int x)
{
  int icycle,j;

  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  fprintf(stderr,"\n\n\t\t\t***  INTERRUPTED!!!  ***\n\n");
  for (j=1;j<=6;j++) hopera(200+j,"/",210+j,220+j,1.0,1.0);
  hrout(0,icycle," ");
  hldir_(" ", " ", 1L, 1L); /* don't remove this line */
  hrend_("esr", 3L);

  exit(1);
}


int fill_call_histos(){
  float rf1=0,rf2=0,sixtyhz=0;
  clasCALL_t *CALL=NULL;
  int i;

  if(CALL=getGroup(&bcs_,"CALL",0)){
    for(i=0;i<CALL->bank.nrow;i++){
      if(CALL->call[i].id==6) rf1=0.04906*CALL->call[i].tdc;
      if(CALL->call[i].id==7) rf2=0.04906*CALL->call[i].tdc;
      if(CALL->call[i].id==1) sixtyhz=CALL->call[i].adc;
      }
    hf2(10003,rf1,rf2,1.0);
    hf1(10002,sixtyhz,1.0);
  }

}



int fill_efficiency_histos(int sec,int sc_cut){
  clasSCP_t *SCP=NULL;
  clasSCG_t *SCG=NULL;
  clasSC_t *SC=NULL;
  clasSC1_t *SC1=NULL;
  clasHDPL_t *HDPL=NULL;
  clasHEAD_t *HEAD=NULL;
  int i,j,k,predicted_id,match=FALSE;

  if((SC=getGroup(&bcs_,"SC  ", sec))&&(HDPL=getGroup(&bcs_,"HDPL",sec))
     &&(HEAD=getBank(&bcs_,"HEAD"))){
    for (j=0;j<HDPL->bank.nrow/10;j++){
      match=FALSE;
      predicted_id=dpl2sc_id(sec,&(HDPL->hdpl[j*10]));
      hf1(210+sec,predicted_id,1);
      if(SC1=getGroup(&bcs_,"SC1 ",sec)){
	int count=0;
	for (k=0;k<SC1->bank.nrow;k++){
	  hf2(250+sec,predicted_id,SC1->sc1[k].id,1);
	  if(abs(SC1->sc1[k].id - predicted_id)<=sc_cut){
	    hf1(200+sec,SC1->sc1[k].id,1);
	    if(predicted_id!=SC1->sc1[k].id){
	      hf1(210+sec,SC1->sc1[k].id,1);
	    }
	    count++;
	    match=TRUE;
	  }
	}
      }
    }
  }
}


int fill_rf_histos(int sec){
  clasTRKS_t *TRKS=NULL;
  clasCL01_t *CL01=NULL;
  int i,j,k;

  if((CL01=getBank(&bcs_,"CL01"))
	&&(TRKS=getGroup(&bcs_,"TRKS",sec))){   
    for (j=0;j<TRKS->bank.nrow;j++){
      if(TRKS->trks[j].beta>0.9999&&TRKS->trks[j].st_time>0.0)
	hf2(240+sec,CL01->cl01[0].rf1-TRKS->trks[j].st_time,TRKS->trks[j].sc_id,1);
    }	
  }	
}




void get_hardware_status(runno)
{
  float Right_ADC_bin_content=0.,Right_TDC_bin_content=0.;
  float Left_ADC_bin_content=0.,Left_TDC_bin_content=0.;

  int sec,bin,index,i;
  int right_status[342],left_status[342];

  FILE *fp;
  char left_status_file[40],right_status_file[40],run_number[10];
  
/* Get status values from histograms */
  
  for(sec=1;sec<=6;sec++){
    
    for (bin=1;bin<=SC_NPADDLES_SEC;bin++) {

      Left_ADC_bin_content = hx(3000+sec,bin);        
      Right_ADC_bin_content = hx(3010+sec,bin);
      Left_TDC_bin_content = hx(3020+sec,bin);        
      Right_TDC_bin_content = hx(3030+sec,bin);
      
      index = (sec-1)*SC_NPADDLES_SEC + (bin-1);

      if((Left_ADC_bin_content==0.)&&(Left_TDC_bin_content!=0.))
	left_status[index]=1;
      else if((Left_ADC_bin_content!=0.)&&(Left_TDC_bin_content==0.))
	left_status[index]=2;
      else if((Left_ADC_bin_content==0.)&&(Left_TDC_bin_content==0.))
	left_status[index]=3;
      else
	left_status[index]=0;

      if((Right_ADC_bin_content==0.)&&(Right_TDC_bin_content!=0.))
	right_status[index]=1;
      else if((Right_ADC_bin_content!=0.)&&(Right_TDC_bin_content==0.))
	right_status[index]=2;
      else if((Right_ADC_bin_content==0.)&&(Right_TDC_bin_content==0.))
	right_status[index]=3;
      else
	right_status[index]=0;

    }

  }



/* Write status values to text files */

  sprintf(run_number,"%d",runno);

  strcpy(left_status_file, run_number);
  strcat(left_status_file, ".SC_hardware_status");
  strcat(left_status_file, ".LEFT");

  fp = fopen(left_status_file, "w");

  for (i=0; i<=(SC_NPADDLES_SEC*6 - 1); i++) {
    fprintf(fp, "%d\n",left_status[i]);
  }

  fclose(fp);

  strcpy(right_status_file, run_number);
  strcat(right_status_file, ".SC_hardware_status");
  strcat(right_status_file, ".RIGHT");

  fp = fopen(right_status_file, "w");

  for (i=0; i<=(SC_NPADDLES_SEC*6 - 1); i++) {
    fprintf(fp, "%d\n",right_status[i]);
  }

  fclose(fp);


}














/* end file */





