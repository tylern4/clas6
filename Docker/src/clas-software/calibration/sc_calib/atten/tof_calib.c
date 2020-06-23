/* adc_calib.c */
/* Author: S. Taylor
   From SC1 bank, create 2 dimensional histograms in 
       ln( Energy_l*M0_l/(Energy_r*M0_r)) vs. x=v*(t_l-t_r)/2
 *  Adding Gamecock mode: Jörn Langheinrich
       */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ntypes.h>
#include <bostypes.h>
#include <map_manager.h>
#include <clas_cern.h> 
#include <sc.h>
#include <scExtern.h>

#define ADC_PEDESTALS 6
#define T0 0
#define T1 1
#define T2 2
#define LEFT 0
#define RIGHT 1
#define PIPELINE 2
#define STAND_DEV 2
#define MEAN 1
#define MEMH 50000000

float pawc_[MEMH];

typedef struct {
  int count;
  int check;
  int atten;
  int rawhist;
  int swap;
} USERFLAGS;

/*prototypes*/
int book_histogram(char *outfile,int sec, USERFLAGS* f, int runno);
int ProcessEvent(int sec, USERFLAGS* f);
void PrintUsage(char *processName);
void copy_pipeline_tdc_();

#ifdef VINTAGE_BUILD
void rootopenw(const char* filename) {}
void rootend() {}
void rootbook1(int hid, const char* title, int xbins, double xmin, double xmax) {}
void rootf1(int hid, double x, double value) {}
void count_initialize(int nfiles, char* filelist[]) {}
double get_percent(int ifile, int eventNo) {return 0.;}
int  coupled_paddle_index(int sector, int stripe, int itbtr) {return 0;}
double z_relative(int sector, int stripe, int itbtr) {return -9999.;}
#else
extern  void rootopenw(const char* filename);
extern  void rootend();
extern  void rootbook1(int hid, const char* title, int xbins, double xmin, double xmax);
extern  void rootf1(int hid, double x, double value);
extern  void count_initialize(int nfiles, char* filelist[]);
extern  double get_percent(int ifile, int eventNo);
extern  int  coupled_paddle_index(int sector, int stripe, int itbtr);
extern  double z_relative(int sector, int stripe, int itbtr);
#endif

/*----------------------------------------------------------------------*/
void PrintUsage(char *processName)
{
  fprintf(stderr,"Usage: %s [-n] [-o] file1 [file2] etc....\n\n",processName);
  fprintf(stderr,"  Options:\n");
  fprintf(stderr,"\t-n[#]\t\tProcess only # number of events\n\n");
  fprintf(stderr,"\t-o[filename]\tHBOOK output file\n\n");
  fprintf(stderr,"\t-s[#]\t\tSector number (1-6) [default=all]\n\n");
  fprintf(stderr,"\t-c\t\tCheckout mode for left-right offsets\n\n");
  fprintf(stderr,"\t-A\t\tAttenuation length histos\n\n");
  fprintf(stderr,"\t-R\t\tRaw data histos\n\n");
  fprintf(stderr,"\t-T\t\tTDC Swap histos\n\n");    
  fprintf(stderr,"\t-P\t\tDon't copy pipeline tdcs\n\n");
  fprintf(stderr,"\t-G\t\tGamecock mode\n\n");    
  fprintf(stderr,"\t-h\t\tPrint this message [no histos !].\n\n");
  exit(0);
}

int gamecockMode = 0;

void myf2(int id, float x, float y, float value) {
  if (gamecockMode)  rootf2 (id, x, y, value);
  else               hf2    (id, x, y, value);
}

int main(int argc,char *argv[]){
  sc_const_t dummy1,dummy2;
  char *argptr;
  char mess[1000];
  char *outfile=NULL;
  char *number;
  FILE *inputfileptr;
  FILE *outputfile;
  clasHEAD_t *HEAD=NULL;
  int Nevents=0,i,j,k,errno,runno=0,eventno=0,max=1.0e6;
  int sec = 0, isec;
  char* dataFile[256];
  int nfiles = 0;
  int memh = MEMH;
  int locUsePipelineFlag = 1;

  USERFLAGS flag;
  memset (&flag, 0, sizeof (flag));

  /* Parse command-line arguments */
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
      case 'n':
	max = atoi(++argptr);
	break;
      case 's':
	sec=atoi(++argptr);
	if(sec>6) exit(1);
	break;
      case 'c':
	flag.check = 1;
	flag.count ++;
	fprintf(stderr,"--- Check-out mode for left-right offsets --- \n");
	break;
      case 'G':
	gamecockMode = 1;
	fprintf(stderr,"--- Gamecock Mode ---\n");
	break;
      case 'R':
	flag.rawhist = 1;
	flag.count ++;
	fprintf(stderr,"--- Raw data histos ---\n");
	break;
      case 'A':
	flag.atten = 1;
	flag.count ++;
	fprintf(stderr,"--- Attenuation Length histos ---\n");
	break;
      case 'T':
        flag.swap = 1;
	flag.count ++;
	fprintf(stderr,"--- TDC Swap histos ---\n");
	break;
      case 'P':
     locUsePipelineFlag = 0;
	fprintf(stderr,"--- Don't Copy Pipeline TDCs ---\n");
	break;
      default:
	fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
	PrintUsage(argv[0]);
	break;
      }
    }
    else {   /* it is data file */
      if (nfiles < 256) {
	dataFile[nfiles++]= argptr;
      }
      else {
	fprintf(stderr, "can't process more than 255 data files\n"); 
	PrintUsage(argv[0]);
      }
    }
  }

  if (! flag.count) {
    flag.check   =1;
    flag.rawhist =1;
    flag.atten   =1;
    flag.swap    =1;
    fprintf(stderr,"--- All histos ---\n");
  }


  if (gamecockMode) {
    count_initialize(nfiles, dataFile);
  }
  else {
  /* Initialize the paw system */
    hlimit_(&memh);
  }

  /* Initialize BOS */
  initbos();
  
	  
  /* Loop over the datafiles specified in command-line args. */
  for (i = 0; i < nfiles; i++) {
    sprintf(mess,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", dataFile[i]);
    if (!fparm_c(mess)) {
      fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",
	      argv[0],dataFile[i],strerror(errno));
    }
    else {
      while ((max ? Nevents < max : 1) && getBOS(&bcs_,1,"E")) {
	Nevents++;
	  /* Get run number from Header bank */
	HEAD=(clasHEAD_t *)getBank(&bcs_,"HEAD");
	if (HEAD){
	  if (!runno) {
	    runno=HEAD->head->nrun;
	    /* Initialize the SC package */
	    if (runno)
           initialize_tof(runno);
	    if (!i)   book_histogram(outfile,sec, &flag, runno);
	  }
	  eventno = HEAD->head->nevent;
	}
     if((runno >= 55357) && (locUsePipelineFlag == 1))
       copy_pipeline_tdc_();

	ProcessEvent(sec, &flag);
	
	if (!sec) { 
	  for (isec=1; isec<=6; isec++) {
	    dropBank(&bcs_, "SC  ", isec);
	    dropBank(&bcs_, "SCT ", isec);
	    dropBank(&bcs_, "SC1 ", isec);
	  }
	}
	else {
	  dropBank(&bcs_, "SC  ", sec);
	  dropBank(&bcs_, "SCT ", sec);
	  dropBank(&bcs_, "SC1 ", sec);
	}
        dropAllBanks(&bcs_, "E");
	cleanBanks(&bcs_);
	
	if (Nevents%100==0){
	  if (gamecockMode) {
	    fprintf(stdout, "Processed: %7.3f \%\n",
		    get_percent(i, eventno));
	    fflush(stdout);
	  }
	  else {
	    fprintf(stderr,"%d\r",Nevents);
	    fflush(stderr);
	  }
	}	
      }
    }
    fparm_c("CLOSE BOSINPUT");
  }

  if (gamecockMode) {
    rootend();
  }
  else {
    hrout(0,2," ");
    hldir_(" ", " ", 1L, 1L); /* don't remove this line?? 
				 nonsens, just go ahead and remove it! */
    hrend("junk");
  }
}

/* Initialize the paw system and book histograms */

int book_histogram(char *histoutfile, int sector, USERFLAGS* flag, int runno){
  int id,j,k,sec;
  char defaultname[255];
  char title[128];
  int   sec_first = sector ? sector : 1;
  int   sec_last  = sector ? sector : 6;
		 
  char* swap_title[6] = {"left lc TDC vs pipeln TDC", 
			 "right lc TDC vs pipeln TDC",
			 "left  ADC vs left TDC",
			 "right ADC vs right TDC",
			 "left ADC  vs right ADC",
			 "left TDC  vs right TDC" };

  sprintf (defaultname, "atten_%d.%s", runno, (gamecockMode?"root":"hbook"));

  if (histoutfile==NULL) /* not defined by command line option? */
    histoutfile=defaultname;

  /* Initialize ROOT system */
  if (gamecockMode) {
    
    /* Open the ROOT file. */
    rootopenw(histoutfile);  

    for(sec=sec_first; sec<=sec_last; sec++) {
      
      if(flag->rawhist){
	sprintf(title,"geometric mean vs ID, sector %d", sec);
	rootbook2(10*sec, title,100,200.,3000.,SC_NPADDLES_SEC,0.5,(float)SC_NPADDLES_SEC + 0.5);   
	sprintf(title,"ID vs ADC LEFT, sector %d", sec);
	rootbook2(10*sec+1, title,100, 0.0,1000.0,SC_NPADDLES_SEC,0.5,(float)SC_NPADDLES_SEC + 0.5);
	sprintf(title,"ID vs ADC RIGHT, sector %d", sec);
	rootbook2(10*sec+2, title,100,0.0,1000.0,SC_NPADDLES_SEC,0.5,(float)SC_NPADDLES_SEC + 0.5);
	sprintf(title,"ID vs TDC LEFT, sector %d", sec);
	rootbook2(10*sec+3, title,100,100.0,4000.0,SC_NPADDLES_SEC,0.5,(float)SC_NPADDLES_SEC + 0.5);
	sprintf(title,"ID vs TDC RIGHT, sector %d", sec);
	rootbook2(10*sec+4, title,100,100.0,4000.0,SC_NPADDLES_SEC,0.5,(float)SC_NPADDLES_SEC + 0.5);
	sprintf(title,"ID vs TDC LEFT - TDC RIGHT, sector %d", sec);
	rootbook2(10*sec+5, title,100,-1000.0,1000.0,SC_NPADDLES_SEC,0.5,(float)SC_NPADDLES_SEC + 0.5);
	sprintf(title,"ID vs pipeline TDC LEFT, sector %d", sec);
	rootbook2(10*sec+7, title,100, 0.0,8000.0,SC_NPADDLES_SEC,0.5,(float)SC_NPADDLES_SEC + 0.5);
	sprintf(title,"ID vs pipeline TDC RIGHT, sector %d", sec);
	rootbook2(10*sec+8, title,100, 0.0,8000.0,SC_NPADDLES_SEC,0.5,(float)SC_NPADDLES_SEC + 0.5);
	sprintf(title,"ID vs pipeline TDC LEFT - TDC RIGHT, sector %d", sec);
	rootbook2(10*sec+9, title,100, -2000.,2000.,SC_NPADDLES_SEC,0.5,(float)SC_NPADDLES_SEC + 0.5);
      }
      if(flag->check){
	sprintf(title,"Left-Right alignment, sector %d", sec); 
	rootbook2(10*sec+6, title, 100, -300., 300., SC_NPADDLES_SEC, 0.5, (float)SC_NPADDLES_SEC + 0.5); 
      }
      if(flag->atten){
	for (id=1;id<=SC_NPADDLES_SEC;id++){
	  sprintf(title,"ln (AL/AR) vs. x, sec %d id %d", sec, id);
	  rootbook2(100*sec+id, title, 100, -300.0, 300.0,100,-8.0,8.0);
	}
      }
    }
    
    if (flag->swap){
      for (j = 0; j<6; j++) {
	rootbook2 (994+j, swap_title[j],  
		   SC_NPADDLES, -0.5, SC_NPADDLES-0.5, 
		   SC_NPADDLES, -0.5, SC_NPADDLES-0.5 );
      }
    }
  }

  /* Initialize PAW system */
  else {
    printf("Booking histograms...\n");
    
    for(sec=sec_first; sec<=sec_last; sec++){
      if(flag->rawhist){
	sprintf(title,"geometric mean vs ID, sector %d", sec);
	hbook2(10*sec, title,100,200.0,3000,SC_NPADDLES_SEC,0.5,(float)SC_NPADDLES_SEC + 0.5,0.);   
	sprintf(title,"ID vs ADC LEFT, sector %d", sec);
	hbook2(10*sec+1, title,100, 0.0,1000.0,SC_NPADDLES_SEC,0.5,(float)SC_NPADDLES_SEC + 0.5,0.);
	sprintf(title,"ID vs ADC RIGHT, sector %d", sec);
	hbook2(10*sec+2, title,100,0.0,1000.0,SC_NPADDLES_SEC,0.5,(float)SC_NPADDLES_SEC + 0.5,0.);
	sprintf(title,"ID vs TDC LEFT, sector %d", sec);
	hbook2(10*sec+3, title,100,100.0,4000.0,SC_NPADDLES_SEC,0.5,(float)SC_NPADDLES_SEC + 0.5,0.);
	sprintf(title,"ID vs TDC RIGHT, sector %d", sec);
	hbook2(10*sec+4, title,100,100.0,4000.0,SC_NPADDLES_SEC,0.5,(float)SC_NPADDLES_SEC + 0.5,0.);
	sprintf(title,"ID vs TDC LEFT - TDC RIGHT, sector %d", sec);
	hbook2(10*sec+5, title,100,-1000.0,1000.0,SC_NPADDLES_SEC,0.5,(float)SC_NPADDLES_SEC + 0.5,0.);
	sprintf(title,"ID vs pipeline TDC LEFT, sector %d", sec);
	hbook2(10*sec+7, title,100, 0.0,8000.0,SC_NPADDLES_SEC,0.5,(float)SC_NPADDLES_SEC + 0.5,0.);
	sprintf(title,"ID vs pipeline TDC RIGHT, sector %d", sec);
	hbook2(10*sec+8, title,100, 0.0,8000.0,SC_NPADDLES_SEC,0.5,(float)SC_NPADDLES_SEC + 0.5,0.);
	sprintf(title,"ID vs pipeline TDC LEFT - TDC RIGHT, sector %d", sec);
	hbook2(10*sec+9, title,100, -2000.,2000.,SC_NPADDLES_SEC,0.5,(float)SC_NPADDLES_SEC + 0.5,0.);
      }
      if(flag->check){
	sprintf(title,"Left-Right alignment, sector %d", sec); 
	hbook2(10*sec+6, title, 100, -300.0, 300.0, SC_NPADDLES_SEC, 0.5, (float)SC_NPADDLES_SEC + 0.5,0.); 
      }
      if(flag->atten){
	for (id=1;id<=SC_NPADDLES_SEC;id++){
	  sprintf(title,"ln (AL/AR) vs. x, sec %d id %d", sec, id);
	  hbook2(100*sec+id, title, 100, -300.0, 300.0,100,-8.0,8.0,0.);
	}
      }
    }
    
    if (flag->swap){
      for (j = 0; j<6; j++) {
	hbook2 (994+j, swap_title[j],  
		SC_NPADDLES, -0.5, SC_NPADDLES-0.5, 
		SC_NPADDLES, -0.5, SC_NPADDLES-0.5, 0.);
      }
    }
    /* Open the HBOOK file. */
    hropen(2,"junk",histoutfile,"N",1024,0);  
  }
}

//===========================================================================================
  int tdc_in_range (int tdcvalue) {
     if (tdcvalue <= 50) return 0;
     if (tdcvalue >= 4000) return 0;
     return 1;
  }

/* Make the SC1 bank and fill the histograms using the data in SC1 */

int ProcessEvent(int sector, USERFLAGS* flag){

  clasSC1_t *SC1 = getBank(&bcs_, "SC1 ");
  clasSC_t  *SC  = NULL;
  clasSCT_t *SCT = NULL;
  int i,j,k,index, indexswl, indexswr, indexswap;
  int exist_SC1 = 0, sec= 0;
  float x,ratio,geom_mean,t_diff,chan_id;
  int   tdc_fired   [342] [2];
  float adc_value   [342] [2];
  int   tdcl_lecroy [342];
  int   tdcr_lecroy [342];
  int   tdc_pipeline[342] [2];
  int   tdc_count   [342] [2];
  int   sec_first = sector ? sector : 1;
  int   sec_last  = sector ? sector : 6;
  int   bits = 0;
  int   leftright = 0;

  extern sc_const_t sc_m0;
  extern sc_const_t sc_pedestals;
  exist_SC1 = (SC1 != NULL);

  memset (tdc_fired, 0, sizeof(tdc_fired));
  memset (adc_value, 0, sizeof(adc_value));
  memset (tdcl_lecroy, 0, sizeof(tdcl_lecroy));
  memset (tdcr_lecroy, 0, sizeof(tdcr_lecroy));
  memset (tdc_pipeline, 0, sizeof(tdc_pipeline));
  memset (tdc_count, 0, sizeof(tdc_count));

  for(sec=sec_first; sec<=sec_last; sec++){
    if (SCT = getGroup(&bcs_,"SCT ", sec)) {
      for (j = 0; j < SCT->bank.nrow; j++){
	leftright = SCT->sct[j].id / 256;
	index     = sc_index(sec, (SCT->sct[j].id & 0xFF));
	indexswap = leftright ? sc_swapindex.right [index] : sc_swapindex.left [index] ;
	tdc_pipeline [indexswap] [leftright] = SCT->sct[j].tdc;
      }
    }
    if (SC = getGroup(&bcs_,"SC  ", sec)) {
      for (j = 0; j < SC->bank.nrow; j++){
	index    = sc_index(sec, SC->sc[j].id & 0xFF);
	indexswl = sc_swapindex.left  [index];
	indexswr = sc_swapindex.right [index];
	if (tdc_in_range (tdcl_lecroy [ indexswl ] = SC->sc[j].tdcl)) {
	  tdc_fired [indexswl] [LEFT] = SC->sc[j].tdcl;
	  tdc_count [indexswl] [LEFT] ++;
	}
	if (tdc_in_range (tdcr_lecroy [ indexswr ] = SC->sc[j].tdcr)) {
	  tdc_fired [indexswr] [RIGHT] = SC->sc[j].tdcr;
	  tdc_count [indexswr] [RIGHT] ++;
	}
	if (SC->sc[j].adcl)
	  adc_value [index] [LEFT] = SC->sc[j].adcl - sc_pedestals.left[index];
	if (SC->sc[j].adcr)
	  adc_value [index] [RIGHT] = SC->sc[j].adcr - sc_pedestals.right[index];
      }

/*      
      if (tdc_fired [280] [LEFT]) {
	if (adc_value [280] [LEFT]) bits |= 1;
	if (adc_value [264] [LEFT]) bits |= 2;
	switch (bits) {
	case 0: break; 
	case 1: break;
	case 2: 
	  tdc_fired [264] [LEFT] = tdc_fired [280] [LEFT];
	  tdcl_lecroy [264] = tdcl_lecroy [280];
	  tdc_fired [280] [LEFT] = 0;
	  tdcl_lecroy [280] = 0;
	  break;
	
	case 3: break; 
	default: printf ("program error \n"); break;
	}  
      } 
*/	

      for (j = 0; j < SC_NPADDLES; j++){
	for (i = 0; i < 1; i++) {
	  if (tdc_count [j] [i] > 1) printf ("\n %d count %3d.%d\n", tdc_count [j] [i], j, i); 
	}
      }

      for (j = 0; j < SC->bank.nrow; j++){
	index = sc_index(sec,SC->sc[j].id & 0xFF); 
  	chan_id = SC->sc[j].id & 0xFF;

	if (flag->rawhist) {
	  if(SC->sc[j].adcl > 50 && SC->sc[j].adcr > 50) {   
	    geom_mean = sqrt( adc_value[index][LEFT] * adc_value[index][RIGHT]);
	    myf2(10*sec, geom_mean, chan_id, 1.);
	  }
	  if(adc_value[index][LEFT] > 0) {  
	    myf2(10*sec+1, adc_value[index][LEFT], chan_id, 1.);
	  }
  
	  if(adc_value[index][RIGHT] > 0) { 
	    myf2(10*sec+2, adc_value[index][RIGHT], chan_id, 1.);
	  }
	    
	  if(tdc_in_range(tdcl_lecroy[index])){ 
	    myf2(10*sec+3,(float)tdcl_lecroy[index], chan_id, 1.);
	  }

	  if(tdc_in_range(tdcr_lecroy[index])){ 
	    myf2(10*sec+4,(float)tdcr_lecroy[index], chan_id, 1.); 
	  }
	  if(tdc_in_range(tdcl_lecroy[index]) && tdc_in_range(tdcr_lecroy[index])) {
	    myf2(10*sec+5, (float)(tdcl_lecroy[index] - tdcr_lecroy[index]), chan_id, 1.);    
	  }
	}    /* fill rawhist         */ 
      }     /* loop over SC-Banks   */  
    }      /* if (SC=GetGroup) ... */

    if (!exist_SC1)
      make_SC1_bank(sec);
  
    if(SC1 = getGroup(&bcs_,"SC1 ", sec)){
      for (i=0; i < SC1->bank.nrow; i++){
	if( SC1->sc1[i].energy_l>0.1 && SC1->sc1[i].energy_r>0.1 &&
	    SC1->sc1[i].time_l>5.0   && SC1->sc1[i].time_l<180.0 &&
	    SC1->sc1[i].time_r>5.0   && SC1->sc1[i].time_r<180.0) {          	
	  ratio=log(sc_m0.left[sc_index(sec,SC1->sc1[i].id)]*SC1->sc1[i].energy_l/
		    (sc_m0.right[sc_index(sec,SC1->sc1[i].id)]*SC1->sc1[i].energy_r));
	  x=(SC1->sc1[i].time_l - SC1->sc1[i].time_r)*8.0;	        
	  if(flag->atten) myf2(100*sec+SC1->sc1[i].id,x,ratio,1.0);
	}
      	
	if( SC1->sc1[i].time_l>5.0   && SC1->sc1[i].time_l<180.0 &&
	    SC1->sc1[i].time_r>5.0   && SC1->sc1[i].time_r<180.0) {          	
	  x=(SC1->sc1[i].time_l - SC1->sc1[i].time_r)*8.0;	        
	  if(flag->check) myf2(10*sec+6, x, (float)SC1->sc1[i].id, 1.0);
	}      
      }
    }
  }       /* loop over sector     */
  if (flag->rawhist) {
    for (i=0; i<SC_NPADDLES; i++) {
      sec      = i / SC_NPADDLES_SEC + 1;
      chan_id  = i % SC_NPADDLES_SEC + 1;
      if (tdc_pipeline[i][LEFT])
	myf2(10*sec+7,(float)tdc_pipeline[i][LEFT], chan_id, 1.); 
      if (tdc_pipeline[i][RIGHT])
	myf2(10*sec+8,(float)tdc_pipeline[i][RIGHT], chan_id, 1.); 
      if (tdc_pipeline[i][LEFT]&&tdc_pipeline[i][RIGHT])
	myf2(10*sec+9,(float)tdc_pipeline[i][LEFT]-tdc_pipeline[i][RIGHT], chan_id, 1.); 
    }
  }

  if (flag->swap) {
    if (SC) {
      for (j=0; j<SC_NPADDLES; j++)
	if (tdc_fired [j] [LEFT])
	  for (i=0; i<SC_NPADDLES; i++) {
	    if (tdc_fired [i] [RIGHT]) {
/*	    if (i==264 && j== 280) {
	      for (k = 0; k < SC->bank.nrow; k++) {
		printf ("%2d  A %4d %4d    T %4d %4d \n", SC->sc[k].id&0xFF, 
			SC->sc[k].adcl, SC->sc[k].adcr, SC->sc[k].tdcl, SC->sc[k].tdcr);
	      }
	      printf ("-------------------------------------------\n");
	    }
*/
	      myf2 (999, i, j, 1.);
	    }
	    if (adc_value [i] [LEFT] > 50.)
	      myf2 (996, j, i, 1.);
	  }
      for (j=0; j<SC_NPADDLES; j++)
	if (adc_value [j] [RIGHT] > 50.)
	  for (i=0; i<SC_NPADDLES; i++) {
	    if (tdc_fired [i] [RIGHT])
	      myf2 (997, i, j, 1.);
	    if (adc_value [i] [LEFT] > 50.)
	      myf2 (998, j, i, 1.);
	  }
    }
    if (SCT && SC) {
      for (j=0; j<SC_NPADDLES; j++)
	if (tdc_fired [j] [LEFT])
	  for (i=0; i<SC_NPADDLES; i++) 
	    if (tdc_pipeline [i] [LEFT])
	      myf2 (994, i, j, 1.);
      for (j=0; j<SC_NPADDLES; j++)
	if (tdc_fired [j] [RIGHT])
	  for (i=0; i<SC_NPADDLES; i++) 
	    if (tdc_pipeline [i] [RIGHT])
	      myf2 (995, i, j, 1.);	   
	
    }
  }

  return(1);
}
