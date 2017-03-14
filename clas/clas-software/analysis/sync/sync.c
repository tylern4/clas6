/*
 *  Program: Search for sync event problems, a precooking skim program
 *
 *  Author : Steve Barrow 
 *
 *  Date :   Summer, 1998. 
 *
*/

/*   Module information: */
#define USE(var) static void * use_##var = (void *) &var
static char *crcsid = "$Id: sync.c,v 1.19 2007/08/22 02:06:39 holtrop Exp $";
USE(crcsid);

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
#include <utility.h>
#include <particleType.h>
#include <pid.h>
#include <gsync.h>

#define ALLONES 0xFFFF /* all 32 bits set to 1 for overflow treatment */ 
#define MAXEVENTS 100000 /* number of events for reference calculation */
#define MAXSCALERS 10 /* number of scaler intervals for reference */
#define TOLERANCE 0.1 /* allowed variation range +/- TOLERANCE */

/* declare the bos common */
BOSbank bcs_;
#ifdef Darwin
BOSbank wcs_;
#endif

/* some global variables */

int Nevents = 0;
int sync_last = 0;
int sync_current = 0;
int sync_bad =0;
int sync_total =0;
static int end_of_data = 0;
int Clock_Scale_Factor =100;
int printTimes = 0;

uint32 sync_last_time = 0;
uint32 sync_current_time = 0;

/* ----------- Function prototypes ------------------ */

void PrintUsage(char *processName);
int ProcessEvent(FILE * file_out);
void LookForTrips(FILE * fp_trip);
int ScalerEvent();
void ctrlCHandle(int);
int configure_run(float *beam_energy, float *Q_live);
void update_sync_info(FILE * file_out, int event_num );
void finish_log(FILE * file_out );
void getReference();
/* -------------------------------------------------- */

void update_sync_info(FILE * file_out, int event_num)
{
extern int sync_last, sync_current;
extern uint32 sync_last_time, sync_current_time;
if (sync_last == 0) sync_last=4;
if(!printTimes)
   fprintf(file_out," %d  %d \n",event_num-(sync_current-sync_last),event_num);
else
  fprintf(file_out," %d  %d %10u %10u \n",event_num-(sync_current-sync_last),
	  event_num,sync_last_time,sync_current_time);

 fflush(file_out);
}
/* --------------------------------------------------- */
void finish_log(FILE * file_out)
{
extern int sync_bad, sync_total;
fprintf(stderr,"There were %d bad syncs out of  %d \n",sync_bad,sync_total);
}
/* ---------------------------------------------------- */
void PrintUsage(char *processName)
{
  fprintf(stderr,"Usage: %s [options] file1 [file2] etc....\n\n",processName);
  fprintf(stderr,"  Options:\n");
  fprintf(stderr,"\t-n[#]\tProcess only # number of events\n\n");
  fprintf(stderr,"\t-t \tCheck for beam trips/bursts also\n\n");
  fprintf(stderr,"\t-s# \tClock scale fator for the Run_gated Clock\n");
  fprintf(stderr,"\t\tDefault (if -s# is not specified) is set to -s100\n"); 
  fprintf(stderr,"\t\tThat setting works for runs prior g8a\n");
  fprintf(stderr,"\t\tFor runs starting from g8a and untill at least e1-6\n");
  fprintf(stderr,"\t\tit needs to be set to -s10\n\n");
  fprintf(stderr,"\t-c\tAdd interrupt times to the sync file\n\n");
  fprintf(stderr,"\t-h\tPrint this message.\n\n");
  exit(0);
}
/* ------------------------------------------------------ */
int main(int argc,char **argv)
{
  int i;
  char *argptr;
  char *outfile = NULL;
  int max = 0;
  char mess[500];
  char file_out[100];
  char file_trip[100];
  int check_trips = 0;
  static int first_file = 0;

  enum file_in {FIRST, NOT_FIRST};
  enum file_in file_check = FIRST;

  FILE *fp_part = NULL;
  FILE *fp_trip = NULL;

  Nevents = 0;
  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
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
      case 't':
	check_trips = 1;
	break;
      case 's' :
	Clock_Scale_Factor = atoi(++argptr);
	break;
      case 'c':
	printTimes = 1;
	break;
      default:
	fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
	PrintUsage(argv[0]);
	break;
      }
    }
  }
  
  initbos();

  for (i = 1;i < argc; ++i) {
    argptr = argv[i];
    if (*argptr != '-') {
      if (file_check == FIRST) {
	/* open file for bad sync events */
	fprintf(stderr,"\n Running sync on file: %s\n",argptr);
	strcpy(file_out,argptr);
	strcat(file_out,".sync");
	file_check = NOT_FIRST;
	fp_part = fopen(file_out, "a");
	if(fp_part)
	  fprintf(stderr,"\n Bad sync output file = %s \n",file_out); 
	else{
	  fprintf(stderr,"\n Unable to open Bad sync output file = %s \n",file_out);
	  exit(1);
	}
	if( check_trips){
	  /* open trip file if requested */
	  strcpy(file_trip,argptr);
	  strcat(file_trip,".trip");
	  fp_trip = fopen(file_trip, "a");
	  if(fp_trip)
	    fprintf(stderr,"\n Trip output file = %s \n",file_trip); 
	  else{
	    fprintf(stderr,"\n Unable to open Trip output file = %s \n",file_trip);
	    exit(1);
	  }
	}
      }
      
      sprintf(mess,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", argptr);
      
      if (!fparm_c(mess)) {
	fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",\
		argv[0],argptr,strerror(errno));
	exit(1);
      }
      else {
	/* start first loop through the data */
	/* to calculate some reference values for trip detedction */
	if(!first_file && check_trips){
	  while ( ((scaler_event_counter >= MAXSCALERS) ? (Nevents <MAXEVENTS) :1) \
		  && getBOS(&bcs_,1,"E")){
	    Nevents++;
	    getReference();
	    dropAllBanks(&bcs_,"E");
	    cleanBanks(&bcs_);
	  }
	
	  /* printout mean reference values */	
	  fprintf(stderr, " Based on %d events and %d scaler intervals\n",Nevents,scaler_event_counter);
	  fprintf(stderr, "\n\n Mean interval %f \n Live time %f \n\n",diff_average,average_live_time);
	  first_file = 1;
	  Nevents = 0 ; /* reset event counter */
	  scaler_event_counter = 0;
	  /* rewind input file */
	  
	  
	  sprintf(mess,"REWIND BOSINPUT");
	  if (!fparm_c(mess)) {
	    fprintf(stderr,"%s: Unable to rewind BOSINPUT: %s %s \n\n",\
		    argv[0],argptr,strerror(errno));
	    exit(1);
	  }
	}     

	
	
	/* second event loop */
	while ((max ? Nevents < max : 1) && getBOS(&bcs_,1,"E")){
	  clasHEAD_t *HEAD = getBank(&bcs_, "HEAD"); 
	  
	  Nevents++;
	  
	  if(check_trips) LookForTrips(fp_trip); /* check trips */
	  
	  ProcessEvent(fp_part); /* check sync */
	  
	  dropAllBanks(&bcs_,"E");
	  cleanBanks(&bcs_);
	  
	}
	
	/*close BOS file*/
	sprintf(mess,"CLOSE BOSINPUT", argptr);
	fparm_c(mess);
	
      }    
    }
  }
  
  /* final report */
  end_of_data = 1;
  
  fprintf(stderr,"\n#  of events processed: %d\n",Nevents);
  finish_log(fp_part);
  fclose(fp_part);
  
  if(check_trips) {
    LookForTrips(fp_trip);
    fclose(fp_trip);
  }
  exit(0);
}

/* this function calculates reference values */
void getReference(){

  static int physics_events = 0;

  clasHEAD_t *HEAD = NULL;
  clasTRGS_t *TRGS = NULL;
  clasTGBI_t *TGBI = NULL;
  clasS1ST_t *S1ST = NULL;
  /* get event */
  
  HEAD = getBank(&bcs_, "HEAD");
  if(HEAD){
    
    if(ScalerEvent()){ /* appears to be scaler event */
      
      TRGS = getBank(&bcs_, "TRGS"); /* get scaler bank */ 
      current_scaler_timer = TRGS->trgs[0].clock_g1; 
      current_live_timer = TRGS->trgs[0].clock_g2;
      
      if(scaler_event_counter){
	
	if(current_scaler_timer < previous_scaler_timer) 
	  scaler_interval = ALLONES - previous_scaler_timer + current_scaler_timer;
	else 
	  scaler_interval = current_scaler_timer - previous_scaler_timer;
	
	if(current_live_timer < previous_live_timer) 
	  live_interval = ALLONES - previous_live_timer + current_live_timer;
	else 
	  live_interval = current_live_timer - previous_live_timer;
	
	scaler_interval = scaler_interval*Clock_Scale_Factor; /* make it micro seconds */
	live_interval = live_interval*Clock_Scale_Factor;
	
	if(physics_events){ /* there was some beam in this interval */
	  diff_scaler_based = (float)scaler_interval/(float)physics_events;
	  live_time = (float)live_interval/(float)scaler_interval;
	  
	  if(scaler_event_counter == 1){
	    diff_average = diff_scaler_based;
	    average_live_time = live_time;
	  }
	  else{
	    diff_average = 0.5*diff_average + 0.5*diff_scaler_based;
	    average_live_time = 0.5*average_live_time + 0.5*live_time;
	    diff_scaler_based_average= diff_average;
	    
	  }
	}
      }
	  
      /* reset at the end of scaler event analysis */
      physics_events = 0;
      previous_scaler_timer = current_scaler_timer;
      previous_live_timer = current_live_timer;
      scaler_event_counter++;
    }
    else physics_events++; /* appears to be physics event */
    
  }
  return;  
}


void LookForTrips(FILE * fp_trip){

  /* this function detects beam trips and big fluctuations */


  float tolerance;
  
  clasHEAD_t *HEAD = NULL;
  clasTRGS_t *TRGS = NULL;
  clasTGBI_t *TGBI = NULL;
  clasS1ST_t *S1ST = NULL;

  /*----------------------------------------------------------*/

  /* executed at the end of file */
  if(end_of_data){
    if(last_type_read == 0){ /* incomplete scaler interval */

      trip = -2;

      if(last_event_timer < first_timer)
	scaler_interval = (ALLONES - first_timer + last_event_timer);
      else
	scaler_interval = (last_event_timer - first_timer);

      last_in_scaler_interval = last_event_number;
      diff_scaler_based = (float)scaler_interval /(float)physics_event_counter;
      diff_summ = scaler_interval/(float)physics_event_counter;
      fprintf(fp_trip, \
	      "%5d %5d %5d %10d %10d %10d %10d %10.0f %10.0f\t%10u %10u\t%f\n", \
	      scaler_event_counter, \
	      scaler_current, \
	      trip, \
	      first_in_scaler_interval, \
	      last_in_scaler_interval, \
	      physics_event_counter, \
	      scaler_interval, \
	      diff_summ, \
	      diff_scaler_based, \
	      last_event_timer, \
	      current_scaler_timer, \
	      live_time);

      /* print final trip report */
      fprintf(stderr,"%d trips detected out of %d scaler intervals\n", total_trips, scaler_event_counter);

      return;
    }
  }    

  /* get event */

  HEAD = getBank(&bcs_, "HEAD");
  if(HEAD){

    if(ScalerEvent()){ /* appears to be scaler event */
      last_type_read = 1;
      scaler_current = HEAD->head[0].nevent;
      TRGS = getBank(&bcs_, "TRGS"); /* get scaler bank */ 

      current_scaler_timer = TRGS->trgs[0].clock_g1; 
      current_live_timer = TRGS->trgs[0].clock_g2;

      last_in_scaler_interval = last_event_number;
      ungated_triggers_current = TRGS->trgs[0].trig_or_ug;
      ungated_triggers = ungated_triggers_current - ungated_triggers_previous;

/*        accepted_triggers_current = S1ST->s1st[0].event_count; */
/*        accepted_triggers = accepted_triggers_current - accepted_triggers_previous; */
/*        accepted_triggers_previous = accepted_triggers_current; */


      ungated_triggers_previous = ungated_triggers_current;

      if(!scaler_event_counter){
	/* first scaler interval */
	trip = -1;

	/* calculate live time */
	
	/*  	if(physics_event_counter){ */
	
	if(last_event_timer < first_timer)
	  scaler_interval = (ALLONES - first_timer + last_event_timer);
	else
	  scaler_interval = (last_event_timer - first_timer);
	diff_summ = scaler_interval/(float)physics_event_counter;
	diff_scaler_based = diff_average;
/*  	diff_average = 0.5*diff_scaler_based+0.5*diff_average; */
	diff_scaler_based_average = diff_average;
	live_time = average_live_time;
	
	fprintf(fp_trip, \
		"%5d %5d %5d %10d %10d %10d %10d %10.0f %10.0f\t%10u %10u\t%f\n", \
		scaler_event_counter, \
		scaler_current, \
		trip, \
		first_in_scaler_interval, \
		last_in_scaler_interval, \
		physics_event_counter, \
		scaler_interval, \
		diff_summ, \
		diff_scaler_based, \
		last_event_timer, \
		current_scaler_timer, \
		live_time);
	fflush(fp_trip);

 	previous_scaler_timer = current_scaler_timer; 
	previous_live_timer = current_live_timer;
	scaler_event_counter++;
	physics_event_counter = 0;
	diff_summ =0.;
      }

      else {
	/* scaler interval other than the first one */

      /* calculate live time */

	if(current_live_timer < previous_live_timer) 
	  live_interval = ALLONES - previous_live_timer + current_live_timer;
	else 
	  live_interval = current_live_timer - previous_live_timer;

	if(current_scaler_timer < previous_scaler_timer) 
	  scaler_interval = ALLONES - previous_scaler_timer + current_scaler_timer;
	else 
	  scaler_interval = current_scaler_timer - previous_scaler_timer;

	scaler_interval = scaler_interval*Clock_Scale_Factor; /* make it micro seconds */
	live_interval = live_interval*Clock_Scale_Factor;

	live_time = (float)live_interval/(float)scaler_interval;
	LT_diff = fabs(live_time-average_live_time);

	if(physics_event_counter){
	  diff_summ = diff_summ/(float)physics_event_counter;
	  diff_scaler_based = (float)scaler_interval /(float)physics_event_counter;
	  
	  if((fabs(diff_scaler_based-diff_summ) > TOLERANCE*diff_average) \
	     || (fabs(diff_average-diff_summ) > TOLERANCE*diff_average)
	     || (LT_diff > TOLERANCE) \
	     || (live_time == 1.)) {
	    trip = 1;

	    total_trips++;
	  }
	  else { 
	    trip = 0;
	    average_live_time = 0.5*average_live_time + 0.5*live_time;
	    diff_scaler_based_average = 0.5*diff_scaler_based_average + 0.5*diff_scaler_based;    
	    diff_average = 0.2*diff_scaler_based_average + 0.6*diff_average + 0.2*diff_summ;
	  }
	}
	else {
	  trip = 1;
	  total_trips++;
	}
	
	fprintf(fp_trip, \
		"%5d %5d %5d %10d %10d %10d %10d %10.0f %10.0f\t%10u %10u\t%f\n", \
		scaler_event_counter, \
		scaler_current, \
		trip, \
		first_in_scaler_interval, \
		last_in_scaler_interval, \
		physics_event_counter, \
		scaler_interval, \
		diff_summ, \
		diff_scaler_based, \
		last_event_timer, \
		current_scaler_timer, \
		live_time);
	fflush(fp_trip);
	
	previous_scaler_timer = current_scaler_timer; 
	previous_live_timer = current_live_timer;
	scaler_event_counter++;
	physics_event_counter = 0;
	diff_summ = 0.;
	
      }
    }	/* end scaler event */
    
    /*-----------------------------------------*/
    /* begin physics event */
    else {
      if(HEAD->head[0].type >0 && HEAD->head[0].type <10){
	
	TGBI = getBank(&bcs_, "TGBI");
	
	if(TGBI){
	  if (physics_event_counter == 0) {
	    first_in_scaler_interval = HEAD->head[0].nevent;
	    first_timer = TGBI->tgbi[0].interrupt_time;
	  }
	  last_type_read = 0;
	  physics_event_counter++;
	  
	  if(!last_event_number){
	    /*first phisics event in file */
	    last_event_number = HEAD->head[0].nevent;
	    /* now get timer from TGBI bank */
	    last_event_timer = TGBI->tgbi[0].interrupt_time;
	  }
	  
	  else {
	    /* not first event */
	    last_event_number = HEAD->head[0].nevent;
	    current_timer = TGBI->tgbi[0].interrupt_time;
	    if(current_timer < last_event_timer)
	      diff_current = (float)(ALLONES - last_event_timer + current_timer);
	    else 
	      diff_current = (float)(current_timer - last_event_timer);
	    diff_summ = diff_summ + diff_current;

	    last_event_timer = current_timer;
	    
	  } 
	} 
      }
    }
    return;
  }
}
 
int ScalerEvent()
{
  clasTRGS_t *TRGS = NULL;
  clasS1ST_t *S1ST = NULL;
  clasHEAD_t *HEAD = NULL;
  HEAD = getBank(&bcs_, "HEAD");
  TRGS = getBank(&bcs_, "TRGS");

  if (HEAD){
    if( HEAD->head[0].type == 10 || TRGS)
      return (1);
  }
  return(0);
}

int ProcessEvent(FILE* fp_part){
  int j, bad =0, got_one=0;
  clasHEAD_t *HEAD = getBank(&bcs_, "HEAD");
  clasTGBI_t *TGBI = NULL;
  /* clasSYNC_t *SYNC = NULL; */

  if (HEAD){
    if (HEAD->head[0].type==2) {
      got_one = 100; 
      /* printf("HEAD->roc = %d \n",HEAD->head[0].nevent); */
      sync_last = sync_current;
      sync_last_time = sync_current_time;

      sync_current = Nevents;
      
      if(TGBI = getBank(&bcs_, "TGBI"))
	sync_current_time = TGBI->tgbi[0].interrupt_time;

	if (HEAD->head[0].roc!=0) {
      bad = 100;
      /* printf("HEAD->roc = %d evnum = %d \n",HEAD->head[0].roc,Nevents); */
	}
    }
  }

    if (bad ==100) {
      sync_bad++;
      update_sync_info(fp_part,HEAD->head[0].nevent);
    }
    if(got_one == 100) sync_total++;
    return(1);
}		
void ctrlCHandle(int x){
  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  fprintf(stderr,"\n\n\t\t\t*** !!! HASTA LA VISTA !!!  ***\n\n");
  exit(1);
}

