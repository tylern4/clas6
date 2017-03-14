/* -=======================================================-
_begin_doc
RCS ID string
$Id: make_pid.c,v 1.25 2004/03/04 23:19:41 pasyuk Exp $
$Author: pasyuk $
$Revision: 1.25 $
$Date: 2004/03/04 23:19:41 $
_end_doc
* -=======================================================- */

/* Added InitTarget to take care of target position issue.
   Target position is overriden anyway with -z option */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ntypes.h>
#include <bostypes.h>
#include <math.h>
#include <bosddl.h>
#include <kinematics.h>
#include <ec.h>
#include <sc.h>
#include <utility.h>
#include <pdgutil.h>
#include <pid.h>
#include <PartUtil.h>
#include <vertex.h>

/*----------------------------------------------------------------------------------------------*/
int ConfigEvent(int RunNo, int pid_flag)
{
  static int CurrentRun = -1;
  char *dir;
  int iret = 0;
  int RunNoForCal;   /* the run number to use for calibrations */
  clasHEAD_t * HEAD = getBank(&bcs_, "HEAD");
  clasRUNC_t * RUNC = NULL;

  if (RunNo != CurrentRun) {
    InitTarget(RunNo);  
    fprintf(stderr, "Making RUNC bank for run number %i.\n", RunNo);
    if(RUNC=make_RUNC_bank(RunNo)){
      pRUNC(stderr, &RUNC->runc);
      CurrentRun = RunNo;
      iret = 1;
    }
     
    if (pid_flag) {
      RunNoForCal = HEAD->head[0].type < 0 ? 1 : RunNo;           /* = 1 for monte carlo data */
      fprintf(stderr, "Initializing PID for run: %d\n", RunNoForCal);
      iret=iret && initPID(RunNoForCal);
    }
  }
  return iret;
}

/*Process time based is now in libtrk.a - JM*/

/*----------------------------------------------------------------------------------------------*/
int initPID(int runno){ 

  clasRUNC_t * RUNC = getBank(&wcs_, "RUNC"); 
  InitTarget(runno); /* Target initialization. */
    vertex_brun(runno); /* beap position intialization */
  /* initialize as much as you can */
  initCL01(runno);
  ec_set_def_();
  ec_brun_(&runno);
  cc_init(runno);
  sc_begin_run(runno);
  ec1_set_def_(); /* large angle calorimeter stuff */
  ec1_brun_(&runno); 

  if (RUNC){
    if (RUNC->runc.beam.type.val.i[0] > 0){
      /*start counter */
      st_set_def_();
      st_init_();
      st_brun_(&runno);

      /*tagger stuff*/
      tagtcl_set_def_();
      tag_init_();
      tag_brun_(&runno);
    }
  }

}

clasBID_t *make_BID_banks(int number){
  clasBID_t *TBID = NULL;
  clasRUNC_t *RUNC = getBank(&wcs_, "RUNC");
  clasHDPL_t *HDPL = getBank(&bcs_, "HDPL");
  int sec;

  int trk_level=TIME_BASED;

  dropAllBanks(&bcs_,"CL01");
  make_CL01_bank();

  dropAllBanks(&bcs_, "SC1 SCR SCRCCC01");
  for(sec=1; sec <= 6; sec++) {
    make_SC1_bank(sec);
    make_SCR_bank(sec,"TDPL");
    make_SCRC_bank(sec);
    make_CC01_bank(sec);       
  }
  dropAllBanks(&bcs_,"ECHBECPI");
  ec_evnt_();
  /* large angle calorimeter stuff */
  dropAllBanks(&bcs_,"EC1R");
  ec1_evnt_(); 

  if (RUNC){
    if (RUNC->runc.beam.type.val.i[0] > 0){
      dropAllBanks(&bcs_, "TAGITAGR");
      tag_evnt_();
      dropAllBanks(&bcs_, "ST1 STR ");    
      st_bevt_();
      st_evnt_(&trk_level);
    }
  }

  dropBank(&bcs_, "TBID", number); 
  return(make_TBID_group(number)); 

}


int make_HDPL_from_TDPL(){
  clasTDPL_t *TDPL = NULL;
  clasTDPL_t *HDPL = NULL; /* not a typo tdpl and hdpl are the same bank*/
  clasTBTR_t *TBTR = NULL;
  int sec, i;
  int trk_pln;

  for(sec=1; sec <= 6; sec++){
    if (TDPL = getGroup(&bcs_, "TDPL", sec)){
      if( HDPL = makeBank(&bcs_, "HDPL", sec, sizeof(hdpl_t)/4, TDPL->bank.nrow)){
	if( TBTR = (clasTBTR_t *) getBank(&bcs_,"TBTR")) {
	  for (i = 0; i < TDPL->bank.nrow; i++){ 
	    HDPL->tdpl[i] = TDPL->tdpl[i];
	    trk_pln = TDPL->tdpl[i].trk_pln;
	    HDPL->tdpl[i].trk_pln=((trk_pln)%100) + 100*(TBTR->tbtr[trk_pln/100].itr_hbt);
	  }
	}
	else return(0);
      }
    } 
  }
return(1);
}


