#include <a1.h>
#include <bitflags.h>
#include <makebanks.h>
#include <utility.h>
#include <ec.h>
#include <sc.h>
#include <pid.h>
#include <call.h>

/* -=======================================================- *
$Id: analysis.c,v 1.98 2004/05/07 03:24:19 pasyuk Exp $
$Author: pasyuk $
$Revision: 1.98 $
$Date: 2004/05/07 03:24:19 $
* -=======================================================- */


int initRun(int runNo, int ProcessFlag ){
  int i, sec;
  clasRUNC_t *RUNC = NULL;

  run_control_set_def_(); /* need this for lac code*/

  if (RUNC= make_RUNC_bank(runNo)){
    pRUNC(stderr, &(RUNC->runc));
  }
  /* Getting target position from Map */
  InitTarget(runNo);

  /* initialize cc package*/
  if (PROCESS_CC & ProcessFlag) {
    cc_brun_(&runNo);
    cc_begin_run(runNo);  /*CC01 initialization*/
  }

  /*initialize tof package*/
  if ( PROCESS_SC & ProcessFlag ) {
    sc_begin_run(runNo);
  }

  /* call bank initialization */
  if (PROCESS_CL01 & ProcessFlag) {
    CL01_begin_run(runNo);
  }

  /*tracking initialization*/
  if ((PROCESS_HBTR & ProcessFlag) || (PROCESS_TBTR & ProcessFlag)) {
    fprintf(stderr, "Initialize tracking package:\n");
    /* dc_set_def_() and trk_set_def_() moved to begnning of program 
       because they can
       be overridden by command line arguments*/
    dc_begin_run(runNo);
  }

  /*tagger initialization */
  if (PROCESS_TAGGER & ProcessFlag){
    tagtcl_set_def_();
    tag_init_();
    tag_brun_(&runNo);
  }

  /* TAGM bank */
  if (PROCESS_TAGM & ProcessFlag) {
    tagM_init();
    tagM_brun(runNo);
  }

  /*st initialization*/
  if (PROCESS_ST & ProcessFlag) {
    st_set_def_();
    st_init_();
    st_begin_run(runNo);
  }

  /*ec initialization*/
  if (PROCESS_EC & ProcessFlag) {
    ec_set_def_();
    ec_begin_run(runNo);
  }

  /*lac initialization*/
  if (PROCESS_LAC & ProcessFlag) {
    ec1_set_def_();
    ec1_brun_(&runNo);
  }

  /*seb initialization*/
  if (PROCESS_SEB & ProcessFlag) {
    /*    evnt_set_def_() moved to the beginning of the program
	  because they can
	  be overridden by command line arguments */
    init_builder_();
    builder_brun_(&runNo);
  }
  if (PROCESS_GPID & ProcessFlag) {
    initGPID(runNo);

  }

  if (PROCESS_REGION1 & ProcessFlag){
    trk_reg1_set_def_();
    trk_reg1_init_();
  }

  if (PROCESS_DCDW & ProcessFlag){	 
    /*L.Todor, CMU,2001; create dcdw bank from DC_STATUS.map*/
    fprintf(stderr, "Remove dead wires %i\n",runNo);
    make_dcdw_banks_(runNo); 				    
  }	
}

/*need this for seb */
int fill_clasmdl(){
  clasHEAD_t *HEAD=getBank(&bcs_, "HEAD");

  if (HEAD) {
    clasmdl_.nccrun  = HEAD->head[0].nrun;
    clasmdl_.version = HEAD->head[0].version;
    clasmdl_.nevent  = HEAD->head[0].nevent;
    clasmdl_.evtype  = HEAD->head[0].type;
    clasmdl_.trec++;
    clasmdl_.evtime  = HEAD->head[0].time;
    clasmdl_.evtwght = HEAD->head[0].trigbits;
    clasmdl_.evtclas = HEAD->head[0].evtclass;
  }

}

/* override recutil definition - WE really NEED this */
int rernev_(int *runno, int *recordno, int *nevent){
  *runno = (int)clasmdl_.nccrun;
  *recordno = (int)clasmdl_.nevent;
  *nevent = (int)clasmdl_.trec;
}


int ProcessEvent(int ProcessFlag,int OptionsFlag, int PIDFlag)
{
  int i,sec;
  static int n = 0;
  int l_tbt;
  int flag;
  int btype;

  clasHEAD_t *HEAD = getBank(&bcs_, "HEAD");
  extern int nneut;
  clasBID_t *HBID=NULL;
  clasBID_t *TBID=NULL;
  clasTBTR_t *TBTR=NULL;
  clasRUNC_t *RUNC=NULL;
  /*We drop banks before we process just to be safe*/
  if (PROCESS_CL01 & ProcessFlag){
    dropAllBanks(&bcs_,"CL01");
    make_CL01_bank();
  }
   
  if(PROCESS_TAGGER & ProcessFlag){
    clasTAGE_t *TAGE = getBank(&bcs_, "TAGE");
    clasTAGE_t *TAGT = getBank(&bcs_, "TAGT");
    if (TAGE && TAGT){ /*temporary fix for dec 97 production*/
      dropAllBanks(&bcs_, TAGGER_BANKS);
      tag_evnt_();
    }
  }

  if (PROCESS_TAGM & ProcessFlag) {
      tagM_evt();
  }
  

  /*might want to give this its own switch?*/
  if ((PROCESS_HBTR & ProcessFlag) || (PROCESS_TBTR & ProcessFlag)){
    dropAllBanks(&bcs_, "DC1 ");
    make_dc1_digi_();
  }

  /* do hit based tracking */
  if (PROCESS_HBTR & ProcessFlag) {
    dropAllBanks(&bcs_, "HBTRHBERHBLAHBTBHDPLDHCL");
    ProcessHitBased(); 
    /*trk_evnt_hbt_();*/
  }


  /* First pass start counter analysis with hit-based tracking */
  if (PROCESS_ST & ProcessFlag) {
    int trk_level=HIT_BASED;
    dropAllBanks(&bcs_, ST_BANKS);
    st_bevt_();
    st_evnt_(&trk_level);
  }  
  
  if (PROCESS_EC & ProcessFlag) {
    dropAllBanks(&bcs_,EC_BANKS);
    /*for(sec=1; sec <= 6; sec++) {
      make_EC01_bank(sec);
    } - not used yet */
    ec_evnt_();
  }

  if (PROCESS_LAC & ProcessFlag) {
    dropAllBanks(&bcs_,"EC1R");
    ec1_evnt_();
  }
  
  if (PROCESS_CC & ProcessFlag) {
    dropAllBanks(&bcs_,CC_BANKS);
    cc_bevt_();
    cc_evnt_(); /* make the CCRC bank */
    for(sec=1; sec <= 6; sec++) {
      make_CC01_bank(sec);
    }
  }

  /* first pass TOF analysis*/
  if (PROCESS_SC & ProcessFlag) {
    dropAllBanks(&bcs_, SC_BANKS);
    for(sec=1; sec <= 6; sec++) {
      make_SC1_bank(sec);
      make_SCR_bank(sec, "HDPL");       
      make_SCRC_bank(sec);
    }
  }

  /* seb level 1 analysis*/
  if((PROCESS_SEB & ProcessFlag)){
      int level =1;
/*      clasHBTR_t *HBTR = getBank(&bcs_, "HBTR"); */
/*      if (HBTR){ */
/*        if (HBTR->bank.nrow <=10){ */
	dropAllBanks(&bcs_, SEB_BANKS);
	run_builder_(&level, &flag);
/*        } */
/*      } */
  }
    
  /* make the hit based particle id bank*/
  if (PROCESS_HBID & ProcessFlag) {
    dropAllBanks(&bcs_,"HBID");
    make_HBID_bank();
  }

  if (PROCESS_TRKS_HBID & ProcessFlag){
    HBID = getBank(&bcs_,"HBID");
    if(HBID){
      if(HBID->bank.nrow){
	dropAllBanks(&bcs_, "TRKS");
	for (sec=1; sec <= 6; sec++){
	  clasTRKS_t *TRKS = make_TRKS_bank(sec, getGroup(&bcs_, "HBID", 0));
	}
      }
    }
  }

  /* secret a1 option */
  if (TRKS_OPTION & OptionsFlag) {
    dropAllBanks(&bcs_,"TRKS");
    makeTRKSmc();
  }

  /* do time based tracking*/
  if (PROCESS_TBTR & ProcessFlag) {
    dropAllBanks(&bcs_,"TBTRTBLATBERTDPL");
    /*trk_evnt_tbt_(&l_tbt);*/
    ProcessTimeBased();
  }

  /*second pass TOF analysis*/
  if (PROCESS_SC & ProcessFlag) {
    dropAllBanks(&bcs_, "SCR SCRC");
    for(sec=1; sec <= 6; sec++) {
      make_SCR_bank(sec, "TDPL");       
      make_SCRC_bank(sec);
    }
    /* fix pointers in HBID bank- D.P. Weygand */
       reMatchSCRC();
  }

  /* Second pass start counter analysis with time-based tracking */
  if ((PROCESS_ST & ProcessFlag) && (PROCESS_TBTR & ProcessFlag)) {
    int trk_level=TIME_BASED;
    st_bevt_();
    st_evnt_(&trk_level);
  }  


  /* make the time based particle id bank*/
  if (PROCESS_TBID & ProcessFlag) {
    dropBank(&bcs_,"TBID", PIDFlag);
    dropBank(&bcs_,"PART", PIDFlag);
    make_TBID_group(PIDFlag);

    make_vert();/*make MVRT bank. liji*/
    make_mvrt();/*liji*/

    make_PART_group(PIDFlag);
  }
  /* do GPID */
  if (PROCESS_GPID & ProcessFlag) {
    dropBank(&bcs_,"GPID", PIDFlag);
    makeGPID(PIDFlag,0);
  }

  if (PROCESS_TBID_NOST & ProcessFlag) {
    if(RUNC=getBank(&wcs_, "RUNC")) {
      btype = RUNC->runc.beam.type.val.i[0]; /* save beam type */
      dropBank(&bcs_,"TBID", 2);
      dropBank(&bcs_,"PART", 2);
      make_TBID_group(2);
      make_PART_group(2);
      RUNC->runc.beam.type.val.i[0] = btype; /* restore beam type */
    }
  }

  if (PROCESS_SEB & ProcessFlag){
    int level =2; 
/*      clasHBTR_t *HBTR = getBank(&bcs_, "HBTR"); */
/*      if (HBTR){ */
/*        if (HBTR->bank.nrow <=10){ */
        dropAllBanks(&bcs_,"HEVTEVNTDCPBSCPBCCPBUNUSEVHB");/* seb banks - TRKS */
	/*	dropAllBanks(&bcs_, SEB_BANKS); */
	run_builder_(&level, &flag);
/*        } */
/*      } */
  }
   

  if (PROCESS_TRKS_TBID & ProcessFlag){
    TBID = getBank(&bcs_,"TBTR");
    if(TBTR){
      if(TBTR->bank.nrow){
	dropAllBanks(&bcs_, "TRKS");
	for (sec=1; sec <= 6; sec++){
	  clasTRKS_t *TRKS = make_TRKS_bank_plus(sec, getGroup(&bcs_, "TBID", PIDFlag));
	}
      }
    }
    dropAllBanks(&bcs_,"TBTRTBLATBERTDPL");
    ProcessTimeBased();    
  }

  if (PROCESS_REGION1 & ProcessFlag){
    clasDHCL_t *DHCL = NULL;
    clasRGLK_t *RGLK = NULL;
    dropAllBanks(&bcs_, REGION1_BANKS);
    trk_region1_();
  }

  return(1);
} 







