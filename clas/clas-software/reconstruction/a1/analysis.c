#include <a1.h>
#include <bitflags.h>
#include <makebanks.h>
#include <utility.h>
#include <ec.h>
#include <sc.h>
#include <pid.h>
#include <call.h>

/* -=======================================================- *
$Id: analysis.c,v 1.100 2008/06/09 18:15:52 goetz Exp $
$Author: goetz $
$Revision: 1.100 $
$Date: 2008/06/09 18:15:52 $
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
    sh_brun(runNo);
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

    /// We drop banks before we process just to be safe
    if (PROCESS_CL01 & ProcessFlag){
        dropAllBanks(&bcs_,"CL01");

        /**
         * input: CALL RFT LSRT HEAD
         * creates: CL01
         *
         * takes RF signal (CALL or RFT) and creates "good" RF times
         * CL01.rf1 and CL01.rf2
         **/
        make_CL01_bank();
    }

    if(PROCESS_TAGGER & ProcessFlag){
        dropAllBanks(&bcs_, TAGGER_BANKS);

        /**
         * input: TAGE TAGT
         * creates: TAGR TAGI
         *
         * takes raw tagger hits and creates TAGR bank which is a list
         * of the photons in the tagger.
         **/
        tag_evnt_();
    }

    if (PROCESS_TAGM & ProcessFlag) {
        /**
         * input: TAGE
         * creates: TAGM
         *
         * takes raw tagger hits and creates TAGM tagger result bank
         * from multi-hit pipeline tdc signals.
         **/
        tagM_evt();
    }


    if ((PROCESS_HBTR & ProcessFlag) || (PROCESS_TBTR & ProcessFlag)){
        dropAllBanks(&bcs_, "DC1 ");

        /**
         * input: DC0, DCDW, DDLY
         * output: DC1
         *
         * filters the dc0 bank to make dc hits more amenable
         * to tracking
         **/
        make_dc1_digi_();
    }

    /// hit based tracking
    if (PROCESS_HBTR & ProcessFlag) {
        dropAllBanks(&bcs_, "HBTRHBERHBLAHBTBHDPLDHCL");

        /**
         * input: DC1
         * output: HBTR HBER TRKS HDPL TRKL DTRK
         *
         * creates hit-based tracks based on DC1 bank (or DC0)
         * via pattern matching from prlink file
         **/
        ProcessHitBased();

        /**
         * input:
         * output:
         *
         *
         **/
        //trk_evnt_hbt_();
    }


    /// First pass start counter analysis with hit-based tracking
    if (PROCESS_ST & ProcessFlag) {
        int trk_level=HIT_BASED;
        dropAllBanks(&bcs_, ST_BANKS);
        /**
         * input:
         * output:
         *
         *
         **/
        st_bevt_();

        /**
         * input:
         * output:
         *
         *
         **/
        st_evnt_(&trk_level);
    }

    if (PROCESS_EC & ProcessFlag) {
        dropAllBanks(&bcs_,EC_BANKS);
        //for(sec=1; sec <= 6; sec++) {
        //    make_EC01_bank(sec);
        //}
        /**
         * output: ECHB,ECPI,ECPO
         **/
        ec_evnt_();
    }

    if (PROCESS_LAC & ProcessFlag) {
        dropAllBanks(&bcs_,"EC1R");
        /**
         * input:
         * output:
         *
         *
         **/
        ec1_evnt_();
    }

    if (PROCESS_CC & ProcessFlag) {
        dropAllBanks(&bcs_,CC_BANKS);
        /**
         * input:
         * output:
         *
         *
         **/
        cc_bevt_();

        /**
         * input:
         * output: CCRC
         *
         *
         **/
        cc_evnt_();

        for(sec=1; sec <= 6; sec++) {
            /**
             * input:
             * output:
             *
             *
             **/
            make_CC01_bank(sec);
        }
    }

    /// first pass TOF analysis
    if (PROCESS_SC & ProcessFlag) {
        dropAllBanks(&bcs_, SC_BANKS);
        for(sec=1; sec <= 6; sec++) {
            /**
             * input:
             * output:
             *
             *
             **/
            make_SC1_bank(sec);

            /**
             * input:
             * output:
             *
             *
             **/
            make_SCR_bank(sec, "HDPL");

            /**
             * input:
             * output:
             *
             *
             **/
            make_SCRC_bank(sec);
        }
    }

    /// seb level 1 analysis
    /// and hodoscope reconstruction
    if((PROCESS_SEB & ProcessFlag)){
        int level =1;
        dropAllBanks(&bcs_, SEB_BANKS);
        /**
         * input:
         * output:
         *
         *
         **/
        run_builder_(&level, &flag);
        sh_evnt();

        /* clasSHPB_t* SHPB; */
        /* if (SHPB = (clasSHPB_t*) getBank(&bcs_, "SHPB")) */
        /* { */
        /*     fprintf(stdout,"found hodoscope hits:\n"); */
        /*     int ii; */
        /*     for (ii=0; ii<SHPB->bank.nrow; ii++) */
        /*     { */
        /*         fprintf(stdout,"    %i,%i,%i,%i,%i,%i,%i\n", SHPB->shpb[ii].id,SHPB->shpb[ii].x,SHPB->shpb[ii].y,SHPB->shpb[ii].z,SHPB->shpb[ii].nphe,SHPB->shpb[ii].time,SHPB->shpb[ii].status); */
        /*     } */
        /* } */
    }

    /// make the hit based particle id bank
    if (PROCESS_HBID & ProcessFlag) {
        dropAllBanks(&bcs_,"HBID");
        /**
         * input:
         * output:
         *
         *
         **/
        make_HBID_bank();
    }

    if (PROCESS_TRKS_HBID & ProcessFlag){
        HBID = getBank(&bcs_,"HBID");
        if(HBID){
            if(HBID->bank.nrow){
                dropAllBanks(&bcs_, "TRKS");
                for (sec=1; sec <= 6; sec++){
                    /**
                     * input:
                     * output:
                     *
                     *
                     **/
                    clasTRKS_t *TRKS = make_TRKS_bank(sec, getGroup(&bcs_, "HBID", 0));
                }
            }
        }
    }

    /// secret a1 option
    if (TRKS_OPTION & OptionsFlag) {
        dropAllBanks(&bcs_,"TRKS");
        /**
         * input:
         * output:
         *
         *
         **/
        makeTRKSmc();
    }

    /// do time based tracking
    if (PROCESS_TBTR & ProcessFlag) {
        dropAllBanks(&bcs_,"TBTRTBLATBERTDPL");
        /**
         * input:
         * output:
         *
         *
         **/
        //trk_evnt_tbt_(&l_tbt);

        /**
         * input:
         * output:
         *
         * fits the tracks using drift time
         **/
        ProcessTimeBased();
    }

    /// second pass TOF analysis
    if (PROCESS_SC & ProcessFlag) {
        dropAllBanks(&bcs_, "SCR SCRC");
        for(sec=1; sec <= 6; sec++) {
            /**
             * input:
             * output:
             *
             *
             **/
            make_SCR_bank(sec, "TDPL");
            /**
             * input:
             * output:
             *
             *
             **/
            make_SCRC_bank(sec);
        }
        /**
         * input:
         * output:
         *
         * fix pointers in HBID bank- D.P. Weygand
         **/
        reMatchSCRC();
    }

    /// Second pass start counter analysis with time-based tracking
    if ((PROCESS_ST & ProcessFlag) && (PROCESS_TBTR & ProcessFlag)) {
        int trk_level=TIME_BASED;
        /**
         * input:
         * output:
         *
         *
         **/
        st_bevt_();
        /**
         * input:
         * output:
         *
         *
         **/
        st_evnt_(&trk_level);
    }

    /// make the time based particle id bank
    if (PROCESS_TBID & ProcessFlag) {
        dropBank(&bcs_,"TBID", PIDFlag);
        dropBank(&bcs_,"PART", PIDFlag);
        /**
         * input:
         * output:
         *
         *
         **/
        make_TBID_group(PIDFlag);

        /**
         * input:
         * output:
         *
         *
         **/
        make_vert();
        /**
         * input:
         * output:
         *
         *
         **/
        make_mvrt();

        /**
         * input:
         * output:
         *
         *
         **/
        make_PART_group(PIDFlag);
    }


    /**
     * This block does a second pass of time-based tracking with the first-pass TBID
     * as an estimate.
     **/
    int do_second_time_based_id = 1;
    if(do_second_time_based_id) {
        if (PROCESS_TRKS_TBID & ProcessFlag){
            TBID = getBank(&bcs_,"TBID");
            if(TBID){
                if(TBID->bank.nrow){
                    dropAllBanks(&bcs_, "TRKS");
                    for (sec=1; sec <= 6; sec++){
                        /**
                         * input:
                         * output:
                         *
                         *
                         **/
                        clasTRKS_t *TRKS = make_TRKS_bank_plus(
                            sec, getGroup(&bcs_, "TBID", PIDFlag) );
                    }
                }
            }
            dropAllBanks(&bcs_,"TBTRTBLATBERTDPLTRL1");
            /**
             * input:
             * output:
             *
             *
             **/
            ProcessTimeBased();
        }

        /// make the time based particle id bank
        if (PROCESS_TBID & ProcessFlag) {
            dropBank(&bcs_,"TBID", PIDFlag);
            dropBank(&bcs_,"PART", PIDFlag);
            /**
             * input:
             * output:
             *
             *
             **/
            make_TBID_group(PIDFlag);

            /**
             * input:
             * output:
             *
             *
             **/
            make_vert();
            /**
             * input:
             * output:
             *
             *
             **/
            make_mvrt();

            /**
             * input:
             * output:
             *
             *
             **/
            make_PART_group(PIDFlag);
        }
    }

    /// GPID
    if (PROCESS_GPID & ProcessFlag) {
        dropBank(&bcs_,"GPID", PIDFlag);
        /**
         * input:
         * output:
         *
         *
         **/
        makeGPID(PIDFlag,0);
    }

    if (PROCESS_TBID_NOST & ProcessFlag) {
        if(RUNC=getBank(&wcs_, "RUNC")) {
            ///save beam type
            btype = RUNC->runc.beam.type.val.i[0];

            dropBank(&bcs_,"TBID", 2);
            dropBank(&bcs_,"PART", 2);

            /**
             * input:
             * output:
             *
             *
             **/
            make_TBID_group(2);

            /**
             * input:
             * output:
             *
             *
             **/
            make_PART_group(2);

            /// restore beam type
            RUNC->runc.beam.type.val.i[0] = btype;
        }
    }

    /// seb level 2 analysis
    if (PROCESS_SEB & ProcessFlag){
        int level = 2;
        /// seb banks without TRKS
        dropAllBanks(&bcs_,"HEVTEVNTDCPBSCPBCCPBUNUSEVHB");
        /**
         * input:
         * output:
         *
         *
         **/
        run_builder_(&level, &flag);
    }

    if (PROCESS_REGION1 & ProcessFlag){
        clasDHCL_t *DHCL = NULL;
        clasRGLK_t *RGLK = NULL;
        dropAllBanks(&bcs_, REGION1_BANKS);
            /**
             * input:
             * output:
             *
             *
             **/
        trk_region1_();
    }

  return(1);
}







