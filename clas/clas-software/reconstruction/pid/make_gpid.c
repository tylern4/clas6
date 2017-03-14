/* make_gpid.c */
/* Author: Luminita Todor */
/* Purpose: makes GPID bank from TBID, TAGR, TBTR banks */
/* Inputs: PART, TBID, TAGR, TBTR, SCRC, STR, TDPL reconstruction banks */
/* if you don't have them, rebuild them before calling this routine */
/* also needs SCG and TGEO geometry banks */
/* Outputs: GPID bank */


/*  RCS ID string
 *  $Id: make_gpid.c,v 1.31 2004/08/04 14:49:47 pasyuk Exp $*/
#define USE(var) static void * use_##var = (void *) &var

static char crcsid[] =
   "$Id: make_gpid.c,v 1.31 2004/08/04 14:49:47 pasyuk Exp $";

USE(crcsid);   /* make sure it is not optimized away */
/* END of RCS information*/


#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <math.h>
#include <bostypes.h>
#include <bosddl.h>
#include <kinematics.h>
#include <sc.h>
#include <pid.h>
#include <ec.h>
#include <utility.h>
#include <makebanks.h>
#include <particleType.h>
#include <map_manager.h>

#define BAD_LENGTH 1000.0
#define NO_TIME -500.0
#define NO_DEDX -1000.0
#define NO_PADDLE -1000
#define BAD_BETA -1.
#define OUT_OF_CLAS 1000.0

/* define mass flags */
/* most of these constants eventually should go to the map
   some of them probably to the include file */

#define SCTAG_MASS    0 /* mass calculated from SC and TAG */
#define SCST_MASS     1 /* mass claculated from SC and ST */
#define NEUTRAL_MASS -1 /* Neutral particle */
#define PART_MASS     2 /* mass based on PART/TBID */

#define K_VERTEX_WINDOW 2.
#define ALL_PART_VERTEX_WINDOW 2.




/* variables defining cuts
   they are initialized from the GPID.map */

float pi_low[50];
float pi_high[50];
float K_low[50];
float K_high[50];
float p_low[50];
float p_high[50];
float d_low[50];
float d_high[50];
float t_low[50];
float t_high[50];
float pi_dbeta;
float K_dbeta;
float p_dbeta;
float d_dbeta;
float t_dbeta;

float dbeta_offset; /* slantad kut for Kpi separation at high momentum */
float dbeta_slope;

float dbeta_offset_p; /* the same for Kp separation */
float dbeta_slope_p;

float st_min=-50.; /* cut for early hits in ST */

/* Fortran wraper */
void makegpid_(int *group, int *calib) {
  makeGPID(*group, *calib);
}

void initgpid_(int *run) {
  initGPID(*run);
}


int initGPID(int runno){

  /* this function reads all GPID cuts from the map */

  char *dir;
  char gpidmap[1000];
  int status = -1;
  int failed = 0;
  int map_run = 0;
  int err;

  dir=getenv("CLAS_PARMS");
  sprintf(gpidmap,"%s/Maps/GPID.map",dir);

  /* begin pion cuts setup */
  /*-read dbeta-*/
  err = map_get_float(gpidmap,
              "pion",
              "dbeta",
              1,
              &pi_dbeta,
              runno,
              &map_run);
  if(err) {
    fprintf(stderr,
        "initGPID -F- could not get pi_dbeta for run %d from %s\n",
        runno, gpidmap);
    failed++;
  }
  else
    fprintf(stderr, "initGPID -I- Set pi_dbeta from run %d \n", map_run);
  /*-read low cut-*/
  err = map_get_float(gpidmap,
              "pion",
              "low",
              50,
              pi_low,
              runno,
              &map_run);
  if(err) {
    fprintf(stderr,
        "initGPID -F- could not get pi_low for run %d from %s\n",
        runno, gpidmap);
    failed++;
  }
  else
    fprintf(stderr, "initGPID -I- Set pi_low from run %d \n", map_run);
  /*-read high cut-*/
  err = map_get_float(gpidmap,
              "pion",
              "high",
              50,
              pi_high,
              runno,
              &map_run);
  if(err) {
    fprintf(stderr,
        "initGPID -F- could not get pi_high for run %d from %s\n",
        runno, gpidmap);
    failed++;
  }
  else
    fprintf(stderr, "initGPID -I- Set pi_high from run %d \n", map_run);
  /* end pion cuts setup */

  /* begin kaon cuts setup */
  /*-read dbeta-*/
  err = map_get_float(gpidmap,
              "kaon",
              "dbeta",
              1,
              &K_dbeta,
              runno,
              &map_run);
  if(err) {
    fprintf(stderr,
        "initGPID -F- could not get K_dbeta for run %d from %s\n",
        runno, gpidmap);
    failed++;
  }
  else
    fprintf(stderr, "initGPID -I- Set K_dbeta from run %d \n", map_run);
  /*-read low cut-*/
  err = map_get_float(gpidmap,
              "kaon",
              "low",
              50,
              K_low,
              runno,
              &map_run);
  if(err) {
    fprintf(stderr,
        "initGPID -F- could not get K_low for run %d from %s\n",
        runno, gpidmap);
    failed++;
  }
  else
    fprintf(stderr, "initGPID -I- Set K_low from run %d \n", map_run);
  /*-read high cut-*/
  err = map_get_float(gpidmap,
              "kaon",
              "high",
              50,
              K_high,
              runno,
              &map_run);
  if(err) {
    fprintf(stderr,
        "initGPID -F- could not get K_high for run %d from %s\n",
        runno, gpidmap);
    failed++;
  }
  else
    fprintf(stderr, "initGPID -I- Set K_high from run %d \n", map_run);
  /* end kaon cuts setup */

  /* begin proton cuts setup */
  /*-read dbeta-*/
  err = map_get_float(gpidmap,
              "proton",
              "dbeta",
              1,
              &p_dbeta,
              runno,
              &map_run);
  if(err) {
    fprintf(stderr,
        "initGPID -F- could not get p_dbeta for run %d from %s\n",
        runno, gpidmap);
    failed++;
  }
  else
    fprintf(stderr, "initGPID -I- Set p_dbeta from run %d \n", map_run);
  /*-read low cut-*/
  err = map_get_float(gpidmap,
              "proton",
              "low",
              50,
              p_low,
              runno,
              &map_run);
  if(err) {
    fprintf(stderr,
        "initGPID -F- could not get p_low for run %d from %s\n",
        runno, gpidmap);
    failed++;
  }
  else
    fprintf(stderr, "initGPID -I- Set p_low from run %d \n", map_run);
  /*-read high cut-*/
  err = map_get_float(gpidmap,
              "proton",
              "high",
              50,
              p_high,
              runno,
              &map_run);
  if(err) {
    fprintf(stderr,
        "initGPID -F- could not get p_high for run %d from %s\n",
        runno, gpidmap);
    failed++;
  }
  else
    fprintf(stderr, "initGPID -I- Set p_high from run %d \n", map_run);
  /* end proton cuts setup */

  /* begin deuteron cuts setup */
  /*-read dbeta-*/
  err = map_get_float(gpidmap,
              "deuteron",
              "dbeta",
              1,
              &d_dbeta,
              runno,
              &map_run);
  if(err) {
    fprintf(stderr,
        "initGPID -F- could not get d_dbeta for run %d from %s\n",
        runno, gpidmap);
    failed++;
  }
  else
    fprintf(stderr, "initGPID -I- Set d_dbeta from run %d \n", map_run);
  /*-read low cut-*/
  err = map_get_float(gpidmap,
              "deuteron",
              "low",
              50,
              d_low,
              runno,
              &map_run);
  if(err) {
    fprintf(stderr,
        "initGPID -F- could not get d_low for run %d from %s\n",
        runno, gpidmap);
    failed++;
  }
  else
    fprintf(stderr, "initGPID -I- Set d_low from run %d \n", map_run);
  /*-read high cut-*/
  err = map_get_float(gpidmap,
              "deuteron",
              "high",
              50,
              d_high,
              runno,
              &map_run);
  if(err) {
    fprintf(stderr,
        "initGPID -F- could not get d_high for run %d from %s\n",
        runno, gpidmap);
    failed++;
  }
  else
    fprintf(stderr, "initGPID -I- Set d_high from run %d \n", map_run);
  /* end deuteron cuts setup */

  /* begin triton cuts setup */
  /*-read dbeta-*/
  err = map_get_float(gpidmap,
              "triton",
              "dbeta",
              1,
              &t_dbeta,
              runno,
              &map_run);
  if(err) {
    fprintf(stderr,
        "initGPID -F- could not get t_dbeta for run %d from %s\n",
        runno, gpidmap);
    failed++;
  }
  else
    fprintf(stderr, "initGPID -I- Set t_dbeta from run %d \n", map_run);
  /*-read low cut-*/
  err = map_get_float(gpidmap,
              "triton",
              "low",
              50,
              t_low,
              runno,
              &map_run);
  if(err) {
    fprintf(stderr,
        "initGPID -F- could not get t_low for run %d from %s\n",
        runno, gpidmap);
    failed++;
  }
  else
    fprintf(stderr, "initGPID -I- Set t_low from run %d \n", map_run);
  /*-read high cut-*/
  err = map_get_float(gpidmap,
              "triton",
              "high",
              50,
              t_high,
              runno,
              &map_run);
  if(err) {
    fprintf(stderr,
        "initGPID -F- could not get t_high for run %d from %s\n",
        runno, gpidmap);
    failed++;
  }
  else
    fprintf(stderr, "initGPID -I- Set t_high from run %d \n", map_run);
  /* end triton cuts setup */
  /* get Kpi cut */
  err = map_get_float(gpidmap,
              "kpi",
              "offset",
              1,
              &dbeta_offset,
              runno,
              &map_run);
  if(err) {
    fprintf(stderr,
        "initGPID -F- could not get Kpi offset for run %d from %s\n",
        runno, gpidmap);
    failed++;
  }
  else
    fprintf(stderr, "initGPID -I- Set Kpi offset from run %d \n", map_run);
  err = map_get_float(gpidmap,
              "kpi",
              "slope",
              1,
              &dbeta_slope,
              runno,
              &map_run);
  if(err) {
    fprintf(stderr,
        "initGPID -F- could not get Kpi slope for run %d from %s\n",
        runno, gpidmap);
    failed++;
  }
  else
    fprintf(stderr, "initGPID -I- Set Kp slope from run %d \n", map_run);
   /* get Kp cut */
  err = map_get_float(gpidmap,
              "kp",
              "offset",
              1,
              &dbeta_offset_p,
              runno,
              &map_run);
  if(err) {
    fprintf(stderr,
        "initGPID -F- could not get Kp offset for run %d from %s\n",
        runno, gpidmap);
    failed++;
  }
  else
    fprintf(stderr, "initGPID -I- Set Kp offset from run %d \n", map_run);
  err = map_get_float(gpidmap,
              "kp",
              "slope",
              1,
              &dbeta_slope_p,
              runno,
              &map_run);
  if(err) {
    fprintf(stderr,
        "initGPID -F- could not get Kp slope for run %d from %s\n",
        runno, gpidmap);
    failed++;
  }
  else
    fprintf(stderr, "initGPID -I- Set Kp slope from run %d \n", map_run);

  /* now read st_min from the map */
    fprintf(stderr, "initGPID -I- Set st_min from run %d \n", map_run);
  err = map_get_float(gpidmap,
              "st",
              "st_min",
              1,
              &st_min,
              runno,
              &map_run);
  if(err) {
    fprintf(stderr,
        "initGPID -W- could not get st_min for run %d from %s\n"
             "will use default value st_min=0.0 ns\n",
        runno, gpidmap);
  }



  /* end reading all constants */

  if(failed)
    fprintf(stderr,"initGPID -F- there was %d "
        "errors while reading constants from the map\n\n",failed);
  else
    fprintf(stderr,"initGPID -I- GPID initialized seuccessfully for run %d\n",runno);

  return failed;
}
/* end initGPID */


float vtime_offset(float x) {
  /* for the time being it is hardcoded.
     Constants has been found from g2 data */
   return (1./(11.1386*x*x + 2.46276*x + 0.17632));
}



clasGPID_t *makeGPID(int bankNum, int calib)
{
  int i;
  int tbid_ind;
  int track;
  int ik;
  int ii;
  int sec;
  int k;
  int scrc_id;
  int j;
  int best_tagr;
  int n_photons;
  int beta_ind;
  int n_charged;

  float best_diff;
  float diff;
  float dbeta;
  float pmag;
  float stlen;
  float sclen;
  float betac;
  float dtc;
  float betam;
  float betadiff;
  float tvertex;
  float mass;
  float vertex_window;
  float tprop;
  float dt_low;
  float dt_high;
  float dbeta_cut;

  clasTDPL_t *TDPL = NULL;
  clasTBTR_t *TBTR = getBank( &bcs_,"TBTR");
  clasPART_t *PART = getGroup(&bcs_,"PART",1);
  clasBID_t  *TBID = getGroup(&bcs_,"TBID",1);
  clasTAGR_t *TAGR = getBank( &bcs_,"TAGR");
  clasGPID_t *GPID = NULL;
  clasHEAD_t *HEAD = getBank( &bcs_,"HEAD");
  clasSTR_t  *STR  = getGroup(&bcs_,"STR ",1);
  clasSCRC_t *SCRC = NULL;

  clasTGEO_t *TGEO = getBank(&wcs_,"TGEO");

  float ztarget = TGEO ? TGEO->tgeo[0].z : 0.0;
  tagr_t *tagr;

  /* let's check if all necessary banks are present */

  if(PART && TBID && TAGR && TBTR) {

    /* register and allocate memory GPID bank in BOS system */

    GPID = makeBank(&bcs_,"GPID",
            bankNum,
            sizeof(gpid_t)/4,
            PART->bank.nrow);

    n_charged = 0;
    /* now we will loop over all the particles in PART bank */

    for (i=0;i < PART->bank.nrow; i++) {

      /* copy some info from PART to GPID first
       * regardles from what happens next */

      tbid_ind = PART->part[i].trkid-1;

      GPID->gpid[i].ppid  = PART->part[i].pid;
      GPID->gpid[i].pid   = PART->part[i].pid;
      GPID->gpid[i].vert  = PART->part[i].vert;
      GPID->gpid[i].q     = (int) PART->part[i].q;
      GPID->gpid[i].p     = PART->part[i].p;
      GPID->gpid[i].trkid = PART->part[i].trkid;
      GPID->gpid[i].sec   = TBID->bid[tbid_ind].sec;

      sec   = GPID->gpid[i].sec;
      tprop = (GPID->gpid[i].vert.z-ztarget)/LIGHT_SPEED;

      /*  just copy stuff from TBID and do nothing */

      GPID->gpid[i].beta     = TBID->bid[tbid_ind].beta;
      GPID->gpid[i].sc_stat  = TBID->bid[tbid_ind].sc.stat;
      GPID->gpid[i].sc_time  = TBID->bid[tbid_ind].sc.time;
      GPID->gpid[i].sc_len   = OUT_OF_CLAS;
      GPID->gpid[i].st_stat  = TBID->bid[tbid_ind].st.stat;
      GPID->gpid[i].st_time  = TBID->bid[tbid_ind].st.time;
      GPID->gpid[i].st_len   = OUT_OF_CLAS;

      if(!GPID->gpid[i].q ) {

        /* neutral particle */

        GPID->gpid[i].betam    = TBID->bid[tbid_ind].beta;
        GPID->gpid[i].mass     = 0.0;
        GPID->gpid[i].mass_ref = NEUTRAL_MASS;
        GPID->gpid[i].epho     = 0.0;
        GPID->gpid[i].tpho     = 0.0;
        GPID->gpid[i].tagrid   = 0;
        GPID->gpid[i].ngrf     = 0;
      }

      else {/* charged particle */

        /* Ok, it is charged particle
         * we will try to guess its type */
        n_charged++;
        pmag = v3mag(PART->part[i].p.space);
        if( GPID->gpid[i].sc_stat &&
            (SCRC = (clasSCRC_t* )getGroup(&bcs_,
                               "SCRC",
                               TBID->bid[tbid_ind].sec))
            && (TDPL = (clasTDPL_t* )getGroup(&bcs_,
                              "TDPL",
                              TBID->bid[tbid_ind].sec) )) {
          /* SC info aavailable
           * copy TOF relevant information to GPID */
          scrc_id = TBID->bid[tbid_ind].sc.id-1;
          GPID->gpid[i].sc_time = TBID->bid[tbid_ind].sc.time;
          GPID->gpid[i].paddle  = SCRC->scrc[scrc_id].id;
          GPID->gpid[i].dedx    = SCRC->scrc[scrc_id].energy;

          /* get pathength to TOF */

          sclen = pathlen2sc(&TBID->bid[tbid_ind],
                     (hdpl_t *)tbid2tdpl(&TBID->bid[tbid_ind]));
          GPID->gpid[i].sc_len = sclen;
        }
        else {

          /* no TOF for this track */

          GPID->gpid[i].paddle  = NO_PADDLE;
          GPID->gpid[i].dedx    = NO_DEDX;

        }

        if(GPID->gpid[i].st_stat && STR) {

          /* retrive ST information */

          GPID->gpid[i].st_time = TBID->bid[tbid_ind].st.time;
          stlen = STR->str[TBID->bid[tbid_ind].st.id-1].st_l;
          GPID->gpid[i].st_len = stlen;
        }
        else
          GPID->gpid[i].st_time = NO_TIME; /* no ST for this track */

        /* if st_time is less than st_min, it usually means that */
        /* it was messed up by earlier particle */
        /* we will deal with as if we don't have st at all for this track. */
        if(GPID->gpid[i].st_time < st_min) GPID->gpid[i].st_stat = 0;

        /* Do another test, sometimes TOF is negative */
        /* This happens beacause of the messed up hit in ST */

        if(GPID->gpid[i].sc_stat && GPID->gpid[i].st_stat){
          betam = (sclen-stlen)/
            (LIGHT_SPEED*(GPID->gpid[i].sc_time-GPID->gpid[i].st_time));
          if(betam <=0.) GPID->gpid[i].sc_stat = 0;
        }

        /* yet another test */
        /* if ST comes earlier than "good" photon */
        /* We will fall back to PART */
        if (TBID->bid[tbid_ind].st.vtime - TBID->bid[tbid_ind].sc.vtime <= -4.9)
          GPID->gpid[i].st_stat = 0;

        if(GPID->gpid[i].sc_stat && GPID->gpid[i].st_stat && STR ) {

          /* if we have ST and SC
             we will try to identify charged particles */

          betadiff = 1.0;

          /* get betam from time-of-flight between SC and ST */

          betam = (sclen-stlen)/
            (LIGHT_SPEED*(GPID->gpid[i].sc_time-GPID->gpid[i].st_time));

          /* LIGHT_SPEED=29.978 is defined in pid.h
             speed of light in cm/nanosec */

          /* calculate betac from measured momentum
             assuming certain particle mass */

          /*------------------------------------------------------------*/
          /*e+/e- ID is turned off until we find out how to do it right */
          /* try e+ or e- first */

          /*      betac = pmag/sqrt(pmag*pmag + ELECTRON_MASS*ELECTRON_MASS); */
          /*      if(fabs(betac-betam)<betadiff && */
          /*         (TBID->bid[tbid_ind].cc.stat)){ */
          /*        if(GPID->gpid[i].q<0.)  */
          /*          GPID->gpid[i].pid = Electron;  */
          /* e- */
          /*        else  */
          /*          GPID->gpid[i].pid = Positron;  */
          /* e+ */

          /*        GPID->gpid[i].p.t = sqrt(pmag*pmag + ELECTRON_MASS*ELECTRON_MASS); */
          /*        betadiff = betac-betam; */
          /*      } */
          /*-----------------------------------------------------------*/

          /* try pion */

          betac = pmag/sqrt(pmag*pmag + PI_CHARGED_MASS*PI_CHARGED_MASS);
          if(fabs(betadiff)>fabs(betac-betam)) {
            if(GPID->gpid[i].q<0.)
              GPID->gpid[i].pid = PiMinus; /* Pi- */
            else
              GPID->gpid[i].pid = PiPlus; /* Pi+ */

            GPID->gpid[i].p.t = sqrt(pmag*pmag + PI_CHARGED_MASS*PI_CHARGED_MASS);
            betadiff = betac-betam;
          }

          /* try Kaon */

          betac = pmag/sqrt(pmag*pmag + KAON_CHARGED_MASS*KAON_CHARGED_MASS);
          if(fabs(betadiff)>fabs(betac-betam)) {
            if(GPID->gpid[i].q<0.)
              GPID->gpid[i].pid = KMinus; /* K- */
            else
              GPID->gpid[i].pid = KPlus; /* K+ */

            GPID->gpid[i].p.t = sqrt(pmag*pmag + KAON_CHARGED_MASS*KAON_CHARGED_MASS);
            betadiff = betac-betam;
          }

          /* try proton */

          betac = pmag/sqrt(pmag*pmag + PROTON_MASS*PROTON_MASS);
          if(fabs(betadiff)>fabs(betac-betam)) {
            if(GPID->gpid[i].q<0.) GPID->gpid[i].pid = AntiProton; /* antiproton */
            else
              GPID->gpid[i].pid = Proton; /* proton */
            GPID->gpid[i].p.t = sqrt(pmag*pmag + PROTON_MASS*PROTON_MASS);
            betadiff = betac-betam;
          }

          /* try deuteron */

          betac = pmag/sqrt(pmag*pmag + DEUTERON_MASS*DEUTERON_MASS);
          if(fabs(betadiff)>fabs(betac-betam)) {
            if(GPID->gpid[i].q>0)
              GPID->gpid[i].pid = Deuteron; /* deuteron */
            GPID->gpid[i].p.t = sqrt(pmag*pmag + DEUTERON_MASS*DEUTERON_MASS);
            betadiff = betac-betam;
          }
          /* try triton */

          betac = pmag/sqrt(pmag*pmag + TRITON_MASS*TRITON_MASS);
          if(fabs(betadiff)>fabs(betac-betam)) {
            if(GPID->gpid[i].q>0)
              GPID->gpid[i].pid = Triton; /* triton */
            GPID->gpid[i].p.t = sqrt(pmag*pmag + TRITON_MASS*TRITON_MASS);
            betadiff = betac-betam;
          }

          /* At this point we presumably guessed particle ID
           * calculated beta from nominal mass and momentum
           * and put it into to the GPID */

          GPID->gpid[i].beta = pmag/GPID->gpid[i].p.t;

          /* special attention to Kaons */
          /* let's find best photon */
          /* in calibration mode we won't do that */

          if(calib == 0){
            if((GPID->gpid[i].pid == KPlus ||
                GPID->gpid[i].pid == KMinus) ) {

              tvertex   = TBID->bid[tbid_ind].st.vtime;
              best_diff = ST_TAG_COINCIDENCE_WINDOW;
              best_tagr = -1;

              /*find best difference */

              for (k=0;k<TAGR->bank.nrow;k++) {
                if (TAGR->tagr[k].stat == 7 || TAGR->tagr[k].stat == 15) {
                  diff = TAGR->tagr[k].tpho+tprop-tvertex-vtime_offset(GPID->gpid[i].beta);
                  if (fabs(diff) <= fabs(best_diff)) {
                    best_diff = diff;
                    best_tagr = k;
                  }
                }
              }

              if(fabs(best_diff) <= K_VERTEX_WINDOW &&
                 best_tagr != -1)  {

                /* we will check dbeta vs beta to remove pions and protons at high beta */

                betam = sclen/
                  (LIGHT_SPEED*(GPID->gpid[i].sc_time-TAGR->tagr[best_tagr].tpho-tprop));

                dbeta = betam - GPID->gpid[i].beta;

                /* clean up pions from Kaons */
                if (dbeta > ((dbeta_offset-betam)*dbeta_slope)) {

                  /* this is pion */

                  if(GPID->gpid[i].q<0.)
                    GPID->gpid[i].pid = PiMinus; /* Pi- */
                  else
                    GPID->gpid[i].pid = PiPlus; /* Pi+*/

                  GPID->gpid[i].p.t  = sqrt(pmag*pmag + PI_CHARGED_MASS*PI_CHARGED_MASS);
                  GPID->gpid[i].beta = pmag/GPID->gpid[i].p.t;
                }
                /* clean up protons from Kaons */
                if (dbeta < ((dbeta_offset_p-betam)*dbeta_slope_p)) {

                  /* this is proton */

                  if(GPID->gpid[i].q<0.)
                    GPID->gpid[i].pid = AntiProton; /* AntiProton */
                  else
                    GPID->gpid[i].pid = Proton; /* Proton */

                  GPID->gpid[i].p.t  = sqrt(pmag*pmag + PROTON_MASS*PROTON_MASS);
                  GPID->gpid[i].beta = pmag/GPID->gpid[i].p.t;
                }
              }
            }
          }

          /* at this point we have "correct" ID for the particle
             It is time to pick "good" photon */

          GPID->gpid[i].epho   = 0.0;
          GPID->gpid[i].tpho   = 0.0;
          GPID->gpid[i].tagrid = 0;
          GPID->gpid[i].ngrf   = 0;

          best_diff = 5.           ;
          best_tagr = -1;
          n_photons = 0;

          if(calib) {
            dt_low  = -5.;
            dt_high =  5.;
          }
          else {
            dt_low  = -5.;
            dt_high =  5.;
            beta_ind = (int) (GPID->gpid[i].beta/0.02);
            if(beta_ind > 50) beta_ind = 50;

            if (GPID->gpid[i].pid == Electron ||
                GPID->gpid[i].pid == Positron) {
              dt_low  = pi_low[50];
              dt_high = pi_high[50];
            }
            else if(GPID->gpid[i].pid == PiPlus ||
                    GPID->gpid[i].pid == PiMinus) {
              dt_low  = pi_low[beta_ind];
              dt_high = pi_high[beta_ind];
            }
            else if(GPID->gpid[i].pid == KPlus ||
                    GPID->gpid[i].pid == KMinus) {
              dt_low  = K_low[beta_ind];
              dt_high = K_high[beta_ind];
            }
            else if(GPID->gpid[i].pid == Proton ||
                    GPID->gpid[i].pid == AntiProton) {
              dt_low  = p_low[beta_ind];
              dt_high = p_high[beta_ind];
            }
            else if(GPID->gpid[i].pid == Deuteron) {
              dt_low  = d_low[beta_ind];
              dt_high = d_high[beta_ind];
            }
            else if(GPID->gpid[i].pid == Triton) {
              dt_low  = t_low[beta_ind];
              dt_high = t_high[beta_ind];
            }
          }
          for (k=0;k<TAGR->bank.nrow;k++) {
            if ((TAGR->tagr[k].stat == 7 || TAGR->tagr[k].stat == 15)) {
              diff = TAGR->tagr[k].tpho+tprop-TBID->bid[tbid_ind].st.vtime;
              if ( fabs(diff) <= fabs(best_diff)) {
                best_diff = diff;
                best_tagr = k;
                if(diff >= dt_low && diff <=dt_high) {
                  n_photons++;
                }
              }
            }
          }

          /* put number of photons found in GPID */

          GPID->gpid[i].ngrf = n_photons;

          if(n_photons) {

            /* we found at least one good photon)*/

            GPID->gpid[i].tpho   = TAGR->tagr[best_tagr].tpho;
            GPID->gpid[i].epho   = TAGR->tagr[best_tagr].erg;

            /* put TAGR row number counting from 1 */

            GPID->gpid[i].tagrid = best_tagr+1;

            /* recalculate SC beta from SC and TAGR*/

            GPID->gpid[i].betam = sclen/
              (LIGHT_SPEED*(GPID->gpid[i].sc_time-GPID->gpid[i].tpho-tprop));
            GPID->gpid[i].mass_ref = SCTAG_MASS;

            if (GPID->gpid[i].betam>1.)
              betam = 1.;
            else
              betam = GPID->gpid[i].betam;

            /* calculate "measured" mass from SC beta*/

            GPID->gpid[i].mass = beta_p_2mass(pmag,betam);

            /* apply final dbeta cut *.
               /* we will change the sign of PID if faild this cut */
            /* that might be handy to estimate background */

            if (GPID->gpid[i].pid == Electron ||
                GPID->gpid[i].pid == Positron ||
                GPID->gpid[i].pid == PiPlus   ||
                GPID->gpid[i].pid == PiMinus )    dbeta_cut = pi_dbeta;
            else if (GPID->gpid[i].pid == KPlus ||
                     GPID->gpid[i].pid == KMinus) dbeta_cut = K_dbeta;
            else if (GPID->gpid[i].pid == Proton ||
                     GPID->gpid[i].pid == AntiProton) dbeta_cut = p_dbeta;
            else if (GPID->gpid[i].pid == Deuteron ) dbeta_cut = d_dbeta;
            else if (GPID->gpid[i].pid == Triton ) dbeta_cut = t_dbeta;

            /* coment out this feature for the time being */
            /*      if(fabs(GPID->gpid[i].betam - GPID->gpid[i].beta) > dbeta_cut) */
            /*        GPID->gpid[i].pid = -1*GPID->gpid[i].pid;     */
          }
          else {

            /* no good photons found */

            if(GPID->gpid[i].pid == GPID->gpid[i].ppid &&
               GPID->gpid[i].ppid != 0) {
              /* it might be that cut is too tight or ST messed up */
              /* will try to find photon PID dependent way then */
              tvertex = GPID->gpid[i].sc_time-sclen/(LIGHT_SPEED*GPID->gpid[i].beta);

              GPID->gpid[i].epho   = 0.0;
              GPID->gpid[i].tpho   = 0.0;
              GPID->gpid[i].tagrid = 0;
              GPID->gpid[i].ngrf   = 0;

              best_diff = ST_TAG_COINCIDENCE_WINDOW;
              best_tagr = -1;
              n_photons = 0;
              for (k=0;k<TAGR->bank.nrow;k++) {
                if ((TAGR->tagr[k].stat == 7 || TAGR->tagr[k].stat == 15)) {
                  diff = fabs(TAGR->tagr[k].tpho+tprop-tvertex);
                  if (diff <= best_diff) {
                    best_diff = diff;
                    best_tagr = k;
                  }
                }
              }
              if (best_diff <=  1.55) {
                /* we found it */
                n_photons = 1;
                GPID->gpid[i].tpho   = TAGR->tagr[best_tagr].tpho;
                GPID->gpid[i].epho   = TAGR->tagr[best_tagr].erg;

                /* put TAGR row number counting from 1 */

                GPID->gpid[i].tagrid = best_tagr+1;

                /* recalculate SC beta from SC and TAGR*/

                GPID->gpid[i].betam = sclen/
                  (LIGHT_SPEED*(GPID->gpid[i].sc_time-GPID->gpid[i].tpho-tprop));
                GPID->gpid[i].mass_ref = SCTAG_MASS;

                if (GPID->gpid[i].betam>1.)
                  betam = 1.;
                else
                  betam = GPID->gpid[i].betam;

                /* calculate "measured" mass from SC beta*/

                GPID->gpid[i].mass = beta_p_2mass(pmag,betam);
                /* apply final dbeta cut */
                /* we will change the sign of PID if faild this cut */
                /* that might be handy to estimate background */

                if (GPID->gpid[i].pid == Electron ||
                    GPID->gpid[i].pid == Positron ||
                    GPID->gpid[i].pid == PiPlus   ||
                    GPID->gpid[i].pid == PiMinus )    dbeta_cut = pi_dbeta;
                else if (GPID->gpid[i].pid == KPlus ||
                         GPID->gpid[i].pid == KMinus) dbeta_cut = K_dbeta;
                else if (GPID->gpid[i].pid == Proton ||
                         GPID->gpid[i].pid == AntiProton) dbeta_cut = p_dbeta;
                else if (GPID->gpid[i].pid == Deuteron ) dbeta_cut = d_dbeta;
                else if (GPID->gpid[i].pid == Triton ) dbeta_cut = t_dbeta;

                /*      if(fabs(GPID->gpid[i].betam - GPID->gpid[i].beta) > dbeta_cut) */
                /*        GPID->gpid[i].pid = -1*GPID->gpid[i].pid;     */
              }
            }
          }

          if (n_photons == 0){
            /* sill not found */

            GPID->gpid[i].betam = (sclen-stlen)/
              (LIGHT_SPEED*(GPID->gpid[i].sc_time-GPID->gpid[i].st_time));

            if (GPID->gpid[i].betam>1.)
              betam = 1.;
            else
              betam = GPID->gpid[i].betam;

            /* we use SC beta from SC and ST because wi did not find good TAGR hit */

            GPID->gpid[i].mass     = beta_p_2mass(pmag,betam);
            GPID->gpid[i].mass_ref = SCST_MASS;
          }
        }

        /*-----------------------------*/
        else {

          /* tracks or SC or ST missing, can't do GPID,
           *  will use classsical PID for this particle */

          GPID->gpid[i].beta     = pmag/GPID->gpid[i].p.t;
          GPID->gpid[i].mass     = PART->part[i].qpid;
          GPID->gpid[i].mass_ref = PART_MASS;
          GPID->gpid[i].betam    = pmag/
            sqrt(pmag*pmag+GPID->gpid[i].mass*GPID->gpid[i].mass);

          GPID->gpid[i].epho   = 0.0;
          GPID->gpid[i].tpho   = 0.0;
          GPID->gpid[i].tagrid = 0;
          GPID->gpid[i].ngrf   = 0;

          n_photons = 0;
          if((tagr = get_photon_tagr(TAGR, TBID, TIME_BASED))) {

            /* got some photon(s) */
            n_photons = 1;
            GPID->gpid[i].ngrf   = 0;
            GPID->gpid[i].epho   = tagr->erg;
            GPID->gpid[i].tpho   = tagr->tpho;

            /* find out tagr index */

            for (k=0;k<TAGR->bank.nrow;k++){
              if((TAGR->tagr[k].stat == 7 ||
                  TAGR->tagr[k].stat == 15) &&
                 (TAGR->tagr[k].e_id == tagr->e_id)) GPID->gpid[i].tagrid = k+1;
            }
          }
        }/* done with classsical PID */

        if(n_photons) {
          /* now we will count how many photons in the bucket */
          n_photons = 0;
          for (k=0;k<TAGR->bank.nrow;k++){
            if((TAGR->tagr[k].stat == 7 ||
                TAGR->tagr[k].stat == 15)&&
               (fabs(TAGR->tagr[k].tpho-GPID->gpid[i].tpho) <=1.)) n_photons++;
          }
          GPID->gpid[i].ngrf   = n_photons;
        }
      } /* end charged particle */
    }  /* END LOOP OVER PARTICLES */
    return GPID;
  }
  else return NULL; /* some banks are missing could not nake it */
}
/* ---------------------------------- End of GPID -------------------------- */










