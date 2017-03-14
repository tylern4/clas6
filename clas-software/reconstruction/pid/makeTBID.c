#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <math.h>
#include <bostypes.h>
#include <kinematics.h>
#include <sc.h>
#include <pid.h>
#include <ec.h>
#include <utility.h>
#include <makebanks.h>
#include <call.h>


/* -=======================================================- *
$Id: makeTBID.c,v 1.36 2008/04/02 03:24:52 fklein Exp $
$Author: fklein $
$Revision: 1.36 $
$Date: 2008/04/02 03:24:52 $
* -=======================================================- */

#define BID_MAX_SIZE 30

int make_tbid_group_(int *groupno){
  make_TBID_group(*groupno);
}

/**
 * for backward compatability
 **/
int make_tbid_bank_(){
  make_TBID_bank();
}

/**
 * for backward compatability
 **/
int make_TBID_bank(){
  if (make_TBID_group(0)) return 1;
  return 0;
}

clasBID_t *make_TBID_group(int number)
{
    //fprintf(stderr, "make TBID bank new event\n");
    /**
     * input: RUNC, HBTR, HDPL, ECHB, EC1R, and SCRC banks.
     * For photon running, also need the TAGR and STR banks.
     * For electron running, also need CC01 bank.
     * output: TBID bank
     **/
    clasRUNC_t *RUNC=getBank(&wcs_,"RUNC");
    clasTGEO_t *TGEO = getBank(&wcs_, "TGEO");
    clasHDPL_t *HDPL=NULL;
    clasSCRC_t *SCRC=NULL;
    clasSTR_t *STR=NULL;
    clasTBTR_t *TBTR=NULL;
    clasBID_t *TBID=NULL;
    clasCC01_t *CC01=NULL;
    clasECHB_t *ECHB=NULL;
    clasEC1R_t *EC1R=NULL;
    clasTAGR_t *TAGR=getBank(&bcs_,"TAGR");

    tdpl_t *tdpl;

    int ec_track_match[BID_MAX_SIZE],ec_id=0;
    int lac_track_match[BID_MAX_SIZE],lac_id=0;
    int sc_track[6][BID_MAX_SIZE],sc_id=0;

    bid_t tbid[BID_MAX_SIZE];
    float vertex_time;
    int nTBID = 0, i, j, ntrk, sec;
    int sort_tbid(const void *tbid1, const void *tbid2);
    int trk_sec;
    int current_sector;
    int matchTOF;
    extern vector3_t beam_position;
    float neutral_path_length = 0.0;
    int trk_ind;

    //fprintf(stderr,"We are in TBID\n");
    memset((void *)ec_track_match,0, BID_MAX_SIZE*sizeof(int));
    memset((void *)lac_track_match,0, BID_MAX_SIZE*sizeof(int));
    memset((void *)sc_track,0,6*BID_MAX_SIZE*sizeof(int));

    /**
     * charged particles
     **/
    if (TBTR = (clasTBTR_t*) getBank(&bcs_, "TBTR"))
    {
        current_sector = 0;
        for (ntrk=0; (ntrk < TBTR->bank.nrow) && (ntrk < BID_MAX_SIZE); ntrk++)
        {
            memset((void*) &(tbid[nTBID].track), 0, sizeof(bid_t));
            tbid[nTBID].track = ntrk+1;
            tbid[nTBID].sec = TBTR->tbtr[ntrk].itr_sec/100;
            if (tbid[nTBID].sec != current_sector)
            {
                current_sector = tbid[nTBID].sec;
                trk_sec = 1;
            }
            else trk_sec++;

            if (SCRC = getGroup(&bcs_,"SCRC", (TBTR->tbtr[ntrk].itr_sec/100)))
            {
                match_to_tof(
                    (hdpl_t*) tbtr2tdpl(&(TBTR->tbtr[ntrk])),
                    SCRC,
                    &(tbid[nTBID].sc) );
                if (tbid[nTBID].sc.stat)
                {
                    sc_track[tbid[nTBID].sec-1][sc_id-1]=1;
                }
            }
            if (STR = getGroup(&bcs_,"STR ",1))
            {
                match_to_st(
                    (hdpl_t*) tbtr2tdpl(&(TBTR->tbtr[ntrk])),
                    STR,
                    &(tbid[nTBID]),
                    trk_sec );
            }


            if (CC01 = getGroup(&bcs_,"CC01", (TBTR->tbtr[ntrk].itr_sec/100)))
            {
                match_to_CC01(
                    (hdpl_t*) tbtr2tdpl(&(TBTR->tbtr[ntrk])),
                    CC01,
                    &(tbid[nTBID].cc) );
            }

            if (ECHB = getBank(&bcs_, "ECHB")){
                match_to_ec(
                    (hdpl_t*) tbtr2tdpl(&(TBTR->tbtr[ntrk])),
                    tbid[nTBID].sec,
                    ECHB,
                    &(tbid[nTBID].ec) );
                if(tbid[nTBID].ec.stat)
                {
                    ec_track_match[tbid[nTBID].ec.id-1] = 1;
                }
            }

            if (EC1R = getGroup(&bcs_, "EC1R", (TBTR->tbtr[ntrk].itr_sec/100)))
            {
                match_to_lac(
                    (hdpl_t*) tbtr2tdpl(&(TBTR->tbtr[ntrk])),
                    tbid[nTBID].sec,
                    EC1R,
                    &(tbid[nTBID].lac) );
                if (tbid[nTBID].lac.stat)
                {
                    lac_track_match[tbid[nTBID].lac.id-1] = 1;
                }
            }
            nTBID++;
        }
    }

    //fprintf(stderr,"TBID Got all charged tracks\n");
    /**
     * neutrals
     **/
    if (ECHB = getBank(&bcs_, "ECHB")){
        for (i=0; i < ECHB->bank.nrow; i++) {
            int layer = echb2layer(&ECHB->echb[i]);
            float closest_app = echb_close2tbtr(&ECHB->echb[i]);
            /**
             * If this EC hit was already
             * matched to a track, go to the next one
             **/
            if (ec_track_match[i])
            {
                continue;
            }
            /**
             * otherwise proceed..
             **/
            sec = echb2sector(&ECHB->echb[i]);
            if (layer == ECHB_WHOLE
                && gamma_energy(&ECHB->echb[i]) > .1)
            {
                memset((void*) &(tbid[nTBID].track), 0, sizeof(tbid_t));
                tbid[nTBID].sec = sec;
                /**
                 * set ec_stat according to a fiducial cut
                 **/
                if (ECHB->echb[i].path_u < EC_FIDUCIAL_CUT ||
                    ECHB->echb[i].path_v < EC_FIDUCIAL_CUT ||
                    ECHB->echb[i].path_w < EC_FIDUCIAL_CUT)
                {
                    tbid[nTBID].ec.stat=NONFIDUCIAL_MATCH;
                }
                else
                {
                    tbid[nTBID].ec.stat=GOOD_MATCH;
                }
                /// Fortran style array index
                tbid[nTBID].ec.id = i+1;
                tbid[nTBID].ec.time = ECHB->echb[i].t_hit;
                tbid[nTBID].ec.qual = closest_app;
                nTBID++;
            }
        }
    }

    /// Two possible LAC planes
    for (sec=1;sec<3;sec++)
    {
        if (EC1R = getGroup(&bcs_, "EC1R",sec))
        {
            for (i=0; i < EC1R->bank.nrow; i++)
            {
                float closest_app = ec1r_close2tbtr(&EC1R->ec1r[i]);

                /**
                 * If this LAC hit was already matched
                 * to a track, go to the next one
                 **/
                if (lac_track_match[i])
                {
                    continue;
                }
                /**
                 * otherwise proceed..
                 **/
                if (EC_MAGIC_NUMBER*EC1R->ec1r[i].e_tot > .1 &&
                    v3dot(EC1R->ec1r[i].pos, EC1R->ec1r[i].pos) > 0.0)
                {
                    memset((void *)&(tbid[nTBID].track),0, sizeof(tbid_t));
                    tbid[nTBID].sec = sec;
                    tbid[nTBID].lac.stat = GOOD_MATCH;
                    /// Fortran style array index
                    tbid[nTBID].lac.id = i+1;
                    tbid[nTBID].lac.time = EC1R->ec1r[i].t_tot;
                    tbid[nTBID].lac.qual = closest_app;
                    nTBID++;
                }
            }
        }
    }

    if (nTBID)
    {
        /**
         * sort by detector hit, charge and momentum
         **/
        qsort((void*) tbid, nTBID, sizeof(bid_t), sort_tbid);
        if (TBID = makeBank(&bcs_, "TBID", number, sizeof(bid_t)/4, nTBID))
        {
            TBID->bid[0] = tbid[0];
            if (RUNC)
            {
                if (RUNC->runc.beam.type.val.i[0] == PHOTON_RUN ||
                    RUNC->runc.beam.type.val.i[0] == G7_RUN ||
                    RUNC->runc.beam.type.val.i[0] == G2B_RUN ||
                    RUNC->runc.beam.type.val.i[0] == G11_RUN)
                {
                    /**
                     * photon-beam case or g7, or g2b
                     **/
                    if (RUNC->runc.beam.type.val.i[0]==PHOTON_RUN)
                    {
                        vertex_time = get_vertex_time(nTBID,tbid,TAGR,TIME_BASED);
                    }
                    else if (RUNC->runc.beam.type.val.i[0]==G7_RUN)
                    {
                        vertex_time = get_vertex_timeG7(nTBID,tbid,TAGR,TIME_BASED);
                    }
                    else if (RUNC->runc.beam.type.val.i[0]==G2B_RUN)
                    {
                        vertex_time = get_vertex_timeG2b(nTBID,tbid,TAGR,TIME_BASED);
                    }
                    else if (RUNC->runc.beam.type.val.i[0]==G11_RUN)
                    {
                        vertex_time = get_vertex_time(nTBID,tbid,TAGR,TIME_BASED);
                    }

                    trk_ind=TBID->bid[0].track;
                    TBID->bid[0].vtime = vertex_time;
                    TBID->bid[0].sc.vtime=TBID->bid[0].vtime;

                    if(trk_ind && TBID->bid[0].sc.stat)
                    {
                        tdpl = tbtr2tdpl(&(TBTR->tbtr[trk_ind-1]));
                        /**
                         * sc beta and beta are the same for
                         * first particle, we assume the first
                         * particle hits the TOF
                         **/
                        TBID->bid[0].sc.beta = TBID->bid[0].beta =
                            ((pathlen2sc(&(TBID->bid[0]),(hdpl_t *)tdpl)/
                            (TBID->bid[0].sc.time - TBID->bid[0].vtime))/SPEED_OF_LIGHT)*1E9;
                    }
                    else
                    {
                        TBID->bid[0].sc.beta = TBID->bid[0].beta = BAD_VERTEX_TIME;
                    }
                }
                else if (TBID->bid[0].sc.stat)
                {
                    /**
                     * electron case.
                     * by defn. equal to one
                     **/
                    TBID->bid[0].sc.beta = TBID->bid[0].beta = 1.0;
                    TBID->bid[0].sc.vtime = TBID->bid[0].sc.time -
                    (dist_to_vertex((hdpl_t*)tbtr2tdpl(&(TBTR->tbtr[tbid[0].track-1])))
                    /SPEED_OF_LIGHT)*1E9;
                    /**
                     * pick a vtime with RF and target correction
                     **/
                    TBID->bid[0].vtime = rf_corr_time(TBID->bid[0].sc.vtime) +
                    (TBTR->tbtr[tbid[0].track-1].vert.z -TGEO->tgeo[0].z )/(SPEED_OF_LIGHT/1E9);
                }
            }
            /**
             * now do all the other particles
             **/
/**
 * calculate beta by getting pathlen and pathtime
 *   trys in this order:
 *     ST to TOF
 *     Vertex to TOF
 *     Vertex to EC
 *     Vertex to LAC
 **/
float pathlen = 0.;
float pathtime = 0.;
for (i=1; i<nTBID; i++)
{
    TBID->bid[i] = tbid[i];
    /// electron vertex time with no RF correction
    TBID->bid[i].sc.vtime = TBID->bid[0].sc.vtime;
    ///electron vertex time with RF correction
    TBID->bid[i].vtime = TBID->bid[0].vtime;
    /// if it's a charged track try to get beta
    TBTR = getBank(&bcs_, "TBTR");
    if (tbid[i].track && TBTR)
    {
        tdpl_t *tdpl = tbtr2tdpl(&(TBTR->tbtr[TBID->bid[i].track -1]));
        if (TBID->bid[i].sc.stat)
        {
            pathlen = pathlen2sc(&(TBID->bid[i]), (hdpl_t*)tdpl);
            pathtime = TBID->bid[i].sc.time - TBID->bid[i].vtime;

            /// RF corrected
            TBID->bid[i].beta = 1.e9 * (pathlen / pathtime) / SPEED_OF_LIGHT;

            /// NO RF correction
            TBID->bid[i].sc.beta = (((pathlen2sc(&(TBID->bid[i]), (hdpl_t*)tdpl))/
                (TBID->bid[i].sc.time - TBID->bid[i].sc.vtime))/SPEED_OF_LIGHT)*1E9;
        }
        else if (TBID->bid[i].ec.stat)
        {
            pathlen = tdpl[ForwardECPlane].tlen;
            pathtime = TBID->bid[i].ec.time - TBID->bid[0].sc.vtime;

            TBID->bid[i].beta = 1.e9 * (pathlen / pathtime) / SPEED_OF_LIGHT;
        }
        else if (TBID->bid[i].lac.stat)
        {
            pathlen = tdpl[LACPlane].tlen;
            pathtime = TBID->bid[i].lac.time - TBID->bid[0].sc.vtime;

            TBID->bid[i].beta = 1.e9 * (pathlen / pathtime) / SPEED_OF_LIGHT;
        }
    }
    /**
     * if it's neutral and the first tbid is charged
     **/
    else if (TBID->bid[0].track && !TBID->bid[i].track)
    {
        if (ECHB = getBank(&bcs_, "ECHB"))
        {
            int ecid=TBID->bid[i].ec.id -1;
            if (ecid>=0 && TBID->bid[i].ec.time > TBID->bid[0].sc.vtime)
            {
                /**
                 * target position must be taken into account
                 **/
                neutral_path_length = sqrt(SQUARE(ECHB->echb[ecid].x_hit - beam_position.x)
                    + SQUARE(ECHB->echb[ecid].y_hit - beam_position.y)
                    + SQUARE(ECHB->echb[ecid].z_hit - target_z()));

                TBID->bid[i].ec.beta = ((neutral_path_length/
                    (TBID->bid[i].ec.time-TBID->bid[i].sc.vtime))/SPEED_OF_LIGHT)*1E9;
                TBID->bid[i].beta = ((neutral_path_length/
                    (TBID->bid[i].ec.time-TBID->bid[i].vtime))/SPEED_OF_LIGHT)*1E9;

            }
        }
        if (EC1R=getGroup(&bcs_, "EC1R",TBID->bid[i].sec))
        {
            if (TBID->bid[i].lac.time>TBID->bid[0].sc.vtime)
            {
                TBID->bid[i].lac.beta=v3mag(EC1R->ec1r[TBID->bid[i].lac.id-1].pos)/
                    (TBID->bid[i].lac.time-TBID->bid[i].sc.vtime)/SPEED_OF_LIGHT*1E9;
                /**
                 * only fill in beta here if not done for EC
                 **/
                if (TBID->bid[i].beta <= 0.)
                {
                    TBID->bid[i].beta=v3mag(EC1R->ec1r[TBID->bid[i].lac.id-1].pos)/
                        (TBID->bid[i].lac.time-TBID->bid[i].vtime)/SPEED_OF_LIGHT*1E9;
                }
            }
        }
    }
}
            return(TBID);
        }
    }
    return(0);
}


float get_sc_energy(tbid_t *tbid, tbtr_t *tbtr){
  clasSCRC_t *SCRC=NULL;
  if (SCRC = getGroup(&bcs_, "SCRC ", tbtr->itr_sec/100)){
    return (SCRC->scrc[(tbid->sc_id) -1].energy);
  }
  return (0);
}

float get_sc_mass(tbid_t *tbid, tbtr_t *tbtr){
  float mass, gamma;

  if ((tbid->beta > 0) && (tbid->beta < 1)){
    gamma = beta2gamma(tbid->beta);
    mass = (v3mag(tbtr->p))/(gamma*(tbid->beta));
    return mass;
  }
  return 0;
}
/*---------------------------------------------------------------------------*/
int sort_tbid( const void *tbid1 , const void *tbid2)
{
  clasTBTR_t *TBTR = NULL;
  tbid_t *h1 = (tbid_t *) tbid1;
  tbid_t *h2 = (tbid_t *) tbid2;
  int ret;

  /*tracks on top*/
  if (h1->track && !h2->track)
    ret = QS_NO_SWAP;
  else if (!h1->track && h2->track)
    ret = QS_SWAP;
  /*sort by sc status*/
  else if (h1->sc_stat > h2->sc_stat)
    ret = QS_NO_SWAP;
  else if (h1->sc_stat < h2->sc_stat)
    ret = QS_SWAP;
  /* now sort by CC status */
  else if (h1->cc_stat > h2->cc_stat)
    ret = QS_NO_SWAP;
  else if (h1->cc_stat < h2->cc_stat)
    ret = QS_SWAP;
  /* now sort by EC status */
  else if (h1->ec_stat > h2->ec_stat)
    ret = QS_NO_SWAP;
  else if (h1->ec_stat < h2->ec_stat)
    ret = QS_SWAP;
  else {
    if(h1->track && h2->track && (TBTR = getBank(&bcs_, "TBTR"))){
      /* now sort by charge*/
      if(TBTR->tbtr[h1->track - 1].q < TBTR->tbtr[h2->track - 1].q)
    ret = QS_NO_SWAP;
      else if(TBTR->tbtr[h1->track - 1].q > TBTR->tbtr[h2->track - 1].q)
    ret= QS_SWAP;
      else {

    /*now sort by momentum*/
    if(v3mag(TBTR->tbtr[h1->track - 1].p) >
        v3mag(TBTR->tbtr[h2->track - 1].p))
      ret = QS_NO_SWAP;
    else if(v3mag(TBTR->tbtr[h1->track - 1].p) <
        v3mag(TBTR->tbtr[h2->track - 1].p))
      ret =  QS_SWAP;
    else {
    /* sort by sc time. For B=0 where p and q are assigned */
    if(h1->sc_time < h2->sc_time)
      ret = QS_NO_SWAP;
    else if (h1->sc_time > h2->sc_time)
      ret = QS_SWAP;
    else
      ret = 0;
    }
      }
    } else  /* no HBTR bank */
      ret = 0;
  }

  return ret;

}
/*---------------------------------------------------------------------------*/
