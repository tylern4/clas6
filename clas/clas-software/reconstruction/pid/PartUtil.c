/* -=======================================================- *
$Id: PartUtil.c,v 1.29 2008/04/15 19:14:05 weygand Exp $
$Author: weygand $
$Revision: 1.29 $
$Date: 2008/04/15 19:14:05 $
* -=======================================================- */

#include <float.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ntypes.h>
#include <bostypes.h>
#include <math.h>
#include <time.h>
#include <bosddl.h>
#include <kinematics.h>
#include <ec.h>
#include <sc.h>
#include <utility.h>
#include <pdgutil.h>
#include <pid.h>
#include <particleType.h>
#include <PartUtil.h>

/*----------------------------------------------------------------------------------------------*/
tbid_t * part2tbid(part_t *part, int GroupNo){
  /* given a pointer to part return a pointer to tbid*/
  clasTBID_t *TBID = (clasTBID_t *)getGroup(&bcs_, "TBID", GroupNo);
  if (TBID) return(&TBID->tbid[part->trkid - 1]);
  return NULL;
}
/*----------------------------------------------------------------------------------------------*/
tdpl_t *tbid2tdpl(tbid_t *tbid){
  clasTBTR_t *TBTR = getBank(&bcs_, "TBTR");
  tdpl_t *tdpl = tbtr2tdpl(&(TBTR->tbtr[tbid->track -1]));
  return(tdpl);
}
/*----------------------------------------------------------------------------------------------*/
cc01_t * tbid2cc01(tbid_t *tbid){
  clasCC01_t *CC01 = getGroup(&bcs_, "CC01", tbid->sec);
  return ( &(CC01->cc01[tbid->cc_id -1]));
}
/*----------------------------------------------------------------------------------------------*/
scrc_t *tbid2scrc(tbid_t *tbid){
  clasSCRC_t *SCRC = getGroup(&bcs_, "SCRC", tbid->sec);
  return ( &(SCRC->scrc[tbid->sc_id -1]));
}
/*----------------------------------------------------------------------------------------------*/
tbtr_t *part2tbtr(part_t *part, int GroupNo){
  /* given a pointer to part return a pointer to tbtr*/
  tbid_t *tbid = part2tbid(part, GroupNo);
  clasTBTR_t *TBTR = getBank(&bcs_, "TBTR");
  if (TBTR && tbid) return(&TBTR->tbtr[tbid->track -1]);
  return (NULL);
}
/*----------------------------------------------------------------------------------------------*/
int ec_get_energy(part_t * part, int GroupNo, ecHit_t * ec_e){

  /*
  purpose: Given a pointer to part bank, return the ec energy of that hit
           Returns 0 if that part was not matched to ec.
  */

  clasECHB_t * echb;
  echb_t * WholeHit;
  echb_t * InHit = NULL;
  echb_t * OutHit = NULL;
  clasBID_t * tbid;
  clasHEAD_t *HEAD;

  /* entry contidions:
     1) part must match to ec
     2) ECHB and TBID banks must exist */
  /* tbid[...].ec_id will be zero if this track is not matched to ec */
  if(!(echb =   (clasECHB_t *)getBank(&bcs_,"ECHB")) ||
     !(tbid =   (clasBID_t *)getGroup(&bcs_, "TBID", GroupNo)) ||
     !(tbid->bid[part->trkid-1].ec.id)               )
    return 0;

  /* part->trkid points to tbid */
  WholeHit = &echb->echb[tbid->bid[part->trkid-1].ec.id-1];

  ec_e->Whole = WholeHit->e__hit;
  if(ec_Whole2InOut(WholeHit, &InHit, &OutHit)){
    ec_e->Inner = InHit  ? InHit->e__hit  : 0.0;
    ec_e->Outer = OutHit ? OutHit->e__hit : 0.0;
  }
  else{
    /* Sometimes there is a hit in ECHB that does not have a corresponding inner or outer match.
       I guess this is supposed to happen */
    return 0;
  }

  return 1;
}
/*----------------------------------------------------------------------------------------------*/
float sc_get_energy(part_t * part, int GroupNo){

  /*
  purpose: Given a pointer to part bank, return the sc energy of that hit.
           Returns -9.9 if that part was not matched to the sc.
  */

  clasBID_t * tbid;
  clasSCRC_t * scrc;

  /* entry conditions :
     1) TBID and SCR banks must be present
     2) Must be a match to sc
     */
  if(tbid=(clasBID_t *)getGroup(&bcs_, "TBID", GroupNo)){
    int tbid_ind = part->trkid-1;
    if ((scrc=getGroup(&bcs_, "SCRC", tbid->bid[tbid_ind].sec)) &&
    (tbid->bid[tbid_ind].sc.stat)){
      int scr_ind = tbid->bid[tbid_ind].sc.id-1;
      return(scrc->scrc[scr_ind].energy);
    }
  }
  return(-9.9);

}
/*----------------------------------------------------------------------------------------------*/
float sc_get_tof(part_t * part, int GroupNo){

  /*
  purpose: Given a pointer to part bank, return the sc beta of that hit.
           Returns -9.9 if that part was not matched to the sc or if there's no TBID bank.
  */

  clasBID_t * tbid;

  /* entry conditions :
     1) TBID and bank must be present
     2) Must be a match to sc
     */
  if(!(tbid=(clasBID_t *)getGroup(&bcs_, "TBID", GroupNo)) ||
     !(tbid->bid[part->trkid-1].sc.id)             )
    return -9.9;

  return tbid->bid[part->trkid-1].sc.time - tbid->bid[part->trkid-1].sc.vtime ;
}
/*----------------------------------------------------------------------------------------------*/
#define MIN_THET   4.5        /* don't assume these are correct for all field settings */
#define MAX_THET 145.0
#define COIL_PHI 3.0          /* width of coils*/

int Accept(part_t *part){
  /* provide crude acceptance  */
  int Ret=1;
  float Theta, Phi, PhiInSector;

  if(fabs(part->q) > 0.1){                                        /* only for charged particles */
    v3dir_deg(part->p.space, &Theta, &Phi);

    /* get phi in the sector */
    /* shift by 30 so that a sector is centered at 30 degs */
    PhiInSector = Phi + 30.0;
    PhiInSector -= (int)((PhiInSector)/60)*60.0;

    Ret = (((Theta > MIN_THET) && (Theta < MAX_THET)) &&
       ((PhiInSector > COIL_PHI) && (PhiInSector < 60.0 - COIL_PHI))) ;
  }
  return Ret;
}
/*----------------------------------------------------------------------------------------------*/
int AcceptVB(part_t *Part, int RunNo, float TorusCurrent){
  /* PID C wrapper for pseudo_spa.f */
  int ElecTron  =  0,
      PosHadron =  1,
      NegHadron = -1,
      UnKnown   =  2,
      Sector,
      Dec97Run = 0,
      ParticleClass = UnKnown,
      IAccept;
  float Mo, Theta, Phi,
        PhiCut = 1.0,
        Accept;

  Sector = get_sector(&Part->p);
  Mo = v3mag(Part->p.space);
  v3dir_deg(Part->p.space, &Theta, &Phi);
  /* get phi in the sector.  This should range from -30.0 to 30.0 */
  Phi += 30.0;
  Phi -= (int)((Phi/60.0))*60.0;
  Phi -= 30.0;

  /* figure out the particle class.  These are the only things that pseudo_spa knows about.*/
  if(Part->pid == Electron)
    ParticleClass = ElecTron;
  else if((Part->pid == Proton) || (Part->pid == PiPlus) || (Part->pid == KPlus))
    ParticleClass = PosHadron;
  else if ((Part->pid == PiMinus) || (Part->pid == KMinus))
    ParticleClass = NegHadron;

  /*pseudo_spa_(&ParticleClass, &Mo, &Theta, &Phi, &TorusCurrent, &Constrained, &Accept); */
  if(ParticleClass != UnKnown)
    pseudo_spaz_(&ParticleClass, &Mo, &Theta, &Phi, &TorusCurrent, &PhiCut, &Accept);
  else
    Accept = 1.0;                           /* keep unknown partcles */

  IAccept = Accept > 0.0;
  /* now get rid of electrons for Dec97 run */
  Dec97Run = (7500 < RunNo) && (RunNo < 8111);;
  if(Dec97Run && (Part->pid == Electron))
    IAccept = IAccept && (Sector != 2) && (Sector != 3);

  return IAccept;
}

/* ---------- target center z position ---------------------------------*/

double target_z()
{

  clasTGEO_t *TGEO = getBank(&wcs_, "TGEO");
  clasHEAD_t *HEAD = getBank(&bcs_,"HEAD");
  int RunNo;
  double z = 0.0;
  if (TGEO) {
    z = TGEO->tgeo[0].z;

  }
  else {
    if (HEAD) {
      RunNo = HEAD->head[0].nrun;
      if (TGEO =  make_TGEO(RunNo)) {
     z = TGEO->tgeo[0].z;
      }
    }
  }

  return(z);
}

int get_avg_st_tpho(int nBID, bid_t* bid, int trk_level, float* st_avg_tpho, float* st_avg_tprop)
{
    /**
     * gets the average of all "good" (meaning they are
     * associated with tracks) ST tpho times
     * from the (T)BID bank
     *
     * this merely averages over all hits in the start counter
     * that are associated with tracks. It would be better
     * to order them in a list and take the first "bunch"
     * of hits as there may be two or more bunches separated
     * in time.
     **/
    
    clasTGEO_t* TGEO = (clasTGEO_t*) getBank(&wcs_, "TGEO");
    clasHBTR_t* HBTR = (clasHBTR_t*) getBank(&bcs_, "HBTR");
    clasTBTR_t* TBTR = (clasTBTR_t*) getBank(&bcs_, "TBTR");

    if ( !TGEO
        || (trk_level == HIT_BASED  && !HBTR)
        || (trk_level == TIME_BASED && !TBTR) )
    {
        *st_avg_tpho  = BAD_VERTEX_TIME;
        *st_avg_tprop = BAD_VERTEX_TIME;
        return 1;
    }

    int i;
    int trk_ind;
    float tprop;
    int ngoodhits = 0;
    *st_avg_tpho = 0.;
    *st_avg_tprop = 0.;
    for (i = 0; i < nBID; i++)
    {
        trk_ind = bid[i].track - 1;
        if (trk_level == HIT_BASED)
        {
            tprop = 1.e9
                * (HBTR->hbtr[trk_ind].vert.z - TGEO->tgeo[0].z)
                / SPEED_OF_LIGHT;
        }
        else /// trk_level == TIME_BASED
        {
            tprop = 1.e9
                * (TBTR->tbtr[trk_ind].vert.z - TGEO->tgeo[0].z)
                / SPEED_OF_LIGHT;
        }
        if (bid[i].st.stat > 0)
        {
            ngoodhits++;
            *st_avg_tpho += bid[i].st.vtime - tprop;
            *st_avg_tprop += tprop;
        }
    }
    if (ngoodhits)
    {
        *st_avg_tpho /= (float) ngoodhits;
        *st_avg_tprop /= (float) ngoodhits;
        return 0;
    }
    else
    {
        *st_avg_tpho = BAD_VERTEX_TIME;
        *st_avg_tprop = BAD_VERTEX_TIME;
        return 1;
    }
}


/* The prototype for this function is different from the get_avg_st_tpho to 
 * remind me that I cannot use get_avg_tof_tpho in the reconstruction. The
 * values used in this function are not yet in place when I pick the vertex time
 * during the first pass of reconstruction.
 */
int get_avg_tof_tpho(clasBID_t *BID, int trk_level, float* tof_avg_tpho, float* tof_avg_tprop)
{
    /**
     * gets the average of all "good" (meaning they are
     * associated with tracks) TOF tpho times
     * from the (T)BID bank
     *
     * this merely averages over all hits in the TOF
     * that are associated with tracks. It would be better
     * to order them in a list and take the first "bunch"
     * of hits as there may be two or more bunches separated
     * in time.
     **/

    clasTGEO_t* TGEO = (clasTGEO_t*) getBank(&wcs_, "TGEO");
    clasHBTR_t* HBTR = (clasHBTR_t*) getBank(&bcs_, "HBTR");
    clasTBTR_t* TBTR = (clasTBTR_t*) getBank(&bcs_, "TBTR");

    if ( !TGEO
        || (trk_level == HIT_BASED  && !HBTR)
        || (trk_level == TIME_BASED && !TBTR) )
    {
        *tof_avg_tpho  = BAD_VERTEX_TIME;
        *tof_avg_tprop = BAD_VERTEX_TIME;
        return 1;
    }

    int i;
    int trk_ind;
    float tprop;
    int ngoodhits = 0;
    *tof_avg_tpho = 0.;
    *tof_avg_tprop = 0.;
    for (i = 0; i < BID->bank.nrow; i++)
    {
        trk_ind = BID->bid[i].track - 1;
        if (trk_level == HIT_BASED)
        {
            tprop = 1.e9
                * (HBTR->hbtr[trk_ind].vert.z - TGEO->tgeo[0].z)
                / SPEED_OF_LIGHT;
        }
        else /// trk_level == TIME_BASED
        {
            tprop = 1.e9
                * (TBTR->tbtr[trk_ind].vert.z - TGEO->tgeo[0].z)
                / SPEED_OF_LIGHT;
        }
        if (BID->bid[i].sc.stat > 0)
        {
            ngoodhits++;
            *tof_avg_tpho += BID->bid[i].sc.vtime - tprop;
            *tof_avg_tprop += tprop;
        }
    }
    if (ngoodhits)
    {
        *tof_avg_tpho /= (float) ngoodhits;
        *tof_avg_tprop /= (float) ngoodhits;
        return 0;
    }
    else
    {
        *tof_avg_tpho = BAD_VERTEX_TIME;
        *tof_avg_tprop = BAD_VERTEX_TIME;
        return 1;
    }
}


int get_closest_tagr_hit(clasTAGR_t *TAGR, float tpho)
{
    /**
     * for each tagger hit
     * get the closest hit (tpho) to the avg_st_tpho
     * or if no ST hits, to the avg_tof_tpho
     *
     * the TAGR hit chosen is swapped with the first
     * hit in the bank. An improvement to this would
     * involve ordering all the TAGR hits based on this
     * difference in time.
     **/
    int i;
    int tagr_id = -1;
    float diff;
    float best_diff = FLT_MAX;
    for (i = 0; i < TAGR->bank.nrow; i++)
    {
        if (TAGR->tagr[i].stat==7 || TAGR->tagr[i].stat==15)
        {
            diff = fabs(tpho - TAGR->tagr[i].tpho);
            if (diff<best_diff)
            {
                best_diff = diff;
		tagr_id = i;
                /// swap TAGR[i] with TAGR[0]
                /*
                tagr_t* tagr = NULL;
                tagr_t tagr_temp;
                tagr_temp = TAGR->tagr[0];
                TAGR->tagr[0] = TAGR->tagr[i];
                TAGR->tagr[i] = tagr_temp;
                tagr = &(TAGR->tagr[0]);
                */
            }
        }
    }
    
    return tagr_id;
}

/**
 * gets the tagger hit that is closest in time to
 * the average of start counter hits
 * (or TOF if ST doesn't have hits)
 * associated with tracks in the (H|T)BID bank given.
 *
 * trk_level:
 *  HIT_BASED || TIME_BASED
 **/
tagr_t* get_photon_tagr(clasTAGR_t *TAGR, clasBID_t *BID, int trk_level)
{
    int tagr_id = 0;
    tagr_t* tagr = NULL;

    /**
     * Exit from function if missing the requisite banks.
     *
     * The test for a TAGR bank is equivalent to testing
     * if this is a photon run or not.
     **/
    if (!TAGR || !BID)
    {
        return(NULL);
    }

    float st_avg_tpho;
    float st_avg_tprop;
    float tof_avg_tpho;
    float tof_avg_tprop;

    if (get_avg_st_tpho(BID->bank.nrow, BID->bid, trk_level, &st_avg_tpho, &st_avg_tprop) == 0)
    {
        tagr_id = get_closest_tagr_hit(TAGR, st_avg_tpho);
        tagr = &(TAGR->tagr[tagr_id]);
    }      
    else if (get_avg_tof_tpho(BID, trk_level, &tof_avg_tpho, &tof_avg_tprop) == 0)
    {
	  tagr_id = get_closest_tagr_hit(TAGR, tof_avg_tpho);
	  tagr = &(TAGR->tagr[tagr_id]);
    }      
    

    return(tagr);
}


/**
 * Sorts the TAGR bank by smallest fabs(tagr->tpho - det_tpho),
 * i.e. upon completion, the trigger photon is TAGR->tagr[0], 
 * the next-best photon is TAGR->tagr[1], etc...
 */
void sort_tagr_hits_dt(clasTAGR_t* TAGR, int low, int high, float det_tpho) {
  if (low >= high)
    return;

  float pivot_diff = fabs(TAGR->tagr[high].tpho - det_tpho); 

  if ((TAGR->tagr[high].stat != 7) && (TAGR->tagr[high].stat != 15))
    pivot_diff = FLT_MAX;

  int pivot_idx  = high;
  int left_idx   = low;
  int i;

  for (i=low; i <=high; i++) {
    float this_diff = fabs(TAGR->tagr[i].tpho - det_tpho);
    if ((TAGR->tagr[i].stat != 7) && (TAGR->tagr[i].stat != 15))
      this_diff = FLT_MAX;

    if (this_diff >= pivot_diff)
      continue;

    tagr_t tmp = TAGR->tagr[left_idx];
    TAGR->tagr[left_idx] = TAGR->tagr[i];
    TAGR->tagr[i] = tmp;
    left_idx++;
  }

  tagr_t tmp = TAGR->tagr[high];
  TAGR->tagr[high] = TAGR->tagr[left_idx];
  TAGR->tagr[left_idx] = tmp;

  sort_tagr_hits_dt(TAGR, low, left_idx - 1, det_tpho);
  sort_tagr_hits_dt(TAGR, left_idx + 1, high, det_tpho);
}

tagr_t *get_photon_tagrEcut(clasTAGR_t *TAGR,clasBID_t *BID,double E0,double E1){
  /* This routine only works for time-based tracks! */
  float best_diff=ST_TAG_COINCIDENCE_WINDOW;
  float tprop=0.0;
  tagr_t *tagr = NULL;
  clasTBTR_t *TBTR=getBank(&bcs_,"TBTR");
  clasTGEO_t *TGEO = getBank(&wcs_, "TGEO");
  clasRUNC_t *RUNC = getBank(&wcs_, "RUNC");
  int i, j;

  /* Exit from function if missing the requisite banks... */
  if(!TAGR || !TBTR || !BID || !TGEO ||!RUNC) return(NULL);

  for (i=0;i<BID->bank.nrow;i++){
    int trk_ind=BID->bid[i].track;
    if(trk_ind){
      tprop=(TBTR->tbtr[trk_ind-1].vert.z - TGEO->tgeo[0].z)/SPEED_OF_LIGHT*1e9;
      if (RUNC->runc.beam.type.val.i[0]==PHOTON_RUN ||
      RUNC->runc.beam.type.val.i[0]==G11_RUN){
    if (BID->bid[i].st.stat){
      for(j=0;j<TAGR->bank.nrow;j++){
        if (TAGR->tagr[j].erg >= E0 && TAGR->tagr[j].erg <= E1) {
          float diff=fabs(BID->bid[i].st.vtime-(TAGR->tagr[j].tpho+tprop));
          if (diff<ST_TAG_COINCIDENCE_WINDOW&&diff<best_diff
          && (TAGR->tagr[j].stat==7 || TAGR->tagr[j].stat==15)){
        best_diff=diff;
        tagr=&(TAGR->tagr[j]);
          }
        } // E cut
          }  // loop over tagr
    }
      }


    }
  } // loop over BID
  if (tagr)
    return(tagr);
  else
    return(get_photon_tagr(TAGR,BID,TIME_BASED));

}

float get_photon_energy(clasTAGR_t *TAGR,clasBID_t *BID){
  tagr_t *tagr = get_photon_tagr(TAGR, BID, TIME_BASED);
  float photon_energy = 0.0;
  if (tagr) photon_energy = tagr->erg;
  return(photon_energy);
}


vector4_t get_photon_v4(clasTAGR_t *TAGR, clasBID_t *BID){
  vector4_t photon = {0.0, 0.0, 0.0, 0.0};
  if (!BID && TAGR){ /* handle part 0,  MC situations*/
    photon.t = photon.space.z = TAGR->tagr[0].erg;
  } else {
    photon.space.z =  photon.t = get_photon_energy(TAGR, BID);
  }
  return(photon);
}

