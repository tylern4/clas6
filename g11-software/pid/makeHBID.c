/* -=======================================================- *
$Id: makeHBID.c,v 1.44 2004/07/04 16:21:49 devita Exp $
$Author: devita $
$Revision: 1.44 $
$Date: 2004/07/04 16:21:49 $
* -=======================================================- */

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
#include <particleType.h>

/*--------------------------- M A C R O S -----------------------------------*/
#define CC_MATCH_CUT 10.0      /* for trk-cc matching; in degrees */
#define SC_MATCH_CUT 30.0     /* for trk-sc matching; in cm      */
/*       see KILL_RADIUS in sc.h for trk-ec matching             */

#define VERTEX_DIFF 5.0 /* for vertex matching */

/*---------------------------------------------------------------------------*/
int make_hbid_bank_(){
  int ret = 0;
  if (make_HBID_bank()) ret =1;
  return ret;
}
/*---------------------------------------------------------------------------*/
clasBID_t *make_HBID_bank(){
  /*input: HBTR, HDPL, and SCR banks
    output: HBID bank */
  clasRUNC_t *RUNC=getBank(&wcs_,"RUNC");
  clasTGEO_t *TGEO = getBank(&wcs_, "TGEO");         
  clasHDPL_t *HDPL=NULL;
  clasSCRC_t *SCRC=NULL;
  clasHBTR_t *HBTR=NULL;
  clasBID_t *HBID=NULL;
  clasCC01_t *CC01=NULL;
  clasECHB_t *ECHB=NULL;
  clasEC1R_t *EC1R=NULL; /* LAC result bank */
  clasTAGR_t *TAGR=getBank(&bcs_,"TAGR");
  clasSTR_t *STR=NULL;
  bid_t *hbid = NULL;
  int nHBID = 0, i = 0, ntrk;
  int sort_hbid(const void *hbid1, const void *hbid2);
  float vertex_time = 0.;
  int beam_type = 0;
  int trk_sec; 
  int current_sector; 
  hdpl_t *hdpl;

  if ( (HBTR = (clasHBTR_t *)getBank(&bcs_, "HBTR")) ){
    /* HBID bank is always the same size as HBTR*/
    hbid = (bid_t *)malloc(sizeof(bid_t)*HBTR->bank.nrow);
    memset((void *)hbid,0, sizeof(hbid_t)*HBTR->bank.nrow);

    /* loop through all the tracks and match to all the detectors*/
    current_sector = 0;
    for (ntrk=0; ntrk < HBTR->bank.nrow; ntrk++){
      hbid[nHBID].track = ntrk+1;
      hbid[nHBID].sec = HBTR->hbtr[ntrk].itr_sec/100;
      if (hbid[nHBID].sec != current_sector) {
	current_sector = hbid[nHBID].sec;
	trk_sec = 1;
      }
      else trk_sec++;

      if (SCRC = getGroup(&bcs_,"SCRC", (HBTR->hbtr[ntrk].itr_sec/100))){
	match_to_tof(hbtr2hdpl(&(HBTR->hbtr[ntrk])), SCRC, &(hbid[nHBID].sc));
      } 
      if (STR = getGroup(&bcs_,"STR ",0)){
	match_to_st(hbtr2hdpl(&(HBTR->hbtr[ntrk])),STR,&(hbid[nHBID]),trk_sec);
      }
      if (CC01 = getGroup(&bcs_,"CC01", (HBTR->hbtr[ntrk].itr_sec/100))){
	match_to_CC01(hbtr2hdpl(&(HBTR->hbtr[ntrk])), CC01, &(hbid[nHBID].cc));
      }
      if (ECHB = getBank(&bcs_, "ECHB"))
	match_to_ec(hbtr2hdpl(&(HBTR->hbtr[ntrk])), hbid[nHBID].sec, ECHB, &(hbid[nHBID].ec));

      if (EC1R = getGroup(&bcs_, "EC1R",(HBTR->hbtr[ntrk].itr_sec/100)))
	match_to_lac(hbtr2hdpl(&(HBTR->hbtr[ntrk])), hbid[nHBID].sec, EC1R, &(hbid[nHBID].lac));
      nHBID++;
    }
    if(nHBID){
      /*sort by detector hit, charge and momentum*/
      qsort((void *)hbid, nHBID, sizeof(bid_t), sort_hbid);
      if (HBID = makeBank(&bcs_, "HBID", 0, sizeof(hbid_t)/4, nHBID)){	
	HBID->bid[0] = hbid[0]; /*copy into bank*/	
	if(RUNC){

	  if(RUNC->runc.beam.type.val.i[0]==PHOTON_RUN || 
	     RUNC->runc.beam.type.val.i[0]== G7_RUN ||
	     RUNC->runc.beam.type.val.i[0]== G2B_RUN ||
	     RUNC->runc.beam.type.val.i[0]== G11_RUN){/* photon-beam case or g7, or g2b*/
	    if (RUNC->runc.beam.type.val.i[0]==PHOTON_RUN)
	      vertex_time = get_vertex_time(nHBID,hbid,TAGR,HIT_BASED);
	    else if (RUNC->runc.beam.type.val.i[0]==G7_RUN)
	      vertex_time = get_vertex_timeG7(nHBID,hbid,TAGR,HIT_BASED);
	    else if (RUNC->runc.beam.type.val.i[0]==G2B_RUN)
	      vertex_time = get_vertex_timeG2b(nHBID,hbid,TAGR,HIT_BASED);
	    else if (RUNC->runc.beam.type.val.i[0]==G11_RUN)
	      vertex_time = get_vertex_time(nHBID,hbid,TAGR,HIT_BASED);

	    hdpl = hbtr2hdpl(&(HBTR->hbtr[HBID->bid[0].track -1]));

	    HBID->bid[0].vtime = vertex_time;
	    HBID->bid[0].beta=((pathlen2sc(&(HBID->bid[0]), hdpl)/
				(HBID->bid[0].sc.time - vertex_time))
			       /SPEED_OF_LIGHT)*1E9;		    
	  }
	  /*Electron case */
	  else if (HBID->bid[0].sc.stat){
	    hdpl = hbtr2hdpl(&(HBTR->hbtr[HBID->bid[0].track -1]));
	    HBID->bid[0].sc.beta = HBID->bid[0].beta = 1.0;
	    HBID->bid[0].sc.vtime = HBID->bid[0].sc.time -
	      (pathlen2sc( &(HBID->bid[0]), hdpl) / SPEED_OF_LIGHT)*1E9;
	    HBID->bid[0].vtime = rf_corr_time(HBID->bid[0].sc.vtime) -
	      (HBTR->hbtr[HBID->bid[0].track -1].vert.z-TGEO->tgeo[0].z)/(SPEED_OF_LIGHT/1E9);
	  }
	}
	/*now do all the other particles*/
	for(i=1; i<nHBID; i++) {
	  hdpl = hbtr2hdpl(&(HBTR->hbtr[hbid[i].track -1]));	 
	  HBID->bid[i] = hbid[i]; /*copy into bank*/
	  HBID->bid[i].vtime = HBID->bid[0].vtime;
	  HBID->bid[i].sc.vtime = HBID->bid[0].sc.vtime;
	  if(hdpl){
	    /* If the track was matched to a scintillator, use the sc time
	       to calculate beta */
	    if (HBID->bid[i].sc.stat){
	      HBID->bid[i].beta = ((pathlen2sc(&(HBID->bid[i]), hdpl)/
				    (HBID->bid[i].sc.time - HBID->bid[i].vtime))/SPEED_OF_LIGHT)*1E9;
	      HBID->bid[i].sc.beta = ((pathlen2sc(&(HBID->bid[i]), hdpl)/
				       (HBID->bid[i].sc.time - HBID->bid[i].sc.vtime))/SPEED_OF_LIGHT)*1E9;
	    }
	    /* Otherwise, use the ec time ... */
	    else if(HBID->bid[i].ec.stat){
	      HBID->bid[i].beta = 
		((hdpl[ForwardECPlane].tlen/
		  (HBID->bid[i].ec.time - HBID->bid[0].vtime))/SPEED_OF_LIGHT)
		*1E9;
	    }
	    /* ... and if that doesn't work, use the large angle calorimeter */
	    else if(HBID->bid[i].lac.stat){
	      HBID->bid[i].beta = 
		((hdpl[LACPlane].tlen/
		  (HBID->bid[i].lac.time - HBID->bid[0].vtime))/SPEED_OF_LIGHT)*1E9;
	    }

	  }
	} 
	
      }
    }
    free(hbid);
  }
  return(HBID);
}
/*---------------------------------------------------------------------------*/
float pathlen2sc(bid_t *bid, hdpl_t *hdpl)
{
  /* works with both hbid and tbid banks*/
  int id;
  clasSCRC_t * SCRC = getGroup(&bcs_, "SCRC", bid->sec);
  clasSCG_t * SCG  = getGroup(&wcs_, "SCG ", bid->sec);
  if (SCRC && bid && SCG && hdpl) { 
    if (bid->sc.id > 0) {
      id = SCRC->scrc[bid->sc.id -1].id;
      return(hdpl[SCG->scg[id -1].panel+3].tlen);
    }
  }
  return(0);
}
/*---------------------------------------------------------------------------*/
int match_to_CC01(hdpl_t *hdpl, clasCC01_t *CC01, match_t *match){
  vector3_t hit_trk, hit_cc, dist;
  int i, plane;
  
  match->qual = 999999.0;
  match->id = 0;
  match->stat=0;
  match->time = 999999.0;

  if (hdpl && CC01 && match){
    if(hdpl[CCPlane].pos.x < 1000.0){ 
      for(i=0; i < CC01->bank.nrow; i++){
	float theta_trk = (RAD2DEG)*acos(hdpl[CCPlane].pos.z/v3mag(hdpl[CCPlane].pos));
	float diff = theta_trk - CC2theta(CC01->cc01[i].id);
	if (fabs(diff) < fabs(match->qual)){
	  match->qual = diff; /*keep the sign information*/
	  match->id = i + 1; /*FORTRAN style pointer*/
	  match->time = CC01->cc01[i].time;
	  match->stat = GOOD_MATCH;
	}
      }
    }
    if(fabs(match->qual) > CC_MATCH_CUT)
      match->stat = 0;
    return(match->stat);
  }
}

/*---------------------------------------------------------------------------*/
int match_to_st(hdpl_t *hdpl,clasSTR_t *STR,bid_t *bid, int trk_sec){
  int i;
  int sector;
  float speed;

  bid->st.id=0;
  if (hdpl && STR && bid){
    for (i=0; i<STR->bank.nrow;i++){
      /*      if((STR->str[i].id == bid->sec) && */
      if(STR->str[i].id>100) {
	sector=STR->str[i].id/100;
      } 
      else 
      {
	sector=STR->str[i].id;
      }
      if((STR->str[i].trk_no == trk_sec) && (sector == bid->sec) &&
	 (bid->sc.stat>1)){
	bid->st.stat=1;
	bid->st.id=i+1;
	bid->st.time=STR->str[i].st_time;
	speed=(pathlen2sc(bid,hdpl) - STR->str[i].st_l)
	  /(bid->sc.time - bid->st.time);
	bid->st.beta=speed/SPEED_OF_LIGHT*1e9;
	bid->st.vtime=bid->st.time - STR->str[i].st_l/speed;	
      }
    }
  }
  return(bid->st.stat);
}



/*---------------------------------------------------------------------------*/
int match_to_tof(hdpl_t *hdpl, clasSCRC_t *SCRC, match_t *match){
  vector3_t hit_trk, hit_sc, dist;
  int i, plane;
  clasSCG_t * SCG  = getGroup(&wcs_, "SCG ", SCRC->bank.nr);

  match->qual = 999999.0;
  match->id = 0; 
  match->stat=0;
  match->time = 999999.0;

  if (hdpl && SCRC && match && SCG){
    for (i=0; i< SCRC->bank.nrow; i++){
      int id = SCRC->scrc[i].id - 1;
      int sc_plane = SCG->scg[id].panel+3; /* no nested []'s in AIX !!!!*/
      vector3_t diff = v3sub(SCRC->scrc[i].pos,hdpl[sc_plane].pos);
      if(fabs(diff.z) <= fabs(match->qual)){
	match->qual = diff.z;
	match->id = i + 1; /*FORTRAN style pointer*/
        match->time = SCRC->scrc[i].time;
	match->stat = GOOD_MATCH;
      }
    }
    
    if(fabs(match->qual) > SC_MATCH_CUT)
      match->stat = 0;
    return(match->stat);
  }
}


/*--------------------------------------------------------------------------*/
int match_to_lac(hdpl_t *hdpl,int TrkSector,clasEC1R_t *EC1R, match_t *match){
  float Diff;               
  int i;
  vector3_t trk_xyz; /* intersection point of track with the lac plane */

  match->qual = 999999.0;
  match->id = 0;
  match->time = 999999.0;
  match->stat = 0;
  if (hdpl && EC1R && match){
    for(i=0;i<EC1R->bank.nrow;i++){
      /* Rotate the track projection to the lac plane into the lab coordinate
	 system */
      trk_xyz=sector2lab(hdpl[LACPlane].pos,TrkSector);
      Diff = v3mag(v3sub(trk_xyz,EC1R->ec1r[i].pos));
      if (Diff < match->qual){
	match->qual = Diff;
	match->id = i + 1;
	match->time = EC1R->ec1r[i].t_tot;
	match->stat = GOOD_MATCH;
      }
    } 
    if(match->qual > KILL_RADIUS)
      match->stat = 0;
    return (match->stat);
  }
}

/*---------------------------------------------------------------------------*/
int match_to_ec(hdpl_t *hdpl, int TrkSector, clasECHB_t *ECHB, match_t *match){

  /* ECHB bank caveat!! :  */
  /* The track intersection is reported in the hdpl bank on the inner surface of the calorimeter, 
     but the WHOLE ec hit location is kind of in the middle of the inner layer (why does it have 
     to be so damn illogical?)
     The INNER layer EC hit location, is on the face of the calorimeter. 
     So to do matching, we use the INNER hit location if it is available (which it is most of the 
     time).   */

  /* (SJT 9/15/98) If there is no INNER hit, I use the WHOLE hit location and
     project the track to the whole plane.  I am using the same matching 
     radius. */

     /*     See the routine ec/ec_set_def for the definitions of the planes that the ec hit positions
     correspond to, and don't be surprised if it changes underneath you.
   */

  int i, Sector;
  int ECLayer;                 /* either INNER, OUTER or WHOLE */
  float Diff;
  vector3_t diff_vector={0.0,0.0,0.0},layer_displacement={0.0,0.0,0.0};
  vector3_t TrkECIntersect; /* intersection point of the track with the ec plane */
  vector3_t ECHitPosition;
  echb_t *ECIn, *ECOut;

  match->qual = 999999.0;
  match->id = 0;
  match->time = 999999.0;
  match->stat = 0;

  if (hdpl && ECHB && match){
    for (i=0; i < ECHB->bank.nrow; i++){ 
      ec_Whole2InOut(&(ECHB->echb[i]), &ECIn, &ECOut);
      /* if there was no EC inner hit, blow up the matching radius */
      /* First, two entry test conditions: 1) same sector 2) WHOLE ec hit layer */
      /* see clasbanks.ddl for packing of ECHB.sect */
      if ((Sector  = echb2sector(&(ECHB->echb[i]))) != TrkSector)  continue;
      if ((ECLayer = echb2layer (&(ECHB->echb[i]))) != ECHB_WHOLE) continue;
   
      if (ECIn){
	ECHitPosition=lab2sector(xyz2v3(&(ECIn->x_hit)),Sector);
      }
      else{
	vector3_t dir=hdpl[ForwardECPlane].dir;
	
	layer_displacement=inner2whole_displacement(dir,Sector);
	ECHitPosition=lab2sector(xyz2v3(&(ECHB->echb[i].x_hit)),Sector);
      }
  
      TrkECIntersect = v3add(hdpl[ForwardECPlane].pos,layer_displacement);
      /* if there was no EC inner hit, use the whole hit location */
       /* rotate ECHB vector into the sector-based coordinate system */
      diff_vector= v3sub(ECHitPosition, TrkECIntersect); 
      Diff = v3mag(diff_vector);
      
      if(Diff < match->qual){
	match->qual = Diff;
	match->id = i + 1;
	match->time = ECHB->echb[i].t_hit;
	match->vtime =  match->time - (hdpl[ForwardECPlane].tlen/SPEED_OF_LIGHT)*1E9;
	/* set ec_stat according to a fiducial cut */
	if(ECHB->echb[i].path_u < EC_FIDUCIAL_CUT || 
	   ECHB->echb[i].path_v < EC_FIDUCIAL_CUT ||
	   ECHB->echb[i].path_w < EC_FIDUCIAL_CUT)   {
	  match->stat=NONFIDUCIAL_MATCH;
	}
	else 
	  match->stat=GOOD_MATCH;
      }
    }
    if(match->qual>KILL_RADIUS)
      match->stat = 0;
    return (match->stat);
  }
}


/*---------------------------------------------------------------------------*/
int sort_hbid( const void *hbid1 , const void *hbid2)
{
  clasHBTR_t *HBTR = NULL;
  hbid_t *h1 = (hbid_t *) hbid1;
  hbid_t *h2 = (hbid_t *) hbid2;
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
    if(h1->track && h2->track && (HBTR = getBank(&bcs_, "HBTR"))){
      /* now sort by charge*/
      if(HBTR->hbtr[h1->track - 1].q < HBTR->hbtr[h2->track - 1].q)
	ret = QS_NO_SWAP;
      else if(HBTR->hbtr[h1->track - 1].q > HBTR->hbtr[h2->track - 1].q)
	ret= QS_SWAP;
      else {
	
	/*now sort by momentum*/
	if(v3mag(HBTR->hbtr[h1->track - 1].p) > 
		v3mag(HBTR->hbtr[h2->track - 1].p))
	  ret = QS_NO_SWAP;
	else if(v3mag(HBTR->hbtr[h1->track - 1].p) < 
		v3mag(HBTR->hbtr[h2->track - 1].p))
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
/*given one track find its distance to the vertex*/
float dist_to_vertex(hdpl_t *hdpl){
  int plane;

  for(plane = 4; plane < 8; plane++){
    if(hdpl[plane].pos.x < 1000.0)
      return ( hdpl[plane].tlen);
  }

  return(-1.0);
}
/*---------------------------------------------------------------------*/
float get_vertex_time(int nBID,bid_t *bid,clasTAGR_t *TAGR,int trk_level){
  int i,j;
  static int count=0;
  float vert_time=BAD_VERTEX_TIME,tprop=0.0;
  float best_diff=ST_TAG_COINCIDENCE_WINDOW;
  clasHBTR_t *HBTR=getBank(&bcs_,"HBTR");
  clasTBTR_t *TBTR=getBank(&bcs_,"TBTR");
  clasTGEO_t *TGEO = getBank(&wcs_, "TGEO");

  /* Exit from function if missing the requisite banks... */
  if(!TAGR) return(BAD_VERTEX_TIME);
  if(trk_level==HIT_BASED && !HBTR) return(BAD_VERTEX_TIME);
  if(trk_level==TIME_BASED && !TBTR) return(BAD_VERTEX_TIME);

  for (i=0;i<nBID;i++){
    int track=bid[i].track-1;
    if(track>=0){     
      if(trk_level==HIT_BASED){
	tprop=(HBTR->hbtr[track].vert.z-TGEO->tgeo[0].z)/SPEED_OF_LIGHT*1e9;
      }
      else{
	tprop=(TBTR->tbtr[track].vert.z-TGEO->tgeo[0].z)/SPEED_OF_LIGHT*1e9;
      }
      if (bid[i].st.stat>0){
	for(j=0;j<TAGR->bank.nrow;j++){
          float diff=fabs(bid[i].st.vtime-(TAGR->tagr[j].tpho+tprop));
          if (diff<ST_TAG_COINCIDENCE_WINDOW&&diff<best_diff
	      && (TAGR->tagr[j].stat==7 || TAGR->tagr[j].stat==15) ){
            best_diff=diff;	
	    vert_time=TAGR->tagr[j].tpho+tprop;
          }
        }
      }
    }
  }
  return(vert_time);
}


/*-----------------------------------------------------------------------*/
/*---------------------------------------------------------------------*/
float get_vertex_timeG7(int nBID,bid_t *bid,clasTAGR_t *TAGR,int trk_level)
{
  int i,j;
  static int count=0;
  float vert_time=BAD_VERTEX_TIME,tprop=0.0;
  float best_diff=1000.0;
  clasHBTR_t *HBTR=getBank(&bcs_,"HBTR");
  clasTBTR_t *TBTR=getBank(&bcs_,"TBTR");
  clasTGEO_t *TGEO = getBank(&wcs_, "TGEO");
  hdpl_t *hdpl;
  tdpl_t *tdpl;
  double beta = 1.0;
  double vertexTime,pl,sct;

  /* Exit from function if missing the requisite banks... */
  if (TAGR) { 
    if( (trk_level==HIT_BASED) ?  HBTR != NULL : TBTR != NULL) {

      for (i=0;i<nBID;i++) {
	int track=bid[i].track-1;
	if(1 || (track>=0 && (trk_level == HIT_BASED) ? (HBTR->hbtr[track].q < 0.0) :  (TBTR->tbtr[track].q < 0.0))) {     
	  if(trk_level==HIT_BASED){
	    tprop=(HBTR->hbtr[track].vert.z-TGEO->tgeo[0].z)/LIGHT_SPEED;
	    hdpl=hbtr2hdpl(&(HBTR->hbtr[track]));
	  }
	  else{
	    tprop=(TBTR->tbtr[track].vert.z-TGEO->tgeo[0].z)/LIGHT_SPEED;
	    tdpl=tbtr2tdpl(&(TBTR->tbtr[track]));
	  }

	  /* find the vertex time assumming particle is a beta = 1 particle */
	  if (trk_level == HIT_BASED) {
	    vertexTime = bid[i].sc.time - pathlen2sc((bid_t *)&bid[i],(hdpl_t *)hdpl)/(LIGHT_SPEED * beta);
	  }
	  else {
	    pl = pathlen2sc((bid_t *)&bid[i],(hdpl_t *)tdpl);
	    sct = pl/(LIGHT_SPEED * beta);
	    vertexTime = bid[i].sc.time - pathlen2sc((bid_t *)&bid[i],(hdpl_t *)tdpl)/(LIGHT_SPEED * beta);
	  }
	  for(j=0;j<TAGR->bank.nrow;j++){
	    float diff=fabs(vertexTime-(TAGR->tagr[j].tpho+tprop));
	    if (diff<best_diff && (TAGR->tagr[j].stat==7 || TAGR->tagr[j].stat==15) ) {
	      best_diff=diff;	
	      vert_time=TAGR->tagr[j].tpho+tprop;
	    }
	  }
	}
      }
    }
  }

  return(vert_time);
}

tagr_t *get_photon_tagrG7(clasTAGR_t *TAGR,clasBID_t  *BID)
{
  int nBID;
  int i,j;
  static int count=0;
  float vert_time=BAD_VERTEX_TIME,tprop=0.0;
  float best_diff=1000.0;
  clasHBTR_t *HBTR=getBank(&bcs_,"HBTR");
  clasTBTR_t *TBTR=getBank(&bcs_,"TBTR");
  clasTGEO_t *TGEO = getBank(&wcs_, "TGEO");
  hdpl_t *hdpl;
  tdpl_t *tdpl;
  double beta = 1.0;
  double vertexTime,pl,sct;
  tagr_t *ret = NULL;
  bid_t *bid;
  int trk_level = TIME_BASED;

  /* Exit from function if missing the requisite banks... */
  if (TAGR && BID) { 
    nBID = BID->bank.nrow;
    bid = &BID->bid[0];
    if( (trk_level==HIT_BASED) ?  HBTR != NULL : TBTR != NULL) {

      for (i=0;i<nBID;i++) {
	int track=bid[i].track-1;
	if(1 || (track>=0 && (trk_level == HIT_BASED) ? (HBTR->hbtr[track].q < 0.0) :  (TBTR->tbtr[track].q < 0.0))) {     
	  if(trk_level==HIT_BASED){
	    tprop=(HBTR->hbtr[track].vert.z-TGEO->tgeo[0].z)/LIGHT_SPEED;
	    hdpl=hbtr2hdpl(&(HBTR->hbtr[track]));
	  }
	  else{
	    tprop=(TBTR->tbtr[track].vert.z-TGEO->tgeo[0].z)/LIGHT_SPEED;
	    tdpl=tbtr2tdpl(&(TBTR->tbtr[track]));
	  }

	  /* find the vertex time assumming particle is a beta = 1 particle */
	  if (trk_level == HIT_BASED) {
	    vertexTime = bid[i].sc.time - pathlen2sc((bid_t *)&bid[i],(hdpl_t *)hdpl)/(LIGHT_SPEED * beta);
	  }
	  else {
	    pl = pathlen2sc((bid_t *)&bid[i],(hdpl_t *)tdpl);
	    sct = pl/(LIGHT_SPEED * beta);
	    vertexTime = bid[i].sc.time - pathlen2sc((bid_t *)&bid[i],(hdpl_t *)tdpl)/(LIGHT_SPEED * beta);
	  }
	  for(j=0;j<TAGR->bank.nrow;j++){
	    float diff=fabs(vertexTime-(TAGR->tagr[j].tpho+tprop));
	    if (diff<best_diff && (TAGR->tagr[j].stat==7 || TAGR->tagr[j].stat==15) ) {
	      best_diff=diff;	
	      vert_time=TAGR->tagr[j].tpho+tprop;
	      ret = &TAGR->tagr[j];
	    }
	  }
	}
      }
    }
  }

  return(ret);
}


/*-----------------------------------------------------------------------*/
/*---------------------------------------------------------------------*/
float get_vertex_timeG2b(int nBID,bid_t *bid,clasTAGR_t *TAGR,int trk_level)
{
  /* This routine tries to get vertex time for g2b run.
     This run was a photon run without start counter.
     The assumption is all tracks are coming from the same photon.
     It loops over all tracks assigning all possible masses.
     For each momentum/mass combination it calcualtes beta.
     Then, it compares vertex time difference for all possible combinations.
     It looks for the best difference in vertex time any pair of tracks.
     For this pair it picks the track with the larger beta and uses its vertex 
     time to pick the photon from TAGR bank. */


  int i;
  int j;
  int jj;
  int ii;
  int k;
  int ntr;
  static int count = 0;
  float vert_time = BAD_VERTEX_TIME;
  float tprop = 0.0;
  float best_diff = 1000.0;
  clasHBTR_t *HBTR = getBank(&bcs_,"HBTR");
  clasTBTR_t *TBTR = getBank(&bcs_,"TBTR");
  clasTGEO_t *TGEO = getBank(&wcs_, "TGEO");
  hdpl_t *hdpl;
  tdpl_t *tdpl;

  double beta = 1.;
  double vertexTime;
  double pl;
  double sct;

  int ntracks=0;
  int track;

  float q;
  float pmag;
  float vert_Z;

  float diff;


  float pmom[150][5];
  float betam[150][5];
  float vert_array[150][5];

  int best_i  = -1;
  int best_j  = -1;
  int best_ii = -1;
  int best_jj = -1;

  /* Exit from function if missing the requisite banks... */
  if (TAGR) { 
    
    if( (trk_level==HIT_BASED) ?  HBTR != NULL : TBTR != NULL) {
      
      /* get number of tracks */
      
      if(trk_level==HIT_BASED) 
	ntracks = HBTR->bank.nrow;
      else 
	ntracks = TBTR->bank.nrow;
      if(ntracks <= 1)
	return(vert_time); /* wee need more than 1 track to do PID, get out */
      
      else{ /* yes, we have more than 1 track */
	
	/* initialize */
	for (i=0; i<50; i++) {
	  for(j=0; j<5; j++) {
	    vert_array[i][j] = BAD_VERTEX_TIME;
	    betam[i][j] = -1.;
	    pmom[i][j] = 0.0001;
	  }
	}

	k=-1;
	
	for (i=0; i<nBID; i++){
	  track = bid[i].track-1;
	  
	  if(track >= 0) {
	    /* GET CHARGE AND MOMENTUM */
	    if(trk_level == HIT_BASED) {
	      q = HBTR->hbtr[track].q;
	      pmag = v3mag(HBTR->hbtr[track].p);
	      hdpl=hbtr2hdpl(&(HBTR->hbtr[track]));
	    }
	    else {
	      q = TBTR->tbtr[track].q;
	      pmag = v3mag(TBTR->tbtr[track].p);
	      tdpl=tbtr2tdpl(&(TBTR->tbtr[track]));
	    }
	    
	    if(q != 0 && pmag >0. && k < 150) {
	      k++;		
	      /* first do assuming electron  mass */
	      beta = pmag/sqrt(pmag*pmag + ELECTRON_MASS*ELECTRON_MASS);
	      pmom[k][0] = pmag;
	      betam[k][0] = beta;
	      if(bid[i].sc.stat) {
		if(trk_level == HIT_BASED) 
		  pl = pathlen2sc((bid_t *)&bid[i],(hdpl_t *)hdpl);
		else 
		  pl = pathlen2sc((bid_t *)&bid[i],(hdpl_t *)tdpl);
		vert_array[k][0] = bid[i].sc.time - pl/(LIGHT_SPEED * beta);
	      }
	      /* no SC time available, will try EC time */ 
	      else if (bid[i].ec.stat) {
		if(trk_level == HIT_BASED) 
		  pl = hdpl[ForwardECPlane].tlen;
		else 
		  pl = tdpl[ForwardECPlane].tlen;
		vert_array[k][0] = bid[i].ec.time - pl/(LIGHT_SPEED * beta);
	      }
	      
	      /* first do assuming pion mass */
	      beta = pmag/sqrt(pmag*pmag + PI_CHARGED_MASS*PI_CHARGED_MASS);
	      pmom[k][1] = pmag;
	      betam[k][1] = beta;
	      if(bid[i].sc.stat) {
		if(trk_level == HIT_BASED) 
		  pl = pathlen2sc((bid_t *)&bid[i],(hdpl_t *)hdpl);
		else 
		  pl = pathlen2sc((bid_t *)&bid[i],(hdpl_t *)tdpl);
		vert_array[k][1] = bid[i].sc.time - pl/(LIGHT_SPEED * beta);
	      }
	      /* no SC time available, will try EC time */ 
	      else if (bid[i].ec.stat) {
		if(trk_level == HIT_BASED) 
		  pl = hdpl[ForwardECPlane].tlen;
		else 
		  pl = tdpl[ForwardECPlane].tlen;
		vert_array[k][1] = bid[i].ec.time - pl/(LIGHT_SPEED * beta);
	      }
	      /* now do assuming Kaon mass */
	      beta = pmag/sqrt(pmag*pmag + KAON_CHARGED_MASS*KAON_CHARGED_MASS);
	      pmom[k][2] = pmag;
	      betam[k][2] = beta;
	      
	      if(bid[i].sc.stat) {
		if(trk_level == HIT_BASED) 
		  pl = pathlen2sc((bid_t *)&bid[i],(hdpl_t *)hdpl);
		else 
		  pl = pathlen2sc((bid_t *)&bid[i],(hdpl_t *)tdpl);
		vert_array[k][2] = bid[i].sc.time - pl/(LIGHT_SPEED * beta);
	      }
	      else if (bid[i].ec.stat) {
		if(trk_level == HIT_BASED) 
		  pl = hdpl[ForwardECPlane].tlen;
		else 
		  pl = tdpl[ForwardECPlane].tlen;
		vert_array[k][2] = bid[i].ec.time - pl/(LIGHT_SPEED * beta);
	      }
	      
	      if (q > 0.) {
		/* do proton mass */
		beta = pmag/sqrt(pmag*pmag + PROTON_MASS*PROTON_MASS);
		pmom[k][3] = pmag;
		betam[k][3] = beta;
		
		if(bid[i].sc.stat) {
		  if(trk_level == HIT_BASED) 
		    pl = pathlen2sc((bid_t *)&bid[i],(hdpl_t *)hdpl);
		  else 
		    pl = pathlen2sc((bid_t *)&bid[i],(hdpl_t *)tdpl);
		  vert_array[k][3] = bid[i].sc.time - pl/(LIGHT_SPEED * beta);
		}
		else if (bid[i].ec.stat) {
		  if(trk_level == HIT_BASED) 
		    pl = hdpl[ForwardECPlane].tlen;
		  else 
		    pl = tdpl[ForwardECPlane].tlen;
		  vert_array[k][3] = bid[i].ec.time - pl/(LIGHT_SPEED * beta);
		}
		/* now do Deuteron mass */
		beta = pmag/sqrt(pmag*pmag + DEUTERON_MASS*DEUTERON_MASS);
		pmom[k][4] = pmag;
		betam[k][4] = beta;
		
		if(bid[i].sc.stat) {
		  if(trk_level == HIT_BASED) 
		    pl = pathlen2sc((bid_t *)&bid[i],(hdpl_t *)hdpl);
		  else 
		    pl = pathlen2sc((bid_t *)&bid[i],(hdpl_t *)tdpl);
		  vert_array[k][4] = bid[i].sc.time - pl/(LIGHT_SPEED * beta);
		}
		else if (bid[i].ec.stat) {
		  if(trk_level == HIT_BASED) 
		    pl = hdpl[ForwardECPlane].tlen;
		  else 
		    pl = tdpl[ForwardECPlane].tlen;
		  vert_array[k][4] = bid[i].ec.time - pl/(LIGHT_SPEED * beta);
		}
	      }
	    }
	  }
	}
	/* now all vert_array is filled */
	/* will try to find best vertex time */
	
	ntr = k+1;
	best_diff = 1000.;
	best_i  = -1;
	best_j  = -1;
	best_ii = -1;
	best_jj = -1;
	
	if(ntr >1){
	  for (i=0; i<ntr; i++) {
	    for (j=0; j<5; j++) {
	      for (ii=0; ii<ntr; ii++) {
		if(ii != i) {
		  for (jj=0; jj<5; jj++) {
		    if(vert_array[i][j] != BAD_VERTEX_TIME 
		       && vert_array[ii][jj] != BAD_VERTEX_TIME) {
		      diff = fabs(vert_array[i][j]-vert_array[ii][jj]);
		      if(diff < best_diff ) {
			best_diff = diff;
			best_i  = i;
			best_j  = j;
			best_ii = ii;
			best_jj = jj;
		      }
		    }
		  }
		}
	      }
	    }
	  }
	  if (best_diff <= VERTEX_DIFF) {
	    
	    if(betam[best_i][best_j] >= betam[best_ii][best_jj]){
	      vertexTime = vert_array[best_i][best_j];
	      if (trk_level == HIT_BASED)
		vert_Z = HBTR->hbtr[bid[best_i].track-1].vert.z; 
	      else 
		vert_Z = TBTR->tbtr[bid[best_i].track-1].vert.z; 
	    }
	    else { 
	      vertexTime = vert_array[best_ii][best_jj];
	      if (trk_level == HIT_BASED) 
		vert_Z = HBTR->hbtr[bid[best_ii].track-1].vert.z;
	      else 
		vert_Z = TBTR->tbtr[bid[best_ii].track-1].vert.z; 
	    }
	    
	    tprop = (vert_Z - TGEO->tgeo[0].z)/LIGHT_SPEED;
	    best_diff = 1000.;
	    
	    for(j=0;j<TAGR->bank.nrow;j++){
	      diff=fabs(vertexTime-(TAGR->tagr[j].tpho+tprop));
	      if (diff<best_diff && (TAGR->tagr[j].stat==7 || 
				     TAGR->tagr[j].stat==15) ) {
		best_diff=diff;	
		vert_time=TAGR->tagr[j].tpho+tprop;
	      }
	    }
	  }
	}
      }
    }
  }
  return(vert_time);
}
  

/*-----------------------------------------------------------------------*/
