/* -=======================================================- *
$Id: PartUtil.c,v 1.28 2004/08/12 20:43:31 pasyuk Exp $
$Author: pasyuk $
$Revision: 1.28 $
$Date: 2004/08/12 20:43:31 $
* -=======================================================- */

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
    

/*----------------------------------------------------------------------*/
tagr_t *get_photon_tagr(clasTAGR_t *TAGR,clasBID_t *BID){
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
	    float diff=fabs(BID->bid[i].st.vtime-(TAGR->tagr[j].tpho+tprop));
	    if (diff<ST_TAG_COINCIDENCE_WINDOW&&diff<best_diff
		&& (TAGR->tagr[j].stat==7 || TAGR->tagr[j].stat==15)){
	      best_diff=diff;
	      tagr=&(TAGR->tagr[j]);
	    }
	  }       
	}
      }
      else if(RUNC->runc.beam.type.val.i[0]==G2B_RUN) {

	if (BID->bid[i].sc.stat){       
	  for(j=0;j<TAGR->bank.nrow;j++){
	    float diff=fabs(BID->bid[i].sc.vtime-(TAGR->tagr[j].tpho+tprop));
	    if (diff<ST_TAG_COINCIDENCE_WINDOW&&diff<best_diff
		&& (TAGR->tagr[j].stat==7 || TAGR->tagr[j].stat==15)){
	      best_diff=diff;
	      tagr=&(TAGR->tagr[j]);
	    }
	  }       
	}
      }









    }
  }
  return(tagr);
}

float get_photon_energy(clasTAGR_t *TAGR,clasBID_t *BID){
  tagr_t *tagr = get_photon_tagr(TAGR, BID);
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

