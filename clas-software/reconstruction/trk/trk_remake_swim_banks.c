/* trk_remake_swim_banks.c */
/* Original written by John McNabb and Robert Feuerbach */
/* Reorganized and rewritten by Rob Feuerbach to be more general and
 * permit creation of the TDPL and TBLA bank.
 */

/*
 * All these routines require that the sdaswim common block be set up, at
 * least as far as containing information about the planes.
 * So run trk_set_def and dc_begin_run before using any of these.
 *
 *  General Routines:
 *     trk_remake_swim_banks   -- recreates the TDPL,TBLA banks.
 *     trk_remake_tbla         -- wrapper to call trk_remake_swim_banks for only TBLA
 *
 *     get_track_hits          -- fills passed tbla, tdpl structures with results
 *                                from a swim.
 *
 */
  

/*_begin_doc
 *  RCS ID string
 *  $Id: trk_remake_swim_banks.c,v 1.10 2002/02/11 22:53:58 pasyuk Exp $*/
#define USE(var) static void * use_##var = (void *) &var
static char crcsid[] = 
   "$Id: trk_remake_swim_banks.c,v 1.10 2002/02/11 22:53:58 pasyuk Exp $";
USE(crcsid);   /* make sure it is not optimized away */
/* END of RCS information*/

#include <ntypes.h>
#include <bostypes.h>
#include <stdio.h>
#include <vertex.h>
#include <math.h>
#include <kinematics.h>
#include <string.h>
#include <trk.h>
#include <trk_run_control.h> /* get access to tcl variables for simplicity */

/* local prototype */
int selectwire(int pickwire,
	       vector3_t sectint, vector3_t sectintdir, int tsector, int flay,
	       float *fitdoca, int *dc1_index, float *tdc);

void fitparms2vect(float lambda, float phi, float z0, float d0,
		    vector3_t *pnt, vector3_t *dir);

/* prototype for external call */
int get_tber_info(tber_t *tber, int slinfo[6], int *psec, int *ptrk_in_sect, int *ptrk_in_hbtr);


/* Actual functions */
int trk_remake_tbla(int sect1banknumber, float zangle,float minstep,float maxstep, int pickwire){
  return trk_remake_swim_banks(sect1banknumber,zangle,minstep,maxstep,pickwire,1,0,1);
}

int trk_remake_tdpl(int sect1banknumber, float zangle, float minstep, float maxstep){
  return trk_remake_swim_banks(sect1banknumber,zangle,minstep,maxstep,1,0,1,1);
}

/* FORTRAN callable version */
int trk_remake_tbla_(int *psect1banknumber, float *pzangle,
		     float *pminstep, float *pmaxstep, int *pickwire){
  return trk_remake_tbla(*psect1banknumber, *pzangle, *pminstep, *pmaxstep,
			 *pickwire);
}

int trk_remake_tdpl_(int *sect1banknumber, float *zangle,
		     float *minstep, float *maxstep){
  return trk_remake_swim_banks(*sect1banknumber,*zangle,*minstep,*maxstep,1,0,1,1);
}


int trk_remake_swim_banks(int sect1banknumber, float zangle,float minstep,float maxstep,
			  int pickwire, int blabank, int dplbank, int tbased){
  /* rebuilds the HBLA/TBLA and HDPL/TDPL banks for given swimming paramters.
   * 
   *
   * Inputs:  sect1banknumber       Number to use for Sector 1 (so doesn't overwrite
   *                                the original/old ?BLA and ?DPL banks)
   *
   *          zangle                Maximum permitted angular change of the track
   *                                per step. Replaces tcl trk_zangle.
   *
   *          minstep, maxstep      Minimum/maximum step size. Replaces tcl
   *                                trk_zstep(1) and trk_zstep(2).
   *
   *          pickwire              how to choose DC wire reported in ?BLA bank
   *                                -1 cell with the smallest tdc time.
   *                                 0 cell returned by trk_rehit.
   *                                +1 cell with the larger tdc time.
   *                                 See the routine selectwire .
   *
   *          blabank              != 0 means remake tbla bank, 0 means don't.
   *          dplbank              != 0 means remake tdpl bank, 0 means don't.
   *
   *          tbased               Choose between reading and making the hit or
   *                                time based swim banks.
   *                               == 0 read in HBER and make the HBLA, HDPL banks.
   *                               != 0 use TBER and make TBLA, TDPL banks.  
   *
   * (Common block) sdaswim         to get plane locations (used in trk_swim).
   *
   * (bosbank) TBER and TRKS banks  swimming is done starting at the vertex plane
   *                                with the location and direction gotten from
   *                                the tracking parameters in the HBER/TBER bank.
   *                                So, the assumption is that the swimming starts
   *                                at the vertex plane. I'm not sure how this would
   *                                work for the polarized target runs where
   *                                tracking might start at first layer of DC.
   *
   * Outputs:  HBLA/TBLA, HDPL/TDPL bank
   * returns:  1 if okay, 0 if not
   */

  /* The hit based and time based banks are nearly identical in structure,
   * and to prevent too much rewriting and possible errors creeping in, all
   * variable names and types with be for the TBER, TDPL, TBLA banks and
   * will be used in processing both the Hit Based and Time Based swimming
   * information.
   */
  int stat;
  int slinfo[6];
  int sect,tsector;
  int trk_in_sect;
  int trk_in_hbtr;
  static int N_no_TBER;

  clasTBER_t *TBER=NULL;
  tber_t *tber;
  clasTBLA_t *TBLA=NULL;
  tbla_t *tbla,*tblatemp;
  clasTDPL_t *TDPL=NULL;
  tdpl_t *tdpl, *tdpltemp, track_tdpl[10];

  clasHBLA_t *HBLA=NULL;
  hbla_t *hbla,*hblatemp;
  clasDCGM_t *DCGM=NULL;
  int col; 
  trks_t *trks;
  tbla_t track_tbla[34];

  int trk;
  int ntrk;
  int ntrkpersect[6]={0,0,0,0,0,0};
  
  char BERbank[5], BLAbank[5], DPLbank[5];
  int bla_size;
  
  tdpl = track_tdpl;  /* default pointer for tdpl */

  if (!tbased) {
    strcpy(BERbank,"HBER");
    strcpy(BLAbank,"HBLA");
    strcpy(DPLbank,"HDPL");
    bla_size = sizeof(hbla_t);
  } else {
    strcpy(BERbank,"TBER");
    strcpy(BLAbank,"TBLA");
    strcpy(DPLbank,"TDPL");
    bla_size = sizeof(tbla_t);
  }

  if( (TBER=(clasTBER_t *)getBank(&bcs_,BERbank)) ){ 
    tber = TBER->tber;          /*bos bank structures(saves pointer junk)*/
    ntrk=(TBER->bank.nrow);              /* number of tracks in the event*/

    for(sect=0;sect<6;sect++){      
      for(trk = 0; trk<ntrk; trk++){
	get_tber_info(tber+trk, slinfo,&tsector,&trk_in_sect,&trk_in_hbtr);
	if(tsector==sect+1)
	  ntrkpersect[sect]++;
      }
    }
    
    for(sect=0;sect<6;sect++){          
      /* are there any tracks in this sector? */
      if (ntrkpersect[sect] == 0) continue;

      col = (bla_size/sizeof(int));
      TBLA = NULL;
      tbla = NULL;
      HBLA = NULL;
      hbla = NULL;
      if( blabank ) {
	if (tbased) {
	  TBLA=(clasTBLA_t *)makeBank(&bcs_,BLAbank,sect+sect1banknumber,col,
				      ntrkpersect[sect]*NTBLA_ROW);
	  if (TBLA != NULL)
	    tbla = TBLA->tbla;
	  else
	    { 
	      printf("Unable to rebuild the TBLA bank %s:%d\n",__FILE__,__LINE__);
	      return (0);
	    }
	} else {
	  HBLA=(clasHBLA_t *)makeBank(&bcs_,BLAbank,sect+sect1banknumber,col,
				      ntrkpersect[sect]*NTBLA_ROW);
	  if (HBLA != NULL)
	    hbla = HBLA->hbla;
	  else
	    {
	      printf("Unable to rebuild the HBLA bank %s:%d\n",__FILE__,__LINE__);
	      return (0);
	    }
	  DCGM = (clasDCGM_t *)getGroup(&wcs_,"DCGM",sect);
	}
      }
      
      
      col = (sizeof(tdpl_t)/sizeof(int));
      TDPL = NULL;
      tdpl = NULL;
      if( dplbank ) {
	TDPL=(clasTDPL_t *)makeBank(&bcs_,DPLbank,sect+sect1banknumber,col,
				    ntrkpersect[sect]*(NPLN-NPL_DC));
	if (TDPL != NULL)
	  tdpl = TDPL->tdpl;
	else
	  {
	    printf("Unable to rebuild the TDPL bank %s:%d\n",__FILE__,__LINE__);
	  return (0);
	  }
      }
      
      /* LOOP thru the tracks */
      for(trk = 0; trk<ntrk; trk++){
	int lay;
	int layer;

	get_tber_info(tber+trk, slinfo,&tsector,&trk_in_sect,&trk_in_hbtr);

	/* pass along NULL as pointer if not supposed to do TBLA stuff */
	if ((tbla != NULL) || (hbla != NULL)) {
	  tblatemp = track_tbla;
	} else {
	  tblatemp = NULL;
	}

	
	/* pass along NULL as pointer if not supposed to do TDPL stuff */
	if (tdpl != NULL) {
	  tdpltemp = tdpl+(trk_in_sect-1)*(NPLN-NPL_DC);
	} else {
	  tdpltemp = NULL;
	}

	/* if the track is in this sector ... */
	/*    test to see of we have the trks bank if we need it */
	trks = NULL;
	if ( (tsector==sect+1) &&
	     (!tbased || (trks = trkn2trks(trk_in_hbtr, tsector)) ) ) {
	  float beta, st_time;

	  if (trks) {
	    beta = trks->beta;
	    st_time = trks->st_time;
	  } else {
	    beta = 20;  /* for hit based, don't want beta correction so
			 * make it (extremely) large so little time is
			 * subtracted off.
			 */
	    st_time = 0.;
	  }
	  if (get_track_hits(tber+trk,beta, st_time,
			     zangle,minstep,maxstep,pickwire,
			     tblatemp,tdpltemp)) {
	    
	    /* do the TBLA stuff if we are supposed to */
	    if ((tbla != NULL) || (hbla != NULL)) {
	      for (layer=0,lay=0; layer < NPL_DC; layer++) {
		if ((layer != (5-1)) && (layer != (6-1)) ){
		  stat = (slinfo[layer/6]>>(layer%6))%2; 
		  stat = (stat+1)%2;
		  if(stat) { /*not a real hit*/
		    track_tbla[lay].calcdoca=0;
		    track_tbla[lay].dtime=0;
		    track_tbla[lay].sgdoca=0;
		    track_tbla[lay].stat=stat;
		  }
		  
		  if (tbla != NULL) {
		    tblatemp=tbla+(trk_in_sect-1)*NTBLA_ROW+lay;
		    /* put stuff into the TBLA bank */
		    memcpy(tblatemp,&track_tbla[lay],bla_size);
		    tblatemp->trk_pln += 100*(trk+1);
		  } else if (hbla != NULL) {
		    hblatemp=hbla+(trk_in_sect-1)*NTBLA_ROW+lay;
		    /* put stuff into the HBLA bank */
		    memcpy(hblatemp,&track_tbla[lay],bla_size);
		    hblatemp->trk_pln += 100*(trk+1);
		    if (DCGM) {
		      if (hblatemp->wire <= DCGM->dcgm[layer+1].max_cylw) {
			hblatemp->sgdoca = DCGM->dcgm[layer+1].cell_size/1.732;
		      } else {
			hblatemp->sgdoca = DCGM->dcgm[layer+1].p_sep/2./1.732;
		      }
		    }
		  }

		  lay++;  /* counter of REAL layers in tbla bank */
		}
	      }
	    } /* done with tbla bank for this track */
	    
	    /* if we are creating the tdpl bank, then the pointer passed in
	     * is to the correct spot. All we have to do is update the
	     * track number information for it.
	     */
	    if (tdpl != NULL) {
	      tdpltemp[0].trk_pln += 100*(trk+1);
	      fitparms2vect((tber+trk)->lambda, (tber+trk)->phi,
			    (tber+trk)->z0, (tber+trk)->d0,
			    &tdpltemp[0].pos,&tdpltemp[0].dir);
	      tdpltemp[0].tlen = 0.;
	      for (layer = 1; layer < (NPLN-NPL_DC); layer++) {
		tdpltemp[layer].trk_pln += 100*(trk+1);
	      }
	    }
	      
	  } /* end of track good -- getting hits succeeded */
	} /* end process track */
      } /* end loop thru tracks */
    } /* sector loop */
    return(1);
  }
  else /*no tber bank*/
    {
      N_no_TBER++; 
       if ((N_no_TBER <= 1000 && N_no_TBER%100 == 0) || \
	  (N_no_TBER > 1000 && N_no_TBER%1000 == 0) ) { 
	fprintf(stderr,"\n%s:Line %d: %d events with no TBER bank.\n",__FILE__,__LINE__,N_no_TBER);
	fflush(stderr);
       } 
      return(0);
    }
}


int get_track_hits(tber_t *tber, float beta, float st_time,
		   float zangle, float minstep, float maxstep,
		   int pickwire, tbla_t *tbla, tdpl_t *tdpl)
  /*
   * Returns list of track-plane intersections after reswimming the track,
   * in TBLA and TDPL formats.
   *
   * Inputs:  tber       pointer to tber structure for the track to be swum.
   *          beta       speed of the particle (in units of c)
   *          st_time    start time in ns.
   *          zangle     max. permitted angular change of track per step.
   *          minstep    min. permitted step size.
   *          maxstep    max. permitted step size.
   *          pickwire   how to choose DC wire reported:
   *                       See the routine selectwire
   *
   *          tbla, tdpl  pointers to arrays for hit information.
   *                      if NULL, hit information not stored.
   *
   * Outputs: tbla       array of tbla_t[34] containing DChit information
   *          tdpl       array of tdpl_t[10] containing non-DC plane intersections
   *
   */
{
  int layer, flay;
  int wire;
  int wirel[NPLN];
  float fitdoca[NPLN];
  vector3_t B[NPLN];
  int dc1_index[NPLN];
  float driftt[NPLN];
  float tdoca[NPLN];
  float tsigma[NPLN];
  float alpha[NPLN];
  float wlen[NPLN];
  float tdc;
  float phi, phin;
  tbla_t *tblatemp;
  tdpl_t *tdpltemp;
  int tblacnt, tdplcnt;
  int tsector, trk_in_sect, trk_in_hbtr, slinfo[6], stat;

  vector3_t trkint[NPLN];    /*outputs of trk_swim*/
  vector3_t trkintdir[NPLN];    /*outputs of trk_swim*/
  vector3_t sectint[NPLN];    /*outputs of trk_swim rotated into sector coordinates*/
  vector3_t sectintdir[NPLN];    /*outputs of trk_swim rotated into sector coordinates*/
  float trklength[NPLN];     /*outputs of trk_swim*/

  get_tber_info(tber,slinfo,&tsector,&trk_in_sect,&trk_in_hbtr);

  if((beta>=0.) && trk_swim(tber,trkint,trkintdir,trklength,zangle,minstep,maxstep)) {
    for(layer=0;layer<NPLN;layer++){
      /*rotate into sector coords*/
      sectint[layer].x=trkint[layer].y;
      sectint[layer].y=trkint[layer].z;
      sectint[layer].z=trkint[layer].x;
      sectintdir[layer].x=trkintdir[layer].y;
      sectintdir[layer].y=trkintdir[layer].z;
      sectintdir[layer].z=trkintdir[layer].x;
      
      /* do we have to collect the drift chamber info? */
      if( (tbla!= NULL) && (layer >= NDC_MIN) && (layer <= NDC_MAX) ) {
	flay=layer-NDC_MIN+1; /* count DC layers starting from 1 (like fortran) */
	if(flay!=5 && flay !=6){
	  wirel[layer] = wire =
	    selectwire(pickwire,sectint[layer],sectintdir[layer],tsector,flay,
		       &fitdoca[layer],&dc1_index[layer],&tdc);
	  
	  phi=atan2(sectintdir[layer].x,sectintdir[layer].z);
	  phin= phinorm(tsector,flay,wire);
	  alpha[layer]=phi-phin; 
	  
	  /* phi in rad, alpha in rad in dtime funcion call,
	   * but degrees in tbla bank, at least according to documentation, ych!
	   */
	  
	  trk_dtime_retbla_(&tsector,&flay,&wire,&(sectint[layer]),
			    &(alpha[layer]),&tdc,&(trklength[layer]),&beta,
			    &st_time,&(driftt[layer]),&(tdoca[layer]),
			    &(tsigma[layer]),&(wlen[layer]),&(B[layer]));
	  /*alpha[layer]=alpha[layer]*180/acos(-1);*//*doc appears to be wrong*/
	}
      }
    }
    
    /*fill_tbla and tdpl for this track & sector*/
    tblacnt=0;
    tdplcnt=0;
    for(layer=0;layer<NPLN;layer++){
      
      /* in the range for the DC's -- fill tbla */
      if ( (layer >= NDC_MIN) && (layer <= NDC_MAX) ) {
	/* Are we permitted to fill in this layer */
	if( (tbla!=NULL) && (layer!=(NDC_MIN+5-1)) && (layer !=(NDC_MIN+6-1) ) ) {
	  /* make best guess at lramb */
	  if( fitdoca[layer] * tdoca[layer]<0) tdoca[layer]*=-1;
	  tblatemp=tbla+tblacnt;
	  /*fill tbla*/
	  tblatemp->trk_pln=(layer+1);
	  tblatemp->pos = sectint[layer];
	  tblatemp->dir = B[layer];
	  tblatemp->tlen = trklength[layer];
	  tblatemp->dc1 = dc1_index[layer];
	  stat = (slinfo[(layer-NDC_MIN)/6]>>((layer-NDC_MIN)%6))%2;
	  stat = (stat+1)%2;
	  tblatemp->stat = stat;
	  tblatemp->wire = wirel[layer];
	  tblatemp->dtime = driftt[layer];
	  tblatemp->alpha=alpha[layer];
	  tblatemp->wlen=wlen[layer];
	  tblatemp->sgdoca=tsigma[layer];
	  tblatemp->fitdoca=fitdoca[layer];
	  tblatemp->calcdoca=tdoca[layer];
	  tblacnt++;
	}
      }

      /* not in DC, fill tdpl */
      else if (tdpl != NULL ) {
	tdpltemp=tdpl+tdplcnt;
	tdpltemp->trk_pln=layer+1;
	tdpltemp->pos=sectint[layer];
	tdpltemp->dir=sectintdir[layer];
	tdpltemp->tlen=trklength[layer];
	tdplcnt++;
      }
    }
  }
  else  /* couldn't swim the track */
    return 0;
  return 1;
}


/*
 * c routines to help with remaking the tbla bank
 *
 */

float trk_find_dc1_hit_tdc(int sector, int layer, int wire, int *index) {
  /* find the hit corresponding to (sector,layer,wire) being hit.
   *   inputs: sector,       sector of the hit (1 to 6)
   *           layer,        layer for the hit (1 to 36)
   *           wire          wire for the hit (1 to 192)
   *           index         the index in the DC1 bank to start looking from.
   *                         (Fortran style, starting at 1)
   *   returns:
   *           (as value)    the tdc signal (in ns) of the hit, -1000. if not found.
   *           index         the index of the hit in the DC1 bank. 0 if not found.
   *                         Fortran style, so starts at 1.
   *
   *   NOTE: hits are stored in increasing layer and wire number in the DC1 bank,
   *         so searches for subsequent hits for the track should be started from
   *         the returned index value. -1 if not found.
   */
  clasDC1_t *DC1 = getGroup(&bcs_,"DC1 ",sector);

  int i;
  int dclay, dcwir;

  int nrow;
  int retindex = 0;
  float tdc = -1000.;

  if (DC1 != NULL) {
    nrow = DC1->bank.nrow;

    /* loop thru the DC1 bank looking for the hit. */
    for (i = (*index > 0 ? *index-1 : 0); i < nrow; i++) {
      dclay = ind2layer(DC1->dc1[i].id);
      dcwir = ind2wire(DC1->dc1[i].id);

      if ( (dclay == layer) && (dcwir == wire) ) {
        retindex = i+1;
        tdc = DC1->dc1[i].time;  /* for now, DC1 contains just the raw tdc value */
        break;
      }
    }
  }

  *index = retindex;
  return tdc;
}
    

void fitparms2vect(float lambda, float phi, float z0, float d0,
		    vector3_t *pnt, vector3_t *dir) {

  /* translates track parameters from the tracking parameter arguements
   * to an actual point and direction in the SCS. Assumes the parameters
   * are coming from the track at the vrt plane (x=0). Outputs pnt, dir.
   */

  float sl, cl, sp, cp, alpha, sa, ca;

  alpha=0;  /* orientation of plane. We are in Sector Coord system
	     * (like Sector 1) -- so alpha=0 and plane at x=0 */

  sl = sin(lambda);
  cl = cos(lambda);
  sp = sin(phi);
  cp = cos(phi);
  sa = sin(alpha);
  ca = cos(alpha);

  pnt->x = -z0 * sa;
  pnt->y = z0 * ca;
  pnt->z = -1 * d0 / sp;
  dir->x = (cl*sp*ca-sl*sa);
             /* (cos(lambda)*sin(phi)*cos(alpha)-sin(lambda)*sin(alpha)*/
  dir->y = (cl*sp*sa+sl*ca);
             /* (cos(lambda)*sin(phi)*sin(alpha)+sin(lambda)*cos(alpha)*/
  dir->z = cl*cp;
             /* cos(lambda)*cos(phi) */

  return;
}


int selectwire(int pickwire,
	       vector3_t sectint, vector3_t sectintdir, int tsector, int flay,
	       float *fitdoca, int *dc1_index, float *tdc)
 /* find and select the appropriate hit from the DC1 bank,
  * depending upon pickwire.
  * Input:
  *   pickwire - for layers with multiple hits for a track, picks out which
  *              wire will be in the TBLA bank.
  *         -1 cell with the smallest tdc time.
  *          0 cell returned by trk_rehit (old way)
  *         +1 cell with the larger tdc time.
  *
  *   sectint, sectintdir - intersection point/dir of track with layer
  *   tsector, flay       - sector (1-6) and layer (1-36) to find hit in
  *
  * Output:
  *   fitdoca   - signed distance of closest approach from wire to track
  *   dc1_index - index of hit wire in dc1 bank
  *   tdc       - tdc value for the hit (NOT Drift time)
  */

{

  int i, index;

  /* arrays of trial values */
  int wir[2];
  float doca[2];
  int dc1_ind[2];
  float tdc_w[2];


  for (i=0; i<2; i++) {
    if (i==0) { /* which wire are we testing */
      /* first wire, get from geometry */
      wir[i] = trk_rehit_(&sectint,&tsector,&flay);
    } else {
      /* next wire, look on side that the track passed by */
      wir[i] = wir[0] + ( (doca[0] > 0) ? 1 : -1);
    }
    
    doca[i] = trk_redoca_(&sectint,&sectintdir,&tsector,&flay,&wir[i]);
    index=0;
    tdc_w[i] = trk_find_dc1_hit_tdc(tsector,flay,wir[i],&index);
    if(index>0){   /* really was a hit on this wire */
      dc1_ind[i] = index;		    
    }
    else {
      dc1_ind[i] = -1; /* dc index not found*/
      tdc_w[i] = 0.; /* flag for next routine that junk hit */
    }
    
  } /* gone through all the wires */
  
  index = 0;  /* index to wire with best match */
  if (pickwire) {
    /* loop through hits, pick one that matches criteria */
    /* it is set up as a loop so we can easily modify it to
     * look at more neighboring wires if necessary
     */
    for (i= 1; i < 2; i++) {
      /* do we have a good hit anywhere */
      if (dc1_ind[index] <= 0) {
	if (dc1_ind[i] > 0) {
	  index = i;
	}
      } else {
	if (dc1_ind[i] > 0) {
	  /* pickwire = -1 for smaller tdc, +1 for larger
	   * tdc value. 
	   */
	  float test = pickwire*(tdc_w[index]-tdc_w[i]);
	  if (test < 0) {
	    index = i;
	  }
	}
      }
    }
  } /* finished selecting the wire */
  
  *fitdoca   = doca[index];
  *dc1_index = dc1_ind[index];
  *tdc       = tdc_w[index];

  return wir[index];
}


trks_t *trkn2trks(int trackno, int sector) {
  /* find the TRKS entry with the specified track number.
   * TRKS uses the HBTR track number.
   */
  
  clasTRKS_t *TRKS = getGroup(&bcs_,"TRKS",sector);
  int i, nrow;

  /* just fall thru and return NULL if not found */

  if (TRKS) {
    nrow = TRKS->bank.nrow;
    for(i=0; i < nrow; i++) {
      if ( (TRKS->trks[i].trk_lev % 100) == trackno)
        return &(TRKS->trks[i]);
    }
  }

  return NULL;
}

float phinorm(int sector, int layer, int wire) {
  clasDCGM_t *DCGM = getGroup(&wcs_,"DCGM",sector);

  float ret = 0.;

  /*note that layer-1 is used to preserve the fortran convention of counting from 1*/
  if (DCGM) {
    if (wire > DCGM->dcgm[layer-1].max_cylw) /* planar section: R1 */
      ret = atan2(DCGM->dcgm[layer-1].x_norm, DCGM->dcgm[layer-1].z_norm);
    else
      ret = DCGM->dcgm[layer-1].theta_start + (wire-1)*DCGM->dcgm[layer-1].d_theta;
  }

  return ret;
}
