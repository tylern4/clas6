/*
 *  filltrack gets info from TBTR, TBER, and TBLA banks and puts
 * it into track. returns 0 if ok, something else if error.
 * inputs require the bottom track on the linked list. If this
 * point is NULL, then it assumes to start from scratch and
 * sets toptrack to the next made track.
 */

#include <stdlib.h>
#include <stdio.h>
#include <kinematics.h>
#include <ntypes.h>
#include <bostypes.h>
#include <utility.h>
#include "nrutil.h"
#include "aligndc.h"
#include "dcg_tools.h"
#include "fmatrix.h"

float weight(track_t *track);
float tracksigma(track_t *track, vector3_t wpos, vector3_t wdir);


int filltrack(track_t **track, int evnt) {
  extern int Sector[];
  extern track_t *toptrack;
  extern int RegMove[3];

  track_t *thistrack = NULL;
  track_t *oldtrack;
  clasTBTR_t *TBTR;
  clasTBER_t *TBER;
  tbla_t *tbla;
  tbtr_t *tbtr;
  int i,j,k;
  int sector,layer,reg,sl;
  int ret = 0;


  oldtrack = *track;
  TBTR = getBank(&bcs_,"TBTR");
  if (TBTR) {     /* there is a TBTR bank */
    tbtr = TBTR->tbtr;
    TBER = getBank(&bcs_,"TBER");
    for (i = 0; i < TBTR->bank.nrow; i++) {   /* walk through the entries */
      if ( Sector[sector = tbtr[i].itr_sec/100] ) { /* found one */

	/* count the number of missing hits in each SL */
	int holes[6] = {0,0,0,0,0,0};

	if ( ! (thistrack=malloc(sizeof(track_t)))) {
	  /* error */
	  fprintf(stderr,"Error: memory full.\n");
	  ret -1;
	  break;
	}

	thistrack->sector = sector;
	thistrack->event = evnt;
	thistrack->number = tbtr[i].itr_sec % 100;
	thistrack->next = NULL;
	thistrack->weight = 0.;

	/* now, from the TBLA bank */
	if ( (tbla = tbtr2tbla(&tbtr[i])) ) {
	  for (j=0; j<34; j++) {
	    layer = thistrack->hits[j].layer = tbla[j].trk_pln % 100 - 3;
	    reg = (layer-1)/12;
	    sl = (layer-1)/6;
	    thistrack->hits[j].wire = tbla[j].wire;
	    thistrack->hits[j].tdoca = tbla[j].calcdoca;
	    thistrack->hits[j].sigma = tbla[j].sgdoca/weightmult[reg];
            thistrack->hits[j].trsigma = -1.;  /* flag to calculate later */
	    
	    if (tbla[j].wire < 1) { /* hole in the track */
	      holes[sl]++;
	    }

	    /* test the input track. if it is bad, ditch it */
	    if ( ( (thistrack->hits[j].wire > 0) &&   /* hit is too far away */
		   (fabs(tbla[j].calcdoca-tbla[j].fitdoca) > 3. ) ) ||
		 ( holes[sl] > MaxHoles[sl] ) )       /* or too many holes */
	      {
	      /* in any case, junk the track */
	      free(thistrack);
	      thistrack = NULL;
	      break;
	    }
	  }
	} else { /* error getting TBLA bank */
	  free(thistrack);
	  thistrack = NULL;
	  ret = -2;
	  break;
	}

	if ( thistrack && TBER) {  /* there had better be, the TBTR bank exists */
               	  /* calculate x,y,z in SCS from TBER bank values */
	  float lambda,phi,d0,z0;
	  float sl, cl, sp, cp;
    float chi2;

	  tber_t *tber = TBER->tber;
	  
	  lambda = tber[i].lambda;
	  phi = tber[i].phi;
	  d0 = tber[i].d0;
	  z0 = tber[i].z0;
	  
	  chi2 = tber[i].chi2;

	  sl = sin(lambda);
	  cl = cos(lambda);
	  sp = sin(phi);
	  cp = cos(phi);
	  
	  thistrack->chi2 = chi2;

	  thistrack->pos.x = 0.;
	  thistrack->pos.y = z0;
	  thistrack->pos.z = - d0/sp;
	  thistrack->dir.x = cl*sp;
	  thistrack->dir.y = sl;
	  thistrack->dir.z = cl*cp;

	  thistrack->weight = weight(thistrack);
	  /* fill in the X and V matrices: mostly 0. */

	  for (j=0; j<5; j++)
	    for (k=0; k<3; k++)
	      thistrack->X[j][k] = thistrack->V[j][k] = 0.;

	  thistrack->X[4][1] = 1.;
	  thistrack->X[2][2] = -d0 * cp/(sp*sp);
	  thistrack->X[3][2] = -1./sp;

	  thistrack->V[1][0] = -sl*sp;
	  thistrack->V[2][0] = cl*cp;
	  thistrack->V[1][1] = cl;
	  thistrack->V[1][2] = -sl*cp;
	  thistrack->V[2][2] = -cl*sp;

	  /* now copy over the covariance matrix */
	  thistrack->sig[0][0] = tber[i].c11;
	  thistrack->sig[0][1] = thistrack->sig[1][0] = tber[i].c12;
	  thistrack->sig[0][2] = thistrack->sig[2][0] = tber[i].c13;
	  thistrack->sig[0][3] = thistrack->sig[3][0] = tber[i].c14;
	  thistrack->sig[0][4] = thistrack->sig[4][0] = tber[i].c15;
	  thistrack->sig[1][1] = tber[i].c22;
	  thistrack->sig[1][2] = thistrack->sig[2][1] = tber[i].c23;
	  thistrack->sig[1][3] = thistrack->sig[3][1] = tber[i].c24;
	  thistrack->sig[1][4] = thistrack->sig[4][1] = tber[i].c25;
	  thistrack->sig[2][2] = tber[i].c33;
	  thistrack->sig[2][3] = thistrack->sig[3][2] = tber[i].c34;
	  thistrack->sig[2][4] = thistrack->sig[4][2] = tber[i].c35;
	  thistrack->sig[3][3] = tber[i].c44;
	  thistrack->sig[3][4] = thistrack->sig[4][3] = tber[i].c45;
	  thistrack->sig[4][4] = tber[i].c55;
	} else {  /* error in TBER bank or no track */
	  if (thistrack) {    /* failed because couldn't get TBER bank */
	    free(thistrack);
	    thistrack = NULL;
	    if (!ret) ret = -3; /* set error value if not already set */
	  }
	}
      
	if (thistrack) {             /* track was created okay. */
	  if (oldtrack != NULL) {
	    oldtrack->next = thistrack;
	  } else {               /* this was our first real track */
	    toptrack = thistrack;    /* put it at the top of the list */
	  }
	  oldtrack = thistrack;
	}

      } /* end of if track in desired sector */

      if (ret) break;
    }
  }
  /* make sure that we return a valid track */
  *track = oldtrack;
  return ret;
}

float get_trsigma(track_t *track, hit_t *hit, vector3_t wpos, vector3_t wdir) {
  /* recover the track sigma. if it has already been calculated, just return
   * it, else call tracksigma and have it calculated */
  /* inputs: track, hit, wpos, wdir
   * returns: sigma due to track uncertainty
   */


  if (hit->trsigma < 0.) /* we have not calculated the track sigma for this hit yet */
    hit->trsigma = tracksigma(track,wpos,wdir);

  return hit->trsigma;
}
      
    

float tracksigma(track_t *track, vector3_t wpos, vector3_t wdir) {
  /* calculate the sigma^2 of the doca due to the uncertainty of the track */
  
  vector3_t xdif, v1;
  vector3_t wdirxv1;
  float mag2_wxv1,mag_wxv1;
  static vector3_t e[3] = { {1., 0., 0.},  /* our unit vectors */
			    {0., 1., 0.},
			    {0., 0., 1.} };
  float q[5],Dv[3],Dx[3],q1[5],q2[5];

  int i;
  float sigma_sqr;

  xdif = v3sub(track->pos,wpos);
  v1 = track->dir;
  
  wdirxv1 = v3cross(wdir,v1);           /* cross product of wire and track dir */
  mag2_wxv1 = v3magsq(wdirxv1);
  mag_wxv1 = sqrt(mag2_wxv1);
  
  /* calculate the partial der. of doca wrt x1 and v1 (track pos and dir at start) */
  for(i=0; i<3; i++) {
    Dx[i] = v3dot(wdirxv1,e[i]) / mag_wxv1;       /* partial wrt x1 */
    Dv[i] = v3dot(xdif,  v3add(  v3mult(mag2_wxv1,  v3cross(wdir,e[i])),
				 v3mult(  v3dot(wdir,v1)*v3dot(wdir,e[i]),wdirxv1))
		  )/(mag2_wxv1*mag_wxv1);          /* partial wrt v1 */
  }
  
  fmat_mult(&track->X[0][0],&Dx[0],&q1[0],5,3,1);
  fmat_mult(&track->V[0][0],&Dv[0],&q2[0],5,3,1);
  
  /* this is now d(doca)/d(q), q parameters of track from fitting */
  fmat_add(&q1[0],&q2[0],&q[0],5,1);
  
  /* finally, find sigmadoca for the track parameters, using covariance matrix */
  fmat_mult(&q[0],&track->sig[0][0],&q1[0],1,5,5);       /* Qt * SIGMA */
  fmat_mult(&q1[0],&q[0],&sigma_sqr,1,5,1);
  
  return FMAX(sigma_sqr,0.05);
}
