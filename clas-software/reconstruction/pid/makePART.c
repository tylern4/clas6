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
#include <particleType.h>

/*_begin_doc
 *  RCS ID string
 *  $Id: makePART.c,v 1.30 2003/11/03 16:59:05 mikewood Exp $*/
#define USE(var) static void * use_##var = (void *) &var
static char crcsid[] = 
   "$Id: makePART.c,v 1.30 2003/11/03 16:59:05 mikewood Exp $";
USE(crcsid);   /* make sure it is not optimized away */
/* END of RCS information*/

#define SIGMA_ETRK  0.01; /* momentum resolution for eletron ID*/
#define ETOT_EL_CUT  3.0; /* number of sigmas for EC total electron cut */
#define EIO_EL_CUT  3.0; /* number of sigmas for EC (in-out) electron cut */

#define EC_IN_CUT        0.045  /* Cuts out pions. Scale by ~3.73 to get electron energy in Gev */
#define E_OVER_P_CUT     0.7    /* also gets pions, and nonfiducial electrons as well */
#define E_P_SLOPE_CUT   3.23
#define E_P_INT_LOW_CUT -0.26
#define E_P_INT_HIGH_CUT 0.56
#define PROTON_MASS_LCUT 0.8    /* lower mass cut for protons GeV */ 
#define PROTON_MASS_UCUT 1.2    /* upper mass cut for protons GeV */ 
#define PION_MASS_CUT    0.3    /* GeV */

#define KAON_MASS_LCUT   0.35   /* GeV */
#define KAON_MASS_UCUT   0.65    /* GeV */
#define DEUTERON_MASS_LCUT 1.75   /* GeV */
#define DEUTERON_MASS_HCUT 2.2   /* GeV */

#define NEUTRON_BETA_HCUT 0.9   /* GeV */

int ElectronID(bid_t * , int tbid_index, part_t *);
int PositronID(bid_t * , int tbid_index, part_t *);
int HadronID  (bid_t * , int tbid_index, part_t *);
int GammaID   (bid_t * , int tbid_index, part_t *);
int make_part_bank_();
int make_part_group_(int *groupno);


/* for backward compatability */
int make_PART_bank(){
  if (make_PART_group(0)){
    return 1;
  }
  return 0;
}

clasPART_t *make_PART_group(int group){
  /*input: RUNC, TBID, TBTR, ECHB, EC1R, SCR, and CC01 banks
    output: PART bank  -> pointers to TBID bank*/
  int npart=0, i;
  clasBID_t *TBID=NULL;
  clasRUNC_t *RUNC=getBank(&wcs_,"RUNC");
  clasPART_t *PART=NULL;
  clasTBTR_t *TBTR= (clasTBTR_t *) getBank(&bcs_,"TBTR");
  part_t part[PART_SIZE];

  if ((TBID = (clasBID_t *)getGroup(&bcs_, "TBID", group))&&RUNC){
    for (npart=0; npart < TBID->bank.nrow; npart++){
      if(npart >= PART_SIZE) {    /* defensive error trap */
	fprintf(stderr, "make_PART_bank: PART_SIZE needs to be increased. %d \n", npart);
	continue;
      }
      part[npart].pid = Unknown;
      if (RUNC->runc.beam.type.val.i[0]==ELECTRON_RUN){
	/* Only try to do electron identification for electron-beam runs */
	ElectronID(&TBID->bid[npart], npart, &part[npart]);
	/* electrons      */
      }
      else if (RUNC->runc.beam.type.val.i[0] == G7_RUN) {
	if (TBTR) {
	  if (TBTR->tbtr[TBID->bid[npart].track-1].q < 0) {
	    ElectronID(&TBID->bid[npart], npart, &part[npart]);
	  }
	  else if (TBTR->tbtr[TBID->bid[npart].track-1].q > 0) {
	    PositronID(&TBID->bid[npart], npart, &part[npart]);
	  }
	}
      }
       if(!part[npart].pid) HadronID(&TBID->bid[npart], npart, &part[npart]);  /* charged hadrons*/
      if(!part[npart].pid) GammaID(&TBID->bid[npart], npart, &part[npart]);   /* photons        */
       if(!part[npart].pid && TBID->bid[npart].track && \
	 (TBTR = getBank(&bcs_, "TBTR"))) {               /* default        */
	int tbtr_index = TBID->bid[npart].track-1;
	/* get_sc_mass still uses old tbid_t */
	float sc_mass = get_sc_mass((tbid_t *)(&TBID->bid[npart]), 
				    &TBTR->tbtr[tbtr_index]);  
	trk2part(&TBID->bid[npart], TBTR, sc_mass, &part[npart]);
	part[npart].qpid = sc_mass;
	part[npart].trkid = npart + 1; /*pointer to TBID bank, FORTRAN style*/
      }
    }
  }

  if (npart){
    if(PART = makeBank(&bcs_, "PART",group,sizeof(part_t)/4,npart)){
      for(i=0; i < npart; i++) PART->part[i] = part[i];
    }
    
  }
  return PART;
}
/*---------------------------------------------------------------------------*/
/* predicted value of electron energy from EC total */
float predict_e(vector3_t pvec){
  float p = v3mag(pvec);
  if (p < 1.0){
    return(.23*p + .071*p*p - .032*p*p*p);
  }
  return(p/EC_MAGIC_NUMBER);  
}
/*---------------------------------------------------------------------------*/
/* predicted value of electron energy from EC inner and outer */
float predict_e_in_out(vector3_t pvec){
  float p = v3mag(pvec);
  return(.03 + .105*p - .025*p*p);
}
/*---------------------------------------------------------------------------*/
/* predicted value of sigma ofelectron energy from EC total */
float predict_sigma_e(vector3_t pvec){
  float p = v3mag(pvec);
  return(sqrt(p)*(.08 + .0013*p));
}
/*---------------------------------------------------------------------------*/
/* predicted value of sigma of electron energy from EC inner and outer */
float predict_sigma_e_in_out(vector3_t pvec){
  float p = v3mag(pvec);
  return(.0634*p - .00022*p*p);
}
/*---------------------------------------------------------------------------*/
float gamma_energy(echb_t *echb){
  float e_deposit;

  /*if (echb->e__hit > .3){ the behavior of high energy photons is linear*/
  /*return(3.15*(echb->e__hit) + 0.12);*/

  return(EC_MAGIC_NUMBER*echb->e__hit);

    /*} else {
    e_deposit = (-23.0/144.0) + (1.0/144.0)*sqrt(529.0 + 28800.0*echb->e__hit);
    fprintf(stderr, "e_deposit: %f\n", e_deposit);
    return (e_deposit);
  }*/
}
/*---------------------------------------------------------------------------*/
/*FORTAN index - that's my convention*/
int ECHBmatch2TBID(int ec_index, clasTBID_t *TBID){
  int i = 0;

  for(i=0; i < TBID->bank.nrow; i++){
    if ((TBID->tbid[i].ec_id == ec_index) &&
	(TBID->tbid[i].ec_stat)) return (1);
  }
  return(0);
}
/*---------------------------------------------------------------------------*/
float echb_close2trk(echb_t *echb, clasTBID_t *TBID){
  int i;
  clasTBTR_t *TBTR = getBank(&bcs_, "TBTR");
  float dist = 999999.0;
  float near = 999999.0;
  
  if (TBTR){
    for(i=0; i < TBID->bank.nrow; i++){
      if(TBID->tbid[i].track){              /* this tbid has a track */
	tbtr_t *tbtr = &(TBTR->tbtr[TBID->tbid[i].track-1]);
	dist = dist_echb_tdpl(echb, tbtr2tdpl(tbtr) );
	if (dist < near) near = dist;
      }
    }
  }
  return(near);
}
/*---------------------------------------------------------------------------*/
float echb_close2tbtr(echb_t *echb){
  int i;
  clasTBTR_t *TBTR = getBank(&bcs_, "TBTR");
  float dist = 999999.0;
  float near = 999999.0;

  if(TBTR){
    for(i=0; i<TBTR->bank.nrow; i++){
      dist = dist_echb_tdpl(echb, tbtr2tdpl(&TBTR->tbtr[i]));
      if (dist < near) near = dist;
    }
  }
  return near;
}/*---------------------------------------------------------------------------*/
float ec1r_close2tbtr(ec1r_t *ec1r){
  int i;
  clasTBTR_t *TBTR = getBank(&bcs_, "TBTR");
  float dist = 999999.0;
  float near = 999999.0;

  if(TBTR){
    for(i=0; i<TBTR->bank.nrow; i++){
      tdpl_t *tdpl=tbtr2tdpl(&(TBTR->tbtr[i]));
      dist=v3mag(v3sub(ec1r->pos,tdpl[LACPlane].pos));
      if (dist < near) near = dist;
    }
  }
  return near;
}

/*----------------------------------------------------------------------------------------------*/
int trk2part(bid_t *tbid, clasTBTR_t *TBTR, float mass, part_t *part){
  int trkptr = tbid->track - 1;

  if(TBTR){
      part->vert = TBTR->tbtr[trkptr].vert;
      part->p = v3_to_v4(&TBTR->tbtr[trkptr].p, mass);
      part->q = TBTR->tbtr[trkptr].q;
      part->qtrk = TBTR->tbtr[trkptr].chi2;
  }
}
/*--------------------------------------------------------------------------*/
int echb2part(echb_t *echb, float mass, part_t *part){
  float mag;
  float energy = gamma_energy(echb);
  vector3_t vert;
  clasMVRT_t *MVRT = NULL;

  /*get particle vertex from MVRT. liji*/
  if(MVRT = (clasMVRT_t *)getBank(&bcs_, "MVRT")){
    vert = MVRT->mvrt[0].vert;
  }else{
    vert.x = 0.0;
    vert.y = 0.0;
    vert.z = target_z();
  }/*liji*/

  mag = sqrt((echb->x_hit-vert.x)*(echb->x_hit-vert.x) + (echb->y_hit-vert.y)*(echb->y_hit-vert.y) + (echb->z_hit-vert.z)*(echb->z_hit-vert.z));
  part->p.space.x = energy*(echb->x_hit-vert.x)/mag;
  part->p.space.y = energy*(echb->y_hit-vert.y)/mag; 
  part->p.space.z = energy*(echb->z_hit-vert.z)/mag;
  part->p.t = energy;
  part->q = 0;

}

/*--------------------------------------------------------------------------*/
int ec1r2part(ec1r_t *ec1r, float mass, part_t *part){
  float mag;
  float energy = ec1r->e_tot;
  vector3_t vert;
  clasMVRT_t *MVRT = NULL;
  
  /*get particle vertex from MVRT. liji*/
  if(MVRT = (clasMVRT_t *)getBank(&bcs_, "MVRT")){
    vert = MVRT->mvrt[0].vert;
  }else{
    vert.x = 0.0;
    vert.y = 0.0;
    vert.z = target_z();
  }/*liji*/
  
  mag = sqrt((ec1r->pos.x-vert.x)*(ec1r->pos.x-vert.x) + (ec1r->pos.y-vert.y)*(ec1r->pos.y-vert.y) + (ec1r->pos.z-vert.z)*(ec1r->pos.z-vert.z));
  /*mag = v3mag(ec1r->pos);
    part->p.space = v3mult(energy/mag,ec1r->pos);*/
  part->p.space.x = energy*(ec1r->pos.x-vert.x)/mag;
  part->p.space.y = energy*(ec1r->pos.y-vert.y)/mag; 
  part->p.space.z = energy*(ec1r->pos.z-vert.z)/mag;
  part->p.t = energy;
  part->q = 0;

}

/*--------------------------------------------------------------------------*/
int ElectronID_old(bid_t * tbid, int tbid_index, part_t * part){
  int tbtr_index = tbid->track-1;
  int echb_index = tbid->ec.id -1;
  int e_p_cut=0;
  float e_p;
  float ECEnergy, Mo;
  clasECHB_t * ECHB = getBank(&bcs_, "ECHB");
  clasTBTR_t * TBTR = getBank(&bcs_, "TBTR");
  echb_t * ECIn  = NULL;
  echb_t * ECOut = NULL;

  part->pid = Unknown;
  /* require  - charged track, match to ec & cc, ECHB & TBTR banks */
  if (ECHB && TBTR && (TBTR->tbtr[tbtr_index].q < 0.0) && tbid->track && tbid->ec.stat && 
      tbid->cc.stat){
    ec_Whole2InOut(&ECHB->echb[echb_index], &ECIn, &ECOut);
    ECEnergy = ECHB->echb[echb_index].e__hit;
    Mo = v3mag(TBTR->tbtr[tbtr_index].p);
    e_p_cut = ((ECEnergy*E_P_SLOPE_CUT + E_P_INT_LOW_CUT) < Mo) && 
      (Mo < (ECEnergy*E_P_SLOPE_CUT + E_P_INT_HIGH_CUT));
    
    /* if ec match was fiducial (GOOD_MATCH), use ec energy for cut */
    if ((tbid->ec.stat==GOOD_MATCH && (e_p_cut && ECIn && (ECIn->e__hit > EC_IN_CUT))) || 
	(tbid->ec.stat==NONFIDUCIAL_MATCH)){
      part->pid = Electron;
      part->trkid = tbid_index + 1; /*pointer to TBID bank, FORTRAN style*/
      trk2part(tbid, TBTR, ELECTRON_MASS, part);
      part->qpid = ECEnergy; 
      part->flags = tbid->ec.stat;   /* use ec hit matching status so nonfiducials are flagged */
    }
  }
  return 1;
}

/*--------------------------------------------------------------------------*/
int ElectronID(bid_t * tbid, int tbid_index, part_t * part){
  int tbtr_index = tbid->track-1;
  int echb_index = tbid->ec.id -1;
  int etot_cut = 0;
  int eio_cut = 0;
  int e_p_cut=0;
  float e_p;
  float ECEnergy, Mo;
  float predicted_ECEnergy, predicted_EC_in_out;
  float sigma_e_whole, sigma_e_in_out;
  float sigma_e_tot, sigma_e_io;
  float diff_e_in_out;
  float factor, sigma_mo,sigma_etot_cut, sigma_eio_cut;
  clasECHB_t * ECHB = getBank(&bcs_, "ECHB");
  clasTBTR_t * TBTR = getBank(&bcs_, "TBTR");
  echb_t * ECIn  = NULL;
  echb_t * ECOut = NULL;

  part->pid = Unknown;
  /* require  - charged track, match to ec & cc, ECHB & TBTR banks */
  /*  if (ECHB && TBTR && (TBTR->tbtr[tbtr_index].q < 0.0) && tbid->track && tbid->ec.stat && tbid->cc.stat){ */
  if (ECHB && TBTR && (TBTR->tbtr[tbtr_index].q < 0.0) && tbid->ec.stat && tbid->cc.stat){
    /* magnitude of momentum from tracking */
    Mo = v3mag(TBTR->tbtr[tbtr_index].p);
    /* create banks for EC inner and outer out of ECHB bank */
    ec_Whole2InOut(&ECHB->echb[echb_index], &ECIn, &ECOut);
    /* energy of EC total */
    ECEnergy = ECHB->echb[echb_index].e__hit;

    if(ECIn || ECOut){
      /* energy between EC inner and EC outer deposited energies */
      if(ECIn && !ECOut){
	diff_e_in_out = ECIn->e__hit;
      }
      if(!ECIn && ECOut){
	diff_e_in_out = -ECOut->e__hit;
      }
      if(ECIn && ECOut){
	diff_e_in_out = (ECIn->e__hit - ECOut->e__hit);
      }
      //      printf("Mo %f, ECe %f, diff_eio %f\n",Mo,ECEnergy,diff_e_in_out);

      /* calculate predicted values for total energy and (inner-outer) */ 
      predicted_ECEnergy = predict_e(TBTR->tbtr[tbtr_index].p);
      predicted_EC_in_out = predict_e_in_out(TBTR->tbtr[tbtr_index].p);
      /* calculate predicted values for sigmas of the above quantities */ 
      sigma_e_whole = predict_sigma_e(TBTR->tbtr[tbtr_index].p);
      sigma_e_in_out = predict_sigma_e_in_out(TBTR->tbtr[tbtr_index].p);

      //    sigma_e_tot = sqrt(pow(SIGMA_ETRK*Mo,2) + pow(sigma_e_whole,2));
      sigma_mo = Mo*SIGMA_ETRK;
      sigma_e_tot = sqrt(pow(factor,2) + pow(sigma_e_whole,2));
      sigma_e_io = sqrt(pow(factor,2) + pow(sigma_e_in_out,2));

      sigma_etot_cut = sigma_e_tot*ETOT_EL_CUT;
      sigma_eio_cut = sigma_e_io*EIO_EL_CUT;
      /* diff of measured and predicted tot. energy must be <= sigma_e_tot */ 
      etot_cut = (fabs(ECEnergy-predicted_ECEnergy) <= sigma_etot_cut);
      /* diff of measured and predicted (in-out) energy must be <= sigma_e_io */ 
      eio_cut = (fabs(diff_e_in_out-predicted_EC_in_out) <= sigma_eio_cut);
      /* electron cut based on EC energies */
      e_p_cut = (etot_cut && eio_cut);
    }
    
    /* if ec match was fiducial (GOOD_MATCH), use ec energy for cut */
    /*    if ((tbid->ec.stat==GOOD_MATCH && (e_p_cut && ECIn && (ECIn->e__hit > EC_IN_CUT))) || (tbid->ec.stat==NONFIDUCIAL_MATCH)){*/
    if (e_p_cut){
      part->pid = Electron;
      part->trkid = tbid_index + 1; /*pointer to TBID bank, FORTRAN style*/
      trk2part(tbid, TBTR, ELECTRON_MASS, part);
      part->qpid = ECEnergy; 
      part->flags = tbid->ec.stat;   /* use ec hit matching status so nonfiducials are flagged */
    }
    }
  return 1;
}

/*--------------------------------------------------------------------------*/
int PositronID(bid_t * tbid, int tbid_index, part_t * part){
  int tbtr_index = tbid->track-1;
  int echb_index = tbid->ec.id -1;
  int etot_cut = 0;
  int eio_cut = 0;
  int e_p_cut=0;
  float e_p;
  float ECEnergy, Mo;
  float predicted_ECEnergy, predicted_EC_in_out;
  float sigma_e_whole, sigma_e_in_out;
  float sigma_e_tot, sigma_e_io;
  float diff_e_in_out;
  float factor, sigma_mo,sigma_etot_cut, sigma_eio_cut;
  clasECHB_t * ECHB = getBank(&bcs_, "ECHB");
  clasTBTR_t * TBTR = getBank(&bcs_, "TBTR");
  echb_t * ECIn  = NULL;
  echb_t * ECOut = NULL;

  part->pid = Unknown;
  /* require  - charged track, match to ec & cc, ECHB & TBTR banks */
  /*  if (ECHB && TBTR && (TBTR->tbtr[tbtr_index].q > 0.0) && tbid->track && tbid->ec.stat && tbid->cc.stat){ */
  if (ECHB && TBTR && (TBTR->tbtr[tbtr_index].q > 0.0) && tbid->ec.stat && tbid->cc.stat){
    /* magnitude of momentum from tracking */
    Mo = v3mag(TBTR->tbtr[tbtr_index].p);
    /* create banks for EC inner and outer out of ECHB bank */
    ec_Whole2InOut(&ECHB->echb[echb_index], &ECIn, &ECOut);
    /* energy of EC total */
    ECEnergy = ECHB->echb[echb_index].e__hit;

    if(ECIn || ECOut){
      /* energy between EC inner and EC outer deposited energies */
      if(ECIn && !ECOut){
	diff_e_in_out = ECIn->e__hit;
      }
      if(!ECIn && ECOut){
	diff_e_in_out = -ECOut->e__hit;
      }
      if(ECIn && ECOut){
	diff_e_in_out = (ECIn->e__hit - ECOut->e__hit);
      }
      //      printf("Mo %f, ECe %f, diff_eio %f\n",Mo,ECEnergy,diff_e_in_out);

      /* calculate predicted values for total energy and (inner-outer) */ 
      predicted_ECEnergy = predict_e(TBTR->tbtr[tbtr_index].p);
      predicted_EC_in_out = predict_e_in_out(TBTR->tbtr[tbtr_index].p);
      /* calculate predicted values for sigmas of the above quantities */ 
      sigma_e_whole = predict_sigma_e(TBTR->tbtr[tbtr_index].p);
      sigma_e_in_out = predict_sigma_e_in_out(TBTR->tbtr[tbtr_index].p);

      //    sigma_e_tot = sqrt(pow(SIGMA_ETRK*Mo,2) + pow(sigma_e_whole,2));
      sigma_mo = Mo*SIGMA_ETRK;
      sigma_e_tot = sqrt(pow(factor,2) + pow(sigma_e_whole,2));
      sigma_e_io = sqrt(pow(factor,2) + pow(sigma_e_in_out,2));

      sigma_etot_cut = sigma_e_tot*ETOT_EL_CUT;
      sigma_eio_cut = sigma_e_io*EIO_EL_CUT;
      /* diff of measured and predicted tot. energy must be <= sigma_e_tot */ 
      etot_cut = (fabs(ECEnergy-predicted_ECEnergy) <= sigma_etot_cut);
      /* diff of measured and predicted (in-out) energy must be <= sigma_e_io */ 
      eio_cut = (fabs(diff_e_in_out-predicted_EC_in_out) <= sigma_eio_cut);
      /* electron cut based on EC energies */
      e_p_cut = (etot_cut && eio_cut);
    }
    
    /* if ec match was fiducial (GOOD_MATCH), use ec energy for cut */
    /*    if ((tbid->ec.stat==GOOD_MATCH && (e_p_cut && ECIn && (ECIn->e__hit > EC_IN_CUT))) || (tbid->ec.stat==NONFIDUCIAL_MATCH)){*/
    if (e_p_cut){
      part->pid = Positron;
      part->trkid = tbid_index + 1; /*pointer to TBID bank, FORTRAN style*/
      trk2part(tbid, TBTR, ELECTRON_MASS, part);
      part->qpid = ECEnergy; 
      part->flags = tbid->ec.stat;   /* use ec hit matching status so nonfiducials are flagged */
    }
  }
  return 1;
}

/*--------------------------------------------------------------------------*/
int PositronID_old(bid_t * tbid, int tbid_index, part_t * part){
  int tbtr_index = tbid->track-1;
  int echb_index = tbid->ec.id -1;
  int e_p_cut=0;
  float e_p;
  float ECEnergy, Mo;
  clasECHB_t * ECHB = getBank(&bcs_, "ECHB");
  clasTBTR_t * TBTR = getBank(&bcs_, "TBTR");
  echb_t * ECIn  = NULL;
  echb_t * ECOut = NULL;

  part->pid = Unknown;
  /* require  + charged track, match to ec & cc, ECHB & TBTR banks */
  if (ECHB && TBTR && (TBTR->tbtr[tbtr_index].q > 0.0) && tbid->track && tbid->ec.stat && tbid->cc.stat){
    ec_Whole2InOut(&ECHB->echb[echb_index], &ECIn, &ECOut);
    ECEnergy = ECHB->echb[echb_index].e__hit;
    Mo = v3mag(TBTR->tbtr[tbtr_index].p);
    e_p_cut = ((ECEnergy*E_P_SLOPE_CUT + E_P_INT_LOW_CUT) < Mo) && 
      (Mo < (ECEnergy*E_P_SLOPE_CUT + E_P_INT_HIGH_CUT));
    
    /* if ec match was fiducial (GOOD_MATCH), use ec energy for cut */
    if ((tbid->ec.stat==GOOD_MATCH && (e_p_cut && ECIn && (ECIn->e__hit > EC_IN_CUT))) || 
	(tbid->ec.stat==NONFIDUCIAL_MATCH)){
      part->pid = Positron;
      part->trkid = tbid_index + 1; /*pointer to TBID bank, FORTRAN style*/
      trk2part(tbid, TBTR, ELECTRON_MASS, part);
      part->qpid = ECEnergy; 
      part->flags = tbid->ec.stat;   /* use ec hit matching status so nonfiducials are flagged */
    }
    }
  return 1;
}

/*--------------------------------------------------------------------------*/
int HadronID(bid_t * tbid, int tbid_index, part_t * part){
  int tbtr_index = tbid->track-1;
  clasTBTR_t * TBTR;
  float sc_mass;

  part->pid = Unknown;
  if(tbid->track && (TBTR = getBank(&bcs_, "TBTR"))){
    sc_mass = get_sc_mass((tbid_t *)tbid, &(TBTR->tbtr[tbtr_index]));
    if (sc_mass <= PION_MASS_CUT){
      if(TBTR->tbtr[tbtr_index].q < 0.0) {
	part->pid = PiMinus;
      }else if(TBTR->tbtr[tbtr_index].q > 0.0) {
	part->pid = PiPlus;
      }
      trk2part(tbid, TBTR, PI_CHARGED_MASS, part); 
    }
    if(sc_mass > KAON_MASS_LCUT && sc_mass < KAON_MASS_UCUT){
      if(TBTR->tbtr[tbtr_index].q < 0.0) part->pid = KMinus;
      else if(TBTR->tbtr[tbtr_index].q > 0.0) part->pid = KPlus;
      trk2part(tbid, TBTR, KAON_CHARGED_MASS, part); 
    }
    if (sc_mass > PROTON_MASS_LCUT && sc_mass <= PROTON_MASS_UCUT){
      if (TBTR->tbtr[tbtr_index].q > 0.0) part->pid = Proton;
      else if(TBTR->tbtr[tbtr_index].q < 0.0) part->pid = AntiProton;
      trk2part(tbid, TBTR, PROTON_MASS, part); 
    }
    if (sc_mass > DEUTERON_MASS_LCUT && sc_mass < DEUTERON_MASS_HCUT
	&& TBTR->tbtr[tbtr_index].q > 0.0){
      part->pid = Deuteron;
      trk2part(tbid, TBTR, DEUTERON_MASS, part); 
    }
    if(part->pid){
      part->trkid = tbid_index + 1; /*pointer to TBID bank, FORTRAN style*/
      part->qpid = sc_mass; 
      part->flags = tbid->sc.stat;
    }
  }
  return 1;
}
/*---------------------------------------------------------------------------*/
int GammaID(bid_t * tbid, int tbid_index, part_t * part){
  int IFail = 0;
  vector3_t zero3 = {0.0,0.0,0.0};
  float gamma=0;
  float p, mag;
  clasECHB_t *ECHB = (clasECHB_t *)getBank(&bcs_, "ECHB");
  echb_t *echb;
  clasEC1R_t *EC1R = getGroup(&bcs_,"EC1R",tbid->sec);
  clasMVRT_t *MVRT = NULL;

  /*look for MVRT bank. liji*/
  if(MVRT = (clasMVRT_t *)getBank(&bcs_, "MVRT")){
    zero3 = MVRT->mvrt[0].vert;
  }else{
    /*get target center position form TGEO bank. liji*/
    zero3.z = target_z(); 
  }

  /* find neutrals in TBID bank from EC -- either photons or neutrons */
  if(!tbid->track && tbid->ec.stat && ECHB){

    /* Neutron ID -- just a beta cut */
    if (tbid->ec.beta>0. && tbid->ec.beta<NEUTRON_BETA_HCUT) {
      part->pid = Neutron;
      p = NEUTRON_MASS*tbid->ec.beta*beta2gamma(tbid->ec.beta);
      /* get the direction from echb bank, then scale it appropriately */
      echb2part(&(ECHB->echb[tbid->ec.id - 1]), 0, part);
      mag = p/v3mag(part->p.space);
      part->p.space = v3mult(mag,part->p.space);
      part->p.t = p/tbid->ec.beta;
    } else {
      /* it is a photon */
      part->pid = Gamma;
      echb2part(&(ECHB->echb[tbid->ec.id - 1]),0, part);
    }

    part->trkid = tbid_index + 1;
    part->vert = zero3;
    part->qpid = tbid->ec.qual;
    part->qtrk = 0.0;
    part->flags = tbid->ec.stat;
    IFail = 1;
  }

  else if(!tbid->track && tbid->lac.stat && EC1R){ 
    if (tbid->lac.beta>0. && tbid->lac.beta<NEUTRON_BETA_HCUT) {
      part->pid = Neutron;
      p = NEUTRON_MASS*tbid->lac.beta*beta2gamma(tbid->lac.beta);
      /* get the direction from echb bank, then scale it appropriately */
      ec1r2part(&(EC1R->ec1r[tbid->lac.id - 1]),0, part);
      mag = p/v3mag(part->p.space);
      part->p.space = v3mult(mag,part->p.space);
      part->p.t = p/tbid->lac.beta;
    } else {
    /* protect against clusters appearing at the origin, don't know why the LAC code does this*/
/*        if (v3dot(EC1R->ec1r[tbid->lac.id -1].pos, \ */
/*  		EC1R->ec1r[tbid->lac.id -1].pos) > 0.0){ */
	part->pid = Gamma;
	ec1r2part(&(EC1R->ec1r[tbid->lac.id - 1]), 0.0, part);
    } 
      
    part->trkid = tbid_index + 1;
    part->vert = zero3;
    part->qpid = tbid->lac.qual;
    part->qtrk = 0.0;
    part->flags = tbid->lac.stat;
    IFail = 1;
  }	       
  
  return IFail;
}

int make_part_bank_(){
  make_PART_bank();
}

int make_part_group_(int *groupno){
  make_PART_group(*groupno);
}
