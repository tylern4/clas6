/*------------------sc_makeSCR.c---------------------------*/
/* Routines for creating the SCR scintillator result bank.  Inputs are the 
   SC1, SCP and SCG banks and the calibration global variables.  Some hits
   are recovered using HDPL.
   ROUTINES:
      make_SCR_bank(s):  This routine loops over all the hits in the SC1 bank
                         for sector s and fills the scrarr local array.  If 
			 there are entries in this array, the SCR bank for 
			 sector s is created.
      make_SCR_hit:      This is the main routine for calculating the energies,
                         times, and positions of the hits in the TOF. Depending
			 on the value of valid_SC_hit, the calculation for 
			 the energy or the time may vary.
      valid_SC1_hit:	 Determines the status word for a scintillator hit.
                         Each scintillator hit contains entries for 2 tdcs 
			 and 2 adcs; not all of these are guaranteed to be 
			 present.  The status word is defined as follows:
			 Value:     Mneumonic:        Meaning:
			   1        TDCL_GOOD         Only tdcl 
			   2        ADCL_GOOD         Only adcl.  Discarded.
			   3        LEFT_OK           tdcl,adcl
			   4        TDCR_GOOD         Only tdcr
			   5        TDCS_ONLY         tdcl,tdcr
			   6        ADCL_TDCR         Self-evident!
			   7        LEFT_OK_NO_ADCR   tdcl,adcl,tdcr
			   8        ADCR_GOOD         Only adcr.  Discarded.
                           9        TDCL_ADCR         Self-evident!
			   10       ADCS_ONLY         adcl,adcr.  Discarded.
			   11       LEFT_OK_NO_TDCR   tdcl,adcl,adcr
			   12       RIGHT_OK          tdcr,adcr
			   13       RIGHT_OK_NO_ADCL  tdcl,tdcr,adcr
			   14       RIGHT_OK_NO_TDCL  adcl,tdcr,adcr
			   15       BOTH_OK           tdcl,adcl,tdcr,adcr.
      get_SCR_time:      Calculates the time of the hit relative to the trigger
                         time for those hits that have both tdcl and tdcr.
      get_SCR_time_unc:  Calcuates the uncertainty in this time.
      get_SCR_energy:    Calculates the energy deposition of the hit for those
                         hits that have both adcl and adcr.
      get_SCR_energy_unc:  Calculates the uncertainty in this energy.
      get_distance_from_time:  Calculates the distance from the center of the
                         the scintillator of the hit using the left and right
			 times.
      get_distance_from_time_unc:  Calculates the uncertainty in this distance.
      get_distance_from_energy:  Calculates the distance from the center of the
                         scintillator of the hit using the left and right 
			 energies.
      get_distance_from_energy_unc:  Calculates the uncertainty in this 
                         distance.
      get_distance_from_dpl:  Attempts to match a track to the scintillator;
                         if a match is found then the hdpl y position is used.
      dpl2sc_id:        Track matching routine:  predicts which scintillator 
                        should have fired.  Works for both hit-based and time-
			based tracks.
*/
/*---------------------------------------------------------*/
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <math.h>
#include <bostypes.h>
#include <sc.h>
#include <scExtern.h>
#include <kinematics.h>
#include <utility.h>

#define NO_MATCH_TO_TRACK 1000.0

#define POW2(x) ((x)*(x))
#define POW4(x) ((x)*(x)*(x)*(x))

/* -=======================================================- *
$Id: sc_makeSCR.c,v 1.42 2008/03/16 04:33:22 fklein Exp $
$Author: fklein $
$Revision: 1.42 $
$Date: 2008/03/16 04:33:22 $
* -=======================================================- */

clasSCR_t *make_SCR_bank(int sector, const char *dpl){
  /*input: SC1 bank, SCG bank, HBTR bank 
    output: SCR bank */
  clasSC1_t *SC1=NULL;
  clasSCG_t *SCG=NULL;
  clasSCR_t *SCR=NULL;
  scr_t scrarr[SC_NPADDLES_SEC_TOTAL];
  int nSC1 = 0;
  int nSCR = 0;
  int status=0; 
 
  if ((SC1 = getGroup(&bcs_, "SC1 ", sector)) && 
      (SCG = getGroup(&wcs_, "SCG ", sector))){
    for(nSC1 = 0; nSC1 < SC1->bank.nrow; nSC1++){ 
      status=valid_SC1_hit(SC1->sc1[nSC1]);
      if(status>0 && status!=ADCS_ONLY && status!=ADCL_GOOD && status!=ADCR_GOOD) {
	scrarr[nSCR]=make_SCR_hit(SC1->sc1[nSC1],SCG,sector,status, dpl);
	nSCR++;
      }
    }
    if (nSCR){  
      if(SCR = makeBank(&bcs_, "SCR ",sector,sizeof(scr_t)/4,nSCR)){
	int i;
	for(i=0; i < nSCR; i++) SCR->scr[i] = scrarr[i];
      } 
    }
  }
  return SCR;
}


/* Determine the status of a given scintillator hit */
int valid_SC1_hit(sc1_t sc1){
  int status=0;
  if(sc1.time_l > SC_TIME_UNDERFLOW && sc1.time_l < SC_TIME_OVERFLOW)
    status+=TDCL_GOOD;  
  if(sc1.energy_l > 0) status+=ADCL_GOOD;
  if(sc1.time_r > SC_TIME_UNDERFLOW && sc1.time_r < SC_TIME_OVERFLOW)
    status+=TDCR_GOOD;
  if(sc1.energy_r > 0) status+=ADCR_GOOD;
  return(status);
}


scr_t make_SCR_hit(sc1_t sc1,clasSCG_t *SCG,int sector,int status, const char *dpl){
  scr_t scr;
  vector3_t bar_center, bar_edge_l, bar_edge_r;
  vector3_t bar_direction, bar_postion, hit_position;
  vector3_t hit_location; /*hit location in the bar*/
  float distance;/* Distance along bar length between center and hit */
  float hitpos_z;
  int hdpl_id;
  vector3_t bar_center_hdpl;
  extern float paddle1[54];
  extern float paddle2[54];
  clasHDPL_t *HDPL=NULL;
  clasSCP_t *SCP=NULL;
  int index=sc_index(sector,sc1.id);
  extern sc_const_t sc_veff,sc_veffu;
  extern sc_const_t sc_lambda;
  extern float yoffset[SC_NPADDLES_TOTAL];
  HDPL=getGroup(&bcs_,dpl,sector);

  /* Get some information about the scintillator coordinates */
  bar_center=get_bar_center(SCG, sc1.id); /*CLAS coordinate sys*/
  bar_direction = get_bar_direction(SCG, sc1.id);/*CLAS coordinate sys*/

  memset((void *)&scr,0,sizeof(scr_t));
  if( !sc_veff.left[index] || !sc_veff.right[index] || !sc_lambda.left[index] || !sc_lambda.right[index] ) { printf("index %d: sc_veff, sc_lambda not initialized\n"); return(scr); } 
    
  scr.id=sc1.id;
  scr.status=status;
  switch(status){
  case TDCS_ONLY:
    distance=get_distance_from_time(sc1,sector);
    scr.err.y=get_distance_from_time_unc(sc1,sector);
    scr.time=get_SCR_time(sc1,sector,distance);
    scr.dtime=get_SCR_time_unc(sc1,sector);
    break;
  case LEFT_OK_NO_TDCR:
    if((distance=get_distance_from_dpl(sc1,sector,SCG,dpl,bar_center,
				       bar_direction))==NO_MATCH_TO_TRACK)
      {
	distance=get_distance_from_energy(sc1,sector);
	scr.err.y=get_distance_from_energy_unc(sc1,sector);
      }
    scr.energy=get_SCR_energy(sc1,sector,distance);
    scr.denergy=get_SCR_energy_unc(sector,sc1,distance,scr.err.y,status);
    scr.time=sc1.time_l-distance/sc_veff.right[index]-yoffset[index]/2;
    scr.dtime=sqrt(sc1.dtime_l*sc1.dtime_l 
		   +scr.err.y*scr.err.y/(sc_veff.right[index]*sc_veff.right[index])
		   +distance*distance*sc_veffu.right[index]
		   *sc_veffu.right[index]/POW4(sc_veff.right[index]));
    break;
  case LEFT_OK_NO_ADCR:
    distance=get_distance_from_time(sc1,sector);
    scr.err.y=get_distance_from_time_unc(sc1,sector);
    scr.time=get_SCR_time(sc1,sector,distance);
    scr.dtime=get_SCR_time_unc(sc1,sector);
    scr.energy=sc1.energy_l*exp(distance/sc_lambda.left[index]);
    scr.denergy=get_SCR_energy_unc(sector,sc1,distance,scr.err.y,status);
    break;
  case TDCL_GOOD:
    if((distance=get_distance_from_dpl(sc1,sector,SCG,dpl,bar_center,
				       bar_direction))==NO_MATCH_TO_TRACK)
      {
	distance=0.0;
	scr.err.y=get_bar_length(SCG,scr.id)/sqrt(12.0);
      }
    scr.time=sc1.time_l-distance/sc_veff.right[index]-yoffset[index]/2;
    scr.dtime=get_bar_length(SCG,scr.id)/sc_veff.left[index]/sqrt(12.0);
    break;
  case LEFT_OK:
    if((distance=get_distance_from_dpl(sc1,sector,SCG,dpl,bar_center,
				       bar_direction))==NO_MATCH_TO_TRACK)
      {
	distance=0.0;
	scr.err.y=get_bar_length(SCG,scr.id)/sqrt(12.0);
      }
    scr.time=sc1.time_l-distance/sc_veff.right[index]-yoffset[index]/2;
    scr.dtime=get_bar_length(SCG,scr.id)/sc_veff.left[index]/sqrt(12.0);
    scr.energy=sc1.energy_l*exp(distance/sc_lambda.left[index]);
    scr.denergy=0;
    break;
  case RIGHT_OK_NO_TDCL:
    if((distance=get_distance_from_dpl(sc1,sector,SCG,dpl,bar_center,
				       bar_direction))==NO_MATCH_TO_TRACK)
      {
	distance=get_distance_from_energy(sc1,sector);
	scr.err.y=get_distance_from_energy_unc(sc1,sector);
      }
    scr.energy=get_SCR_energy(sc1,sector,distance);
    scr.denergy=get_SCR_energy_unc(sector,sc1,distance,scr.err.y,status);
    scr.time=sc1.time_r+distance/sc_veff.right[index]+yoffset[index]/2;
    scr.dtime=sqrt(sc1.dtime_r*sc1.dtime_r 
		   +scr.err.y*scr.err.y/(sc_veff.right[index]*sc_veff.right[index])
		   +distance*distance*sc_veffu.right[index]
		   *sc_veffu.right[index]/POW4(sc_veff.right[index]));
    break;
  case RIGHT_OK_NO_ADCL:
    distance=get_distance_from_time(sc1,sector);
    scr.err.y=get_distance_from_time_unc(sc1,sector);
    scr.time=get_SCR_time(sc1,sector,distance);
    scr.dtime=get_SCR_time_unc(sc1,sector);
    scr.energy=sc1.energy_r*exp(-distance/sc_lambda.right[index]);
    scr.denergy=get_SCR_energy_unc(sector,sc1,distance,scr.err.y,status);
    break;
  case TDCR_GOOD:
    if((distance=get_distance_from_dpl(sc1,sector,SCG,dpl,bar_center,
				       bar_direction))==NO_MATCH_TO_TRACK)
      {
	distance=0.0;
	scr.err.y=get_bar_length(SCG,scr.id)/sqrt(12.0);
      }
    scr.time=sc1.time_r+distance/sc_veff.right[index]+yoffset[index]/2;
    scr.dtime=get_bar_length(SCG,scr.id)/sc_veff.right[index]/sqrt(12.0);
    scr.energy=0;
    scr.denergy=0;
    break;
  case RIGHT_OK:
    if((distance=get_distance_from_dpl(sc1,sector,SCG,dpl,bar_center,
				       bar_direction))==NO_MATCH_TO_TRACK)
      {
	distance=0.0;
	scr.err.y=get_bar_length(SCG,scr.id)/sqrt(12.0);
      }
    scr.time=sc1.time_r+distance/sc_veff.right[index]+yoffset[index]/2;
    scr.dtime=get_bar_length(SCG,scr.id)/sc_veff.right[index]/sqrt(12.0);
    scr.energy=sc1.energy_r*exp(-distance/sc_lambda.right[index]);
    scr.denergy=0;
    break;
  case ADCL_TDCR:
    if((distance=get_distance_from_dpl(sc1,sector,SCG,dpl,bar_center,
				       bar_direction))==NO_MATCH_TO_TRACK)
      distance=0.0;
    scr.err.y=get_bar_length(SCG,scr.id)/sqrt(12.0);
    scr.time=sc1.time_r+distance/sc_veff.right[index]+yoffset[index]/2;
    scr.dtime=get_bar_length(SCG,scr.id)/sc_veff.right[index]/sqrt(12.0);
    scr.energy=sc1.energy_l*exp(distance/sc_lambda.left[index]);
    scr.denergy=0;
    break;
  case TDCL_ADCR:
    if((distance=get_distance_from_dpl(sc1,sector,SCG,dpl,bar_center,
				       bar_direction))==NO_MATCH_TO_TRACK)
      {
	distance=0.0;
	scr.err.y=get_bar_length(SCG,scr.id)/sqrt(12.0);
      }
    scr.time=sc1.time_l-distance/sc_veff.left[index]-yoffset[index]/2;
    scr.dtime=get_bar_length(SCG,scr.id)/sc_veff.left[index]/sqrt(12.0);
    scr.energy=sc1.energy_r*exp(-distance/sc_lambda.right[index]);
    break;
  default:
    distance=get_distance_from_time(sc1,sector);
    scr.err.y=get_distance_from_time_unc(sc1,sector);
    scr.time=get_SCR_time(sc1,sector,distance);
    scr.dtime=get_SCR_time_unc(sc1,sector);
    scr.energy=get_SCR_energy(sc1,sector,distance);
    scr.denergy=get_SCR_energy_unc(sector,sc1,distance,scr.err.y,status);
    break;
  }
  /*  Routine that handles the coupled counter 40-48 from either the HPDL */
  /*  or the TDPL banks dependent on which is passed to the fuction */
	if(SC_VERSION_FLAG ==1 && HDPL){
		int track;
		
  		for(track=0;track<(HDPL->bank.nrow/10);track++){
			hdpl_id=dpl2sc_id(sector,&(HDPL->hdpl[track*10]));
			if(hdpl_id>=40&&hdpl_id<=48){
				if(hdpl_id>=40&&hdpl_id<=42)
					hitpos_z=HDPL->hdpl[scPlane3 + (track*10)].pos.z;
				else hitpos_z=HDPL->hdpl[scPlane4 + (track*10)].pos.z;
					bar_center_hdpl=get_bar_center(SCG,hdpl_id);
				if(bar_center_hdpl.z>hitpos_z)
					scr.time=scr.time+paddle1[9*(sector-1)+hdpl_id-40];
				else scr.time=scr.time+paddle2[9*(sector-1)+hdpl_id-40];
			}
		}
	}
     
  hit_location = v3mult(distance, bar_direction);/*CLAS coordinate sys*/
 
  /*determine the hit postion in sector coordinate system*/
  hit_position = v3add(hit_location, bar_center);
  hit_position = lab2sector(hit_position, sector);
  scr.pos = hit_position;
  return(scr);
}

/* Calculate the time from the average of Tl and Tr, correcting for the
   distance of the hit from the center along the paddle */
float get_SCR_time(sc1_t sc1,int sector,float distance){
  extern sc_const_t sc_veff;
  int index=sc_index(sector,sc1.id);

  return ((sc1.time_l + sc1.time_r)/2.0 
	  - distance*(sc_veff.right[index]-sc_veff.left[index])
	  /(2.0*sc_veff.left[index]*sc_veff.right[index]));
}

/* Calculate the uncertainty in the time for the case where both TDCs are
   present */
float get_SCR_time_unc(sc1_t sc1,int sector){
  float dtdvl,dtdvr,dtdtl,dtdtr;
  int index=sc_index(sector,sc1.id);
  extern sc_const_t sc_veff,sc_veffu;
  float vl=sc_veff.left[index];
  float vr=sc_veff.right[index];
  float dvl=sc_veffu.left[index];
  float dvr=sc_veffu.right[index];

  dtdvl=sc1.time_l*vr/((vl+vr)*(vl+vr));
  dtdvr=sc1.time_r*vl/((vl+vr)*(vl+vr));
  dtdtl=vr/(vl+vr);
  dtdtr=vl/(vl+vr);
  return(sqrt(dtdvl*dtdvl*dvl*dvl + dtdvr*dtdvr*dvr*dvr 
	      + dtdtl*dtdtl*sc1.dtime_l*sc1.dtime_l
	      + dtdtr*dtdtr*sc1.dtime_r*sc1.dtime_r));
}

/* Calculate the energy deposited from the calibrated ADCL and ADCR values,
   correcting for the distance of the hit from the center along the paddle */
float get_SCR_energy(sc1_t sc1,int sector,float distance){
  extern sc_const_t sc_lambda;
  int index=sc_index(sector,sc1.id);

  return( sqrt(sc1.energy_l * sc1.energy_r *
       exp(distance*(sc_lambda.right[index]-sc_lambda.left[index])
	   /(sc_lambda.left[index]*sc_lambda.right[index]))));
}

/* Calculate the uncertainty in the energy */
float get_SCR_energy_unc(int sector,sc1_t sc1,float y,float dy,int status){
  extern sc_const_t sc_lambda,sc_lambdau;
  float dEdy,dEdEl,dEdEr,dEdLl,dEdLr,ex,unc;
  int i=sc_index(sector,sc1.id);

  switch(status){
  case RIGHT_OK_NO_TDCL:
  case LEFT_OK_NO_TDCR:
    dEdEl=exp((sc_lambda.right[i]-sc_lambda.left[i]) / 2.0
	      / (sc_lambda.right[i]+sc_lambda.left[i]));
    dEdLr=sc1.energy_l * sc_lambda.left[i] * dEdEl 
      / (sc_lambda.left[i]+sc_lambda.right[i])
      / (sc_lambda.left[i]+sc_lambda.right[i]);
    dEdLl=-sc1.energy_l * sc_lambda.right[i] * dEdEl
      / (sc_lambda.left[i]+sc_lambda.right[i])
      / (sc_lambda.left[i]+sc_lambda.right[i]);
    unc=sqrt(dEdEl*dEdEl*sc1.denergy_l*sc1.denergy_l
	     + dEdLl*dEdLl*sc_lambdau.left[i]*sc_lambdau.left[i]
	     + dEdLr*dEdLr*sc_lambdau.right[i]*sc_lambdau.right[i]);
    break;
  case RIGHT_OK_NO_ADCL:
    dEdEr=exp(-y/sc_lambda.right[i]);
    dEdy=-dEdEr/sc_lambda.right[i];
    dEdLr=dEdEr*y/(sc_lambda.right[i]*sc_lambda.right[i]);
    unc=sqrt(dEdEr*dEdEr*sc1.denergy_r*sc1.denergy_r + dEdy*dEdy*dy*dy
	     + dEdLr*dEdLr*sc_lambdau.right[i]*sc_lambdau.right[i]);
    break;
  case LEFT_OK_NO_ADCR:
    dEdEl=exp(y/sc_lambda.left[i]);
    dEdy=dEdEl/sc_lambda.left[i];
    dEdLl=-dEdEl*y/(sc_lambda.left[i]*sc_lambda.left[i]);
    unc=sqrt(dEdEl*dEdEl*sc1.denergy_l*sc1.denergy_l + dEdy*dEdy*dy*dy
	     + dEdLl*dEdLl*sc_lambdau.left[i]*sc_lambdau.left[i]);
    break;
  default: 
    ex= exp(y*(sc_lambda.right[i]-sc_lambda.left[i])/
	    (2.0*sc_lambda.right[i]*sc_lambda.left[i]));
    dEdEl=0.5*sqrt(sc1.energy_r/sc1.energy_l)*ex;
    dEdEr=0.5*sqrt(sc1.energy_l/sc1.energy_r)*ex;
    dEdy=sqrt(sc1.energy_l*sc1.energy_r)*ex
      * (sc_lambda.left[i]-sc_lambda.right[i])
      / (2.0*sc_lambda.left[i]*sc_lambda.right[i]);
    dEdLr=sqrt(sc1.energy_l*sc1.energy_r)*ex*y
      /(2.0*sc_lambda.left[i]*sc_lambda.right[i]);
    dEdLl=-dEdLr;
    unc=sqrt(dEdEl*dEdEl*sc1.denergy_l*sc1.denergy_l + dEdy*dEdy*dy*dy
	     + dEdEr*dEdEr*sc1.denergy_r*sc1.denergy_r
	     + dEdLl*dEdLl*sc_lambdau.left[i]*sc_lambdau.left[i]
	     + dEdLr*dEdLr*sc_lambdau.right[i]*sc_lambdau.right[i]);
    break;
  }
  
 return unc;
}


/* Calculate the distance from the center of the hit using the time 
   difference between left and right */
float get_distance_from_time(sc1_t sc1,int sector){
  extern sc_const_t sc_veff;
  int index=sc_index(sector,sc1.id);

  return((sc1.time_l - sc1.time_r-yoffset[index])*sc_veff.left[index]*sc_veff.right[index]/
    (sc_veff.left[index]+sc_veff.right[index]));
}

float get_distance_from_time_unc(sc1_t sc1,int sector){
  float dydtl,dydtr,dydvl,dydvr;
  extern sc_const_t sc_veff;
  extern sc_const_t sc_veffu;
  int i=sc_index(sector,sc1.id);

  dydtl=sc_veff.left[i]*sc_veff.right[i]/(sc_veff.left[i]+sc_veff.right[i]);
  dydtr=-dydtl;
  dydvl=POW2(sc_veff.right[i]/(sc_veff.left[i]+sc_veff.right[i]))*
    (sc1.time_l-sc1.time_r);
  dydvr=POW2(sc_veff.left[i]/(sc_veff.left[i]+sc_veff.right[i]))*
    (sc1.time_l-sc1.time_r);

  return(sqrt(dydtl*dydtl*sc1.dtime_l*sc1.dtime_l 
	      + dydtr*dydtr*sc1.dtime_r*sc1.dtime_r
	      + dydvl*dydvl*sc_veffu.left[i]*sc_veffu.left[i]
	      + dydvr*dydvr*sc_veffu.right[i]*sc_veffu.right[i]));
}


/* Calculate the distance from the center of the hit using the energy 
   deposition as seen from each side */
float get_distance_from_energy(sc1_t sc1,int sector){
  extern sc_const_t sc_lambda;
  int index=sc_index(sector,sc1.id);

  return(sc_lambda.left[index]*sc_lambda.right[index]
	 *log(sc1.energy_l/sc1.energy_r)/
	 (sc_lambda.left[index]+sc_lambda.right[index]));
}

float get_distance_from_energy_unc(sc1_t sc1,int sector){
  extern sc_const_t sc_lambda;
  int i=sc_index(sector,sc1.id);
  float dydLl,dydLr,dydEl,dydEr;

  dydLl=log(sc1.energy_l/sc1.energy_r)
    * POW2(sc_lambda.left[i]/(sc_lambda.left[i]+sc_lambda.right[i]));
  dydLr=log(sc1.energy_l/sc1.energy_r)
    * POW2(sc_lambda.right[i]/(sc_lambda.left[i]+sc_lambda.right[i]));
  dydEl=sc_lambda.left[i]*sc_lambda.right[i]/sc1.energy_l
    /(sc_lambda.left[i]+sc_lambda.right[i]);
  dydEr=sc_lambda.left[i]*sc_lambda.right[i]/sc1.energy_r
    /(sc_lambda.left[i]+sc_lambda.right[i]);

  return(sqrt(dydLl*dydLl*sc_lambdau.left[i]*sc_lambdau.left[i]
	      + dydLr*dydLr*sc_lambdau.right[i]*sc_lambdau.right[i]
	      + dydEl*dydEl*sc1.denergy_l*sc1.denergy_l
	      + dydEr*dydEr*sc1.denergy_r*sc1.denergy_r));

}

/* Calculate the distance from the center of the hit using tracking */
float get_distance_from_dpl(sc1_t sc1,int sector,clasSCG_t *SCG,
			    const char *dpl,vector3_t bar_center, 
			    vector3_t bar_direction){
  clasHDPL_t *HDPL=getGroup(&bcs_,dpl,sector);
  clasSCP_t *SCP=getGroup(&wcs_,"SCP ",sector);
  int plane;
  
  if (HDPL && SCP){
    int ntrk_sec;
    vector3_t trk_lab_pos,diff;
    
    for (ntrk_sec=0; ntrk_sec<(HDPL->bank.nrow/10); ntrk_sec++){
      plane=tof_trk_match(&(HDPL->hdpl[ntrk_sec*10]),sc1.id,SCG,SCP,SLOP_FACTOR);
      if(plane){
	trk_lab_pos=sector2lab(HDPL->hdpl[plane+10*ntrk_sec].pos,sector);
	diff=v3sub(trk_lab_pos,bar_center);
	return(v3dot(diff,bar_direction));
      } 
    }
    return (NO_MATCH_TO_TRACK);
  }
  return (NO_MATCH_TO_TRACK);
}

/* Finds the scintillator to which a track points */
int dpl2sc_id(int sec, hdpl_t *hdpl){
  clasSCP_t *SCP=NULL;
  clasSCG_t *SCG=NULL;
  /* This variable counts the number of times the z-component of the 
     track projection is 1000.0 */
  int hit_error_level; 
  int i,plane_index,sc_plane,predicted_id=0;
  float thickness,width,zrange,zmax,zmin,length;
  float xrange,xmin,xmax,ymin,ymax;
  vector3_t normal,bar_center,hitpos;

  if((SCG=getGroup(&wcs_,"SCG ",sec))&&(SCP=getGroup(&wcs_,"SCP ",sec))){
    for (i=0; i < SCG->bank.nrow; i++){
      /* Dimensions of the scintillator */
      thickness=SCG->scg[i].thick;
      width=SCG->scg[i].width;
      length=get_bar_length(SCG,SCG->scg[i].id);
      
      bar_center=lab2sector(get_bar_center(SCG,SCG->scg[i].id),sec);

      /* range in z-direction for identifying bar */
      normal=get_bar_normal(SCP, SCG, SCG->scg[i].id);
      zrange=width*sqrt(1-normal.z*normal.z)/2.0+SC_MATCH_DELTA;	
      zmax=bar_center.z+zrange;
      zmin=bar_center.z-zrange;     

      /*range in y-direction */
      ymin=bar_center.y-length/2.0;
      ymax=bar_center.y+length/2.0;

      /*      fprintf(stderr,"scg_id: %d\n",SCG->scg[i].id); */
      /*fprintf(stderr,"zmax: %f zmin: %f ymax: %f ymin: %f\n",zmax,zmin,ymax,ymin); */
      /* Loop over the 4 sc panels */
      for (sc_plane=scPlane1;sc_plane<=scPlane4;sc_plane++){
	hitpos.z=hdpl[sc_plane].pos.z;
	hitpos.y=hdpl[sc_plane].pos.y;
	/*fprintf(stderr,"hitpos.z %f hitpos.y %f\n",hitpos.z,hitpos.y);*/
	if (hitpos.z<1000.0&&hitpos.y<1000.0
	    &&hitpos.z>=zmin&&hitpos.z<=zmax
	    /*&&hitpos.y>=ymin&&hitpos.y<=ymax*/
	    &&SCG->scg[i].panel==(sc_plane-3))
	  return(SCG->scg[i].id);
	if (hitpos.z<1000.0&&hitpos.y<1000.0
	    && i==0 && hitpos.z >= zmax
	    &&SCG->scg[i].panel==(sc_plane-3)) 
	  return(SCG->scg[i].id);     
      }
    } 
  }
  return -10;
}


/*---------------------------------------------------------------------------*/
int tof_trk_match(hdpl_t *hdpl, int id, clasSCG_t *SCG, clasSCP_t *SCP,
		  float factor){
  vector3_t hit_trk, hit_sc, dist;
  int i, plane;
  float width,length,zmin,zmax,zrange;
  vector3_t bar_center,normal;
  
  if (hdpl && SCG){  
    width=SCG->scg[id-1].width;
    length=get_bar_length(SCG,id);
    
    /* Get the center of the bar. Since we are only comparing z-positions,
       the bar_center does not need to be rotated into the sector frame */
    bar_center=get_bar_center(SCG,id);
    
    /* range in z-direction for identifying bar */
    normal=get_bar_normal(SCP, SCG, id);
    zrange=width*sqrt(1-normal.z*normal.z)/2.0*factor;	
    zmax=bar_center.z+zrange;
    zmin=bar_center.z-zrange;     
    plane = SCG->scg[id-1].panel+3;
    if (hdpl[plane].pos.z>=zmin && hdpl[plane].pos.z<=zmax){
      return plane;
    }    
  }
  return 0;
}






