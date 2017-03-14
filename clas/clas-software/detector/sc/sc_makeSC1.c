/*---------------------sc_makeSC1.c------------------------*/
/*  Routines to create the SC1 bank.  ADCs are converted to energy units and
  times are corrected for left-right matching, counter-to-counter relative 
  delays and time-walk.
  ROUTINES:
     make_SC_banks:   Makes the SC1, SCR, and SCR banks for each sector.
     make_SC1_bank:   This is the main routine for creating the SC1 bank for
	              a particular sector.  It requires the SC and SCG banks
		      and the calibration global variables as input.
     get_SC1_time:    Calculates the walk-corrected time in ns for a given tdc
		      hit.
     get_SC1_time_unc: Calculates the uncertainty in this time.
     get_SC1_energy:  Converts an ADC into energy units (MeV)
     get_SC1_energy_unc:  Calculates the uncertainty in this "energy"
     comp_sc_t:      comparison routine for qsort
     valid_SC_hit:   tests for "good" hits (hits that have at least one
		     nonzero/non-overflow tdc value) 
*/
/*----------------------------------------------------------*/
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <math.h>
#include <bostypes.h>
#include <scExtern.h>

/* -=======================================================- *
$Id: sc_makeSC1.c,v 1.28 2008/03/16 04:33:22 fklein Exp $
$Author: fklein $
$Revision: 1.28 $
$Date: 2008/03/16 04:33:22 $
* -=======================================================- */

/* Make and fill SC1 and SCR BOS banks */
int make_sc_banks_(){
  int sec;

  dropAllBanks(&bcs_, "SC1 SCR SCRC");
  for (sec=1;sec<=6;sec++){
    make_SC1_bank(sec);
    make_SCR_bank(sec, "HDPL");
    make_SCRC_bank(sec);
  }
}

/* Make and fill SCR and SCRC banks at level 2 analysis*/
int make_sc_banks_tb_(){
  int sec;

  dropAllBanks(&bcs_, "SCR SCRC");
  for (sec=1;sec<=6;sec++){
    make_SCR_bank(sec, "TDPL");
    make_SCRC_bank(sec);
  }
}


/* Convert raw tdcs to times in ns and raw adcs to energies in MeV */
clasSC1_t *make_SC1_bank(int sector){
  /*input: SC bank, sc_calib_t, sector
    output: SC1 bank */
  clasSC_t *SC=NULL;
  clasSC1_t *SC1=NULL;
  clasSCG_t *SCG=NULL;
  sc1_t sc1arr[SC_NPADDLES_SEC_TOTAL];
  int  tdcl_swapped[SC_NPADDLES_TOTAL];
  int  tdcr_swapped[SC_NPADDLES_TOTAL];
  int nswap = 0;
  int cal_index;
  //  extern sc_const_t sc_m0,sc_m0u;
  //  extern sc_const_t sc_pedestals,sc_pedu;
  //  extern sc_const_t sc_t0,sc_t0u,sc_t1,sc_t1u,sc_t2,sc_t2u;
  //  extern sc_const_t sc_walk0,sc_walk0u,sc_walk1,sc_walk1u,sc_walk2,sc_walk2u;
  int nSC1 = 0, n, ii;
  float thickness;
  float disc_thresh=35.0; /*This is something we should get from data stream */
  int comp_sc_t(const void *sc1, const void *sc2);

  nSC1 = 0;
  memset (tdcl_swapped, 0, sizeof(tdcl_swapped));
  memset (tdcr_swapped, 0, sizeof(tdcr_swapped));

  if((SC=getGroup(&bcs_,"SC  ",sector))&&(SCG=getGroup(&wcs_,"SCG ",sector))){
    int scBank_index;     /* Index for the SC Raw bank  0..bank.nrow */

/* The following code corrects for miscabling at TOF patch panel for left TDCs during e1e run period */

    nswap = 0;
    for(scBank_index = 0; scBank_index < SC->bank.nrow; scBank_index++){
	cal_index = sc_index(sector, (SC->sc[scBank_index].id&0xFF));
        tdcl_swapped [ sc_swapindex.left  [cal_index]] = SC->sc[scBank_index].tdcl;
        tdcr_swapped [ sc_swapindex.right [cal_index]] = SC->sc[scBank_index].tdcr;
        if (sc_swapindex.left  [cal_index] != cal_index) nswap++;
    }


    for(scBank_index = 0; scBank_index < SC->bank.nrow; scBank_index++){
/*check paddle id, map status and tdc values - make sure they are valid*/
      if (valid_SC_hit( SC, sector, scBank_index)){
	cal_index = sc_index(sector,(SC->sc[scBank_index].id&0xFF));
	sc1arr[nSC1].id = SC->sc[scBank_index].id&0xFF;
	thickness=SCG->scg[sc1arr[nSC1].id-1].thick;
	sc1arr[nSC1].energy_l = get_SC1_energy(SC->sc[scBank_index].adcl,
					       sc_pedestals.left[cal_index],
					       sc_m0.left[cal_index],
					       thickness);

	sc1arr[nSC1].energy_r = get_SC1_energy(SC->sc[scBank_index].adcr,
					       sc_pedestals.right[cal_index],
					       sc_m0.right[cal_index],
					       thickness);

	sc1arr[nSC1].time_r = get_SC1_time(tdcr_swapped[cal_index],
					   SC->sc[scBank_index].adcr,
					   sc_pedestals.right[cal_index],
					   disc_thresh,
					   sc_t0.right[cal_index],
					   sc_t1.right[cal_index],
					   sc_t2.right[cal_index],
					   sc_walk0.right[cal_index],
					   sc_walk1.right[cal_index],
					   sc_walk2.right[cal_index],
					   adc_ref,pulse_norm);

	sc1arr[nSC1].time_l = get_SC1_time(tdcl_swapped[cal_index],
					   SC->sc[scBank_index].adcl,
					   sc_pedestals.left[cal_index],
					   disc_thresh,
					   sc_t0.left[cal_index],
					   sc_t1.left[cal_index],
					   sc_t2.left[cal_index],
					   sc_walk0.left[cal_index],
					   sc_walk1.left[cal_index],
					   sc_walk2.left[cal_index],
					   adc_ref,pulse_norm);

	sc1arr[nSC1].denergy_l = get_SC1_energy_unc(sc1arr[nSC1].energy_l,
						    sc_pedu.left[cal_index],
						    sc_m0.left[cal_index],
						    sc_m0u.left[cal_index],
						    thickness);

	sc1arr[nSC1].denergy_r = get_SC1_energy_unc(sc1arr[nSC1].energy_r,
						    sc_pedu.right[cal_index],
						    sc_m0.right[cal_index],
						    sc_m0u.right[cal_index],
						    thickness);

	sc1arr[nSC1].dtime_r = get_SC1_time_unc(tdcr_swapped[cal_index],
					       SC->sc[scBank_index].adcr,
					       sc_pedestals.right[cal_index],
					       sc_pedu.right[cal_index],
					       disc_thresh,
					       sc_t0.right[cal_index],
					       sc_t1.right[cal_index],
					       sc_t2.right[cal_index],
					       sc_walk0.right[cal_index],
					       sc_walk1.right[cal_index],
					       sc_walk2.right[cal_index],
					       adc_ref,pulse_norm,
					       sc_t0u.right[cal_index],
					       sc_t1u.right[cal_index],
					       sc_t2u.right[cal_index],
					       sc_walk0u.right[cal_index],
					       sc_walk1u.right[cal_index],
					       sc_walk2u.right[cal_index]);

	sc1arr[nSC1].dtime_l = get_SC1_time_unc(tdcl_swapped[cal_index],
					       SC->sc[scBank_index].adcl,
					       sc_pedestals.left[cal_index],
					       sc_pedu.left[cal_index],
					       disc_thresh,
					       sc_t0.left[cal_index],
					       sc_t1.left[cal_index],
					       sc_t2.left[cal_index],
					       sc_walk0.left[cal_index],
					       sc_walk1.left[cal_index],
					       sc_walk2.left[cal_index],
					       adc_ref,pulse_norm,
					       sc_t0u.left[cal_index],
					       sc_t1u.left[cal_index],
					       sc_t2u.left[cal_index],
					       sc_walk0u.left[cal_index],
					       sc_walk1u.left[cal_index],
					       sc_walk2u.left[cal_index]);
	nSC1++;
      }
    }

    if (nSC1){
      /*sort*/
      qsort((void*)sc1arr, nSC1, sizeof(sc1_t), comp_sc_t);
      if(SC1 = makeBank(&bcs_, "SC1 ",sector,sizeof(sc1_t)/4,nSC1)){
	int i;
	for(i=0; i < nSC1; i++) SC1->sc1[i] = sc1arr[i];
      }
    }
  }

  return SC1;
}

/* Function for filtering bad hits from the data.  Bad hits are those hits
   which do not contain any usable time information. It also zeroes adc and/or
   tds if the cnannel marked bad in the map*/
int valid_SC_hit(clasSC_t *SC, int sector, int index){
  int map_index;
  if ((SC->sc[index].id&0xFF) > 0 && (SC->sc[index].id&0xFF) <= SC_NPADDLES_SEC){
    /* check the hw status of the left side */
    switch(sc_status.left[sc_index(sector, (SC->sc[index].id&0xFF))]){
    case SC_STATUS_NO_ADC:
      SC->sc[index].adcl = 0;
      break;
    case SC_STATUS_NO_TDC:
      SC->sc[index].tdcl = 0;
      break;
    case SC_STATUS_DEAD:
      SC->sc[index].tdcl = 0;
      SC->sc[index].adcl = 0;
      break;
    }
    /* check the hw status of the right side */
    switch(sc_status.right[sc_index(sector, (SC->sc[index].id&0xFF))]){
    case SC_STATUS_NO_ADC:
      SC->sc[index].adcr = 0;
      break;
    case SC_STATUS_NO_TDC:
      SC->sc[index].tdcr = 0;
      break;
    case SC_STATUS_DEAD:
      SC->sc[index].tdcr = 0;
      SC->sc[index].adcr = 0;
      break;
    }
    if(SC->sc[index].tdcl <= 0 && SC->sc[index].tdcr <=0) return 0;
    if(SC->sc[index].tdcl >= SC_TDC_MAX && SC->sc[index].tdcr >=SC_TDC_MAX) return 0;
    if(SC->sc[index].tdcl >= SC_TDC_MAX && SC->sc[index].tdcr <=0) return 0;
    if(SC->sc[index].tdcr >= SC_TDC_MAX && SC->sc[index].tdcl <=0) return 0;
    return 1;
  }
  return(0);
}

/* Calculate the walk-corrected time for a given tdc value.  The time depends 
   on the tdc value in the following way:
                           2
       T = T0 + T1*t + T2*t  + time_walk,  where t=tdc value.  
    */
float get_SC1_time(int tdc, int adc, float pedestal,float disc_thresh,float t0,
		   float t1, float t2, float walk0, float walk1, float walk2,
		   float tw_adc_ref,float t_scale_factor){
  
  float ped_sub_adc = adc - pedestal;
  float norm_adc = ped_sub_adc/disc_thresh;
  float norm_max = tw_adc_ref/disc_thresh;  
  float uncorrected_time=t_scale_factor*(t0 + t1*tdc + t2*tdc*tdc);
  float time_walk=0,walk_max=0;

  if (tdc <=0) return(SC_TIME_UNDERFLOW);
  else if (tdc >= SC_TDC_MAX) return(SC_TIME_OVERFLOW);
  else if(ped_sub_adc <=0 || ped_sub_adc >= SC_ADC_MAX || walk0<=0){
    return(uncorrected_time);
  }
  if (norm_adc < walk0) time_walk=walk1/pow(norm_adc,walk2);
  else time_walk=walk1*(1.0+walk2)/pow(walk0,walk2)-walk1*walk2*norm_adc/
	 pow(walk0,walk2+1.0);
 
  if (norm_max < walk0) walk_max=walk1/pow(norm_max,walk2);
  else walk_max=walk1*(1.0+walk2)/pow(walk0,walk2)-walk1*walk2*norm_max/
	 pow(walk0,walk2+1.0);
 
  return(uncorrected_time + t_scale_factor*( walk_max - time_walk));	 
} 


float get_SC1_time_unc(int tdc, int adc, float pedestal,float dp, 
		       float disc_thresh,float t0, float t1, float t2, 
		       float w0, float w1, float w2, float tw_adc_ref,
		       float t_scale_factor, float dt0,float dt1,
		       float dt2,float dw0,float dw1, float dw2){
  float ped_sub_adc = adc - pedestal;
  float norm_adc = ped_sub_adc/disc_thresh;
  float norm_max = tw_adc_ref/disc_thresh;
  float dtdA, dtdP,dtdw1,dtdw2,dtdw0;
  float unc=sqrt(dt0*dt0 + tdc*tdc*dt1*dt1 + tdc*tdc*tdc*tdc*dt2*dt2
		 +(t1+2*t2*tdc)*(t1+2*t2*tdc)*TDC_JITTER*TDC_JITTER);

  if (tdc <=0 || tdc >= SC_TDC_MAX) return 0;
  if (ped_sub_adc<=0 || ped_sub_adc>=SC_ADC_MAX || w0<=0) return(unc);
  
  if (norm_adc<w0 && norm_max<w0){

    /***************************************************************
      In the following formulas, N=norm_adc, Nmax=norm_max.
  
                               w1           w1
       t = t(uncorrected) +   ----    -    ---- 
	          		 w2	     w2
			      Nmax          N
    ***************************************************************/

    dtdP=w1*w2*pow(norm_adc,-w2-1)/disc_thresh;
    dtdA=-dtdP;
    dtdw0=0;
    dtdw1=pow(norm_max,-w2) - pow(norm_adc,-w2);
    dtdw2=w1*(log(norm_max)/pow(norm_max,w2) - log(norm_adc)/pow(norm_adc,w2));
  }
  if (norm_adc<w0 && norm_max>=w0){

    /***************************************************************   
                             w1(1+w2)       w1 w2 Nmax        w1
       t = t(uncorrected) +  --------   -   ----------   -   ----
                                 w2               w2+1         w2
                               w0               w0            N
    ***************************************************************/

    dtdA=w1*w2*pow(norm_adc,-w2-1)/disc_thresh;
    dtdP=-dtdA;
    dtdw0=w1*w2*(1+w2)*pow(w0,-w2-1)*(norm_max/w0-1);
    dtdw1=(1+w2)*pow(w0,-w2) - w2*norm_max*pow(w0,-w2-1) - pow(norm_adc,-w2);
    dtdw2=w1*(pow(w0,-w2)*((1-norm_max/w0)*(1+w2*log(w0)) + log(w0))
	      - log(norm_adc)*pow(norm_adc,-w2));
  }
  if (norm_adc>=w0 && norm_max<w0){
  
    /***************************************************************   
                               w1           w1(1+w2)       w1 w2 N
       t = t(uncorrected) +   ----    -    ---------   +   -------
	          	         w2	        w2            w2+1
			      Nmax            w0            w0
    ***************************************************************/  

    dtdA = w1*w2*pow(w0,-w2-1)/disc_thresh;
    dtdP = -dtdA;
    dtdw0= w1*w2*(1+w2)*pow(w0,-w2-1)*(1-norm_adc/w0);
    dtdw1= pow(norm_max,-w2) - (1+w2)*pow(w0,-w2) + w2*norm_adc*pow(w0,-w2-1);
    dtdw2= w1*(log(norm_max)*pow(norm_max,-w2) 
	       - pow(w0,-w2)*((1-norm_adc/w0)*(1+w2*log(w0)) + log(w0))); 
  }
  if (norm_adc>=w0 && norm_max>=w0){
 
    /***************************************************************   
                              w1 w2 (N - Nmax)         
       t = t(uncorrected) +   ----------------    
	          	            w2+1
			          w0
    ***************************************************************/  

    dtdA= w1*w2*pow(w0,-w2-1)/disc_thresh;
    dtdP= -dtdA;
    dtdw0= -w1*w2*(norm_adc-norm_max)*(w2+1)*pow(w0,-w2-2);
    dtdw1= w2*(norm_adc-norm_max)*pow(w0,-w2-1);
    dtdw2= w1*(norm_adc-norm_max)*pow(w0,-w2-1)*(1 + w2*log(w0));
  }
  return(sqrt(unc*unc + dtdP*dtdP*dp*dp + dtdw0*dtdw0*dw0*dw0 
	      + dtdw1*dtdw1*dw1*dw1 + dtdw2*dtdw2*dw2*dw2
	      + dtdA*dtdA*ADC_JITTER*ADC_JITTER));

}

/* Calculate the energy deposited in the scintillator from the adc value.
   This is really the ADC value converted into energy units using the formula
          k(A-P) 
      E = ------ , where k= 10 MeV and G is the ADC value of the N.M.I.P peak. 
            G             
*/
float get_SC1_energy(int adc, float pedestal, float gain, float thick ){
  if ((adc - pedestal) <= 0) {
    return(0);
  }
  return(((adc-pedestal)/gain)*DEDX_NMIP*thick); 
}

float get_SC1_energy_unc(float energy,float dP, float gain,float dG,
			 float thick){
  float K=DEDX_NMIP*thick;
  if (gain>0) return(K/gain*sqrt(ADC_JITTER*ADC_JITTER + dP*dP 
				      + energy*energy*dG*dG/K*K));
  return 0.0;
}

/* Comparison function needed for qsort */
int comp_sc_t(const void *sc1, const void *sc2){
  if (((sc1_t *)sc1)->id > ((sc1_t *)sc2)->id) return 1;
  if (((sc1_t *)sc1)->id < ((sc1_t *)sc2)->id) return -1;
  return 0;   
}
