#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ntypes.h>
#include <bostypes.h>
#include <ec.h>
#include <errno.h>
#include <math.h>
#include <bosddl.h>
#include <makebanks.h>

#define EC_TIME_UNDERFLOW  -100000.0
#define EC_TDC_MAX 4095
#define EC_TIME_OVERFLOW 100000.0
#define EC_ADC_MAX 8192

float EcCal_Atten[NSEC][EC_LAYERS][EC_ORIENTATION][EC_STRIPS];
float EcCal_Ech[NSEC][EC_LAYERS][EC_ORIENTATION][EC_STRIPS];
float EcCal_Eo[NSEC][EC_LAYERS][EC_ORIENTATION][EC_STRIPS];
/*
float EcCal_Tch[NSEC][EC_LAYERS][EC_ORIENTATION][EC_STRIPS];
float EcCal_To[NSEC][EC_LAYERS][EC_ORIENTATION][EC_STRIPS];
*/

float EcCal_Tch[NSEC][EC_LAYERS-1][EC_ORIENTATION][EC_STRIPS];
float EcCal_To[NSEC][EC_LAYERS-1][EC_ORIENTATION][EC_STRIPS];
float EcCal_Tadc[NSEC][EC_LAYERS-1][EC_ORIENTATION][EC_STRIPS];
float EcCal_dT1[NSEC][EC_LAYERS-1][EC_ORIENTATION][EC_STRIPS];
float EcCal_dT2[NSEC][EC_LAYERS-1][EC_ORIENTATION][EC_STRIPS];
float EcCal_Trms[NSEC][EC_LAYERS-1][EC_ORIENTATION][EC_STRIPS];

/* a crude calculation of EC time using one the TDC slople and offset
   this must not be used as a true EC hit time as is in the ECHB or some other EC bank */
float get_EC01_time(int tdc, int adc, float pedestal, float tdc_offset, float gain)
{
  float ped_sub_adc = adc - pedestal;
  float uncorrected_time = tdc;
  float corrected_time = (tdc - tdc_offset)*gain;
  
  if (tdc <=0) return(EC_TIME_UNDERFLOW);
  else if (tdc >= EC_TDC_MAX)  return(EC_TIME_OVERFLOW);
  else if(ped_sub_adc <=0 || ped_sub_adc >= EC_ADC_MAX) return(uncorrected_time);
  else return(corrected_time);                                 
}


float get_EC01_energy(int adc, float pedestal, float energy_per_channel)
{
  if ((adc - pedestal) <= 0) {
    return(0);
  }
  return((adc-pedestal)*energy_per_channel); 
}


int valid_EC_hit(ec_t  ec)
{
  if(ec.id >= 257 && ec.id <= 1572){
    /* as observed in ec.ddl */
    return 1;
  }
  return(0);
}



int make_EC01_bank(int sector)
{
  /*input: EC bank, EcCal01_t, sector
    output: EC01 bank */
  
  ec_t *ec=NULL;
  clasEC01_t *EC01=NULL;
  ec01_t ec01arr[EC_CHANNELS_PER_LAYER];
  int nEC01 = 0;
  clasEC_t *EC;
  int id;

   
  if (EC = getGroup(&bcs_, "EC  ", sector)) {
    int ec0_index;
    for(ec0_index = 0; ec0_index < EC->bank.nrow; ec0_index++) {
      /*check paddle id and tdc values - make sure they are valid*/
      if (valid_EC_hit(EC->ec[ec0_index])){
	id = EC->ec[ec0_index].id;
	
	ec01arr[nEC01].id = id;
	
	ec01arr[nEC01].energy = get_EC01_energy(EC->ec[ec0_index].adc,
						EcCal_Eo[sector-1][ec_layer(id)][ec_orientation(id)][ec_strip(id)],  EcCal_Ech[sector-1][ec_layer(id)][ec_orientation(id)][ec_strip(id)]);

	ec01arr[nEC01].time = get_EC01_time(EC->ec[ec0_index].tdc, 
					    EC->ec[ec0_index].adc,
					    EcCal_Eo[sector-1][ec_layer(id)][ec_orientation(id)][ec_strip(id)],
					    EcCal_To[sector-1][ec_layer(id)][ec_orientation(id)][ec_strip(id)],
					    EcCal_Tch[sector-1][ec_layer(id)][ec_orientation(id)][ec_strip(id)]);
	nEC01++;
      }
    }

    if (nEC01){
      /*sort*/
      /*qsort((void*)ec01arr, nEC01, sizeof(ec01_t), ec01arr.time);*/
      
      if(EC01 = makeBank(&bcs_, "EC01 ",sector,3,nEC01)){
	int i;
	for(i=0; i < nEC01; i++) 
	  EC01->ec01[i] = ec01arr[i];
      }
    }
  }
}

ec_init(int runno)
{
  ec_read_adc_map(runno,EcCal_Atten,"EC_ATTEN");
  ec_read_adc_map(runno,EcCal_Ech,"EC_GAIN");
  ec_read_adc_map(runno,EcCal_Eo,"EC_PEDESTALS");

  ec_read_tdc_map(runno, EcCal_Tch, "EC_Tch");
  ec_read_tdc_map(runno, EcCal_To, "EC_To");
  ec_read_tdc_map(runno, EcCal_Tadc, "EC_Tadc");
  ec_read_tdc_map(runno, EcCal_dT1, "EC_dT1");
  ec_read_tdc_map(runno, EcCal_dT2, "EC_dT2");
  ec_read_tdc_map(runno, EcCal_Trms, "EC_Trms");
  /*
  ec_read_map_tdc(runno,EcCal_Tch,"EC_TCH");
  ec_read_map_tdc(runno,EcCal_To,"EC_T0");
  */
}


