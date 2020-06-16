#include <stdio.h>
#include <ntypes.h>       /* data types like uint16 */
#include <bostypes.h>
#include <TrigSector.h>
#include <ec.h>

#define CC_CHANNELS 216

int FirstCall=1;

int TrigSector(int RunNo){

  /* Purpose: Return the sector that caused the trigger.
     Inputs:  CC and EC banks
     Outputs: number of the sector that caused the trigger
     Author:  Richard A Thompson 97.12

     this routine returns the sector that caused the trigger based on the following criteria:

     1)  The sector with the highest ec summed energy over threshold and a cc hit over 
     threshold 
     2)  Some cc tubes may be masked out.  See CC_ID_CUT.
     3)  EC and CC thresholds may be applied.  See EC(CC)_ADC_THLD.  But since these are currently
     not in the Map, you have to set them by hand in TrigSector.h.  They must be zero in the code 
     in the repository until this is Map managed.

     0 will be returned if there was no sector that satisfied the above criteria 

     The cc trigger threshold is applied to groups of eight CC PMTs (which I call CC trigger bits).
     The cc trigger bits are then ||-ed together.
     The trigger bits are staggered by four tubes so that each tube goes into two trigger bits 
     except for the first four tubes which only go into one trigger bit).
     The grouping of CC PMTs into cc trigger bits is done in cc_id2TrigBit.

     The ec trigger threshold is simply applied to the sum of all ec hits in a sector.
   
     */

  static int LastRunNo = -666;
  int Sect, TrigSect=0, i,j;
  int id, adc;          
  int CCBit1, CCBit2;
  int CCTrigBitAmpl[6][N_CC_TRIG_BITS];  /* indexed on sector and trigger bit within that sector */

  static float ECPeds[6][EC_LAYERS][EC_ORIENTATION][EC_STRIPS]; 
  static float CCPedsMean[CC_CHANNELS], CCPedsSigma[CC_CHANNELS];
  
  char *MapDir, ChMap[100];

  pmtHit_t CCAmpl[6];                  /* the amplitude of the trigger bit with the greatest adc */
  pmtHit_t ECAmpl[6];                  /* the sum of ec hits indexed on sector */
  clasCC0_t *cc = NULL;
  clasEC_t *ec = NULL;
  
  /* initialize */
  for (Sect=1; Sect<7; Sect++){
    CCAmpl[Sect-1].sec = CCAmpl[Sect-1].adc = 0;
    ECAmpl[Sect-1].sec = ECAmpl[Sect-1].adc = 0;
    for (i=0; i<N_CC_TRIG_BITS; i++){
      CCTrigBitAmpl[Sect-1][i] = 0;
    }
  }

  /* print out parameters */
  if(FirstCall && EC_ADC_THLD) 
    fprintf(stdout, "TrigSector: EC_ADC_THLD is %d\n",EC_ADC_THLD);
  if(FirstCall && CC_ADC_THLD) 
    fprintf(stdout, "TrigSector: CC_ADC_THLD is %d\n",CC_ADC_THLD);
  if(FirstCall && MIN_CC_TRIG_BIT) 
    fprintf(stdout, "TrigSector: MIN_CC_TRIG_BIT is %d\n",MIN_CC_TRIG_BIT);

  /* get the pedestals if the run number has changed*/
  if(RunNo != LastRunNo){
    ec_read_map(RunNo, ECPeds, "EC_PEDESTALS");
    if(MapDir = (char*)getenv("CLAS_PARMS")){
      sprintf(ChMap, "%s/Maps/CC_CALIB.map", MapDir);
    }
    get_cc_pedestals_(ChMap, RunNo, CCPedsMean, CCPedsSigma);
  }
  LastRunNo = RunNo;

  /* parse the cc hits (from tubes) into the appropriate cc trigger bits */
  for (Sect=1; Sect<7; Sect++){
    if(cc = getGroup(&bcs_, "CC  ", Sect)){
      for(i=0; i<cc->bank.nrow; i++){
	cc_id2TrigBit(cc->cc0[i].id, &CCBit1, &CCBit2);
	id = cc->cc0[i].id;
	if(adc=cc->cc0[i].adc){
	  if(CCBit1>0) 
	    CCTrigBitAmpl[Sect-1][CCBit1 - 1] += adc - (int)CCPedsMean[cc_index(Sect,id)] ;
	  if(CCBit2>0) 
	    CCTrigBitAmpl[Sect-1][CCBit2 - 1] += adc - (int)CCPedsMean[cc_index(Sect,id)];
	}
      }
    }
  }
  
  /* fill CCAmpl with the amplitude of the trigger bit with the greatest amplitude*/
  for (Sect=1; Sect<7; Sect++){
    CCAmpl[Sect-1].sec = Sect;       /* this looks weird, but it's so that I can later sort without
				        losing track of sector */
    for(i=MIN_CC_TRIG_BIT; i<N_CC_TRIG_BITS; i++){
      if(CCTrigBitAmpl[Sect-1][i] > CCAmpl[Sect-1].adc)
	CCAmpl[Sect-1].adc = CCTrigBitAmpl[Sect-1][i];
    }
  }

  /* fill ECAmpl with the summed energy in that sector */
  for (Sect=1; Sect<7; Sect++){
    ECAmpl[Sect-1].sec = Sect;
    if(ec = getGroup(&bcs_, "EC  ", Sect)){
      for(i=0; i< ec->bank.nrow; i++){
	id = ec->ec[i].id;
	if(adc=ec->ec[i].adc)         /* sometimes there are zeros, don't add those because after
					  pedestal subtraction they'll be negative */
	  ECAmpl[Sect-1].adc += (int)adc - 
	    (int)(ECPeds[Sect-1][ec_layer(id)][ec_orientation(id)][ec_strip(id)]);
      }
    }
  }
  /* figure out the trigger sector */
  /* this will report the sector with the highest EC sum over threshold and a CC hit over 
     threshold */
  qsort((void *)CCAmpl, 6, sizeof(pmtHit_t), sort_pmtHit);
  qsort((void *)ECAmpl, 6, sizeof(pmtHit_t), sort_pmtHit);
  for (j=5; j>-1; j--){       /* loop over CC sectors */
    for (i=5; i>-1; i--){     /* loop over EC sectors */
      if (ECAmpl[j].sec != CCAmpl[i].sec) continue;
      if (!TrigSect && ECAmpl[j].adc > EC_ADC_THLD && CCAmpl[i].adc > CC_ADC_THLD) 
	TrigSect =  ECAmpl[j].sec;
    }
  }
  FirstCall = 0;
  return TrigSect;
}

int sort_pmtHit(const void *pmtHit1, const void *pmtHit2){

  pmtHit_t *hit1 = (pmtHit_t *) pmtHit1;
  pmtHit_t *hit2 = (pmtHit_t *) pmtHit2;

  if (hit1->adc < hit2->adc)  return -1;
  if (hit1->adc == hit2->adc) return  0;
  if (hit1->adc > hit2->adc)  return  1;

}

int cc_id2TrigBit(int id, int * Bit1, int * Bit2){

  /* input : id        --  a cc pmt tube id
     output: Bit1 Bit2 --  the cc trigger bits that tube id goes into.
     */

  *Bit1 = id<33 ? 2*( (int)((id-1)/8) + 1) - 1 : 0; /* odd numbered bits */
  *Bit2 = id>4  ? 2*( (int)((id-5)/8) + 1)     : 0; /* even numbered bits are offset by 4 */
  
  return 1;
}
