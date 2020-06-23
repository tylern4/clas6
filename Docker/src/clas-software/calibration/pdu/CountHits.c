#include <stdio.h>
#include <ntypes.h>
#include <bostypes.h>
#include <printBOS.h>
#include "pdu.h"
#include <errno.h>
#include <string.h>

int CountHits(int OutHits[SECTORS][LAYERS-2][WIRES]){

/*  Purpose: Acquire hit spectra.
    Inputs : InFilePtr - input data file
             max       - number of events to process
	     OutPeaks  - stb pulsed timing peaks
    Outputs: OutHits   - number of hits per wire 
    Author:  Richard A Thompson
             University of Pittsburgh
             September 1997
*/
  int   Nevents = 0,
        FirstBank,            
        Sector, Layer, RealLayer, SL, Region, Wire,
        i;
  clasDC0_t *DC0 = NULL;
  FILE *fp;

  FirstBank = 1;
  /* get DC0 bank */
  while( DC0 = (FirstBank ? (clasDC0_t *)getBank(&bcs_,"DC0 ") : 
		(clasDC0_t *)getNextBank(&bcs_,(bankHeader_t *)DC0))){
    FirstBank = 0;
    Sector = DC0->bank.nr;
    if(Sector<1 || Sector > 6){
      /*printf("CountHits: Bad sector %d encountered", Sector);*/
      continue;
    }
    for (i = 0; i < DC0->bank.nrow; ++i){
      Layer = (int)((DC0->dc0[i].id)/256);
      if (Layer>36 || Layer<1){
	/*printf("CountHits: Bad layer %d encountered\r", Layer);*/
	continue;
      }
      SL = (int)((Layer-1)/6) + 1;
      Wire = DC0->dc0[i].id % 256;
      if (Wire>WIRES){
	printf("CountHits: Bad wire %d encountered\r", Wire);
	continue;
      }
      
      OutHits[Sector-1][(Layer>4) ? Layer-3 : Layer-1][Wire-1]++;

    }                                           /* end of hit loop */
  }                                           /* end of sector loop */
  
  return 1;
}
