
#include <stdio.h>
#include "pdu.h"
#include <math.h>
#include <status.h>

extern FILE *OutFile;

int FlagBadWires(int Hits[SECTORS][LAYERS-2][WIRES], int WireFlag[SECTORS][LAYERS][WIRES]){

/*  Purpose:  Flag all drift chamber channels as either dead, nominal, or hot.
    Inputs :  Hits     - occupancy spectra.
    Outputs:  WireFlag - wire status. 
    Layout:   1)  Determine "nominal" occupancy.
              2)  Compare measured occupancy with "nominal" and flag occording to cuts.
	      3)  Flag those wires that are between 1 and 192, but do not physically exist.
    Author:   Richard A Thompson
              University of Pittsburgh
              September 1997
*/
  int NomOcc[SECTORS][SUPERLAYERS],                   /* the nominal occupancy */
      Sector, SL, Layer, RealLayer, Wire, NWire,
      PulseType,
      IFail,
      HotFactor  = 10,                        /* if a wire has > HotFactor*NomOcc,  it is hot */
      DeadFactor = 10;                        /* if a wire has < DeadFactor/NomOcc, it is dead */
  Llimits_t WireLimits[LAYERS];
 
  /* write out routine parameters */
  fprintf(OutFile, "\nFlagBadWires: pathology determination factors:\n");
  fprintf(OutFile, "\tHot  - %d\n",HotFactor);
  fprintf(OutFile, "\tDead - %d\n",DeadFactor);

  /*initialize */
  for (Sector=0; Sector<6; Sector++){
    for(SL=0; SL<6; SL++){
      NomOcc[Sector][SL] = 0;
    }
  }
  InitWireLimits(WireLimits);
  
  IFail = WeightAve(Hits, NomOcc);
  for (Sector=0; Sector<6; Sector++){
    for (Layer=0; Layer<LAYERS; Layer++){
      SL = Layer/6;
      RealLayer = Layer>3 ? Layer-2 : Layer;
      for (Wire = WireLimits[Layer].Min-1; Wire<WireLimits[Layer].Max; Wire++){
	if(Hits[Sector][RealLayer][Wire] >= NomOcc[Sector][SL]*HotFactor){
	  /*if(Hits[Sector][RealLayer][Wire-1] >= (int)(max*HotFactor)){*/
	  /* it's a hot wire */
	  WireFlag[Sector][Layer][Wire] = DC_HOT_WIRE;
	}
	else if (Hits[Sector][RealLayer][Wire] <= NomOcc[Sector][SL]/DeadFactor) {
	  /* it's a dead wire */
	  WireFlag[Sector][Layer][Wire] = DC_DEAD_WIRE;
	}
	else {
	  /* it's a fine wire*/
	  WireFlag[Sector][Layer][Wire] = DC_GOOD_WIRE;
	}
	
      }
    }
  }

  /* flag the wires that do not exist */
  for (Sector =0; Sector<6; Sector++){
    for (Layer = 0; Layer<LAYERS; Layer++){
      for(Wire=0; Wire<WireLimits[Layer].Min - 1; Wire++) 
	WireFlag[Sector][Layer][Wire] = DC_DNE;
      for(Wire=WireLimits[Layer].Max; Wire<WIRES; Wire++)
	WireFlag[Sector][Layer][Wire] = DC_DNE;
    }
    /* now get the two R1 phantom layers */
    for(Layer = 4; Layer < 6; Layer++){
      for(Wire=0; Wire<WIRES; Wire++){
	WireFlag[Sector][Layer][Wire] = DC_DNE;
      }
    }
  }
  return 0;
}

int WeightAve(int Hits[SECTORS][LAYERS-2][WIRES], int NomOcc[SECTORS][SUPERLAYERS]){

/*  Purpose:  Calculate a weighted average for occupancy spectra nominal occupancy.
    Inputs :  Hits -   occupancy spectra.
    Outputs:  NomOcc - nominal occupancies. 
    Layout:   1)  Compute a weighted average.
    Comments: The weight here employed is the sum of the change in occupancy of a wire from 
              its neighbors.  So a spike, or an isolated dead wire will be weighted low and 
	      channels with flat occupancies will be weighted high.  This is appropriate for pulser
	      data.
    Author:   Richard A Thompson
              University of Pittsburgh
              September 1997
*/
  float Weight, RunWeight;
  float RunSum;
  int LeftWire, RightWire, LeftHit, RightHit, ThisHit;
  int Sector, SL, Layer, RealLayer, Wire;
  Llimits_t WireLimits[LAYERS];

  InitWireLimits(WireLimits);
  for (Sector = 0; Sector <6; Sector++){
    for (SL = 0; SL<6; SL++){
      RunSum = RunWeight = 0;
      for (Layer = 6*SL; Layer<(6*(SL+1)); Layer++){
	if(Layer==4 || Layer==5) continue;                            /* skip the phantom layers */
	RealLayer = Layer>3 ? Layer-2 : Layer;
	for (Wire = WireLimits[RealLayer].Min; Wire<=WireLimits[RealLayer].Max; Wire++){
	  if(ThisHit  =  Hits[Sector][RealLayer][Wire-1]){
	    LeftWire  = Wire-1<WireLimits[RealLayer].Min ? Wire+1 : Wire-1;
	    RightWire = Wire+1>WireLimits[RealLayer].Max ? Wire-1 : Wire+1;
	    LeftHit   = Hits[Sector][RealLayer][LeftWire-1];
	    RightHit  = Hits[Sector][RealLayer][RightWire-1];
	    /*Weight = 1./(fabs((double)ThisHit-(double)LeftHit/(double)ThisHit) + fabs((double)ThisHit-(double)RightHit/(double)ThisHit) + 1.);*/
	    Weight = 1./(float)( abs(ThisHit - LeftHit) + abs(ThisHit - RightHit) + 1 );
	    RunSum    += Weight*(float)ThisHit;
	    RunWeight += Weight;
	  }
	}
      }
      NomOcc[Sector][SL] = RunWeight==0 ? 0 : (int)((RunSum/RunWeight)+0.5);

      fprintf(OutFile, "WeightAve: the nominal occupancy for sector %d superlayer %d is %d\n",Sector, SL, NomOcc[Sector][SL]);

    }
  }
  return 0;
}

