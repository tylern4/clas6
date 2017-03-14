#include <stdio.h>
#include "pdu.h"

int InitWireLimits(Llimits_t * WireLimits){

  int Layer; 
  
  /* Region 1 */
  WireLimits[0].Min = WireLimits[6].Min = 4;
  WireLimits[1].Min = WireLimits[2].Min = WireLimits[7].Min = WireLimits[8].Min = 3;
  WireLimits[3].Min = WireLimits[9].Min = WireLimits[10].Min = 2;
  WireLimits[11].Min = 1;

  for (Layer=0; Layer<4; Layer++) WireLimits[Layer].Max = 130;
  for (Layer=6; Layer<9; Layer++) WireLimits[Layer].Max = 142;
  WireLimits[9].Max =  126;
  WireLimits[10].Max =  121;
  WireLimits[11].Max = 120;

  /* the phantom layers in region 1: */
  WireLimits[4].Min = WireLimits[4].Max = WireLimits[5].Min = WireLimits[5].Max = 0;

  /* Region 2 */
  for(Layer = 12; Layer<24; Layer++){
    WireLimits[Layer].Min = 1;
  }
  
  WireLimits[12].Max = 184;
  WireLimits[13].Max = 185;
  WireLimits[14].Max = 186;
  WireLimits[15].Max = 187;
  WireLimits[16].Max = 188;
  WireLimits[17].Max = WireLimits[18].Max = WireLimits[19].Max = 189;
  WireLimits[20].Max = 190;
  WireLimits[21].Max = 191;
  WireLimits[22].Max = WireLimits[23].Max = 192;

  /* Region 3 */
  for (Layer=24; Layer<36; Layer++){
    WireLimits[Layer].Min =   1;
    WireLimits[Layer].Max = 192;
  }
  
  return 0;

}
    
  
