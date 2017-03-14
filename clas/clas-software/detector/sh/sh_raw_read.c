//Reads raw SH and SHT banks (SH and TAGE for g12)
//Puts values in the arrays for further processing

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "sh.h"
#include "sh_global.h"
#include "sh_extern.h"

int sh_read_adc(){
  clasSH_t *SH=NULL;
  int SHN=0,raw_ind=0;
  if ( (SH=getBank(&bcs_, "SH  ")) ){//1
    for(raw_ind=0; raw_ind<SH->bank.nrow && SHN<SH_MAX_ADCHITS; raw_ind++){//2
      int id=SH->sh[raw_ind].id;
      int adc=SH->sh[raw_ind].adc;
      if(id>=257&&id<=1042){// these are SH channels
    adcev[SHN].id=get_chan_id(id);
    adcev[SHN].val=adc;
    SHN++;
    //printf("ADC %d %d \n",id,adc);
      }
    }//2
  }//1
  return SHN;
}

int sh_read_tdc(){
  clasSHT_t *SHT=NULL;
  int SHNT=0,raw_ind=0;
  if ( (SHT = getBank(&bcs_, "SHT ")) ){//1
    for(raw_ind=0; raw_ind<SHT->bank.nrow && SHNT<SH_MAX_TDCHITS; raw_ind++){//2
      int id=SHT->sht[raw_ind].id;
      int tdc=SHT->sht[raw_ind].tdc;
      tdcev[SHNT].id=get_chan_id(id);
      tdcev[SHNT].val=tdc;
      SHNT++;
      //printf("TDC %d %d \n",id,tdc);
    }//2
  }//1
  return SHNT;
}

int sh_read_adc_g12(){
  //same as sh_read_adc(),but can be used for pcal prototype beam test
  clasSH_t *SH=NULL;
  int SHN=0,raw_ind=0;
  if ( (SH=getBank(&bcs_, "TAGE")) ){//1
    for(raw_ind=0; raw_ind<SH->bank.nrow && SHN<SH_MAX_ADCHITS; raw_ind++){//2
      int id=SH->sh[raw_ind].id;
      int adc=SH->sh[raw_ind].adc;
      if(id>=257&&id<=1042){// these are SH channels
    adcev[SHN].id=get_chan_id(id);
    adcev[SHN].val=adc;
    SHN++;
    //printf("ADC %d %d \n",id,adc);
      }
    }//2
  }//1
  return SHN;
}

int sh_read_tdc_g12(){
  clasSHT_t *SHT=NULL;
  int SHNT=0,raw_ind=0;
  if ( (SHT = getBank(&bcs_, "TAGE")) ){//1
    for(raw_ind=0; raw_ind<SHT->bank.nrow && SHNT<SH_MAX_TDCHITS; raw_ind++){//2
      int id=SHT->sht[raw_ind].id;
      int tdc=SHT->sht[raw_ind].tdc;
      if(id>=4097&&id<=11794){ // these are SH channels
    tdcev[SHNT].id=get_chan_id_g12(id);
    tdcev[SHNT].val=tdc;
    SHNT++;
    //printf("TDC %d %d \n",id,tdc);
      }
    }//2
  }//1
  return SHNT;
}


