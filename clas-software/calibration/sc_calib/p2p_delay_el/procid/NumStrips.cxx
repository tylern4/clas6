//Author: Paul Mattione (12/14/2008)

#include "NumStrips.h"

NumStrips::NumStrips(){
  Set_Defaults();
}

NumStrips::NumStrips(const NumStrips& locNumStrips){
}

NumStrips::~NumStrips(){
}

void NumStrips::Initialize_NumStrips(int locRunNumber){
  nsNumStrips = (locRunNumber >= 55357) ? 57 : 48;
  nsNumTotalStrips = (locRunNumber >= 55357) ? 342 : 288;
}

NumStrips& NumStrips::Instance(){
  static NumStrips locNumStrips;
  return locNumStrips;
}

void NumStrips::Set_Defaults(){
  nsNumStrips = 48;
  nsNumTotalStrips = 288;
}

