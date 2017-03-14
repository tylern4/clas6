//=================================================
//
//  TPhysClass is a template class for introducing
//  our own analyses in TClasTool framework
//  just follow examples in this directory
//
//   Author: G.Gavalian (UNH) 04/12/2002
//   
//
//==================================================


#ifndef __TPHYS_CLASS__
#define __TPHYS_CLASS__

#include "TClasTool.h"

class TPhysClass : public TClasTool {

 public:
  TPhysClass();
  virtual ~TPhysClass();

  
 ClassDef(TPhysClass,2)
};

#endif
