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


#include "TPhysClass.h"

ClassImp(TPhysClass)

TPhysClass::TPhysClass() : TClasTool(){
  cout << "TPHYSCLASS: Initializing the class" << endl;
}

TPhysClass::~TPhysClass()
{
}

