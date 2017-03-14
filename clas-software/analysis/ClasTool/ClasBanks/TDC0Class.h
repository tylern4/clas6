////////////////////////////////////////////////////////////////////////
// File TECPBBank.h  
//
// Initial Author: Gagik Gavalian  UNH  02/13/2017
//
//
// This file is generated automatically by make_bank_class.pl 
//
// Generation DATE : Tue Nov 23 18:21:50 EST 1999
//
// Full documentation and comments can be found at
// http://improv.unh.edu/RootBosClasses.html 
//
////////////////////////////////////////////////////////////////////////
#ifndef _TDC0Class_ 
#define _TDC0Class_ 
#include <iostream>
using namespace std;


#include "TObject.h"
#include "TString.h"

class TDC0Class: public TObject{
  
 public:
  Int_t     ID; // id of DC hit
  Int_t     TDC; // tdc value
 public:
  TDC0Class(){};
  TDC0Class(TDC0Class *TmpSCPB);
  virtual ~TDC0Class(){};
  inline Int_t     GetID() { return ID; }
  inline Int_t     GetTDC() { return TDC; }
  void 	 Print();
  //
  ClassDef(TDC0Class,1) // Class for accessing the SCPB bank: Time of Flight.
    };
#endif


