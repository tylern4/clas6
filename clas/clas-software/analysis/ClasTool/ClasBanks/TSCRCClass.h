////////////////////////////////////////////////////////////////////////
// File TECPBBank.h  
//
// Initial Author: Gagik Gavalian  UNH  02/13/2017 
//
//
// This file is generated automatically by make_bank_class.pl 
//
//
// Full documentation and comments can be found at
// http://improv.unh.edu/RootBosClasses.html 
//
////////////////////////////////////////////////////////////////////////
#ifndef _TSCRCClass_ 
#define _TSCRCClass_ 
#include <iostream>
using namespace std;


#include "TObject.h"
#include "TString.h"

class TSCRCClass: public TObject{
  
 public:
  Int_t     ID; // id of the hit
  Float_t   Energy;   // energy of the hit
  Float_t   dEnergy;   // error on energy
  Float_t   Time;   // time
  Float_t   dTime; // error in time
  Float_t   X; // X coordinate of the hit
  Float_t   Y; // Y coordinate of the hit
  Float_t   Z; // Z coordinate of the hit
  Float_t   dX; // error X coordinate of the hit
  Float_t   dY; // error Y coordinate of the hit
  Float_t   dZ; // error Z coordinate of the hit
  Int_t     Status; // Status word (not defined yet)
 public:
  TSCRCClass(){};
  TSCRCClass(TSCRCClass *TmpSCRC);
  virtual ~TSCRCClass(){};
  inline Int_t     GetID() { return ID; }
  inline Float_t   GetEnergy()   { return Energy; }
  inline Float_t   GetdEnergy()   { return dEnergy; }
  inline Float_t   GetTime()   { return Time; }
  inline Float_t   GetdTime()   { return dTime; }
  inline Float_t   GetX()   { return X; }
  inline Float_t   GetY()   { return Y; }
  inline Float_t   GetZ()   { return Z; }
  inline Float_t   GetdX()   { return dX; }
  inline Float_t   GetdY()   { return dY; }
  inline Float_t   GetdZ()   { return dZ; }
  inline Int_t     GetStatus() { return Status; }

  void 	 Print();
  //
  ClassDef(TSCRCClass,1) // Class for accessing the SCPB bank: Time of Flight.
    };
#endif


