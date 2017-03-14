#include "tmapman.h"

//______________________________________________________________________________
TMapMan::TMapMan() {
  SetMapFileName("");
  SetMapSubsystemName("");
  SetMapItemName("");
  fMapStatus = 0;
  fNItems = 0;
  fEntryTime = -1;
  fLength = 0;
  fType = -1;
  fNArrays = 0;

}

TMapMan::TMapMan(const char* name1,const char* name2,const char* name3) {
  fMapStatus = 0;
  fNItems = 0;
  fEntryTime = -1;
  fLength = 0;
  fType = -1;
  fNArrays = 0;
  SetMapFileName(name1);
  SetMapSubsystemName(name2);
  SetMapItemName(name3);
  fMapStatus=0;
}

TMapMan::~TMapMan() {;}

//______________________________________________________________________________
int TMapMan::SetMapFile(int verbose,const char* name) {
  SetMapFileName(name);
  if (verbose) Print();
  return fMapStatus;
}

//______________________________________________________________________________
void TMapMan::PrintMapFileName() {
  if (IsDefined(fMapFileName)) cerr << fMapFileName ;
  else cerr << "Map File Name is not defined"<<endl;
}

//______________________________________________________________________________
int TMapMan::SetMapSubsystem(int verbose,const char* name) {
  SetMapSubsystemName(name);
  if (verbose) PrintSubsystem();
  Read_NItems();
  return fMapStatus;
}

//______________________________________________________________________________
void TMapMan::PrintMapSubsystemName() {
  if (IsDefined(fSubsystemName)) cerr << fSubsystemName ;
  else cerr << "Subsystem Name is not defined"<<endl;
}

//______________________________________________________________________________
int TMapMan::SetMapItem(int verbose,const char* name) {
  SetMapItemName(name);
  Read_NArrays();
  Read_ItemInfo();
  if (verbose) PrintItem();
  return fMapStatus;
}

//______________________________________________________________________________
void TMapMan::PrintMapItemName() {
  if (IsDefined(fItemName)) cerr << fItemName ;
  else cerr << "Item Name is not defined"<<endl;
}

//______________________________________________________________________________
int TMapMan::SetMap(int verbose,const char* name1, const char* name2,const char* name3) {

  if (IsDefined(name3)) {
    SetMapFile(verbose,name1);
    SetMapSubsystem(verbose,name2);
    SetMapItem(verbose,name3); 
  } else if (IsDefined(name2)) {
    if (!MapIsDefined()) {
      SetMapFile(verbose,name1);
      SetMapSubsystem(verbose,name2);
    } else {
      SetMapSubsystem(verbose,name1);
      SetMapItem(verbose,name2); 
    }
  } else if (IsDefined(name1)) {
    if (!MapIsDefined()) SetMapFile(verbose,name1);
    else if (!SubsystemIsDefined()) SetMapSubsystem(verbose,name1);
    else SetMapItem(verbose,name1); 
  }
  return fMapStatus;
}

//______________________________________________________________________________
int TMapMan::SetMapSub(int verbose,const char* name1,const char* name2){
  if (IsDefined(name2)) {
    SetMapFile(verbose,name1);
    SetMapSubsystem(verbose,name2);
  } else if (IsDefined(name1)) {
    if (!MapIsDefined()) SetMapFile(verbose,name1);
    else SetMapSubsystem(verbose,name1);
  }
  return 0;
}

//______________________________________________________________________________
int TMapMan::ClearMap() {
  SetMapFileName("");
  return ClearSubsystem();
}


//______________________________________________________________________________
int TMapMan::ClearSubsystem() {
  fMapStatus = 0;
  fNItems = 0;
  ClearSubsystemName();
  return ClearItem();

}

//______________________________________________________________________________
int TMapMan::ClearItem() {
  fMapStatus = 0;
  fEntryTime = -1;
  fLength = 0;
  fType = -1;
  fNArrays = 0;
  ClearItemName();
  return 0;
}

