#include "tmapman.h"

//_______________________________________________________________________
int TMapMan::Create(const char* name) {
  if (IsDefined(name)) SetMapFileName(name);
  fMapStatus=-1;
  if (MapIsDefined()) {
    fMapStatus=map_create(fMapFileName);
  }
  else cerr << "ERROR: map file name not defined, map not created "<<endl;
  return fMapStatus;
}

//_______________________________________________________________________
int TMapMan::AddItem(int length,int arraytype,
		     const char* name1,const char* name2,const char* name3) {
  SetMap(name1,name2,name3);
  fMapStatus = -1;
  if (arraytype>=0 || arraytype<=2) {
    if (ItemIsDefined())
      fMapStatus=map_add_item(fMapFileName, fSubsystemName, fItemName, length, arraytype);
    else cerr << "ERROR: item is not defined, item not added"<<endl;
  } else cerr << "ERROR: array type "<<arraytype<<" not valid"<<endl;
  return fMapStatus;
}

int TMapMan::ArrayType(const char* type) {
  if (!strcmp(type,_MAPINTS)) return _MAPINTV;
  if (!strcmp(type,_MAPFLOATS)) return _MAPFLOATV;
  if (!strcmp(type,_MAPSTRINGS)) return _MAPSTRINGV;
  return -1;
}

int TMapMan::AddItem(int length,const char* arraytype,
		     const char* name1,const char* name2,const char* name3) {
  SetMap(name1,name2,name3);
  return AddItem(length,ArrayType(arraytype));
}

//_______________________________________________________________________________________
int TMapMan::RemoveArray(int time,const char* name1,const char*
name2,const char* name3) { SetMap(name1,name2,name3); fMapStatus=-1; if
(ItemIsDefined())
fMapStatus=map_rem_arr(fMapFileName,fSubsystemName,fItemName,time);
else cerr << "WARNING item not defined, Array not removed" << endl;
return fMapStatus; }

//_______________________________________________________________________________________
int TMapMan::RemoveItem(const char* name1,const char* name2,const char* name3) {
  SetMap(name1,name2,name3);
  fMapStatus=-1;
  if (ItemIsDefined())
    fMapStatus=map_rem_item(fMapFileName,fSubsystemName,fItemName);
  else cerr << "ERROR: item not defined, Item not removed" << endl;
  return fMapStatus;
}

//_______________________________________________________________________________________
int TMapMan::RemoveSubsystem(const char* name1,const char* name2) {
  SetMapSub(name1,name2);
  fMapStatus=-1;
  if (SubsystemIsDefined())
    fMapStatus=map_rem_sub(fMapFileName,fSubsystemName);
  else cerr << "ERROR : subsystem is not defined, Subsystem not removed" <<endl;
  return fMapStatus;
}


