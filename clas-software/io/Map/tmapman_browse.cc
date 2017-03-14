#include "tmapman.h"

//_________________________________________________________________________________________
int TMapMan::NextSubsystem(const char* name1,const char* name2){
 //Find next Subsystem
  fMapStatus=-1;
  SetMapSub(name1,name2);
  char nextsubsystem[99];
  if (MapIsDefined()) {
    if (!SubsystemIsDefined()) SetMapSubsystemName("*");
    fMapStatus = map_next_sub(fMapFileName,fSubsystemName,nextsubsystem,&fNItems);
    if (nextsubsystem[0]!='*') SetMapSubsystemName(nextsubsystem);    
    else fMapStatus=-7;
  }
  return fMapStatus;
}

//_________________________________________________________________________________________
int TMapMan::NextItem(const char* name1,const char* name2,const char* name3){
//find next item
  fMapStatus=-1;
  SetMap(name1,name2,name3);
  char nextitem[99];
  if (SubsystemIsDefined()) {
    if (!ItemIsDefined()) SetMapItemName("*");
    fMapStatus = map_next_item(fMapFileName,fSubsystemName,fItemName,nextitem,&fLength,&fType,&fNArrays);
    if (nextitem[0]!='*') SetMapItemName(nextitem);
    else fMapStatus=-6;
  }
  return fMapStatus;
}

//_____________________________________________________________________________________________
int TMapMan::FindFloatArray(int length,float* array,int time,
			    const char* name1,const char* name2,const char* name3) {
// Find first occurence of matching float array, parameters are (length, floatarray, beforetime)
  SetMap(name1,name2,name3);
  fMapStatus=-1;
  fEntryTime=-3;
  if (ItemIsDefined())
    fMapStatus=map_fnd_float(fMapFileName,fSubsystemName,fItemName,length,array,time,&fEntryTime);
  else cerr << "ERROR: item is not defined, could not find float array"<<endl;
  return fEntryTime;
}

//_____________________________________________________________________________________________
int TMapMan::FindIntArray(int length,int* array,int time,
const char* name1,const char* name2,const char* name3) {
// Find first occurence of matching int array, parameters are (length, intarray, beforetime)
  SetMap(name1,name2,name3);
  fMapStatus=-1;
  fEntryTime=-3;
  if (ItemIsDefined())
    fMapStatus=map_fnd_int(fMapFileName,fSubsystemName,fItemName,length,array,time,&fEntryTime);
  else cerr << "ERROR: item is not defined, could not find int array"<<endl;
  return fEntryTime;
}

