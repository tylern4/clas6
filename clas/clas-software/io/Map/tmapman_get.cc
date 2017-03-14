#include "tmapman.h"

//_____________________________________________________________________________________
int TMapMan::GetCharArray(int length,char* array,int time,
			  const char* name1,const char* name2,const char* name3) {
  // Get array of char, parameters are (length, time)
  int olength;
  SetMap(name1,name2,name3);
  fMapStatus=-1;

  if (ItemIsDefined()) 
    fMapStatus=map_get_char(fMapFileName,fSubsystemName,fItemName,length,array,time,&fEntryTime,&olength);
  
  return fMapStatus;
}

//_____________________________________________________________________________________
int TMapMan::GetFloatArray(int length,float* array,int time,
			   const char* name1,const char* name2,const char* name3) {
  // Get array of float, parameters are (length, array,  time)
  fMapStatus=-1;
  SetMap(name1,name2,name3);
  
  if (ItemIsDefined())
    fMapStatus=map_get_float(fMapFileName,fSubsystemName,fItemName,length,array,time,&fEntryTime);
  
  return fMapStatus;
}

//_____________________________________________________________________________________
int TMapMan::GetIntArray(int length,int* array,int time,
			 const char* name1,const char* name2,const char* name3) {
  // Get array of int, parameters are (length, array, time)
  fMapStatus=-1;
  SetMap(name1,name2,name3);

  if (ItemIsDefined()) 
    fMapStatus=map_get_int(fMapFileName,fSubsystemName,fItemName,length,array,time,&fEntryTime);
  return fMapStatus;
}


//_____________________________________________________________________________________

/*
float TMapMan::GetFloat(int position,int time,const char* name1,const char* name2,const char* name3) {
}
float TMapMan::GetChar(int position,int time,const char* name1,const char* name2,const char* name3) {
}
float TMapMan::GetInt(int position,int time,const char* name1,const char* name2,const char* name3) {
}

*/
