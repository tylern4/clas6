#include "tmapman.h"

//_____________________________________________________________________________________________
int TMapMan::PutCharArray(int length,const char* array,int time,
			  const char* name1,const char* name2,const char* name3) {
  // Put array of char, parameters are (length, chararray, time)
  fMapStatus=-1;
  SetMap(name1,name2,name3);
  if (time>0) {
    if (ItemIsDefined())
      fMapStatus=map_put_char(fMapFileName,fSubsystemName,fItemName,length,array,time);
    else cerr << "ERROR: item is not defined, char array not put"<<endl;
  } else cerr << "ERROR: Can't put item at time "<<time<<endl;
  return fMapStatus;
}

//_____________________________________________________________________________________________
int TMapMan::PutFloatArray(int length,const float* array,int time,
			   const char* name1,const char* name2,const char* name3) {
  // Put array of float, parameters are (length, floatarray, time)
  fMapStatus=-1;
  SetMap(name1,name2,name3);
  if (time>0) {
    if (ItemIsDefined())
      fMapStatus=map_put_float(fMapFileName,fSubsystemName,fItemName,length,array,time);
    else cerr << "ERROR: item is not defined, float array not put"<<endl;
  } else cerr << "ERROR: Can't put item at time "<<time<<endl;
  return fMapStatus;
}

//_____________________________________________________________________________________________
int TMapMan::PutIntArray(int length,const int* array,int time,
			 const char* name1,const char* name2,const char* name3) {
  // Put array of int, parameters are (length, intarray, time)
  fMapStatus = -1;
  SetMap(name1,name2,name3);
  if (time>0) {
    if (ItemIsDefined())
      fMapStatus=map_put_int(fMapFileName,fSubsystemName,fItemName,length,array,time);
    else cerr << "ERROR: item is not defined, int array not put"<<endl;
  } else cerr << "ERROR: Can't put item at time "<<time<<endl;
  return fMapStatus;
}
