#include "tmapman.h"

char mapinttypename[10]=_MAPINTS;
char mapfloattypename[10]=_MAPFLOATS;
char mapchartypename[10]=_MAPSTRINGS;
char mapundefinedtypestring[15]="undefined type";

//_____________________________________________________________________________________
char TMapMan::MapExists(const char* name) {
  if (strlen(name)!=0) SetMapFileName(name);
  char save[99];
  strcpy(save,fSubsystemName);
  SetMapSubsystemName("*");
  NextSubsystem();
  strcpy(fSubsystemName,save);
  if (fMapStatus<0) return 0;
  return 1;
}

//_____________________________________________________________________________________
int TMapMan::Print(int time,const char* name1,const char* name2,const char* name3) {
// Prints informations on current seting;
  SetMap(name1,name2,name3);
  int returnval=-1;
  if (!MapIsDefined()) {
    cerr << "Map File name is not defined"<<endl;
  } else {
    cerr << "Map File:"; PrintMapFileName(); cerr<< endl;
    if (MapExists()) returnval=PrintSubsystem(time);
    else cerr << "Warning : no such file "<<endl;
  }
  return returnval;
}

//_____________________________________________________________________________________
/*char TMapMan::FileExist() {
  int returnval=0;
  char savesubname[99];
  strcpy(savesubname,fSubsystemName);
  if (NextSubsystem()!=-7) returnval= 1;
  SetMapSubsystemName(savesubname);
  Read_NItems();
  return returnval;
}
*/

//_____________________________________________________________________________________
int TMapMan::PrintSubsystem(int time,const char* name1,const char* name2,const char* name3) {
// Prints informations on current subsystem;
  SetMap(name1,name2,name3);
  int returnval=-2;
  
  if (!SubsystemIsDefined()) {
    cerr << "Subsystem name is not defined"<<endl;
    cerr << "Subsystems are:" << endl;
    while (!NextSubsystem()) {
      cerr << "> ";
      PrintMapSubsystemName();
      cerr << " containing "<<fNItems<<" items"<<endl;
    }
    ClearSubsystemName();
  } else {
    cerr << "Subsystem: ";
    PrintMapSubsystemName();
    cerr<<endl;
    if (SubsystemExists()) returnval=PrintItem(time);
    else cerr << " Warning : no such subsystem "<<endl;
  }
  return returnval;
}

//_____________________________________________________________________________________
int TMapMan::PrintItem(int time,const char* name1,const char* name2,const char* name3) {
// Prints informations on current item;
  int returnval=-3;
  int* dumint;
  float* dumfloat;
  char* dumchar;
  SetMap(name1,name2,name3);
  if (!ItemIsDefined()) {
    cerr << "Item name is not defined"<<endl;
    cerr << "Items are:" << endl;
    while (!NextItem()) {
      PrintMapItemName();
      cerr << " : length: "<<fLength<<" - type: "<<PrintType(fType)
	   <<" - Nentries: "<<fNArrays<<endl;
    }
    ClearItemName();
  } else {
    cerr << "Item: ";
    PrintMapItemName();
    if (ItemExists()) {
      Read_ItemInfo();
      cerr << " - Length: "<<fLength<<" - type: "<< PrintType(fType)<<endl;
      dumint = new int[fLength];
      dumfloat = new float[fLength];
      dumchar = new char[fLength];
      if (time>=0) {
	int timeentry = Read_DefItem(time,dumint,dumfloat,dumchar);
	cout << "Time of entry was : " << timeentry<<endl;
	for (int iloop=0;iloop<fLength;iloop++) {
	  if ( iloop % 10 == 0) cerr <<endl;
	  cerr <<" - ";
	  switch (fType) {
	  case _MAPINTV:
	    cerr << dumint[iloop];
	    break;
	  case _MAPFLOATV:
	    cerr << dumfloat[iloop];
	    break;
	  case _MAPSTRINGV:
	    cerr << dumchar[iloop];
	    break;
	  default:
	    break;
	  } // switch
	} // for
      } else {// else if (time>=0)
	time = 0;
	cout << "UNDEF ";
	while (time>=0) {
	  time = Read_DefItem(time,dumint,dumfloat,dumchar);
	  cout << " <- "<<time;
	  time--;
	  if (time==0) time--; // patch in case there is one entry at time=1 !!
	}
	cout << endl;
      } // else
      delete [] dumint;
      delete [] dumfloat;
      delete [] dumchar;
      returnval=0;
    } else {
      cerr << endl << "Warning : no such item "<< endl;
      returnval =-1;
    }
  }
  return returnval;
}

//____________________________________________________________________________________
int TMapMan::Read_DefItem(int time,int* dumint,float *dumfloat,char* dumchar) {
  
  switch (fType) {
  case _MAPINTV:
    GetIntArray(fLength,dumint,time);
    break;
  case _MAPFLOATV:
    GetFloatArray(fLength,dumfloat,time);
    break;
  case _MAPSTRINGV:
    GetCharArray(fLength,dumchar,time);
    break;
  default:
    break;
  }
  return fEntryTime;
}

//______________________________________________________________________________________
int TMapMan::Read_EntryTime(int time,const char* name1,const char* name2,const char* name3) {
  SetMap(name1,name2,name3);
  int* dumint;
  float* dumfloat;
  char* dumchar;
  Read_ItemInfo();
  dumint = new int[fLength];
  dumfloat = new float[fLength];
  dumchar = new char[fLength];
  Read_DefItem(time,dumint,dumfloat,dumchar);
  delete [] dumint;
  delete [] dumfloat;
  delete [] dumchar;
  return fEntryTime;
}


//_____________________________________________________________________________________
char* TMapMan::PrintType(int type) {
  char* TypeString;
  switch(type) {
  case (_MAPINTV):
    TypeString=mapinttypename;
    break;
  case (_MAPFLOATV):
    TypeString=mapfloattypename;
    break;
  case (_MAPSTRINGV):
    TypeString=mapchartypename;
    break;
  default:
    TypeString=mapundefinedtypestring;
    break;
  }
  return TypeString;
}

//_____________________________________________________________________________________
int TMapMan::Read_NItems(const char* name1,const char* name2) {
  // returns -1 if no such subsystem;

  int returnval = -1;
  char subsystemsave[99];
  SetMapSub(name1,name2);
  
  if (SubsystemIsDefined()) {
    strcpy(subsystemsave,fSubsystemName);
    ClearSubsystemName();
    int limit=0;
    int notequal=1;
    while (!limit && notequal) {
      limit=NextSubsystem();
      if (!limit) notequal=strcmp(fSubsystemName,subsystemsave);
    }
    if (!limit) returnval=fNItems;
    strcpy(fSubsystemName,subsystemsave);
  } 
  return returnval;
}

//_____________________________________________________________________________________
int TMapMan::Read_ItemInfo(const char* name1,const char* name2,const char* name3) {
  // map_stat_item
  SetMap(name1,name2,name3);
  fMapStatus=-10;
  if (ItemIsDefined()) {
    fMapStatus=map_stat_item(fMapFileName,fSubsystemName,fItemName,&fLength,&fType);
  }
  return fMapStatus;
}

//_____________________________________________________________________________________
int TMapMan::Read_ItemLength(const char* name1,const char* name2,const char* name3) {
  SetMap(name1,name2,name3);
  int returnval=0;
  if (!Read_ItemInfo()) returnval=fLength;
  return returnval;
}

//_____________________________________________________________________________________
int TMapMan::Read_ItemType(const char* name1,const char* name2,const char* name3) {
  SetMap(name1,name2,name3);
  int returnval=0;
  if (!Read_ItemInfo()) returnval=fType;
  return returnval;
}

//_____________________________________________________________________________________
int TMapMan::Read_NArrays(const char* name1,const char* name2,const char* name3) {
  // returns -1, if there is no such item.
  SetMap(name1,name2,name3);
  char itemNameSave[99];
  int returnval = -1;
  char limit=0;
  char notequal=1;

  if(ItemIsDefined()) {
    strcpy(itemNameSave,fItemName);
    ClearItemName();
    while (!limit && notequal) {
      limit = NextItem();
      if (!limit) notequal=strcmp(itemNameSave,fItemName);
    }
    if (!limit) returnval = fNArrays;
    SetMapItemName(itemNameSave);
  }
  return returnval;
}

//_____________________________________________________________________________________
int TMapMan::GetStatus() {
  return fMapStatus;
}

//_____________________________________________________________________________________
void TMapMan::PrintStatus() {
  PrintMessStatus(GetStatus());
}

//_____________________________________________________________________________________
void TMapMan::PrintMessStatus(int stat) {
  
  switch (stat) {
  case 0:
    cerr <<"Everything is fine " <<endl;
    break;
  case -1:
    cerr <<"ERROR: user supplied argument incorrect "<<endl;
    break;
  case -2:
    cerr<<"WARNING: previously performed operation is not repeated "<<endl;
    break;
  case -3:
    cerr <<"WARNING: time not found for item "<<endl;
    break;
  case -4:
    cerr <<"WARNING: subsystem or item name no found "<<endl;
    break;
  case -5:
    cerr <<"WARNING: values not replaced for previously filled time "<<endl;
    break;
  case -6:
    cerr <<"CLASS WARNING: end of list of items "<<endl;
    break;
  case -7:
    cerr <<"CLASS WARNING: end of list of subsystems "<<endl;
    break;
  case -10:
    cerr <<"CLASS ERROR: Object item not defined "<<endl;
    break;
  case -101:
    cerr <<"SYSTEM ERROR: system error opening map file "<<endl;
    break;
  case -102:
    cerr <<"SYSTEM ERROR: system file i/o error after opening "<<endl;
    break;
  case -103:
    cerr <<"SYSTEM ERROR: system error allocating/deallocating memory "<<endl;
    break;
  default:
    cerr<<"Unrecognize ERROR type "<<endl;
    break;
  } 
}
