#include "TDBItem.h"

ClassImp(TDBItem)
  
  TDBItem::TDBItem(){
  // Defaulf Constructor
  //
  
}

TDBItem::~TDBItem(){
  // Defualt Destructor
  //
}

void TDBItem::Print(){
  // Prints out Current Item
  //
  cout << " Printing Item " << endl;
  cout << "ItemId  = " << fItemId << endl;
  cout << "fItemName " << fItemName << endl;
  cout << "fSubsystemId " << fSubsystemId << endl;
  cout << "fLength " << fLength << endl;
  cout << "fType " << fType << endl;
  cout << "fDescription " << fDescription << endl;
  cout << endl;
  
}
