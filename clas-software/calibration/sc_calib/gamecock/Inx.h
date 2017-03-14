#ifndef __INX_H
#define __INX_H

#include <iostream>
#include "jdefine.h"

using namespace std;

class Inx {
  int index;
public:
  Inx(): index(0) {}
  Inx(int index_): index(index_) {
    if (index<0 || index >= N_CHANNEL) 
      throw "Index out of range";
  }
  
  int GetIndex()   {return index;}
  int GetCSector() {return index/N_SECTOR; }
  int GetCStripe() {return index%N_SECTOR; }
  int GetFSector() {return GetCSector() + 1; }
  int GetFStripe() {return GetCStripe() + 1; }

  Inx& operator++ () { index++; return *this; }
  Inx& operator++ (int) {index ++; return *this; }
  Inx& operator= (int a) { index = a; return *this; }
  int operator= (Inx& i) { return index; }
  friend bool operator<  (Inx& i, int a);
  friend bool operator>  (Inx& i, int a);
  friend bool operator>= (Inx& i, int a);
  friend bool operator<= (Inx& i, int a);
  friend bool operator== (Inx& i, int a);
  friend bool operator!= (Inx& i, int a);
  friend ostream& operator<< (ostream& os, Inx& i);
};

#endif
