#ifndef __READROC_H
#define __READROC_H

using namespace std;

#include <stdio.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <stdlib.h>
#include <string>
#include <string.h>
#include "BankDescription.h"

// this class represents one line (one channel) from the translation table
class RocChannel {
public:
  string bank;
  int sector;
  int layer;
  int stripe;
  int lrinx;
  int crate;
  int slot;
  int type;
  int channel;
  int id;
  RocChannel() {};
  RocChannel(char* cline);
  RocChannel(int cr,int sl,int ch) : crate(cr),slot(sl),channel(ch) {}

  void SetID(int id_) { id = id_; } 
  bool operator< (const RocChannel) const;
  bool operator> (const RocChannel) const;
  bool operator== (const RocChannel) const;
  bool operator!= (const RocChannel) const;
}; 

// this clas hold the translation for every channel from a given bank
class ReadRoc {
  BankDescription* bd;
  RocChannel* rc;
  int    status;
  int  parseLine (char* cline);
public:
  ReadRoc (BankDescription* bd_);
  ~ReadRoc () {};
  int  GetLength ()            { return bd->GetMaxIndex(); }
  int  GetStatus ()            { return status;}
  int  GetSlot  (int i)        { return rc[i].slot;}
  int  GetCrate (int i)        { return rc[i].crate; }
  int  GetTDCchannel (int i)   { return rc[i].channel; }
  RocChannel GetRoc (int i)    { return rc[i]; }
  void show ();
};

#endif

