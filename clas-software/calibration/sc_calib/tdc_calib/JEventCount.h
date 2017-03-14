#ifndef __EVENTCOUNT_H
#define __EVENTCOUNT_H

#include <stdio.h>
#include <iostream>
#include <string>
#include <string.h>
#include <vector>
#include <sstream>
#include <map>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

using namespace std;

extern "C" {
  void count_initialize(int nfiles, char* filelist[]);
  double get_percent(int ifile, int eventNo);
} 

enum formatid_t { fm_unknown, fm_int, fm_float, fm_short, fm_byte };

class JAtom {
  int z;
  formatid_t format;
public:
  JAtom(int z_, formatid_t format_): z(z_), format(format_) {}
  friend ostream& operator<< (ostream&, const JAtom);
};

class JBuffer;

class JBosFileBank {
public:
  unsigned int headLength;
  unsigned int bankLength;
  string name;
  string format;
  int sector;
  int sizeofElem;
  int numberElem;
  int flagCont;
  int offsetCont;
  int* data;
  JBosFileBank() : headLength(0), bankLength(0), data(NULL) {} 
  JBosFileBank(JBuffer* buf);
  ~JBosFileBank() {} // if (data) delete [] data; }
  JBosFileBank operator= (const JBosFileBank b);
  string GetKey();
  friend bool operator== (const JBosFileBank a, const JBosFileBank b);
  friend ostream& operator<< (ostream&, const JBosFileBank);
};

class JBuffer {
  int *b;
  int bhead[13];
  bool swap;
  int len;
  int used;
  JBosFileBank* bank;
  FILE* ff;
public:
  int inx;
  JBuffer(FILE* ff_): b(NULL), swap(false), len(0), bank(NULL), ff(ff_), inx(0) { }
  int operator[] (int index);
  int GetSwap (int index);
  int GetLength () { return len; }
  int GetNused () { return used; }
  bool IsNextBank ();
  bool eof() { return feof(ff); }
  void ReadEventHeader ();
  void ReadBank ();
  JBosFileBank* GetBank () { return bank; }
  int  ReadBlock( ); 
  void ReadLastblock();
  string GetHolerith (int index, int length);
};

class JEventCount {
  bool ok;
  int  first;
  int  last;
public:
  JEventCount (const char* filename);
  bool   good()     { return ok && last>first;      }
  int    GetFirst() { return ok ? first : 0;        }
  int    GetLast()  { return ok ? last  : 0;        }
  double GetDiff()  { return ok ? last-first : 1E9; } 
};

class JFileInfo: public JEventCount {
  string name;
  long int bsize;
  double perc0;
  double perc1;
public:
  JFileInfo(string name_);
  long int GetBsize() { return bsize; }
  void SetPercent(double min, double max) { perc0=min; perc1=max; }
  void SetScale(double x) { perc0*=x; perc1*=x; }
  double GetPercent(int eventNo);
};

class JFileCount {
  vector<JFileInfo> v;
public:
  JFileCount(vector<string>* v_);
  JFileCount(int nfiles, char* filelist[]);
  void Constructor();
  double GetPercent(int ifile, int iev) { return v[ifile].GetPercent(iev); }
};

static JFileCount * gFileCount;

#endif
