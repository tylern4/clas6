#ifndef __TNtupleUtil
#define __TNtupleUtil
//*-- Author :    Paul Eugenio 22-Feb-1999
//*-- CMZ : PME 22-Feb-1999

/// TNtupleUtil:  A ROOT TNtuple utility class
/**
 This object simplifies the creation of TNtuples by 
 storing ntuple creation information until one event
 is processased after which the ntuple is created
*/


#include <stdlib.h>
#include <iostream>
#include <string.h>
#include <Rtypes.h>

using namespace std;

/*   Work in progress
clas NtupleUtil{
 private:
  TntpNames ntpname;
  TNtuple *_ntp;
 public:
  NtupleUtil();
  ~TntpNames();
  fill(float value, char *name, char *summary);
  create();

};
*/
/// TntpLables:  A container for the vector lables
/**
 This object simplifies the creation of TNtuples by
 storing ntuple creation information until one event
 is processased after which the ntuple is created
*/

class TntpLables{
private: 
  /// array of vector lables is format "var1:var2:..."
  Char_t flables[2000];  // a static cheat 
  /// number of ntuple vector lables
  Int_t fNlabels;  
  /// static char not used at present
  static Char_t *gflables;
public:
  /// default constructor
  TntpLables();
  /// default destructor
  ~TntpLables();
  /// add new vector lable to the container
  void Add(Char_t *lable);
  /// get the array of vector lables
  Char_t * GetLables() {return flables;}
  /// get the number of vector lables
  Int_t GetNlabels() {return fNlabels;}
  void Print(void);
};
 
#endif
