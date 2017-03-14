#ifndef __JBOSFILE_H
#define __JBOSFILE_H

using namespace std;

#include <iostream>
#include <string>
#include <vector>
#include <stdlib.h>
#include <errno.h>
extern "C" {
#include "ntypes.h"
#include "bostypes.h"
}


//------------- JBosEvent -------------------------
class JBosEvent {
  bool isGood;
public:
  JBosEvent ();
  ~JBosEvent ();
  bool good () { return isGood; }
};


//------------- JBosFile --------------------------
class JBosFile {
  bool isGood;
public:
  JBosFile (string fileName);
  ~JBosFile ();
  bool good()         { return isGood;    }
  void setEof()       { isGood = false;   }
};

#endif
