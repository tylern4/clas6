#ifndef __READPIPE_H
#define __READPIPE_H
#include <iostream>
#include <stdio.h>
#include "ROOT.h"

class ReadPipe : public TFileHandler {
  FILE* f;
public:
  ReadPipe (FILE* f_);
  Bool_t ReadNotify();
};

#endif
