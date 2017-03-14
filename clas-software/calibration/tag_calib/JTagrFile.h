#ifndef __JTAGRFILE_H
#define __JTAGRFILE_H

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <stdlib.h>

class JTagrFile {
  std::fstream* f;
  std::ios_base::openmode  flags;
public:
  JTagrFile (std::string fileName, 
	     std::ios_base::openmode flags_=std::ios::in);
  ~JTagrFile();
  void write();
  void read();
  bool good();
};

#endif
