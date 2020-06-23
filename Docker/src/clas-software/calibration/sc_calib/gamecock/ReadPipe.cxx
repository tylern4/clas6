#include "jglobal.h"
#include "ReadPipe.h"
#include "gamecock.h"

using namespace std;

ReadPipe::ReadPipe (FILE* f_) : 
  TFileHandler (fileno(f_), TFileHandler::kRead), f(f_) {
  cout << "handler installed" << endl;
  gSystem->AddFileHandler(this);
}


Bool_t ReadPipe::ReadNotify() {
  char line [1024];
  if (feof(f)) {
    gCock->AddLog("============= program ended =====================");
    fclose(f);
    delete this;
    return kTRUE;
  }
  fgets (line, 1023, f);
  
  // remove trailing end of line
  if (strlen(line)) {
    char* last = line + strlen(line) - 1;
    if (*last == '\n') *last = 0;
  }

  // update progress meter
  string keyword;
  istringstream sline(line);
  sline >> keyword;
  if (keyword == "Processed:") {
    double percentage;
    sline >> percentage;
    int position = (int) floor (percentage + 0.5);
    gCock->GoProgress(position);
  }
  else 
    gCock->AddLog(line);
	    
  return kTRUE;
} 

