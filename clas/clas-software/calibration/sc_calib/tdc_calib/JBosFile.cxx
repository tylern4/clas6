#include <string.h>
#include "JBosFile.h"

extern bool lookforSync;

//------------- JBosFile -------------------------

JBosFile::JBosFile (string fileName) : isGood(true) {
  string openCommand("OPEN BOSINPUT UNIT=1 FILE=\"");
  openCommand += fileName + string("\" READ"); 
  int iresult;
  if (!(iresult = fparm_c((char*)openCommand.c_str()))) {
    cerr << "Unable to open file " << fileName << ": " <<
      strerror(errno);
    isGood = false;
  }
  cout << "running " << fileName << "... " << endl; 
} 

JBosFile::~JBosFile () {
  fparm_c("CLOSE BOSINPUT");
} 

//------------- JBosEvent -------------------------
JBosEvent::JBosEvent () : isGood(true) {
  if (! getBOS(&bcs_,1,"E") ) {
    cerr << "Error reading next event (might be end of file)" << endl;
    isGood = false;
  }
}
// destructor clears bos banks
JBosEvent::~JBosEvent () {
  dropAllBanks(&bcs_,"E");
  cleanBanks(&bcs_);
}
