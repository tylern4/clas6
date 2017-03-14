#include <mwKfit.h>
#include <clasEvent.h>

void setPrlinkx(string pr)
{
  cerr << "setting prlink file: " << pr << endl;
  sprintf(trktcl_.spar_prlink_name,pr.c_str());
} 
