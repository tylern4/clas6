
#ifndef TREACTION_INCLUDED_
#define TREACTION_INCLUDED_

#include <iostream>
#include "c_stds.h"

class physEVNT_c;

class TReaction
{
 public:
 	virtual ~TReaction() { } ;
 	virtual void print( std::ostream *os ) { } ;
 	virtual void Process( physEVNT_c* EVENT ) = 0;
};


#endif
