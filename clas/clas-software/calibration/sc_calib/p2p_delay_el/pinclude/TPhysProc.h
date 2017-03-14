
#ifndef REACTIONS_INCLUDED_
#define REACTIONS_INCLUDED_

#include "c_stds.h"
#include "physEVNT.h"
#include "TReaction.h"


class physEVNT ;
class TReaction ;

class TPhysProc
{
 public:
 			TPhysProc();
 			~TPhysProc();
 	void 		Process( physEVNT_c* EVENT ) ;
 	
 private:

 protected:
 	int 		N_Reactions ;		// Number of reactions
 	int*		RcAllowed ;		// Flag fopr processes
 	TReaction** 	Reaction  ;		// Pointers to reactions	
};



#endif
