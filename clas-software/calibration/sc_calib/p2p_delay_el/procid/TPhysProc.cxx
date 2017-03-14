
#include "TPhysProc.h"
#include "RunControl.h"

#include "TTofCalib.h"


TPhysProc::TPhysProc()
{
 
 N_Reactions = 1 ;				// Number of reactions

 Reaction = new TReaction*[N_Reactions]  ;	// Create pointers  						// to reactions
 
 Reaction[0] = new TTofCalib;

 RcAllowed = new int[N_Reactions] ;		// Flag showing if one
 for ( int iRc = 0; iRc < N_Reactions; iRc++ )	// wants a process
  RcAllowed[iRc] = 1 ;				// analized

}


TPhysProc::~TPhysProc()
{
 for ( int iRc = 0 ; iRc < N_Reactions ; iRc++) 
  delete ( Reaction[iRc] )  ;
 delete[] RcAllowed ;
 delete[] Reaction;
 printf("PhysProc destructior Executed\n");
}



void TPhysProc::Process ( physEVNT_c* EVENT ) 
{
 for ( int iRc = 0 ; iRc < N_Reactions ; iRc++) 
  if ( RcAllowed[iRc] )  
   Reaction[iRc]->Process( EVENT );
}

