
#include "TReaction.h"

std::ostream &operator<<( std::ostream &os, TReaction &Reaction )
{
 Reaction.print( &os ) ;
 return os ;
}

// 	TReaction::~TReaction(){} ;
//void 	TReaction::print( ostream *os ) { } ;
