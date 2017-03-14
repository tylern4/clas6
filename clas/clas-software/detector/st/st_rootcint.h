/* this wraper is necessary for the ddl.mk makefile to get the definition of 
call routines which are in the header file include/call.h and pass it to
rootcint fot the Cint dictionnary automatic generation  - E.A. */
extern "C" {
#include "ntypes.h"
#include "bostypes.h"
	   }
#include "call.h"
