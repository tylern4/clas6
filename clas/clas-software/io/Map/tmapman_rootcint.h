/* this wraper is necessary for the ddl.mk makefile to get the definition of 
TMapMan class which is in the header file include/tmapman.h and pass it to
rootcint fot the Cint dictionnary automatic generation */

/* CPP Class */
#include "tmapman.h"

/* C routines */
#include "map_manager.h"
