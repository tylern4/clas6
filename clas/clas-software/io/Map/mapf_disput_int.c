/* 
	mapf_disput_int.c  : 30-Nov-1993 by R. Hackenburg,


	FORTRAN Callable, as:

	LOGICAL FUNCTION mapf_disput_int( Filename, Subsystem, Itemname, ArrayLength, Array, Firsttime )

*  Inputs:
	CHARACTER*(*) Filename       !Map file name.
	CHARACTER*(*) Subsystemname  !Map subsytem name.
	CHARACTER*(*) Itemname       !Map item name.
	INTEGER       ArrayLength    !Length of Array (no. of elements).
	INTEGER       Array(*)       !Array of item-values.
	INTEGER       Firsttime      !Map "firsttime"

*  Returns:
*	.TRUE. for success,
*	.FALSE. for general map failure.

*  Description:
	This is an interface for SGI FORTRAN to Const Olchanksi's
	disIO_mapRequest routine, which provides limited mapmanager
	capabilities through the event Dispatcher.

*/

#include <stdio.h>                   /* I/O definitions                       */
#include <string.h>
#include <map_manager.h>
#include "map_internal.h"
#include "disIO.h"
#define TRUE  1
#define FALSE 0

/*  Prototype for a routine from strbytec_vms.c and *_sgi.c  */

	void strfc( char *filename, int filename_len, char *filename_copy, int filename_copy_len, int *filename_lnb );

/***********************************************************************/
/*                                                                     */
/*   mapf_disput_int                                                      */
/*                                                                     */
/***********************************************************************/
 
#ifndef vaxc
int mapf_disput_int_(char *filename, char *subsystemname,
                   char *itemname, int *arraylength, int *array, 
                   int *firsttime ,
	           int filename_len, int subsystemname_len, int itemname_len)
#else
int mapf_disput_int( char *filename, char *subsystemname,
                   char *itemname, int *arraylength, int *array, 
                   int *firsttime ,
	           int filename_len, int subsystemname_len, int itemname_len)
#endif
{
    char filename_copy[MAP_FILENAMELENGTH];
    char subsystemname_copy[MAP_NAMELENGTH];
    char itemname_copy[MAP_NAMELENGTH];
    int  filename_lnb;
    int  subsystemname_lnb;
    int  itemname_lnb;

    int status;

/*	Copy the FORTRAN-defined filename into a local, C-defined one:  */
	strfc( filename, filename_len, filename_copy, MAP_FILENAMELENGTH, &filename_lnb );

/*	Copy the FORTRAN-defined itemname into a local, C-defined one:  */
	strfc( itemname, itemname_len, itemname_copy, MAP_NAMELENGTH, &itemname_lnb );

/*	Copy the FORTRAN-defined subsystemname into a local, C-defined one:  */
	strfc( subsystemname, subsystemname_len, subsystemname_copy, MAP_NAMELENGTH, &subsystemname_lnb );

/*	map_put_int(filename_copy,subsystemname_copy,itemname_copy,*arraylength, array,*firsttime); */

	status = disIO_mapRequest( MAP_REQUEST_PUT, filename_copy, subsystemname_copy, itemname_copy
	                         , *firsttime, MAP_DATATYPE_INT, *arraylength, array );

/*	What's the return value (status) of that?   */
/*	Is it the same as the mapmanager status?    */
/*	status non-zero means it failed.     */
	if ( status ) { return( FALSE ); }
        else          { return( TRUE  ); }
}

