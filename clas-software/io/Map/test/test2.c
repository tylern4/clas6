/* 
        test2.c
         Created           :  1-OCT-1992 by Thom Sulanke
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include <string.h>
#include <map_manager.h>
 

/***********************************************************************/
/*                                                                     */
/*   test2                                                        */
/*   ----------                                                        */
/*                                                                     */
/*         Created     :  1-OCT-1992    Author : Thom Sulanke          */
/*         Purpose     : test the map routines when called from c      */
/*                                                                     */
/***********************************************************************/
 
main( )

{
    char subsystemname[MAP_NAMELENGTH], itemname[MAP_NAMELENGTH];
    int i;                /* index */
    int array[100];        /* array of values */
     int atime, vtime;
     int length;
    int nvals;

    map_add_item( "TheMap", "ABC", "repeat", 2, 0 );
 
/* add some values */
 
    for ( i=0 ; i<100 ; i++ )
    {
        map_get_int( "TheMap", "ABC", "repeat", 2, array, 0, &vtime);
	if ( vtime < 0 )
	  vtime = 1000;
	else
	  vtime = vtime + 11;
        array[0] = 2;
        array[1] = i;
        map_put_int( "TheMap", "ABC", "repeat", 2, array, vtime );
    }  /* For */

    return 0;
}
