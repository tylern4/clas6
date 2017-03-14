/* 
        mapcreate
         Created           : 18-DEC-1992 by Thom Sulanke
*/
 
#include <stdio.h>                   /* I/O definitions                */
#include <map_manager.h>
#include <stdlib.h>
char *progname;


/***********************************************************************/
/*                                                                     */
/*   mapcreate                                                         */
/*   ---------                                                         */
/*                                                                     */
/*         Created     : 18-DEC-1992    Author : Thom Sulanke          */
/*         Purpose     : create a map                                  */
/*                                                                     */
/***********************************************************************/
 
void main( argc, argv )
    int argc ;
    char *argv[]; 
{
    int f1, f2, n ;
    
    progname = argv[0];
    
    if ( argc != 2 )
    {
        printf("Usage: %s themap\n", progname);
	exit(0);
    }
    map_create(argv[1]);

    exit(0);
}
