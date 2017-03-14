/* 
   mapdump.c
   Created           : 18-DEC-1992 by Thom Sulanke
*/
 
#include <stdio.h>                   /* I/O definitions                      */
#include <string.h>
#include <limits.h>
#include <map_manager.h>
#include <map_internal.h>
#include <stdlib.h>
char *progname;
 

/***********************************************************************/
/*                                                                     */
/*   mapdump                                                           */
/*   -------                                                           */
/*                                                                     */
/*         Created     : 18-DEC-1992    Author : Thom Sulanke          */
/*         Purpose     : dump the map                                  */
/*                                                                     */
/***********************************************************************/
 
main( argc, argv )
     int argc;
     char *argv[];
{
  
  progname = argv[0];
  
  if ( argc != 2 )
    {
      printf("Usage: %s themap\n", progname);
      exit(0);
    }
  
  /* dump list of subsystems and items */
  
  return map_dump( argv[1] );

}
