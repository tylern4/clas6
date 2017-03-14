/* 
        test_sig.c
         Created           :  3-MAR-1994 by Thom Sulanke
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include <string.h>
#include <signal.h>
#include <map_manager.h>
 

/***********************************************************************/
/*                                                                     */
/*   test_sig                                                          */
/*   ----------                                                        */
/*                                                                     */
/*         Created     :  3-MAR-1994    Author : Thom Sulanke          */
/*         Purpose     : test the map routines with signals            */
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
  static void (*saved_handler)(int v);

  map_add_item( "TheMap", "ABC", "repeat", 2, 0 );
  
  /* add some values */
  
  for ( i=0 ; i<1000 ; i++ )
    {
      saved_handler = signal(SIGUSR1,SIG_IGN);
      if ( saved_handler != SIG_IGN )
	{
	  fprintf(stderr, "Handler for SIGUSR1 was not SIG_IGN.\n");
	  fprintf(stderr, "Handler for SIGUSR1 has been set to SIG_IGN by test_sig.\n");
	}
      
      map_get_int( "TheMap", "ABC", "repeat", 2, array, 0, &vtime);
      vtime = vtime + 7;
      array[0] = 1;
      array[1] = i;
      sleep(1);
      map_put_int( "TheMap", "ABC", "repeat", 2, array, vtime );
    }  /* For */
  
  return 0;
}
