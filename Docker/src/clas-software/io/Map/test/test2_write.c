/* 
        test2_write.c
         Created           :  5-JUL-1994 by Thom Sulanke
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include <string.h>
#include <map_manager.h>
 

/***********************************************************************/
/*                                                                     */
/*   test2_write                                                        */
/*   ----------                                                        */
/*                                                                     */
/*         Created     :  1-OCT-1992    Author : Thom Sulanke          */
/*         Purpose     : do random write operations, use with test2_read*/
/*                                                                     */
/***********************************************************************/
 
main( )

{
  int i;                /* index */
  int array[100];        /* array of values */
  int atime, firsttime;
  int ret;         /* return code */
  int sub, item;   /* logical flags for subsystem or item is in map */
  int count;

  for ( i=0; i<10; i++ ) array[i] = i;

  /* add subsystem for end of list */

  ret = map_add_item("test2map","Z","Z",10,0);
  if ( ret != MAP_OK ) return (1);
  ret = map_put_int("test2map","Z","Z",10,array,10);
  if ( ret != MAP_OK ) return (1);

  /* add item for end of list */

  ret = map_add_item("test2map","B","Z",10,0);
  if ( ret != MAP_OK ) return (1);
  ret = map_put_int("test2map","B","Z",10,array,10);
  if ( ret != MAP_OK ) return (1);

  /* add values for end of list */

  ret = map_add_item("test2map","C","C",10,0);
  if ( ret != MAP_OK ) return (1);
  ret = map_put_int("test2map","C","C",10,array,10);
  if ( ret != MAP_OK ) return (1);

  sub = 0;
  item = 0;
  firsttime = 100;

  count = 0;
  while (1)
    {
      count++;
      printf("%d\r",count);
      fflush(NULL);

      switch (rand() & 0x3) {
      case 0:
	/* add/rem sub to/from top of list */
	if ( !sub )
	  ret = map_add_item("test2map","A","A",10,0);
	else
	  ret = map_rem_sub("test2map","A");
	if ( ret != MAP_OK ) return (1);
	sub = 1 - sub;
	break;

      case 1:
	/* add/rem item to/from top of list */
	if ( !item )
	  ret = map_add_item("test2map","B","B",10,0);
	else
	  ret = map_rem_item("test2map","B","B");
	if ( ret != MAP_OK ) return (1);
	item = 1 - item;
	break;

      case 2:
	/* put values at top of list */
	firsttime++;

	ret = map_put_int("test2map","C","C",10,array,firsttime);
	if ( ret != MAP_OK ) return (1);
	break;

      }
    }
  return 0;
}
