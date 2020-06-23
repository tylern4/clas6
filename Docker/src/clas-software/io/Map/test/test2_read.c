/* 
        test2_read.c
         Created           :  5-JUL-1994 by Thom Sulanke
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include <string.h>
#include <map_manager.h>
 

/***********************************************************************/
/*                                                                     */
/*   test2_read                                                        */
/*   ----------                                                        */
/*                                                                     */
/*         Created     :  1-OCT-1992    Author : Thom Sulanke          */
/*         Purpose     : do random read operations, use with test2_write*/
/*                                                                     */
/***********************************************************************/
 
main( )

{
  int i;                /* index */
  int array[100];        /* array of values */
  int atime, firsttime;
  int length, olength;
  int ret;         /* return code */
  int count;

  count = 0;
  while (1)
    {
      count++;
      switch (rand() & 0x3) {

      case 0:
	/* get last subsystem */
	ret = map_get_int("test2map","Z","Z",10,array,10,&firsttime);
	if ( ret != MAP_OK ) return (1);
	printf("%d\r",count);
	fflush(NULL);
	break;

      case 1:
	/* get last item */
	ret = map_get_int("test2map","B","Z",10,array,10,&firsttime);
	if ( ret != MAP_OK ) return (1);
	printf("%d\r",count);
	break;

      case 2:
	/* get last values in list */
	ret = map_get_int("test2map","C","C",10,array,10,&firsttime);
	if ( ret != MAP_OK ) return (1);
	printf("%d\r",count);
	break;

      }
    }
  return 0;
}
