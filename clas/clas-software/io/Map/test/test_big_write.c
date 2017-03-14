/* 
        test_big_write.c
         Created           : 29-OCT-1993 by Thom Sulanke
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include <string.h>
#include <time.h>
#include <map_manager.h>
 

/***********************************************************************/
/*                                                                     */
/*   test_big_write                                                        */
/*   ----------                                                        */
/*                                                                     */
/*         Created     : 29-OCT-1993    Author : Thom Sulanke          */
/*         Purpose     : test the map for various number of items of   */
/*                       various sizes                                 */
/*                                                                     */
/***********************************************************************/
 
void oops(int status, int code)
{
  printf("unexpected status code: %d, call code: %d\n",status,code);
  exit(1);
}

main( )

{
    char subsystemname[MAP_NAMELENGTH], itemname[MAP_NAMELENGTH];
    char nextsubsystemname[MAP_NAMELENGTH], nextitemname[MAP_NAMELENGTH];
    int i;                /* index */
    int atime, vtime;
    int length, olength;
    int type;
    int status;
    int item_size = 300;
    int number_item = 4*7*2;
    int number_times = 40;
    float *farray;
    int iitem;
    int itimes;
    char item[] = "item000X";
    time_t t1,t2;

/* add items */

    for ( iitem = 0 ; iitem<number_item ; iitem++ )
      {
	sprintf(item,"item%d",iitem+1000);
	if ( (status = map_add_item( "BigMap", "subsystem", item, item_size, 1 )) != MAP_OK ) oops(status,1);
      }

    farray = (float *) calloc(item_size,sizeof(float));
    for ( i = 0 ; i<item_size ; i++ )
      farray[i] = i;

/* add some values */
 
    for ( itimes=0 ; itimes<number_times ; itimes++ )
      {
	for ( iitem = 0 ; iitem<number_item ; iitem++ )
	  {
	    farray[0] = iitem;
	    farray[1] = itimes;
	    sprintf(item,"item%d",iitem+1000);
	    if ( (status = map_put_float( "BigMap", "subsystem", item, item_size, farray, itimes+10 )) != MAP_OK ) oops(status,16);
	  }
      }
    
    printf("all values added\n");
    
    return 0;
}

