/* 
        test_read.c
         Created           :  5-JUL-1994 by Thom Sulanke
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include <string.h>
#include <map_manager.h>
 

/***********************************************************************/
/*                                                                     */
/*   test_read                                                        */
/*   ----------                                                        */
/*                                                                     */
/*         Created     :  1-OCT-1992    Author : Thom Sulanke          */
/*         Purpose     : do random read operations, use with test_write*/
/*                                                                     */
/***********************************************************************/
 
main( )

{
  char subsystemname[MAP_NAMELENGTH], itemname[MAP_NAMELENGTH];
  int i;                /* index */
  int array[100];        /* array of values */
  float farray[100];        /* array of values */
  char carray[] = "abcdefghijklmnopqrstuvwxyz";
  char fixarray[] = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz";
  int atime, firsttime;
  int length, olength;
  int nvals;
  int ret;         /* return code */
  int change;

  while (1)
    {
      switch (rand() & 0x3) {

      case 0: case 3:
	/* get_int */
	atime = (rand() & 0x7) + 10;
	switch (rand() & 0x7) {
	case 0: 
	  ret = map_get_int("testmap","int","int0",10,array,atime,&firsttime);
	  length = 10;
	  break;
	case 1: 
	  ret = map_get_int("testmap","int","int1",11,array,atime,&firsttime);
	  length = 11;
	  break;
	case 2: 
	  ret = map_get_int("testmap","int","int2",12,array,atime,&firsttime);
	  length = 12;
	  break;
	case 3: 
	  ret = map_get_int("testmap","int","int3",13,array,atime,&firsttime);
	  length = 13;
	  break;
	case 4: 
	  ret = map_get_int("testmap","int","int4",14,array,atime,&firsttime);
	  length = 14;
	  break;
	case 5: 
	  ret = map_get_int("testmap","int","int5",15,array,atime,&firsttime);
	  length = 15;
	  break;
	case 6: 
	  ret = map_get_int("testmap","int","int6",16,array,atime,&firsttime);
	  length = 16;
	  break;
	case 7: 
	  ret = map_get_int("testmap","int","int7",17,array,atime,&firsttime);
	  length = 17;
	  break;
	}
	if ( ret == MAP_USER_WARN_NOMATCH || ret == MAP_USER_WARN_NOFIND ) break; 
	if ( ret != MAP_OK ) return (1);
	for ( i=0; i<length; i++ ) 
	  if ( (array[i]%10000) != 100*firsttime + i) 
	    return(1);
	break;

      case 1:
	/* get_float */
	atime = (rand() & 0x7) + 10;
	switch (rand() & 0x7) {
	case 0: 
	  ret = map_get_float("testmap","float","float0",10,farray,atime,&firsttime);
	  length = 10;
	  break;
	case 1: 
	  ret = map_get_float("testmap","float","float1",11,farray,atime,&firsttime);
	  length = 11;
	  break;
	case 2: 
	  ret = map_get_float("testmap","float","float2",12,farray,atime,&firsttime);
	  length = 12;
	  break;
	case 3: 
	  ret = map_get_float("testmap","float","float3",13,farray,atime,&firsttime);
	  length = 13;
	  break;
	case 4: 
	  ret = map_get_float("testmap","float","float4",14,farray,atime,&firsttime);
	  length = 14;
	  break;
	case 5: 
	  ret = map_get_float("testmap","float","float5",15,farray,atime,&firsttime);
	  length = 15;
	  break;
	case 6: 
	  ret = map_get_float("testmap","float","float6",16,farray,atime,&firsttime);
	  length = 16;
	  break;
	case 7: 
	  ret = map_get_float("testmap","float","float7",17,farray,atime,&firsttime);
	  length = 17;
	  break;
	}
	if ( ret == MAP_USER_WARN_NOMATCH || ret == MAP_USER_WARN_NOFIND ) break; 
	if ( ret != MAP_OK ) return (1);
	for ( i=0; i<length; i++ ) 
	  if ( (((int)farray[i])%10000) != 100*firsttime + i) 
	    return(1);
	break;

      case 2:
	/* get_char */
	atime = (rand() & 0x7) + 10;
	switch (rand() & 0x7) {
	case 0: 
	  ret = map_get_char("testmap","char","char0",10,carray,atime,&firsttime,&olength);
	  length = 10;
	  break;
	case 1: 
	  ret = map_get_char("testmap","char","char1",11,carray,atime,&firsttime,&olength);
	  length = 11;
	  break;
	case 2: 
	  ret = map_get_char("testmap","char","char2",12,carray,atime,&firsttime,&olength);
	  length = 12;
	  break;
	case 3: 
	  ret = map_get_char("testmap","char","char3",13,carray,atime,&firsttime,&olength);
	  length = 13;
	  break;
	case 4: 
	  ret = map_get_char("testmap","char","char4",14,carray,atime,&firsttime,&olength);
	  length = 14;
	  break;
	case 5: 
	  ret = map_get_char("testmap","char","char5",15,carray,atime,&firsttime,&olength);
	  length = 15;
	  break;
	case 6: 
	  ret = map_get_char("testmap","char","char6",16,carray,atime,&firsttime,&olength);
	  length = 16;
	  break;
	case 7: 
	  ret = map_get_char("testmap","char","char7",17,carray,atime,&firsttime,&olength);
	  length = 17;
	  break;
	}
	if ( ret == MAP_USER_WARN_NOMATCH || ret == MAP_USER_WARN_NOFIND ) break; 
	if ( ret != MAP_OK ) 
	  return (1);
	if ( olength != length ) 
	  return (1);
	for ( i=0; i<length; i++ ) 
	  if ( carray[i] != fixarray[i+length] ) 
	    return(1);
	break;

      }
    }
  return 0;
}
