/* 
        test_write.c
         Created           :  5-JUL-1994 by Thom Sulanke
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include <string.h>
#include <map_manager.h>
 

/***********************************************************************/
/*                                                                     */
/*   test_write                                                        */
/*   ----------                                                        */
/*                                                                     */
/*         Created     :  1-OCT-1992    Author : Thom Sulanke          */
/*         Purpose     : do random write operations, use with test_read*/
/*                                                                     */
/***********************************************************************/
 
main( )

{
  char subsystemname[MAP_NAMELENGTH], itemname[MAP_NAMELENGTH];
  int i;                /* index */
  int array[100];        /* array of values */
  float farray[100];        /* array of values */
  char carray[] = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz";
  int atime, firsttime;
  int length;
  int nvals;
  int ret;         /* return code */
  int change;

  while (1)
    {
      switch (rand() & 0x7) {
      case 0:
	/* add_item int */
	switch (rand() & 0x7) {
	case 0: 
	  ret = map_add_item("testmap","int","int0",10,0);
	  break;
	case 1: 
	  ret = map_add_item("testmap","int","int1",11,0);
	  break;
	case 2: 
	  ret = map_add_item("testmap","int","int2",12,0);
	  break;
	case 3: 
	  ret = map_add_item("testmap","int","int3",13,0);
	  break;
	case 4: 
	  ret = map_add_item("testmap","int","int4",14,0);
	  break;
	case 5: 
	  ret = map_add_item("testmap","int","int5",15,0);
	  break;
	case 6: 
	  ret = map_add_item("testmap","int","int6",16,0);
	  break;
	case 7: 
	  ret = map_add_item("testmap","int","int7",17,0);
	  break;
	}
	if ( ret != MAP_OK && ret != MAP_USER_WARN_NOREPEAT) return (1);
	break;

      case 1:
	/* add_item float */
	switch (rand() & 0x7) {
	case 0: 
	  ret = map_add_item("testmap","float","float0",10,1);
	  break;
	case 1: 
	  ret = map_add_item("testmap","float","float1",11,1);
	  break;
	case 2: 
	  ret = map_add_item("testmap","float","float2",12,1);
	  break;
	case 3: 
	  ret = map_add_item("testmap","float","float3",13,1);
	  break;
	case 4: 
	  ret = map_add_item("testmap","float","float4",14,1);
	  break;
	case 5: 
	  ret = map_add_item("testmap","float","float5",15,1);
	  break;
	case 6: 
	  ret = map_add_item("testmap","float","float6",16,1);
	  break;
	case 7: 
	  ret = map_add_item("testmap","float","float7",17,1);
	  break;
	}
	if ( ret != MAP_OK && ret != MAP_USER_WARN_NOREPEAT) return (1);
	break;

      case 2:
	/* add_item float */
	switch (rand() & 0x7) {
	case 0: 
	  ret = map_add_item("testmap","char","char0",10,2);
	  break;
	case 1: 
	  ret = map_add_item("testmap","char","char1",11,2);
	  break;
	case 2: 
	  ret = map_add_item("testmap","char","char2",12,2);
	  break;
	case 3: 
	  ret = map_add_item("testmap","char","char3",13,2);
	  break;
	case 4: 
	  ret = map_add_item("testmap","char","char4",14,2);
	  break;
	case 5: 
	  ret = map_add_item("testmap","char","char5",15,2);
	  break;
	case 6: 
	  ret = map_add_item("testmap","char","char6",16,2);
	  break;
	case 7: 
	  ret = map_add_item("testmap","char","char7",17,2);
	  break;
	}
	if ( ret != MAP_OK && ret != MAP_USER_WARN_NOREPEAT) return (1);
	break;

      case 3:
	/* rem_item */

	switch (rand() & 0x7f) {
	case 0: 
	  ret = map_rem_item("testmap","int","int0");
	  break;
	case 1: 
	  ret = map_rem_item("testmap","int","int1");
	  break;
	case 2: 
	  ret = map_rem_item("testmap","int","int2");
	  break;
	case 3: 
	  ret = map_rem_item("testmap","int","int3");
	  break;
	case 4: 
	  ret = map_rem_item("testmap","int","int4");
	  break;
	case 5: 
	  ret = map_rem_item("testmap","int","int5");
	  break;
	case 6: 
	  ret = map_rem_item("testmap","int","int6");
	  break;
	case 7: 
	  ret = map_rem_item("testmap","int","int7");
	  break;
	case 10: 
	  ret = map_rem_item("testmap","float","float0");
	  break;
	case 11: 
	  ret = map_rem_item("testmap","float","float1");
	  break;
	case 12: 
	  ret = map_rem_item("testmap","float","float2");
	  break;
	case 13: 
	  ret = map_rem_item("testmap","float","float3");
	  break;
	case 14: 
	  ret = map_rem_item("testmap","float","float4");
	  break;
	case 15: 
	  ret = map_rem_item("testmap","float","float5");
	  break;
	case 16: 
	  ret = map_rem_item("testmap","float","float6");
	  break;
	case 17: 
	  ret = map_rem_item("testmap","float","float7");
	  break;
	case 20: 
	  ret = map_rem_item("testmap","char","char0");
	  break;
	case 21: 
	  ret = map_rem_item("testmap","char","char1");
	  break;
	case 22: 
	  ret = map_rem_item("testmap","char","char2");
	  break;
	case 23: 
	  ret = map_rem_item("testmap","char","char3");
	  break;
	case 24: 
	  ret = map_rem_item("testmap","char","char4");
	  break;
	case 25: 
	  ret = map_rem_item("testmap","char","char5");
	  break;
	case 26: 
	  ret = map_rem_item("testmap","char","char6");
	  break;
	case 27: 
	  ret = map_rem_item("testmap","char","char7");
	  break;
	default:
	  ret = MAP_OK;
	  break;
	}
	if ( ret != MAP_OK && ret != MAP_USER_WARN_NOMATCH ) return (1);
	break;

      case 4:
	/* rem_sub */

	switch (rand() & 0xf) {
	case 0: 
	  ret = map_rem_sub("testmap","int");
	  break;
	case 1: 
	  ret = map_rem_sub("testmap","float");
	  break;
	case 2: 
	  ret = map_rem_sub("testmap","char");
	  break;
	default:
	  ret = MAP_OK;
	  break;
	}
	if ( ret != MAP_OK && ret != MAP_USER_WARN_NOMATCH ) return (1);
	break;

      case 5:
	/* put_int */
	firsttime = (rand() & 0x7) + 10;
	change = rand() & 0x7;
	for ( i=0; i<18; i++ ) array[i] = 10000* change + 100*firsttime + i;
	switch (rand() & 0x7) {
	case 0: 
	  ret = map_put_int("testmap","int","int0",10,array,-firsttime);
	  break;
	case 1: 
	  ret = map_put_int("testmap","int","int1",11,array,-firsttime);
	  break;
	case 2: 
	  ret = map_put_int("testmap","int","int2",12,array,-firsttime);
	  break;
	case 3: 
	  ret = map_put_int("testmap","int","int3",13,array,-firsttime);
	  break;
	case 4: 
	  ret = map_put_int("testmap","int","int4",14,array,-firsttime);
	  break;
	case 5: 
	  ret = map_put_int("testmap","int","int5",15,array,-firsttime);
	  break;
	case 6: 
	  ret = map_put_int("testmap","int","int6",16,array,-firsttime);
	  break;
	case 7: 
	  ret = map_put_int("testmap","int","int7",17,array,-firsttime);
	  break;
	}
	if ( ret != MAP_OK && ret != MAP_USER_WARN_NOMATCH ) return (1);
	break;

      case 6:
	/* put_float */
	firsttime = (rand() & 0x7) + 10;
	change = rand() & 0x7;
	for ( i=0; i<18; i++ ) farray[i] = 10000* change + 100*firsttime + i;
	switch (rand() & 0x7) {
	case 0: 
	  ret = map_put_float("testmap","float","float0",10,farray,-firsttime);
	  break;
	case 1: 
	  ret = map_put_float("testmap","float","float1",11,farray,-firsttime);
	  break;
	case 2: 
	  ret = map_put_float("testmap","float","float2",12,farray,-firsttime);
	  break;
	case 3: 
	  ret = map_put_float("testmap","float","float3",13,farray,-firsttime);
	  break;
	case 4: 
	  ret = map_put_float("testmap","float","float4",14,farray,-firsttime);
	  break;
	case 5: 
	  ret = map_put_float("testmap","float","float5",15,farray,-firsttime);
	  break;
	case 6: 
	  ret = map_put_float("testmap","float","float6",16,farray,-firsttime);
	  break;
	case 7: 
	  ret = map_put_float("testmap","float","float7",17,farray,-firsttime);
	  break;
	}
	if ( ret != MAP_OK && ret != MAP_USER_WARN_NOMATCH ) return (1);
	break;

      case 7:
	/* put_char */
	firsttime = (rand() & 0x7) + 10;

	switch (rand() & 0x7) {
	case 0: 
	  ret = map_put_char("testmap","char","char0",10,&carray[10],-firsttime);
	  break;
	case 1: 
	  ret = map_put_char("testmap","char","char1",11,&carray[11],-firsttime);
	  break;
	case 2: 
	  ret = map_put_char("testmap","char","char2",12,&carray[12],-firsttime);
	  break;
	case 3: 
	  ret = map_put_char("testmap","char","char3",13,&carray[13],-firsttime);
	  break;
	case 4: 
	  ret = map_put_char("testmap","char","char4",14,&carray[14],-firsttime);
	  break;
	case 5: 
	  ret = map_put_char("testmap","char","char5",15,&carray[15],-firsttime);
	  break;
	case 6: 
	  ret = map_put_char("testmap","char","char6",16,&carray[16],-firsttime);
	  break;
	case 7: 
	  ret = map_put_char("testmap","char","char7",17,&carray[17],-firsttime);
	  break;
	}
	if ( ret != MAP_OK && ret != MAP_USER_WARN_NOMATCH ) return (1);
	break;

      }
    }
  return 0;
}
