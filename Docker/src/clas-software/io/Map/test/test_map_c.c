/* 
        test_map_c.c
         Created           :  1-OCT-1992 by Thom Sulanke
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include <string.h>
#include <map_manager.h>
void oops(int status, int code);
 

/***********************************************************************/
/*                                                                     */
/*   test_map_c                                                        */
/*   ----------                                                        */
/*                                                                     */
/*         Created     :  1-OCT-1992    Author : Thom Sulanke          */
/*         Purpose     : test the map routines when called from c      */
/*                                                                     */
/***********************************************************************/
 
main( )

{
    char subsystemname[MAP_NAMELENGTH], itemname[MAP_NAMELENGTH];
    char nextsubsystemname[MAP_NAMELENGTH], nextitemname[MAP_NAMELENGTH];
    int i;                /* index */
    int array[100];        /* array of int values */
    float farray[100];    /* array of float values */
    char carray[] = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuv";
    int atime, vtime;
    int length, olength;
    int type;
    int status;
    
    if ( (status = map_add_item( "TheMap", "LGD", "High Voltage", 43*71, 0 )) != MAP_OK ) oops(status,1);
    if ( (status = map_add_item( "TheMap", "abcdefghijklmno", "1234", 10, 0 )) != MAP_OK ) oops(status,2);
    if ( (status = map_add_item( "TheMap", "subsystem   long", "long", 10, 0 )) != MAP_USER_ERROR_ARGUMENT ) oops(status,3);
    if ( (status = map_add_item( "TheMap", "7171717171", "item    too long", 10, 0 )) != MAP_USER_ERROR_ARGUMENT ) oops(status,4);
    if ( (status = map_add_item( "TheMap", "zero arr size", "repeat", 0, 0 )) != MAP_USER_ERROR_ARGUMENT ) oops(status,5);
    if ( (status = map_add_item( "TheMap", "large arr size", "repeat", 10001, 0 )) != MAP_OK ) oops(status,6);
    if ( (status = map_add_item( "TheMap", "ABC", "repeat", 10, 0 )) != MAP_OK ) oops(status,7);
    if ( (status = map_add_item( "TheMap", "0", "short", 75, 0 )) != MAP_OK ) oops(status,8);
    if ( (status = map_add_item( "TheMap", "ABC", "repeat", 10, 0 )) != MAP_USER_WARN_NOREPEAT ) oops(status,9);
    if ( (status = map_add_item( "TheMap", "test", "5 first", 5, 0 )) != MAP_OK ) oops(status,10);
    if ( (status = map_add_item( "TheMap", "test", "1 front", 1, 0 )) != MAP_OK ) oops(status,11);
    if ( (status = map_add_item( "TheMap", "test", "9 end", 9, 0 )) != MAP_OK ) oops(status,12);
    if ( (status = map_add_item( "TheMap", "test", "5 middle", 5, 0 )) != MAP_OK ) oops(status,13);
 
/* add some values */
 
    if ( (status = map_get_int( "TheMap", "ABC", "repeat", 10, array, 9, &vtime )) != MAP_USER_WARN_NOFIND ) oops(status,14);
    printf("time %d, array[1] %d, vtime %d\n",9,array[1],vtime);    

    if ( (status = map_add_item( "TheMap", "ABC", "repeat", 10, 0 )) != MAP_USER_WARN_NOREPEAT ) oops(status,15);
    for ( i=0 ; i<10 ; i++ )
    {
        array[i] = 3*i;
    }  /* For */

    if ( (status = map_put_int( "TheMap", "ABC", "repeat", 10, array, 0 )) != MAP_OK ) oops(status,16);

    sleep(1);
    for ( i=0 ; i<10 ; i++ )
    {
        array[i] = 4*i;
    }  /* For */

    if ( (status = map_put_int( "TheMap", "ABC", "repeat", 10, array, 0 )) != MAP_OK ) oops(status,17);
    
    for ( i=0 ; i<10 ; i++ )
    {
        array[i] = 1*i;
    }  /* For */

    if ( (status = map_put_int( "TheMap", "ABC", "repeat", 10, array, 5 )) != MAP_OK ) oops(status,18);
    
    for ( i=0 ; i<10 ; i++ )
    {
        array[i] = 2*i;
    }  /* For */

    if ( (status = map_put_int( "TheMap", "ABC", "repeat", 10, array, 10 )) != MAP_OK ) oops(status,19);
 
    if ( (status = map_add_item( "TheMap", "ABC", "floating", 10, 1 )) != MAP_OK ) oops(status,20);
    for ( i=0 ; i<10 ; i++ )
    {
        farray[i] = 3.0*i;
    }  /* For */
    if ( (status = map_put_float( "TheMap", "ABC", "floating", 10, farray, 10 )) != MAP_OK ) oops(status,21);
    farray[0] = 1.0;
    if ( (status = map_put_float( "TheMap", "ABC", "floating", 10, farray, -10 )) != MAP_OK ) oops(status,22);
    farray[0] = 2.0;
    if ( (status = map_put_float( "TheMap", "ABC", "floating", 10, farray, 10 )) != MAP_USER_WARN_NOREPLACE ) oops(status,23);
    if ( (status = map_add_item( "TheMap", "ABC", "finding", 2, 0 )) != MAP_OK ) oops(status,24);
    array[0] = 1; array[1] = 1; 
    if ( (status = map_put_int( "TheMap", "ABC", "finding", 2, array, 10 )) != MAP_OK ) oops(status,25);
    array[0] = 1; array[1] = 2; 
    if ( (status = map_put_int( "TheMap", "ABC", "finding", 2, array, 20 )) != MAP_OK ) oops(status,26);
    array[0] = 2; array[1] = 2; 
    if ( (status = map_put_int( "TheMap", "ABC", "finding", 2, array, 30 )) != MAP_OK ) oops(status,27);
    array[0] = 1; array[1] = 2; 
    if ( (status = map_put_int( "TheMap", "ABC", "finding", 2, array, 40 )) != MAP_OK ) oops(status,28);
    array[0] = 3; array[1] = 3; 
    if ( (status = map_put_int( "TheMap", "ABC", "finding", 2, array, 50 )) != MAP_OK ) oops(status,29);

    if ( (status = map_add_item( "TheMap", "ABC", "strings", 100, 2 )) != MAP_OK ) oops(status,30);

    if ( (status = map_put_char( "TheMap", "ABC", "strings", 10, carray, 10 )) != MAP_OK ) oops(status,31);
    if ( (status = map_put_char( "TheMap", "ABC", "strings", 20, carray, 20 )) != MAP_OK ) oops(status,32);
    if ( (status = map_put_char( "TheMap", "ABC", "strings", 100, carray, 100 )) != MAP_OK ) oops(status,33);
    if ( (status = map_put_char( "TheMap", "ABC", "strings", 200, carray, 200 )) != MAP_USER_ERROR_ARGUMENT ) oops(status,34);

    /* use stat_item */

    if ( (status = map_stat_item( "TheMap", "x", "repeat", &length, &type )) != MAP_USER_WARN_NOMATCH ) oops(status,341);
    printf("array length = %d, array type = %d\n", length, type);
    if ( (status = map_stat_item( "TheMap", "ABC", "repeat", &length, &type )) != MAP_OK ) oops(status,342);
    printf("array length = %d, array type = %d\n", length, type);

/* replace some values */

    array[0] = 3; array[1] = 4;
    if ( (status = map_put_int( "TheMap", "ABC", "finding", 2, array, 50 )) != MAP_USER_WARN_NOREPLACE ) oops(status,35);
    array[0] = 3; array[1] = 5;
    if ( (status = map_put_int( "TheMap", "ABC", "finding", 2, array, -50 )) != MAP_OK ) oops(status,36);
    
    if ( (status = map_put_char( "TheMap", "ABC", "strings", 15, carray, 20 )) != MAP_USER_WARN_NOREPLACE ) oops(status,37);
    if ( (status = map_put_char( "TheMap", "ABC", "strings", 15, carray, -20 )) != MAP_OK ) oops(status,375);

/* check bug for time = 1 */

    array[0] = 1111; array[1] = 111;
    if ( (status = map_put_int( "TheMap", "ABC", "finding", 2, array, 1 )) != MAP_OK ) oops(status,38);

/* retrieve some values */

    if ( (status = map_get_int( "TheMap", "ABC", "repeat", 10, array, 0, &vtime )) != MAP_OK ) oops(status,39);
    printf("time %d, array[1] %d, vtime %d\n",0,array[1],vtime);    
    if ( (status = map_get_int( "TheMap", "ABC", "repeat", 10, array, 11, &vtime )) != MAP_OK ) oops(status,40);
    printf("time %d, array[1] %d, vtime %d\n",11,array[1],vtime);    
    if ( (status = map_get_int( "TheMap", "ABC", "repeat", 10, array, 10, &vtime )) != MAP_OK ) oops(status,41);
    printf("time %d, array[1] %d, vtime %d\n",10,array[1],vtime);    
    if ( (status = map_get_int( "TheMap", "ABC", "repeat", 10, array, 9, &vtime )) != MAP_OK ) oops(status,42);
    printf("time %d, array[1] %d, vtime %d\n",9,array[1],vtime);    

    if ( (status = map_stat_item("TheMap","ABC","floating",&length,&type)) != MAP_OK) oops(status,425);

    if ( (status = map_get_float( "TheMap", "ABC", "floating", 10, farray, 0, &vtime )) != MAP_OK ) oops(status,43);
    printf("time %d, farray[1] %f, vtime %d\n",0,farray[1],vtime);    

    array[0] = 1; array[1] = 2; 
    if ( (status = map_fnd_int("TheMap", "ABC", "finding", 2, array, 0, &vtime )) != MAP_OK ) oops(status,44);
    printf("time %d, vtime %d\n",0,vtime);    
    if ( (status = map_fnd_int("TheMap", "ABC", "finding", 2, array, 1, &vtime )) != MAP_OK ) oops(status,45);
    printf("time %d, vtime %d\n",1,vtime);    
    if ( (status = map_fnd_int("TheMap", "ABC", "finding", 2, array, 19, &vtime )) != MAP_OK ) oops(status,46);
    printf("time %d, vtime %d\n",19,vtime);    
    if ( (status = map_fnd_int("TheMap", "ABC", "finding", 2, array, 20, &vtime )) != MAP_OK ) oops(status,47);
    printf("time %d, vtime %d\n",20,vtime);    
    if ( (status = map_fnd_int("TheMap", "ABC", "finding", 2, array, 39, &vtime )) != MAP_OK ) oops(status,48);
    printf("time %d, vtime %d\n",39,vtime);    
    if ( (status = map_fnd_int("TheMap", "ABC", "finding", 2, array, 40, &vtime )) != MAP_OK ) oops(status,49);
    printf("time %d, vtime %d\n",40,vtime);   

    memset(carray,'?',100);
    if ( (status = map_get_char( "TheMap", "ABC", "strings", 10, carray, 20, &vtime, &olength )) != MAP_OK ) oops(status,50);
    printf("time %d, carray %s, olength %d, vtime %d\n",10,carray,olength,vtime);
    memset(carray,'?',100);
    if ( (status = map_get_char( "TheMap", "ABC", "strings", 100, carray, 20, &vtime, &olength )) != MAP_OK ) oops(status,51);
    printf("time %d, carray %s, olength %d, vtime %d\n",10,carray,olength,vtime);    

/* dump map */

    map_dump("TheMap");

/* remove arrays */

    if ( (status = map_rem_arr("TheMap", "ABC", "finding", 50)) != MAP_OK ) oops(status,52);
    if ( (status = map_rem_arr("TheMap", "ABC", "finding", 0)) != MAP_USER_WARN_NOFIND ) oops(status,53);
    if ( (status = map_rem_arr("TheMap", "ABC", "finding", -50)) != MAP_USER_WARN_NOFIND ) oops(status,54);
    if ( (status = map_rem_arr("TheMap", "ABC", "finding", 49)) != MAP_USER_WARN_NOFIND ) oops(status,55);
    if ( (status = map_rem_arr("TheMap", "ABC", "floating", 10)) != MAP_OK ) oops(status,56);
    if ( (status = map_rem_arr("TheMap", "ABC", "strings", 10)) != MAP_OK ) oops(status,57);

/* dump map */

    map_dump("TheMap");

/* remove some items */

    if ( 0 )
      if ( (status = map_rem_item("TheMap","nosub","noitem")) != MAP_USER_WARN_NOMATCH ) oops(status,60);
    if ( 0 )
      if ( (status = map_rem_item("TheMap","LGD","noitem")) != MAP_USER_WARN_NOMATCH ) oops(status,61);
    if ( 0 )
      if ( (status = map_rem_item("TheMap","0","short")) != MAP_OK ) oops(status,62);
    if ( 0 )
      if ( (status = map_rem_item("TheMap","test","1 front")) != MAP_OK ) oops(status,63);
    if ( 0 )
      if ( (status = map_rem_item("TheMap","test","9 end")) != MAP_OK ) oops(status,64);
    if ( 0 )
      if ( (status = map_rem_item("TheMap","test","5 middle")) != MAP_OK ) oops(status,65);
    if ( 0 )
      if ( (status = map_rem_item("TheMap","ABC","a")) != MAP_USER_WARN_NOMATCH ) oops(status,66);
    if ( 0 )
      if ( (status = map_rem_item("TheMap","ABC","m")) != MAP_USER_WARN_NOMATCH ) oops(status,67);
    if ( 0 )
      if ( (status = map_rem_item("TheMap","ABC","z")) != MAP_USER_WARN_NOMATCH ) oops(status,68);
    if ( 0 )
      {
	if ( (status = map_rem_item("TheMap","0","short")) != MAP_OK ) oops(status,69);
	if ( (status = map_rem_item("TheMap","0","short")) != MAP_USER_WARN_NOMATCH ) oops(status,70);
      }
    if ( 0 )
      if ( (status = map_rem_sub("TheMap","nosub")) != MAP_USER_WARN_NOMATCH ) oops(status,71);
    if ( 0 )
      if ( (status = map_rem_sub("TheMap","0")) != MAP_OK ) oops(status,72);
    if ( 0 )
      if ( (status = map_rem_sub("TheMap","LGD")) != MAP_OK ) oops(status,73);
    if ( 0 )
      if ( (status = map_rem_sub("TheMap","test")) != MAP_OK ) oops(status,74);

    return 0;
}

void oops(int status, int code)
{
  printf("unexpected status code: %d, call code: %d\n",status,code);
/*  exit(1); */
  return;
}
