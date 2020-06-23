/* 
        test_map2.c
         Created           : 15-NOV-1993 by Thom Sulanke
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include <string.h>
#include <map_manager.h>
void oops(int status, int code);
 

/***********************************************************************/
/*                                                                     */
/*   test_map2                                                        */
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
    int array[10];        /* array of int values */
    float farray[10];    /* array of float values */
    char carray[] = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuv";
    int time, firsttime;
    int length, olength;
    int type;
    int status;
    
    /* add only subsystem */

    if ( (status = map_add_item( "TheMap2", "float-s", "float-i", 10, 1 )) != MAP_OK ) oops(status,1);

    /* add subsystem to front of list */

    if ( (status = map_add_item( "TheMap2", "char-s", "char-i", 100, 2 )) != MAP_OK ) oops(status,2);

    /* add subsystem to end of list */

    if ( (status = map_add_item( "TheMap2", "int-s", "int-i", 10, 0 )) != MAP_OK ) oops(status,3);

    /* add subsystem to interior of list */

    if ( (status = map_add_item( "TheMap2", "dtmp-s", "tmp-i", 10, 2 )) != MAP_OK ) oops(status,4);
    if ( (status = map_add_item( "TheMap2", "gtmp-s", "tmp-i", 10, 2 )) != MAP_OK ) oops(status,5);


    /* add item to front of list */

    if ( (status = map_add_item( "TheMap2", "dtmp-s", "a", 10, 2 )) != MAP_OK ) oops(status,6);

    /* add item to end of list */

    if ( (status = map_add_item( "TheMap2", "dtmp-s", "z", 10, 2 )) != MAP_OK ) oops(status,7);

    /* add item to interior of list */

    if ( (status = map_add_item( "TheMap2", "dtmp-s", "m", 10, 2 )) != MAP_OK ) oops(status,8);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "v", 10, 2 )) != MAP_OK ) oops(status,9);


    /* add only int values */
 
    time = 500;
    for ( i=0 ; i<10 ; i++ )
      {
        array[i] = time + i;
      }  /* For */

    if ( (status = map_put_int( "TheMap2", "int-s", "int-i", 10, array, time )) != MAP_OK ) oops(status,10);
    
    /* add int values at front of list */
 
    time = 100;
    for ( i=0 ; i<10 ; i++ )
      {
        array[i] = time + i;
      }  /* For */

    if ( (status = map_put_int( "TheMap2", "int-s", "int-i", 10, array, time )) != MAP_OK ) oops(status,11);
    
    /* add int values to end of list */
 
    time = 900;
    for ( i=0 ; i<10 ; i++ )
      {
        array[i] = time + i;
      }  /* For */

    if ( (status = map_put_int( "TheMap2", "int-s", "int-i", 10, array, time )) != MAP_OK ) oops(status,12);
    
    /* add int values to interior of list */
 
    time = 300;
    for ( i=0 ; i<10 ; i++ )
      {
        array[i] = time + i;
      }  /* For */

    if ( (status = map_put_int( "TheMap2", "int-s", "int-i", 10, array, time )) != MAP_OK ) oops(status,13);

    time = 700;
    for ( i=0 ; i<10 ; i++ )
      {
        array[i] = time + i;
      }  /* For */

    if ( (status = map_put_int( "TheMap2", "int-s", "int-i", 10, array, time )) != MAP_OK ) oops(status,14);
    

    /* add only float values */
 
    time = 500;
    for ( i=0 ; i<10 ; i++ )
      {
        farray[i] = time + i;
      }  /* For */

    if ( (status = map_put_float( "TheMap2", "float-s", "float-i", 10, farray, time )) != MAP_OK ) oops(status,15);
    
    /* add float values at front of list */
 
    time = 100;
    for ( i=0 ; i<10 ; i++ )
      {
        farray[i] = time + i;
      }  /* For */

    if ( (status = map_put_float( "TheMap2", "float-s", "float-i", 10, farray, time )) != MAP_OK ) oops(status,16);
    
    /* add float values to end of list */
 
    time = 900;
    for ( i=0 ; i<10 ; i++ )
      {
        farray[i] = time + i;
      }  /* For */

    if ( (status = map_put_float( "TheMap2", "float-s", "float-i", 10, farray, time )) != MAP_OK ) oops(status,17);
    
    /* add float values to interior of list */
 
    time = 300;
    for ( i=0 ; i<10 ; i++ )
      {
        farray[i] = time + i;
      }  /* For */

    if ( (status = map_put_float( "TheMap2", "float-s", "float-i", 10, farray, time )) != MAP_OK ) oops(status,18);
    
    time = 700;
    for ( i=0 ; i<10 ; i++ )
      {
        farray[i] = time + i;
      }  /* For */

    if ( (status = map_put_float( "TheMap2", "float-s", "float-i", 10, farray, time )) != MAP_OK ) oops(status,19);
    


    /* add only char values */
 
    time = 500;
    for ( i=0 ; i<10 ; i++ )
      {
        farray[i] = time + i;
      }  /* For */

    if ( (status = map_put_char( "TheMap2", "char-s", "char-i", time/10, carray+time/100, time )) != MAP_OK ) oops(status,20);
    
    /* add char values at front of list */
 
    time = 100;
    for ( i=0 ; i<10 ; i++ )
      {
        farray[i] = time + i;
      }  /* For */

    if ( (status = map_put_char( "TheMap2", "char-s", "char-i", time/10, carray+time/100, time )) != MAP_OK ) oops(status,21);
    
    /* add char values to end of list */
 
    time = 900;
    for ( i=0 ; i<10 ; i++ )
      {
        farray[i] = time + i;
      }  /* For */

    if ( (status = map_put_char( "TheMap2", "char-s", "char-i", time/10, carray+time/100, time )) != MAP_OK ) oops(status,22);
    
    /* add char values to interior of list */
 
    time = 300;
    for ( i=0 ; i<10 ; i++ )
      {
        farray[i] = time + i;
      }  /* For */

    if ( (status = map_put_char( "TheMap2", "char-s", "char-i", time/10, carray+time/100, time )) != MAP_OK ) oops(status,23);

    time = 700;
    for ( i=0 ; i<10 ; i++ )
      {
        farray[i] = time + i;
      }  /* For */

    if ( (status = map_put_char( "TheMap2", "char-s", "char-i", time/10, carray+time/100, time )) != MAP_OK ) oops(status,24);

    /* get a dump before going on */

    printf("\nNEW DUMP\n");
    map_dump( "TheMap2" );

    /* replace int values at front of list */

    time = 100;
    for ( i=0 ; i<10 ; i++ )
      {
        array[i] = time*10 + i;
      }  /* For */

    if ( (status = map_put_int( "TheMap2", "int-s", "int-i", 10, array, -time )) != MAP_OK ) oops(status,30);
    
    /* replace int values in interior of list */

    time = 500;
    for ( i=0 ; i<10 ; i++ )
      {
        array[i] = time*10 + i;
      }  /* For */

    if ( (status = map_put_int( "TheMap2", "int-s", "int-i", 10, array, -time )) != MAP_OK ) oops(status,31);
    
    /* replace int values at end of list */

    time = 900;
    for ( i=0 ; i<10 ; i++ )
      {
        array[i] = time*10 + i;
      }  /* For */

    if ( (status = map_put_int( "TheMap2", "int-s", "int-i", 10, array, -time )) != MAP_OK ) oops(status,32);
    
    /* replace float values at front of list */

    time = 100;
    for ( i=0 ; i<10 ; i++ )
      {
        farray[i] = time*10 + i;
      }  /* For */

    if ( (status = map_put_float( "TheMap2", "float-s", "float-i", 10, farray, -time )) != MAP_OK ) oops(status,40);
    
    /* replace float values in interior of list */

    time = 500;
    for ( i=0 ; i<10 ; i++ )
      {
        farray[i] = time*10 + i;
      }  /* For */

    if ( (status = map_put_float( "TheMap2", "float-s", "float-i", 10, farray, -time )) != MAP_OK ) oops(status,41);
    
    /* replace float values at end of list */

    time = 900;
    for ( i=0 ; i<10 ; i++ )
      {
        farray[i] = time*10 + i;
      }  /* For */

    if ( (status = map_put_float( "TheMap2", "float-s", "float-i", 10, farray, -time )) != MAP_OK ) oops(status,42);
    
    /* replace char values at front of list */

    time = 100;

    if ( (status = map_put_char( "TheMap2", "char-s", "char-i", time/10+5, carray+time/100+1, -time )) != MAP_OK ) oops(status,50);
    
    /* replace char values in interior of list */

    time = 500;

    if ( (status = map_put_char( "TheMap2", "char-s", "char-i", time/10-5, carray+time/100+1, -time )) != MAP_OK ) oops(status,51);
    
    /* replace char values at end of list */

    time = 900;

    if ( (status = map_put_char( "TheMap2", "char-s", "char-i", time/10+5, carray+time/100+1, -time )) != MAP_OK ) oops(status,52);
    

    /* get a dump before going on */

    printf("\nNEW DUMP\n");
    map_dump( "TheMap2" );

    /* find int values at front of list */

    time = 100;
    for ( i=0 ; i<10 ; i++ )
      {
        array[i] = time*10 + i;
      }  /* For */

    if ( (status = map_fnd_int( "TheMap2", "int-s", "int-i", 10, array, 0, &firsttime )) != MAP_OK ) oops(status,60);
    if ( time != firsttime ) oops(0,60);
    
    /* find int values in interior of list */

    time = 500;
    for ( i=0 ; i<10 ; i++ )
      {
        array[i] = time*10 + i;
      }  /* For */

    if ( (status = map_fnd_int( "TheMap2", "int-s", "int-i", 10, array, 0, &firsttime )) != MAP_OK ) oops(status,61);
    if ( time != firsttime ) oops(0,61);
    
    /* find int values at end of list */

    time = 900;
    for ( i=0 ; i<10 ; i++ )
      {
        array[i] = time*10 + i;
      }  /* For */

    if ( (status = map_fnd_int( "TheMap2", "int-s", "int-i", 10, array, 0, &firsttime )) != MAP_OK ) oops(status,62);
    if ( time != firsttime ) oops(0,62);
    
    /* find non-existant int values */

    time = 333;
    for ( i=0 ; i<10 ; i++ )
      {
        array[i] = time*10 + i;
      }  /* For */

    if ( (status = map_fnd_int( "TheMap2", "int-s", "int-i", 10, array, 0, &firsttime )) != MAP_OK ) oops(status,63);
    if ( firsttime != -1 ) oops(0,63);
    

    /* find float values at front of list */

    time = 100;
    for ( i=0 ; i<10 ; i++ )
      {
        farray[i] = time*10 + i;
      }  /* For */

    if ( (status = map_fnd_float( "TheMap2", "float-s", "float-i", 10, farray, 0, &firsttime )) != MAP_OK ) oops(status,70);
    if ( time != firsttime ) oops(0,70);
    
    /* find float values in interior of list */

    time = 500;
    for ( i=0 ; i<10 ; i++ )
      {
        farray[i] = time*10 + i;
      }  /* For */

    if ( (status = map_fnd_float( "TheMap2", "float-s", "float-i", 10, farray, 0, &firsttime )) != MAP_OK ) oops(status,71);
    if ( time != firsttime ) oops(0,71);
    
    /* find float values at end of list */

    time = 900;
    for ( i=0 ; i<10 ; i++ )
      {
        farray[i] = time*10 + i;
      }  /* For */

    if ( (status = map_fnd_float( "TheMap2", "float-s", "float-i", 10, farray, 0, &firsttime )) != MAP_OK ) oops(status,72);
    if ( time != firsttime ) oops(0,72);
    
    /* find non-existant int values */

    time = 333;
    for ( i=0 ; i<10 ; i++ )
      {
        farray[i] = time*10 + i;
      }  /* For */

    if ( (status = map_fnd_float( "TheMap2", "float-s", "float-i", 10, farray, 0, &firsttime )) != MAP_OK ) oops(status,73);
    if ( firsttime != -1 ) oops(0,73);
    

    /* remove item at front of list */

    if ( (status = map_rem_item( "TheMap2", "dtmp-s", "a" )) != MAP_OK ) oops(status,80);

    /* get a dump before going on */

    printf("\nNEW DUMP\n");
    map_dump( "TheMap2" );

    /* remove item from interior of list */

    if ( (status = map_rem_item( "TheMap2", "dtmp-s", "tmp-i" )) != MAP_OK ) oops(status,81);

    /* get a dump before going on */

    printf("\nNEW DUMP\n");
    map_dump( "TheMap2" );

    /* remove item from end of list */

    if ( (status = map_rem_item( "TheMap2", "dtmp-s", "z" )) != MAP_OK ) oops(status,82);

    /* get a dump before going on */

    printf("\nNEW DUMP\n");
    map_dump( "TheMap2" );


    /* remove remaining items from list */

    if ( (status = map_rem_item( "TheMap2", "dtmp-s", "m" )) != MAP_OK ) oops(status,83);
    if ( (status = map_rem_item( "TheMap2", "dtmp-s", "v" )) != MAP_OK ) oops(status,84);

    /* get a dump before going on */

    printf("\nNEW DUMP\n");
    map_dump( "TheMap2" );


    /* remove subsystem at front of list */

    if ( (status = map_rem_sub( "TheMap2", "char-s" )) != MAP_OK ) oops(status,90);

    /* get a dump before going on */

    printf("\nNEW DUMP\n");
    map_dump( "TheMap2" );

    /* remove subsystem from interior of list */

    if ( (status = map_rem_sub( "TheMap2", "gtmp-s" )) != MAP_OK ) oops(status,91);

    /* get a dump before going on */

    printf("\nNEW DUMP\n");
    map_dump( "TheMap2" );

    /* remove subsystem from end of list */

    if ( (status = map_rem_sub( "TheMap2", "int-s" )) != MAP_OK ) oops(status,92);

    /* get a dump before going on */

    printf("\nNEW DUMP\n");
    map_dump( "TheMap2" );



    /* remove array from interior of list */

    if ( (status = map_rem_arr( "TheMap2", "float-s", "float-i", 500 )) != MAP_OK ) oops(status,93);

    /* get a dump before going on */

    printf("\nNEW DUMP\n");
    map_dump( "TheMap2" );


    /* remove array from end of list */

    if ( (status = map_rem_arr( "TheMap2", "float-s", "float-i", 900 )) != MAP_OK ) oops(status,94);

    /* get a dump before going on */

    printf("\nNEW DUMP\n");
    map_dump( "TheMap2" );



    /* remove array from begining of list */

    if ( (status = map_rem_arr( "TheMap2", "float-s", "float-i", 100 )) != MAP_OK ) oops(status,95);

    /* get a dump before going on */

    printf("\nNEW DUMP\n");
    map_dump( "TheMap2" );



    /* add many values */

    for ( time = 5; time<950 ; time = time+10 )
      {
	for ( i=0 ; i<10 ; i++ )
	  {
	    farray[i] = time + i;
	  }  /* For */
	
	if ( (status = map_put_float( "TheMap2", "float-s", "float-i", 10, farray, time )) != MAP_OK ) oops(status,1000+time);
      }
    

    /* get a dump before going on */

    printf("\nNEW DUMP\n");
    map_dump( "TheMap2" );

    /* delete item with many arrays */

    if ( (status = map_rem_item( "TheMap2", "float-s", "float-i" )) != MAP_OK ) oops(status,1999);
    

    /* add many items */

    if ( (status = map_add_item( "TheMap2", "dtmp-s", "a-s", 10, 1 )) != MAP_OK ) oops(status,2101);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "b-s", 10, 1 )) != MAP_OK ) oops(status,2102);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "c-s", 10, 1 )) != MAP_OK ) oops(status,2103);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "d-s", 10, 1 )) != MAP_OK ) oops(status,2104);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "e-s", 10, 1 )) != MAP_OK ) oops(status,2105);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "f-s", 10, 1 )) != MAP_OK ) oops(status,2106);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "g-s", 10, 1 )) != MAP_OK ) oops(status,2107);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "h-s", 10, 1 )) != MAP_OK ) oops(status,2108);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "i-s", 10, 1 )) != MAP_OK ) oops(status,2109);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "j-s", 10, 1 )) != MAP_OK ) oops(status,2110);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "k-s", 10, 1 )) != MAP_OK ) oops(status,2111);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "l-s", 10, 1 )) != MAP_OK ) oops(status,2112);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "m-s", 10, 1 )) != MAP_OK ) oops(status,2113);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "n-s", 10, 1 )) != MAP_OK ) oops(status,2114);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "o-s", 10, 1 )) != MAP_OK ) oops(status,2115);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "p-s", 10, 1 )) != MAP_OK ) oops(status,2116);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "q-s", 10, 1 )) != MAP_OK ) oops(status,2117);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "r-s", 10, 1 )) != MAP_OK ) oops(status,2118);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "s-s", 10, 1 )) != MAP_OK ) oops(status,2119);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "t-s", 10, 1 )) != MAP_OK ) oops(status,2120);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "u-s", 10, 1 )) != MAP_OK ) oops(status,2121);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "v-s", 10, 1 )) != MAP_OK ) oops(status,2122);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "w-s", 10, 1 )) != MAP_OK ) oops(status,2123);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "x-s", 10, 1 )) != MAP_OK ) oops(status,2124);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "y-s", 10, 1 )) != MAP_OK ) oops(status,2125);
    if ( (status = map_add_item( "TheMap2", "dtmp-s", "z-s", 10, 1 )) != MAP_OK ) oops(status,2126);


    /* get a dump before going on */

    printf("\nNEW DUMP\n");
    map_dump( "TheMap2" );

    /* remove subsystem with many items */

    if ( (status = map_rem_sub( "TheMap2", "dtmp-s" )) != MAP_OK ) oops(status,2199);


    /* add many subsystems */

    if ( (status = map_add_item( "TheMap2", "a-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2001);
    if ( (status = map_add_item( "TheMap2", "b-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2002);
    if ( (status = map_add_item( "TheMap2", "c-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2003);
    if ( (status = map_add_item( "TheMap2", "d-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2004);
    if ( (status = map_add_item( "TheMap2", "e-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2005);
    if ( (status = map_add_item( "TheMap2", "f-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2006);
    if ( (status = map_add_item( "TheMap2", "g-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2007);
    if ( (status = map_add_item( "TheMap2", "h-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2008);
    if ( (status = map_add_item( "TheMap2", "i-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2009);
    if ( (status = map_add_item( "TheMap2", "j-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2010);
    if ( (status = map_add_item( "TheMap2", "k-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2011);
    if ( (status = map_add_item( "TheMap2", "l-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2012);
    if ( (status = map_add_item( "TheMap2", "m-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2013);
    if ( (status = map_add_item( "TheMap2", "n-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2014);
    if ( (status = map_add_item( "TheMap2", "o-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2015);
    if ( (status = map_add_item( "TheMap2", "p-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2016);
    if ( (status = map_add_item( "TheMap2", "q-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2017);
    if ( (status = map_add_item( "TheMap2", "r-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2018);
    if ( (status = map_add_item( "TheMap2", "s-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2019);
    if ( (status = map_add_item( "TheMap2", "t-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2020);
    if ( (status = map_add_item( "TheMap2", "u-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2021);
    if ( (status = map_add_item( "TheMap2", "v-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2022);
    if ( (status = map_add_item( "TheMap2", "w-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2023);
    if ( (status = map_add_item( "TheMap2", "x-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2024);
    if ( (status = map_add_item( "TheMap2", "y-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2025);
    if ( (status = map_add_item( "TheMap2", "z-s", "tmp-i", 10, 1 )) != MAP_OK ) oops(status,2026);

    /* get a dump before going on */

    printf("\nNEW DUMP\n");
    map_dump( "TheMap2" );


    return 0;
}

void oops(int status, int code)
{
  printf("unexpected status code: %d, call code: %d\n",status,code);
/*  exit(1); */
  return;
}
