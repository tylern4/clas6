/* 
 *  calib_test.c: prototype main for testing out MySQL C API
 *  Created        : 25-MAY-2000 by Riad Suleiman
 *
 */

#include <stdio.h>
#include "mysql.h"
#include "calib_manager.h"
#include "calib_manager_tcl.h"

char *malloc(int);
MYSQL mysql; 

char *RunIndex_table;

main() {  
  int rstat, k, l;
  int arraylength, arraytype; 
  int array[6];
  int iarray[6912];
  float farray[10];
  char carray[10000]; 
  int vtime=100000;

  RunIndex_table = (char *) malloc(32);
  strcpy(RunIndex_table, "calib_user.test0");

  printf("Testing map_stat_item\n");

  rstat= map_stat_item("DC_SWAP", "swap_info","bits", &arraylength, &arraytype);

  printf("arraylength = %i \n", arraylength);
  printf("arraytype = %i \n", arraytype);

  for (k = 0 ; k < 6; k++) {
    array[k]=k;
  }

  printf("Testing map_put_int\n");
  rstat= map_put_int("DC_SWAP", "swap_info","bits",6,array,100000);

  printf("Testing map_get_int\n");
  rstat= map_get_int("DC_SWAP", "swap_info","bits",6,array,100000,&vtime);

  for (k=0; k<6; k++) {
    printf("array_%i = %i \n", k+1, array[k]); 
  }
  
  for (k = 0 ; k < 10; k++) {
    farray[k]=k+1.5;
  }

  printf("Testing map_put_float\n");
  rstat= map_put_float("CALL_CALIB", "T0","unc",10,farray,100000);

  printf("Testing map_get_float\n");
  rstat= map_get_float("CALL_CALIB", "T0","unc",10,farray,100000,&vtime);

  for (k=0; k<10; k++) {
    printf("array_%i = %f \n", k+1, farray[k]); 
  }
  
  sprintf(carray,"ttttttttttttt");


  printf("Testing map_put_char\n");
  rstat= map_put_char("DC_DOCA", "t_max","comment",1024,carray,100000);

  printf("Testing map_get_char\n");
  rstat= map_get_char("DC_DOCA", "t_max","comment",1024,carray,100000,&vtime,&l);

  printf("array_%i = %s \n", 1, carray);
  printf("Length = %i \n", l);
 
 

  for (k = 0 ; k < 6912; k++) {
    iarray[k]=1;
  }
  
  printf("Testing map_put_int with blob\n");
  rstat= map_put_int("DC_STATUS" ,"sector6", "status",6912,iarray,100000);
  
  printf("Testing map_get_int with blob\n");
  rstat= map_get_int("DC_STATUS" ,"sector6", "status",6912,iarray,100000,&vtime);
  
  for (k=0; k<6912; k=k+500) {
    printf("iarray_%i = %i \n", k+1, iarray[k]); 
  }
  k=6912-1;
  printf("iarray_%i = %i \n", k+1, iarray[k]); 
}
