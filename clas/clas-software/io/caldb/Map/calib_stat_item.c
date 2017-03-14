/* 
 *  calib_stat_item.c: get rray length and data type of an item 
 *  Created        : 25-MAY-2000 by Riad Suleiman
*/
 
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include "mysql.h"
#include "calib_manager.h"

MYSQL mysql;

int calib_stat_item(const char filename[],  const char subsystemname[],
		    const char itemname[], int *arraylength, int *arraytype)
{
  MYSQL_ROW systemId, subsystemId, itemId;
  MYSQL_RES *mysql_res_system, *mysql_res_subsystem, *mysql_res_item;

  char sql_cmd[10000];
  int status, len, nrows;

  char intstr[100];
  int check, k;
  char typei[]="int";
  char typef[]="float";

  calib_connect(READ_DB);
  if(CALDB_VERBOSE)printf("calib_get_int PING = %i\n", mysql_ping(&mysql));

  status = 0;

  /* get systemId ... */

  sprintf(sql_cmd,"SELECT systemId FROM System WHERE System.systemName=\'%s\'", filename);
  len = strlen(sql_cmd);
  
  if (mysql_real_query(&mysql, sql_cmd, len)) {
    status=1;
    printf("calib_get_int SQL_ERR: %s\n", mysql_error(&mysql));
    printf("calib_get_int SQL_ERR: query error %s\n", sql_cmd);
  }

  mysql_res_system = mysql_store_result(&mysql);
  systemId = mysql_fetch_row(mysql_res_system);

  if(CALDB_VERBOSE)printf("systemId = %s \n", systemId[0]); 
  /* get subsystemId ... */

  sprintf(sql_cmd,
	  "SELECT subsystemId FROM Subsystem WHERE Subsystem.systemId=\'%s\' AND Subsystem.subsystemName=\'%s\'",
	  systemId[0], subsystemname);
  len = strlen(sql_cmd);
  
  if (mysql_real_query(&mysql, sql_cmd, len)) {
    status=1;
    printf("calib_get_int SQL_ERR: %s\n", mysql_error(&mysql));
    printf("calib_get_int SQL_ERR: query error %s\n", sql_cmd);
  }

  mysql_res_subsystem = mysql_store_result(&mysql);
  subsystemId = mysql_fetch_row(mysql_res_subsystem);
  if(CALDB_VERBOSE)printf("subsystemId = %s \n", subsystemId[0]); 

  /* get itemId ... */
  sprintf(sql_cmd,
	  "SELECT itemId, length, type FROM Item WHERE Item.subsystemId=\'%s\' AND Item.itemName=\'%s\'",
	  subsystemId[0], itemname);
  len = strlen(sql_cmd);
  
  if (mysql_real_query(&mysql, sql_cmd, len)) {
    status=1;
    printf("calib_get_int SQL_ERR: %s\n", mysql_error(&mysql));
    printf("calib_get_int SQL_ERR: query error %s\n", sql_cmd);
  }
  
  mysql_res_item = mysql_store_result(&mysql);
  itemId = mysql_fetch_row(mysql_res_item);
  if(CALDB_VERBOSE)printf("itemId = %s \n", itemId[0]); 
  if(CALDB_VERBOSE)printf(" arraylength = %s \n", itemId[1]);
  if(CALDB_VERBOSE)printf(" arraytype = %s \n", itemId[2]); 
  
  /* extract the length and type from mysql result */

  sscanf(itemId[1], "%i", arraylength);

  if (strcmp(itemId[2],typei)==0) {
    *arraytype =0;
  } else if (strcmp(itemId[2],typef)==0) {
    *arraytype =1;
  } else {
    *arraytype =2;    
  } 
  
  mysql_free_result(mysql_res_system);
  mysql_free_result(mysql_res_subsystem);
  mysql_free_result(mysql_res_item);

  /* disconnect the database */
  calib_disconnect();

  return(status);
}
