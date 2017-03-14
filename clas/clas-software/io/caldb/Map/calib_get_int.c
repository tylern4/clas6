/* 
 *  calib_get_int.c: get array of int values into the calib database
 *  Created        : 25-MAY-2000 by Riad Suleiman
*/
 
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include "mysql.h"
#include "calib_manager.h"
#include "calib_manager_tcl.h"

MYSQL mysql;

int calib_get_int( const char filename[], const char subsystemname[], 
		const char itemname[], int length, int array[], int atime , 
		int *vtime )
{
  MYSQL_ROW systemId, subsystemId, itemId, itemValue, row;
  MYSQL_RES *mysql_res_system, *mysql_res_subsystem, *mysql_res_item,
    *mysql_res_itemValue, *mysql_res_values;
  char *itemValueId;

  char sql_cmd[10000];
  char table_name[100];
  int status, len, nrows;

  int run_requested;

  char intstr[100], value[10000];
  int check, k;

  run_requested = atime;

  calib_connect(READ_DB);
  if(CALDB_VERBOSE)printf("calib_get_int PING = %i\n", mysql_ping(&mysql));

  strcpy(table_name,filename);
  strcat(table_name,"_");
  strcat(table_name,subsystemname);
  strcat(table_name,"_");
  strcat(table_name,itemname);
  if(CALDB_VERBOSE)printf("table_name: %s\n", table_name);

  status = 0;

  /* get systemId ... */
  sprintf(sql_cmd,
	  "SELECT systemId FROM System WHERE System.systemName=\'%s\'",
	  filename);
  len = strlen(sql_cmd);
  
  if (mysql_real_query(&mysql, sql_cmd, len)) {
    status=1;
    printf("calib_get_int SQL_ERR: %s\n", mysql_error(&mysql));
    printf("calib_get_int SQL_ERR: query error %s\n", sql_cmd);
  }

  mysql_res_system = mysql_store_result(&mysql);
  systemId = mysql_fetch_row(mysql_res_system);
  if (!systemId) return CALIB_USER_WARN_NOMATCH;

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
  if (!subsystemId) return CALIB_USER_WARN_NOMATCH;
  if(CALDB_VERBOSE)printf("subsystemId = %s \n", subsystemId[0]);

  /* get itemId ... */
  sprintf(sql_cmd,
	  "SELECT itemId FROM Item WHERE Item.subsystemId=\'%s\' AND Item.itemName=\'%s\'",
	  subsystemId[0], itemname);
  len = strlen(sql_cmd);
  
  if (mysql_real_query(&mysql, sql_cmd, len)) {
    status=1;
    printf("calib_get_int SQL_ERR: %s\n", mysql_error(&mysql));
    printf("calib_get_int SQL_ERR: query error %s\n", sql_cmd);
  }
  
  mysql_res_item = mysql_store_result(&mysql);
  itemId = mysql_fetch_row(mysql_res_item);
  if (!itemId) return CALIB_USER_WARN_NOMATCH;
  if(CALDB_VERBOSE)printf("itemId = %s \n", itemId[0]);     

  /* select info from RunIndex table */
  sprintf(sql_cmd,
	  "SELECT itemValueId, minRun, maxRun, officer, time, comment FROM %s WHERE %s.itemId=%s AND %s.minRun<=%i AND %s.maxRun>=%i %s",
	  RunIndex_table, RunIndex_table, itemId[0], RunIndex_table, run_requested, 
	  RunIndex_table, run_requested, latest);
  len = strlen(sql_cmd);
  if (mysql_real_query(&mysql, sql_cmd, len)) {
    status=1;
    printf("calib_get_int SQL_ERR: %s\n", mysql_error(&mysql));
    printf("calib_get_int SQL_ERR: query error %s\n", sql_cmd);
  }

  /* get itemValueId ... */
  mysql_res_itemValue = mysql_store_result(&mysql);
  row = mysql_fetch_row(mysql_res_itemValue);
  if (!row) return CALIB_USER_WARN_NOFIND;
  itemValueId = row[0];
  *vtime = atoi(row[1]);
  if(CALDB_VERBOSE)printf("itemValueId = %s \n", itemValueId);     
  
  /* select values from value table */
  sprintf(value, "v_%04d", 1);
  
  for (k = 2; k <= length; k++) {
    sprintf(intstr, "v_%04d", k);
    strcat(value, ", ");
    strcat(value, intstr);
  }

  sprintf(sql_cmd,"SELECT itemValueId, %s FROM %s WHERE %s.itemValueId=%s",
	  value, table_name, table_name, itemValueId);
  len = strlen(sql_cmd);

  if (mysql_real_query(&mysql, sql_cmd, len)) {
    status=1;
    printf("calib_get_int SQL_ERR: %s\n", mysql_error(&mysql));
    printf("calib_get_int SQL_ERR: query error %s\n", sql_cmd);
  }

  mysql_res_values = mysql_store_result(&mysql);
  itemValue = mysql_fetch_row(mysql_res_values);
  if (!itemValue) return CALIB_USER_WARN_NOVALUE; 

  /* extract the values from mysql result */
  for (k=0; k<length; k++) {
    if(CALDB_VERBOSE)printf("v_%04d = %s \n", k+1, itemValue[k+1]); 
  }

  for (k=0; k<length; k++)
  		array[k]=atoi(itemValue[k+1]);

  mysql_free_result(mysql_res_system);
  mysql_free_result(mysql_res_subsystem);
  mysql_free_result(mysql_res_item);
  mysql_free_result(mysql_res_itemValue);
  mysql_free_result(mysql_res_values);
 
  /* disconnect the database */
  calib_disconnect();
  
  return(status);
}
