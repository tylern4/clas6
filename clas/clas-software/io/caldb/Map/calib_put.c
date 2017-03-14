/* 
 *  calib_put.c: put array of float values into the calib database
 *  Created        : 25-MAY-2000 by Riad Suleiman
 *  $Id: calib_put.c,v 1.5 2002/11/07 20:57:29 marki Exp $
*/

#include <stdio.h>            
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include "mysql.h"
#include "calib_manager.h"
#include "calib_manager_tcl.h"

MYSQL mysql;

int calib_put_char(const char filename[], const char subsystemname[], 
		    const char itemname[], int length, const char carray[], 
		    int atime)
     
{
  int status;
  char *value;

  value = (char *) malloc(strlen(carray) + 3);

  sprintf(value, "\'%s\'", carray);
  if (CALDB_VERBOSE)
    printf("calib_put_char: value = %s\n", value);
  status = calib_put(filename, subsystemname, itemname, value, atime);
  free(value);
  return(status);
}

int calib_put_float(const char filename[], const char subsystemname[], 
		    const char itemname[], int length, const float farray[], 
		    int atime)
     
{
  int k, status;
  char value_one[100];
  char value[LEN_VALUE];

  sprintf(value, "%f", farray[0]);
  for (k = 1; k < length; k++) {
    sprintf(value_one, "%f", farray[k]);
    strcat(value, ", ");
    strcat(value, value_one);
  }
  if (CALDB_VERBOSE)
    printf("calib_put_float: value = %s\n", value);
  status = calib_put(filename, subsystemname, itemname, value, atime);
  return(status);
}

int calib_put_int(const char filename[], const char subsystemname[], 
		    const char itemname[], int length, const int array[], 
		    int atime)
     
{
  int k, status;
  char value_one[100];
  char value[LEN_VALUE];

  sprintf(value, "%i", array[0]);
  for (k = 1; k < length; k++) {
    sprintf(value_one, "%i", array[k]);
    strcat(value, ", ");
    strcat(value, value_one);
  }
  if (CALDB_VERBOSE)
    printf("calib_put_int: value = %s\n", value);
  status = calib_put(filename, subsystemname, itemname, value, atime);
  return(status);
}

int calib_put( const char filename[], const char subsystemname[], 
		   const char itemname[], const char value[], 
		   int atime_in )
     
{
  MYSQL_ROW systemId, subsystemId, columns;
  MYSQL_RES *mysql_res_itemId, *mysql_res_run_max;
  my_ulonglong itemValueId;

  char *itemId;
  char *sql_cmd;
  char table_name[100];
  int status, len, nrows;

  int run_min;
  int run_max;
  int atime;

  calib_connect(WRITE_DB);

  MAKE_VALUE_TABLE_NAME

  atime = abs(atime_in);

  status = 0;
  sql_cmd = (char *) malloc(strlen(value) + SQL_CMD_SPACE);

  /* get itemId ... */

  sprintf(sql_cmd,
	  "SELECT itemId FROM System, Subsystem, Item WHERE Subsystem.systemId = System.systemId AND Item.subsystemId = Subsystem.subsystemId AND systemName = \'%s\' AND subsystemName = \'%s\' AND itemName = \'%s\'",
	  filename, subsystemname, itemname);
  len = strlen(sql_cmd);
  
  if (mysql_real_query(&mysql, sql_cmd, len)) {
    status=1;
    printf("calib_put SQL_ERR: %s\n", mysql_error(&mysql));
    printf("calib_put SQL_ERR: query error %s\n", sql_cmd);
  }

  mysql_res_itemId = mysql_store_result(&mysql);
  columns = mysql_fetch_row(mysql_res_itemId);
  if (!columns) return CALIB_USER_WARN_NOMATCH;
  itemId = columns[0];
  printf("calib_put: itemId = %s \n", itemId);     

  /* insert constants value into VALUE table */
  sprintf(sql_cmd,
	  "INSERT INTO %s VALUES (NULL, '%s', NULL, NULL, NULL, 'Map-style put', %s)",
	  table_name, author, value);
  len = strlen(sql_cmd);

  if (mysql_real_query(&mysql, sql_cmd, len)) {
    status=1;
    printf("calib_put SQL_ERR: %s\n", mysql_error(&mysql));
    printf("calib_put SQL_ERR: query error %s\n", sql_cmd);
  }

  /* get itemValueId ... */
  itemValueId = mysql_insert_id(&mysql);
  
  /* set run boundaries */

  run_min = atime;

  sprintf(sql_cmd,
	  "SELECT maxRun FROM %s WHERE itemId = %s AND minRun <= %i AND maxRun >= %i ORDER BY time DESC LIMIT 1",
	  RunIndex_table, itemId, atime, atime);
  len = strlen(sql_cmd);
  
  if (mysql_real_query(&mysql, sql_cmd, len)) {
    status=1;
    printf("calib_put SQL_ERR: %s\n", mysql_error(&mysql));
    printf("calib_put SQL_ERR: query error %s\n", sql_cmd);
  }

  mysql_res_run_max = mysql_store_result(&mysql);
  columns = mysql_fetch_row(mysql_res_run_max);
  if (!columns) return CALIB_USER_ERROR_NOUPPER;
  run_max = atoi(columns[0]);

  /* insert into RunIndex table */
  sprintf(sql_cmd,
	  "INSERT INTO %s VALUES (NULL, %i, %i, %s, %i, \'%s\', NULL, 'Map-style put')", 
	  RunIndex_table, run_min, run_max, itemId, (int)itemValueId, author);
  len = strlen(sql_cmd);
  if (CALDB_VERBOSE)
    printf("calib_put: sql_cmd = %s\n", sql_cmd);

  if (mysql_real_query(&mysql, sql_cmd, len)) {
    status=1;
    printf("calib_put SQL_ERR: %s\n", mysql_error(&mysql));
    printf("calib_put SQL_ERR: query error %s\n", sql_cmd);
  }
  
  /* clean up */

  mysql_free_result(mysql_res_itemId);
  mysql_free_result(mysql_res_run_max);

  calib_disconnect();
  free(sql_cmd);

  return(status);
}

