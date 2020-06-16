/* caldb_getItemId 
*   input:
*     systemname
*     subsystemname
*     itemname
*     verbose 
*  output:
*     itemid  char pointer to itemId
*     return integer keeping the status of connection
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <mysql.h>
#include "calib_connect.h"
int caldb_getItemId(MYSQL *conn, char *systemname,char *subsystemname,char *itemname,
int *length,char **type, char **itemid) {
	MYSQL_ROW rows_in_query;
	MYSQL_RES *mysql_res;
	char query[SQL_CMD_SPACE];
	char fieldList[]="itemId,length,type";
	char tableList[]="System,Subsystem,Item";
	char condList[SQL_CMD_SPACE];
	int len;

	if(systemname && subsystemname && itemname  )
	{                                                           /*  all defined */
		sprintf(condList, "Subsystem.systemId= System.systemId  AND "
		    "Item.subsystemId  = Subsystem.subsystemId AND "
		    "systemName = \'%s\'  AND "
		    "subsystemName = \'%s\'  AND "
		    "itemName = \'%s\'",systemname, subsystemname, itemname);
		sprintf(query,"SELECT %s FROM %s WHERE %s",fieldList,tableList,condList);
		len = strlen(query);
		if(caldb_verbose) printf("mysql pointer %d \n",conn);

		if(caldb_verbose) printf("getitemId query: query  %s len=%d \n", query,len);
		if(conn==NULL) printf("baaaaadddddddddd!!!\n");
		if (mysql_real_query(conn, query, len) !=0)
		{
			printf("getitemId query: 1 \n");
			fprintf (stderr,"caldb_getItemId:mysql_real_query() failed: \n MySQL Error %u  %s\n",
			    mysql_errno(conn),mysql_error(conn));
			return mysql_errno(conn);
		}
		else
		{
			if(conn==NULL) printf("baaaaadddddddddd!!!\n");
			mysql_res = mysql_store_result(conn);
			rows_in_query = mysql_fetch_row(mysql_res);
			if (!rows_in_query)
			{
				fprintf (stderr,"caldb_getItemId: No data in mysql_real_query()\n" );
				return CALIB_USER_WARN_NOMATCH;
			}
			else
			{
				*itemid=strdup(rows_in_query[0]);
				*length=atoi(rows_in_query[1]);
				*type=strdup(rows_in_query[2]);
			}
		}
		if(caldb_verbose) printf("getitemId: itemid = %s for %s %s %s type-%s-length- %d-\n",  *itemid,systemname,
		    subsystemname,itemname,*type,*length);
                    mysql_free_result(mysql_res);
		return CALIB_OK;
	}
	else 
	{
		fprintf (stderr,"caldb_getItemId: Error: components not defined" );
		return CALIB_USER_WARN_NOMATCH;
	}
}

