/* caldb_getItemValueId 
*   input:
*     tablename
*     itemvalueid
*     author
*     time
*  output:
*     itemvalueid  char pointer to itemvalueId
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <mysql.h>
#include "calib_connect.h"
int caldb_getItemValueId(MYSQL *conn, char *tablename, itemvalue *itemvalueid ,commentstruc *runcomm) {
	MYSQL_ROW rows_in_query;
	MYSQL_RES *mysql_res;
	char query[SQL_CMD_SPACE];
	char fieldList[]="itemValueId,author,time,minRunSource,maxRunSource,comment";
	char condList[SQL_CMD_SPACE];
	int len;
	commentstruc  tmpruncomm;

	if(tablename)
	{             /*  all defined */
		if(*itemvalueid)
		{
			sprintf(condList, "itemValueId=%d",*itemvalueid);
		}
		else
		{
		  /*sprintf(condList, "author=\'%s\'.author ORDER BY time  DESC LIMIT 1",tablename); */
			return CALIB_USER_ERROR_ARGUMENT;  
			/*for now no other choice*/
		}

		sprintf(query,"SELECT %s FROM %s WHERE %s",fieldList,
		    tablename,condList);
		len = strlen(query);
		if(caldb_verbose)
			printf("getitemValueId query: query  %s len=%d \n", query,len);
		if (mysql_real_query(conn, query, len) !=0)
		{
			fprintf (stderr,
			    "caldb_getItemValueId:mysql_real_query() failed: \n MySQL Error %u  %s\n",
			    mysql_errno(conn),mysql_error(conn));
			return mysql_errno(conn);
		}
		else
		{
			mysql_res = mysql_store_result(conn);
			rows_in_query = mysql_fetch_row(mysql_res);
			if (!rows_in_query)
			{
				fprintf (stderr,"caldb_getItemValueId: No data in mysql_real_query()\n" );
				return CALIB_USER_WARN_NOMATCH;
			}
			else
			{
				tmpruncomm.minRunSource=atoi((rows_in_query[3]== NULL ) ? "": rows_in_query[3]);
				tmpruncomm.maxRunSource=atoi((rows_in_query[4]== NULL ) ? "": rows_in_query[4]);
				tmpruncomm.author=(rows_in_query[1]== NULL)  ? "NULL": rows_in_query[1];
				tmpruncomm.time=(rows_in_query[2]== NULL)  ? "NULL": rows_in_query[2];
				tmpruncomm.comment=(rows_in_query[5]== NULL)  ? "NULL": rows_in_query[5];
				*runcomm=tmpruncomm;
			}
		}
		if(caldb_verbose) printf("getitemValueId: itemvalueid = %d for author/time %s/%s \n",*itemvalueid,rows_in_query[1],
		    rows_in_query[2]);
                    mysql_free_result(mysql_res);
		return CALIB_OK;
	}
	else 
	{
		fprintf (stderr,"caldb_getItemValueId: Error: components not defined" );
		return CALIB_USER_WARN_NOMATCH;
	}
}

