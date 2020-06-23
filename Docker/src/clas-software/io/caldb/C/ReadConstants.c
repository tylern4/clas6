/********************************************************/
/* H.Avakian
/* Date  : 2001-03-01
/********************************************************/
/* ReadConstants.c 
*   input:
*     char *systemname
*     char *subsystemname    system definition
*     char *itemname
*     char *RunIndexTable  Name of the RunIndex table
*     int   runno        # of the run for calib.constants
*     char *date         list with input dbase.date <date
*  output:
*     itemvalue *itemvaluid     Id# of the _latest_entry in valueId table
*      commentstruc *runcomm a structure with calib. details from valueID table
*      runcomm.minRun
*      runcomm.maxRun
*      runcomm.author
*      runcomm.time
*      runcomm.comment
*
*      valuestruc *tlvalue  a  structure with calib constants
*      tlvalue.item    (char **item)  a pointer array to constants
*      tlvalue.type    ( char *type)  type of constants (int,float,...)
*      tlvalue.length  ( int lengt)   length of the array of calib.constants
*
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <mysql.h>
#include "calib_connect.h"
int ReadConstants(MYSQL *conn, char *systemname,char *subsystemname,char *itemname,char *RunIndexTable, int runno, char *
date,itemvalue *itemvaluid, commentstruc *runcomm,valuestruc *tlvalue ) {
	MYSQL_ROW rows_in_query;
	MYSQL_RES *mysql_res;
	char query[SQL_CMD_SPACE];
	int itemlength;
	char *itemid;
	char *itemtype;
	char tablename[SQL_FIELD_SPACE];
	char fieldList[SQL_FIELD_SPACE];
	char tableList[SQL_FIELD_SPACE];
	char condList[SQL_CMD_SPACE];
	valuestruc tmptlvalue;
	commentstruc  tmpruncomm;
	int len;
	int istatus=0;

	if((istatus=caldb_getItemId(conn,systemname,subsystemname,itemname,&itemlength,&itemtype,&itemid))!=0) return istatus;
	if(caldb_verbose) printf("Read: itemId=%s  length=%d  type=%s \n",itemid,itemlength,itemtype);
	MAKE_VALUE_TABLE_NAME(tablename,systemname,subsystemname,itemname);
	sprintf(tableList, "%s, %s ",RunIndexTable,tablename);
	sprintf(fieldList, "%s.*",tablename);
	sprintf(condList, "%s.itemId = %s "
	    "AND minRun <= %d AND maxRun >= %d "
	    "AND %s.time <= \'%s\' "
	    "AND  %s.itemValueId=%s.itemValueid"
	    " ORDER BY %s.time  DESC LIMIT 1"
	    ,RunIndexTable,itemid,runno,runno,RunIndexTable,date,RunIndexTable,tablename,RunIndexTable);
	sprintf(query,"SELECT %s FROM %s WHERE %s",fieldList,tableList,condList);
	len = strlen(query);

	if(caldb_verbose) printf("ReadCons: RunIndex %s, sending query  %s  for itemId %d (mysql=%d)\n", RunIndexTable,query,conn);

	if (mysql_real_query(conn, query, len) !=0)
	{
		fprintf (stderr,"ReadConstants:mysql_real_query() failed: \n MySQL Error %u  %s\n",
		    mysql_errno(conn),mysql_error(conn));
		return mysql_errno(conn);
	}
	else
	{
		if(caldb_verbose) printf("ReadConstants: item# %s, query OK\n",itemname);
		mysql_res = mysql_store_result(conn);
		rows_in_query = mysql_fetch_row(mysql_res);
		if (!rows_in_query)
		{
			fprintf (stderr,"ReadConstants: No data in mysql_real_query()\n" );
			return CALIB_USER_WARN_NOMATCH;
		}
		else /* looks OK,  filling output structures */
		{
			if(caldb_verbose) printf("ReadCons: rows_in_query[0] =%s\n",rows_in_query[0]);
			tmptlvalue.length=itemlength;
			tmptlvalue.type=itemtype;
			tmptlvalue.item=&rows_in_query[VALUE_INDEX];
			tmpruncomm.minRunSource=atoi((rows_in_query[3]== NULL ) ? "": rows_in_query[3]);
			tmpruncomm.maxRunSource=atoi((rows_in_query[4]== NULL ) ? "": rows_in_query[4]);
			tmpruncomm.author=(rows_in_query[1]== NULL)  ? "NULL": rows_in_query[1];
			tmpruncomm.time=(rows_in_query[2]== NULL)  ? "NULL": rows_in_query[2];
			tmpruncomm.comment=(rows_in_query[5]== NULL)  ? "NULL": rows_in_query[5];
			*tlvalue=tmptlvalue;
			*runcomm=tmpruncomm;
			*itemvaluid=atol((rows_in_query[0]== NULL ) ? "": rows_in_query[0]);
			if(caldb_verbose) printf("ReadConstants: itemvalueid=%d \n",*itemvaluid);
		}
	}
                    mysql_free_result(mysql_res);
	return CALIB_OK;
}




