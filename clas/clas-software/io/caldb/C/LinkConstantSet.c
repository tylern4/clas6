/********************************************************/
/* H.Avakian
/* Date  : 2001-03-01
/********************************************************/
/*  LinkConstants.c
*   input:
*     char *systemname
*     char *subsystemname    system definition
*     char *itemname
*     itemvalue itemvalueid   ID# of entry to be linked in valueId table
*     char *RunIndexTable  Name of the RunIndex table
*     int   minrun        min run for source    -> validity range(RunIndex)
*     int   maxrun        max run for source
*     char *runindex_comment
*           
*     int caldb_verbose   (external)
*     char *author defined from $USER env. variable 
*  output:
*   itemvalue *runindexid  ID# in RunIndex for *itemvalueid
*
*  internal calls
*   caldb_getItemId(conn,systemname,subsystemname,itemname
*                         ,&itemlength,&itemtype,&itemid)
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <mysql.h>
#include "calib_connect.h"
int LinkConstantSet(MYSQL *conn, char *systemname,char *subsystemname,char *itemname,char *RunIndexTable, int minrun, int
maxrun,char *runindex_comment,itemvalue itemvalueid, commentstruc runcomm,itemvalue *runindexid) {
	MYSQL_ROW rows_in_query;
	MYSQL_RES *mysql_res;
	char query[SQL_CMD_SPACE];
	char *itemid;
	char *itemtype;
	int   itemlength;
	valuestruc tlvalue;
	char tablename[SQL_FIELD_SPACE];
	char fieldList[SQL_FIELD_SPACE];
	char tableList[SQL_FIELD_SPACE];
	char condList[SQL_CMD_SPACE];
	int len;
	int istatus=0;
	char *author=strdup(cal_getenv("USER","clasuser",REQUIRED));

	if((istatus=caldb_getItemId(conn,systemname,subsystemname,itemname
	    ,&itemlength,&itemtype,&itemid))!=0) return istatus;
	MAKE_VALUE_TABLE_NAME(tablename,systemname,subsystemname,itemname);

	if((istatus=caldb_getItemValueId(conn,tablename,&itemvalueid,&runcomm))!=0) return istatus;

	if(caldb_verbose) printf("LinkConstants: itemvalueid= -%d- author \n",
	    itemvalueid,runcomm.author);

	sprintf(query,"INSERT INTO  %s  VALUES (NULL, %d, %d, '%s', %d, '%s', NULL, '%s')",
	    RunIndexTable, minrun,maxrun,itemid,itemvalueid,author,runindex_comment);
	len = strlen(query);
	if(caldb_verbose) printf("LinkCons:  RunIndex=%s,sending query:  %s  \n", RunIndexTable,query);
	/*free(author);*/

	if (mysql_real_query(conn, query, len) !=0)
	{
		fprintf (stderr,"Linkconst:mysql_real_query() failed: \n MySQL Error %u  %s\n",
		    mysql_errno(conn),mysql_error(conn));
		return mysql_errno(conn);
	}
	else
	{
		/* record written, get it's ID in the value table*/
		*runindexid = mysql_insert_id(conn);
		if(caldb_verbose) printf("LinkConstants: runindexid# %d, query OK\n",*runindexid);
	}
	return CALIB_OK;


}




