/********************************************************/
/* H.Avagyan
/* Date  : 2001-03-01
/********************************************************/
/* WriteandLinkConstantSet .c 
*   input:
*     char *systemname
*     char *subsystemname    system definition
*     char *itemname
*
*     int   minrunsource  min run for source    -> filled in valuId tables         
*     int   maxrunsource  max run for source           
*     char *calib_comment 
*
*     char *RunIndexTable  Name of the RunIndex table
*     int   minrun        min run for source    -> validity range(RunIndex)
*     int   maxrun        max run for source
*     char *runindex_comment
*           
*     int caldb_verbose   (external)
*     char *author defined from $USER env. variable 
*  output:
*   itemvalue *itemvalueid ID# in the ValueID table (also Runindex)
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

int WriteAndLinkConstantSet(MYSQL *conn, char *systemname,char *subsystemname,char *itemname, int minrunsource,
int maxrunsource, char *calib_comment,char *value, itemvalue *itemvalueid,
char *RunIndexTable, int minrun,int maxrun, char *runindex_comment,
itemvalue *runindexid){
	MYSQL_ROW rows_in_query;
	MYSQL_RES *mysql_res;
	char query[SQL_CMD_SPACE];
	char tablename[SQL_FIELD_SPACE];
	char *itemid;
	char *itemtype;
	int   itemlength;
	int len;
	int istatus=0;
	char *author=strdup(cal_getenv("USER","",REQUIRED));

	MAKE_VALUE_TABLE_NAME(tablename,systemname,subsystemname,itemname);

	if((istatus=caldb_getItemId(conn,systemname,subsystemname,itemname
	    ,&itemlength,&itemtype,&itemid))!=0) return istatus;
	sprintf(query,"INSERT INTO  %s  VALUES (NULL, '%s', NULL, %d, %d, '%s', %s)",
	    tablename, author, minrunsource,maxrunsource,calib_comment,value);
	len = strlen(query);

	if(caldb_verbose) printf("WriteAndLinkCons: sending query  %s  \n",query);
	/*free(author);*/

	if (mysql_real_query(conn, query, len) !=0)
	{
		fprintf (stderr,"Writeconst:mysql_real_query() failed: \n MySQL Error %u  %s\n",
		    mysql_errno(conn),mysql_error(conn));
		return mysql_errno(conn);
	}
	else
	{
		/* record written, get its ID in the value table*/
		*itemvalueid = mysql_insert_id(conn);
		if(caldb_verbose) printf("WriteAndLinkConstants: itemvalueid# %d, query OK\n",*itemvalueid);
	}

	/* Values successfuly recorded in valuId table now linking to RunIndex   */
	if(caldb_verbose) printf("WriteAndLinkConstants: itemvalueid= -%d- \n",*itemvalueid);

	sprintf(query,"INSERT INTO  %s  VALUES (NULL, %d, %d, '%s', %d, '%s', NULL, '%s')",
	    RunIndexTable, minrun,maxrun,itemid,*itemvalueid,author,runindex_comment);
	len = strlen(query);

	if(caldb_verbose) printf("LinkCons:  RunIndex %s,sending query  %s  \n", RunIndexTable,query);

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
		if(caldb_verbose) printf("WriteAndLinkConstants:Table:%s runindexid# %d, query OK\n",RunIndexTable,*runindexid);
	}

	return CALIB_OK;
}




