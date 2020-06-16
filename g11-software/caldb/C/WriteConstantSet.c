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
*     int caldb_verbose   (external)
*     char *author defined from $USER env. variable 
*  output:
*   itemvalue *itemvalueid ID# in the ValueID table (to ref. Runindex)
*
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <mysql.h>
#include "calib_connect.h"

int WriteConstantSet(MYSQL *conn, char *systemname,char *subsystemname,char *itemname, int minrunsource,int maxrunsource,
char *calib_comment,char *value, itemvalue *itemvalueid){
	MYSQL_ROW rows_in_query;
	MYSQL_RES *mysql_res;
	char query[SQL_CMD_SPACE];
	char tablename[SQL_FIELD_SPACE];
	int len;
	char *author=strdup(cal_getenv("USER","",REQUIRED));

	MAKE_VALUE_TABLE_NAME(tablename,systemname,subsystemname,itemname);
	sprintf(query,"INSERT INTO  %s  VALUES (NULL, '%s', NULL, %d, %d, '%s', %s)",
	    tablename, author, minrunsource,maxrunsource,calib_comment,value);
	len = strlen(query);
	/*free(author);*/
	if(caldb_verbose) printf("WriteCons: sending query:  %s  \n",query);

	if (mysql_real_query(conn, query, len) !=0)
	{
		fprintf (stderr,"Writeconst:mysql_real_query() failed: \n MySQL Error %u  %s\n",
		    mysql_errno(conn),mysql_error(conn));
		return mysql_errno(conn);
	}
	else
	{
		/* record written, get it's ID in the value table*/
		*itemvalueid = mysql_insert_id(conn);
		if(caldb_verbose) printf("WriteConstants: itemvalueid# %d, query OK\n",*itemvalueid);
	}

	return CALIB_OK;
}




