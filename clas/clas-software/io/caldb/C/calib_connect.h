/* 
 *        calib_connect.h: user header for use with MySQL C API connection routines
 *        Created:   H.Avakian
*/
#include "calib_envir.h"
/* Function prototypes */

void calib_init(int argc, char **argv, char **host,char **dbname,char **user,char **pass,char **runindex);
char *cal_getenv(char *envvar,char *vardef,int notnull);
int caldb_getItemValueId(MYSQL *conn, char *tablename, itemvalue *itemvalueid ,commentstruc *runcomm);
int caldb_getItemId(MYSQL *mysql, char *systemName,char *subsystemName,char *itemName, int *length,char **type, char **itemId);



MYSQL *ConnectToServer(char *host,char *dbname,char *user,char *pass);


int ReadConstants(MYSQL *mysql, char *systemname,char *subsystemname,char *itemname,char *RunIndexTable, int runno, char *date, itemvalue *itemvaluid, commentstruc *runcomm,valuestruc *tlvalue);

int WriteConstantSet(MYSQL *conn, char *systemname,char *subsystemname,char *itemname, int minRun,int maxRun,char *calib_comment,char *value, itemvalue *itemvaluid);

int LinkConstantSet(MYSQL *conn, char *systemname,char *subsystemname,char *itemname,char *RunIndexTable, int minRun, int maxRun,char *runindex_comment,itemvalue itemvalueid, commentstruc runcomm,itemvalue *runindexid);


int WriteAndLinkConstantSet(MYSQL *conn, char *systemname,char *subsystemname,char *itemname, int minrunsource,
                             int maxrunsource, char *calib_comment,char *value, itemvalue *itemvalueid,
                             char *RunIndexTable, int minrun,int maxrun, char *runindex_comment,
			     itemvalue *runindexid);

void DisconnectFromServer(MYSQL *mysql);

