#include "csql.h"
#include <unistd.h>
#include <pwd.h>
#include <sys/types.h>
#include <mysql.h>

MYSQL_RES  *execute_query(const char *query_str)
{
  MYSQL_RES *mysql_result;

  if(connect_mSQL_server(&mysql,CSQL_Hostname,CSQL_Username,CSQL_Database,NULL) < 0){
    printf("EXECUTE_QUERY: Failed to connect to Server..\n");
    return;
  }
  if(mysql_real_query(&mysql, query_str, strlen(query_str))){
    printf("EXECUTE_QUERY: ERROR Could not execute query %s\n\nERROR: %s\n\n",query_str,mysql_error(&mysql));
    disconnect_mSQL_server(&mysql); 
  } else {
    printf("EXECUTE_QUERY: query executed successfuly..\n");
  }
  mysql_result = mysql_store_result(&mysql);
  
  disconnect_mSQL_server(&mysql);
  return mysql_result;

  /*   */
}
