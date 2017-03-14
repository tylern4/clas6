#include <stdio.h>
#include <unistd.h>
#include <pwd.h>
#include <sys/types.h>
#include "csql.h"

int connect_mSQL_server(MYSQL *mysql,char *DB_hostname, char *DB_username,char *DB_name,char *DB_passwd){

#if MYSQL_VERSION_ID >= 32200
    mysql_init(mysql);
#endif

#if MYSQL_VERSION_ID >= 32200
  if (!(mysql_real_connect(mysql,DB_hostname,DB_username,NULL,DB_name,0,NULL,0))) {
#else
  if (!(mysql_real_connect(mysql,DB_hostname,DB_username,NULL,0,NULL,0))) {
#endif
	printf("CONNECT_SQL_SERVER: ERROR Connecting server\n%s\n",mysql_error(mysql));
	/*    disconnect_mSQL_server(mysql); */
	return -1;
  }

  if(mysql_select_db(mysql,DB_name)){
	printf("WRITE_SQL_TABLE: Error Selecting DataBase (%s)\n%s\n", DB_name, mysql_error(mysql));
    disconnect_mSQL_server(mysql);
	return -1;
  }
 return 1;
} 

void disconnect_mSQL_server(MYSQL *mysql){
	mysql_close(mysql);	
	/*	isConnected = SERVER_NOT_CONNECTED; */
}



