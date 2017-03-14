/*
 *  calib_connect.c: connect to the database
 *  Created        : 25-MAY-2000 by Riad Suleiman
 *  $Id: calib_connect.c,v 1.10 2005/07/18 16:23:51 marki Exp $
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mysql.h"
#include "calib_manager.h"
#include "calib_manager_tcl.h"

char *RunIndex_table;
char author[LEN_AUTHOR];
char latest[100]="";

MYSQL mysql;
int CALDB_VERBOSE=0;

int calib_mysql_connected = 0;

int calib_connect(int writeflag) {
  static int init = 0;
  static int writemode = 0;
  static char host_name[20], user_name[40]=NAME, user_pass[40],
    db_name[20]=DB_NAME;
  char *host_name_ptr, *user_name_ptr, *password_ptr;
  char *timestamp_ptr;    /* adding a timestamp to query */
  char latest_message[128];
  char database_delimiter = '.';
  char *dotptr;
  char it_temp[256];
  int it_in_calib;

  int status=0;



  if (writeflag && calib_mysql_connected && !writemode) {
    mysql_close(&mysql);
    calib_mysql_connected = 0;
  }

  if (init==0) {
    mysql_init(&mysql);
    calib_tcl_init();
    printf("calib_connect: RunIndex_table = \"%s\"\n", RunIndex_table);
    /* set the server host name */
    host_name_ptr = getenv("CLAS_CALDB_HOST");
    if (host_name_ptr) {
      strcpy (host_name, host_name_ptr);
      printf ("calib_connect: using server=%s (from CLAS_CALDB_HOST environment variable)\n", host_name);
    } else {
      strcpy(host_name, HOST_DEFAULT);
      printf ("calib_connect: using server=%s (default)\n", host_name);
    }

    user_name_ptr = getenv("CLAS_CALDB_USER");
    if (user_name_ptr) {
        strcpy (user_name, user_name_ptr);
        printf ("calib_connect: using user name=%s (from CLAS_CALDB_USER environment variable)\n", user_name);
    }
    /* adding a timestamp to query */
    timestamp_ptr = getenv("CLAS_CALDB_TIME");
    if (timestamp_ptr) {
      strcat(latest, " AND time <= '");
      strcat(latest, timestamp_ptr);
      strcat(latest, "'");
      sprintf(latest_message, "using constants from %s", timestamp_ptr);
    } else {
      sprintf(latest_message, "using latest constants");
    }
    printf("calib_connect: %s\n", latest_message);
    strcat(latest," ORDER BY time DESC, itemValueId DESC LIMIT 1 ");
    init=1;
  }

  if (!calib_mysql_connected) {

    if (writeflag) {
      strcpy(author, getenv("USER")); /* identify the author */
      /* get name of database of the run index table */
      strcpy(it_temp, RunIndex_table);
      dotptr = strchr(it_temp, database_delimiter); /* look for a dot */
      if (dotptr) {  /* table name has database qualifier */
    *dotptr = '\0'; /* truncate the name leaving only the database */
    if (strcmp(it_temp, "calib") == 0) {
      it_in_calib = 1;
    } else {
      it_in_calib = 0;
    }
      } else { /* no database qualifier in the name, will default to calib */
    it_in_calib = 1;
      }
      if (it_in_calib) { /* if in the calib database */
    strcpy(user_name, author); /* connect to database with author's name */
      }
    }

    /* set the mysql password */
    strcpy(user_pass, PASSWORD_DEFAULT);

    printf("calib_connect: connecting as user=%s\n", user_name);

    if (!(mysql_real_connect(&mysql, host_name, user_name, user_pass,
                 db_name, 0, NULL, 0))){
      if (!(mysql_real_connect(&mysql, host_name, user_name, user_pass,
                   db_name, 0, NULL, 0))) {
    printf("calib_connect: connection error. %s\n",
           mysql_error(&mysql));
      } else if (!(mysql_select_db(&mysql,db_name))) {
    printf("calib_connect: db section error. %s\n",
           mysql_error(&mysql));
    status = 1;
    return(status);
      }
    }
    calib_mysql_connected = 1;
    mysql_options(&mysql, MYSQL_OPT_COMPRESS, NULL);
  }else{
    if(mysql_ping(&mysql)!=0){
      /* We were supposed to be connected, but appearantly we are not. */
      /* Some other part of the code may have grabbed the connection. */
      /* So we need to reconnect. */
      calib_mysql_connected=0;
      calib_connect(writeflag);
    }
  }

  /* once we are in write mode, stay in write mode forever */
  if (writeflag) {
    writemode = 1;
  }

  if(CALDB_VERBOSE){
    printf("calib_connect PING = %i\n", mysql_ping(&mysql));
    printf("calib_connect SQL_INFO: %s\n", mysql_get_client_info());
    printf("calib_connect SQL_INFO: %s\n", mysql_get_host_info(&mysql));
    printf("calib_connect SQL_INFO: %s\n", mysql_get_server_info(&mysql));
  }

  return(status);
}

void calib_mysql_disconnect()
{
    mysql_close(&mysql);
    calib_mysql_connected=0;
}
