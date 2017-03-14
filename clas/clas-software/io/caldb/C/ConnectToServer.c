/********************************************************/
/* H.Avakian
/* Date  : 2001-03-01
/********************************************************/
/* ConnectToServer     !!!  This will not work with old versions of MySQL !!! 
*   input:
*     hostname
*     user
*     password
*     database
*     verbose 
*  output:
*      mysql pointer through external variable in calib_connect.h
*     return integer keeping the status of connection
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <mysql.h>
#include "calib_connect.h"
MYSQL *ConnectToServer(char *host,char *dbname,char *user,char *pass) {
	MYSQL *conn;
        static iconnect=0;
        iconnect++;
	if(caldb_verbose){   /*  print details */
		printf("ConnectToServer: Connecting to dbase: \n");
		printf("      user:      %s \n",user);
		printf("      host:      %s \n",host);
		printf("      dbase:     %s \n",dbname);
		/*		printf("      pass:     %s \n",pass);*/
		if(iconnect>1) printf("       multiple connections:     %d \n",iconnect);
                    	}
        
	conn= mysql_init(NULL);
	if(caldb_verbose) printf ("ConnectToServerinit: pointer %d %d\n",conn,*conn);
	if(conn==NULL)
	{
		fprintf(stderr,"mysql_init() failed\n");
		return (NULL);
	}
	if(mysql_real_connect(conn,host,user,pass,dbname,0,NULL,0) == NULL)
	{
		fprintf (stderr,"mysql_real_connect() failed: \n MySQL Error %u  %s\n",
		    mysql_errno(conn),mysql_error(conn));
		return (NULL);
	}
	else 
	{
		if(caldb_verbose)
		{
			printf("ConnectToServer: mysql_ping:            %i\n", mysql_ping(conn));
			printf("mysql_get_client_info: %s\n", mysql_get_client_info());
			printf("mysql_get_host_info:   %s\n", mysql_get_host_info(conn));
			printf("mysql_get_server_info: %s\n", mysql_get_server_info(conn));
		}
		return (conn);
	}

}

