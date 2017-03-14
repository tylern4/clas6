/* DisconnectFromServer 
*   input:
*      mysql
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <mysql.h>
#include "calib_connect.h"

void DisconnectFromServer(MYSQL *conn) {

	if(caldb_verbose){   /*  print details */
		printf("DisconnectFromServer: Close connection: %d -%d-\n",conn,*conn);
	}

	mysql_close(conn);
}

