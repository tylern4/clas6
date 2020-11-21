/* 
 *  calib_disconnect.c: disconnect from the database
 *  Created           : 25-MAY-2000 by Riad Suleiman
 *
 */

#include <stdio.h>
#include "mysql.h"
#include "calib_manager.h"

MYSQL mysql;

int calib_disconnect() {
  int status;  
  // Uncommenting mysql_close to fix database staying open Nov/20/2020 @tylern
  mysql_close(&mysql);
  status = 1;
  return(status);
}
