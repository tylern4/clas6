#include "csql.h"

void delete_table(const char *tablename)
{
  char sql_query[64];
  sprintf(sql_query,"DROP TABLE %s",tablename);
  execute_query(sql_query);
}
