#include <stdlib.h>
#include "csql.h"


void create_table(const char *tablename, const char *group_list)
{
  
  MYSQL       mysql;
  MYSQL_ROW   mysql_row;
  MYSQL_RES  *mysql_res;

  int         shift;
  char        line_group[33];
  char        sql_query[10000];
  char        bank[5];
  int         grp_indx;
  int         col_indx;
  
  strcpy(sql_query,"CREATE TABLE ");
  strcat(sql_query,tablename);
  string_append(sql_query," ( id INT NOT NULL auto_increment " );

  strncpy(line_group,group_list,32); line_group[33]='\0';
  printf("CREATE TABLE: Group List = [%s]\n",line_group);
  while(strlen(line_group)>=4){
    strncpy(bank,line_group,4); bank[4] = '\0';
    grp_indx = find_group_byname(bank);
    if(grp_indx>=0&&grp_indx<MAX_GROUPS){
      printf("CREATE TABLE: Retrieving group [%s]\n",bank);
      for(col_indx=0;col_indx<CSQL_Groups[grp_indx].ncolumns;col_indx++){
	if(CSQL_Groups[grp_indx].fColumns[col_indx].mode!=0){
	  string_append(sql_query,",");string_append(sql_query,CSQL_Groups[grp_indx].fColumns[col_indx].name);
	  string_append(sql_query," ");string_append(sql_query,CSQL_Groups[grp_indx].fColumns[col_indx].type);
	}
      }
    } else {
      printf("CREATE TABLE: Group [%s] can not be found. Skipping..\n",bank);
    }
    
    for(shift=0;shift<4;shift++) string_shift(line_group,0,32);
  }
  string_append(sql_query,",PRIMARY KEY(id))");
  
  printf("CREATE TABLE: Query = %s\n",sql_query);
  execute_query(sql_query);
}


int   is_table(const char *tablename)
{
  int         nrows;
  MYSQL_RES  *mysql_res;
  char chk_query[64];

  sprintf(chk_query,"show tables like \'%s\'",tablename);
  mysql_res = execute_query(chk_query);
  if(mysql_res != NULL){
    nrows     = mysql_num_rows(mysql_res);  
    /*  printf("isTABLE: %d\n",nrows);*/
    if(nrows>0) return 1;
  }
  
  return -1;
}
