#include <unistd.h>
#include "csql.h"


void fill_table(const char *tablename,const char *group_list)
{

  int         shift;
  char        line_group[33];
  char        sql_query[20000];
  char        sql_variables[5000];
  char        sql_values[5000];
  char        bank[5];
  int         grp_indx;
  int         col_indx;
  int         count;

  int icyl;
  group_t  table_info;

  if(is_table(tablename)<0) {
    printf("FILL_TABLE: table %s does not exist. I'll create one for you..\n",tablename);
    create_table(tablename,group_list);
  }

  get_table_columns(&table_info,tablename);
  
  for(icyl=0;icyl<table_info.ncolumns;icyl++){
    if(fDebugMode>3)printf("COL %d : %s\n",icyl,table_info.fColumns[icyl].name);
  }

  strncpy(line_group,group_list,32);
  if(fDebugMode>1)printf("FILL TABLE: Group List = [%s]\n",line_group);

  sql_variables[0] = '\0';
  sql_values[0]    = '\0';
  
  count = 0;
  
  while(strlen(line_group)>=4){
    strncpy(bank,line_group,4); bank[4] = '\0';
    grp_indx = find_group_byname(bank);

    if(grp_indx>=0&&grp_indx<MAX_GROUPS){
      if(fDebugMode>1)printf("FILL TABLE: Retrieving group [%s]\n",bank);
      /* Looping ove all columns from the group */
      for(col_indx=0;col_indx<CSQL_Groups[grp_indx].ncolumns;col_indx++){
	if(find_column(table_info,CSQL_Groups[grp_indx].fColumns[col_indx].name)>0){
	  if(CSQL_Groups[grp_indx].fColumns[col_indx].mode==COL_CW_TBL){
	    if(count>0)string_append(sql_variables,",");
	    string_append(sql_variables,CSQL_Groups[grp_indx].fColumns[col_indx].name);
	    if(count>0)string_append(sql_values,",");
	    string_append(sql_values,"\"");
	    string_append(sql_values,CSQL_Groups[grp_indx].fColumns[col_indx].value);
	    string_append(sql_values,"\"");
	    count++;
	  } else {printf("FILL_TABLE: skipping [%s]\n",CSQL_Groups[grp_indx].fColumns[col_indx].name);}
	}else printf(" NAME: %s\n",CSQL_Groups[grp_indx].fColumns[col_indx].name );
      }

      /* End of loop */

    } else {
      printf("FILL TABLE: Group [%s] can not be found. Skipping..\n",bank);
    }
    for(shift=0;shift<4;shift++) string_shift(line_group,0,32);
  } 

  sprintf(sql_query,"INSERT INTO %s (%s) VALUES(%s)",tablename,sql_variables,sql_values);
  if(fDebugMode>0)printf("VARIABLES: %s\n",sql_variables);
  if(fDebugMode>0)printf("VALUES   : %s\n",sql_values);
  if(fDebugMode>0)printf("QUERY    : %s\n",sql_query);

  execute_query(sql_query);
  
}

void fill_mon_table(char *group_list)
{ 
  fill_table(CSQL_MonTable,group_list);
}

void fill_def_table(char *group_list)
{ 
  fill_table(CSQL_DefTable,group_list);
}


void get_table_columns(group_t *tbl_col,const char *tablename)
{
  MYSQL_RES    *mysql_result;
  MYSQL_ROW     mysql_row;
  char         get_query[64];
  int          nrows;

  tbl_col->ncolumns = 0;
  sprintf(get_query,"SHOW COLUMNS FROM %s",tablename);

  mysql_result = execute_query(get_query);
  if(mysql_result != NULL ){
    while((mysql_row = mysql_fetch_row(mysql_result))){
      strcpy(tbl_col->fColumns[tbl_col->ncolumns].name,mysql_row[0]);
      tbl_col->ncolumns++;
    /*     printf("Stage %s\n",mysql_row[0]); */
    }
  }


}
