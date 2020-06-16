#include "csql.h"

void  add_column(const char *group, const char *colname)
{
  int grp_indx;
  grp_indx = find_group_byname(group);
  if(grp_indx<0||grp_indx>=MAX_GROUPS){printf("ADD_COLUMN:(ERROR!!!) Failed to find group [%s]\n",group);return;}
  if(find_column_byname(grp_indx,colname)>=0){printf("ADD_COLUMN:(ERROR!!!) columns [%s] alredy exists at position \n",
						     colname,find_column_byname(grp_indx,colname));return;}

  if(CSQL_Groups[grp_indx].ncolumns>=MAX_COLUMNS){
    printf("ADD_COLUMN: Group [%s] is full. can not create a column.[%s]\n",group,colname);return;}

  strcpy(CSQL_Groups[grp_indx].fColumns[CSQL_Groups[grp_indx].ncolumns].name,colname);
  strcpy(CSQL_Groups[grp_indx].fColumns[CSQL_Groups[grp_indx].ncolumns].type,"INT");
  CSQL_Groups[grp_indx].fColumns[CSQL_Groups[grp_indx].ncolumns].mode = COL_CW_TBL;
  if(fDebugMode>3) printf("ADD_COLUMN: adding column [%s] to group [%s]\n",CSQL_Groups[grp_indx].fColumns[CSQL_Groups[grp_indx].ncolumns].name,group);
  CSQL_Groups[grp_indx].ncolumns++;
}

int  find_column_byname(int grp_indx, const char *colname)
{
  int indx;
  for(indx=0;indx<MAX_COLUMNS;indx++) if(strcmp(CSQL_Groups[grp_indx].fColumns[indx].name,colname)==0) return indx;
    return -1;
}

int  find_column(group_t group, const char *name)
     /* Returns 1 if column is found and -1 otherwise */
{
  int indx;
  for(indx=0;indx<MAX_COLUMNS;indx++) if(strcmp(group.fColumns[indx].name,name)==0)  return 1;
    return -1;
}

void  init_column(const char *group, const char *colname, const char *type, int itype, int mode)
{
  int grp_indx;
  int col_indx;
  grp_indx = find_group_byname(group);
  if(grp_indx<0||grp_indx>=MAX_GROUPS){printf("INIT_COLUMN:(ERROR!!!) Failed to find group [%s]\n",group);return;}
  col_indx = find_column_byname(grp_indx,colname);
  if(col_indx<0||col_indx>=MAX_COLUMNS){printf("INIT_COLUMN:(ERROR!!!) columns [%s] does not exist\n",colname);}
  strcpy(CSQL_Groups[grp_indx].fColumns[col_indx].type,type);
  CSQL_Groups[grp_indx].fColumns[col_indx].itype = itype;
  CSQL_Groups[grp_indx].fColumns[col_indx].mode  = mode;
}

void  disable_column(const char *group, const char *colname)
{
  int grp_indx;
  int col_indx;
  grp_indx = find_group_byname(group);
  if(grp_indx<0||grp_indx>=MAX_GROUPS){printf("INIT_COLUMN:(ERROR!!!) Failed to find group [%s]\n",group);return;}
  col_indx = find_column_byname(grp_indx,colname);
  if(col_indx<0||col_indx>=MAX_COLUMNS){printf("INIT_COLUMN:(ERROR!!!) columns [%s] does not exist\n",colname);}
  /*  strcpy(CSQL_Groups[grp_indx].fColumns[col_indx].type,type);*/
  /*  CSQL_Groups[grp_indx].fColumns[col_indx].itype = itype; */
  CSQL_Groups[grp_indx].fColumns[col_indx].mode  = 0;
}

void  enable_column(const char *group, const char *colname)
{
  int grp_indx;
  int col_indx;
  grp_indx = find_group_byname(group);
  if(grp_indx<0||grp_indx>=MAX_GROUPS){printf("INIT_COLUMN:(ERROR!!!) Failed to find group [%s]\n",group);return;}
  col_indx = find_column_byname(grp_indx,colname);
  if(col_indx<0||col_indx>=MAX_COLUMNS){printf("INIT_COLUMN:(ERROR!!!) columns [%s] does not exist\n",colname);}
  /*  strcpy(CSQL_Groups[grp_indx].fColumns[col_indx].type,type);*/
  /*  CSQL_Groups[grp_indx].fColumns[col_indx].itype = itype; */
  CSQL_Groups[grp_indx].fColumns[col_indx].mode  = COL_CW_TBL;
}

