#include "csql.h"

void  set_column_int(const char *group, const char *col, int value)
{
  int grp_indx;
  int col_indx;
  grp_indx = find_group_byname(group);
  if(grp_indx<0||grp_indx>=MAX_GROUPS){printf("SET_COLUMN:(ERROR!!!) Failed to find group [%s]\n",group);return;}
  col_indx = find_column_byname(grp_indx,col);
  if(col_indx<0||col_indx>=MAX_COLUMNS){printf("SET_COLUMN:(ERROR!!!) columns [%s] does not exist\n",col);}
  sprintf(CSQL_Groups[grp_indx].fColumns[col_indx].value,"%d",value);
					      
}
void  set_column_float(const char *group, const char *col, float value)
{
  int grp_indx;
  int col_indx;
  grp_indx = find_group_byname(group);
  if(grp_indx<0||grp_indx>=MAX_GROUPS){printf("SET_COLUMN:(ERROR!!!) Failed to find group [%s]\n",group);return;}
  col_indx = find_column_byname(grp_indx,col);
  if(col_indx<0||col_indx>=MAX_COLUMNS){printf("SET_COLUMN:(ERROR!!!) columns [%s] does not exist\n",col);}
  sprintf(CSQL_Groups[grp_indx].fColumns[col_indx].value,"%f",value);

}
void  set_column_char(const char *group, const char *col, const char *value)
{
  int grp_indx;
  int col_indx;
  grp_indx = find_group_byname(group);
  if(grp_indx<0||grp_indx>=MAX_GROUPS){printf("SET_COLUMN:(ERROR!!!) Failed to find group [%s]\n",group);return;}
  col_indx = find_column_byname(grp_indx,col);
  if(col_indx<0||col_indx>=MAX_COLUMNS){printf("SET_COLUMN:(ERROR!!!) columns [%s] does not exist\n",col);}
  sprintf(CSQL_Groups[grp_indx].fColumns[col_indx].value,"%s",value);
}
