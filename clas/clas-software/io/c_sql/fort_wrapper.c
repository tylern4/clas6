#include "csql.h"


void init_csql_()
{
  init_csql();
}

void init_csql_clas_(int *runno_p, int *runext_p, char *comment_p,int clen)
{
  char comment[64];
  int runno, runext;
  int act_len ;
  
  act_len = get_len(comment_p,clen);
  
  strncpy(comment,comment_p,act_len);
  comment[act_len] = '\0';
  runno  = *runno_p;
  runext = *runext_p;
  init_clas(runno,runext,comment);
}

void drop_mon_table_()
{
  delete_table(CSQL_MonTable);
}

void set_group_(char *grpname, int *buffer, int glen){
char  *ch_buff;
char   nextgroup[5];
printf("Setting Group Values\n");
ch_buff = (char *) buffer;
strncpy(nextgroup,grpname,4); nextgroup[4] = '\0';
set_group(nextgroup,ch_buff);
}

void write_mon_table_(char *group_list,int glen)
{ 
  int act_len ;
  char groups[33];
  act_len = get_len(group_list, glen);
  strncpy(groups,group_list,act_len);
  groups[act_len] = '\0';
  fill_table(CSQL_MonTable,groups);
}

void write_def_table_(char *group_list,int glen)
{ 
  int act_len ;
  char groups[33];
  act_len = get_len(group_list, glen);
  strncpy(groups,group_list,act_len);
  groups[act_len] = '\0';
  fill_table(CSQL_DefTable,groups);
}


void read_ddlfile2_(char *filename,int flen)
{
  int act_len;
  char fname[256];
  act_len = get_len(filename, flen);
  strncpy(fname,filename,act_len);
  fname[act_len] = '\0';
  read_ddlfile2(fname);
}
void print_all_groups_()
{
  print_all_groups();
}

void fill_table_(const char *tablename,const char *group_list, int tlen, int glen)
{
  int tact_len, gact_len;
  char tname[64],glist[64];
  tact_len = get_len(tablename,tlen);
  gact_len = get_len(group_list,glen);

  strncpy(tname,tablename,tact_len); tname[tact_len] = '\0';
  strncpy(glist,group_list,gact_len); glist[gact_len] = '\0';
  fill_table(tname,glist);
}

void disable_column_(const char *group,const char *column, int glen, int clen)
{
  int cact_len, gact_len;
  char col_name[64],gr_name[64];

  gact_len = get_len(group,glen);
  cact_len = get_len(column,clen);

  strncpy(gr_name,group,gact_len); gr_name[gact_len] = '\0';
  strncpy(col_name,column,cact_len); col_name[cact_len] = '\0';
  disable_column(gr_name,col_name);
}

void enable_column_(const char *group,const char *column, int glen, int clen)
{
  int cact_len, gact_len;
  char col_name[64],gr_name[64];

  gact_len = get_len(group,glen);
  cact_len = get_len(column,clen);

  strncpy(gr_name,group,gact_len); gr_name[gact_len] = '\0';
  strncpy(col_name,column,cact_len); col_name[cact_len] = '\0';
  enable_column(gr_name,col_name);
}

