#include "csql.h"


void add_group(const char *name)
{
  int grp_indx;
  if(find_group_byname(name)>=0) {printf("ADD_GROUP: Group with name [%s] already exists.\n",name);return;}
  grp_indx = get_next_available();
  if(grp_indx<0||grp_indx>=MAX_GROUPS) {printf("ADD_GROUP: Not enought space to open a group...\n"); return;}
  CSQL_Groups[grp_indx].isActive = 2;
  strncpy(CSQL_Groups[grp_indx].name,name,4);CSQL_Groups[grp_indx].name[4] = '\0';
  CSQL_Groups[grp_indx].ncolumns = 0;
  printf("ADD_GROUP: creatin group [%s] at possition %d\n",CSQL_Groups[grp_indx].name,grp_indx);
}


int get_next_available()
{
  int indx = 0;
  for(indx = 0;indx < MAX_GROUPS; indx++)
    {
      if(CSQL_Groups[indx].isActive < 0) return indx;
    }
  return -1;
}

int find_group_byname(const char *name)
{
  int indx = 0;
  for(indx = 0;indx < MAX_GROUPS; indx++)
    {
      if(CSQL_Groups[indx].isActive > 0){
	if(strncmp(CSQL_Groups[indx].name,name,4)==0) return indx;
      }
    }  
  return -1;
}
