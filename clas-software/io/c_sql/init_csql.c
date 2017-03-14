#include <stdio.h>
#include <stdlib.h>
#include "csql.h"


void init_csql(){

int i;

fNGroups = 0; 
 printf("INIT_CSQL: Initializing CSQL package..\n"); 
 printf("INIT_CSQL: Resetting all groups..\n");
 for(i = 0; i < MAX_GROUPS; i++){
   reset_group(&CSQL_Groups[i]);
 }
 set_database("claspc10.jlab.org","clas_offline","","clas_offline");
 fDebugMode = 0;

}


void csql_init_(){
 printf("INIT_CSQL: Initializing CSQL package..\n"); 
 init_csql();
return;
}

void  set_database(char *hostname,char *username, char *password,char *DB_name){

  strcpy(CSQL_Hostname,hostname);
  strcpy(CSQL_Username,username);
  strcpy(CSQL_Database,DB_name);
  CSQL_Password[0] = '\0';

  printf("\nSET_DATABASE: Database settings has been changed to the following..\n");
  printf("HostName:  %s\n",CSQL_Hostname);
  printf("UserName:  %s\n",CSQL_Username);
  printf("DataBase:  %s\n",CSQL_Database);
  printf("Password:  No Password\n\n");
}



void  reset_group(group_t *grp)
{
  int j ;
  grp->isActive = -1;
  strcpy(grp->name,"DDDD"); grp->name[4] = '\0';
  grp->ncolumns = 0;
  for(j=0;j<MAX_COLUMNS;j++){
    strcpy(grp->fColumns[j].name,"UNDEF");
    strcpy(grp->fColumns[j].type,"UNDEF");
    strcpy(grp->fColumns[j].value,"UNDEF");
    grp->fColumns[j].itype = 0;
    grp->fColumns[j].index = 0;
    grp->fColumns[j].mode = 0;
  }
}



void print_all_params()
{
  printf("Programs Initialized with following parameters..\n");
  printf("======>>>>>\n");
  printf("PAR=> DATABASE HOST   : %s\n",CSQL_Hostname);
  printf("PAR=> DATABASE USER   : %s\n",CSQL_Username);
  printf("PAR=> DATABASE DB     : %s\n",CSQL_Database);
  printf("PAR=> MONITOR TABLE   : %s\n",CSQL_MonTable);
  printf("PAR=> DEFAULT TABLE   : %s\n",CSQL_DefTable);
  printf("PAR=> DDL FILENAME    : %s\n",CSQL_DDLFile);
  printf("PAR=> CALIB DATABASE  : %s\n",CSQL_CALIBHost);
  printf("PAR=> CALIB RUNINDEX  : %s\n",CSQL_RunIndex);
  printf("PAR=> CALIB TIMESTAMP : %s\n",CSQL_TimeStamp);

  printf("======>>>>>\n");
}
