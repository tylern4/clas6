#include <stdlib.h>
#include "csql.h"

void init_clas(int runno, int runext, const char *jobname)
{
 
  init_csql();
  create_sys_group(jobname);
  init_env();
  init_clas_def();
  /*  add_column();*/

  add_column("SYST","runno");
  init_column("SYST","runno","INT",COL_TYPE_INT,COL_CW_TBL);
  set_column_int("SYST","runno",runno);

  add_column("SYST","runext");
  init_column("SYST","runext","INT",COL_TYPE_INT,COL_CW_TBL);
  set_column_int("SYST","runext",runext);


  print_all_params();
  /*  print_group("SYST");
      print_group("CSQL");*/
  print_all_groups();
}

void  init_clas_def()
{
char *csql_db_database_env;
char *csql_db_databasehost_env;
char *csql_db_username_env;

char *csql_db_databasetable_env;
char *csql_db_comment_env;
char *csql_ddlfilename_env;
char *csql_calibddl_env;

 csql_db_database_env      =  getenv("CSQL_DB");
 csql_db_databasehost_env  =  getenv("CSQL_DBHOST");
 csql_db_username_env      =  getenv("CSQL_USER");
 csql_db_databasetable_env =  getenv("CSQL_TABLE");
 csql_db_comment_env       =  getenv("CSQL_COMM");
 csql_ddlfilename_env      =  getenv("CSQL_DDL");
 csql_calibddl_env         =  getenv("CSQL_CALIB");

 if(csql_db_database_env){
   strcpy(CSQL_Database,csql_db_database_env);
     printf("CSQL_INIT : found envar CSQL_DB ==>  %s\n",CSQL_Database);
 }

  if(csql_db_databasehost_env){
   strcpy(CSQL_Hostname,csql_db_databasehost_env);
     printf("CSQL_INIT : found envar CSQL_DBHOST ==>  %s\n",CSQL_Hostname);
 }

  if(csql_db_username_env){
     strcpy(CSQL_Username,csql_db_username_env);
     printf("CSQL_INIT : found envar CSQL_USER ==>  %s\n",CSQL_Username);
  }
  if( csql_db_databasetable_env){
    strcpy(CSQL_DefTable,csql_db_databasetable_env);
    printf("CSQL_INIT : found envar CSQL_TABLE ==>  %s\n",CSQL_DefTable);
  } else {
    strcpy(CSQL_DefTable,"test");
    printf("CSQL_INIT : env CSQL_TABLE is not found. set table ==>  %s\n",CSQL_DefTable);
  }
  
  if(csql_db_comment_env){
    /*    strcpy(CSQL_DefTable,csql_db_databasetable_env);*/
    printf("CSQL_INIT : found envar CSQL_COMM ==>  %s\n",csql_db_comment_env);
	set_column_char("SYST","jobname",csql_db_comment_env);
  }
  if(csql_ddlfilename_env){
    strcpy(CSQL_DDLFile,csql_ddlfilename_env);
    printf("CSQL_INIT : found envar CSQL_DDL == > %s\n",CSQL_DDLFile);
    printf("\n\tCSQL_INIT : WARNING !!! if CSQL_DDL is set the group will be automaticaly \n");
    printf("\t\t created from this file..\n\n");
    read_ddlfile2(CSQL_DDLFile);
    
  }
  if(csql_calibddl_env){
    /*    strcpy(CSQL_DDLFile,csql_ddlfilename_env);*/
    printf("CSQL_INIT : found envar CSQL_CALIB == > %s\n",csql_calibddl_env);
    printf("\n\tCSQL_INIT : WARNING !!! if CSQL_CALIB is set the group will be automaticaly \n");
    printf("\t\t created from this file..\n\n");
    read_ddlfile2(csql_calibddl_env);
  }
  
}
void init_env()
{

  char *calib_hostname_env;
  char *calib_runindex_env;
  char *calib_timestamp_env;
  
  calib_hostname_env   =  getenv("CLAS_CALDB_HOST");
  calib_runindex_env   =  getenv("CLAS_CALDB_RUNINDEX");
  calib_timestamp_env  =  getenv("CLAS_CALDB_TIME");


  if (calib_hostname_env) {
    strcpy (CSQL_CALIBHost,calib_hostname_env);
  } else{
    strcpy(CSQL_CALIBHost, "default");
  }
  
  if(calib_runindex_env) {
    strcpy(CSQL_RunIndex,calib_runindex_env);
  } else{
    strcpy(CSQL_RunIndex, "RunIndex");
  } 
  if(calib_timestamp_env) {
    strcpy(CSQL_TimeStamp,calib_timestamp_env);
  } else {
    strcpy(CSQL_TimeStamp,"20370101220926");
  }


 
 add_column("SYST","calibdb");
 init_column("SYST","calibdb","CHAR(64)",64,COL_CW_TBL);
 set_column_char("SYST","calibdb",CSQL_CALIBHost);
 
 add_column("SYST","runindex");
 init_column("SYST","runindex","CHAR(32)",32,COL_CW_TBL);
 set_column_char("SYST","runindex",CSQL_RunIndex);
 
 add_column("SYST","timestamp");
 init_column("SYST","timestamp","CHAR(16)",20,COL_CW_TBL);
 set_column_char("SYST","timestamp",CSQL_TimeStamp);
  
}
