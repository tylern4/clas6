/********************************************************/
/* calibinit.c
/* H.Avakian
/* Date  : 2001-03-01
/*
/********************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "calib_envir.h"
char *hostvar=CALDB_HOST;
char *hostdef=CALDB_HOST_DEFAULT;
char *dbnamevar=CALDB_DBNAME;
char *dbnamedef=CALDB_DBNAME_DEFAULT;
char *passvar=CALDB_PASS;
char *passdef=CALDB_PASS_DEFAULT;
char *uservar=CALDB_USER;
char *userdef=CALDB_USER_DEFAULT;
char *rnindexvar=RUN_INDEX;
char *rnindexdef=RUN_INDEX_DEFAULT;

char  *cal_getenv(char *envvar,char *vardef,int notnull);

void calib_init(int argc,char **argv, char **host,char **dbname,char **user,char **pass,char **runindex)
{
	*host=strdup(cal_getenv(hostvar,hostdef,OPTIONAL));
	*dbname=strdup(cal_getenv(dbnamevar,dbnamedef,OPTIONAL));
	*user=strdup(cal_getenv(uservar,userdef,OPTIONAL));
	*pass=CALDB_PASS_DEFAULT;
	*runindex=strdup(cal_getenv(rnindexvar,rnindexdef,OPTIONAL));
	if(caldb_verbose) printf("calib_init: %s %s %s %s\n",*user,*pass,*dbname,*host,*runindex);
	return;
}





























