/********************************************************/
/* H.Avakian
/* Date  : 2001-03-01
/********************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "calib_envir.h"

char envvalue[ENV_VAR_LENGTH];

char  *cal_getenv(char *envvar,char *vardef,int notnull) {
	char *p;

	if( (p = getenv(envvar )) ){
		strcpy(envvalue,p);
	} else{
		strcpy(envvalue,vardef);
	}
	if(!strcmp(envvalue,"") && notnull ) {
		printf("cal_getenv: Enter the value for %s:",envvar);
		scanf(" %s",envvalue);
	}


	if(caldb_verbose) printf("cal_getenv: env %s  def %s  current %s %d\n",
                                  envvar, vardef, envvalue,notnull);
	return envvalue;
}
