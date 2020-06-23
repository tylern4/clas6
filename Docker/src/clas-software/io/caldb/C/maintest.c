/********************************************************/
/* maintest.c
/*
/* Date  : 2001-01-31 test
/*
/********************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <mysql.h>
#include "calib_connect.h"
MYSQL *mysql;
char *host=NULL;
char *dbname=NULL;
char *uspass=NULL;
char *user=NULL;
char *RunIndexTable=NULL;

/* print additional info (turn off for caldb_verbose=0 */
int caldb_verbose=1;

int main(int argc, char **argv)
{
/* get the itemId and corresponding value table  (arbitrary numbers)  */
	char *systemName="LAC_CALIB";
	char *subsystemName="sector1";
	char *itemName="c1l";

/* needed for RunIndex  (arbitrary numbers)  */
	int minRun=8430;
	int maxRun=8560;
	char *runindex_comment="runindex_comment";
	int  runno=8550;
	char *date="2037-1-1";

/* needed for systemName_subsystemName_itemName  (arbitrary numbers)  */
	int minRunSource=8410;
	int maxRunSource=8550;
	char *calib_comment="calib_comment";


/* string of constants their length and type for Write/LinkConstants */
	char *value;
	int  length;
	char *type;


/* output structures for ReadConstants */
	valuestruc tlvalue;
	commentstruc runcomm;
	itemvalue itemvaluid,runindexid;

/*  arrays to stor test set of constants */ 
	float fvalue[500];
	float fvalue1[500];
	float fvalue2[128];
	int itemlength;
/* status variable 0 if transaction OK */
	int istat=0;
	int i;

/* get dbase name,user,password and the RunIndex table name */
	calib_init(argc,argv,&host,&dbname,&user,&uspass,&RunIndexTable);
	if(caldb_verbose){   /*  print details */
		printf("main: Connecting to dbase: \n");
		printf("      user:      %s \n",user);
		printf("      host:      %s \n",host);
		printf("      dbase:     %s \n",dbname);
		printf("      runindex:  %s \n",RunIndexTable);
	}
/* MySQL init */
	mysql=ConnectToServer(host,dbname,user,uspass);

	if(mysql==NULL) {
		printf(" maintest: Connection problem!: error # %d \n",istat);
		exit(1);
	}

/* Read a set of constants from MySQL dbase for a particular Item */

      istat=ReadConstants(mysql,systemName,subsystemName,itemName,RunIndexTable,runno,date, &itemvaluid, &runcomm,&tlvalue);
      if(istat) {
		printf(" maintest: ReadContsants failed!: error # %d \n",istat);
		exit(1);
	}
/* Read was OK now get the contsnt array from a char string in tlvalue.item */
	istat=calib_value2float(&itemlength,&fvalue,tlvalue );

/* print all constants (from 1...tlvalue.length) */
	for(i=0;i<tlvalue.length;i++){
	  fvalue2[i]=1.5*(i+1);       /* define new arrays for Write/LinkConstants test  */
	  fvalue1[i]=fvalue[i]+1.5;
	 if(caldb_verbose) printf("maintest: channel %d of %d cval -> =%s=  fval2 -> =%f=\n",i+1,itemlength,tlvalue.item[i],fvalue2[i]);
	}


/* convert the test array of floats (fvalue2[i]) to a value string */
        if(caldb_verbose) printf("maintest++: tlvalue.length %d\n",tlvalue.length);
	istat=calib_float2value(tlvalue.length,fvalue2,&value);
        if(caldb_verbose) printf("maintest:float2valuestring %s\n", value);

/*  WriteConstantSet  */
       	istat=WriteConstantSet(mysql,systemName,subsystemName,itemName,minRun,maxRun,calib_comment,value,&itemvaluid);
        if(istat) {
		printf(" maintest: WriteContsants failed!: error # %d \n",istat);
		exit(1);
	}

/*  WriteConstantSet  succeded print the record number in the valuID table (to be used in LinkConstants)*/

	if(caldb_verbose) printf(" maintest:  itemvalueid= %d \n",itemvaluid);

/* Make a LinkConstants using the itemvalueid and some additional info (runMin,runMax.....) */ 

       	istat=LinkConstantSet(mysql,systemName,subsystemName,itemName,RunIndexTable,minRun,maxRun,calib_comment,itemvaluid,runcomm,&runindexid);

        if(istat) {
		printf(" maintest: LinkContsants failed!: error # %d \n",istat);
		exit(1);
	}
/* Link succeded print the record number in the RunIndex table */
	if(caldb_verbose) printf(" maintest: Record # %d in  %s table added: \n",runindexid,RunIndexTable);

/* Make a Write&LinkConstants in one call */ 
/* convert the test array of floats (fvalue1[i]) to a value string */
	istat=calib_float2value(tlvalue.length,fvalue1,&value);

       istat=WriteAndLinkConstantSet(mysql,systemName,subsystemName,itemName,minRunSource,maxRunSource,calib_comment,value,&itemvaluid,RunIndexTable,minRun,maxRun,runindex_comment,&runindexid);
        if(istat) {
		printf(" maintest: Write&LinkContsants failed!: error # %d \n",istat);
		exit(1);
	}

/* We are done, disconnect from server */
	DisconnectFromServer(mysql);

	exit (0);


}





























