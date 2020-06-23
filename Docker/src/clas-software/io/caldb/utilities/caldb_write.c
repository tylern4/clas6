/* 
 * calib_write.c: write a calibration file with constants
 * for System_Subsystem_Item to caldb using MySQL C API routines
 *        Created:   H.Avagyan 4/5/01
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* MySQL types */
static const char *type_names[]=
{ 	"char", "short", "long" ,"int","float",
	  "double"
};

char *systemName="", *subsystemName="", *itemName="" ;
char *runindex_comment, *calib_comment;

int noaction=0;    /* by default will not write (only show the action) */

int mapemulation=0;   /* by default turned off */
int minRun=0;
int maxRun=0;
int minRunSource=0;
int maxRunSource=0;
int verbosity=1;  /* print all the stuff by default */

/* define dbase stuff */
#include <mysql.h>
#include "calib_envir.h"
MYSQL *mysql;
char *host=NULL;
char *dbname=NULL;
char *uspass=NULL;
char *user=NULL;
char *RunIndexTable=NULL;
int caldb_verbose=0;          /* 1-for detailed database output */
/* define dbase stuff */

static void get_options(int argc,char **argv);
static void  syntax(void);
int getMaxRun(MYSQL *conn,int runno, char *itemId);


int main(int argc, char **argv)
{
	char *value;
	int  length;
	char *type;
	char *itemid;
	itemvalue itemvaluid,runindexid;
	int i,j,istat;
	float floatarray[100000];
	float intarray[100000];

	get_options(argc,argv);     /* get command line options */


	/* get dbase name,user,password and the RunIndex table name */
	calib_init(argc,argv,&host,&dbname,&user,&uspass,&RunIndexTable);
	if(verbosity){   /*  print details */
		printf("caldb_write: Connecting to dbase: \n");
		printf("      user:      %s \n",user);
		printf("      host:      %s \n",host);
		printf("      dbase:     %s \n",dbname);
		printf("      runindex:  %s \n",RunIndexTable);
	}
	/* MySQL init */
	mysql=ConnectToServer(host,dbname,user,uspass);
	if(mysql==NULL) {
		fprintf(stderr, "caldb_write: Connection problem!: error # %d \n",istat);
		printf("please check env, variables: \n CALDB_HOST,CALDB_USER,CALDB_PASSWORD \n CALDB_DBNAME,RUN_INDEX \n");

		exit(1);
	}

	istat=caldb_getItemId(mysql,systemName,subsystemName,itemName,&length,&type,&itemid);
	if(istat) {
		fprintf(stderr, "\n caldb_write: ERROR: check the system,  error # %d \n",istat);
		DisconnectFromServer(mysql);
		syntax();               /* exits */

	} else {
		/* good system, reading data from stdin */
		if(strcmp(type,type_names[4])==0) {
			for(i=0; fscanf(stdin,"%g",&floatarray[i]) != EOF; i++);
			if(verbosity) for(j=0; j<i; j++) printf("float type data--->  %d %d %f \n",length,j, floatarray[j]
			    );
		}
		else if (strcmp(type,type_names[3])==0) {
			for(i=0; fscanf(stdin,"%d",&intarray[i]) != EOF; i++);
			if(verbosity) for(j=0; j<i; j++) printf("int type data--->  %d %d %d \n",length,j, intarray[j] );
		}
		else
		{
			fprintf(stderr," caldb_write: ERROR! Type %s not supported yet \n",type);
			DisconnectFromServer(mysql);
			exit(1);
		}

		/* System OK, now check the data integrity */

		if(length!=i){
			fprintf(stderr,"wrong length in input file %d  while should be %d\n",i,length);
			DisconnectFromServer(mysql);
			exit(1);
		}

	}

	/* convert the test array of floats (fvalue1[i]) to a value string */
	if(strcmp(type,type_names[4])==0) {
		istat=calib_float2value(length,floatarray,&value);
	}
	else if (strcmp(type,type_names[3])==0) {
		istat=calib_int2value(length,intarray,&value);
	}
	if(mapemulation) { /* replace the maxRun with the value from dbase */
	  maxRun=getMaxRun(mysql,minRun,itemid);  
	  if( maxRun <= 0 ) {
			fprintf(stderr,"bad maxRun in emulation %d exiting...\n",maxRun);
			DisconnectFromServer(mysql);
			exit(1);
	  }
	}

	if(noaction==0) {
		printf(" \n Use option --write to execute \n\n WriteAndLinkConstantSet(mysql,%s,%s,%s,%d,%d,%s,%s,%s,%d,%d %s)\n",
		    systemName,subsystemName,itemName,minRunSource,maxRunSource,calib_comment,value,RunIndexTable,minRun,maxRun,
		    runindex_comment);
	} else {
		/* Make a Write&LinkConstants in one call */
		istat=WriteAndLinkConstantSet(mysql,systemName,subsystemName,itemName,minRunSource,maxRunSource,calib_comment,
		    value,&itemvaluid,RunIndexTable,minRun,maxRun,runindex_comment,&runindexid);
		if(istat) {
			printf(" caldb_write: Write&LinkContsants failed!: error # %d \n",istat);
			printf("please check env, variables: \n CALDB_HOST,CALDB_USER,CALDB_PASSWORD \n CALDB_DBNAME,RUN_INDEX \n");
		} else {
			if(verbosity) printf(" caldb_write: Write&LinkContsants DONE! runindexId=%d\n",runindexid);
		}
	}
	DisconnectFromServer(mysql);
	exit(0);

}

static void syntax()
{
	char synstr[] = 
	"\n\n --caldb_write is writing the stdin to caldb database.-- \n"
	"  ENV variables used in input:\n"
	" CALDB_HOST,CALDB_USER,CALDB_PASSWORD,CALDB_DBNAME,RUN_INDEX \n"
	" \n"
	"syntax:\n"
	"    caldb_write [options] < input_calibration_file\n"
	"     options:\n"
	"       --system    <System>    system name \n"
	"       --subsystem <Subsystem> system name \n"
	"       --item      <Item>      item name \n"
	"       --map                   map emulation: maxRun not used   \n"
	"       --write                  write to database (by dflt is  just showing without writing)   \n"
	"       --runmin  <minRun>         min run # for validity of current set\n"
	"       --runmax  <maxRun>  \n"
	"       --smin  <minRunSource>     min run used in calibration \n"
	"       --smax  <maxRunSource>   \n"
	"       --comr  <comment>        a comment line for RunIndex table   \n"
	"       --comv  <comment>        a comment line for Sys_Subsys_Item table   \n"
	"       --v     <verbos_level>        >0 print details by dflt  \n";
	puts(synstr);
	exit(0);
}


/* end default values */
static void  get_options(int argc,char **argv)
{
	int i;
	for(i = 1; i < argc; i++) {
		if( !strcmp(argv[i], "--v" ) && i < argc - 1 )
		{
			i++;
			verbosity= atoi(argv[i]);
		}
		else if( !strcmp(argv[i], "--runmax" ) && i < argc - 1 )
		{
			i++;
			maxRun= atoi(argv[i]);
		}
		else if( !strcmp(argv[i], "--runmin" ) && i < argc - 1 )
		{
			i++;
			minRun= atoi(argv[i]);
		}
		else if( !strcmp(argv[i], "--smax" ) && i < argc - 1 )
		{
			i++;
			maxRunSource= atoi(argv[i]);
		}
		else if( !strcmp(argv[i], "--smin" ) && i < argc - 1 )
		{
			i++;
			minRunSource= atoi(argv[i]);
		}
		else if( !strcmp(argv[i], "--item" ) && i < argc - 1 )
		{
			i++;
			itemName=argv[i] ;
		}
		else if( !strcmp(argv[i], "--subsystem" ) && i < argc - 1 )
		{
			i++;
			subsystemName=argv[i] ;
		}
		else if( !strcmp(argv[i], "--system" ) && i < argc - 1 )
		{
			i++;
			systemName=argv[i] ;
		}
		else if( !strcmp(argv[i], "--comv" ) && i < argc - 1 )
		{
			i++;
			calib_comment =argv[i] ;
		}
		else if( !strcmp(argv[i], "--comr" ) && i < argc - 1 )
		{
			i++;
			runindex_comment =argv[i] ;
		}
		else if( !strcmp(argv[i], "--write" )  )
		{
			noaction= 1;
		}
		else if( !strcmp(argv[i], "--map" )  )
		{
			mapemulation= 1;
		}
		else if(   !strcmp(argv[i], "--help" )
		    || !strcmp(argv[i], "-help" )
		    || !strcmp(argv[i], "-h")
		    || !strcmp(argv[i], "-?") )
			syntax();                 /* exits */
		else /* no listed command line*/
		{
			fprintf(stderr,"caldb_write: Unknown commandline parameter `%s'\n",argv[i]);
			if( isatty(0) && isatty(2) )
			{
				fprintf(stderr, "press return> ");
				getchar();
			}
			syntax();               /* exits */
		}/* no valid  command line*/
	}/* end for */


	if(verbosity) printf("System  Subsystem Item \n %s %s %s \n  runMin/Max      %d /  %d  \n Source runmin/max %d / %d \n RunIndex_comment %s\n ValuId_comment %s \n action: %d %d \n ",
	    systemName,subsystemName,itemName,minRun,maxRun,minRunSource,maxRunSource,runindex_comment,calib_comment,noaction,
	    verbosity);
        if(!mapemulation && maxRun<minRun) {
		syntax();               /* exits */
	}

}


/*
function getMaxRun returns the maxRun from RunIndex table for current 
runno and itemId or -1 in case of error
*/

int getMaxRun(MYSQL *conn,int runno, char *itemId)
{
	MYSQL_ROW rows_in_query;
	MYSQL_RES *mysql_res;
	char condList[SQL_CMD_SPACE];
	char query[SQL_CMD_SPACE];
        int len,runmax=-1;
	sprintf(condList, "itemId = %s AND minRun > %d ORDER BY minRun  LIMIT 1 ",itemId, runno);
	sprintf(query,"SELECT minRun FROM %s WHERE %s",RunIndexTable,condList);
len = strlen(query);

	if(verbosity) printf("getMaxRun: RunIndexTable %s, sending query  %s  for itemId %d (mysql=%d)\n", RunIndexTable,query,conn);

	if (mysql_real_query(conn, query, len) !=0)
	{
		fprintf (stderr,"getMaxRun:mysql_real_query() failed: \n MySQL Error %u  %s\n",
		    mysql_errno(conn),mysql_error(conn));
		return (-1);
	}
	else
	{
		mysql_res = mysql_store_result(conn);
		rows_in_query = mysql_fetch_row(mysql_res);
		if (!rows_in_query)
		{
			fprintf (stderr,"getMaxRun: No data in mysql_real_query()\n" );
			return (-1);
		}
		else /* looks OK,  filling output structures */
		{
		  runmax=atoi(rows_in_query[0])-1;
	          if(verbosity) printf("getMaxRun: item# %s, runno=%d runmax=%d\n",itemId,runno,runmax);
         	  return runmax;
		}
	}
}





