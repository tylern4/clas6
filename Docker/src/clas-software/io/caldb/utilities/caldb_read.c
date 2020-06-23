/* 
 * calib_read.c: read calibration constants
 * for System_Subsystem_Item to caldb using MySQL C API routines
 *        Created:   H.Avagyan 4/5/01
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* define dbase stuff */
#include <mysql.h>
#include "calib_envir.h"
MYSQL *mysql;
char *host=NULL;
char *dbname=NULL;
char *uspass=NULL;
char *user=NULL;
char *RunIndexTable=NULL;
char *systemName="", *subsystemName="", *itemName="" ;
char *date="2037-1-1";
/* define dbase stuff */

int runno=0;
int verbosity=1;  /* print all the stuff by default */
int caldb_verbose=0;          /* 1-for detailed database output */
int nodetails=0;    /* by default will not write (only show the action) */


static void get_options(int argc,char **argv);
static void  syntax(void);


int main(int argc, char **argv)
{
	char *value;
	int  length;
	char *type;
	char *itemid;

/* output structures for ReadConstants */
	valuestruc tlvalue;
	commentstruc runcomm;
	itemvalue itemvaluid,runindexid;
	int i,j,istat;
	float floatarray[100000];
	float intarray[100000];
	get_options(argc,argv);     /* get command line options */

	/* get dbase name,user,password and the RunIndex table name */
	calib_init(argc,argv,&host,&dbname,&user,&uspass,&RunIndexTable);
	if(verbosity){   /*  print details */
		printf("caldb_read: Connecting to dbase: \n");
		printf("      user:      %s \n",user);
		printf("      host:      %s \n",host);
		printf("      dbase:     %s \n",dbname);
		printf("      runindex:  %s \n",RunIndexTable);
	}
	/* MySQL init */
	mysql=ConnectToServer(host,dbname,user,uspass);
	if(mysql==NULL) {
		fprintf(stderr, "caldb_read: Connection problem!: error # %d \n",istat);
		printf("please check env, variables: \n CALDB_HOST,CALDB_USER,CALDB_PASSWORD \n CALDB_DBNAME,RUN_INDEX \n");

		exit(1);
	}
	istat=caldb_getItemId(mysql,systemName,subsystemName,itemName,&length,&type,&itemid);
	if(istat) {
		fprintf(stderr, "\n caldb_read: ERROR: check the system,  error # %d \n",istat);
		printf("please check env, variables: \n CALDB_HOST,CALDB_USER,CALDB_PASSWORD \n CALDB_DBNAME,RUN_INDEX \n");
		DisconnectFromServer(mysql);
		syntax();               /* exits */

	}
	/* good system, reading data from stdin */
/* Read a set of constants from MySQL dbase for a particular Item */
              istat=ReadConstants(mysql,systemName,subsystemName,itemName,RunIndexTable,runno,date, &itemvaluid, &runcomm,&tlvalue);
        if(istat) {
		printf(" maintest: ReadContsants failed!: error # %d \n",istat);
		DisconnectFromServer(mysql);
		exit(1);
         }
/* Read was OK now get the contsnt array from a char string in tlvalue.item */

	if(nodetails==0) {

	 if(verbosity) {
	   printf("Total channels for %s.%s.%s  is %d   \n",systemName,subsystemName,itemName,tlvalue.length);
               for(i=0;i<tlvalue.length;i++) printf("channel %d  %s  \n",i+1,tlvalue.item[i]);
	 } else {
               for(i=0;i<tlvalue.length;i++)printf(" %s \n",tlvalue.item[i]);
	 }
	    

	} else {
	   printf("Total channels for %s.%s.%s  is %d   \n",systemName,subsystemName,itemName,tlvalue.length);
       	    for(i=0;i<tlvalue.length;i++) printf("channel %d  %s %s %s  \n",i+1,tlvalue.item[i],runcomm.author,runcomm.comment);
	}
	DisconnectFromServer(mysql);
	exit(0);
}



static void syntax()
{
	char synstr[] = 
	"\n\n --caldb_read is writing to stdin the set of calib. constants.-- \n"
	"  ENV variables used in input:\n"
	" CALDB_HOST,CALDB_USER,CALDB_PASSWORD,CALDB_DBNAME,RUN_INDEX \n"
	" \n"
	"syntax:\n"
	"    caldb_read [options]  \n"
	"     options:\n"
	"       --system    <System>    system name \n"
	"       --subsystem <Subsystem> system name \n"
	"       --item      <Item>      item name \n"
	"       --runno  <runno>        run # for validity of current set\n"
	"       --date   <upper_date>   upper limit on date for validity (dflt 2037-1-1) of current set\n"
	"       --all                   print all; by dflt print constants   \n"
	"       --v     <verbos_level>        >0 print details by dflt  \n";
	puts(synstr);
	exit(0);
}


/* end default values */
static void  get_options(int argc,char **argv)
{
	int i=0;
    for(i = 1; i < argc; i++) {
         
	       if( !strcmp(argv[i], "--v" ) && i < argc - 1 )
		{
			i++;
			verbosity= atoi(argv[i]);
		}
		else if( !strcmp(argv[i], "--runno" ) && i < argc - 1 )
		{
			i++;
			runno= atoi(argv[i]);
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
		else if( !strcmp(argv[i], "--date" ) && i < argc - 1 )
		{
			i++;
			date=argv[i] ;
		}
		else if( !strcmp(argv[i], "--all" )  )
		{
			nodetails= 1;
		}
		else if(   !strcmp(argv[i], "--help" )
		    || !strcmp(argv[i], "-help" )
		    || !strcmp(argv[i], "-h")
		    || !strcmp(argv[i], "-?") )
			syntax();                 /* exits */
		else /* no listed command line*/
		{
			fprintf(stderr,"caldb_read: Unknown commandline parameter `%s'\n",argv[i]);
			if( isatty(0) && isatty(2) )
			{
				fprintf(stderr, "press return> ");
				getchar();
			}
			syntax();               /* exits */
		}/* no valid  command line*/
	}/* end for */


	if(verbosity) printf("--System  Subsystem Item \n %s %s %s \n  runno      %d  \n date: %s \n ",
	    systemName,subsystemName,itemName,runno,date,nodetails,
	    verbosity);

}


