

int ConnectToDatabase(int disconnect_after_check);

#include <stdio.h>
#include <iostream>
using namespace std;

#include <mysql.h>
//#include <calib_manager.h>
//#include <calib_envir.h>
extern "C" {
#include <calib_connect.h> // for caldb

int caldb_verbose;

};

#undef CALDB_HOST
#undef CALDB_USER

char *CALDB_HOST = "clasdb";
char *CALDB_USER = "clasuser";
char *CALDB_PASSWORD = "";
char *CALDB_RUNINDEX = "calib_user.RunIndexe1fDC";
int RUNNUMBER = 38750;

MYSQL *mysql_dc3=NULL;


//==================================================================
// Check that the tables we're about to write to have the proper
// format.
//==================================================================
int main(int narg, char *argv[])
{
	char subsystem[256]="*";
	char item[256];
	int firsttime, olength;
	int array[36];
	float farray[36];
	int  xvst_item_exists[7][7]    , tmax_item_exists[7];
	int  timewalk_region_exists[4];
	int  timewalk_factor_exists       , timewalk_tau_exists;
	int  timewalk_functiontype_exists , timewalk_betaslope_exists;
	char str[2048];
	int sec,sup,reg;
	int missing_sub=0,missing_tw_sub=0;
	int err=0;
	char mess[256],cmd[512];
	int ret_val=0;
	itemvalue iv;
	commentstruc comments;
	valuestruc  values;
	char future[16]="2037-01-01"; // A date in the future (limit of database)

	// Connect to the database
	caldb_verbose = 0;
	if(ConnectToDatabase(0))return -1;	

	// Initialize arrays
	timewalk_factor_exists=0;
	timewalk_functiontype_exists=0;
	timewalk_tau_exists=0;
	timewalk_betaslope_exists=0;
	for(reg=1;reg<=3;reg++)timewalk_region_exists[reg]=0;
	for(sec=1;sec<=6;sec++){
		tmax_item_exists[sec]=0;
		for(sup=1;sup<=6;sup++){
			xvst_item_exists[sec][sup]=0;
		}
	}

	// Check t_max subsystem
	strcpy(subsystem,"t_max");
	for(sec=1;sec<=6;sec++){
		sprintf(item,"Sector%d",sec);
		if(CALIB_OK==ReadConstants(mysql_dc3, "DC_DOCA", subsystem, item, CALDB_RUNINDEX
			, RUNNUMBER, future, &iv, &comments, &values))tmax_item_exists[sec]=1; 
	}

	// Check xvst subsystems
	for(sup=1;sup<=6;sup++){
		sprintf(item,"SL%d",sup);
		for(sec=1;sec<=6;sec++){
			sprintf(subsystem,"xvst_par_Sect%d",sec);
			if(sec==1)strcpy(subsystem,"xvst_params");
			if(CALIB_OK==ReadConstants(mysql_dc3, "DC_DOCA", subsystem, item, CALDB_RUNINDEX
				, RUNNUMBER, future, &iv, &comments, &values))xvst_item_exists[sec][sup]=1; 
		}
	}

	// Check Timewalk subsystem
	strcpy(subsystem,"Timewalk");
	for(reg=1;reg<=3;reg++){
		sprintf(item,"Region%d",reg);
		if(CALIB_OK==ReadConstants(mysql_dc3, "DC_DOCA", subsystem, item, CALDB_RUNINDEX
			, RUNNUMBER, future, &iv, &comments, &values))timewalk_region_exists[reg]=1;
	}
	if(CALIB_OK==ReadConstants(mysql_dc3, "DC_DOCA", subsystem, "factor", CALDB_RUNINDEX
		, RUNNUMBER, future, &iv, &comments, &values))timewalk_factor_exists=1;
	if(CALIB_OK==ReadConstants(mysql_dc3, "DC_DOCA", subsystem, "tau", CALDB_RUNINDEX
		, RUNNUMBER, future, &iv, &comments, &values))timewalk_tau_exists=1;
	if(CALIB_OK==ReadConstants(mysql_dc3, "DC_DOCA", subsystem, "functiontype", CALDB_RUNINDEX
		, RUNNUMBER, future, &iv, &comments, &values))timewalk_functiontype_exists=1;
	if(CALIB_OK==ReadConstants(mysql_dc3, "DC_DOCA", subsystem, "betaslope", CALDB_RUNINDEX
		, RUNNUMBER, future, &iv, &comments, &values))timewalk_betaslope_exists=1;


	// Compare that the required subsystems/items exist
	for(sec=1;sec<=6;sec++){
		for(sup=1;sup<=6;sup++){
			if(true){
				if(!xvst_item_exists[sec][sup])missing_sub=1;
				if(!tmax_item_exists[sec]     )missing_sub=1;
			}
		}
	}
	for(reg=1;reg<=3;reg++){
		if(!timewalk_region_exists[reg])missing_tw_sub=1;
	}
	if(!timewalk_factor_exists      )missing_tw_sub=1;
	if(!timewalk_tau_exists         )missing_tw_sub=1;
	if(!timewalk_functiontype_exists)missing_tw_sub=1;
	if(!timewalk_betaslope_exists   )missing_tw_sub=1;

	// If non-comment subsystems/arrays were missing, alert user and abort write
	if(missing_sub || missing_tw_sub){
		strcpy(mess,"The format of the database is incorrect.");
		strcat(mess,"Make sure the RunIndex table you're using has valid entries ");
		strcat(mess,"for the DC_DOCA system.");
		printf(mess);
		ret_val = -1;
	}
	
	printf("No errors found.\n");

	DisconnectFromServer(mysql_dc3);
	mysql_dc3=NULL;
	return ret_val;
}


//==================================================================
// Check that we can connect to the database using the supplied
// parameters. Optionally leave the connection open.
//==================================================================
int ConnectToDatabase(int disconnect_after_check)
{
	int cannot_connect=1;
	
	mysql_dc3 = ConnectToServer(CALDB_HOST,"calib",CALDB_USER,CALDB_PASSWORD);
	if(mysql_dc3){
		cannot_connect=0;
		if(disconnect_after_check){
			DisconnectFromServer(mysql_dc3);
			mysql_dc3=NULL;
		}
	}else{
		char cmd[512],mess[512];
		
		strcpy(mess,"Error connecting to the database. Check the parameters and your network connection.");
		printf(mess);
	}

	return cannot_connect;
}
