// This was modified on 7/25/2001 to use the caldb C-API   D.L.

#include "dc3.h"

#include <map_manager.h>
#include <mysql.h>
extern "C" {
#include <calib_connect.h> // for caldb
#undef CALDB_HOST
#undef CALDB_USER
#undef CALDB_PASSWORD

int calib_int2value(int itemlength, int array[], char **valuestring);
int calib_value2float(int *itemlength, float array[], valuestruc tlvalue );
int calib_value2int(int *itemlength, int array[], valuestruc tlvalue );
int calib_char2value( int itemlength, char *carray, char **valuestring);
int calib_float2value(int itemlength, float array[], char **valuestring);
}

float write_to_caldb[7][7];


MYSQL *mysql_dc3=NULL;
int caldb_verbose=0; // This is used in caldbC. I don't know why it's not declared there (??)
char COMMENT[2*MAX_COMMENT_SIZE];

int ExpertCheck(void);
int TimewalkCheck(void);
int CaldbCheck(void);
void WriteToCaldb(void);
int GetComments(char *comment);


int dc3_put_float_to_db(char *sys, char* subsys, char *item, int itemlen, float array[]);
int dc3_put_int_to_db(char *sys, char* subsys, char *item, int itemlen, int array[]);
int WriteAndLinkConstantSet_int(
	MYSQL *conn, char *systemname,char *subsystemname,char *itemname,
	int minrunsource, int maxrunsource,
	char *calib_comment,
	int itemlength, int array[],
	itemvalue *itemvalueid,
	char *RunIndexTable,
	int minrun,int maxrun,
	char *runindex_comment,
	itemvalue *runindexid);
int WriteAndLinkConstantSet_float(
	MYSQL *conn, char *systemname,char *subsystemname,char *itemname,
	int minrunsource, int maxrunsource,
	char *calib_comment,
	int itemlength, float array[],
	itemvalue *itemvalueid,
	char *RunIndexTable,
	int minrun,int maxrun,
	char *runindex_comment,
	itemvalue *runindexid);


//extern "C" {
//	char* crypt(char*,char*);
//}

//============================================================
// Entry point for writing calibration constants to database.
// This is called when the "Write to Database" button is pressed.
//============================================================
int CaldbWrite(ClientData clientData,Tcl_Interp *interp
   ,int argc, const char *argv[])
{
	int sec,sup;
	char varname[32];
	int err;

	// Check if any values obtained in "expert" mode
	if(ExpertCheck())return TCL_OK;

	// Check if we need to write timewalk parameters
	if(TimewalkCheck())return TCL_OK;

	// Check that we can connect to the database
	if(ConnectToDatabase(1))return TCL_OK;

	// Check format of the tables to be written to
	if(CaldbCheck())return TCL_OK;

	// Form comment string
	if(GetComments(COMMENT))return TCL_OK;

	// Everything checks out. Go ahead and write to the database
	WriteToCaldb();

	return TCL_OK;
}

//=================================================================
// Check if any values to be written were obtained while in
// "expert" mode. If so, do the password verification. If
// everything checks out, return 0. Returning a non-zero value
// indicates writing to the Caldb should not proceed.
//=================================================================
int ExpertCheck(void)
{
	int sec,sup;
	char list[256]="";
	char passwd[256];
	char *encrypted_pass;
	char actual_encrypted_pass[256]="dc2.uaEoZNYMU";
	char cmd[512],mess[512];

	// Get List of sec/sup that have expert flag set
	for(sec=1;sec<=6;sec++){
		for(sup=1;sup<=6;sup++){
			if(USER_PARMS[sec][sup].expert_mode){
				sprintf(list,"%s, S%dSL%d",list,sec,sup);
			}
		}
	}

	// Continue with write if no expert flags are set
	if(strlen(list)==0)return 0;

	// Ask for password 
	Tcl_Evaluate("GetPassword");
	strcpy(passwd,interp->result);
	if(strlen(passwd)==0)return 1; // User canceled

	// Check Password. Return if correct
	encrypted_pass=crypt(passwd,actual_encrypted_pass);
	if(!strcmp(encrypted_pass,actual_encrypted_pass))return 0;

	// Password was incorrect. Inform user and cancel write operation
	strcpy(mess,"Sorry, the password you entered was incorrect.");
	sprintf(cmd,"tk_dialog .wrongpass \"Password Incorrect\" \"%s\" warning 0 \"Rats!\"",mess);
	Tcl_Evaluate(cmd);
	return -1;
}

//=========================================================================
// Check if the timewalk parameters have changed. If so, ask user if we
// want to continue with the write or not.
//=========================================================================
int TimewalkCheck(void)
{
	int i;
	int diff=0;
	char mess[256],cmd[512];
	
	for(i=1;i<=10;i++){
		if(FILE_PARMS[1][1].tw_parm_r1[i]!=USER_PARMS[1][1].tw_parm_r1[i])diff=1;
		if(FILE_PARMS[1][1].tw_parm_r2[i]!=USER_PARMS[1][1].tw_parm_r2[i])diff=1;
		if(FILE_PARMS[1][1].tw_parm_r3[i]!=USER_PARMS[1][1].tw_parm_r3[i])diff=1;
	}
	for(i=1;i<=3;i++){
		if(FILE_PARMS[1][1].tw_fact[i]!=USER_PARMS[1][1].tw_fact[i])diff=1;
		if(FILE_PARMS[1][1].tw_tau[i]!=USER_PARMS[1][1].tw_tau[i])diff=1;
	}	
	for(i=1;i<=6;i++){
		if(FILE_PARMS[1][1].tw_betaslope[i]!=USER_PARMS[1][1].tw_betaslope[i])diff=1;
	}
	if(FILE_PARMS[1][1].tw_functiontype!=USER_PARMS[1][1].tw_functiontype)diff=1;

	// If differences found, ask user what they want to do
	if(diff){
		strcpy(mess,"Timewalk parameters have changed. Do you want to write the new parameters?");
		sprintf(cmd,"tk_dialog .writetimewalk \"Timewalk parameters changed\" \"%s\" questhead 0 ",mess);
		strcat(cmd,"\"Continue\" \"Cancel\"");
		Tcl_Evaluate(cmd);
		switch(atoi(interp->result)){
			case 0:
				return 0;
			case 1:
				return -1;
			default:
				sprintf(mess,"Invalid value at %s:%d",__FILE__,__LINE__);
				sprintf(cmd,"tk_dialog .badresponse \"Program error\" \"%s\" warning 0 \"OK\"",mess);
				Tcl_Evaluate(cmd);
				return -1;
		}
	}

	return 0;
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
		sprintf(cmd,"tk_dialog .dberror \"DB connection Error\" \"%s\" warning 0 ",mess);
		strcat(cmd,"\"#$?*%!!!\"");
		Tcl_Evaluate(cmd);
	}

	return cannot_connect;
}

//==================================================================
// Check that the tables we're about to write to have the proper
// format.
//==================================================================
int CaldbCheck(void)
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
	cout<<"Checking the database for run:"<<RUNNUMBER<<endl;

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
			if(write_to_caldb[sec][sup]){
				if(!xvst_item_exists[sec][sup]){
					missing_sub=1;
					cout<<"Couldn't find xvst item for sec="<<sec<<" sup="<<sup<<" (RUNNUMBER="<<RUNNUMBER<<")"<<endl;
				}
				if(!tmax_item_exists[sec]     ){
					missing_sub=1;
					cout<<"Couldn't find tmax item for sec="<<sec<<" sup="<<sup<<" (RUNNUMBER="<<RUNNUMBER<<")"<<endl;
				}
			}
		}
	}
	for(reg=1;reg<=3;reg++){
		if(!timewalk_region_exists[reg]){
			missing_tw_sub=1;
			cout<<"Couldn't find timewalk region item for reg="<<reg<<" (RUNNUMBER="<<RUNNUMBER<<")"<<endl;
		}
	}
	if(!timewalk_factor_exists      )missing_tw_sub=1;
	if(!timewalk_tau_exists         )missing_tw_sub=1;
	if(!timewalk_functiontype_exists)missing_tw_sub=1;
	if(!timewalk_betaslope_exists   )missing_tw_sub=1;
	cout<<"One of the timewalk items doesn't exist"<<" (RUNNUMBER="<<RUNNUMBER<<")"<<endl;

	// If non-comment subsystems/arrays were missing, alert user and abort write
	if(missing_sub || missing_tw_sub){
		strcpy(mess,"The format of the database is incorrect.");
		strcat(mess,"Make sure the RunIndex table you're using has valid entries ");
		strcat(mess,"for the DC_DOCA system.");
		sprintf(cmd,"tk_dialog .badformat \"Bad database format\" \"%s\" warning 0 \"Oops!\"",mess);
		Tcl_Evaluate(cmd);
		ret_val = -1;
	}

	DisconnectFromServer(mysql_dc3);
	mysql_dc3=NULL;
	return ret_val;
}


//=============================================
// Write all selected values to the database
//=============================================
void WriteToCaldb(void)
{
	int sec,sup,layer,i;
	char subsystem[256],item[256];
	int err,err_rem;
	int firsttime;
	float farray[36],*par;
	char cmd[512],mess[512];
	int *ipar;

	// Connect to the database
	caldb_verbose = 0;
	if(ConnectToDatabase(0))return;	

	// Write to Tmax subsystem:
	// We loop over sectors, filling farray with values
	// from USER_PARMS[][]. Each sector is written to the
	// caldb individually, but with the same comment/run ranges. 
	strcpy(subsystem,"t_max");
	for(sec=1;sec<=6;sec++){
		sprintf(item,"Sector%d",sec);
		for(sup=1;sup<=6;sup++){
			for(i=0;i<6;i++){
				layer=((sup-1)*6)+i;
				farray[layer]=USER_PARMS[sec][sup].tmax[layer];
			}
		}

		// Write to the database and link to RunIndex table
		err = dc3_put_float_to_db("DC_DOCA", subsystem, item, 36, farray);
	}

	// Write to the XvsT subsystems
	for(sec=1;sec<=6;sec++){
		sprintf(subsystem,"xvst_par_Sect%d",sec);
		if(sec==1)strcpy(subsystem,"xvst_params");
		for(sup=1;sup<=6;sup++){

			sprintf(item,"SL%d",sup);
			par=USER_PARMS[sec][sup].parms.par;
			err = dc3_put_float_to_db("DC_DOCA", subsystem, item, 24, par);
		}
	}

	// Timewalk Subsystems
	// The timewalk parameters are handled a little differently in that
	// the same parameters should exist for all USER_PARMS[][] regardless
	// which sector/superlayer is indexed. Here, we write what is in S1SL1.
	par=&USER_PARMS[1][1].tw_parm_r1[1];
	err = dc3_put_float_to_db("DC_DOCA", "Timewalk","Region1",10, par);

	par=&USER_PARMS[1][1].tw_parm_r2[1];
	err = dc3_put_float_to_db("DC_DOCA", "Timewalk","Region2",10, par);

	par=&USER_PARMS[1][1].tw_parm_r3[1];
	err = dc3_put_float_to_db("DC_DOCA", "Timewalk","Region3",10, par);

	ipar=&USER_PARMS[1][1].tw_functiontype;
	err = dc3_put_int_to_db("DC_DOCA", "Timewalk","functiontype",1, ipar);

	par=&USER_PARMS[1][1].tw_betaslope[1];
	err = dc3_put_float_to_db("DC_DOCA", "Timewalk","betaslope",6, par);
     
	switch(USER_PARMS[1][1].tw_functiontype){
		case DC_TIMEWALK_TYPE_LIMING:
			par=&USER_PARMS[1][1].tw_fact[1];
			err = dc3_put_float_to_db("DC_DOCA", "Timewalk","factor",3, par);

			par=&USER_PARMS[1][1].tw_tau[1];
			err = dc3_put_float_to_db("DC_DOCA", "Timewalk","tau",3, par);
			break;
		case DC_TIMEWALK_TYPE_VIPULI:
			par=&USER_PARMS[1][1].tw_betaslope[1];
			err = dc3_put_float_to_db("DC_DOCA", "Timewalk","betaslope",6, par);
			break;
		case DC_TIMEWALK_TYPE_MAC:
		case DC_TIMEWALK_TYPE_NONE:
			break;
		default:
			sprintf(mess,"Unknown timewalk function type (%d). ",USER_PARMS[1][1].tw_functiontype);
			strcat(mess,"It's not likely this calibration will work well. ");
			sprintf(cmd,"tk_dialog .mfcerr \"Bad timewalk type\" \"%s\" warning 0 \"Rats!\"",mess);
			Tcl_Evaluate(cmd);
			err=-1;
	}

	if(err){
		strcpy(mess,"Error(s) occured while writing to the map. Check your ");
		strcat(mess,"write permissions on the map file and try again. If the ");
		strcat(mess,"problem persists, try creating a new map file and writing ");
		strcat(mess,"to that.");
		sprintf(cmd,"tk_dialog .mfcerr \"Error writing to Mapfile\" \"%s\" warning 0 \"OK\"",mess);
		Tcl_Evaluate(cmd);		
	}else{
		sprintf(mess,"Constants successfully written to map for run %d.",RUNNUMBER);
		sprintf(cmd,"tk_dialog .mfcerr \"Writing to Mapfile Successful\" \"%s\" info 0 \"OK\"",mess);
		Tcl_Evaluate(cmd);
	}

	DisconnectFromServer(mysql_dc3);
	mysql_dc3=NULL;
}

//======================================================
// This prompts the user for comments to be entered into
// the database and pre-pends information about dc3 
// (version, execution host, etc.) 
//======================================================
int GetComments(char *comment)
{
	int err;
	char item[32]="comment";
	char *ptr,str[MAX_COMMENT_SIZE];
	int firsttime,olength;
	time_t t;
	char *LOGNAME,*HOST,*OSNAME,*PWD;
	char mess[512],cmd[512];
	char dclib_versionstr[32];
	char dch_dclib_versionstr[32];
	char dch_dc3_versionstr[32];

	// Get user comments. Any comments entered by the user are
	// copied into the linked variable USERCOMMENTS from tcl.
	Tcl_Evaluate("AdditionalComments");
	if(!strcmp(interp->result,"Cancel"))return -1;

	// Write dc3 info at top of comment
	comment[0]=0;
	LOGNAME=getenv("LOGNAME");
	HOST=getenv("HOST");
	OSNAME=getenv("OSNAME");
	PWD=getenv("PWD");
	t=time(NULL);
#ifdef DC_DCH_VERSION_MAJOR_h
	sprintf(dclib_versionstr,"%d.%d",DC_DCLIB_VERSION_MAJOR,DC_DCLIB_VERSION_MINOR);
	sprintf(dch_dclib_versionstr,"%d.%d",DC_DCH_VERSION_MAJOR,DC_DCH_VERSION_MINOR);
	sprintf(dch_dc3_versionstr,"%d.%d",DC_DCH_VERSION_MAJOR_h,DC_DCH_VERSION_MINOR_h);
#else
	strcpy(dclib_versionstr,"unknown");
	strcpy(dch_dclib_versionstr,"unknown");
	strcpy(dch_dc3_versionstr,"unknown");
#endif
	sprintf(str,"execution host:%s\n",HOST ? HOST:"unknown");			strcat(comment,str);
	sprintf(str,"host OS:%s\n",OSNAME ? OSNAME:"unknown");				strcat(comment,str);
	sprintf(str,"host time:%s",ctime(&t));										strcat(comment,str);
	sprintf(str,"user:%s\n",LOGNAME ? LOGNAME:"unknown");					strcat(comment,str);
	sprintf(str,"Version Info:\n");												strcat(comment,str);
	sprintf(str,"  dc3: %s\n",VERSIONSTR);										strcat(comment,str);
	sprintf(str,"  dclib: %s\n",dclib_versionstr);							strcat(comment,str);
	sprintf(str,"  dc.h: %s(dclib) %s(dc3)\n"
		,dch_dclib_versionstr,dch_dc3_versionstr);							strcat(comment,str);
	sprintf(str,"hbook file: %s\n",INPUTFILE ? INPUTFILE:"unknown");	strcat(comment,str);
	sprintf(str,"working dir: %s\n",PWD ? PWD:"unknown");					strcat(comment,str);

	sprintf(str,"\nUser comments follow\n");									strcat(comment,str);
	sprintf(str,"========================\n");								strcat(comment,str);
	

	if(USERCOMMENT){
		if(strlen(USERCOMMENT)+strlen(comment)+2>=MAX_COMMENT_SIZE){
			sprintf(str,"Comment string is too long. It will be truncated.");
			sprintf(cmd,"tk_dialog .commerr \"Comment too long\" \"%s\" warning 0 \"OK\" \"Cancel write\"",str);
			Tcl_Evaluate(cmd);
			if(atoi(interp->result))return -1;
			strncat(comment,USERCOMMENT, MAX_COMMENT_SIZE-strlen(comment)-2);
		}else{
			strcat(comment,USERCOMMENT); // From GUI
			strcat(comment,"\n");
		}
	}
   
	return 0;
}

//=========================================================
// Routines with minimal arguments for writing and
// linking to database. This takes several parameters
// from global variables which should be the same for
// all calls.
//=========================================================
int dc3_put_float_to_db(char *sys, char* subsys, char *item, int itemlen, float array[])
{
	itemvalue itemvalueid, runindexid;
	int err;
	
	// Check that we have an open connection
	if(!mysql_dc3){
		cerr<<__FILE__<<":"<<__LINE__<<" ";
		cerr<<"Attempt to write to database without connection"<<endl;
		return -1;
	}

	// Write to the database
	err = WriteAndLinkConstantSet_float(mysql_dc3,sys,subsys,item
		,RUNNUMBER ,RUNNUMBER
		,COMMENT
		,itemlen ,array
		,&itemvalueid
		,CALDB_RUNINDEX
		,MINRUN ,MAXRUN
		,COMMENT
		,&runindexid);

	return err;
}

int dc3_put_int_to_db(char *sys, char* subsys, char *item, int itemlen, int array[])
{
	itemvalue itemvalueid, runindexid;
	int err;
	
	// Check that we have an open connection
	if(!mysql_dc3){
		cerr<<__FILE__<<":"<<__LINE__<<" ";
		cerr<<"Attempt to write to database without connection"<<endl;
		return -1;
	}

	// Write to the database
	err = WriteAndLinkConstantSet_int(mysql_dc3,sys,subsys,item
		,RUNNUMBER ,RUNNUMBER
		,COMMENT
		,itemlen ,array
		,&itemvalueid
		,CALDB_RUNINDEX
		,MINRUN ,MAXRUN
		,COMMENT
		,&runindexid);

	return err;
}

//----------------------------------------------
// Get a default value from the MySQL Database
//----------------------------------------------
char* GetDCDefault(char *key)
{
	static char value[255];
	value[0] = 0;

	// What we want here is to attempt to connect to the clasdb server. These
	// are not the same as calibration constants so we want to make sure all
	// values come from the same place. Hence, we make this connection
	// "by hand" and keep it separate from the caldb connection.
	static MYSQL *mysql_dcdefault = NULL;
	if(!mysql_dcdefault){
		mysql_dcdefault = (MYSQL*)malloc(sizeof(MYSQL));
		mysql_init(mysql_dcdefault);
		MYSQL *tmp = mysql_real_connect(mysql_dcdefault, "clasdb.jlab.org", "clasuser", NULL, "dc_settings", 0, NULL, 0);
		if(!tmp){
			free(mysql_dcdefault);
			mysql_dcdefault = NULL;
		}
	}
	if(!mysql_dcdefault)return value;
	
	// We must have a connection. Now query for the value
	char query[1024];
	sprintf(query,"SELECT * FROM dc3_defaults WHERE property=\"%s\" ORDER BY time DESC LIMIT 1",key);
	if(mysql_real_query(mysql_dcdefault, query, strlen(query))) { //query failed
		fprintf(stderr,mysql_error(mysql_dcdefault));
	}else{
		MYSQL_RES *mysql_res = mysql_store_result(mysql_dcdefault);
		if(mysql_res){     // query returned some rows
			int numRows = mysql_num_rows(mysql_res);
			if(numRows>0){
				MYSQL_ROW row = mysql_fetch_row(mysql_res);
				strcpy(value, row[1]);
			}
		}
	}

	return value;
}

//----------------------------------------------
// Get a default value from the MySQL Database
//----------------------------------------------
float GetDCDefaultFloat(char *key)
{
	return atof(GetDCDefault(key));
}

// =============================================================================
//       The following routines should eventually reside in libcaldbC.a
// =============================================================================


int WriteAndLinkConstantSet_float(
	MYSQL *conn, char *systemname,char *subsystemname,char *itemname,
	int minrunsource, int maxrunsource,
	char *calib_comment,
	int itemlength, float array[],
	itemvalue *itemvalueid,
	char *RunIndexTable,
	int minrun,int maxrun,
	char *runindex_comment,
	itemvalue *runindexid)
{
	int err;
	char *value;
	
	err = calib_float2value(itemlength, array, &value);

	if(err)return err;

	err = WriteAndLinkConstantSet(
		conn, systemname,subsystemname,itemname,
		minrunsource, maxrunsource,
		calib_comment,
		value,
		itemvalueid,
		RunIndexTable,
		minrun, maxrun,
		runindex_comment,
		runindexid);

	free(value);

	return err;
}

int WriteAndLinkConstantSet_int(
	MYSQL *conn, char *systemname,char *subsystemname,char *itemname,
	int minrunsource, int maxrunsource,
	char *calib_comment,
	int itemlength, int array[],
	itemvalue *itemvalueid,
	char *RunIndexTable,
	int minrun,int maxrun,
	char *runindex_comment,
	itemvalue *runindexid)
{
	int err;
	char *value;
	
	err = calib_int2value(itemlength, array, &value);

	if(err)return err;

	err = WriteAndLinkConstantSet(
		conn, systemname,subsystemname,itemname,
		minrunsource, maxrunsource,
		calib_comment,
		value,
		itemvalueid,
		RunIndexTable,
		minrun, maxrun,
		runindex_comment,
		runindexid);

	free(value);

	return err;
}












