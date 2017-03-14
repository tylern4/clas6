

#include "dc3.h"

#include <map_manager.h>

#ifdef SunOS
#include <strings.h> // I guess Sun decided to move bzero from string.h to strings.h ??
#endif

float write_to_map[7][7];
int WRITE_TIMEWALK_TO_MAP=0;

int ExpertCheck(void);
int TimewalkCheck(void);
int MapCheck(void);
void WriteToMap(void);
void UpdateComment(char *subsys,char *additional_comment);

//extern "C" {
//	char* crypt(char*,char*);
//}

//============================================================
// Entry point for writing calibration constants to map.
// This is called when the "Write to Map" button is pressed.
//============================================================
int MapWrite(ClientData clientData,Tcl_Interp *interp
   ,int argc, const char *argv[])
{
	int sec,sup;
	char varname[32];
	int err;

	// Get which secs/sups are to be written
	for(sec=1;sec<=6;sec++){
		for(sup=1;sup<=6;sup++){
			sprintf(varname,"s%dsl%d",sec,sup);
			write_to_map[sec][sup]=atoi(Tcl_GetVar(interp,varname,TCL_GLOBAL_ONLY));
		}
	}

	// Check if any values obtained in "expert" mode
	if(ExpertCheck())return TCL_OK;

	// Check if we need to write timewalk parameters
	if(TimewalkCheck())return TCL_OK;

	// Check format of the map file to be written to
	if(MapCheck())return TCL_OK;

	// Get Any additional comments
	Tcl_Evaluate("AdditionalComments");
	if(!strcmp(interp->result,"Cancel"))return TCL_OK;

	// Everything checks out. Go ahead and write to the map
	WriteToMap();

	return TCL_OK;
}


//==================================================================
// Check that the map file we're about to write to has the proper
// format. Also, if it is missing "comment" items, then add them.
//==================================================================
int MapCheck(void)
{
	char subsystem[256]="*";
	char item[256];
	int firsttime, olength;
	int array[36];
	float farray[36];
	int  xvst_item_exists[7][7]    , tmax_item_exists[7];
	int  xvst_comment_exists[7]    , tmax_comment_exists;
	int  timewalk_region_exists[4]    , timewalk_comment_exists;
	int  timewalk_factor_exists       , timewalk_tau_exists;
	int  timewalk_functiontype_exists , timewalk_betaslope_exists;
	char str[2048];
	int sec,sup,reg;
	int missing_sub=0,missing_comment=0,missing_tw_sub=0;
	int err=0;
	char mess[256],cmd[512];
	
	// Turn off printing map_manager warnings to the terminal
#ifndef CALDB
	map_log_mess(0,0);
#endif

	// Initialize arrays
	tmax_comment_exists=0;
	timewalk_factor_exists=0;
	timewalk_functiontype_exists=0;
	timewalk_comment_exists=0;
	timewalk_tau_exists=0;
	timewalk_betaslope_exists=0;
	for(reg=1;reg<=3;reg++)timewalk_region_exists[reg]=0;
	for(sec=1;sec<=6;sec++){
		tmax_item_exists[sec]=0;
		xvst_comment_exists[sec]=0;
		for(sup=1;sup<=6;sup++){
			xvst_item_exists[sec][sup]=0;
		}
	}

	// Check t_max subsystem
	strcpy(subsystem,"t_max");
	for(sec=1;sec<=6;sec++){
		sprintf(item,"Sector%d",sec);
		if(MAP_OK==map_get_float(MAPFILE,subsystem,item,36,farray,RUNNUMBER,&firsttime))
			tmax_item_exists[sec]=1;
	}
	if(MAP_OK==map_get_char(MAPFILE,subsystem,"comment",1024,str,RUNNUMBER,&firsttime,&olength))
		tmax_comment_exists=1;

	// Check xvst subsystems
	for(sup=1;sup<=6;sup++){
		sprintf(item,"SL%d",sup);
		for(sec=1;sec<=6;sec++){
			sprintf(subsystem,"xvst_par_Sect%d",sec);
			if(sec==1)strcpy(subsystem,"xvst_params");
			if(MAP_OK==map_get_float(MAPFILE,subsystem,item,24,farray,RUNNUMBER,&firsttime))
				xvst_item_exists[sec][sup]=1;
			if(MAP_OK==map_get_char(MAPFILE,subsystem,"comment",1024,str,RUNNUMBER,&firsttime,&olength))
				xvst_comment_exists[sec]=1;
		}
	}

	// Check Timewalk subsystem
	strcpy(subsystem,"Timewalk");
	for(reg=1;reg<=3;reg++){
		sprintf(item,"Region%d",reg);
		if(MAP_OK==map_get_float(MAPFILE,subsystem,item,10,farray,RUNNUMBER,&firsttime))
			timewalk_region_exists[reg]=1;
	}
	if(MAP_OK==map_get_float(MAPFILE,subsystem,"factor",3,farray,RUNNUMBER,&firsttime))
		timewalk_factor_exists=1;
	if(MAP_OK==map_get_float(MAPFILE,subsystem,"tau",3,farray,RUNNUMBER,&firsttime))
		timewalk_tau_exists=1;
	if(MAP_OK==map_get_int(MAPFILE,subsystem,"functiontype",1,array,RUNNUMBER,&firsttime))
		timewalk_functiontype_exists=1;
	if(MAP_OK==map_get_float(MAPFILE,subsystem,"betaslope",6,farray,RUNNUMBER,&firsttime))
		timewalk_betaslope_exists=1;
	if(MAP_OK==map_get_char(MAPFILE,subsystem,"comment",1024,str,RUNNUMBER,&firsttime,&olength))
		timewalk_comment_exists=1;

	// Compare that the required subsystems/items exist
	if(!timewalk_comment_exists)missing_comment=1;
	for(sec=1;sec<=6;sec++){
		for(sup=1;sup<=6;sup++){
			if(write_to_map[sec][sup]){
				if(!xvst_item_exists[sec][sup])missing_sub=1;
				if(!tmax_item_exists[sec]     )missing_sub=1;
				if(!xvst_comment_exists[sec]  )missing_comment=1;
				if(!tmax_comment_exists       )missing_comment=1;
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
	if(missing_sub){
		strcpy(mess,"The format of the chosen mapfile is incorrect.");
		strcat(mess,"The file selected should be called 'DC_DOCA.map'.");
		sprintf(cmd,"tk_dialog .badformat \"Bad mapfile format\" \"%s\" warning 0 \"Oops!\"",mess);
		Tcl_Evaluate(cmd);
		return -1;
	}

	// If only items in the Timewalk subsystem are missing, optionally
	// create them
	if(missing_tw_sub){
		strcpy(mess,"The chosen mapfile is missing some items in the Timewalk ");
		strcat(mess,"susbsystem which were recently added. Do you wish to add ");
		strcat(mess,"the items now? ");
		sprintf(cmd,"tk_dialog .badformat \"Add timewalk items\" \"%s\" warning 0 \"Yes\" \"No\"",mess);
		Tcl_Evaluate(cmd);
		if(atoi(interp->result)==0){

			for(reg=1;reg<=3;reg++){
				if(!timewalk_region_exists[reg]){
					sprintf(item,"Region%d",reg);
					err|=MAP_OK!=map_add_item(MAPFILE,subsystem,item,10,1);
				}
			}
			if(!timewalk_factor_exists      )err|=MAP_OK!=map_add_item(MAPFILE,subsystem,"factor",3,1);
			if(!timewalk_tau_exists         )err|=MAP_OK!=map_add_item(MAPFILE,subsystem,"tau",3,1);
			if(!timewalk_functiontype_exists)err|=MAP_OK!=map_add_item(MAPFILE,subsystem,"functiontype",1,0);
			if(!timewalk_betaslope_exists   )err|=MAP_OK!=map_add_item(MAPFILE,subsystem,"betaslope",6,1);
		}
	}

	// General format is OK. Check that comment items exist. If not, 
	// ask user before creating them.
	if(missing_comment){
		strcpy(mess,"The selected mapfile is missing some 'comment' items.");
		strcat(mess,"Do you wish to add them?");
		sprintf(cmd,"tk_dialog .nocomment \"No Comments\" \"%s\" warning 0 ",mess);
		strcat(cmd,"\"Add Comment item\" \"Abort Map write\"");
		Tcl_Evaluate(cmd);
		if(atoi(interp->result))return -2;

		// Create comment items
		if(!tmax_comment_exists){
			err|= (MAP_OK!=map_add_item(MAPFILE,"t_max","comment",1024,2));
			map_put_char(MAPFILE,"t_max","comment",1," ",RUNNUMBER);
		}
		for(sec=1;sec<=6;sec++){
			if(!xvst_comment_exists[sec]){
				sprintf(subsystem,"xvst_par_Sect%d",sec);
				if(sec==1)strcpy(subsystem,"xvst_params");
				err|= (MAP_OK!=map_add_item(MAPFILE,subsystem,"comment",1024,2));
				map_put_char(MAPFILE,subsystem,"comment",1," ",RUNNUMBER);
			}
		}
		if(!timewalk_comment_exists){
			err|= (MAP_OK!=map_add_item(MAPFILE,"Timewalk","comment",1024,2));
			map_put_char(MAPFILE,"Timewalk","comment",1," ",RUNNUMBER);
		}

		// Inform user of results of creating comment items
		if(err){
			strcpy(mess,"An error occurred while creating comment items. ");
			strcat(mess,"Check your write permissions or choose another map file.");
		}else{
			strcpy(mess,"Comment items successfully added to map file.");
		}
		sprintf(cmd,"tk_dialog .caddresult \"Add comments\" \"%s\" info 0 \"OK\"",mess);
		Tcl_Evaluate(cmd);
		if(err)return -3;
	}

	return 0;
}

//============================================================
// Entry point for creating a new map file
// This is called when the "Create Map File" button is pressed.
//============================================================
int CreateMapFile(ClientData clientData,Tcl_Interp *interp
   ,int argc, const char *argv[])
{
	int sec,sup;
	int err;
	char mess[256],cmd[512];
	char subsystem[256],item[256];
	int runnumber=1;

	// Liming values
	float tw_r1_liming[10]={ -9.45, 39.34, -15.03, 11.53, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
	float tw_r2_liming[10]={-17.34, 53.70, -23.76,  8.85, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
	float tw_r3_liming[10]={-24.10, 72.05, -33.42,  9.87, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};

	// Vipuli values (these need to be replaced wit h real Vipuli values)
	float tw_r1[10]={ 33.228, -24.701,  8.849, -2.912, 1.168, 36.886, -35.177, 16.552, -4.832, 1.334};
	float tw_r2[10]={ 89.072, -109.53, 42.027, -4.806, 1.075, 74.749, -89.640, 30.454, -1.434, 0.959};
	float tw_r3[10]={ 171.25, -211.79, 73.174, -6.003, 1.477, 102.05, -112.19, 29.206, 0.9799, 1.519};
	
	float tw_factor[3]={2.0, 2.0, 3.0};
	float tw_tau[3]={50.0, 70.0, 100.0};
	int   tw_functiontype=DC_TIMEWALK_TYPE_VIPULI;
	int   tw_functiontype_liming=DC_TIMEWALK_TYPE_LIMING;
	float tw_betaslope[6]={22.0, 22.0, 22.0, 22.0, 22.0, 22.0};

	float t_max[36]={	155.950, 157.666, 159.400, 161.153,       0,       0, 
							160.558, 162.324,  164.11, 165.915,  167.74, 169.585, 
							629.865, 636.793, 643.798,  650.88,  658.04, 665.278, 
							682.771, 689.599, 696.495,  703.46, 710.495, 717.600, 
							1153.91, 1166.61, 1179.44, 1192.41, 1205.53, 1218.79, 
							1306.97, 1320.04, 1333.24, 1346.57, 1360.04, 1373.64};


	float sig_r1[50]={1036.699951,788.200012,655.700012,522.599976,477.299988,
							408.700012,364.600006,349.100006,362.299988,320.500000,
							335.399994,302.799988,305.399994,308.200012,291.799988,
							265.899994,267.600006,266.299988,262.000000,250.300003,
							259.700012,244.199997,261.200012,262.399994,257.200012,
							243.500000,243.300003,243.300003,244.800003,245.699997,
							236.000000,221.800003,217.899994,225.100006,225.300003,
							225.800003,220.899994,207.500000,212.300003,202.800003,
							224.899994,223.800003,247.600006,273.600006,303.799988,
							335.799988,398.399994,487.899994,621.299988,846.500000};

	float sig_r2[50]={1075.800049,688.000000,608.400024,523.799988,442.500000,
							403.000000,361.000000,333.100006,304.200012,301.799988,
							268.700012,286.000000,252.699997,257.500000,255.199997,
							248.399994,244.800003,245.100006,233.199997,215.500000,
							219.199997,204.500000,224.300003,205.000000,212.000000,
							202.899994,221.899994,216.000000,228.000000,230.000000,
							226.899994,228.600006,261.200012,260.500000,257.000000,
							267.899994,251.000000,268.100006,260.200012,274.799988,
							347.000000,350.100006,376.799988,450.700012,517.900024,
							568.400024,648.400024,768.700012,948.599976,1159.599976};

	float sig_r3[50]={1158.800049,762.900024,555.000000,477.200012,409.500000,
							360.700012,313.600006,307.100006,310.100006,310.299988,
							287.799988,274.600006,256.200012,280.500000,296.100006,
							274.399994,276.500000,268.500000,231.000000,258.100006,
							225.199997,261.500000,245.300003,236.399994,253.800003,
							239.399994,238.699997,227.899994,265.899994,249.899994,
							275.100006,292.399994,287.200012,306.000000,324.200012,
							357.000000,349.299988,374.799988,387.700012,472.700012,
							453.100006,511.200012,542.500000,615.700012,672.500000,
							740.099976,871.400024,974.599976,1173.900024,1367.400024};


	float rms_r1[50]={ 0.084381  ,0.0807041 ,0.0803665 ,0.0760064 ,0.0719802 
							,0.0667794 ,0.0659661 ,0.0632236 ,0.0604758 ,0.0590896 
							,0.057358  ,0.0559005 ,0.0535067 ,0.0525853 ,0.0504444 
							,0.0497853 ,0.0480626 ,0.047839  ,0.0460757 ,0.0468155 
							,0.0458292 ,0.0474769 ,0.0472105 ,0.0481008 ,0.0478223 
							,0.0515618 ,0.0475871 ,0.0465182 ,0.0457941 ,0.0440293 
							,0.0418735 ,0.0425885 ,0.041167  ,0.0415381 ,0.0413567 
							,0.0399924 ,0.0413594 ,0.03888   ,0.0396589 ,0.0408851 
							,0.0446838 ,0.0457259 ,0.0462711 ,0.0455393 ,0.0476383 
							,0.0730622 ,0.0472179 ,0.0495858 ,0.0467318 ,0.0468712};

	float rms_r2[50]={ 0.0935029 ,0.0929236 ,0.0845817 ,0.0779169 ,0.0728323 
							,0.069555  ,0.0660365 ,0.0621443 ,0.0599556 ,0.0575761 
							,0.0538892 ,0.0522913 ,0.0505516 ,0.0500548 ,0.0478909 
							,0.0484243 ,0.0492542 ,0.0482391 ,0.048797  ,0.0494508 
							,0.0470079 ,0.047831  ,0.0485209 ,0.0489091 ,0.05132 
							,0.0470277 ,0.0459573 ,0.0450853 ,0.0441741 ,0.0455223 
							,0.0475216 ,0.0464617 ,0.0463844 ,0.0494103 ,0.0502738 
							,0.0517405 ,0.0554116 ,0.0625021 ,0.0654517 ,0.0707841 
							,0.0732791 ,0.0710402 ,0.0684037 ,0.0656162 ,0.0605946 
							,0.0573169 ,0.0575698 ,0.0620217 ,0.0710907 ,0.0826642};

	float rms_r3[50]={ 0.088265  ,0.0989662 ,0.0985844 ,0.0970182 ,0.0828674 
							,0.0743798 ,0.0670314 ,0.0626996 ,0.0595708 ,0.0523395 
							,0.051547  ,0.0513371 ,0.0525355 ,0.0572842 ,0.0591405 
							,0.0567289 ,0.0584834 ,0.0583518 ,0.0588804 ,0.0555678 
							,0.0601463 ,0.0594701 ,0.0652514 ,0.0552825 ,0.0557335 
							,0.057892  ,0.0654115 ,0.0612146 ,0.0585192 ,0.0554039 
							,0.0574585 ,0.059622  ,0.0596917 ,0.0568657 ,0.0587866 
							,0.0608153 ,0.0631117 ,0.0701809 ,0.0758597 ,0.0823053 
							,0.0882779 ,0.0956875 ,0.10299   ,0.0967239 ,0.0993666 
							,0.101219  ,0.100507  ,0.101938  ,0.10851   ,0.122973 };

	// Turn off printing map_manager warnings to the terminal
#ifndef CALDB
	map_log_mess(0,0);
#endif

	// Get Name/directory of file to create
	Tcl_Evaluate("tk_getSaveFile -initialfile \"DC_DOCA.map\" -title \"Create New Map File\"");
	if(!strlen(interp->result))return TCL_OK;

	// Create Map File and set it to be the current write destination
	if(MAP_OK!=map_create(interp->result)){

		strcpy(mess,"Unable to create a new mapfile. Make sure the file does ");
		strcat(mess,"NOT already exist and that you have write permission for ");
		strcat(mess,"the directory in which you are creating it.");
		sprintf(cmd,"tk_dialog .mfcerr \"Error creating Mapfile\" \"%s\" warning 0 \"OK\"",mess);
		Tcl_Evaluate(cmd);

		return TCL_OK;
	}
	strcpy(MAPFILE,interp->result);
	Tcl_UpdateLinkedVar(interp,"mapfile");

	// Create all the subsystems/items
	err=0;
	strcpy(subsystem,"t_max");
	for(sec=1;sec<=6;sec++){
		sprintf(item,"Sector%d",sec);
		err|=MAP_OK!=map_add_item(MAPFILE,subsystem,item,36,1);
	}

	// Create all xvst_parms subsystems/items
	for(sup=1;sup<=6;sup++){
		sprintf(item,"SL%d",sup);
		for(sec=1;sec<=6;sec++){
			sprintf(subsystem,"xvst_par_Sect%d",sec);
			if(sec==1)strcpy(subsystem,"xvst_params");
			err|=MAP_OK!=map_add_item(MAPFILE,subsystem,item,24,1);
		}
	}

	// Write an entry into tmax/xvst subsystems. (Apparently this must be done
	// AFTER all the items in the subsystem are created.)
	strcpy(subsystem,"t_max");
	for(sec=1;sec<=6;sec++){
		sprintf(item,"Sector%d",sec);
		err|=MAP_OK!=map_put_float(MAPFILE,subsystem,item,36,t_max,runnumber);
	}
	for(sup=1;sup<=6;sup++){
		sprintf(item,"SL%d",sup);
		for(sec=1;sec<=6;sec++){
			sprintf(subsystem,"xvst_par_Sect%d",sec);
			if(sec==1)strcpy(subsystem,"xvst_params");
			err|=MAP_OK!=map_put_float(MAPFILE,subsystem,item,24,USER_PARMS[sec][sup].parms.par,runnumber);
		}
	}

	// Create Timewalk subsystem/items
	err|=MAP_OK!=map_add_item(MAPFILE,"Timewalk","Region1",10,1);
	err|=MAP_OK!=map_add_item(MAPFILE,"Timewalk","Region2",10,1);
	err|=MAP_OK!=map_add_item(MAPFILE,"Timewalk","Region3",10,1);
	err|=MAP_OK!=map_add_item(MAPFILE,"Timewalk","factor" , 3,1);
	err|=MAP_OK!=map_add_item(MAPFILE,"Timewalk","tau"    , 3,1);
	err|=MAP_OK!=map_add_item(MAPFILE,"Timewalk","functiontype", 1,0);
	err|=MAP_OK!=map_add_item(MAPFILE,"Timewalk","betaslope"   , 6,1);

	// Write entries into Timewalk susbsystem since this is not normally
	// done when writing a calibration (at this time)
	err|=MAP_OK!=map_put_float(MAPFILE,"Timewalk","Region1",10,tw_r1,runnumber);
	err|=MAP_OK!=map_put_float(MAPFILE,"Timewalk","Region2",10,tw_r2,runnumber);
	err|=MAP_OK!=map_put_float(MAPFILE,"Timewalk","Region3",10,tw_r3,runnumber);
	err|=MAP_OK!=map_put_int(MAPFILE,"Timewalk","functiontype", 1,&tw_functiontype,runnumber);
	err|=MAP_OK!=map_put_float(MAPFILE,"Timewalk","betaslope" , 6,tw_betaslope,runnumber);

	// Always write Liming values into run 8774 too
	err|=MAP_OK!=map_put_float(MAPFILE,"Timewalk","Region1",10,tw_r1_liming,8774);
	err|=MAP_OK!=map_put_float(MAPFILE,"Timewalk","Region2",10,tw_r2_liming,8774);
	err|=MAP_OK!=map_put_float(MAPFILE,"Timewalk","Region3",10,tw_r3_liming,8774);
	err|=MAP_OK!=map_put_float(MAPFILE,"Timewalk","factor" , 3,tw_factor,8774);
	err|=MAP_OK!=map_put_float(MAPFILE,"Timewalk","tau"    , 3,tw_tau,8774);
	err|=MAP_OK!=map_put_float(MAPFILE,"Timewalk","factor" , 3,tw_factor,1);
	err|=MAP_OK!=map_put_float(MAPFILE,"Timewalk","tau"    , 3,tw_tau,1);
	err|=MAP_OK!=map_put_int(MAPFILE,"Timewalk","functiontype", 1,&tw_functiontype_liming,8774);

	// Create Sigma subsystem/items
	err|=MAP_OK!=map_add_item(MAPFILE,"Sigma","Region1",50,1);
	err|=MAP_OK!=map_add_item(MAPFILE,"Sigma","Region2",50,1);
	err|=MAP_OK!=map_add_item(MAPFILE,"Sigma","Region3",50,1);

	// Write entries into the Sigma subsystem
	err|=MAP_OK!=map_put_float(MAPFILE,"Sigma","Region1",50,sig_r1,runnumber);
	err|=MAP_OK!=map_put_float(MAPFILE,"Sigma","Region2",50,sig_r2,runnumber);
	err|=MAP_OK!=map_put_float(MAPFILE,"Sigma","Region3",50,sig_r3,runnumber);

	// Create RMS subsystem/items
	err|=MAP_OK!=map_add_item(MAPFILE,"RMS","Region1",50,1);
	err|=MAP_OK!=map_add_item(MAPFILE,"RMS","Region2",50,1);
	err|=MAP_OK!=map_add_item(MAPFILE,"RMS","Region3",50,1);

	// Write entries into the RMS subsystem
	err|=MAP_OK!=map_put_float(MAPFILE,"RMS","Region1",50,rms_r1,runnumber);
	err|=MAP_OK!=map_put_float(MAPFILE,"RMS","Region2",50,rms_r2,runnumber);
	err|=MAP_OK!=map_put_float(MAPFILE,"RMS","Region3",50,rms_r3,runnumber);

	// Alert the user if an error occured
	if(err){
		Tcl_SetResult(interp,"An error occured while creating the mapfile.",TCL_STATIC);
		return TCL_ERROR;
	}

	printf("Map created\n");

	// Fill in entries at current run number
	WRITE_TIMEWALK_TO_MAP=1;
	WriteToMap();
	WRITE_TIMEWALK_TO_MAP=0;
	
	return TCL_OK;
}


//=============================================
// Write all selected values to the map file.
//=============================================
void WriteToMap(void)
{
	int sec,sup,layer,i;
	char subsystem[256],item[256];
	int err,err_rem;
	int firsttime;
	float farray[36],*par;
	char cmd[512],mess[512];
	int wrote_something;
	int *ipar;

	// Write to Tmax subsystem : 
	// This is a little tricky. Since the t_max subsytem has
	// one item per sector which contains information for all
	// superlayers, we must read in the current values first,
	// modify them for the superlayers to be written, and then
	// write the array. 
	err=err_rem=0;
	wrote_something=0;
	strcpy(subsystem,"t_max");
	for(sec=1;sec<=6;sec++){
		sprintf(item,"Sector%d",sec);
		err|=MAP_OK!=map_get_float(MAPFILE,subsystem,item,36,farray,RUNNUMBER,&firsttime);
		for(sup=1;sup<=6;sup++){
			if(!write_to_map[sec][sup])continue;

			for(i=0;i<6;i++){
				layer=((sup-1)*6)+i;
				farray[layer]=USER_PARMS[sec][sup].tmax[layer];
			}
		}

		// Write the modified array back to the map
		err_rem|=MAP_OK!=map_rem_arr(MAPFILE,subsystem,item,RUNNUMBER);
		err|=MAP_OK!=map_put_float(MAPFILE,subsystem,item,36,farray,RUNNUMBER);
		wrote_something=1;
	}
	if(wrote_something)UpdateComment("t_max",NULL);

	// Write to the XvsT subsystems
	for(sec=1;sec<=6;sec++){
		sprintf(subsystem,"xvst_par_Sect%d",sec);
		if(sec==1)strcpy(subsystem,"xvst_params");
		wrote_something=0;
		for(sup=1;sup<=6;sup++){
			if(!write_to_map[sec][sup])continue;

			sprintf(item,"SL%d",sup);
			par=USER_PARMS[sec][sup].parms.par;
			err_rem|=MAP_OK!=map_rem_arr(MAPFILE,subsystem,item,RUNNUMBER);
			err|=MAP_OK!=map_put_float(MAPFILE,subsystem,item,24,par,RUNNUMBER);
			wrote_something=1;
		}
		if(wrote_something)UpdateComment(subsystem,NULL);
	}

	// Timewalk Subsystems
	// The timewalk parameters are handled a little differently in that
	// the same parameters should exist for all USER_PARMS[][] regardless
	// which sector/superlayer is indexed. Here, we write what is in S1SL1.
	if(WRITE_TIMEWALK_TO_MAP){
		err_rem|= (MAP_OK!=map_rem_arr(MAPFILE,"Timewalk","Region1",RUNNUMBER));
		par=&USER_PARMS[1][1].tw_parm_r1[1];
		err|= (MAP_OK!=map_put_float(MAPFILE,"Timewalk","Region1",10,par,RUNNUMBER));

		err_rem|= (MAP_OK!=map_rem_arr(MAPFILE,"Timewalk","Region2",RUNNUMBER));
		par=&USER_PARMS[1][1].tw_parm_r2[1];
		err|= (MAP_OK!=map_put_float(MAPFILE,"Timewalk","Region2",10,par,RUNNUMBER));

		err_rem|= (MAP_OK!=map_rem_arr(MAPFILE,"Timewalk","Region3",RUNNUMBER));
		par=&USER_PARMS[1][1].tw_parm_r3[1];
		err|= (MAP_OK!=map_put_float(MAPFILE,"Timewalk","Region3",10,par,RUNNUMBER));

		err_rem|= (MAP_OK!=map_rem_arr(MAPFILE,"Timewalk","functiontype",RUNNUMBER));
		ipar=&USER_PARMS[1][1].tw_functiontype;
		err|= (MAP_OK!=map_put_int(MAPFILE,"Timewalk","functiontype",1,ipar,RUNNUMBER));

		err_rem|= (MAP_OK!=map_rem_arr(MAPFILE,"Timewalk","betaslope",RUNNUMBER));
		par=&USER_PARMS[1][1].tw_betaslope[1];
		err|= (MAP_OK!=map_put_float(MAPFILE,"Timewalk","betaslope",6,par,RUNNUMBER));
	     
		switch(USER_PARMS[1][1].tw_functiontype){
			case DC_TIMEWALK_TYPE_LIMING:
				err_rem|= (MAP_OK!=map_rem_arr(MAPFILE,"Timewalk","factor",RUNNUMBER));
				par=&USER_PARMS[1][1].tw_fact[1];
				err|= (MAP_OK!=map_put_float(MAPFILE,"Timewalk","factor",3,par,RUNNUMBER));

				err_rem|= (MAP_OK!=map_rem_arr(MAPFILE,"Timewalk","tau",RUNNUMBER));
				par=&USER_PARMS[1][1].tw_tau[1];
				err|= (MAP_OK!=map_put_float(MAPFILE,"Timewalk","tau",3,par,RUNNUMBER));
				break;
			case DC_TIMEWALK_TYPE_VIPULI:
				err_rem|= (MAP_OK!=map_rem_arr(MAPFILE,"Timewalk","betaslope",RUNNUMBER));
				par=&USER_PARMS[1][1].tw_betaslope[1];
				err|= (MAP_OK!=map_put_float(MAPFILE,"Timewalk","betaslope",6,par,RUNNUMBER));
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

}


/*******************************************************/
/* This will check for comments already in the map for */
/* this subsystem and run. If comments exist, then a   */
/* comment for this time will be appended (assuming    */
/* there is room, if not, the oldest comments will be  */
/* discarded until room is made. If the "comment" item */
/* doesn't exist, then it will be created.             */
/*******************************************************/
void UpdateComment(char *subsys,char *additional_comment)
{
   int err;
   char item[32]="comment";
   char comment[2*MAX_COMMENT_SIZE];
   char *ptr,str[MAX_COMMENT_SIZE];
   int firsttime,olength;
   time_t t;
   char *LOGNAME,*HOST;
	char mess[512],cmd[512];
	char dclib_versionstr[32];
	char dch_dclib_versionstr[32];
	char dch_dc3_versionstr[32];
	
   /* Get current comment from the Map */
   bzero(comment,2*MAX_COMMENT_SIZE);
   err=map_get_char(MAPFILE,subsys,item
      ,MAX_COMMENT_SIZE
      ,comment
      ,RUNNUMBER
      ,&firsttime
      ,&olength);

   
   /* append comment for current */
   LOGNAME=getenv("LOGNAME");
   HOST=getenv("HOST");
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
   sprintf(str,":::%suser:%s\nexecution host:%s\ndc3 %s\ndclib %s\ndc.h %s(dclib) %sdc3\n"
      ,ctime(&t)
      ,LOGNAME ? LOGNAME:"unknown"
      ,HOST    ?    HOST:"unknown"
      ,VERSIONSTR
      ,dclib_versionstr
      ,dch_dclib_versionstr
      ,dch_dc3_versionstr);
   strcat(comment,str);
      
   if(additional_comment){
      strcat(comment,"\n");
      strcat(comment,additional_comment);   
   }
   if(MAPCOMMENT){ /* From GUI */
      strcat(comment,"\n");
      strcat(comment,MAPCOMMENT);
   }
   strcat(comment,"\n");
   
   
   /* make sure comment array size is within limits */
   ptr=comment;
   while(strlen(ptr)>MAX_COMMENT_SIZE){
      ptr=strstr(++ptr,":::");
      if(ptr==NULL){
			strcpy(mess,"Error in generating comment (size too big?)");
			sprintf(cmd,"tk_dialog .mfcerr \"Error in comment\" \"%s\" warning 0 \"OK\"",mess);
			Tcl_Evaluate(cmd);		
         return;
      }
   }
   
   // remove old comment and insert new comment
   map_rem_arr(MAPFILE,subsys,item,RUNNUMBER);
   map_put_char(MAPFILE,subsys,item,strlen(ptr),ptr,RUNNUMBER);
}



















