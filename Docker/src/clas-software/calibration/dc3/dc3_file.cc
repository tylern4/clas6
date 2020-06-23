
#include "dc3.h"

#include <mysql.h>
extern "C" {
#include <calib_connect.h> // for caldb
#undef CALDB_HOST
#undef CALDB_USER
#undef CALDB_PASSWORD
}



char *FILENAME=NULL;
int FILE_ATTACHED=0;
int HBOOK_HAS_PARMS=0;
char PROGRESS_MESSAGE[256];
nt_field_t NTUPLE_FIELD[MAX_NTUPLE_FIELDS];
int NUM_NTUPLE_FIELDS=0;
char NTUPLE_FIELD_NAMES[][32]={
    "sector", "layer", "wire", "time", "doca", "resi"
   ,"B", "B1", "B2", "beta", "phi", "locangle", "P", "q"
   ,"chisq","hit","DC1","calcdoca","ctime"};

void Read_Map_parms_from_hbook_file(void);
void Ntuple_Error_Message(char *field,char *wanted,int index);

void* dc_fill_t2x_table_thread(void *arg);
void* dc_xvst_init_thread(void *arg);
void* Read_Ntuple_thread(void *arg);

// function declarations for external routines in libdc.a
extern "C" {
void dc_set_def_(void);
void dc_fill_t2x_table_(void);
void dc_time_to_dist_(int *sec,int *layer,int *wire,float *beta,
   float *locangle,float *t, float* B, float* B2,float *dist,float *sigma);
}

//========================
// Attach an HBOOK file
//========================
int OpenFile(ClientData clientData,Tcl_Interp *interp,int argc, const char *argv[])
{
   char str[256],cmd[256];
   int i, ISTAT=0;
   char CHTOP[9]="LUN1    ";
   char *ptr;
   static int initialized=0;
   int sec,sup;
   pthread_t progress_thread;
   float progress=0.0;
   int err;
   
   // Initialize
   if(!initialized){
      initialized=1;
   }
   
   /* get name of file to open */
   if(argc>1){
      strcpy(FILENAME,argv[1]);
   }else{
      Tcl_Evaluate("tk_getOpenFile");
      strcpy(FILENAME,interp->result);
      sprintf(str,"set filename \"%s\"",FILENAME);
      Tcl_Evaluate(str);
   }
   Tcl_UpdateLinkedVar(interp,"filename");
   if(strlen(FILENAME)==0)return TCL_OK;
   
   /* get run number info from name */
   strcpy(str,FILENAME);
   while(ptr=strstr(str,"/"))strcpy(str,++ptr);
   for(i=0;i<strlen(str);i++)if(isdigit(str[i]))break;
   sscanf(&str[i],"%d",&RUNNUMBER);
   MINRUN=MAXRUN=RUNNUMBER;
	Tcl_UpdateLinkedVar(interp,"runnumber");
	Tcl_UpdateLinkedVar(interp,"minrun");
	Tcl_UpdateLinkedVar(interp,"maxrun");
	Tcl_Evaluate("UpdateMinMaxRun minrun $minrun");
	Tcl_Evaluate("UpdateMinMaxRun maxrun $maxrun");
   
   /* close any file currently open */
   if(FILE_ATTACHED)hrend(CHTOP);
   
   /* open file */
   cout<<"\nAttaching file:\n"<<FILENAME<<"\n\n";
   hropen(1,CHTOP,FILENAME," ",1024,ISTAT);
   if(ISTAT!=0){
      cout<<"Unable to open file. This may be because HROPEN() forces\n";
      cout<<"filenames to be all lower case and their are directories\n";
      cout<<"in the full path to the file which contain capital letters.\n";
      cout<<"Filename:\n"<<FILENAME<<"\n";
      return TCL_OK;
   }
   
   FILE_ATTACHED=1;
   
   //-------------- read in values from database ----------------
   if(FAST_STARTUP)
		cout<<"Skipping dc_xvst_init_() for fast startup. BE CAREFUL!!"<<endl;
	else
   	cout<<"Reading database for run "<<RUNNUMBER<<"\n";
  
   dc_set_def_();

	// Check that we can connect to database. The
	// dc package's initialization routine will
	// make calls to the map emulator which will
	// kill the program if a connection cannot be 
	// established. We will user 
	do{
		MYSQL *mysql;

		if(FAST_STARTUP)break; // don't read parms from database if FAST_STARTUP flag is set

		// connect using the username and password the emulator will use
		cerr<<"Testing database connection..."; cerr.flush();
		mysql = ConnectToServer(CALDB_HOST,"calib","clasuser","");
		if(mysql){
			DisconnectFromServer(mysql);
			err=0;
			cerr<<"OK"<<endl;
		}else{
			char cmd[768],mess[768];
		
			err=-1;
			strcpy(mess,"Error connecting to the database.");
			strcat(mess,"Check that the HOST and RunIndex parameters are set correctly ");
			strcat(mess,"in the following window. ");
			strcat(mess,"(The default username and password will be used here. The username ");
			strcat(mess,"and password you specify will only be used when writing to the database.)");
			sprintf(cmd,"tk_dialog .dberror \"DB connection Error\" \"%s\" warning 0 ",mess);
			strcat(cmd,"\"Ja Voll\"");
			Tcl_Evaluate(cmd);
			cerr<<"failed"<<endl;
			Tcl_Evaluate("ModifyDBInfo");
			if(MDBI_RESULT==1)continue;
			break;
		}

		// Copy the correct parameters for connecting to the database
		// into the environment where the map emulator will find them
		sprintf(str,"CLAS_CALDB_HOST=%s",CALDB_HOST);			putenv(strdup(str));
		sprintf(str,"CLAS_CALDB_PASSWORD=%s","");					putenv(strdup(str));
		sprintf(str,"CLAS_CALDB_RUNINDEX=%s",CALDB_RUNINDEX);	putenv(strdup(str));

		cout<<"CLAS_CALDB_HOST="<<getenv("CLAS_CALDB_HOST")<<"("<<CALDB_HOST<<")"<<endl;
		cout<<"CLAS_CALDB_RUNINDEX="<<getenv("CLAS_CALDB_RUNINDEX")<<"("<<CALDB_RUNINDEX<<")"<<endl;

		// Launch thread to allow screen updates in Tcl
		progress=0.0;
		DC_FILL_XVST_TABLE=0; /* don't fill the large tables here (only when recalculating residuals */
		pthread_create(&progress_thread,NULL,dc_xvst_init_thread,(void*)&progress);

		// Draw progress clock on GUI since this may take a while
		strcpy(PROGRESS_MESSAGE,"initializing...");
		ProgressClock((void*)&progress);
		pthread_join(progress_thread,NULL);
		DC_FILL_XVST_TABLE=1;
	}while(err);

	// Copy map paramters into dc3 MAP_PARMS object array
   for(sec=1;sec<=6;sec++)
      for(sup=1;sup<=6;sup++)
         CopyParmsFromDC(&MAP_PARMS[sec][sup],sec,sup);


   /* Check if this file has a MAP directory with parms */
   Read_Map_parms_from_hbook_file();

   /* If the HBOOK file has parms in it, use them to fill the timewalk table                  */
   /* overwriting the values calculated using the map parms in the dc_fill_t2x_table_thread() */
	if(HBOOK_HAS_PARMS){
		for(sec=0;sec<=6;sec++)
      	for(sup=1;sup<=6;sup++)
         	CopyParmsToDC(&FILE_PARMS[sec][sup],sec,sup);
		dc_fill_timewalk_table();
   }
   
   /* clear any histograms or ntuples from memory */
   hcdir("//PAWC"," ");
   hdelet(0);
   Book_Histograms();
   
   /* All sectors/superlayers will need to be re-read */
   for(sup=0;sup<=6;sup++){
   	DATA_FILTER[sup].stale=1;
   	DATA_FILTER[sup].sup=sup;
   	DATA_FILTER[sup].sec=0;
   }
      

   /* Read info about Ntuple fields */
   Get_Ntuple(1000+SUP);

	// initialize filter
   Tcl_GlobalEval(interp,"$filter_path.lbf.lb delete 0 end");
	for(i=0;i<NUM_NTUPLE_FIELDS;i++){
   	sprintf(cmd,"$filter_path.lbf.lb insert end \" %s\"",NTUPLE_FIELD_NAMES[i]);
   	Tcl_GlobalEval(interp,cmd);
		((float*)&ISCUT)[i]=0.0;
		((float*)&CUTMAX)[i]=NTUPLE_FIELD[i].max;
		((float*)&CUTMIN)[i]=NTUPLE_FIELD[i].min;
	}

   return TCL_OK;
}

void Read_Map_parms_from_hbook_file(void)
{
   int NID,ICYCLE=999,IDN=0,IERR,IEVENT=0;
   int i;
   int sec,sup;
   int SUP,SEC;
   float X[50];
   float tmax;
   float tmp;
   char mess[1024],cmd[1024];
   
   hcdir("//PAWC"," ");
   hcdir("//LUN1/MAP"," ");
   
   /*-- The histograms could be ICYCLE=1 or ICYCLE=2 --*/
   NID=0;
   hrin(NID,ICYCLE,0); //this should load ALL histograms in the directory into memory
   NID = 1000;
   if(!hexist(NID)){
     ICYCLE++;
     hrin(NID,ICYCLE,0);
   }

   /*-- Disable the option if the info is not here --*/
   if(!hexist_(&NID)){
      //Tcl_Evaluate(".xvst.src.fil configure -state disabled");
      HBOOK_HAS_PARMS=0;
      return;
   }
   HBOOK_HAS_PARMS=1;
   
	// Check if Timewalk parameters are present in hbook file
   NID=4001;
   if(!hexist_(&NID)){
		strcpy(mess,"This file is missing the timewalk parameters. It was probably made ");
		strcat(mess,"by an older version of trk_mon. It is strongly reccomended that you ");
		strcat(mess,"update your version of trk_mon and reproduce the hbook file. ");
		sprintf(cmd,"tk_dialog .timewalkmissing \"Timewalk parameters missing\" \"%s\" warning 0 ",mess);
		strcat(cmd,"\"Quit\" \"I'll chance it\"");
		Tcl_Evaluate(cmd);
		if(atoi(interp->result)==0){
			Tcl_Evaluate(".top.quit invoke");
			return;
		}else{
			strcpy(mess,"It's your funeral.");
			sprintf(cmd,"tk_dialog .daring \"Good Luck Sucker!\" \"%s\" warning 0 ",mess);
			strcat(cmd,"\"Courage is my middle name\"");
			Tcl_Evaluate(cmd);
		}
	}

   /*-- read in timewalk parameters --*/
   hunpak(4001,&FILE_PARMS[0][0].tw_parm_r1[1],"HIST",0);
   hunpak(4002,&FILE_PARMS[0][0].tw_parm_r2[1],"HIST",0);
   hunpak(4003,&FILE_PARMS[0][0].tw_parm_r3[1],"HIST",0);
   hunpak(4004,&FILE_PARMS[0][0].tw_fact[1],"HIST",0);
   hunpak(4005,&FILE_PARMS[0][0].tw_tau[1],"HIST",0);
   hunpak(4007,&FILE_PARMS[0][0].tw_betaslope[1],"HIST",0);

	// TW function type
   tmp=-1;
   hunpak(4006,&tmp,"HIST",0);
   if(tmp>=0.0 && tmp<(float)NUM_DC_TIMEWALK_TYPES)FILE_PARMS[0][0].tw_functiontype=(int)tmp;
	// If the timewalk type was entered on the command line, use it 10/28/2007 D.L.
	if(TW_TYPE_COMMAND_LINE>=0.0)FILE_PARMS[0][0].tw_functiontype = TW_TYPE_COMMAND_LINE;
   TW_TYPE=FILE_PARMS[0][0].tw_functiontype;
   Tcl_UpdateLinkedVar(interp,"tw_type");

	/* Get run number from file */
	// The following was commented out because it was overwriting the
	// run number used for dc_xvst_init_ and causing failures in the
	// database check for e1f.
	// August, 20, 2007  D.L.
   //tmp=-1;
   //hunpak(5000,&tmp,"HIST",0);
   //if(tmp>7000.0 && tmp<500000.0)RUNNUMBER=(int)tmp;

   /*-- read in xvst and tmax paramters --*/
   for(sec=1;sec<=6;sec++){
      
      NID=2000;
      hgn(NID,&IDN,sec,X,&IERR);
      if(IERR)printf("Error reading Map ntuple %d row %d\n",NID,sec);
      SEC=(int)X[0];
      for(sup=1;sup<=6;sup++){
         for(i=0;i<36;i++)FILE_PARMS[SEC][sup].tmax[i]=X[i+1];
      }
      
      for(sup=1;sup<=6;sup++){
      
         /*-- read in xvst params --*/
         NID=1000;
         IEVENT++;
         hgn_(&NID,&IDN,&IEVENT,X,&IERR);
         if(IERR)printf("Error reading Map ntuple %d row %d\n",NID,IEVENT);
         
         SEC=(int)X[0];
         SUP=(int)X[1];
         for(i=0;i<24;i++)FILE_PARMS[SEC][SUP].parms.par[i]=X[i+2];
         
         /*-- initialize fit results --*/
         FILE_PARMS[SEC][SUP].chisq         =0.0;
         FILE_PARMS[SEC][SUP].DOF           =0  ;
         FILE_PARMS[SEC][SUP].POINTS_IN_FIT =0  ;
         FILE_PARMS[SEC][SUP].expert_mode   =0  ;
         
         /*-- copy timewalk parameters --*/
         for(i=1;i<=10;i++){
            FILE_PARMS[SEC][SUP].tw_parm_r1[i]=FILE_PARMS[0][0].tw_parm_r1[i];
            FILE_PARMS[SEC][SUP].tw_parm_r2[i]=FILE_PARMS[0][0].tw_parm_r2[i];
            FILE_PARMS[SEC][SUP].tw_parm_r3[i]=FILE_PARMS[0][0].tw_parm_r3[i];
         }
         for(i=1;i<=3;i++){
            FILE_PARMS[SEC][SUP].tw_fact[i]=FILE_PARMS[0][0].tw_fact[i];
            FILE_PARMS[SEC][SUP].tw_tau[i]=FILE_PARMS[0][0].tw_tau[i];
         }
			for(i=1;i<=6;i++){
				FILE_PARMS[SEC][SUP].tw_betaslope[i]=FILE_PARMS[0][0].tw_betaslope[i];
			}
			FILE_PARMS[SEC][SUP].tw_functiontype=FILE_PARMS[0][0].tw_functiontype;

         /*-- initialize fit_parms with file_parms --*/
         FILE_PARMS[SEC][SUP].sector=SEC;
         FILE_PARMS[SEC][SUP].superlayer=SUP;
         MAP_PARMS[SEC][SUP].sector=SEC;
         MAP_PARMS[SEC][SUP].superlayer=SUP;

			/* Make sure parms area tmax is aligned with tmax array. We keep       */
			/* tmax + tzero in the xvst_parms arrays. This is because tmax+tzero   */
			/* corresponds to a certain fraction of the uncorrected time histogram */
			/* which is what the tmax finder is based on.                          */
			FILE_PARMS[SEC][SUP].parms.p.tmax = 
				FILE_PARMS[SEC][SUP].tmax[EXCLUDED[SUP]-1] + FILE_PARMS[SEC][SUP].parms.p.tzero;

         USER_PARMS[SEC][SUP]=FILE_PARMS[SEC][SUP];

         /* Fill "all sectors" area as well */
         USER_PARMS[0][SUP]=FILE_PARMS[0][SUP]=FILE_PARMS[1][SUP];

      }
   }
   
   
}

/******************************************/
// Find the indices for the ntuple fields 
/******************************************/
void Get_Ntuple(int NID)
{
   int i,j,NVAR;
   char CHTAG[MAX_NTUPLE_FIELDS][16];
   char CHTITL[256];
   float NT_LOW[MAX_NTUPLE_FIELDS];
   float NT_HIGH[MAX_NTUPLE_FIELDS];
   if(NID>1006||NID<1001)return;
   char cmd[256];
   
   /*-- Clear all other ntuples from memory --*/
   for(i=1001;i<=1006;i++){
      if(i==NID)continue;
      if(hexist(NID))hdelet(NID);
   }

   hcdir("//LUN1/tbt/proton"," ");

   /*-- read ntuple into memory --*/
   if(!hexist(NID))hrin(NID,999,0);
   
   /*-- get info about ntuple fields --*/
   memset(CHTITL,' ',8);
   NVAR=MAX_NTUPLE_FIELDS;
   memset(CHTAG,' ',MAX_NTUPLE_FIELDS*16);
   hgiven(NID,CHTITL,&NVAR,(char*)CHTAG,NT_LOW,NT_HIGH);
   if(NVAR==0){
      cerr<<"Ntuple "<<NID<<" empty! Make sure this Ntuple\n";
      cerr<<"exists in tbt/proton.\n";
      return; 
   }
   if(NVAR>MAX_NTUPLE_FIELDS){
      cerr<<"\n\nNumber of Ntuple fields exceeds "<<MAX_NTUPLE_FIELDS<<"\n";
      cerr<<"You need to change \"MAX_NTUPLE_FIELDS\" in\n";
      cerr<<"dc3.h and recompile to use all fields.\n";
      NVAR=MAX_NTUPLE_FIELDS;
   }


   /*-- store the information in data structures --*/
   NUM_NTUPLE_FIELDS=0;
   for(i=0;i<NVAR;i++){
      j=16;
      while(CHTAG[i][--j]==' ')CHTAG[i][j]=0;
      strcpy(NTUPLE_FIELD[NUM_NTUPLE_FIELDS].name,CHTAG[i]);
      NTUPLE_FIELD[NUM_NTUPLE_FIELDS].pos=i+1;
      NTUPLE_FIELD[NUM_NTUPLE_FIELDS].min=NT_LOW[i];
      NTUPLE_FIELD[NUM_NTUPLE_FIELDS].max=NT_HIGH[i];
      /*---- Check that the fields are in the standard order ----*/
      if(!strcmp(NTUPLE_FIELD[NUM_NTUPLE_FIELDS].name,"fitdoca"))
      	strcpy(NTUPLE_FIELD[NUM_NTUPLE_FIELDS].name,"doca"); //backwards compatibility
      if(strcmp(NTUPLE_FIELD_NAMES[NUM_NTUPLE_FIELDS]
         		,NTUPLE_FIELD[NUM_NTUPLE_FIELDS].name)){
               Ntuple_Error_Message(NTUPLE_FIELD[NUM_NTUPLE_FIELDS].name
                  ,NTUPLE_FIELD_NAMES[NUM_NTUPLE_FIELDS]
                  ,NUM_NTUPLE_FIELDS);
      }

      NUM_NTUPLE_FIELDS++;
   }
   
}

//=============================================
// Display error message about Ntuple format
//=============================================
void Ntuple_Error_Message(char *field,char *wanted,int index)
{
   char mess[256]=" Error in the format of the Ntuple! \
Found field '%s' when looking \
for '%s' at index %d . This \
file is unusable by this program. \
Please contact davidl@jlab.org \
for assistance.";
   
   char str[256],cmd[256];

   sprintf(str,mess,field,wanted,index);
   sprintf(cmd
   	,"tk_dialog .ntuple_error \"Bad Ntuple Format\" \"%s\" warning 0 \"OK\""
   	,str);
   Tcl_Evaluate(cmd);
}

//=============================================================
// Scan all events in the Ntuple for superlayer "sup"
// and fill histograms for all sectors using the
// filter "filter". This routine really just sets some
// things up. The real work is done in Read_Ntuple_Thread()
// below. A thread is used so that the main thread can
// update the progress bar on the Tcl/Tk command window.
//=============================================================
void Read_Ntuple(filter_t filter)
{
   int i,sec;
   pthread_t progress_thread;
   float progress=0.0;

   // Here, we want to do all the time corrections explictily
   program_is_dc_calib_check=0;
   do_tzero_subtraction=1;
   do_timewalk_correction=1;

   // Setup for residual recalculation
   if(filter.recalculated_residuals){
      for(sec=1;sec<=6;sec++)CopyParmsToDC(&USER_PARMS[sec][filter.sup],sec,filter.sup);
      if(filter.recalculate_method==1){

			// Launch thread to fill the t2x tables
			progress=0.0;
         pthread_create(&progress_thread,NULL,dc_fill_t2x_table_thread,(void*)&progress);

         // Draw progress clock on GUI since this may take a while
         strcpy(PROGRESS_MESSAGE,"filling table...");
         ProgressClock((void*)&progress);
         pthread_join(progress_thread,NULL);
         //do_tzero_subtraction=0;
         //do_timewalk_correction=0;
      }
   }


   // Set the state flags in the data filter
	filter.progress=0.0;

	// Launch the Ntuple reading thread
	pthread_create(&progress_thread,NULL,Read_Ntuple_thread,(void*)&filter);

   // Draw progress bar on GUI since this may take a while
   strcpy(PROGRESS_MESSAGE,"working...");
   ProgressBar(&filter.progress);
   pthread_join(progress_thread,NULL);
   
   // Remember filter settings in global data structure
   DATA_FILTER[filter.sup]=filter;

}

//======================================================
// This just calls dc_xvst_init_() in the dc
// package. It is wrapped in a thread compliant routine
// so that Tcl can use the main thread to update the
// progress clock.
//======================================================
void* dc_xvst_init_thread(void *arg)
{
	int save_dc_silent=DC_SILENT;

	DC_SILENT=0;
	dc_xvst_init_(&RUNNUMBER);
	DC_SILENT=save_dc_silent;

	*((float*)arg)=1.0;
	pthread_exit(NULL);
}

//======================================================
// This just calls dc_fill_xvst_table() in the dc
// package. It is wrapped in a thread compliant routine
// so that Tcl can use the main thread to update the
// progress clock.
//======================================================
void* dc_fill_t2x_table_thread(void *arg)
{
	int save_dc_silent=DC_SILENT;

	DC_SILENT=0;
	dc_fill_xvst_table();
	DC_SILENT=save_dc_silent;

	*((float*)arg)=1.0;
	pthread_exit(NULL);
}

//======================================================
// This does the real work of reading through the ntuple
// and filling the histograms.
// It is wrapped in a thread compliant routine
// so that Tcl can use the main thread to update the
// progress bar.
//======================================================
void* Read_Ntuple_thread(void *arg)
{
	int i,j;
	filter_t *filter=(filter_t*)arg;
	int sec,layer,wire=100,sup;
	int N,NID;
   int err;
   ntuple_row_t row;
   float *X=&row.sector;
   float t,tmax,dist,sigma,locangle;
   int RID,XID,TID,RSID,XSID,RPID,RFID,XFID,TNID;
	ntuple_row_t N_avg[7];
	int ICYCLE=5;
	float hist[300];
	char CHTITL[81];
	int HID,NX,NY,NWT,LOC;
	float XMI,XMA,YMI,YMA,x;
	
   // First, zero out all the histograms
   RID=100+filter->sup;
   XID=RID+100;
   TID=RID+200;
   RSID=RID+300;
   XSID=RID+400;
   RPID=RID+500;
   RFID=RID+600;
   XFID=RID+700;
   TNID=RID+800;

   for(sec=0;sec<=6;sec++){
      hreset(RID ," ");		hreset(RID +(sec*10)," ");
      hreset(XID ," ");		hreset(XID +(sec*10)," ");
      hreset(TID ," ");		hreset(TID +(sec*10)," ");
      hreset(RSID," ");		hreset(RSID+(sec*10)," ");
      hreset(XSID," ");		hreset(XSID+(sec*10)," ");
      hreset(RPID," ");		hreset(RPID+(sec*10)," ");
      hreset(RFID," ");		hreset(RFID+(sec*10)," ");
      hreset(XFID," ");		hreset(XFID+(sec*10)," ");
      hreset(TNID," ");		hreset(TNID+(sec*10)," ");

		// Reset averages
		for(i=0;i<sizeof(ntuple_row_t)/sizeof(float);i++){
			X[i]=0.0; 
			((float*)&N_avg[sec].sector)[i]=0.0;
			((float*)&AVG[sec][filter->sup].sector)[i]=0.0;
		}
   }

	// Auto find limits on time.
   // Here we use the histograms created by trk_mon that plot
   // Fit DOCA vs. DTIME. This is much quicker than going through
   // the Ntuple. In order to extract the one dimensional dtime
   // histograms, we create projections onto the x-axis and call
   // hfilpr(). The hfilpr() routine does not appear to be documented
   // in the HBOOK manual and I had to figure it out by running
   // a debugger on pawX11 and interupting it at the right time.
   //   DumpFilter(filter);
   if(filter->std_tcut && filter->autocut_t){
   	hcdir("//LUN1"," ");
   	for(sec=1;sec<=6;sec++){
   		HID=(10000*sec)+(1000*filter->sup)+2;
			hrin(HID,ICYCLE,0);
			hgive(HID,CHTITL,&NX,&XMI,&XMA,&NY,&YMI,&YMA,&NWT,&LOC);
			hbprox(HID,0.0);
			hfilpr(HID);
			hunpak(HID,hist,"PROX",0);
			hdelet(HID);

			// Integrate and Find point at integral fraction=AUTOCUT_T
			for(i=1;i<NX;i++) hist[i]+=hist[i-1];
			for(i=0;i<NX-1;i++)if(((float)hist[i]/hist[NX-1])>AUTOCUT_T_FRAC)break;
			x=(((float)i)+0.5)*(XMA-XMI)/(float)NX;
			STD_TCUT_HI[sec][filter->sup]=x;
			STD_TCUT_LOW[sec][filter->sup]=TLIM_AUTO_LOW[SUP];
			//STD_TCUT_LOW[sec][filter->sup]=0.05*STD_TCUT_HI[sec][filter->sup];
		}
		hcdir("//LUN1/tbt/proton"," ");
   }
   if(filter->std_tcut && !filter->autocut_t){
   	for(sec=1;sec<=6;sec++){
			STD_TCUT_HI[sec][filter->sup]=TLIM_AUTO_HI[SUP];
			STD_TCUT_LOW[sec][filter->sup]=TLIM_AUTO_LOW[SUP];
		}
   }

	// Make sure DC parms area is aligned with USER_PARMS[][] for time corrections
	for(sec=1;sec<=6;sec++)
		for(sup=1;sup<=6;sup++)
			CopyParmsToDC(&USER_PARMS[sec][sup],sec,sup);
   
   // Use average std time cuts for the "all" sector in expert mode
   STD_TCUT_HI[0][filter->sup]=STD_TCUT_LOW[0][filter->sup];
   for(sec=1;sec<=6;sec++){
		STD_TCUT_HI[0][filter->sup]+=STD_TCUT_HI[sec][filter->sup]/6.0;
		STD_TCUT_LOW[0][filter->sup]+=STD_TCUT_LOW[sec][filter->sup]/6.0;
   }

   // Read in Ntuple and set it up for looping over events
	NID=1000+filter->sup;
   Get_Ntuple(NID);
   hnoent(NID,&N);
   hgnpar(NID,"Read_Ntuple");

   // Loop over events
   for(i=0;i<N;i++){

      // read in next event
      hgnf(NID,i+1,X,&err);
      filter->progress=(float)i/(float)(N-1);
      
      if (row.DC1<0 || row.hit!=0){
			//the status markers for this event are not good, 
			//so it will not be used to fill the histos.
			continue;
      }


      // Do time correction
      sec = (int)row.sector;
      if(sec<1 || sec>6){
			printf("bad sector value in ntuple (%d)  %s:%d\n",sec,__FILE__,__LINE__);
      }
      filter->sec=sec;
      t=row.time;
      if(filter->used_corrected_time)
      	t=dc_time_correction_(&row.time,&excluded[filter->sup],&sec,&row.beta);

      // Recalculate residual if neccessary
      if(filter->recalculated_residuals){
         layer = (int)row.layer;
         locangle=row.locangle*DEG2RAD;
         if(filter->recalculate_method==0){
            // Call XvsT function routine
            dc_xvst_fct_(&sec,&layer,&locangle,&t,&row.B,&dist,&tmax);
         }else{
            // Call Xvst Table interpolation routine
            if(filter->sup!=3 && filter->sup!=4)row.B=row.B1=0.0;
            dc_time_to_dist_(&sec,&layer,&wire,&row.beta,&locangle,&row.time,&row.B,&row.B1,&dist,&sigma);
         }
         row.resi=fabs(row.doca)-fabs(dist);
      }

      // Some histos need to be filled with non-filtered events
      hf1(RPID,row.resi,1.0);
      hf1(RPID+(sec*10),row.resi,1.0);

      // Time histogram always filled with uncorrected time (for finding tzero)
      hf1(TID,row.time,1.0);
      hf1(TID+(sec*10),row.time,1.0);

		// Overwrite time in Ntuple row with corrected time so that filter can see it
		row.time=t;

      // Apply filter
      if(row*(*filter))continue;

      // Fill cumulative histograms
      hfill(RID,t,row.resi,1.0);
      hfill(XID,t,row.doca,1.0);
      hf2(RSID,t,row.resi,1.0);
      hf2(XSID,t,row.doca,1.0);
      
      // Fill sector dependant histograms
      hfill(RID+(sec*10),t,row.resi,1.0);
      hfill(XID+(sec*10),t,row.doca,1.0);
      hf2(RSID+(sec*10),t,row.resi,1.0);
      hf2(XSID+(sec*10),t,row.doca,1.0);
      hf1(TNID,t,1.0);
		
		// Add to averages
		for(j=0;j<sizeof(ntuple_row_t)/sizeof(float);j++){
			if(finite(X[j])){
				((float*)&N_avg[sec].sector)[j]+=1.0;
				((float*)&AVG[sec][filter->sup].sector)[j]+=X[j];
				// average for "all sectors" 
				((float*)&N_avg[0].sector)[j]+=1.0;
				((float*)&AVG[0][filter->sup].sector)[j]+=X[j];
			}
		}
		      
   }

	// Calculate final averages
	for(sec=0;sec<=6;sec++)
   	for(i=0;i<sizeof(ntuple_row_t)/sizeof(float);i++)
			((float*)&AVG[sec][filter->sup].sector)[i]/=((float*)&N_avg[sec].sector)[i];

	filter->progress=1.0;
	pthread_exit(NULL);
}

//======================================================
// This reads through the Ntuple for the sole purpose of
// filling a DOCA histogram. It is done here for optimal
// speed since the DOCA histogram is expected to be filled
// repeatedly in the tzero finder.
//======================================================
void* Fill_DOCA_From_Ntuple_thread(void *arg)
{
	int i, err;
	float *progress=(float*)arg;
	int sec,sup,layer,wire=100;
	int N;
	ntuple_row_t row;
	float *X=&row.sector;
	float dist,sigma,locangle;
	int HID,NID;
	
	// First, zero out the histogram
	HID=2000;
	hreset(HID ," ");

	// Make sure DC parms area is aligned with USER_PARMS[][] for time corrections
	CopyParmsToDC(&USER_PARMS[SEC][SUP],SEC,SUP);
	
	// Read in Ntuple and set it up for looping over events
	NID=1000+SUP;
	Get_Ntuple(NID);
	hnoent(NID,&N);
	hgnpar(NID,"Read_Ntuple");

	// Loop over events
	for(i=0;i<N;i++){

		// read in next event
		hgnf(NID,i+1,X,&err);
		*progress=(float)i/(float)(N-1);

		layer = (int)row.layer;
		sup = (layer-1)/6 +1;
		if(row.sector != SEC || sup!=SUP)continue;
		
		if (row.DC1<0 || row.hit!=0){
			//the status markers for this event are not good, 
			//so it will not be used to fill the histos.
			continue;
		}

		// Find calc doca including time corrections
		if(SUP!=3 && SUP!=4)row.B=row.B1=0.0;
		locangle=row.locangle*DEG2RAD;
		dc_time_to_dist_(&SEC,&layer,&wire,&row.beta,&locangle,&row.time,&row.B,&row.B1,&dist,&sigma);
		if(row.doca<0.0)dist=-dist;
		
		// Fill histo
		hf1(HID,dist,1.0);
 				
	}

	*progress=1.0;
	pthread_exit(NULL);
}

//==================================================
// This routine is called as a thread to update
// a progress bar on the GUI. It uses the
// variable PROGRESS_FRACTION to determine the
// current progress and updates at 10 Hz
//==================================================
void* ProgressBar(void *arg)
{
   int width=80,height=15;
   int x=100,y=2;
   char cmd[256];
   float *progress=(float*)arg;
   
   // "working..." text
   sprintf(cmd,".top.progress create text %d %d -text \"%s\""
      ,x/2,10,PROGRESS_MESSAGE);
   Tcl_Evaluate(cmd);
   // Outline of progress bar
   sprintf(cmd,".top.progress create rectangle %d %d %d %d "
      ,x,y,x+width,y+height);
   strcat(cmd," -fill \"#88F\" -outline \"#000\" -width 1");
   Tcl_Evaluate(cmd);   
   
   x++;
   y++;
   width-=2;
   height-=2;
   
   // update bar once every 100 ms until progress = 1
   do{
      
      // Interior of progress bar
      sprintf(cmd,".top.progress create rectangle %d %d %d %d "
         ,x,y,x+(int)((float)width*(*progress)),y+height);
      strcat(cmd," -fill \"#22F\" -tags PBAR");
      Tcl_Evaluate(".top.progress delete PBAR"); 
      Tcl_Evaluate(cmd);      
      
      Tcl_Evaluate("update;update");
      USLEEP(100000);
   }while((*progress)<1.0);
   
   Tcl_Evaluate(".top.progress delete all");
   
   return arg;
}

//==================================================
// This routine is called as a thread to update
// a progress clock on the GUI. This is different
// from ProgressBar() in that it has no handle on
// how far along the current job is going to take.
// The argument is therefore used simply as a flag to
// signal the completion of the job.
//==================================================
void* ProgressClock(void *arg)
{
   float *progress=(float*)arg;
   
	ProgressClockStart();
   
   // update clock once every 100 ms until progress = 1
   do{
      
		ProgressClockUpdate();
      USLEEP(100000);
      if(CANCEL)break;
   }while((*progress)<1.0);
   
   ProgressClockStop();

   return arg;
}
//============================
// Initialize the progress clock
//============================
void ProgressClockStart(void)
{
   int width=20,height=18,x=100,y=2;
	char cmd[256];
	
   // "working..." text
   sprintf(cmd,".top.progress create text %d %d -text \"%s\""
      ,x/2,10,PROGRESS_MESSAGE);
   Tcl_Evaluate(cmd);
   // Outline of progress bar
   sprintf(cmd,".top.progress create oval %d %d %d %d "
      ,x,y,x+width,y+height);
   strcat(cmd," -fill \"#2AA\" -outline \"#000\" -width 2");
   Tcl_Evaluate(cmd);   
}
//============================
// Update the progress clock
//============================
void ProgressClockUpdate(void)
{
   int width=20,height=18,x=100,y=2;
	static int clk=0;
	char cmd[256];
	
	// Interior of progress bar
	sprintf(cmd,".top.progress create arc %d %d %d %d "
		,x,y,x+width,y+height);
	strcat(cmd," -fill \"#2F2\" -tags PCLK ");
	sprintf(cmd,"%s -start %d -extent %d",cmd,clk+90,clk-360);
	Tcl_Evaluate(".top.progress delete PCLK"); 
	Tcl_Evaluate(cmd);
	clk+=10;
	if(clk>=360)clk=0;
      
	Tcl_Evaluate("update;update");
}
//============================
// Destroy the progress clock
//=============================
void ProgressClockStop(void)
{
	Tcl_Evaluate(".top.progress delete all");
}



//===========================================================
// Copy Parameters from dc_parms_t object into xvst_parms_t
// data structure in dc library (i.e. the place where
// the dc_xvst_fct() routine looks for them.
//===========================================================
void CopyParmsToDC(dc_parms_t* dc_parms,int sec,int sup)
{
   xvst_parms_t *p;
   
   p=(xvst_parms_t*)&(dc_parms->parms.p);
   map_parms[sec][sup]=*p;
}

//===========================================================
// Copy Parameters to dc_parms_t object from xvst_parms_t
// data structure in dc library (i.e. the place where
// the dc_xvst_fct() routine looks for them.
//===========================================================
void CopyParmsFromDC(dc_parms_t* dc_parms,int sec,int sup)
{
   xvst_parms_t *p;
   
   p=(xvst_parms_t*)&(dc_parms->parms.p);
   *p=map_parms[sec][sup];
} 

//===========================================================
// Copy Timewalk parameters to all sectors/superlayers to
// account for the redundancy
//===========================================================
void CopyTimewalkParmsToAll(dc_parms_t tp)
{
	int i,sec,sup;
		
	// Copy Timewalk parameters to all other sec/sup's for consistancy
	for(sup=0;sup<=6;sup++){
		for(sec=0;sec<=6;sec++){
			for(i=1;i<=11;i++){
				USER_PARMS[sec][sup].tw_parm_r1[i]=tp.tw_parm_r1[i];
				USER_PARMS[sec][sup].tw_parm_r2[i]=tp.tw_parm_r2[i];
				USER_PARMS[sec][sup].tw_parm_r3[i]=tp.tw_parm_r3[i];
			}
			for(i=1;i<=3;i++){
				USER_PARMS[sec][sup].tw_fact[i]=tp.tw_fact[i];
				USER_PARMS[sec][sup].tw_tau[i]=tp.tw_tau[i];
				USER_PARMS[sec][sup].tw_betaslope[(2*i)-1]=tp.tw_betaslope[(2*i)-1];
				USER_PARMS[sec][sup].tw_betaslope[(2*i)-0]=tp.tw_betaslope[(2*i)-0];
			}
		}
	}
}

//===========================================================
// Copy user parms from one superlayer/sector to another
//===========================================================
int CopyUserParms(ClientData clientData,Tcl_Interp *interp,int argc, const char *argv[])
{
	int i,j;
	int copy_from_sec, copy_to_sec;
	int copy_from_sup, copy_to_sup;
	int copy_selected_parms;
	int err=0;
	dc_parms_t *f,*t;
	map_parms_t *fp,*tp;

	// Extract values from arguments array
	copy_from_sec = atoi(argv[1]);
	copy_to_sec   = atoi(argv[2]);
	copy_from_sup = atoi(argv[3]);
	copy_to_sup   = atoi(argv[4]);
	copy_selected_parms = atoi(argv[5]);

	// Make sure they are in range
	if(copy_from_sec<0 || copy_from_sec>6)err=-1;
	if(copy_from_sup<1 || copy_from_sup>6)err=-1;
	if(copy_to_sec<0 || copy_to_sec>6)err=-1;
	if(copy_to_sup<1 || copy_to_sup>6)err=-1;
	if(err){
		printf("%s:%d  Bad sup/sec value (from sec/sup=%d/%d  to sec/sup=%d/%d)\n"
			,__FILE__,__LINE__
			,copy_from_sec,copy_from_sup,copy_to_sec,copy_to_sup);
		return TCL_OK;
	}

	cout<<"Parms being copied from sup/sec="<<copy_from_sup<<"/"<<copy_from_sec
		<<" to sup/sec="<<copy_to_sup<<"/"<<copy_to_sec<<"\n";

	// Copy the parameters
	// Here, we don't want to overwrite things like average local angle or
	// layer. So, we just copy selected items
	f  = &USER_PARMS[copy_from_sec][copy_from_sup];
	t  = &USER_PARMS [copy_to_sec ][ copy_to_sup ];
	fp = &f->parms.p;
	tp = &t->parms.p;

	// x vs t parms
	tp->ff			= fp->ff;
	tp->p1			= fp->p1;
	tp->p2			= fp->p2;
	tp->p3			= fp->p3;
	tp->p4			= fp->p4;
	tp->xvst_functiontype= fp->xvst_functiontype;

	// timewalk parms
	for(i=0;i<11;i++){
		t->tw_parm_r1[i] = f->tw_parm_r1[i];
		t->tw_parm_r2[i] = f->tw_parm_r2[i];
		t->tw_parm_r3[i] = f->tw_parm_r3[i];
	}
	for(i=0;i<4;i++){
		t->tw_fact[i] = f->tw_fact[i];
		t->tw_tau[i]  = f->tw_tau[i];
	}
	for(i=0;i<7;i++){
		t->tw_betaslope[i] = f->tw_betaslope[i];
	}
	t->tw_functiontype = f->tw_functiontype;

	// Other stuff
	t->expert_mode |= f->expert_mode;

	// If the Copy Selected Parms button is checked then
	// return now, otherwise, copy the remaining parameters
	if(copy_selected_parms)return TCL_OK;

	tp->tzero		= fp->tzero;
	tp->tmax			= fp->tmax;
	tp->tau_p1		= fp->tau_p1;
	tp->tau_p2		= fp->tau_p2;
	tp->tau_p3		= fp->tau_p3;
	tp->spare1		= fp->spare1;
	tp->B_p1			= fp->B_p1;
	tp->B_p2			= fp->B_p2;
	tp->B_p3			= fp->B_p3;
	tp->B_p4			= fp->B_p4;
	tp->spare2		= fp->spare2;
	tp->tmax_B_p1	= fp->tmax_B_p1;
	tp->tmax_B_p2	= fp->tmax_B_p2;
	tp->tmax_B_p3	= fp->tmax_B_p3;

	return TCL_OK;
}





