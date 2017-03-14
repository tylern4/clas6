
#include "dc3.h"
#include "tcl_procs.h"
#include <ntypes.h>
#include <bostypes.h>

#ifdef SunOS
#include <siginfo.h>
#endif

/* global variable declarations */
#define NWPAWC_ 1000000
int NWPAWC=NWPAWC_;
Tcl_Interp* interp;
int REALPLOTTHREAD=0;

// Globals
float pawc_[NWPAWC_];
int quest_[100];
char INPUTFILE[256]="";
int EXCLUDED[7]={33,3,9,16,22,27,33};
ntuple_row_t AVG[7][7];
BOSbank bcs_,wcs_;

// Tcl/Tk globals
int RUNNUMBER=0;
int NOTEBOOK_PAGE=0;
int SEC=1,SUP=1;
int EXPERT;
int FAST_STARTUP=0;
int SKIP_DC3_DEFAULTS_DB=0;
int TLIM[7],TLIMT[7],TLIM_LOW;
int TLIM_AUTO_LOW[7]={0,  15,  15,  15,  15,  15,  15};
int TLIM_AUTO_HI[7] ={0,180,180, 500, 600,1000,1200};
int AUTOCUT_T=1;
int STD_TCUT=1;
float STD_TCUT_LOW[7][7],STD_TCUT_HI[7][7];
double AUTOCUT_T_FRAC=0.87;
char *TZERO_FRAC,*TMAX_FRAC,*TZERO_AUTOFRAC;
char *TZERO_AUTOFRAC_INIT = "0.002";
char *TMAX_REGION1_AUTOFRAC, *TMAX_REGION2_AUTOFRAC, *TMAX_REGION3_AUTOFRAC;
char *TMAX_REGION1_AUTOFRAC_INIT = "0.99";
char *TMAX_REGION2_AUTOFRAC_INIT = "0.97";
char *TMAX_REGION3_AUTOFRAC_INIT = "0.97";
char *USERCOMMENT;
char *MAPFILE,*MAPCOMMENT;
char *CALDB_HOST, *CALDB_RUNINDEX, *CALDB_USER, *CALDB_PASSWORD;
int MINRUN, MAXRUN;
int SHOW_RESIDUAL=0,SHOWPLOTS=1,SHOW_SCATTER=1,SHOW_PROFILE=1;
int SHOW_FUNCTION=1,SHOW_PREDICTED_RESI=1,SHOW_ZERO_LINE=1;
int RECALC_RESIDUALS=0;
int RECALC_METHOD=0;
int REPLOT=0;
int HOLD_PAR1=0 ,HOLD_PAR2=0 ,HOLD_PAR3=1 ,HOLD_PAR4=0;
int HOLD_TMAX=1 ,HOLD_TZERO=1 ,HOLD_FF=1;
int SEEK_FIRST=1;
int CANCEL=0;
int USE_CORRECTED_TIME=1;
int TW_TYPE=0 ,XVST_TYPE=0 ,EXPERT_CONTROLS=0;
int TW_TYPE_COMMAND_LINE=-1;
int MDBI_RESULT=0;
int TZERO_PLOT_MIN=1,TZERO_PLOT_MAX=500;
int TZERO_FIT_MIN=10,TZERO_FIT_MAX=30;

/* For CALDB */
char RUNINDEX_TABLE[256]="RunIndex";
extern char *RunIndex_table;
char *RunIndex_table; /* this should probably be in libcaldb.a */

dc_parms_t FILE_PARMS[7][7];
dc_parms_t MAP_PARMS[7][7];
dc_parms_t USER_PARMS[7][7];
filter_t DATA_FILTER[7];


void ParseCommandLineArgs(int narg,char* argv[]);
void LinkTclToC(void);
void Book_Histograms(void);
void SigHandler(int sig);
void* TclThread(void* arg);
void Tcl_Evaluate(char* cmd);
void Tcl_Evaluate_Now(char* cmd);
void SetDefaultsFromDB(void);

void SplashScreen(void);

//======================
// Program Entry point
//======================
int main(int narg,char* argv[])
{
   char str[256],fname[256],*ptr;
   int i;
   int quit=0;
   pthread_t plot_thread,tcl_thread;
   dc_parms_t dc_parms;

   /*---------- Parse Command line args ---------*/
   ParseCommandLineArgs(narg,argv);

   PrintVersion();
   /*--------------- initialize -----------------*/
   program_is_dc_calib_check=1;
   do_tzero_subtraction=1;
   do_timewalk_correction=1;
   signal(SIGINT,SigHandler);
   signal(SIGFPE,SigHandler);

#ifdef CALDB
   //	RunIndex_table = (char *) malloc(32);
	if(ptr=getenv("CLAS_CALDB_RUNINDEX"))strcpy(RUNINDEX_TABLE,ptr);
	//   RunIndex_table=RUNINDEX_TABLE; /* this should come from environment */
#endif /* CALDB */

   /* Check for certain incompatibilty bug with older dc libraries */ 
     if(((long)&dc_parms.filter-(long)&dc_parms.parms)!=sizeof(xvst_parms_t)){
		printf("Mismatch in dc_parms_t class and xvst_parms_t data structure at %s:%d\n"
			,__FILE__,__LINE__);
		exit(-1);
   }

   /* cernlib */
   hlimit(NWPAWC); /* initialize hbook */
   hplint(1);

   /* Tcl/Tk */
	char *TK_LIBRARY="TK_LIBRARY=/usr/lib/tk8.4";		// Man, don't even ask
	putenv(TK_LIBRARY);													//         ""
   interp=Tcl_CreateInterp();
   if(!interp){printf("Unable to create Tcl/Tk interpreter!\n");exit(-1);}
   if(TCL_OK != Tcl_Init(interp)){cerr<<interp->result<<endl; exit(-1);}
   if(TCL_OK != Tk_Init(interp)){cerr<<interp->result<<endl; exit(-1);}

   /* Flash up logo */
   if(!FAST_STARTUP)SplashScreen(); else Tcl_Evaluate("set splash_win 0");
   
 	/* Override hardcoded defaults with ones from database (if available) */
	if(!SKIP_DC3_DEFAULTS_DB)SetDefaultsFromDB();

  // .tcl files made into static strings and compiled in C objects
   Tcl_Evaluate(go_tcl_proc);
   Tcl_Evaluate(notebook_tcl_proc);
   Tcl_Evaluate(caldb_tcl_proc);
   Tcl_Evaluate(map_tcl_proc);
   Tcl_Evaluate(misc_tcl_proc);
   Tcl_Evaluate(options_tcl_proc);
   Tcl_Evaluate(quality_tcl_proc);
   Tcl_Evaluate(tmax_tcl_proc);
   Tcl_Evaluate(tzero_tcl_proc);
   Tcl_Evaluate(filter_tcl_proc);
   Tcl_Evaluate(fit_tcl_proc);
   Tcl_Evaluate(main_tcl_proc);
   Tcl_Evaluate(expert_controls_tcl_proc);
   Tcl_Evaluate(copy_parms_tcl_proc);
   Tcl_Evaluate(expert_tcl_proc); /* this should be last */
   LinkTclToC();

   
   /* Initialize some variables */
   for(i=0;i<7;i++)DATA_FILTER[i].N=0;
	
   /* Try and force Tcl/Tk to draw control window */
   Tcl_Evaluate("update;wm geometry . -0-100 ;update");
   
   /* Attach input file if it was specified on command line */
   sprintf(str,"OpenFile \"%s\"",INPUTFILE);
   if(strlen(INPUTFILE))Tcl_Evaluate(str);

   /* Startup Plotting thread */
   if(REALPLOTTHREAD){
   	pthread_create(&plot_thread,NULL,PlotThread,(void*)&quit);
	}else{
		Tcl_Evaluate("after idle PlotThread");
	}

   /*----------------- main loop -----------------*/
   Tk_MainLoop();
   
   
   /*----------- completion and clean-up ---------*/
   quit=1;
   if(REALPLOTTHREAD){
   int stat=pthread_join(plot_thread,NULL);
   if(stat!=0) printf("stat pthread_join %d\n",stat);
   }
   return 0;
}


void ParseCommandLineArgs(int narg,char* argv[])
{
   int i;
   
   for(i=1;i<narg;i++){
      if(!strcmp(argv[i],"-v")){PrintVersion();exit(0);}

      if(!strcmp(argv[i],"-h")){Usage();exit(0);}

      if(!strcmp(argv[i],"-t")){REALPLOTTHREAD=1;}

      if(!strcmp(argv[i],"-f")){FAST_STARTUP=1;}

      if(!strcmp(argv[i],"-d")){SKIP_DC3_DEFAULTS_DB=1;}

      if(!strncmp(argv[i],"-twtype=",strlen("-twtype="))){
			// This is a quick hack for Seema. 10/28/2007 D.L.
			const char* twtypestr=&argv[i][strlen("-twtype=")];
			TW_TYPE_COMMAND_LINE = atoi(twtypestr);
		}

      if(argv[i][0]!='-')strcpy(INPUTFILE,argv[i]);
   }
}

//==========================================
// Setup all the links/traces between tcl
// variables and C variables/routines
//==========================================
void LinkTclToC(void)
{
   int i;
   char str[256],*ptr;
   int flags=TCL_GLOBAL_ONLY | TCL_TRACE_WRITES;

   Tcl_CreateCommand(interp,"OpenFile",OpenFile,0,NULL);
   Tcl_CreateCommand(interp,"QualityCheck",QualityCheck,0,NULL);
   Tcl_CreateCommand(interp,"FindTFraction",FindTFraction,0,NULL);
   Tcl_CreateCommand(interp,"PlotThreadTcl",PlotThreadTcl,0,NULL);
   Tcl_CreateCommand(interp,"CaldbWrite",CaldbWrite,0,NULL);
   Tcl_CreateCommand(interp,"MapWrite",MapWrite,0,NULL);
   Tcl_CreateCommand(interp,"CreateMapFile",CreateMapFile,0,NULL);
   Tcl_CreateCommand(interp,"ResetTValue",ResetTValue,0,NULL);
   Tcl_CreateCommand(interp,"ResetTValues",ResetTValues,0,NULL);
   Tcl_CreateCommand(interp,"FitOne",FitOne,0,NULL);
   Tcl_CreateCommand(interp,"FitSuperlayer",FitSuperlayer,0,NULL);
   Tcl_CreateCommand(interp,"FitAll",FitAll,0,NULL);
   Tcl_CreateCommand(interp,"SetCutButton",SetCutButton,0,NULL);
   Tcl_CreateCommand(interp,"SetCuts",SetCuts,0,NULL);
   Tcl_CreateCommand(interp,"ResetUserParms",ResetUserParms,0,NULL);
   Tcl_CreateCommand(interp,"ChisqVector",ChisqVector,0,NULL);
   Tcl_CreateCommand(interp,"ChisqTable",ChisqTable,0,NULL);

   Tcl_CreateCommand(interp,"TzeroTable",TzeroTable,0,NULL);
   Tcl_CreateCommand(interp,"TmaxTable",TmaxTable,0,NULL);

   Tcl_CreateCommand(interp,"ApplyExpertParameters",ApplyExpertParameters,0,NULL);
   Tcl_CreateCommand(interp,"SetParametersToDefault",SetParametersToDefault,0,NULL);
   Tcl_CreateCommand(interp,"CopyUserParms",CopyUserParms,0,NULL);
   Tcl_CreateCommand(interp,"PlotCalcDOCA",PlotCalcDOCA,0,NULL);
   Tcl_CreateCommand(interp,"SetTzeroFromLinearFit",SetTzeroFromLinearFit,0,NULL);
   

   FILENAME=Tcl_Alloc(256); strcpy(FILENAME,"None");
   Tcl_LinkVar(interp,"filename",(char*)&FILENAME,TCL_LINK_STRING);
   TZERO_FRAC=Tcl_Alloc(32); strcpy(TZERO_FRAC,"0.0");
   Tcl_LinkVar(interp,"tzero_frac",(char*)&TZERO_FRAC,TCL_LINK_STRING);
   TMAX_FRAC=Tcl_Alloc(32); strcpy(TMAX_FRAC,"0.0");
   Tcl_LinkVar(interp,"tmax_frac",(char*)&TMAX_FRAC,TCL_LINK_STRING);
   TZERO_AUTOFRAC=Tcl_Alloc(32); strcpy(TZERO_AUTOFRAC,TZERO_AUTOFRAC_INIT);
   Tcl_LinkVar(interp,"tzero_autofrac",(char*)&TZERO_AUTOFRAC,TCL_LINK_STRING);
   TMAX_REGION1_AUTOFRAC=Tcl_Alloc(32); strcpy(TMAX_REGION1_AUTOFRAC,TMAX_REGION1_AUTOFRAC_INIT);
   Tcl_LinkVar(interp,"tmax_region1_autofrac",(char*)&TMAX_REGION1_AUTOFRAC,TCL_LINK_STRING);
   TMAX_REGION2_AUTOFRAC=Tcl_Alloc(32); strcpy(TMAX_REGION2_AUTOFRAC,TMAX_REGION2_AUTOFRAC_INIT);
   Tcl_LinkVar(interp,"tmax_region2_autofrac",(char*)&TMAX_REGION2_AUTOFRAC,TCL_LINK_STRING);
   TMAX_REGION3_AUTOFRAC=Tcl_Alloc(32); strcpy(TMAX_REGION3_AUTOFRAC,TMAX_REGION3_AUTOFRAC_INIT);
   Tcl_LinkVar(interp,"tmax_region3_autofrac",(char*)&TMAX_REGION3_AUTOFRAC,TCL_LINK_STRING);

   USERCOMMENT=Tcl_Alloc(1024); strcpy(USERCOMMENT,"");
   Tcl_LinkVar(interp,"usercomment",(char*)&USERCOMMENT,TCL_LINK_STRING);

   CALDB_HOST=Tcl_Alloc(256);
   strcpy(CALDB_HOST,(ptr=getenv("CLAS_CALDB_HOST")) ? ptr:"clasdb.jlab.org");
   Tcl_LinkVar(interp,"caldb_host",(char*)&CALDB_HOST,TCL_LINK_STRING);

   CALDB_RUNINDEX=Tcl_Alloc(256);
   strcpy(CALDB_RUNINDEX,(ptr=getenv("CLAS_CALDB_RUNINDEX")) ? ptr:"calib.RunIndex");
   Tcl_LinkVar(interp,"caldb_runindex",(char*)&CALDB_RUNINDEX,TCL_LINK_STRING);

   CALDB_USER=Tcl_Alloc(256);
   strcpy(CALDB_USER,(ptr=getenv("USER")) ? ptr:"clasuser");
   Tcl_LinkVar(interp,"caldb_user",(char*)&CALDB_USER,TCL_LINK_STRING);

   CALDB_PASSWORD=Tcl_Alloc(256);
   strcpy(CALDB_PASSWORD,(ptr=getenv("CLAS_CALDB_PASSWORD")) ? ptr:"");
   Tcl_LinkVar(interp,"caldb_password",(char*)&CALDB_PASSWORD,TCL_LINK_STRING);
   
	MAPFILE=Tcl_Alloc(256); strcpy(MAPFILE,"DC_DOCA.map");
	if(ptr=getenv("CLAS_PARMS"))sprintf(MAPFILE,"%s/Maps/DC_DOCA.map",ptr);
	Tcl_LinkVar(interp,"mapfile",(char*)&MAPFILE,TCL_LINK_STRING);

	MAPCOMMENT=Tcl_Alloc(1024); strcpy(MAPCOMMENT,"");
	strcpy(MAPCOMMENT,"");
	Tcl_LinkVar(interp,"mapcomment",(char*)&MAPCOMMENT,TCL_LINK_STRING);

   Tcl_LinkVar(interp,"expert",(char*)&EXPERT,TCL_LINK_INT);
   Tcl_LinkVar(interp,"notebook_page",(char*)&NOTEBOOK_PAGE,TCL_LINK_INT);
   Tcl_LinkVar(interp,"showplots",(char*)&SHOWPLOTS,TCL_LINK_INT);
   Tcl_LinkVar(interp,"show_scatter",(char*)&SHOW_SCATTER,TCL_LINK_INT);
   Tcl_LinkVar(interp,"show_profile",(char*)&SHOW_PROFILE,TCL_LINK_INT);
   Tcl_LinkVar(interp,"show_function",(char*)&SHOW_FUNCTION,TCL_LINK_INT);
   Tcl_LinkVar(interp,"show_predicted_resi",(char*)&SHOW_PREDICTED_RESI,TCL_LINK_INT);
   Tcl_LinkVar(interp,"show_zero_line",(char*)&SHOW_ZERO_LINE,TCL_LINK_INT);
   Tcl_LinkVar(interp,"show_residual",(char*)&SHOW_RESIDUAL,TCL_LINK_INT);
   Tcl_LinkVar(interp,"recalc_residuals",(char*)&RECALC_RESIDUALS,TCL_LINK_INT);
   Tcl_LinkVar(interp,"recalc_method",(char*)&RECALC_METHOD,TCL_LINK_INT);
   Tcl_LinkVar(interp,"replot",(char*)&REPLOT,TCL_LINK_INT);
   Tcl_LinkVar(interp,"runnumber",(char*)&RUNNUMBER,TCL_LINK_INT);
   Tcl_LinkVar(interp,"hold_par1",(char*)&HOLD_PAR1,TCL_LINK_INT);
   Tcl_LinkVar(interp,"hold_par2",(char*)&HOLD_PAR2,TCL_LINK_INT);
   Tcl_LinkVar(interp,"hold_par3",(char*)&HOLD_PAR3,TCL_LINK_INT);
   Tcl_LinkVar(interp,"hold_par4",(char*)&HOLD_PAR4,TCL_LINK_INT);
   Tcl_LinkVar(interp,"hold_tmax",(char*)&HOLD_TMAX,TCL_LINK_INT);
   Tcl_LinkVar(interp,"hold_tzero",(char*)&HOLD_TZERO,TCL_LINK_INT);
   Tcl_LinkVar(interp,"hold_ff",(char*)&HOLD_FF,TCL_LINK_INT);
   Tcl_LinkVar(interp,"cancel",(char*)&CANCEL,TCL_LINK_INT);
   Tcl_LinkVar(interp,"std_tcut",(char*)&STD_TCUT,TCL_LINK_INT);
   Tcl_LinkVar(interp,"autocut_t",(char*)&AUTOCUT_T,TCL_LINK_INT);
   Tcl_LinkVar(interp,"autocut_t_frac",(char*)&AUTOCUT_T_FRAC,TCL_LINK_DOUBLE);
   Tcl_LinkVar(interp,"use_corrected_time",(char*)&USE_CORRECTED_TIME,TCL_LINK_INT);
   Tcl_LinkVar(interp,"seek_first",(char*)&SEEK_FIRST,TCL_LINK_INT);
   Tcl_LinkVar(interp,"tw_type",(char*)&TW_TYPE,TCL_LINK_INT);
   Tcl_LinkVar(interp,"xvst_type",(char*)&XVST_TYPE,TCL_LINK_INT);
   Tcl_LinkVar(interp,"expert_controls",(char*)&EXPERT_CONTROLS,TCL_LINK_INT);
   Tcl_LinkVar(interp,"minrun",(char*)&MINRUN,TCL_LINK_INT);
   Tcl_LinkVar(interp,"maxrun",(char*)&MAXRUN,TCL_LINK_INT);
   Tcl_LinkVar(interp,"mdbi_result",(char*)&MDBI_RESULT,TCL_LINK_INT);
   Tcl_LinkVar(interp,"tzero_plot_min",(char*)&TZERO_PLOT_MIN,TCL_LINK_INT);
   Tcl_LinkVar(interp,"tzero_plot_max",(char*)&TZERO_PLOT_MAX,TCL_LINK_INT);
   Tcl_LinkVar(interp,"tzero_fit_min",(char*)&TZERO_FIT_MIN,TCL_LINK_INT);
   Tcl_LinkVar(interp,"tzero_fit_max",(char*)&TZERO_FIT_MAX,TCL_LINK_INT);

   for(i=1;i<=6;i++){
      sprintf(str,"t_lo%d",i);
      Tcl_LinkVar(interp,str,(char*)&TLIM_AUTO_LOW[i],TCL_LINK_INT);
      sprintf(str,"t_hi%d",i);
      Tcl_LinkVar(interp,str,(char*)&TLIM_AUTO_HI[i],TCL_LINK_INT);
   }

   
   Tcl_TraceVar(interp,"sec",flags,NewSelection,NULL);
   Tcl_TraceVar(interp,"sup",flags,NewSelection,NULL);
   Tcl_TraceVar(interp,"tzero",flags,NewTzeroTmax,NULL);
   Tcl_TraceVar(interp,"tmax",flags,NewTzeroTmax,NULL);
   Tcl_TraceVar(interp,"notebook_page",flags,NewNotebookPage,NULL);


   // Copy Tcl default time limits
   for(i=0;i<=6;i++){
      sprintf(str,"%d",i);
      TLIM[i]=atoi(Tcl_GetVar2(interp,"tlim",str,TCL_GLOBAL_ONLY));
      TLIMT[i]=atoi(Tcl_GetVar2(interp,"tlimt",str,TCL_GLOBAL_ONLY));
   }
   TLIM_LOW=atoi(Tcl_GetVar(interp,"tlim_low",TCL_GLOBAL_ONLY));
}

//======================================================
// Book all of the histograms. These will live for the
// duration of the program.
//======================================================
void Book_Histograms(void)
{
   int sec,sup;
   int XID,RID,TID,RSID,XSID,RPID,RFID,XFID,TNID;
   char title[256];
   float cell_size;
   
   hcdir("//LUN1/tbt/proton"," ");
   
   // Make sure cell sizes are set
   xvst_set_cell_sizes();
   
   // Loops include sec/sup=0 for expert "All" options
   for(sec=0;sec<=6;sec++){
      for(sup=0;sup<=6;sup++){
         sprintf(title,"S%dSL%d",sec,sup);
         if(sec==0)sprintf(title,"S%d All SL",sec);
         if(sup==0)sprintf(title,"SL%d All Sectors",sup);
         if(sec==0 || sup==0)sprintf(title,"All sectors All SL");
         RID=100+(sec*10)+sup;
         XID=RID+100;
         TID=RID+200;
         RSID=RID+300;
         XSID=RID+400;
         RPID=RID+500;
         RFID=RID+600;
         XFID=RID+700;
         TNID=RID+800;
         cell_size=DC_HEXSIZE[EXCLUDED[sup]];
         hbprof(RID,title,75,TLIM_LOW,TLIM[sup],-0.2,0.2," ");
         hbprof(XID,title,75,TLIM_LOW,TLIM[sup],0.0,cell_size," ");
         hbook1(TID,title,500,TLIM_LOW,TLIMT[sup],0);
         hbook2(RSID,title,75,TLIM_LOW,TLIM[sup],100,-0.2,0.2,1020);
         hbook2(XSID,title,75,TLIM_LOW,TLIM[sup],100,0.0,cell_size*1.05,1020);
         hbook1(RPID,title,100,-0.2,0.2,0);
         hbook1(TNID,title,75,TLIM_LOW,TLIM[sup],0);
         hcopy(RID,RFID," ");
         hcopy(XID,XFID," ");
      }
   }
}

//==================================================
// Print the list of currently booked histograms
// (this is only for debugging purposes)
//==================================================
void ListHistos(void)
{
   int IDVECT[4096],N=4096;
   int i,j;
   char CHTITL[256];
   int NX,NY,NWT,LOC;
   float XMI,XMA,YMI,YMA;

   cerr<<"Listing all histos:\n\n";
   hidall(IDVECT,&N);

   for(i=0;i<N;i++){
      memset(CHTITL,' ',256);
      CHTITL[255]=0;
      hgive(IDVECT[i],CHTITL,&NX,&XMI,&XMA,&NY,&YMI,&YMA,&NWT,&LOC);
      for(j=250;j>0;j--){
         if(CHTITL[j]==' ')
            CHTITL[j]=0;
         else
            break;
      }
      cerr<<IDVECT[i]<<" "<<CHTITL<<"\n";
   }
}

//===================================================================
// Wrapper for a sleep function. This is neccessary because SunOS
// doesn't have a thread safe usleep()
// ==================================================================
void USLEEP(long microseconds)
{
	usleep(microseconds);
#if 0

#ifdef Linux
	usleep(microseconds);
#else
#if defined(LinuxRH6) || defined(LinuxRH7)
	usleep(microseconds);
#else
#ifdef SunOS
		timespec_t ts;

		ts._tv_sec=0;
		ts._tv_nsec=microseconds*1000L;

		nanosleep(&ts,NULL);

#else
#error : OS not recognized. This program is only supported on SunOS and Linux
#endif
#endif
#endif

#endif
}

void SigHandler(int sig)
{
	psignal(sig,"Signal caught");
}

//==============================================
// Evaluate a tcl command and check for errors
//==============================================
void Tcl_Evaluate(char* cmd)
{
	int i,j;
	int len,block_size;
	char *ptr;

	if(Tcl_GlobalEval(interp,cmd)==TCL_ERROR){
		cerr<<"Error in Tcl command:\n";
		cerr<<interp->result<<"\n";
		cerr<<"\nCommand is:\n\n"<<cmd<<"\n";
		exit(-1);
	}
	return;

}


//==============================================
// SetDefaultsFromDB
//
// Try grabbing some default values from the clasdb database.
//==============================================
void SetDefaultsFromDB(void)
{
	char *ptr;
	
	ptr = GetDCDefault("AUTOCUT_T_FRAC"); if(strlen(ptr))AUTOCUT_T_FRAC = atof(ptr);

	ptr = GetDCDefault("HOLD_PAR1");  if(strlen(ptr))HOLD_PAR1 = atoi(ptr);
	ptr = GetDCDefault("HOLD_PAR2");  if(strlen(ptr))HOLD_PAR2 = atoi(ptr);
	ptr = GetDCDefault("HOLD_PAR3");  if(strlen(ptr))HOLD_PAR3 = atoi(ptr);
	ptr = GetDCDefault("HOLD_PAR4");  if(strlen(ptr))HOLD_PAR4 = atoi(ptr);
	ptr = GetDCDefault("HOLD_TMAX");  if(strlen(ptr))HOLD_TMAX = atoi(ptr);
	ptr = GetDCDefault("HOLD_TZERO"); if(strlen(ptr))HOLD_TZERO = atoi(ptr);
	ptr = GetDCDefault("HOLD_FF");    if(strlen(ptr))HOLD_FF = atoi(ptr);

	ptr = GetDCDefault("SEEK_FIRST"); if(strlen(ptr))SEEK_FIRST = atoi(ptr);
	ptr = GetDCDefault("USE_CORRECTED_TIME"); if(strlen(ptr))USE_CORRECTED_TIME = atoi(ptr);

	ptr = GetDCDefault("TW_TYPE"); if(strlen(ptr))TW_TYPE = atoi(ptr);
	ptr = GetDCDefault("XVST_TYPE"); if(strlen(ptr))XVST_TYPE = atoi(ptr);

	ptr = GetDCDefault("SHOW_RESIDUAL"); if(strlen(ptr))SHOW_RESIDUAL = atoi(ptr);

	ptr = GetDCDefault("AUTOCUT_T"); if(strlen(ptr))AUTOCUT_T = atoi(ptr);
	ptr = GetDCDefault("STD_TCUT"); if(strlen(ptr))STD_TCUT = atoi(ptr);

	ptr = GetDCDefault("TLIM_AUTO_LOW[1]"); if(strlen(ptr))TLIM_AUTO_LOW[1] = atoi(ptr);
	ptr = GetDCDefault("TLIM_AUTO_LOW[2]"); if(strlen(ptr))TLIM_AUTO_LOW[2] = atoi(ptr);
	ptr = GetDCDefault("TLIM_AUTO_LOW[3]"); if(strlen(ptr))TLIM_AUTO_LOW[3] = atoi(ptr);
	ptr = GetDCDefault("TLIM_AUTO_LOW[4]"); if(strlen(ptr))TLIM_AUTO_LOW[4] = atoi(ptr);
	ptr = GetDCDefault("TLIM_AUTO_LOW[5]"); if(strlen(ptr))TLIM_AUTO_LOW[5] = atoi(ptr);
	ptr = GetDCDefault("TLIM_AUTO_LOW[6]"); if(strlen(ptr))TLIM_AUTO_LOW[6] = atoi(ptr);
	
	ptr = GetDCDefault("TLIM_AUTO_HI[1]"); if(strlen(ptr))TLIM_AUTO_HI[1] = atoi(ptr);
	ptr = GetDCDefault("TLIM_AUTO_HI[2]"); if(strlen(ptr))TLIM_AUTO_HI[2] = atoi(ptr);
	ptr = GetDCDefault("TLIM_AUTO_HI[3]"); if(strlen(ptr))TLIM_AUTO_HI[3] = atoi(ptr);
	ptr = GetDCDefault("TLIM_AUTO_HI[4]"); if(strlen(ptr))TLIM_AUTO_HI[4] = atoi(ptr);
	ptr = GetDCDefault("TLIM_AUTO_HI[5]"); if(strlen(ptr))TLIM_AUTO_HI[5] = atoi(ptr);
	ptr = GetDCDefault("TLIM_AUTO_HI[6]"); if(strlen(ptr))TLIM_AUTO_HI[6] = atoi(ptr);

	ptr = GetDCDefault("TZERO_PLOT_MIN"); if(strlen(ptr))TZERO_PLOT_MIN = atoi(ptr);
	ptr = GetDCDefault("TZERO_PLOT_MAX"); if(strlen(ptr))TZERO_PLOT_MAX = atoi(ptr);
	ptr = GetDCDefault("TZERO_FIT_MIN"); if(strlen(ptr))TZERO_FIT_MIN = atoi(ptr);
	ptr = GetDCDefault("TZERO_FIT_MAX"); if(strlen(ptr))TZERO_FIT_MAX = atoi(ptr);
	
	// These are actually kept as strings in Tcl rather than numbers. The "init"
	// string is overridden here since initialization isn't actually done until
	// the Tcl variable is created above in LinkTclToC().
	ptr = GetDCDefault("TZERO_AUTOFRAC"); if(strlen(ptr))TZERO_AUTOFRAC_INIT = strdup(ptr);	
	ptr = GetDCDefault("TMAX_REGION1_AUTOFRAC"); if(strlen(ptr))TMAX_REGION1_AUTOFRAC_INIT = strdup(ptr);
	ptr = GetDCDefault("TMAX_REGION2_AUTOFRAC"); if(strlen(ptr))TMAX_REGION2_AUTOFRAC_INIT = strdup(ptr);
	ptr = GetDCDefault("TMAX_REGION3_AUTOFRAC"); if(strlen(ptr))TMAX_REGION3_AUTOFRAC_INIT = strdup(ptr);
}



