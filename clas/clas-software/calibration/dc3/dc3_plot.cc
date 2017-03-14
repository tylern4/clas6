


#include "dc3.h"

// These are for use by fast_xvst_fct() since it needs
// to be compatible with hbfun1_() from cernlib
int FAST_SEC,FAST_SUP;

// Used for ensuring latest constants are in xvst table
int REFILL_XVST_TABLE_FOR_DOCA_PLOT=1;

extern "C" {void dc_fill_timewalk_table(void);}

//======================================================
// This routine is called by Tcl/Tk whenever the user
// selects a Sector or Superlayer button
//======================================================
char* NewSelection(ClientData clientData,Tcl_Interp *interp,const char* name1,const char* name2,int flags)
{
   filter_t f;
   int tmax,tzero;
   char cmd[256];
   static int working=0;
   int last_sup=SUP;

   // ignore incoming requests while already busy
   if(working)return NULL;
   working=1;

   //Get Current Sec/Sup
   SEC=atoi(Tcl_GetVar(interp,"sec",TCL_GLOBAL_ONLY));
   SUP=atoi(Tcl_GetVar(interp,"sup",TCL_GLOBAL_ONLY));
   if(SEC<0||SEC>6||SUP<0||SUP>6){working=0;return TCL_OK;}
   
   // Check if filter has changed
   f=GetFilter();
   if(f!=DATA_FILTER[SUP])
   	Read_Ntuple(f);
   else
   	if(SUP!=last_sup)Get_Ntuple(1000+f.sup);
 
   // Set Tcl variables to values in USER_PARMS
   tmax=(int)USER_PARMS[SEC][SUP].parms.p.tmax;
   tzero=(int)USER_PARMS[SEC][SUP].parms.p.tzero;
   sprintf(cmd,"set tmax %d ; set tzero %d ",(int)tmax,(int)tzero);
   Tcl_Evaluate(cmd);
   
   // Make tmax/tzero scrollbar postions update
   Tcl_Evaluate("update;update");
   Tcl_Evaluate("entryvalue $tlim_low $tlimt($sup) $find_tmax_win.tzero.ctls.sbf");
   Tcl_Evaluate("entryvalue $tlim_low $tlimt($sup) $find_tmax_win.tmax.ctls.sbf");

   // Set flag so PlotThread will re-plot
   REPLOT=1;
   Tcl_UpdateLinkedVar(interp,"replot");

   working=0;
   return NULL;
}

//======================================================
// This routine is called by Tcl/Tk whenever the 
// notebook page is changed.
//======================================================
char* NewNotebookPage(ClientData clientData,Tcl_Interp *interp,const char* name1,const char* name2,int flags)
{

	return NULL;
}

//===========================================================
// This routine is run as a thread throughout the lifetime
// of the program. It replots the current histogram(s)
// whenever the REPLOT global variable is set. It will not
// try to plot more often than once per second.
//===========================================================
void* PlotThread(void* arg)
{
   int *quit=(int*)arg;
   int RID;
  
   do{
      
      // sleep 1/10th second so we don't use CPU time when
      // nothing is being done
      if(*quit==0)USLEEP(100000);
      
      // no need to go on if not drawing plots at all
      if(!SHOWPLOTS)continue;
      
      // Check if global flag says it's time to re-draw the plot
      if(!REPLOT)continue;

      // Make sure a valid sector/superlayer is selected
      if(SEC<0||SEC>6||SUP<0||SUP>6)continue;
      
      // Reset flag and sleep for 1/10th second. This should all
      // but eliminate the possibility of a request coming in at
      // just the wrong time (i.e. so that the last in a fast string
      // of requests is not shown).
      REPLOT=0;
      Tcl_UpdateLinkedVar(interp,"replot");
      USLEEP(100000);      
   

      // Set all hplot options to defaults
      hplopt("*   ",1);
      hplset("*   ",0.0);

      // Call the appropriate Subroutine for plotting
      switch(NOTEBOOK_PAGE){
         case 0: // Calibration Quality
            PlotResidualProjection();
            break;
         case 1: // T0
         case 2: // tmax
            PlotDriftTime();
            break;
         case 3: // Fit
            if(SHOW_RESIDUAL && EXPERT)
               PlotResidual();
            else
               PlotXvsT();
            break;
         case 4: // CALDB
         case 5: // Map
         
            break;
      }

	   // Update parameter values in GUI
   	FILE_PARMS[SEC][SUP].ShowParameters("$fit_path.novice.sv",1);
   	USER_PARMS[SEC][SUP].ShowParameters("$fit_path.novice.fv",1);
		TW_TYPE   = (int)USER_PARMS[SEC][SUP].tw_functiontype;
		XVST_TYPE = (int)USER_PARMS[SEC][SUP].parms.p.xvst_functiontype;
		Tcl_UpdateLinkedVar(interp,"tw_type");
		Tcl_UpdateLinkedVar(interp,"xvst_type");

      // Flush X buffer so screen is completely drawn
      ixupdwi(0);
   
      // Sleep a little so that we don't try and plot too often
      if(*quit==0)USLEEP(200000);

   }while(*quit==0);

	return arg;
}

int PlotThreadTcl(ClientData clientData,Tcl_Interp *interp,int argc, const char *argv[])
{
	int quit=1;

	// Calling the Plot Thread with quit=1 will force the loop
	// to iterate just once before returning and skip the sleeping
	PlotThread(&quit);


	return TCL_OK;
}

void PlotResidual(void)
{

   int RID,RSID,RFID,XFID,TMPID=9999;
   float X[2]={0.0,0.0},T[2]={TLIM_LOW,TLIM[SUP]};
   
   RID=100+(SEC*10)+SUP;
   RSID=RID+300;
   RFID=RID+600;
   XFID=RID+700;
   
   // Setup General plotting options
   hplopt("TIC ",1);

   // Copy the Scatterplot histogram and reset it so
   // we can use it to draw empty axes which everything
   // else can be superimposed on.
   hcopy(RSID,TMPID," ");
   hreset(TMPID," ");
   hplot(TMPID,"SCAT","HIST",1);
   hdelet(TMPID);

   if(SHOW_SCATTER){hplset("HCOL",4);hplot(RSID,"SSCAT","HIST",1);}
   if(SHOW_PROFILE){hplset("HCOL",2);hplot(RID,"S","HIST",1);}
   if(SHOW_ZERO_LINE){ipl(2,T,X);}
   if(SHOW_FUNCTION || SHOW_PREDICTED_RESI){
   	USER_PARMS[SEC][SUP].MakeFunctionHist(RFID);
   	FILE_PARMS[SEC][SUP].MakeFunctionHist(XFID);
   	hopera(RFID,"-",XFID,RFID,1.0,1.0);
   }
   if(SHOW_FUNCTION){
   	hplset("HCOL",1);
   	hplot(RFID,"S","HIST",1);
   }
   if(SHOW_PREDICTED_RESI){
   	hplset("PLCI",6);
		hcopy(RFID,TMPID," ");
		hopera(RID,"-",TMPID,TMPID,1.0,1.0);
		hplot(TMPID,"S","HIST",1);
		hdelet(TMPID);
   }

}

//=================================================================
// Calculate average X vs T function 
// This is designed for use with the hbook HBFUN1() routine which
// takes only a single argument. The other arguments needed are
// accessed via the global variables FAST_SEC and FAST_SUP
//=================================================================
float fast_xvst_fct(float *t)
{
	int   layer    = EXCLUDED[FAST_SUP];
	float locangle = AVG[FAST_SEC][FAST_SUP].locangle*DEG2RAD;
	float B        = AVG[FAST_SEC][FAST_SUP].B;
	float doca ,dummy_tmax;

	// Call X vs T function to calculate doca
	// parameters should already be copied to dc package prior
	// to calling hbfun1_()
	dc_xvst_fct_(&FAST_SEC ,&layer ,&locangle ,t ,&B ,&doca ,&dummy_tmax);

	return fabs(doca);
}

//=======================================================
// Create a function histogram with the specified ID
// using the object's parameters at the averages
//=======================================================
void dc_parms_t::MakeFunctionHist(int FID)
{
	static int working=0;

	// Make sure calls by multiple threads don't interfere
	while(working)usleep(1000);
	working=1;
	
	// Copy parms from this object to where DC package will use them
	CopyParmsToDC(this,sector,superlayer);

	// Create function hist and fill it with values
	FAST_SEC=sector;
	FAST_SUP=superlayer;
	if(hexist(FID))hdelet(FID);
	hbfun1(FID,"func",75,TLIM_LOW,TLIM[superlayer],(void*)fast_xvst_fct);

	working=0;
}

void PlotXvsT(void)
{

   int RID,XID,XSID,XFID,TMPID=9999;
   float cell_size=DC_HEXSIZE[EXCLUDED[SUP]];
   float X[2]={cell_size,cell_size},T[2]={TLIM_LOW,TLIM[SUP]};
   
   RID=100+(SEC*10)+SUP;
   XID=RID+100;
   XSID=RID+400;
   XFID=RID+700;
   
   // Setup General plotting options
   hplopt("TIC ",1);

   // Copy the Scatterplot histogram and reset it so
   // we can use it to draw empty axes which everything
   // else can be superimposed on.
   hcopy(XSID,TMPID," ");
   hreset(TMPID," ");
   hplot(TMPID,"SCAT","HIST",1);

   if(SHOW_SCATTER){hplset("HCOL",4);hplot(XSID,"SSCAT","HIST",1);}
   if(SHOW_PROFILE){hplset("HCOL",2);hplot(XID,"S","HIST",1);}
   if(SHOW_ZERO_LINE){ipl(2,T,X);}
   if(SHOW_FUNCTION){
   	hplset("HCOL",1);
   	USER_PARMS[SEC][SUP].MakeFunctionHist(XFID);
   	hplot(XFID,"S","HIST",1);
   }

   // Delete the temporary histogram
   hdelet(TMPID);
}

void PlotDriftTime(void)
{
	int RID,TID;
	float Y[2],T[2];
	float tmax,tzero;
	float ymax,X[2];
	int NOENT;
	char str[256];
	
	RID=100+(SEC*10)+SUP;
	TID=RID+200;
	
	// Setup plotting options and plot histogram
	hplopt("TIC ",1);
	hplset("HCOL",4);
	hplset("HTYP",2);
	if(NOTEBOOK_PAGE == 1){
		hplzom(TID," ",TZERO_PLOT_MIN,TZERO_PLOT_MAX);	// t0 plots limited range
		hplset("HCOL",2);	hplset("HTYP",2);
		hplzom(TID,"S",TZERO_FIT_MIN,TZERO_FIT_MAX);		// re-color fit region red
		TzeroFromLinearFit(TID, 1);							// Find tzero and draw line on screen
	}else{
		hplot(TID," ","HIST",1);	// tmax plots whole range
	}

	// Draw tmax/tzero lines
	ymax=1.1*hmax(TID);
	
	tmax=atoi(Tcl_GetVar(interp,"tmax",TCL_GLOBAL_ONLY));
	Y[0]=0.0; Y[1]=ymax*1.1;	 
	X[0]=X[1]=(float)tmax;
	isplci(2);
	ipl(2,X,Y);

	tzero=atoi(Tcl_GetVar(interp,"tzero",TCL_GLOBAL_ONLY));
	Y[0]=0.0; Y[1]=ymax*1.1;	 
	X[0]=X[1]=(float)tzero;
	isplci(6);
	ipl(2,X,Y);

	// Print Number of events
	iselnt(0);
	ischh(0.015);
	hnoent(TID,&NOENT);
	sprintf(str,"%d entries",NOENT);
	itx(0.75,0.91,str);
}

void PlotResidualProjection(void)
{

   int RID,RPID;
   float X[2]={0.0,0.0},T[2]={TLIM_LOW,TLIM[SUP]};
   float sigma_n=0.0,sigma_w=0.0,mean,amp_ratio;
   char str[256];
   int NOENT;

   RID=100+(SEC*10)+SUP;
   RPID=RID+500;
   
   hnoent(RPID,&NOENT);
   if(NOENT<500)return;
   
   // Fit to double Gaussian
   FindResidualWidths(RPID,&sigma_n,&sigma_w,&mean,&amp_ratio);
   
   // Setup General plotting options
   hplopt("TIC ",1);
   hplset("HCOL",2);
   hplset("HTYP",2);
   hplot(RPID," ","HIST",1);

   // Show sigmas
   iselnt(0);
   ischh(0.020);
   sprintf(str,"[s]?total!  =%d microns",(int)TotalWidth(amp_ratio, sigma_n, sigma_w));
   itx(0.11,0.86 ,str);
   sprintf(str,"[s]?narrow! =%d microns",(int)sigma_n);
   itx(0.11,0.83 ,str);
   sprintf(str,"[s]?wide!   =%d microns",(int)sigma_w);
   itx(0.11,0.80,str);
   ischh(0.015);
   sprintf(str,"%d entries",NOENT);
   itx(0.75,0.91,str);
   
}

int PlotCalcDOCA(ClientData clientData,Tcl_Interp *interp,int argc, const char *argv[])
{
	int HID;
	char str[256];
	float cell_size=DC_HEXSIZE[EXCLUDED[SUP]];
	pthread_t progress_thread;
	float progress=0.0;
	int save_do_tzero_subtraction, save_do_timewalk_correction;

	// Set flags so dc library will do time correction
	save_do_tzero_subtraction = do_tzero_subtraction;
	save_do_timewalk_correction = do_timewalk_correction;
	do_tzero_subtraction = 1;
	do_timewalk_correction = 1;

	// Book histogram
	HID=2000;
	if(hexist(HID))hdelet(HID);
	sprintf(str,"DOCA S%dSL%d",SEC,SUP);
	hbook1(HID,str,100,-1.1*cell_size,1.1*cell_size,0.0);
	
	// Launch thread to fill the t2x tables
	if(REFILL_XVST_TABLE_FOR_DOCA_PLOT){
		progress=0.0;
		pthread_create(&progress_thread,NULL,dc_fill_t2x_table_thread,(void*)&progress);

		// Draw progress clock on GUI since this may take a while
		strcpy(PROGRESS_MESSAGE,"filling tables...");
		ProgressClock((void*)&progress);
		pthread_join(progress_thread,NULL);
	
		// filling the timewalk tables should be quick enough that we don't need a progress bar
		dc_fill_timewalk_table();
		
		REFILL_XVST_TABLE_FOR_DOCA_PLOT = 0;
	}

	// Launch the Ntuple reading thread to fill histogram
	pthread_create(&progress_thread,NULL,Fill_DOCA_From_Ntuple_thread,(void*)&progress);

	// Draw progress bar on GUI since this may take a while
	strcpy(PROGRESS_MESSAGE,"working...");
	ProgressBar(&progress);
	pthread_join(progress_thread,NULL);

	// Plot DOCA histogram
	hplset("HCOL",6);
	hplset("HTYP",2);
	hplot(HID," ","HIST",1);

	// Flush X buffer so screen is completely drawn
	ixupdwi(0);

	// Restore flags time correction flags
	do_tzero_subtraction = save_do_tzero_subtraction;
	do_timewalk_correction = save_do_timewalk_correction;
	
	return TCL_OK;
}

void dc_parms_t::ShowParameters(char *path,int recalc_chisq)
{
	char cmd[256],str[256],win[256];
	map_parms_t *a=&parms.p;
	int i,p,region,section;

	sprintf(cmd,"%s.p1 configure -text \"par 1: %+1.5g\"",path,a->p1);
	Tcl_Evaluate(cmd);
	sprintf(cmd,"%s.p2 configure -text \"par 2: %+1.5g\"",path,a->p2);
	Tcl_Evaluate(cmd);
	sprintf(cmd,"%s.p3 configure -text \"par 3: %+1.5g\"",path,a->p3);
	Tcl_Evaluate(cmd);
	sprintf(cmd,"%s.p4 configure -text \"par 4: %+1.5g\"",path,a->p4);
	Tcl_Evaluate(cmd);
	sprintf(cmd,"%s.tmax configure -text \"tmax : %1.5g\"",path,a->tmax);
	Tcl_Evaluate(cmd);
	sprintf(cmd,"%s.tzero configure -text \"tzero: %1.5g\"",path,a->tzero);
	Tcl_Evaluate(cmd);
	sprintf(cmd,"%s.ff configure -text \"ff   : %1.5g\"",path,a->ff);
	Tcl_Evaluate(cmd);
	sprintf(cmd,"%s.chisq configure -text \"chisq: %1.5g\"",path
		,recalc_chisq ? Chisq():chisq);
	Tcl_Evaluate(cmd);

	// Update expert controls if the window is open
	if(EXPERT_CONTROLS){
		// Timewalk
		strcpy(win,".expert_controls.timewalk.bot");
		for(region=1;region<=3;region++){
			for(p=1;p<=10;p++){
				sprintf(str,"%s.f%d.%c.p%d.en",win,region,p>5 ? 'r':'l',p);
				sprintf(cmd,"%s delete 0 end ; %s insert 0 %6g",str,str
					,region==1 ? tw_parm_r1[p]:region==2 ? tw_parm_r2[p]:tw_parm_r3[p]);
				Tcl_Evaluate(cmd);
			}
			sprintf(str,"%s.f%d.l.fac.en",win,region);
			sprintf(cmd,"%s delete 0 end ; %s insert 0 %g",str,str,tw_fact[region]);
			Tcl_Evaluate(cmd);
			sprintf(str,"%s.f%d.l.tau.en",win,region);
			sprintf(cmd,"%s delete 0 end ; %s insert 0 %g",str,str,tw_tau[region]);
			Tcl_Evaluate(cmd);
			sprintf(str,"%s.f%d.r.betaslope_inner.en",win,region);
			sprintf(cmd,"%s delete 0 end ; %s insert 0 %g",str,str,tw_betaslope[(2*region)-1]);
			Tcl_Evaluate(cmd);
			sprintf(str,"%s.f%d.r.betaslope_outer.en",win,region);
			sprintf(cmd,"%s delete 0 end ; %s insert 0 %g",str,str,tw_betaslope[(2*region)-0]);
			Tcl_Evaluate(cmd);
			sprintf(cmd,"set tw_type %d",tw_functiontype);
			Tcl_Evaluate(cmd);
		}

		// X vs. T
		strcpy(win,".expert_controls.xvst.bot");
		for(p=1;p<=24;p++){
			section = 1 + ((p-1)/4);
			sprintf(str,"%s.f%d.p%d.en",win,section,p);
			sprintf(cmd,"%s delete 0 end ; %s insert 0 %g",str,str,parms.par[p-1]);
			Tcl_Evaluate(cmd);
		}
	}


	Tcl_Evaluate("update");
}

//=====================================================================
// Get the Parameters from the Expert controls windows (if it exists)
//=====================================================================
void dc_parms_t::GetExpertParameters(int get_timewalk, int get_xvst)
{
	char cmd[256],str[256],win[256];
	int i,p,region,section;
	float a;

	// Only try if the window is open
	if(!EXPERT_CONTROLS)return;

	// Timewalk
	if(get_timewalk){
		strcpy(win,".expert_controls.timewalk.bot");
		for(region=1;region<=3;region++){
			for(p=1;p<=10;p++){
				sprintf(cmd,"%s.f%d.%c.p%d.en get",win,region,p>5 ? 'r':'l',p);
				Tcl_Evaluate(cmd);
				a=atof(interp->result);
				if(region==1)tw_parm_r1[p]=a;
				if(region==2)tw_parm_r2[p]=a;
				if(region==3)tw_parm_r3[p]=a;
			}
			sprintf(cmd,"%s.f%d.l.fac.en get",win,region);
			Tcl_Evaluate(cmd);
			tw_fact[region]=atof(interp->result);
			sprintf(cmd,"%s.f%d.l.tau.en get",win,region);
			Tcl_Evaluate(cmd);
			tw_tau[region]=atof(interp->result);
			sprintf(cmd,"%s.f%d.r.betaslope_inner.en get",win,region);
			Tcl_Evaluate(cmd);
			tw_betaslope[(2*region)-1]=atof(interp->result);
			sprintf(cmd,"%s.f%d.r.betaslope_outer.en get",win,region);
			Tcl_Evaluate(cmd);
			tw_betaslope[(2*region)-0]=atof(interp->result);
		}
		tw_functiontype=TW_TYPE;
	}
	
	// X vs. T
	if(get_xvst){
		strcpy(win,".expert_controls.xvst.bot");
		for(p=1;p<=24;p++){
			section = 1 + ((p-1)/4);
			sprintf(cmd,"%s.f%d.p%d.en get",win,section,p);
			Tcl_Evaluate(cmd);
			parms.par[p-1]=atof(interp->result);
		}
		parms.par[23]=XVST_TYPE;
	}

	// Set expert mode flag and update window
	if(get_xvst || get_timewalk){
		USER_PARMS[SEC][SUP].expert_mode=1;
		ShowParameters("$fit_path.novice.fv",1);
	}
}

//=====================================================================
// Entry point from GUI when "Apply" button is hit on Expert Controls
//=====================================================================
int ApplyExpertParameters(ClientData clientData,Tcl_Interp *interp,int argc, const char *argv[])
{
	dc_parms_t orig_parms=USER_PARMS[SEC][SUP],tp;
	int i,regen=0,sec,sup,k;
	int get_timewalk=0,get_xvst=0;
	int bypass_histo_refill=0;

	// Look at arguments to see which "Apply" button was hit (timewalk or xvst)
	for(i=0;i<argc;i++){
		if(strstr(argv[i],"timewalk"))get_timewalk=1;
		if(strstr(argv[i],"xvst"))get_xvst=1;
		if(strstr(argv[i],"bypass_histo_refill"))bypass_histo_refill=1;
	}
	
	// Get parameters from GUI
	USER_PARMS[SEC][SUP].GetExpertParameters(get_timewalk, get_xvst);

	// Check if we need to re-generate histograms (e.g. tzero or timewalk parms changed)
	if(orig_parms.tw_functiontype!=USER_PARMS[SEC][SUP].tw_functiontype)regen=1;
	if(orig_parms.parms.p.tzero!=USER_PARMS[SEC][SUP].parms.p.tzero)regen=1;
	for(i=1;i<=11;i++){
		if(orig_parms.tw_parm_r1[i]!=USER_PARMS[SEC][SUP].tw_parm_r1[i])regen=1;
		if(orig_parms.tw_parm_r2[i]!=USER_PARMS[SEC][SUP].tw_parm_r2[i])regen=1;
		if(orig_parms.tw_parm_r3[i]!=USER_PARMS[SEC][SUP].tw_parm_r3[i])regen=1;
	}
	for(i=1;i<=3;i++){
		if(orig_parms.tw_fact[i]!=USER_PARMS[SEC][SUP].tw_fact[i])regen=1;
		if(orig_parms.tw_tau[i] !=USER_PARMS[SEC][SUP].tw_tau[i] )regen=1;
		if(orig_parms.tw_betaslope[(2*i)-1] !=USER_PARMS[SEC][SUP].tw_betaslope[(2*i)-1] )regen=1;
		if(orig_parms.tw_betaslope[(2*i)-0] !=USER_PARMS[SEC][SUP].tw_betaslope[(2*i)-0] )regen=1;
	}
	if(regen){
		CopyParmsToDC(&USER_PARMS[SEC][SUP],SEC,SUP);
		dc_fill_timewalk_table();
		if(!bypass_histo_refill){
			DATA_FILTER[SUP].stale=1;
			Tcl_Evaluate("set sec $sec");
		}
	}

	// Copy Timewalk parameters to all other sec/sup's for consistancy
	CopyTimewalkParmsToAll(USER_PARMS[SEC][SUP]);

	REFILL_XVST_TABLE_FOR_DOCA_PLOT = 1;
	REPLOT=1;

	return TCL_OK;
}

//=====================================================================
// Entry point from GUI when "Defaults" button is hit on Expert Controls
// Note that this only copies the values to the GUI. The apply button
// must still be pressed to copy them into the USER_PARMS data structure.
//=====================================================================
int SetParametersToDefault(ClientData clientData,Tcl_Interp *interp,int argc, const char *argv[])
{
	int i;
	dc_parms_t tmp_parms;
	int set_timewalk=0,set_xvst=0, bypass_warning=0;
	float *tw_r1, *tw_r2, *tw_r3, *tw_fac, *tw_tau, *tw_betaslope;
	
	float tw_r1_liming[11]={0.0, -9.45, 39.34, -15.03, 11.53, 0.0,  -9.45, 39.34, -15.03, 11.53, 0.0};
	float tw_r2_liming[11]={0.0,-17.34, 53.70, -23.76,  8.85, 0.0, -17.34, 53.70, -23.76,  8.85, 0.0};
	float tw_r3_liming[11]={0.0,-24.10, 72.05, -33.42,  9.87, 0.0, -24.10, 72.05, -33.42,  9.87, 0.0};
	float tw_fac_liming[4]={0.0, 2.0, 2.0, 3.0};
	float tw_tau_liming[4]={0.0, 50.0, 70.0, 100.0};
	float tw_betaslope_liming[7]={0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};

	float tw_r1_vipuli[11]={0.0, 33.228, -24.701,  8.849, -2.912, 1.168, 36.886, -35.177, 16.552, -4.832, 1.334};
	float tw_r2_vipuli[11]={0.0, 89.072, -109.53, 42.027, -4.806, 1.075, 74.749, -89.640, 30.454, -1.434, 0.959};
	float tw_r3_vipuli[11]={0.0, 171.25, -211.79, 73.174, -6.003, 1.477, 102.05, -112.19, 29.206, 0.9799, 1.519};
	float tw_fac_vipuli[4]={0.0, 0.0, 0.0, 0.0};
	float tw_tau_vipuli[4]={0.0, 0.0, 0.0, 0.0};
	float tw_betaslope_vipuli[7]={0.0, 22.0, 22.0, 22.0, 22.0, 22.0, 22.0};

	float tw_r1_mac[11]={0.0, 0.125, 0.15, 0.25, 1.15, 0.0, 0.125, 0.15, 0.25, 1.15, 0.0};
	float tw_r2_mac[11]={0.0, 0.150, 0.20, 0.30, 1.10, 0.0, 0.150, 0.20, 0.30, 1.10, 0.0};
	float tw_r3_mac[11]={0.0, 0.120, 0.20, 0.40, 1.10, 0.0, 0.120, 0.20, 0.40, 1.10, 0.0};
	float tw_fac_mac[4]={0.0, 0.0, 0.0, 0.0};
	float tw_tau_mac[4]={0.0, 0.0, 0.0, 0.0};
	float tw_betaslope_mac[7]={0.0, 16.0, 16.0, 16.0, 16.0, 24.0, 24.0};

	float tw_r1_none[11]={0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
	float tw_r2_none[11]={0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
	float tw_r3_none[11]={0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
	float tw_fac_none[4]={0.0, 0.0, 0.0, 0.0};
	float tw_tau_none[4]={0.0, 0.0, 0.0, 0.0};
	float tw_betaslope_none[7]={0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};

	// Look at arguments to see which "Default" button was hit (timewalk or xvst)
	for(i=0;i<argc;i++){
		if(strstr(argv[i],"timewalk"))set_timewalk=1;
		if(strstr(argv[i],"xvst"))set_xvst=1;
		if(strstr(argv[i],"bypass_warning"))bypass_warning=1;
	}

	// Here we wish to set the defaults for whichever function type is
	// currently set in the expert controls panel. To do this, we first read in
	// all parameters to get the function type. Then, the other
	// parameters are overwritten.
	tmp_parms=USER_PARMS[SEC][SUP];
	tmp_parms.GetExpertParameters(set_timewalk, set_xvst);

	// Overwrite timewalk parameters
	if(set_timewalk){

		switch(tmp_parms.tw_functiontype)
		{
			case DC_TIMEWALK_TYPE_LIMING:
				tw_r1=tw_r1_liming;
				tw_r2=tw_r2_liming;
				tw_r3=tw_r3_liming;
				tw_fac=tw_fac_liming;
				tw_tau=tw_tau_liming;
				tw_betaslope=tw_betaslope_liming;
				break;
			case DC_TIMEWALK_TYPE_VIPULI:
				tw_r1=tw_r1_vipuli;
				tw_r2=tw_r2_vipuli;
				tw_r3=tw_r3_vipuli;
				tw_fac=tw_fac_vipuli;
				tw_tau=tw_tau_vipuli;
				tw_betaslope=tw_betaslope_vipuli;
				break;
			case DC_TIMEWALK_TYPE_MAC:
				tw_r1=tw_r1_mac;
				tw_r2=tw_r2_mac;
				tw_r3=tw_r3_mac;
				tw_fac=tw_fac_mac;
				tw_tau=tw_tau_mac;
				tw_betaslope=tw_betaslope_mac;
				break;
			case DC_TIMEWALK_TYPE_NONE:
				tw_r1=tw_r1_none;
				tw_r2=tw_r2_none;
				tw_r3=tw_r3_none;
				tw_fac=tw_fac_none;
				tw_tau=tw_tau_none;
				tw_betaslope=tw_betaslope_none;
				break;

			default:
				fprintf(stderr," Unsupported timewalk function type(%f) %s:%d\n"
					,USER_PARMS[SEC][SUP].tw_functiontype,__FILE__,__LINE__);
				return TCL_OK;
		}

		for(i=0;i<11;i++){
			tmp_parms.tw_parm_r1[i]=tw_r1[i];
			tmp_parms.tw_parm_r2[i]=tw_r2[i];
			tmp_parms.tw_parm_r3[i]=tw_r3[i];
		}
		for(i=0;i<4;i++){
			tmp_parms.tw_fact[i]=tw_fac[i];
			tmp_parms.tw_tau[i]=tw_tau[i];
		}
		for(i=0;i<7;i++)tmp_parms.tw_betaslope[i]=tw_betaslope[i];

		tmp_parms.ShowParameters("$fit_path.novice.fv",0);
	}

	// Overwrite timewalk parameters
	if(set_xvst){
		
	
		switch((int)tmp_parms.parms.p.xvst_functiontype){
			case DC_XVST_TYPE_LIMING: // If set to Liming, force to Power
				tmp_parms.parms.p.xvst_functiontype = DC_XVST_TYPE_POWER;
			case DC_XVST_TYPE_POWER:
				tmp_parms.parms.p.tzero  = 5.0;
				tmp_parms.parms.p.ff     = 0.96;
				tmp_parms.parms.p.p1     = 0.012781;
				tmp_parms.parms.p.p2     = 0.83176;
				tmp_parms.parms.p.p3     = 1.4;
				tmp_parms.parms.p.p4     = 1.4794;
				tmp_parms.parms.p.layer  = EXCLUDED[tmp_parms.superlayer];
				tmp_parms.parms.p.tau_p1 = 0.36;
				tmp_parms.parms.p.tau_p2 = 2.5;
				tmp_parms.parms.p.tau_p3 = 2.0;
				tmp_parms.parms.p.tmax_correction = 1.1;
				if(tmp_parms.superlayer==3 || tmp_parms.superlayer==4){
					tmp_parms.parms.p.B_p1 = -0.01473;
					tmp_parms.parms.p.B_p2 = 0.7634;
					tmp_parms.parms.p.B_p3 = 0.2547;
					tmp_parms.parms.p.B_p4 = 0.0863;
					tmp_parms.parms.p.tmax_B_p1 = 470.3;
					tmp_parms.parms.p.tmax_B_p2 = 0.0;
					tmp_parms.parms.p.tmax_B_p3 = 0.953;
				}
				break;
			case DC_XVST_TYPE_POLY:
				//break;
			case DC_XVST_TYPE_BESSEL:
				//break;
			case DC_XVST_TYPE_LEGENDRE:
				//break;
			default:
				char mess[512],cmd[768];
				strcpy(mess,"No default parameters are available for the chosen function. ");
				strcat(mess,"Try selecting \"Power\" for the function type. ");
				sprintf(cmd,"tk_dialog .setdefaultserr \"DC3: Caution\" \"%s\" warning 0 ",mess);
				strcat(cmd,"\"OK\"");
				Tcl_Evaluate(cmd);
				set_xvst = 0;
		}
		tmp_parms.ShowParameters("$fit_path.novice.fv",0);
	}

	if((!bypass_warning) &&(set_timewalk || set_xvst)){
		char mess[512],cmd[768];
		strcpy(mess,"Note: You must still hit 'Apply' to use these parameters. ");
		sprintf(cmd,"tk_dialog .setdefaultswarn \"DC3: Caution\" \"%s\" warning 0 ",mess);
		strcat(cmd,"\"OK\"");
		Tcl_Evaluate(cmd);
	}

	return TCL_OK;
}

void dc_parms_t::PrintComparison(dc_parms_t *d,char *mess)
{
	map_parms_t a,b;
	int i,layer;
	
	a=parms.p;
	b=d->parms.p;

	cout<<"\n"<<mess<<"\n";

	cout<<"           sector: "<<sector             <<"\r\t\t\t\t"<<d->sector          <<"\n";
	cout<<"       superlayer: "<<superlayer         <<"\r\t\t\t\t"<<d->superlayer      <<"\n";
	cout<<"           locang: "<<a.locang           <<"\r\t\t\t\t"<<b.locang           <<"\n";
	cout<<"            tzero: "<<a.tzero            <<"\r\t\t\t\t"<<b.tzero            <<"\n";
	cout<<"             tmax: "<<a.tmax             <<"\r\t\t\t\t"<<b.tmax             <<"\n";
	cout<<"                B: "<<a.B                <<"\r\t\t\t\t"<<b.B                <<"\n";
	cout<<"               ff: "<<a.ff               <<"\r\t\t\t\t"<<b.ff               <<"\n";
	cout<<"               p1: "<<a.p1               <<"\r\t\t\t\t"<<b.p1               <<"\n";
	cout<<"               p2: "<<a.p2               <<"\r\t\t\t\t"<<b.p2               <<"\n";
	cout<<"               p3: "<<a.p3               <<"\r\t\t\t\t"<<b.p3               <<"\n";
	cout<<"               p4: "<<a.p4               <<"\r\t\t\t\t"<<b.p4               <<"\n";
	cout<<"            layer: "<<a.layer            <<"\r\t\t\t\t"<<b.layer            <<"\n";
	cout<<"           tau_p1: "<<a.tau_p1           <<"\r\t\t\t\t"<<b.tau_p1           <<"\n";
	cout<<"           tau_p2: "<<a.tau_p2           <<"\r\t\t\t\t"<<b.tau_p2           <<"\n";
	cout<<"           tau_p3: "<<a.tau_p3           <<"\r\t\t\t\t"<<b.tau_p3           <<"\n";
	cout<<"           spare1: "<<a.spare1           <<"\r\t\t\t\t"<<b.spare1           <<"\n";
	cout<<"  tmax_correction: "<<a.tmax_correction  <<"\r\t\t\t\t"<<b.tmax_correction  <<"\n";
	cout<<"             B_p1: "<<a.B_p1             <<"\r\t\t\t\t"<<b.B_p1             <<"\n";
	cout<<"             B_p2: "<<a.B_p2             <<"\r\t\t\t\t"<<b.B_p2             <<"\n";
	cout<<"             B_p3: "<<a.B_p3             <<"\r\t\t\t\t"<<b.B_p3             <<"\n";
	cout<<"             B_p4: "<<a.B_p4             <<"\r\t\t\t\t"<<b.B_p4             <<"\n";
	cout<<"           spare2: "<<a.spare2           <<"\r\t\t\t\t"<<b.spare2           <<"\n";
	cout<<"        tmax_B_p1: "<<a.tmax_B_p1        <<"\r\t\t\t\t"<<b.tmax_B_p1        <<"\n";
	cout<<"        tmax_B_p2: "<<a.tmax_B_p2        <<"\r\t\t\t\t"<<b.tmax_B_p2        <<"\n";
	cout<<"        tmax_B_p3: "<<a.tmax_B_p3        <<"\r\t\t\t\t"<<b.tmax_B_p3        <<"\n";
	cout<<"xvst_functiontype: "<<a.xvst_functiontype<<"\r\t\t\t\t"<<b.xvst_functiontype<<"\n";

	for(i=0;i<6;i++){
		layer=((superlayer-1)*6)+i;
		cout<<"       tmax["<<layer<<"]:"<<tmax[layer]  <<"\r\t\t\t\t"<<d->tmax[layer]<<"\n";
	}
}







