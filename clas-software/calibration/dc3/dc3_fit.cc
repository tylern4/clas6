

#include "dc3.h"

typedef void fcn_type(int *NPAR,double *GRAD,double *FVAL,double *XVAL,int *IFLAG,int *test);

void* FitOne_thread(void* arg);
int MNCOMD(fcn_type* FUNCTION,char *cmd);
int MNPARM(int NUM,char *CHNAM,double STVAL,double STEP,double BND1,double BND2);
fcn_type fcn;

extern "C" {
	void mncomd_(fcn_type *,char *,int *,int *,int);
	void mnparm_(int*,char*,double*,double*,double*,double*,int*,int);
}

int NPARS;
float CHISQVEC[75],CHISQVEC_T[75],CHISQVEC_USED[75];
float FUNCVEC[75],DATAVEC[75];

// These are used to keep track of the best chisq and
// corresponding parms found during the fit. They are
// re-initialized at the beginning of every fit.
dc_parms_t best_parms;

//============================================
// Loop over fitting all sectors/superlayers
//============================================
int FitAll(ClientData clientData,Tcl_Interp *interp,int argc,const char *argv[])
{
	int sec,sup;
	char cmd[256];

	for(sup=1;sup<=6;sup++){
		sprintf(cmd,".middle.left.secsup.sup.sup%d invoke ; update ; update",sup);
		Tcl_Evaluate(cmd);

		FitSuperlayer(clientData,interp,argc,argv);

		if(CANCEL)break;
	}

	return TCL_OK;
}

//============================================
// Loop over fitting all sectors/superlayers
//============================================
int FitSuperlayer(ClientData clientData,Tcl_Interp *interp,int argc,const char *argv[])
{
	int sec;
	char cmd[256];

	for(sec=1;sec<=6;sec++){
		sprintf(cmd,".middle.left.secsup.sec.sec%d invoke ; update ; update",sec);
		Tcl_Evaluate(cmd);
		sprintf(cmd,"$fit_path.novice.fit.fitone invoke ; update ; update");
		Tcl_Evaluate(cmd);

		if(CANCEL)break;
	}

	return TCL_OK;
}


//=============================
// Fit one Sector/Superlayer
//=============================
int FitOne(ClientData clientData,Tcl_Interp *interp,int argc,const char *argv[])
{
	int i;
	map_parms_t *p=&USER_PARMS[SEC][SUP].parms.p;
	static int initialized=0;
	float chisq,last_chisq=1.0E10;
	pthread_t progress_thread;
	float progress=0.0;
	int cntr=0;
	static int working=0;
	char varname[16];

	// Don't let user get impatient
	if(working){
		cout<<"I'm busy, try again later\n";
		return TCL_OK;
	}
	working=1;
	Tcl_Evaluate("set sec $sec ; set cancel 0 ; update ; update");

	// Launch thread to do MINUIT fit
	Tcl_Evaluate("set cancel 0");
   pthread_create(&progress_thread,NULL,FitOne_thread,(void*)&progress);

   // Draw progress clock on GUI since this may take a while
   // We call the ProgressClockStart(), ProgressClockStop()
   // routines here directly since we want to update the parameters
   // continuously as well
   sprintf(PROGRESS_MESSAGE,"fitting S%dSL%d...",SEC,SUP);
   ProgressClockStart();
   while(progress<1.0){
   	ProgressClockUpdate();
		if(((cntr++)%1)==0)
			USER_PARMS[SEC][SUP].ShowParameters("$fit_path.novice.fv",0);
		USLEEP(100000);
		if(CANCEL)break;
	}
   ProgressClockStop();
   pthread_join(progress_thread,NULL);

	if(EXPERT)USER_PARMS[SEC][SUP].expert_mode=1;
	REPLOT=1;

	// Set button for writing the sector/superlayer to map appropriately
	sprintf(varname,"s%dsl%d",SEC,SUP);
	Tcl_SetVar(interp,varname,(char*)(USER_PARMS[SEC][SUP].chisq<2.0 ? "1":"0"),TCL_GLOBAL_ONLY);

	REFILL_XVST_TABLE_FOR_DOCA_PLOT = 1;
	working=0;
	
	return TCL_OK;
}

//==============================================================
// This does most of the work on setting up and doing the fit.
// This routine is called as a thread so the main thread can
// update the Tcl/Tk control window while fitting.
//==============================================================
void* FitOne_thread(void* arg)
{
	int i;
	map_parms_t *p=&USER_PARMS[SEC][SUP].parms.p;
	static int initialized=0;
	float chisq,last_chisq,inital_chisq;
	pthread_t progress_thread;
	float *progress=(float*)arg;
	dc_parms_t best_parms;
	
	//Setup MINUIT
	if(!initialized){mninit(0,0,0);initialized=1;}
   MNCOMD(fcn,"CLEAR");
   MNCOMD(fcn,"SET PRI -1");
   MNCOMD(fcn,"SET NOWARNINGS");
   MNCOMD(fcn,"SET BATCH");
   MNCOMD(fcn,"SET STRATEGY 2");

	// Setup parameters
	NPARS=0;
	MNPARM(1,"par1" ,p->p1    ,0.0 ,0.0 ,0.0);
	MNPARM(2,"par2" ,p->p2    ,0.0 ,0.0 ,0.0);
	MNPARM(3,"par3" ,p->p3    ,0.0 ,0.0 ,0.0);
	MNPARM(4,"par4" ,p->p4    ,0.0 ,0.0 ,0.0);
	MNPARM(5,"ff"   ,p->ff    ,0.0 ,0.0 ,0.0);
	MNPARM(6,"tmax" ,p->tmax  ,5.0 ,0.0 ,0.0);
	MNPARM(7,"tzero",p->tzero ,1.0 ,0.0 ,0.0);

	// Hold parameters
	if(HOLD_PAR1 )MNCOMD(fcn,"FIX 1");
	if(HOLD_PAR2 )MNCOMD(fcn,"FIX 2");
	if(HOLD_PAR3 )MNCOMD(fcn,"FIX 3");
	if(HOLD_PAR4 )MNCOMD(fcn,"FIX 4");
	if(HOLD_FF   )MNCOMD(fcn,"FIX 5");
	if(HOLD_TMAX )MNCOMD(fcn,"FIX 6");
	if(HOLD_TZERO)MNCOMD(fcn,"FIX 7");

	// Copy averages to parms area. This is a little odd. We want the average
	// local angle and B-field for this data set to be written as the parameters
	// in the xvst_parms section. However, the initial parameters (from the hbook
	// file or the map) will likely have different values for these averages.
	// This means the only way to ensure an identically zero residual to start
	// with, is to use the old averages. So... by changing these to the new averages,
	// we're really not starting from an identically zero residual.
	USER_PARMS[SEC][SUP].parms.p.locang = AVG[SEC][SUP].locangle;
	USER_PARMS[SEC][SUP].parms.p.B      = (SUP==3 || SUP==4) ? AVG[SEC][SUP].B:0.0;
	best_parms=USER_PARMS[SEC][SUP];

	// Initialize last_chisq
	inital_chisq=last_chisq=USER_PARMS[SEC][SUP].Chisq();

	// Search parameter space (if selected)
	if(SEEK_FIRST)MNCOMD(fcn,"SEEK 5000 3");

	// Do The fit
	for(i=0;i<50;i++){
		MNCOMD(fcn,"MINIMIZE 20000");

		chisq=USER_PARMS[SEC][SUP].Chisq();
		if(chisq<last_chisq)best_parms=USER_PARMS[SEC][SUP];
		if(fabs(chisq-last_chisq)<1.0E-5)break;
		last_chisq=chisq;
		if(CANCEL)break;
	}
	printf("S%dSL%d %s after %d iterations\n"
		,SEC
		,SUP
		,(inital_chisq>last_chisq ? "converged":"failed")
		,i);
		
	USER_PARMS[SEC][SUP]=best_parms;

	*progress=1.0;
	pthread_exit(NULL);
}

//=====================================================
// Calculate X vs T function using object's parameters
//=====================================================
float dc_parms_t::xvst_fct(ntuple_row_t *row)
{
	int   layer    = (int)row->layer;
	float locangle = row->locangle*DEG2RAD;
	float time     = row->time;
	float B        = row->B;
	float doca ,dummy_tmax;

	// Copy parms from this object to where DC package will use them
	CopyParmsToDC(this,sector,superlayer);

	// Call X vs T function to calculate doca	
	dc_xvst_fct_(&sector ,&layer ,&locangle ,&time ,&B ,&doca ,&dummy_tmax);

	return fabs(doca);
}


//=====================================
// Wrapper for minuit mncomd command
//=====================================
int MNCOMD(fcn_type* FUNCTION,char *cmd)
{
   int ICONDN;
   int zero=0;

   mncomd_(FUNCTION,cmd,&ICONDN,&zero,strlen(cmd));
   return ICONDN;
}
//=====================================
// Wrapper for minuit mnparm command
//=====================================
int MNPARM(int NUM,char *CHNAM,double STVAL,double STEP,double BND1,double BND2)
{
   int IERFLG;

   NPARS++;
   if(STEP==0.0){
		STEP=STVAL*0.05;
		if(STEP==0.0)STEP=0.1;
   }
   STEP=fabs(STEP);
   
   mnparm_(&NUM,CHNAM,&STVAL,&STEP,&BND1,&BND2,&IERFLG,strlen(CHNAM));

   return IERFLG;
}

//===========================================================================
// Wrapper for Chisq function below. This is used for non-speed-critical
// parts of the program while fitting will call the Chisq(float) directly
//===========================================================================
float dc_parms_t::Chisq(void)
{
	int i;
	int RID,XID,TNID;
	float err[75],thist[75];
	float dt;
	
	RID=100+superlayer+(sector*10);
   XID=RID+100;
   TNID=RID+800;

   // Get data from histogram
   if(SHOW_RESIDUAL)
   	hunpak(RID,DATAVEC,"HIST",0);
   else
   	hunpak(XID,DATAVEC,"HIST",0);

	hunpak(TNID,thist,"HIST",0);
	// Calculate error array
	for(i=0;i<75;i++)err[i]=0.0300;

   return Chisq(DATAVEC,err);
}

//=====================================================================
// Calculate Chisq using objects parameters and the data arrays passed
//=====================================================================
float dc_parms_t::Chisq(float *data,float *err)
{
	int i;
	float chisq=0.0;
	float distold=0.0,distnew;
	int RID,RFID,XFID,FID;
	float t=0.0,dt=0.0;
	static int working=0;

	while(working)USLEEP(1000);
	working=1;

   RID=100+(sector*10)+superlayer;
   RFID=RID+600;
   XFID=RID+700;
   
	// Use function histograms
	MakeFunctionHist(XFID);
	if(SHOW_RESIDUAL){
   	FILE_PARMS[sector][superlayer].MakeFunctionHist(RFID);
		hopera(XFID,"-",RFID,RFID,1.0,1.0);
		FID=RFID;
	}else{
		FID=XFID;
	}
	hunpak(FID,FUNCVEC,"HIST",0);

	// Now, calculate chisq vector. Include all finite values in chisq.
	DOF=0;
	hix(FID,1,&t);
	hix(FID,2,&dt);
	dt-=t;
	if(dt==0.0)printf("Error getting bin width for FID=%d\n",FID);
	t+=dt/2.0;
	
	for(i=0;i<75;i++){

		t+=dt; // move to center of time bin
		CHISQVEC_T[i]=t;
		CHISQVEC[i]=0.0;
		CHISQVEC_USED[i]=0;
		
		// time cut in filter box
		if(ISCUT.time!=0.0){	
			if(t>CUTMAX.time)continue;
			if(t<CUTMIN.time)continue;
		}

		// std time cut
		if(STD_TCUT){
			if(t>STD_TCUT_HI[sector][superlayer])continue;
			if(t<STD_TCUT_LOW[sector][superlayer])continue;
		}
		
		CHISQVEC[i]=pow((double)(FUNCVEC[i]-data[i])/err[i],2.0);
		if(finite(CHISQVEC[i])){
			chisq+=CHISQVEC[i];
			DOF++;
			CHISQVEC_USED[i]=1;
		}
	}
	for(i=0;i<75;i++)CHISQVEC[i]/=DOF;

	working=0;

	this->chisq=chisq/(float)DOF;

	return this->chisq;
}


//=================================
// Routine compatible with MINUIT 
//=================================
void fcn(int *NPAR,double *GRAD,double *FVAL,double *XVAL,int *IFLAG,int *test)
{
	dc_parms_t parms=USER_PARMS[SEC][SUP];
	map_parms_t *p=&parms.parms.p;

	if(CANCEL){*FVAL=1000000.0;return;}

	p->p1   =XVAL[0];	
	p->p2   =XVAL[1];	
	p->p3   =XVAL[2];	
	p->p4   =XVAL[3];	
	p->ff   =XVAL[4];	
	p->tmax =XVAL[5];	
	p->tzero=XVAL[6];	

	FillTmaxLayers(&parms);

	*FVAL=parms.Chisq();

	if(*FVAL<USER_PARMS[SEC][SUP].chisq){
		USER_PARMS[SEC][SUP]=parms;
		REPLOT=1;
	}
}

//===============================================================
// Reset USER_PARMS for the current SEC/SUP to the file values
// (This disgards the results of the last fit for this sec/sup)
//===============================================================
int ResetUserParms(ClientData clientData,Tcl_Interp *interp,int argc,const char *argv[])
{
	USER_PARMS[SEC][SUP]=FILE_PARMS[SEC][SUP];

	REFILL_XVST_TABLE_FOR_DOCA_PLOT = 1;
	REPLOT=1;

	return TCL_OK;
}

//==========================================
// Print the chisq vector from the last fit
//==========================================
int ChisqVector(ClientData clientData,Tcl_Interp *interp,int argc,const char *argv[])
{
	int i;

	cout<<"\n\n Chisq vector from last fit:\n\n";
	for(i=0;i<75;i++){
		cout<<(CHISQVEC_USED[i] ? "*":" ");
		cout<<"chisqvect["<<i<<"]="<<CHISQVEC[i]<<"  t="<<CHISQVEC_T[i]<<"\n";
	}
	cout<<"\n";
	
	return TCL_OK;
}


//============================================
// Print the chisq table summarizing all fits
//============================================
int ChisqTable(ClientData clientData,Tcl_Interp *interp,int argc,const char *argv[])
{
	int sec,sup;

   printf("\n\n\t\tCHI-SQ Summary\n");
   printf("      ");
   for(sec=1;sec<=6;sec++)printf("\tsec%d",sec);
   for(sup=1;sup<=6;sup++){
      printf("\nsl%d : ",sup);
      for(sec=1;sec<=6;sec++){
         if(USER_PARMS[sec][sup].Chisq()>3.0)cout<<ansi_red;
         if(USER_PARMS[sec][sup].chisq==0.0)cout<<ansi_red;
         if(!finite(USER_PARMS[sec][sup].chisq))cout<<ansi_red;
            
         printf("\t%4.2lf",USER_PARMS[sec][sup].chisq);
         cout<<ansi_normal;
      }
   }
   printf("\n");
	return TCL_OK;
}


