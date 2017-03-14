

#include "dc3.h"
#include "tcl_procs.h"




//======================================================
// This routine is called by Tcl/Tk whenever the value
// of tzero or tmax is changed.
//======================================================
char* NewTzeroTmax(ClientData clientData,Tcl_Interp *interp
   ,const char* name1,const char* name2,int flags)
{   
   int i;
   int tmax;
   int tzero;
   float hist[500];
   float tmax_frac,tzero_frac;
   float binwidth;
   int tmax_bin,tzero_bin;
   int RID,TID;
   
   // Get values from GUI
   tmax=atoi(Tcl_GetVar(interp,"tmax",TCL_GLOBAL_ONLY));
   tzero=atoi(Tcl_GetVar(interp,"tzero",TCL_GLOBAL_ONLY));

   // Copy GUI values into object
   USER_PARMS[SEC][SUP].parms.p.tmax=(float)tmax;
   USER_PARMS[SEC][SUP].parms.p.tzero=(float)tzero;
   FillTmaxLayers(SEC,SUP);
   
   // Get contents of drift time histogram
   RID=100+(SEC*10)+SUP;
   TID=RID+200;
   hunpak(TID,hist,"HIST",0);
   
   // Calculate bin for tmax/tzero
   binwidth=(float)(TLIMT[SUP] - TLIM_LOW)/500.0;
   tmax_bin=(int)(((float)tmax-(TLIM_LOW+(binwidth/2.0)))/binwidth);
   tzero_bin=(int)(((float)tzero-(TLIM_LOW+(binwidth/2.0)))/binwidth);
   
   // Ensure the indexes are inside the bounds of the array
   tmax_bin=tmax_bin<0 ? 0:tmax_bin>499 ? 499:tmax_bin;
   tzero_bin=tzero_bin<0 ? 0:tzero_bin>499 ? 499:tzero_bin;
   
   // Integrate histogram
   for(i=1;i<500;i++)hist[i]+=hist[i-1];
   
   // Find integrals at tmax and tzero
   tmax_frac=hist[tmax_bin]/hist[499];
   tzero_frac=hist[tzero_bin]/hist[499];
   
   // Update the GUI
   sprintf(TMAX_FRAC,"%3.3f",tmax_frac);
   sprintf(TZERO_FRAC,"%3.3f",tzero_frac);
   Tcl_UpdateLinkedVar(interp,"tmax_frac");
   Tcl_UpdateLinkedVar(interp,"tzero_frac");

   // Set "expert" flag (if appropriate)
   if(EXPERT)USER_PARMS[SEC][SUP].expert_mode=1;
   
   // Tell Replot thread to replot
   REPLOT=1;

   return NULL;
}

//===================================================
// Find postion of a given fraction of integral
//===================================================
int FindTFraction(ClientData clientData,Tcl_Interp *interp,int argc, const char *argv[])
{
   int i;
   float hist[500];
   float t,t_frac;
   float binwidth;
   int RID,TID;
   char cmd[256];
   
   // Get the target fraction
   t_frac=atof(argv[1]);
   
   // Get contents of drift time histogram
   RID=100+(SEC*10)+SUP;
   TID=RID+200;

	// Find the integral fraction
   t=FindTFractionFromHID(t_frac,TID);

   // Write new value to Tcl variavble
   sprintf(cmd,"set %s %d",argv[2],(int)t);
   Tcl_Evaluate(cmd);

   return TCL_OK;
}

//========================================================
// Find the point in a time histogram where the integral
// is the dpecified fraction of the total
//========================================================
float FindTFractionFromHID(float t_frac,int TID)
{
	int i;
	float binwidth;
	float hist[500];
	float t;
	
   // Get contents of drift time histogram
   hunpak(TID,hist,"HIST",0);
   
   // Calculate binwidth
   binwidth=(float)(TLIMT[SUP] - TLIM_LOW)/500.0;
   
   // Integrate histogram
   for(i=1;i<500;i++)hist[i]+=hist[i-1];
   
   // Find t at t_frac of integral
   for(i=0;i<500;i++)if(hist[i]/hist[499] > t_frac)break;
   t=TLIM_LOW+(binwidth*(float)i);

   return t;
}


//==============================================================
// Copy the value of tmax in the parms area to the tmax area
// and calculate the appropriate tmaxes for the rest of the
// layers in the superlayer
// =============================================================
void FillTmaxLayers(dc_parms_t *dc_parms)
{
	int i,layer;
	float tmax,corr;
	int sec,sup;
	float tzero_file,tzero_parms;

	sec=dc_parms->sector;
	sup=dc_parms->superlayer;

	// Since this program keeps tmax as an integer for the excluded
	// layers and the Map values have no such restriction, I add the
	// difference of the map values onto whatever it is set to here
	// so that the initial residual distribtuions will be identically
	// zero.
	dc_parms->parms.p.tmax = floor(dc_parms->parms.p.tmax)
		+ FILE_PARMS[sec][sup].parms.p.tmax-floor(FILE_PARMS[sec][sup].parms.p.tmax);

	// Similar trick for Tzero's .(A little more complicated since tzero can be negative)
	tzero_file  = fabs(FILE_PARMS[sec][sup].parms.p.tzero);
	tzero_parms = fabs(dc_parms->parms.p.tzero);
	tzero_parms = floor(tzero_parms) + (tzero_file - floor(tzero_file) );
	if(dc_parms->parms.p.tzero < 0.0) tzero_parms = -tzero_parms;
	dc_parms->parms.p.tzero = tzero_parms;

	// Get Tmax and correction factor from USER_PARMS object
	tmax=dc_parms->parms.p.tmax;
	corr=1.0+(dc_parms->parms.p.tmax_correction/100.0);

	// Tmax should be measured relative to tzero. The tmax finder works on
	// the histogram of UN-corrected times and so should have tzero subtracted.
	// Doing this here means the values in the t_max subsystem of the map will
	// be different from what is in the xvst_params sections. This is OK since
	// the official place is the t_max subsystem.
	tmax-=dc_parms->parms.p.tzero;

	for(i=0;i<6;i++){
		layer=((sup-1)*6)+i;

		dc_parms->tmax[layer]=
			tmax*pow(corr,(float)(layer+1-EXCLUDED[sup]));

		if(layer==4 || layer==5)dc_parms->tmax[layer]=0.0;
	}

}

//==================================================
// Wrapper for above filling USER_PARMS for sec/sup
//==================================================
void FillTmaxLayers(int sec,int sup)
{
	FillTmaxLayers(&USER_PARMS[sec][sup]);
}

//==========================================================
// Reset Tmax and Tzero values to those in the hbook file
//==========================================================
int ResetTValue(ClientData clientData,Tcl_Interp *interp,int argc, const char *argv[])
{
	char cmd[256];

	// Copy value from FILE_PARMS to USER_PARMS
	USER_PARMS[SEC][SUP].parms.p.tmax  = FILE_PARMS[SEC][SUP].parms.p.tmax;
   USER_PARMS[SEC][SUP].parms.p.tzero = FILE_PARMS[SEC][SUP].parms.p.tzero;
   FillTmaxLayers(SEC,SUP);

   // Make Tcl update values displayed
   sprintf(cmd,"set tmax %d",(int)USER_PARMS[SEC][SUP].parms.p.tmax);
   Tcl_Evaluate(cmd);
   sprintf(cmd,"set tzero %d",(int)USER_PARMS[SEC][SUP].parms.p.tzero);
   Tcl_Evaluate(cmd);

   return TCL_OK;
}

//===============================================================
// Same as above except it is done for all sectors/superlayers
//===============================================================
int ResetTValues(ClientData clientData,Tcl_Interp *interp,int argc, const char *argv[])
{
	char cmd[256];
	int sec,sup;

	for(sec=1;sec<=6;sec++){
		for(sup=1;sup<=6;sup++){
			// Copy value from FILE_PARMS to USER_PARMS
			USER_PARMS[sec][sup].parms.p.tmax  = FILE_PARMS[sec][sup].parms.p.tmax;
   		USER_PARMS[sec][sup].parms.p.tzero = FILE_PARMS[sec][sup].parms.p.tzero;
   		FillTmaxLayers(sec,sup);
   	}
   }

   // Make Tcl update values displayed
   return ResetTValue(clientData,interp,argc,argv);
}

//=======================================================================
// Fit a line to the left edge of the currently selected time plot
// between two limits. This is used for tzero finding.
//=======================================================================
float TzeroFromLinearFit(int hid, int plot)
{
	int i;
	float y,x1,x2,x,hist[500];
	float xavg,x2avg,yavg,xyavg,N;
	float slope,intercept;

	// parameters of linear fit are calculated directly from points
	// with having to search for minimum chisq
	xavg=x2avg=yavg=xyavg=0.0;
	hunpak(hid,hist,"HIST",0);
	for (i=TZERO_FIT_MIN; i<=TZERO_FIT_MAX ;i++){
		y = hist[i-1];
		hix(hid,i,&x1);
		hix(hid,i+1,&x2);
		x = (x1+x2)/2.0;

		xavg	+= x;
		x2avg	+= x*x;
		yavg	+= y;
		xyavg	+= x*y;
	}
	N = (float)(TZERO_FIT_MAX - TZERO_FIT_MIN + 1);
	xavg	/= N;
	x2avg	/= N;
	yavg	/= N;
	xyavg	/= N;

	slope = (xyavg - xavg*yavg)/(x2avg - xavg*xavg);
	intercept = yavg - slope*xavg;

	// Draw line on plot showing "fit" results
	float X[2],Y[2];
	Y[0]=0.0;				X[0]=(Y[0]-intercept)/slope;
	Y[1]=1.1*hmax(hid);	X[1]=(Y[1]-intercept)/slope;
	if(plot){
		isplci(1);
		ipl(2,X,Y);
	}

	return X[0];
}

//===================================================================
// Set the tzero value to that found via linear fit to left edge
//===================================================================
int SetTzeroFromLinearFit(ClientData clientData,Tcl_Interp *interp,int argc, const char *argv[])
{
	int TID,RID;
	float tzero;
	int sup,sec, sec_min,sec_max,sup_min,sup_max;

	if(argc<2){printf("%s:%d not enough arguments\n",__FILE__,__LINE__);return TCL_OK;}
	
	if(!strcmp(argv[1],"one")){
		sec_min = sec_max = SEC;
		sup_min = sup_max = SUP;
	}else if(!strcmp(argv[1],"sup")){
		sup_min = sup_max = SUP;
		sec_min = 1;
		sec_max = 6;
	}else{
		sec_min = sup_min = 1;
		sec_max = sup_max = 6;
	}

	for(sup=sup_min; sup<=sup_max ; sup++){
		for(sec=sec_min; sec<=sec_max ; sec++){
			char cmd[256];

			sprintf(cmd,"set sec %d ; set sup %d ; update ; update",sec,sup);
			Tcl_GlobalEval(interp,cmd);
			
			RID=100+(sec*10)+sup;
			TID=RID+200;
			tzero = TzeroFromLinearFit(TID, 0);

			sprintf(cmd,"set tzero %d ; update ; update",(int)tzero);
			Tcl_GlobalEval(interp,cmd);
		}
	}

	return TCL_OK;
}


//============================================
// Print the Tzero table summarizing all fits
//  added Philip Coltharp
//============================================
int TzeroTable(ClientData clientData,Tcl_Interp *interp,int argc, const char *argv[])
{
	int sec,sup;

   printf("\n\n\t\tTzero Summary\n");
   printf("      ");
   for(sec=1;sec<=6;sec++)printf("\tsec%d",sec);
   for(sup=1;sup<=6;sup++){
      printf("\nsl%d : ",sup);
      for(sec=1;sec<=6;sec++)
         printf("\t%4.2lf",USER_PARMS[sec][sup].parms.p.tzero);
   }
   printf("\n");
	return TCL_OK;
}


//============================================
// Print the Tmax table summarizing all fits
//  added Philip Coltharp
//============================================
int TmaxTable(ClientData clientData,Tcl_Interp *interp,int argc, const char *argv[])
{
	int sec,sup;

   printf("\n\n\t\tTmax Summary\n");
   printf("      ");
   for(sec=1;sec<=6;sec++)printf("\tsec%d",sec);
   printf("\tFrac");
   for(sup=1;sup<=6;sup++){
      printf("\nsl%d : ",sup);
      for(sec=1;sec<=6;sec++)
         printf("\t%4.2lf",USER_PARMS[sec][sup].parms.p.tmax);
   }
   if (sec==2) printf("\t%s", TMAX_REGION1_AUTOFRAC_INIT);
   if (sec==4) printf("\t%s", TMAX_REGION2_AUTOFRAC_INIT);
   if (sec==6) printf("\t%s", TMAX_REGION3_AUTOFRAC_INIT);
   printf("\n");
	return TCL_OK;
}


