

#include <dc.h>
#include <map_manager.h>
#include <ntypes.h>
#include <bostypes.h>

int DC_DCLIB_VERSION_MAJOR = 2;
int DC_DCLIB_VERSION_MINOR = 10;
int DC_DCH_VERSION_MAJOR   = DC_DCH_VERSION_MAJOR_h;
int DC_DCH_VERSION_MINOR   = DC_DCH_VERSION_MINOR_h;



#if 0
/* this may not be the best way to this but...*/
float beta_,B_,locangle_,tmax_,tzero_,ff_;
int sup_,sec_,layer_;

int zero=0,one=1,two=2,three=3,four=4,five=5,six=6,seven=7;
int eight=8,nine=9,ten=10,eleven=11,twelve=12,thirteen=13;
int fourteen=14,fifteen=15,sixteen=16,seventeen=17;
float fzero=0.0,fone=1.0,ftwo=2.0,fthree=3.0,ffour=4.0,ffive=5.0;
float fsix=6.0,fseven=7.0,feight=8.0,fnine=9.0,ften=10.0;
#endif

int _dc_run_number_=-1;
int excluded[7]={0,3,9,16,22,27,33};

/* Flags. Some used purely by calibration programs. */
int DC_XVST_INITIALIZED   = 0;
int DC_XVST_PRINT_TABLE   = 0;
int DC_FILL_XVST_TABLE    = 1;
int DC_SILENT             = 0;
int DC_XVST_VERBOSE       = 0;
int DC_XVST_PRINT_PAR_SEC = 0;
int DC_XVST_PRINT_PAR_SUP = 0;
int program_is_dc_calib_check=0;

/* Tables kept in GLOBAL variables */
float DC_HEXSIZE[MAXLAYER]; 
float DC_XVST_TABLE_R1E[n_lang_bins][n1_dtime_bins][r1layers][MAXSECTOR];
float DC_XVST_TABLE_R2E[n_mag_bins][n_lang_bins][n2_dtime_bins][r2layers][MAXSECTOR];
float DC_XVST_TABLE_R3E[n_lang_bins][n3_dtime_bins][r3layers][MAXSECTOR];
float DC_SIGMA_TABLE[n_rdoca_bins][MAXSUPER];


xvst_parms_t map_parms[7][7];
xvst_parms_t *xvst_parms=&map_parms[0][0];

/* internal routine declarations */
double dc_factorial(int N);
double dc_Pn(int n,double x);


/* dc_drift_parm_SLx(1):  average local angle (the real local angle has a - sign.)
c dc_drift_parm_SLx(2):  Tzero (0.0)
c dc_drift_parm_SLx(3):  correction factor for Tmax(Sector,Layer)
c dc_drift_parm_SLx(4):  average magnetic field strength
c dc_drift_parm_SLx(5):  effective cell size (unit: 1)
c dc_drift_parm_SLx(6):  para 1
c dc_drift_parm_SLx(7):  para 2
c dc_drift_parm_SLx(8):  para 3
c dc_drift_parm_SLx(9):  para 4
c dc_drift_parm_SLx(10): layer number for the parameters
c dc_drift_parm_SLx(11): para 1 for angular dep (tau)
c dc_drift_parm_SLx(12): para 2 for angular dep
c dc_drift_parm_SLx(13): para 3 for angular dep
c dc_drift_parm_SLx(14): spare
c dc_drift_parm_SLx(15): correction to maximum drift time per layer in %
c dc_drift_parm_SLx(16): para 1 for R2 B dep
c dc_drift_parm_SLx(17): para 2 for R2 B dep
c dc_drift_parm_SLx(18): para 3 for R2 B dep
c dc_drift_parm_SLx(19): para 4 for R2 B dep
c dc_drift_parm_SLx(20): spare
c dc_drift_parm_SLx(21): para 1 for R2 tmax B dep
c dc_drift_parm_SLx(22): para 2 for R2 tmax B dep
c dc_drift_parm_SLx(23): para 3 for R2 tmax B dep
c dc_drift_parm_SLx(24): spare
*/

float tzero_liming[6][6]={
  { 10.0,    10.0,    10.0,    10.0,    10.0,    10.0 },
  { 11.5,    11.5,    11.5,    11.5,    11.5,    11.5 },
  {  7.5,     7.5,     7.5,     7.5,     7.5,     7.5 },
  { 10.5,    10.5,    10.5,    10.5,    10.5,    10.5 },
  {  9.5,     9.5,     9.5,     9.5,     9.5,     9.5  },
  { 12.0,    12.0,    12.0,    12.0,    12.0,    12.0 } };


/*============================================================*
 * Initialize the DC package by reading in map values and
 * filling tables.
 *============================================================*/
void dc_xvst_init_(int * runno)
{
	int i,j;
   char subsys[256], item[256], mapfile[256]="DC_DOCA.map";
   char *ptr;
   int firsttime;
   float *par;
   float *tmax_map;
   int sec,sup;

	/* Check environment for debugging flags */
	if( (ptr=getenv("DC_DEBUG_PRINT_VERSIONS")) ){
		printf("\nDC: Version Info: dclib=%d.%d  dc.h=%d.%d\n\n"
			,DC_DCLIB_VERSION_MAJOR,DC_DCLIB_VERSION_MINOR
			,DC_DCH_VERSION_MAJOR,DC_DCH_VERSION_MINOR);
	}
	if( (ptr=getenv("DC_DEBUG_PRINT_XVST_TABLE"    )) )DC_XVST_PRINT_TABLE=1;
	if( (ptr=getenv("DC_DEBUG_PRINT_TIMEWALK_TABLE")) )DC_TIMEWALK_PRINT_TABLE=1;
	if( (ptr=getenv("DC_DEBUG_XVST_VERBOSE")) )DC_XVST_VERBOSE = strlen(ptr)==0 ? 1:atoi(ptr);
	if( (ptr=getenv("DC_DEBUG_TIMEWALK_VERBOSE")) )DC_TIMEWALK_VERBOSE = strlen(ptr)==0 ? 1:atoi(ptr);
	if( (ptr=getenv("DC_DEBUG_XVST_PARAMETERS")) ){
		DC_XVST_PRINT_PAR_SEC = atoi(ptr)/10;
		DC_XVST_PRINT_PAR_SUP = atoi(ptr)%10;
	}
	if( (ptr=getenv("DC_DEBUG_TIMEWALK_PARAMETERS")) )DC_TIMEWALK_PRINT_PAR = 1;
	
   /* put run number in global variable (mainly so dc_calib_check */
   /* can use recsis timewalk with homegrown rernev,...           */
   _dc_run_number_=*runno;
   
   /* make sure cell sizes get defined */
   xvst_set_cell_sizes();

   if( (ptr=getenv("CLAS_PARMS")) )sprintf(mapfile,"%s/Maps/DC_DOCA.map",ptr);

   if(!DC_SILENT)
   	fprintf(stderr,"DC:Reading DC Map parms for run %d:\n",*runno);fflush(stdout);
   /* read xvst parms and tmax values */
   for(sec=1;sec<=6;sec++){
      /* redundancy here is required */
      for(sup=1;sup<=6;sup++){
         xvst_parms=&map_parms[sec][sup];

         par      = xvst_parms->map_parms;
         tmax_map = xvst_parms->tmax;

         sprintf(subsys,"t_max");
         sprintf(item,"Sector%d",sec);
         map_get_float(mapfile,subsys,item,36,tmax_map,*runno,&firsttime);
         xvst_parms->tmax_runno=firsttime;

         sprintf(subsys,"xvst_par_Sect%d",sec);
         if(sec==1)strcpy(subsys,"xvst_params");
         sprintf(item,"SL%d",sup);
         map_get_float(mapfile,subsys,item,24,par,*runno,&firsttime);
         xvst_parms->xvst_runno=firsttime;
        
         /* replace tmax with the proper one */
         xvst_parms->map_parms[2]=xvst_parms->tmax[excluded[sup]-1];

			/* Print map parms to screen. The default values of DC_XVST_PRINT_PAR_SEC */
			/* and DC_XVST_PRINT_PAR_SUP are set to zero so this should not actually  */
			/* print anything unless explictly set through the environment variables  */
			if(sec==DC_XVST_PRINT_PAR_SEC && sup==DC_XVST_PRINT_PAR_SUP){
				printf("\n\nDC XVST parms for Sector %d Superlayer %d\n\n",sec,sup);
				for(j=0;j<8;j++){
					for(i=0;i<3;i++)printf("par[%2d] = %+4.4f  ",8*i+j+1,par[8*i+j]);
					printf("\n");
				}
				printf("\n\nDC Tmax parms for Sector %d Superlayer %d\n\n",sec,sup);
				for(j=0;j<12;j++){
					for(i=0;i<3;i++)printf("par[%2d] = %+4.4f  ",12*i+j+1,tmax_map[12*i+j]);
					printf("\n");
				}
				printf("\n");
			}

      }
      if(!DC_SILENT)fprintf(stderr,"DC:\tSector %d (maprun %d)\n",sec,firsttime);
   }
   
   DC_XVST_INITIALIZED=1;

   /* initialize timewalk */
   dc_timewalk_init(_dc_run_number_);

   /* initialize sigma */
   dc_sigma_init(_dc_run_number_);

   /* Fill xvst table */
   if(DC_FILL_XVST_TABLE)dc_fill_xvst_table();

   if(!DC_SILENT)fprintf(stderr,"DC: Initialization finished for run %d\n",*runno);
}

/*===========================================
 * Fill the time to DIST conversion table.
 *===========================================*/
void dc_fill_xvst_table(void)
{
	int j,k,l,m;
	int sec,layer;
	float alpha,tdrift,B,tmax,doca;
	float locang_binwidth=(langle_value_hi-langle_value_lo)/( n_lang_bins-1);

	if(!DC_SILENT)fprintf(stderr,"DC: Filling t2x tables");fflush(stderr); 

	/* Region 1 */
	for(sec=1;sec<=6;sec++){
		for(j=1;j<=r1layers;j++){

			layer = j>4 ? j+2:j;
			for(l=0;l<n_lang_bins;l++){
				alpha = (locang_binwidth*(float)l) + langle_value_lo;
				for(k=1;k<=n1_dtime_bins;k++){
					tdrift = ((float)k - 0.5)*DC_NS_PER_BIN_R1;
					B=0.0;
					dc_xvst_fct(sec, layer, alpha, tdrift, B, &doca, &tmax);
					DC_XVST_TABLE_R1E[l][k-1][j-1][sec-1] = doca;

					if(DC_XVST_PRINT_TABLE)
						fprintf(stdout,"dc_xvst_table_r1e[alpha=%f][t=%f][layer=%d][sec=%d] = %f\n"
							,alpha,tdrift,layer,sec,doca);
				}
			}
		}
		if(!DC_SILENT)fprintf(stderr,".");fflush(stderr); 
	}

	/* Region 2 */
	for(sec=1;sec<=6;sec++){
		for(j=1;j<=r2layers;j++){

			layer = 12 + j;
			for(l=0;l<n_lang_bins;l++){
				alpha = (locang_binwidth*(float)l) + langle_value_lo;
				for(m=1;m<=n_mag_bins;m++){
					B = DC_B_value_del*((float)m-0.5) + DC_B_value_lo;
					for(k=1;k<=n2_dtime_bins;k++){
						tdrift = ((float)k - 0.5)*DC_NS_PER_BIN_R2;
						dc_xvst_fct(sec, layer, alpha, tdrift, B, &doca, &tmax);
						DC_XVST_TABLE_R2E[m-1][l][k-1][j-1][sec-1] = doca;

						if(DC_XVST_PRINT_TABLE)
							fprintf(stdout,"dc_xvst_table_r21e[B=%f][alpha=%f][t=%f][layer=%d][sec=%d] = %f\n"
								,B,alpha,tdrift,layer,sec,doca);
					}
				}
			}
		}
		if(!DC_SILENT)fprintf(stderr,".");fflush(stderr); 
	}

	/* Region 3 */
	for(sec=1;sec<=6;sec++){
		for(j=1;j<=r3layers;j++){

			layer = 24 + j;
			for(l=0;l<n_lang_bins;l++){
				alpha = (locang_binwidth*(float)l) + langle_value_lo;
				for(k=1;k<=n3_dtime_bins;k++){
					tdrift = ((float)k - 0.5)*DC_NS_PER_BIN_R3;
					B=0.0;
					dc_xvst_fct(sec, layer, alpha, tdrift, B, &doca, &tmax);
					DC_XVST_TABLE_R3E[l][k-1][j-1][sec-1] = doca;

					if(DC_XVST_PRINT_TABLE)
						fprintf(stdout,"dc_xvst_table_r1e[alpha=%f][t=%f][layer=%d][sec=%d] = %f\n"
							,alpha,tdrift,layer,sec,doca);
				}
			}
		}
		if(!DC_SILENT)fprintf(stderr,".");fflush(stderr); 
	}
	
	if(!DC_SILENT)fprintf(stderr,"Done\n");fflush(stderr); 

}


/*=======================================================*
 * Fortran wrapper for below for backward compatibility
 *=======================================================*/
void dc_xvst_fct_(int *sector,int *layer,float *locangle
   ,float *time,float *B,float *doca,float *tmax)
{
	dc_xvst_fct(*sector, *layer, *locangle, *time, *B, doca, tmax);
}


/*=========================================================*
 * This routine is the main entry point for time to DIST
 * conversion. It will normally be called only during
 * initialization to fill the t2x table. It is also used
 * a lot by the calibration programs.
 *=========================================================*/
void dc_xvst_fct(int sector,int layer,float locangle
   ,float time,float B,float *doca_return,float *tmax_return)
{
   float *par=xvst_parms->map_parms;
   float t,doca,beta;
   float theta,theta0,tmax,tzero,ff,dc_size;
   float a,b,c,d;
   float B0;
   float cc,cc1,bb;
   int   xvst_type;
   float t_x10;
   int superlayer,region;


   if(!DC_XVST_INITIALIZED){
      fprintf(stderr,"DC: dc_xvst_init_() was never called! Fix the code!\n");
      exit(-1);
   }

   /* set xvst_parms and par to point to correct values */
   superlayer=((layer-1)/6)+1;
   region =((superlayer-1)/2)+1;
   xvst_parms=&map_parms[sector][superlayer];
   par=xvst_parms->map_parms;
   
   tmax = *tmax_return = xvst_parms->tmax[layer-1];
   
   /* overwrite excluded layer tmax with tmax for this layer */
   xvst_parms->map_parms[2]=tmax;

	/* There are two local angles here: The "event" local angle theta and 
	 * the map local angle theta0 (taken from the average of the data set
	 * used to calibrate). The event local angle should already be B-field
	 * corrected before calling this routine.
	 * 
	 * Recsis only calls this routine to fill two tables at 0 and 30 degrees.
	 * The local angle B-field correction is then applied in dc_xvst_table()
	 * below before interpolating between the two tables.
	 * 
	 * Other programs (such as dc3) should pass local angles which have already
	 * been B-field corrected via locangle_correction().
	 * 
	 * The map local angle represents an average which has NOT been B-field
	 * corrected. It is corrected here before reducing it.
	*/
   theta  = reduced_angle_(&locangle);
   theta0 = par[0]*PI/180.0; /* degrees in map for historical reasons*/
	if((region==2) && (B!=0.0))theta0 -= (B/fabs(B))*locangle_correction(fabs(B),region);
	theta0 = reduced_angle_(&theta0);
   tzero  = par[1];
   B0     = par[3];
   ff     = par[4];
   
   dc_size = DC_HEXSIZE[layer];
      
   a=par[5];
   b=par[6];
   c=par[7];
   d=par[8];
      
      
   /* tmax and local angle corrections for Region 2 */
   tmax  *= tmax_correction(fabs(B),B0,region);
  
   t=time;

   /* The following is never used by recsis which should always */
   /* have program_is_dc_calib_check set to zero.               */
   /* Calibration programs can use it to                        */
   /* correct the time i.e. tzero subtraction and timewalk.     */
   /* The corrections can be turned off using the               */
   /* do_tzero_subtraction and do_timewalk_correction flags     */
   /* The time should only be corrected here for dc_calib_check */
   /* since normally it is corrected before indexing the table. */
   beta=-1.0; /* "time" should already be timewalk corrected    */
   if(program_is_dc_calib_check)t=dc_time_correction_(&t,&superlayer,&sector,&beta);


   /* Note: The first step here is to find the DOCA for a local */
   /* angle given by the map average. The DOCA is corrected for */
   /* the "event" local angle when the doca_locangle_correction */
   /* is applied below.                                         */
   /*                                                           */
   /* The base form of the doca function is constrained to hit  */
   /* cc at tmax. The cc calculated here is done so with a      */
   /* B-field corrected angle.                                  */
   cc = dc_size*cos(PI6-theta0)*ff;

   
	/* Determine normalized time and force in range */   
   t/=tmax;
   if(t>1.0){
     cc1 = cc + doca_locangle_correction(theta,1.0,layer);
   	/* for times larger than tmax, just return a doca which */
   	/* linearly increases to the cell size at t=1.25        */
		doca = cc1 + (((cc1/ff)-cc1)*(t-1.0)/0.25);
		if(doca>(cc1/ff))doca=(cc1/ff);
		*doca_return=doca;
   	return;
   }
   if(t<0.0)t=0.0;
   
   /* choose appropriate functional form */
   xvst_type=(int)par[23];

   /* (Classic "Liming" style used both polys and powers,         */
   /* but without flags. This is done for backward compatibility) */
   if(xvst_type==DC_XVST_TYPE_LIMING)
     xvst_type = (superlayer<=4) ? DC_XVST_TYPE_POLY:DC_XVST_TYPE_POWER;
   
   switch(xvst_type){
	   case DC_XVST_TYPE_LIMING:
	     fprintf(stderr,"DC: Program should never get here %s:%d\n",__FILE__,__LINE__);
	     exit(-1);
	     break;
	   case DC_XVST_TYPE_POLY:
	     bb = cc-a-b-c-d;
	     doca=t*(a+t*(b+t*(c+t*(d+bb*t))));
	     break;
	   case DC_XVST_TYPE_POWER:
	     bb = (cc-(a*tmax))/(1.0-b);
	     doca=a*tmax*t + bb*(pow(t,c) - b*pow(t,d));
	     break;
	   case DC_XVST_TYPE_BESSEL:
	     t_x10=t*10.0; /* scale t up to get appropriate range for bessel */
	     /* don't use j0 since it is non-zero at t=0.0 */
	     bb = cc  - a*jn(1,10.0) 
	       - b*jn(2,10.0)
	       - c*jn(3,10.0)
	       - d*jn(4,10.0);
	     bb/=jn(5,10.0);
	     doca =  a*jn(1,t_x10)
	       + b*jn(2,t_x10)
	       + c*jn(3,t_x10)
	       + d*jn(4,t_x10)
	       +bb*jn(5,t_x10);
	     break;
	   case DC_XVST_TYPE_LEGENDRE:
	     bb = cc  - a*dc_Pn(1,1.0) 
	       - b*dc_Pn(2,1.0)
	       - c*dc_Pn(3,1.0)
	       - d*dc_Pn(4,1.0);
	     bb/=dc_Pn(5,1.0);
	     doca =  a*dc_Pn(1,t)
	       + b*dc_Pn(2,t)
	       + c*dc_Pn(3,t)
	       + d*dc_Pn(4,t)
	       +bb*dc_Pn(5,t)
	       - a*dc_Pn(1,0.0)   /* <---|                                 */
	       - b*dc_Pn(2,0.0)   /*     |                                 */
	       - c*dc_Pn(3,0.0)   /*     |- these terms insure doca(t=0)=0 */
	       - d*dc_Pn(4,0.0)   /*     |                                 */
	       -bb*dc_Pn(5,0.0);  /* <---|                                 */
	     
	     break;
	   default:
	     fprintf(stderr,"DC: unsupported xvst function type (%d) %s:%d\n",xvst_type,__FILE__,__LINE__);
	     exit(-1);
   } /* end switch on xvst_type */
   
   /* force in range */
   if(doca<0.0)doca=0.0;

   /* correct for "event" local angle */
   doca+=doca_locangle_correction(theta,t,layer);

   /* B correction for Region 2 (asymetric gaussian)*/
   doca+=doca_B_correction(t,fabs(B),B0,region);

	*doca_return = doca;

	/* Print Debugging info if requested */
   if(DC_XVST_VERBOSE>=5){
		printf("DC: xvst_func() called\n");
		printf("    Intput Parms:\n");
		printf("\t       sector = %d \n",sector);
		printf("\t        layer = %d \n",layer);
		printf("\t     locangle = %f \n",locangle);
		printf("\t         time = %f \n",time);
		printf("\t            B = %f \n",B);
		printf("    Output Parms:\n");
		printf("\t *doca_return = %f \n",*doca_return);
		printf("\t *tmax_return = %f \n",*tmax_return);
   }
	/* Print Debugging info if requested */
   if(DC_XVST_VERBOSE>=9){
		printf("DC: additional xvst_func() info\n");
		printf("    Intput Parms:\n");
		printf("\t     theta = %f \n",theta);
		printf("\t    theta0 = %f \n",theta0);
		printf("\t     tzero = %f \n",tzero);
		printf("\t      tmax = %f \n",tmax);
		printf("\t     layer = %d \n",layer);
		printf("\t        B0 = %f \n",B0);
		printf("\t        ff = %f \n",ff);
		printf("\t         a = %f \n",a);
		printf("\t         b = %f \n",b);
		printf("\t         c = %f \n",c);
		printf("\t         d = %f \n",d);
		printf("    Return Value:\n");
		printf("\t      doca = %f \n",doca);
   }

}






/*==============================================*
 * Convert time to distance using table.
 * This is what gets called during runtime.
 *==============================================*/
void dc_xvst_table(int sec,int layer,int wire,float beta
	,float locangle,float time,float B, float B_para,float *dist,float *sigma)
{
	int   sup, region, regionlayer;
	float dist1, dist2, distB1, distB2, theta, timec;
	float dc_size, cc, cos_theta_ratio, B_ratio;
	int   t_bin, B_bin, rdoca_bin;
	float dc_ns_per_bin=0.0;

	sup         = 1 + ((layer-1)/6);
	region      = (sup+1)/2;
	regionlayer = layer - ((region-1)*12);

	/* For Monte Carlo */
	if(DC_XVST_CHOICE==0){
		*dist  = time*DC_XVST_DEFAULT_SLOPE;
		*sigma = DC_SIGMA_DOCA[sup-1];
		return;
	}

	/* No "choices" other than 2 supported */
	if(DC_XVST_CHOICE!=2){
		fprintf(stderr,"\n\nYour dc_xvst_choice variable is set to an unsupported\n");
		fprintf(stderr,"value (%d). You should set it as follows:\n",DC_XVST_CHOICE);
		fprintf(stderr,"\n\t dc_xvst_choice = 0   (Monte Carlo)\n");
		fprintf(stderr,"\n\t dc_xvst_choice = 2   (    Data   )\n");
		fprintf(stderr,"\nThis is set in your tcl file if using recsis, or via\n");
		fprintf(stderr,"the -X# flag if using a1c (-X0 for MC ; -X2 for data).\n\n");
		exit(-1);
	}

	/*------------ Find value from table via interpolation ------------*/

	/* local angle (B-field correct and reduce)*/
	theta = locangle;
	if((region==2) && (B_para!=0.0))
		theta -= (B_para/fabs(B_para))*locangle_correction(fabs(B),region);
	theta = reduced_angle_(&theta);

	/* cell size at localangle theta */
	dc_size = DC_HEXSIZE[layer];
	cc = dc_size*cos(PI6-theta);

	/* correct time (tzero and timewalk) */
	timec = dc_time_correction_(&time,&layer,&sec,&beta);

	/* Find time bin */
	if(region==1)dc_ns_per_bin=DC_NS_PER_BIN_R1;
	if(region==2)dc_ns_per_bin=DC_NS_PER_BIN_R2;
	if(region==3)dc_ns_per_bin=DC_NS_PER_BIN_R3;
	t_bin = (int)floor(timec/dc_ns_per_bin);
	if (t_bin<0) t_bin = 0;

	/* local angle interpolation slope */
	cos_theta_ratio = (cos(PI6-theta)-cos(PI6-langle_value_lo))/
							(cos(PI6-langle_value_hi)-cos(PI6-langle_value_lo));

	/* Region 1 */
	if(region==1){
		if(t_bin>=n1_dtime_bins)t_bin=n1_dtime_bins-1;
		if(regionlayer>6)regionlayer-=2;

		dist1 = DC_XVST_TABLE_R1E[0][t_bin][regionlayer-1][sec-1];
		dist2 = DC_XVST_TABLE_R1E[1][t_bin][regionlayer-1][sec-1];
		*dist = dist1 + ((dist2-dist1)*cos_theta_ratio);
	}

	/* Region 2 */
	if(region==2){
		if(t_bin>=n2_dtime_bins)t_bin=n2_dtime_bins-1;

		B_bin   = (int)floor((fabs(B)-DC_B_value_lo)/DC_B_value_del);
		B_bin   = B_bin<0 ? 0:B_bin>=n_mag_bins-1 ? n_mag_bins-2:B_bin;
		
		dist1   = DC_XVST_TABLE_R2E[B_bin][0][t_bin][regionlayer-1][sec-1];
		dist2   = DC_XVST_TABLE_R2E[B_bin][1][t_bin][regionlayer-1][sec-1];
		distB1  = dist1 + ((dist2-dist1)*cos_theta_ratio);

		dist1   = DC_XVST_TABLE_R2E[B_bin+1][0][t_bin][regionlayer-1][sec-1];
		dist2   = DC_XVST_TABLE_R2E[B_bin+1][1][t_bin][regionlayer-1][sec-1];
		distB2  = dist1 + ((dist2-dist1)*cos_theta_ratio);

		B_ratio = ( fabs(B) - ((DC_B_value_del*(float)B_bin)+DC_B_value_lo) ) 
			/DC_B_value_del;
		
		*dist   = distB1 + (distB2 - distB1)*B_ratio;
	}

	/* Region 3 */
	if(region==3){
		if(t_bin>=n3_dtime_bins)t_bin=n3_dtime_bins-1;

		dist1 = DC_XVST_TABLE_R3E[0][t_bin][regionlayer-1][sec-1];
		dist2 = DC_XVST_TABLE_R3E[1][t_bin][regionlayer-1][sec-1];
		*dist = dist1 + ((dist2-dist1)*cos_theta_ratio);
	}

	/* This should be unneccessary since it is enforced at table       */
	/* filling time. It shouldn't hurt anything having it here though. */
	if((*dist)>dc_size)*dist=dc_size;

	/* Get sigma as function of DOCA from table */
	rdoca_bin = (int)floor((float)n_rdoca_bins*(*dist)/cc);
	rdoca_bin = rdoca_bin<0 ? 0:rdoca_bin>=n_rdoca_bins ? n_rdoca_bins-1:rdoca_bin;
	*sigma = DC_SIGMA_TABLE[rdoca_bin][sup-1];

	/* Print Debugging info if requested */
   if(DC_XVST_VERBOSE>=4){
		clasHEAD_t *HEAD = getBank(&bcs_, "HEAD");
		static int last_event=-1;

   	if(HEAD)if(HEAD->head[0].nevent!=last_event){
			last_event=HEAD->head[0].nevent;
			printf("DC: NEW EVENT\n");
   	}
		printf("DC: dc_xvst_table() called for event %d\n",last_event);
		printf("    Intput Parms:\n");
		printf("\t          sector = %d \n",sec);
		printf("\t           layer = %d \n",layer);
		printf("\t            wire = %d \n",wire);
		printf("\t            beta = %f \n",beta);
		printf("\t        locangle = %f \n",locangle);
		printf("\t            time = %f \n",time);
		printf("\t               B = %f \n",B);
		printf("\t          B_para = %f \n",B_para);
		printf("    Calculated values:\n");
		printf("\t           timec = %f \n",timec);
		printf("\t           theta = %f \n",theta);
		printf("\t cos_theta_ratio = %f \n",cos_theta_ratio);
		printf("\t   dc_ns_per_bin = %f \n",dc_ns_per_bin);
		printf("    Output Values:\n");
		printf("\t            *dist = %f \n",*dist);
		printf("\t           *sigma = %f \n",*sigma);
   }
}

/*========================================================*
 * Fortran wrapper for above for backward compatibility 
 *========================================================*/
void dc_time_to_dist_(int *sec,int *layer,int *wire,float *beta
	,float *locangle,float *time,float *B, float *B_para,float *dist,float *sigma)
{
	dc_xvst_table(*sec,*layer,*wire,*beta,*locangle,*time,*B,*B_para,dist,sigma);
}


/***********************************************/
/*                                             */
/*                 CORRECTIONS                 */
/*                                             */
/* These are correction routines for tmax and  */
/* DOCA. Originally, they were were imbedded   */
/* in xvst_func() but have been broken up into */
/* separate routines to aid in calibration and */
/* diagnostics.                                */ 
/***********************************************/


float tmax_correction(float B,float B0,int region)
{
   float tmax1,tmax2;
   float *par=xvst_parms->map_parms;

   if(region==2){
      tmax1 = par[20] + (par[21]*B) + (par[22]*B*B);
      tmax2 = par[20] + (par[21]*B0) + (par[22]*B0*B0);
      return tmax1/tmax2;
   }
   
   return 1.0;
}

float locangle_correction(float B,int region)
{
   float B_angle_slope=0.002;

   if(region==2)
      return acos(1.0 - B_angle_slope*B);
   
   return 0.0;
}


float doca_locangle_correction(float theta,float t,int layer)
{
   float *par=xvst_parms->map_parms;
   float tau,pma,pna;
   float dc_size,theta0,ff,cc,cc1;
   
   theta0  = par[0]*PI/180.0;
   theta0 = reduced_angle_(&theta0);
   ff      = par[4];
   dc_size = DC_HEXSIZE[layer];
   
   tau=par[10];
   pma=par[11];
   pna=par[12];

   cc = dc_size*cos(PI6-theta0)*ff;
   cc1 = dc_size*cos(PI6-theta)*ff;
   return (cc1-cc)*exp(-pow((1.0-t)/tau,pma))*pow(t,pna);
}

float doca_B_correction(float t,float B,float B0,int region)
{
   float *par=xvst_parms->map_parms;
   float amp,mean,sig1,sig2,sig,z,offset;
   
   if((region==2) && B0!=0){
      amp=par[15];
      mean=par[16];
      sig1=par[17];
      sig2=par[18];
      z = t-mean;
      
      offset = exp(-pow(mean/sig1,2.0)/2.0);
      sig= z<=0.0 ? sig1:sig2;
      
      return (B-B0)*amp*(1.0 + offset)*(exp(-pow(z/sig,2.0)/2.0) -offset);
   }

   return 0.0;
}


/* returns reduced angle. Input and output in radians */
float reduced_angle_(float *alpha)
{
   float ralpha;
   
   ralpha=fabs(*alpha);
   
   while(ralpha>PI3)ralpha-=PI3;
   if(ralpha>PI6)ralpha=PI3-ralpha;
   
   return ralpha;
} 






/*==================================================
 * Misc routines that are needed by the above.
 *==================================================*/



/* factorial. This is for the Legendre function below */
double dc_factorial(int N)
{
	double f=1.0;

	for(;N>0;N--)f*=(double)N;

	return f;
}

/* Legendre Polynomial (surely this is defined somewhere else!?)*/
double dc_Pn(int n,double x)
{
	int k;
	double Pn=0.0, a;
	
	for(k=0;k<(n/2);k++){
		a=pow(-1.0,(double)k)*dc_factorial(2*(n-k))*pow(x,(double)(n-(2*k)));
		a/=pow(2.0,(double)k)*dc_factorial(k)*dc_factorial(n-k)*dc_factorial(n-(2*k));
		Pn+=a;
	}

	return Pn;
}

/* Geometric cell sizes */
void xvst_set_cell_sizes(void)
{
/*   DC_HEXSIZE[0] =  0.0; */
   DC_HEXSIZE[1] =  0.7869201;
   DC_HEXSIZE[2] =  0.7956614;
   DC_HEXSIZE[3] =  0.8044137;               
   DC_HEXSIZE[4] =  0.8131712;               
   DC_HEXSIZE[5] =  0.0;               
   DC_HEXSIZE[6] =  0.0;
   DC_HEXSIZE[7] =  0.7837682;              
   DC_HEXSIZE[8] =  0.7918822;               
   DC_HEXSIZE[9] =  0.7999969;           
   DC_HEXSIZE[10] = 0.8081058;               
   DC_HEXSIZE[11] = 0.8162189;               
   DC_HEXSIZE[12] = 0.8243337;
   DC_HEXSIZE[13] =  1.312441;               
   DC_HEXSIZE[14] =  1.323807;              
   DC_HEXSIZE[15] =  1.335167;               
   DC_HEXSIZE[16] =  1.346542;
   DC_HEXSIZE[17] =  1.357887;              
   DC_HEXSIZE[18] =  1.369269;
   DC_HEXSIZE[19] =  1.422092;               
   DC_HEXSIZE[20] =  1.434615;               
   DC_HEXSIZE[21] =  1.447109;               
   DC_HEXSIZE[22] =  1.459625; 
   DC_HEXSIZE[23] =  1.472133;               
   DC_HEXSIZE[24] =  1.484651;
   DC_HEXSIZE[25] =  2.028670;               
   DC_HEXSIZE[26] =  2.047623;               
   DC_HEXSIZE[27] =  2.066554;
   DC_HEXSIZE[28] =  2.085484;               
   DC_HEXSIZE[29] =  2.104432;               
   DC_HEXSIZE[30] =  2.123351;
   DC_HEXSIZE[31] =  2.186400;               
   DC_HEXSIZE[32] =  2.206796;               
   DC_HEXSIZE[33] =  2.227222;
   DC_HEXSIZE[34] =  2.247618;               
   DC_HEXSIZE[35] =  2.268037;               
   DC_HEXSIZE[36] =  2.288429;
}




