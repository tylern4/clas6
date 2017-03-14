

#include <dc.h>
#include <ntypes.h>
#include <bostypes.h>
#include <math.h>

#define DC_TIMEWALK_N_BETA_BINS  10
#define DC_TIMEWALK_N_T_BINS    450
#define DC_TIMEWALK_TIMELO      (-50.0)



/* Globals used by timewalk function(s) */
float DC_TIMEWALK_TABLE[7][DC_TIMEWALK_N_BETA_BINS][DC_TIMEWALK_N_T_BINS];
int   DC_TIMEWALK_RUNNUMBER       = 0;
int   DC_TIMEWALK_PRINT_TABLE     = 0;
int   DC_TIMEWALK_PRINT_PAR       = 0;
int   DC_TIMEWALK_VERBOSE         = 0;
int   DC_TIMEWALK_NORMALIZED_TIME = 0;


float DC_TIMEWALK_TMAX[7];
int   DC_TIMEWALK_TABLE_TYPE=-1;


/*===============================================================
 * This is the master function for the dc timewalk routines.
 * This function should only be called by dc_timewalk_init()
 * while filling the timewalk tables. In the case of special
 * circumstances requiring this to be called from somewhere
 * else, the routine dc_timewalk_init() should at least be
 * called before calling dc_timewalk() to initialize the global
 * arrays using values from the Map.
 * 
 * The routine dc_timewalk_table() should normally be called
 * during runtime.
/*===============================================================*/
float dc_timewalk(float superlayer,float beta,float time,float tmax)
{
	float *par=NULL;
	int region=(superlayer+1)/2;
	int sup=(int)superlayer;

	/* determine calibration parameters for the specified Region */ 
	if(region==0)return 0.0;
	if(region==1)par=map_parms[1][sup].tw_parm_r1;
	if(region==2)par=map_parms[1][sup].tw_parm_r2;
	if(region==3)par=map_parms[1][sup].tw_parm_r3;
	if(!par){
		fprintf(stderr,"DC: Bad region number (%d) for timewalk  %s:%d\n"
			,region,__FILE__,__LINE__);
		exit(-1); 
	}

	/* Call the timewalk function specified in calibration parameter */
	switch(map_parms[1][sup].tw_functiontype)
	{
		case DC_TIMEWALK_TYPE_LIMING:
			return dc_timewalk_liming(superlayer,beta,time,tmax);
			break;
		case DC_TIMEWALK_TYPE_VIPULI:
			return dc_timewalk_vipuli(superlayer,beta,time,tmax);
			break;
		case DC_TIMEWALK_TYPE_MAC:
			return dc_timewalk_mac(superlayer,beta,time,tmax);
			break;
		case DC_TIMEWALK_TYPE_NONE:
			return dc_timewalk_none(superlayer,beta,time,tmax);
			break;

		default:
			fprintf(stderr,"DC: Unsupported timewalk function type(%f) %s:%d\n"
				,par[9],__FILE__,__LINE__);
	}
}


/*===============================================================
 * Initialize the tables for timewalk corrections using paramters
 * from the Map.
 * dc_xvst_init_() MUST be called before calling this routine.
 * (Actually, this routine is called from dc_xvst_init_().)
 * This should be called whenever the runnumber changes. 
/*===============================================================*/
void dc_timewalk_init(int runnumber)
{
	int i,j,sup,sec;
	char mapfile[256]="DC_DOCA.map";
	int firsttime;
	char *ptr;
	float tw_parm_r1[10];
	float tw_parm_r2[10];
	float tw_parm_r3[10];
	float tw_fact[3];
	float tw_tau[3];
	float tw_betaslope[6];

	/* get full path to DC_DOCA.map file from environment variable */
	if(ptr=getenv("CLAS_PARMS"))sprintf(mapfile,"%s/Maps/DC_DOCA.map",ptr);

	/* remember run number used it case we want to check it later */
	DC_TIMEWALK_RUNNUMBER=runnumber;

	map_get_int(  mapfile,"Timewalk","functiontype",1,&DC_TIMEWALK_TABLE_TYPE,DC_TIMEWALK_RUNNUMBER,&firsttime);
	map_get_float(mapfile,"Timewalk","Region1",10,tw_parm_r1,DC_TIMEWALK_RUNNUMBER,&firsttime);
	map_get_float(mapfile,"Timewalk","Region2",10,tw_parm_r2,DC_TIMEWALK_RUNNUMBER,&firsttime);
	map_get_float(mapfile,"Timewalk","Region3",10,tw_parm_r3,DC_TIMEWALK_RUNNUMBER,&firsttime);

	if(DC_TIMEWALK_PRINT_PAR){
		printf("\n\nDC Timewalk parms\n\n");
		printf("       Region1  \t       Region2  \t       Region3\n");
		for(i=0;i<10;i++){
			printf("tw_par[%2d] = %+4.4f\t",i,tw_parm_r1[i]);
			printf("tw_par[%2d] = %+4.4f\t",i,tw_parm_r2[i]);
			printf("tw_par[%2d] = %+4.4f"  ,i,tw_parm_r3[i]);
			printf("\n");
		}
		printf("\nFunction type : ");
	}

	switch(DC_TIMEWALK_TABLE_TYPE){
		case DC_TIMEWALK_TYPE_LIMING:
			map_get_float(mapfile,"Timewalk","factor" , 3,tw_fact   ,DC_TIMEWALK_RUNNUMBER,&firsttime);
			map_get_float(mapfile,"Timewalk","tau"    , 3,tw_tau    ,DC_TIMEWALK_RUNNUMBER,&firsttime);
			if(DC_TIMEWALK_PRINT_PAR){
				printf("Liming\n");
				printf("factor[1]=%+4.4f\tfactor[2]=%4.4f\tfactor[3]=%+4.4f\n",tw_fact[0],tw_fact[1],tw_fact[2]);
				printf("tau[1]   =%+4.4f\ttau[2]   =%4.4f\ttau[3]   =%+4.4f\n",tw_tau[0],tw_tau[1],tw_tau[2]);
			}
			break;
			
		case DC_TIMEWALK_TYPE_VIPULI:
		case DC_TIMEWALK_TYPE_MAC:
			map_get_float(mapfile,"Timewalk","betaslope", 6,tw_betaslope,DC_TIMEWALK_RUNNUMBER,&firsttime);
			if(DC_TIMEWALK_PRINT_PAR){
				printf("%s\n",DC_TIMEWALK_TABLE_TYPE==DC_TIMEWALK_TYPE_MAC ? "Mac":"Vipuli");
				printf("betaslope[1]=%+4.4f betaslope[2]=%4.4f betaslope[3]=%+4.4f\n"
					,tw_betaslope[0],tw_betaslope[1],tw_betaslope[2]);
				printf("betaslope[4]=%+4.4f betaslope[5]=%4.4f betaslope[6]=%+4.4f\n"
					,tw_betaslope[3],tw_betaslope[4],tw_betaslope[5]);
			}
			break;
		case DC_TIMEWALK_TYPE_NONE:
			if(DC_TIMEWALK_PRINT_PAR)printf("none\n");
			break;
		default:
		
			fprintf(stderr,"Bad DC timewalk function type (%d)   %s:%d\n"
				,DC_TIMEWALK_TABLE_TYPE,__FILE__,__LINE__);
			exit(-1);
	}
	if(DC_TIMEWALK_PRINT_PAR)printf("\n\n");


	/* Copy map parms into global map_parms data structure.
	 * This means there is some redundancy in storing timewalk
	 * paramters since timewalk is not sector dependant. However,
	 * it allows the full calibration for a specific superlayer/
	/* sector to be stored in a single xvst_parms_t data structure */
	for(sup=1;sup<=6;sup++){
		for(sec=1;sec<=6;sec++){

			map_parms[sec][sup].tw_functiontype=DC_TIMEWALK_TABLE_TYPE;
			
			for(i=0;i<10;i++){
				map_parms[sec][sup].tw_parm_r1[i+1]=tw_parm_r1[i];
				map_parms[sec][sup].tw_parm_r2[i+1]=tw_parm_r2[i];
				map_parms[sec][sup].tw_parm_r3[i+1]=tw_parm_r3[i];
			}
			for(i=0;i<3;i++){
				map_parms[sec][sup].tw_fact[i+1]=tw_fact[i];
				map_parms[sec][sup].tw_tau[i+1]=tw_tau[i];
			}
			for(i=0;i<6;i++){
				map_parms[sec][sup].tw_betaslope[i+1]=tw_betaslope[i];
			}
		}
	}


   /* announce runnumber for timewalk map parms */
   fprintf(stderr,"DC:Filling timewalk table for run %d (%d):\n"
   	,DC_TIMEWALK_RUNNUMBER,firsttime);


	/* Fill the timewalk table */
	dc_fill_timewalk_table();
	
}

/*======================================================================
 * Fill the timewalk table.
 * Recsis and a1 will only call this once from the dc_timewalk_init()
 * routine above. Calibration programs, however, may need to call this
 * several times while tweaking the timewalk paramters.
/*======================================================================*/
void dc_fill_timewalk_table(void)
{
	int i,sup,sec;
	float xval,beta,tdrift,tbin_width;
	int beta_bin,t_bin;
	int tw_type;

	for(sup=1;sup<=6;sup++){


		/* The timewalk table can be filled in two different ways
		 * 
		 * a.) the "classic", Liming way which indexes it by absoulte
		 * time from -50ns to 400ns
		 * 
		 * or
		 * 
		 * b.) the newer, Vipuli way which effectively indexes it by
		 * normalized time. The table is filled from -50ns to 1.25*tmax.
		 * The values of tmax used are the averages over sectors of the 
		/* excluded layer tmaxes. */

		switch(map_parms[1][sup].tw_functiontype){ /* Use sector 1 function type to fill table */
			case DC_XVST_TYPE_LIMING:
				DC_TIMEWALK_TMAX[sup]=400.0/1.25; /* dividing by 1.25 is a kludge. See tbin_width line below */
				break;
				
			case DC_TIMEWALK_TYPE_VIPULI:
			case DC_TIMEWALK_TYPE_MAC:
			case DC_TIMEWALK_TYPE_NONE:
				DC_TIMEWALK_TMAX[sup]=0.0;
				for(sec=1;sec<=6;sec++)
					DC_TIMEWALK_TMAX[sup]+=map_parms[sec][sup].tmax[excluded[sup]]/6.0;
				break;
			default:
			
				fprintf(stderr,"Bad DC timewalk function type (%d)   %s:%d\n"
					,tw_type,__FILE__,__LINE__);
				exit(-1);
		}

		/* Determine width of time bin.            */ 
		/* For Liming timewalk, this will be 1.0ns */
		/* Fill table up to 125% of average tmax   */
		tbin_width=((1.25*DC_TIMEWALK_TMAX[sup])-DC_TIMEWALK_TIMELO)/(float)DC_TIMEWALK_N_T_BINS;

		/* loop over beta and t bins, filling the timewalk table */
		for(beta_bin=0;beta_bin<DC_TIMEWALK_N_BETA_BINS;beta_bin++){
			beta=(((float)beta_bin)+0.5)/(float)DC_TIMEWALK_N_BETA_BINS;

			for(t_bin=0;t_bin<DC_TIMEWALK_N_T_BINS;t_bin++){

				tdrift = ((((float)t_bin)+0.5)*tbin_width)+(float)DC_TIMEWALK_TIMELO;

				DC_TIMEWALK_TABLE[sup][beta_bin][t_bin]
					=dc_timewalk(sup,beta,tdrift,DC_TIMEWALK_TMAX[sup]);

				/* Optionally print the timewalk table to the screen as it is filled */
				if(DC_TIMEWALK_PRINT_TABLE){
					fprintf(stdout,"DC_TIMEWALK_TABLE[SL=%d][beta=%1.3f][t=%3.1f] = %f\n"
						,sup,beta,tdrift,DC_TIMEWALK_TABLE[sup][beta_bin][t_bin]);
				}
			}
		}
	}
}


/*=================================================================
 * Get the value of the timewalk function from a table.
 * This is what should be called during runtime. 
/*=================================================================*/
float dc_timewalk_table(float superlayer,float beta,float time,float tmax)
{
   int beta_bin,t_bin;
   float tbin_width;
   int sup=(int)superlayer;

	/* if using Liming timewalk, don't scale the time. Otherwise
	 * scale the time to account for the fact that the tmax used
	 * to fill the table isn't neccessarily the same as the one
	/* for the sector the current hit is in. */
	if(map_parms[1][(int)superlayer].tw_functiontype!=DC_TIMEWALK_TYPE_LIMING)
		time*=DC_TIMEWALK_TMAX[sup]/tmax;

	/* Find time bin */
	tbin_width=((1.25*DC_TIMEWALK_TMAX[sup])-DC_TIMEWALK_TIMELO)/(float)DC_TIMEWALK_N_T_BINS;
	t_bin = (int)((time - DC_TIMEWALK_TIMELO)/tbin_width);
	
	if(t_bin>=DC_TIMEWALK_N_T_BINS)t_bin=DC_TIMEWALK_N_T_BINS-1;
	if(t_bin<0)t_bin=0;
	
	/* Find beta bin */
	beta_bin = (int)(beta*(float)DC_TIMEWALK_N_BETA_BINS);
	if(beta_bin<0)beta_bin=0;
	if(beta_bin>=DC_TIMEWALK_N_BETA_BINS)beta_bin=DC_TIMEWALK_N_BETA_BINS-1;

	/* Print Debugging info if requested */
   if(DC_TIMEWALK_VERBOSE>=4){
		clasHEAD_t *HEAD = getBank(&bcs_, "HEAD");
		static int last_event=-1;

   	if(HEAD)if(HEAD->head[0].nevent!=last_event){
			last_event=HEAD->head[0].nevent;
			printf("DC: NEW EVENT\n");
   	}
		printf("DC: dc_timewalk_table() called for event %d\n",last_event);
		printf("    Intput Parms:\n");
		printf("\t   superlayer = %f \n",superlayer);
		printf("\t         beta = %f \n",beta);
		printf("\t         time = %f \n",time);
		printf("\t         tmax = %f \n",tmax);
		printf("    Return Value:\n");
		printf("\t     timewalk = %f \n",DC_TIMEWALK_TABLE[sup][beta_bin][t_bin]);
   }

	return DC_TIMEWALK_TABLE[sup][beta_bin][t_bin];
}








/*========================================================
 * Timewalk functions
 * 
 * The following are the different timewalk functions which
 * can be used to correct for tmewalk in the DC.
/*========================================================*/









float dc_timewalk_liming(float superlayer,float beta,float time,float tmax)
{
   int i;
   int sup=(int)superlayer;
   float *parm_r1=map_parms[1][sup].tw_parm_r1;
   float *parm_r2=map_parms[1][sup].tw_parm_r2;
   float *parm_r3=map_parms[1][sup].tw_parm_r3;
   float *fact   =map_parms[1][sup].tw_fact;
   float *tau    =map_parms[1][sup].tw_tau;
   float dc_timewalk0;
   float *parm,fac;
   float dc_timewalk,tau0,time_ee0,time_ee;
   
   switch(sup){
      case 1:
      case 2:
         parm=parm_r1;
         fac=fact[1];
         tau0=tau[1];
         break;
      case 3:
      case 4:
         parm=parm_r2;
         fac=fact[2];
         tau0=tau[2];         
         break;
      case 5:
      case 6:
         parm=parm_r3;
         fac=fact[3];
         tau0=tau[3];
         break;
      default:
         return 0.0;
   }
   dc_timewalk0 = 
      fac*( parm[1] + (parm[2]*beta) + (parm[3]*beta*beta) - parm[4] );

   if(tau0==0.0)tau0=100.0;


   /* The time walk at doca>0 is parameterized in term of the */
   /* electron equivalent drift time. So we iterate to get    */
   /* the the time walk for the drift time of beta.           */

   if(time<dc_timewalk0){
        dc_timewalk = time;
   }else{
      dc_timewalk=0.0;
      for(i=0;i<50;i++){
         time_ee0 = time - dc_timewalk;
         dc_timewalk = dc_timewalk0*exp(-time_ee0/tau0);
         time_ee = time - dc_timewalk;
         if(fabs(time_ee0-time_ee)<=0.5)break;
      }
	}

	return dc_timewalk;
}


float dc_timewalk_vipuli(float superlayer,float beta,float time,float tmax)
{
   int sup=(int)superlayer;
	float dc_timewalk=0.0;
	float betaslope,p1,p2,p3,p4,p5;
	int region=(sup+1)/2;
	float *p;
	float t,tw;

	if(region==1)p=map_parms[1][sup].tw_parm_r1;
	if(region==2)p=map_parms[1][sup].tw_parm_r2;
	if(region==3)p=map_parms[1][sup].tw_parm_r3;
	betaslope=map_parms[1][sup].tw_betaslope[sup];

	if(sup%2){ /* superlayers 1,3, and 5 */
		p1=p[1]; p2=p[2]; p3=p[3]; p4=p[4]; p5=p[5];
	}else{     /* superlayers 2,4, and 6 */
		p1=p[6]; p2=p[7]; p3=p[8]; p4=p[9]; p5=p[10];
	}

   t=time/tmax;
	tw = p5 + t*(p4 + t*(p3 + t*(p2 + t*p1)));
	dc_timewalk=betaslope*beta*tw;

	return dc_timewalk;
}


float dc_timewalk_mac(float superlayer,float beta,float time,float tmax)
{
	int sup=(int)superlayer;
	float dc_timewalk=0.0;
	float betaslope,a,b,c,d;
	int region=(sup+1)/2;
	float *p;
	float t,tw;
	float t3=0.0 ,bs3=0.0;

	if(region==1)p=map_parms[1][sup].tw_parm_r1;
	if(region==2)p=map_parms[1][sup].tw_parm_r2;
	if(region==3)p=map_parms[1][sup].tw_parm_r3;
	betaslope=map_parms[1][sup].tw_betaslope[sup];

	if(sup%2){ /* superlayers 1,3, and 5 */
		a=p[1]; b=p[2]; c=p[3]; d=p[4]; 
	}else{     /* superlayers 2,4, and 6 */
		a=p[6]; b=p[7]; c=p[8]; d=p[9];
	}

   t=time/tmax;

	/* limit the timewalk correction to its limits at t=0 */
	/* and t=1 for times outside that range */
	if(t<0.0) t=0.0;
	if(t>1.0) t=1.0;
	tw =(a/(t+b)) + (c/(d-t));
	
	/* now multiply the time-dependant part times the beta-dep. part */
	dc_timewalk=betaslope*beta*tw;

	/* Disabled 2/14/2002 MM and DL */
	/* "Turn-on" function */
	/*t3  = time*time*time;                   /* for some reason, pow() was seg. faulting */
	/*bs3 = betaslope*betaslope*betaslope;
	/*dc_timewalk *= t3/(bs3 + t3);
	*/
	
	/* Disabled lower limit, 2/21/02 MM */
	/* Enforce a lower limit */
	/* if(time<=0.0)dc_timewalk=0.0; */

	return dc_timewalk;
}


float dc_timewalk_none(float superlayer,float beta,float time,float tmax)
{
	return 0.0;
}





