
#include "trk_mon_lite.h"

//------------ PAW DEFINES -----------------
#define MEMH 5000000
#define LREC 1024 // record length of hbook direct access file in WORDS
#define LUN 3 // logical unit number of hbook file

// declare the hbook common
float pawc_[MEMH];
float hcfitd_[25];
int quest_[100];

char OUTPUTFILE[256]="";
char CHTAGS[512],MAPTAGS[512];
char SAVE_CHPATH[256];
int LOCAL_ANGLE_HISTOS_ONLY=0, AUTOFIND_LOCAL_ANGLE_CUTS=0;
tm_ntuple_cut_t CUTS[100];
int tm_num_ntuple_cuts=0;
tm_mass_cut_t mass_cuts;



float langle_cuts_lo[7]={0.0, -29.0, -27.0, -12.0, -12.0, 2.0 , 2.0 };
float langle_cuts_hi[7]={0.0, -22.0, -20.0, -2.0 , -2.0 , 20.0, 20.0};

int tm_book_histos(int runnumber);
void SAVE_HBOOK_DIR(void);
void RESTORE_HBOOK_DIR(void);
int  tm_define_ntuple_fields(void);
void tm_book_ntuples(int runno);
int tm_fill_ntuples(void);
void tm_end_ntuples(void);
int  tm_find_ntuple_index(char *name);
int  tm_pass_ntuple_cut(float *vals);
void tm_set_ntuple_cut(char *arg);
void tm_set_mas_upper(float upper);
void tm_set_mass_lower(float lower);
void tm_use_mass_cuts(void);
int  mass_cut_var=0;
int  set_mass_cuts=0;
float mass;

//--------------------
// hist_book
//--------------------
int hist_book(int runnumber)
{
	// Initialize CERNLIB
	quest_[9] = 65000;
	hlimit(MEMH);

	// Automatic output file name
	if(!strlen(OUTPUTFILE)) {
		sprintf(OUTPUTFILE,"dc_calib%06d.hbook", runnumber);
	}

	// If we are "feeling" out the local angle cuts, then we don't
	// want to open a file the first time through. We'll work with
	// the histograms in memory only. The file will be created only
	// on the second pass.
	int dont_book_histos = AUTOFIND_LOCAL_ANGLE_CUTS && LOCAL_ANGLE_HISTOS_ONLY;
	if(!dont_book_histos){
		cout<<"Opening output file \""<<OUTPUTFILE<<"\""<<endl;
		hropen(LUN, "esr", OUTPUTFILE , "N", LREC, 0);
	}

	// Book the histograms
	tm_book_histos(runnumber);

	// Book the ntuples
	if(!LOCAL_ANGLE_HISTOS_ONLY)tm_book_ntuples(runnumber);

	return 0;
}

//--------------------
// tm_book_histos
//--------------------
int tm_book_histos(int runnumber)
{
	// Local angle histograms
	for(int reg=1;reg<=3;reg++){
		char title[256];
		sprintf(title, "Local angle, R%d (no cut)",reg);
		hbook1(reg + 4000, title, 100, -70.0, 30.0, 0);
		sprintf(title, "Local angle, R%d (cut)",reg);
		hbook1(reg + 5000, title, 100, -70.0, 30.0, 0);
	}

	// return here is we're only producing local angle histograms
	if(LOCAL_ANGLE_HISTOS_ONLY)return 0;

	// A few histograms produced by trk_mon are used by dc3.
	// reproduce them here.
	float dtime_max[7]={0.,300.,350.,650.,750.,1450.,1650.};
	for(int sec=1; sec<=6; sec++){
		char title[256];
		for(int sup=1; sup<=6; sup++){
			int offset = 10000 * sec + 1000*sup;
			sprintf(title, "Fit DOCA vs DTime, S%d, SL%d",sec,sup);
			hbook2(2 + offset, title,200,-50.,dtime_max[sup],50,0.,((sup+1)/2)*1.0,0);
			sprintf(title, "Hits per TBT, S%d, SL%d",sec,sup);
			hbook1(7 + offset, title,7, 0.0, 7.0, 0);
			
			sprintf(title,"Residual S%dSL%d",sec,sup);
			int RPID=500+100+(sec*10)+sup;
			hbook1(RPID,title,100,-0.2,0.2,0);
		}
		sprintf(title, "Chisq, S%d",sec);
		hbook1(sec + 2000, title, 1000, 0.0, 100.0, 0);
	}
	
	return 0;
}


//--------------------
// SAVE_HBOOK_DIR
//--------------------
void SAVE_HBOOK_DIR(void)
{
   bzero(SAVE_CHPATH,256);
   hcdir(SAVE_CHPATH,"R");
}

//--------------------
// RESTORE_HBOOK_DIR
//--------------------
void RESTORE_HBOOK_DIR(void)
{
   hcdir(SAVE_CHPATH," ");
}

//--------------------
// tm_define_ntuple_fields
//--------------------
int tm_define_ntuple_fields(void)
{
   int ntag=0;
   
   /* define ntuple fields */
   *CHTAGS=0;
   strcat(CHTAGS,"sector  "); ntag++;
   strcat(CHTAGS,"layer   "); ntag++;
   strcat(CHTAGS,"wire    "); ntag++;
   strcat(CHTAGS,"time    "); ntag++;
   strcat(CHTAGS,"fitdoca "); ntag++;
   strcat(CHTAGS,"resi    "); ntag++;
   strcat(CHTAGS,"B       "); ntag++;
   strcat(CHTAGS,"B1      "); ntag++;
   strcat(CHTAGS,"B2      "); ntag++;
   strcat(CHTAGS,"beta    "); ntag++;
   strcat(CHTAGS,"phi     "); ntag++;
   strcat(CHTAGS,"locangle"); ntag++;
   strcat(CHTAGS,"P       "); ntag++;
   strcat(CHTAGS,"q       "); ntag++;
   strcat(CHTAGS,"chisq   "); ntag++;
   strcat(CHTAGS,"hit     "); ntag++;
   strcat(CHTAGS,"DC1     "); ntag++;
   strcat(CHTAGS,"calcdoca"); ntag++;
   strcat(CHTAGS,"ctime   "); ntag++;

   return ntag;
}

//--------------------
// tm_define_map_ntuple_fields
//--------------------
int tm_define_map_ntuple_fields(void)
{
   int ntag=0;
   
   /* define ntuple fields */
   *MAPTAGS=0;
   strcat(MAPTAGS,"sector  "); ntag++;
   strcat(MAPTAGS,"super   "); ntag++;
   strcat(MAPTAGS,"avg_lang"); ntag++;
   strcat(MAPTAGS,"tzero   "); ntag++;
   strcat(MAPTAGS,"tmax    "); ntag++;
   strcat(MAPTAGS,"avg_B   "); ntag++;
   strcat(MAPTAGS,"ff      "); ntag++;
   strcat(MAPTAGS,"par1    "); ntag++;
   strcat(MAPTAGS,"par2    "); ntag++;
   strcat(MAPTAGS,"par3    "); ntag++;
   strcat(MAPTAGS,"par4    "); ntag++;
   strcat(MAPTAGS,"layer   "); ntag++;
   strcat(MAPTAGS,"par1_ang"); ntag++;
   strcat(MAPTAGS,"par2_ang"); ntag++;
   strcat(MAPTAGS,"par3_ang"); ntag++;
   strcat(MAPTAGS,"spare1  "); ntag++;
   strcat(MAPTAGS,"tim_corr"); ntag++;
   strcat(MAPTAGS,"par1_B  "); ntag++;
   strcat(MAPTAGS,"par2_B  "); ntag++;
   strcat(MAPTAGS,"par3_B  "); ntag++;
   strcat(MAPTAGS,"par4_B  "); ntag++;
   strcat(MAPTAGS,"spare2  "); ntag++;
   strcat(MAPTAGS,"par1tmax"); ntag++;
   strcat(MAPTAGS,"par2tmax"); ntag++;
   strcat(MAPTAGS,"par3tmax"); ntag++;
   strcat(MAPTAGS,"spare3  "); ntag++;

   return ntag;
}

//--------------------
// tm_define_map_tmax_ntuple_fields
//--------------------
int tm_define_map_tmax_ntuple_fields(void)
{
   int ntag=0;
   
   // define ntuple fields
   *MAPTAGS=0;
   strcat(MAPTAGS,"sector  "); ntag++;
   for(int i=1;i<=36;i++){
		char str[256];
      sprintf(str,"layer%d  ",i);
		str[8] = 0; // ensure string is only 8 characters long
		strcat(MAPTAGS, str);
		ntag++;
   }
   return ntag;
}

//--------------------
// tm_book_ntuples
//--------------------
void tm_book_ntuples(int runno)
{
   char TAG[32];
   int ntag=0;
   int NID,book_this_one;
   int sec,sup,k;
   char CHTITL[16],title[128];
   float a,nt_array[32];
   int i,j;
   char *ptr,mapfile[256]="/group/clas/parms/Maps/DC_DOCA.map";
   char subsys[64],item[64];
   int firsttime;
   float par[50];
   int icycle;
   FILE *fpx;
   char fname[256];

   
   /* make directories that mirror sslt */
   hmdir("TBT","S");
   hcdir("//PAWC"," ");
   hmdir("TBT","S"); 

   hcdir("//esr/TBT"," ");
   hmdir("proton","S");
   hcdir("//PAWC/TBT"," ");
   hmdir("proton","S");
   
   /* define ntuple fields */
   ntag=tm_define_ntuple_fields();
   
   /* book the ntuples */
   for(sup=1;sup<=6;sup++){
		NID=1000+sup;
		sprintf(CHTITL,"SL%d",sup);
		k=1000;
		hbookn(NID,CHTITL,ntag,"esr/TBT/proton",k,CHTAGS);
		fprintf(stderr,"Ntuple %d booked.\n",NID);
   }

   /* Create directory to hold map constants */
   hcdir("//esr"," ");
   hmdir("MAP","S");
   hcdir("//PAWC"," ");
   hmdir("MAP","S");
   
   ntag=tm_define_map_ntuple_fields();
   NID=1000;
   sprintf(CHTITL,"xvst_params");
   hbookn(NID,CHTITL,ntag,"esr/MAP",k,MAPTAGS);
   
   ntag=tm_define_map_tmax_ntuple_fields();
   NID=2000;
   sprintf(CHTITL,"t_max");
   hbookn(NID,CHTITL,ntag,"esr/MAP",k,MAPTAGS);
   
   hbook1(3001,"sigma R1", 50, 0.0, 1.0, 0);
   hbook1(3002,"sigma R2", 50, 0.0, 1.0, 0);
   hbook1(3003,"sigma R3", 50, 0.0, 1.0, 0);

   hbook1(4001,"timewalk R1" ,10,1.0,11.0,0);
   hbook1(4002,"timewalk R2" ,10,1.0,11.0,0);
   hbook1(4003,"timewalk R3" ,10,1.0,11.0,0);
   hbook1(4004,"factor"      , 3,1.0, 4.0,0);
   hbook1(4005,"tau"         , 3,1.0, 4.0,0);
   hbook1(4006,"functiontype", 1,1.0, 2.0,0);
   hbook1(4007,"betaslope"   , 6,1.0, 7.0,0);

   hbook1(5000,"runnumber"   , 1,1.0, 2.0,0);
   hbook1(5001,"dclib version numbers",6,1.0, 7.0,0);

   ptr=getenv("CLAS_PARMS");
   if(ptr)sprintf(mapfile,"%s/Maps/DC_DOCA.map",ptr);

   /* now fill with map contents */
   for(sec=1;sec<=6;sec++){
      strcpy(subsys,"t_max");
      sprintf(item,"Sector%d",sec);
      par[0]=sec;
      map_get_float(mapfile,subsys,item,36,&par[1],runno,&firsttime);
      hfn(2000,par);
      
      sprintf(subsys,"xvst_par_Sect%d",sec);
      if(sec==1)strcpy(subsys,"xvst_params");
      for(sup=1;sup<=6;sup++){
         sprintf(item,"SL%d",sup);
         par[0]=sec;
         par[1]=sup;
         map_get_float(mapfile,subsys,item,24,&par[2],runno,&firsttime);
         hfn(1000,par);
      }
   }


   map_get_float(mapfile,"Sigma","Region1",50,par,runno,&firsttime);
   hpak(3001,par);
   map_get_float(mapfile,"Sigma","Region2",50,par,runno,&firsttime);
   hpak(3002,par);
   map_get_float(mapfile,"Sigma","Region3",50,par,runno,&firsttime);
   hpak(3003,par);

   map_get_float(mapfile,"Timewalk","Region1",10,par,runno,&firsttime);
   hpak(4001,par);
   map_get_float(mapfile,"Timewalk","Region2",10,par,runno,&firsttime);
   hpak(4002,par);
   map_get_float(mapfile,"Timewalk","Region3",10,par,runno,&firsttime);
   hpak(4003,par);
   map_get_float(mapfile,"Timewalk","factor",3,par,runno,&firsttime);
   hpak(4004,par);
   map_get_float(mapfile,"Timewalk","tau",3,par,runno,&firsttime);
   hpak(4005,par);
   map_get_int(mapfile,"Timewalk","functiontype",1,&j,runno,&firsttime);
	par[0]=(float)j;
   hpak(4006,par);
   map_get_float(mapfile,"Timewalk","betaslope",6,par,runno,&firsttime);
   hpak(4007,par);

	par[0]=(float)runno;
   hpak(5000,par);
   
#ifdef DC_DCH_VERSION_MAJOR_h
	par[0]=DC_DCLIB_VERSION_MAJOR;        /* from dc_xvst_fct.c in libdc.a */
	par[1]=DC_DCLIB_VERSION_MINOR;        /* from dc_xvst_fct.c in libdc.a */
	par[2]=DC_DCH_VERSION_MAJOR;          /* from dc.h used in libdc.a     */
	par[3]=DC_DCH_VERSION_MINOR;          /* from dc.h used in libdc.a     */
	par[4]=DC_DCH_VERSION_MAJOR_h;        /* from dc.h included here       */
	par[5]=DC_DCH_VERSION_MINOR_h;        /* from dc.h included here       */
#else
   par[0]=par[1]=par[2]=par[3]=par[4]=par[5]=0.0;
#endif
	hpak(5001,par);
   
   hcdir("//PAWC/MAP"," ");
   hcdir("//esr/MAP"," ");
   /*hrout(0,icycle,"T");*/

   hcdir("//esr"," ");   
   hcdir("//PAWC"," ");

}


/*************************************************/
/* Find index if string identifying ntuple field */
/*************************************************/
int tm_find_ntuple_index(char *name)
{
   int n;
   char *ptr;

   ptr=strstr(CHTAGS,name);
   if(ptr==NULL){
      fprintf(stderr,"\n UNKNOWN NTUPLE FIELD \"%s\" !\n\n",name);
      exit(-1);
   }
   n=(int)(ptr-CHTAGS)/8;
   return n;
}

/*******************************************************************/
/* Check if the current event passes all the cuts on ntuple fields */
/* (See note at top about how cuts are implemented)                */
/*******************************************************************/
int tm_pass_ntuple_cut(float *vals)
{
   int i;
   int reg;
   
   for(i=0;i<19;i++)if(isnan((double)vals[i]))return 0;

   for(i=0;i<tm_num_ntuple_cuts;i++){
      /*printf("\tfield:%d  factor:%+1.1f  limit:%f  val:%f\n"
         ,CUTS[i].field
         ,CUTS[i].factor
         ,CUTS[i].limit
         ,vals[CUTS[i].field]
         );fflush(stdout);
      */
      if(vals[CUTS[i].field]*CUTS[i].factor > CUTS[i].limit) return 0;
   }

   if (mass_cut_var == 1){
     mass = vals[12]*sqrt(1/(vals[9]*vals[9]) - 1);
     if (mass>mass_cuts.upper || mass<mass_cuts.lower){
       return 0;
     }
   }

   reg = (int)((vals[1]-1)/12) + 1;
   /*   fprintf(stderr,"Local Angle cuts:  reg[%d] L.A. LOW: %f:   , L.A. HIGH: %f: \n",reg,LOCANGLE_CUT_LOW[reg],LOCANGLE_CUT_HIGH[reg]); */
   if(reg<1 || reg>3) return 0;
   if(vals[11]<LOCANGLE_CUT_LOW[reg]) return 0;
   if(vals[11]>LOCANGLE_CUT_HIGH[reg]) return 0;
      
   return 1;
}


/**********************************************/
/* Parse a command-line argument to set a cut */
/**********************************************/
void tm_set_ntuple_cut(char *arg)
{
   char field[256],op[64],limit[256];
   int k;
   
   arg++;
   k=0;
   while(*arg!='.' && *arg!=0) field[k++]=*(arg++);
   field[k]=0;

   if(*arg=='.')arg++;
   k=0;
   while(*arg!='.' && *arg!=0)op[k++]=*(arg++);
   op[k]=0;

   if(*arg=='.')arg++;
   k=0;
   while(*arg!=0)limit[k++]=*(arg++);
   limit[k]=0;


   if(strlen(field)==0 || strlen(op)==0 || strlen(limit)==0 ){
      fprintf(stderr,"Bad ntuple cut parameter \"%s\"\n",arg);
      fprintf(stderr,"Should be of the form \"-Cfield.op.limit\"\n");
      fprintf(stderr,"where\n field is an ntuple field (e.g. beta)\n");
      fprintf(stderr," op is one of \"lt\" or \"gt\"\n");
      fprintf(stderr," limit is the exclusive limit on the field.\n\n");
      fprintf(stderr,"e.g  \"-Cbeta.lt.0.9\" will only place events in\n");
      fprintf(stderr,"in the ntuples in which the particle had a beta of\n");
      fprintf(stderr,"less than 0.9\n\n");
      exit(-1);
   }
   
   tm_define_ntuple_fields();

   CUTS[tm_num_ntuple_cuts].field=tm_find_ntuple_index(field);
   if(!strcmp(op,"lt")){
      CUTS[tm_num_ntuple_cuts].factor=+1.0;
      CUTS[tm_num_ntuple_cuts].limit = atof(limit);
   }else{
      CUTS[tm_num_ntuple_cuts].factor=-1.0;
      CUTS[tm_num_ntuple_cuts].limit =-atof(limit);
   }
   
   fprintf(stderr,"Ntuple cut added: %s(field=%d) %s %f\n"
      ,field
      ,CUTS[tm_num_ntuple_cuts].field
      ,CUTS[tm_num_ntuple_cuts].factor<0.0 ? ">":"<"
      ,CUTS[tm_num_ntuple_cuts].limit*CUTS[tm_num_ntuple_cuts].factor);
   
   tm_num_ntuple_cuts++;
}


//--------------------
// tm_set_mass_upper
//--------------------
void tm_set_mass_upper(float upper){
  mass_cuts.upper = upper;
}


//--------------------
// tm_set_mass_lower
//--------------------
void tm_set_mass_lower(float lower){
  mass_cuts.lower = lower;
}


//--------------------
// tm_use_mass_cuts
//--------------------
void tm_use_mass_cuts(void){
  if (set_mass_cuts == 0){
    set_mass_cuts++;
    mass_cuts.lower = -1;
    mass_cuts.upper = 500000;
    mass_cut_var = 1;
  }
}


