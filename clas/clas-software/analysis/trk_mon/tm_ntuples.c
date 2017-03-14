
/*********************************************************/
/*  tm_ntuples.c                                         */
/*                                                       */
/* 2/99  D. Lawrence                                     */
/*                                                       */
/* Make the ntuples like those made by sslt and used by  */
/* dc_calib_check.                                       */
/*********************************************************/


#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <utility.h>
#include <particleType.h>
#include <clas_cern.h>
#include <dc.h>

extern int mkntuple[7][7];
extern int quit_program;

char CHTAGS[512],MAPTAGS[512];

char SAVE_CHPATH[256];

int MAX_NTUPLE_ROWS=1600000;

float langle_cuts_lo[7]={0.0, -29.0, -27.0, -12.0, -12.0, 2.0 , 2.0 };
float langle_cuts_hi[7]={0.0, -22.0, -20.0, -2.0 , -2.0 , 20.0, 20.0};

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
float LOCANGLE_CUT_HIGH[4],LOCANGLE_CUT_LOW[4];


/*********************************************************/
/* Data structure to hold info on cuts.                  */
/*                                                       */
/* The test will be:                                     */
/*                                                       */
/*   val[field]*factor<limit                             */
/*                                                       */
/* So if the desired test is a "less than" test, then    */
/* factor=+1.0 and limit is just the limit passed on     */
/* the command line.                                     */
/*                                                       */
/* If the desired test is a "greater than" test, then    */
/* factor=-1.0 and limit is the negative of what was     */
/* passed on the command line. This way, the test really */
/* just requires one extra multiplication at event time  */
/* making the program faster.                            */
/*********************************************************/
typedef struct {
   int field;
   float limit;
   float factor;
}tm_ntuple_cut_t;
tm_ntuple_cut_t CUTS[100];
int tm_num_ntuple_cuts=0;

typedef struct {
  float lower;
  float upper;
}tm_mass_cut_t;

tm_mass_cut_t mass_cuts;

void SAVE_HBOOK_DIR(void)
{
   bzero(SAVE_CHPATH,256);
   hcdir_(SAVE_CHPATH,"R",64,strlen("R"));
}

void RESTORE_HBOOK_DIR(void)
{
   hcdir(SAVE_CHPATH," ");
}

int tm_define_ntuple_fields(void)
{
   char TAG[256];
   int ntag=0;
   
   /* define ntuple fields */
   memset(CHTAGS,' ',8*20);
   CHTAGS[(8*20)-1]=0;
   strcpy(TAG,"sector  ");strncpy(&CHTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"layer   ");strncpy(&CHTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"wire    ");strncpy(&CHTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"time    ");strncpy(&CHTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"fitdoca ");strncpy(&CHTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"resi    ");strncpy(&CHTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"B       ");strncpy(&CHTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"B1      ");strncpy(&CHTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"B2      ");strncpy(&CHTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"beta    ");strncpy(&CHTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"phi     ");strncpy(&CHTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"locangle");strncpy(&CHTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"P       ");strncpy(&CHTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"q       ");strncpy(&CHTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"chisq   ");strncpy(&CHTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"hit     ");strncpy(&CHTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"DC1     ");strncpy(&CHTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"calcdoca");strncpy(&CHTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"ctime   ");strncpy(&CHTAGS[8*(ntag++)],TAG,strlen(TAG));

   return ntag;
}

int tm_define_map_ntuple_fields(void)
{
   char TAG[256];
   int ntag=0;
   
   /* define ntuple fields */
   memset(MAPTAGS,' ',8*27);
   MAPTAGS[(8*26)-1]=0;
   strcpy(TAG,"sector  ");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"super   ");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"avg_lang");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"tzero   ");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"tmax    ");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"avg_B   ");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"ff      ");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"par1    ");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"par2    ");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"par3    ");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"par4    ");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"layer   ");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"par1_ang");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"par2_ang");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"par3_ang");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"spare1  ");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"tim_corr");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"par1_B  ");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"par2_B  ");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"par3_B  ");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"par4_B  ");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"spare2  ");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"par1tmax");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"par2tmax");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"par3tmax");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   strcpy(TAG,"spare3  ");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));

   return ntag;
}

int tm_define_map_tmax_ntuple_fields(void)
{
   char TAG[256];
   int ntag=0;
   int i;
   
   /* define ntuple fields */
   memset(MAPTAGS,' ',8*38);
   MAPTAGS[(8*37)-1]=0;
   strcpy(TAG,"sector  ");strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   for(i=1;i<=36;i++){
      sprintf(TAG,"layer%d  ",i);strncpy(&MAPTAGS[8*(ntag++)],TAG,strlen(TAG));
   }
   return ntag;
}

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
   
   /* book the requested ntuples */
   for(sup=1;sup<=6;sup++){
      book_this_one=0;
      NID=1000+sup;
      for(sec=1;sec<=6;sec++){
         if(mkntuple[sec][sup])book_this_one=1;
      }
      if(book_this_one){
         sprintf(CHTITL,"SL%d",sup);
         k=1000;
         hbookn_(&NID,CHTITL,&ntag,"esr/TBT/proton",&k,CHTAGS
            ,strlen(CHTITL),strlen("esr/TBT/proton"),8L);
         fprintf(stderr,"Ntuple %d booked.\n",NID);
      }else{
         fprintf(stderr,"Ntuple %d skipped.\n",NID);
      
      }
   }

   /* Create directory to hold map constants */
   hcdir("//esr"," ");
   hmdir("MAP","S");
   hcdir("//PAWC"," ");
   hmdir("MAP","S");
   
   ntag=tm_define_map_ntuple_fields();
   NID=1000;
   sprintf(CHTITL,"xvst_params");
   hbookn_(&NID,CHTITL,&ntag,"esr/MAP",&k,MAPTAGS
            ,strlen(CHTITL),strlen("esr/MAP"),8L);
   
   ntag=tm_define_map_tmax_ntuple_fields();
   NID=2000;
   sprintf(CHTITL,"t_max");
   hbookn_(&NID,CHTITL,&ntag,"esr/MAP",&k,MAPTAGS
            ,strlen(CHTITL),strlen("esr/MAP"),8L);
   
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

/*  from bosddl.h */
/* ------------------------ tbla -----------------*/
/*typedef struct {
/*        int trk_pln;    /*  (track_number) *100 + Trk_plane_number */
/*        vector3_t pos;    /* coord [cm]  for track in this plane */
/*        vector3_t dir;    /*  direction cosine (cx) at coord.{x,y,z} */
/*        float tlen;    /*  track length [cm] from origin to this plane */
/*        int dc1;    /*  Pointer to DC1 bank */
/*        int stat;    /*  Status of the hit */
/*        int wire;    /*  Wire number  */
/*        float dtime;    /*  drift time  [ns] */
/*        float alpha;    /*  track angle (relative to R of SL) [deg] */
/*        float wlen;    /*  Wire length (hit pos. to preamp)  [cm] */
/*        float sgdoca;    /*  sigma DOCA  [cm] */
/*        float fitdoca;    /*  Fitted DOCA [cm] */
/*        float calcdoca;    /*  calculated DOCA (via dtime)  [cm] */
/*} tbla_t;
*/

/********************************************************************/
/* OK, here we need info from at least three banks:
/*
/* TBID -> beta
/* TBLA -> dtime, fitdoca, calcdoca, alpha, wire, B-field
/* TBTR -> phi,P
/*
/* The TBID bank has an overall track number, as well as the sector
/* the track was in.
/*
/* The TBLA bank has the overall track number, but is indexed by
/* the sector (getGroup(&bcs,"TBLA",sector)).
/*
/* The TBTR bank only has the track number within the sector and 
/* the sector, but is indexed by "track" in TBID if "track">0.
/* 
/*******************************************************************/
int tm_fill_ntuples(void)
{
  int success = 0;
   float nt_array[32];
   int sup,layer;
   float *B;
   int i,j,NID;
   clasTBID_t *TBID = getGroup(&bcs_, "TBID",1);
   clasTBTR_t *TBTR = getBank(&bcs_, "TBTR");
   clasTBLA_t *TBLA = NULL;
   tbid_t *tbid;
   tbtr_t *tbtr;
   tbla_t *tbla;
   float phi,P,px,py,pz;
   vector3_t *pvec;
   float q,chisq;
   int hit;
   static int TOTAL_NTUPLE_ROWS=0;
   float c_time;
   float time,beta;
   int sec;
   int sec_t,sup_t;
   float time_t,timec_t,beta_t;

   /* Make sure we don't overfill the ntuples */
   if(TOTAL_NTUPLE_ROWS>=MAX_NTUPLE_ROWS){
      if(!quit_program)printf("\n Maximum number of Ntuple rows filled (%d).\n\n"
         ,MAX_NTUPLE_ROWS);
      quit_program=1;
      return(success);
   }

	if(!TBID)TBID=getBank(&bcs_, "TBID");
  	if(TBID==NULL || TBTR==NULL)return(success);

   /*--- Loop over TBID banks ---*/
   for(i=0;i<TBID->bank.nrow;i++){
      tbid=&TBID->tbid[i];

      if(tbid->track<1)continue;

      tbtr=&TBTR->tbtr[tbid->track - 1];
      
      /*-- loop over tracks in sector to find right set of TBLA banks --*/
      TBLA=getGroup(&bcs_, "TBLA",tbid->sec);
      if(TBLA==NULL)continue;
      tbla=NULL;
      for(j=0;j<TBLA->bank.nrow;j+=34){
/*         printf("%d:TBLA:%d  tbid:%d  nrow:%d  trk_pln:%d  sec:%d\n"
            ,cntr
            ,TBLA->tbla[j].trk_pln/100
            ,tbid->track
            ,TBLA->bank.nrow
            ,TBLA->tbla[j].trk_pln
            ,tbid->sec);
*/
         if((TBLA->tbla[j].trk_pln/100) == tbid->track){
            tbla=&TBLA->tbla[j];
            break;
         }
      }
      if(!tbla){
         /*fprintf(stderr,"Unable to find TBT track in TBLA banks!\n");*/
         return(success);
      }
      
      /*----------------------------------------------------------------*/
      /* at this point the tbid,tbtr, and tbla pointers should be valid */
      /*----------------------------------------------------------------*/

      pvec=&tbtr->p;
      px=pvec->x;  py=pvec->y;  pz=pvec->z;

      P=sqrt((px*px) + (py*py) + (pz*pz));
      
      q=tbtr->q;
      chisq=tbtr->chi2;

      /*NEW METHOD. 12/15/00 -JL */
      phi = atan2(py,px)*(180.0/3.1415);
      if(phi<0.0){
	phi+=360.0;
      }

      /*--- loop over tbla rows ---*/
      for(j=0;j<34;j++){

	 /* if(tbla->stat != 0){tbla++;continue;} */ 
         if(tbla->dtime<-50.0 || tbla->dtime>3000.0){tbla++;continue;}
	 /* if(tbla->dc1<0){tbla++;continue;} */
	 /* if(chisq >= 10.0){tbla++;continue;}*/ /*throw out unreasonable tracks*/

         layer=(tbla->trk_pln)%100 - 3;
         sup=((layer-1)/6) + 1;
         if(sup>6 || sup<1){tbla++;continue;}
         B=(float*)&tbla->dir; /* changed to hold B-field some time ago by Franz */

         nt_array[0] =tbid->sec;
         nt_array[1] =layer;
         nt_array[2] =tbla->wire;
         nt_array[3] =tbla->dtime;
         nt_array[4] =tbla->fitdoca;
         nt_array[5] =fabs(tbla->fitdoca)-fabs(tbla->calcdoca);
         nt_array[6] =sqrt((B[0]*B[0]) + (B[1]*B[1]) + (B[2]*B[2]));
         nt_array[7] =B[0];
         nt_array[8] =B[1];
         nt_array[9] =tbid->beta;
         nt_array[10]=phi;
         nt_array[11]=tbla->alpha*180.0/3.14159;
         nt_array[12]=P;
         nt_array[13]=q;
	 nt_array[14]=chisq;
	 nt_array[15]=tbla->stat;
	 nt_array[16]=tbla->dc1;
	 nt_array[17]=tbla->calcdoca;
	 
	 c_time=0.0;
	 time = tbla->dtime;
	 sec = tbid->sec;
	 beta = tbid->beta;
	
	 c_time = dc_time_correction_(&time,&layer,&sec,&beta);

	 /**************************************************/
	 /*
	 sec_t=3;
	 sup_t=3;
	 time_t=300;
	 fprintf(stderr,"a");
	 for(i=1;i<=100;i++){
	   beta_t=((float)i)*0.01;
	   timec_t=dc_time_correction_(&time_t,&sup_t,&sec_t,&beta_t);
	   fprintf(stderr,"beta: %f    timec: %f\n",beta_t,timec_t);
	 }
	 exit(0);
	 */
	 /**************************************************/

	 nt_array[18]=c_time;

	 /*
	 if (tbla->stat!=0){
	   nt_array[15]=0;
	 }else{
	   nt_array[15]=1;
	 }
	 */

          /* Check command-line cuts and write out event if passed */
         if(tm_pass_ntuple_cut(nt_array)){
            hcdir("//esr/TBT/proton"," ");
            hcdir("//PAWC/TBT/proton"," ");
            NID=1000+sup;
            hfn_(&NID,nt_array);
            hcdir("//esr"," ");
            hcdir("//PAWC"," ");
            TOTAL_NTUPLE_ROWS++;
	    success = 1;
         }
	 tbla++;
         
      }/* end of loop over TBLA bank rows (34 of them) */

   } /* end of loop over TBID->bank.nrow */
   return(success);
}

void tm_end_ntuples(void)
{
   int k=0;
   
   hcdir("//PAWC/MAP"," ");
   hcdir("//esr/MAP"," ");  

   hcdir("//PAWC/TBT/proton"," ");
   hcdir("//esr/TBT/proton"," ");
   
   rzpurg_(&k);
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

   reg = (vals[1]-1)/12 + 1;
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

void tm_set_mass_upper(float upper){
  mass_cuts.upper = upper;
}

void tm_set_mass_lower(float lower){
  mass_cuts.lower = lower;
}

void tm_use_mass_cuts(void){
  if (set_mass_cuts == 0){
    set_mass_cuts++;
    mass_cuts.lower = -1;
    mass_cuts.upper = 500000;
    mass_cut_var = 1;
  }
}








