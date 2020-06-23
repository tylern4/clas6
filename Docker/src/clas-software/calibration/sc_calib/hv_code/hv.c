/*********************************************** 
A PROGRAM TO CALCULATE PMT HIGH VOLTAGE SETTINGS
FROM ANALYSED AND FITTED COSMIC RAY DATA
****************************************
 May 10, 2001
 Joe Santoro
****************/
  
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <curses.h>
#include <bostypes.h>
#include <clas_cern.h>
#include <kinematics.h>
#include <map_manager.h>
#include <mysql.h>
#include <utility.h>
#include <particleType.h>
#include <printBOS.h> 
#include <pid.h>

/******** PAW definitions ************/
#define MEMH 500000 
#define LREC 1024 /* record length of hbook direct access file in WORDS */
#define LUN 3 /* logical unit number of hbook file */
/***************************************************/

float V_old_l,V_old_r;
float V_new_l,V_new_r;
float V_old_minus_l,V_old_minus_r; 
float V_dum1,V_dum2;
float DEL;

float G_peak_l[290];
float centroid_l[290];
float G_peak_l2[290];
float centroid_l2[290];
float l_correct[48] = {9.291, 6.232, 4.688, 3.757, 3.134, 2.815,
         2.451, 2.170, 1.946, 1.765, 1.614, 1.487, 1.379, 1.285,
         1.204, 1.132, 1.068, 1.011, .9595, .9132, .8711, .8328,
         .7977, .8080, .7933, .7792, .7656, .7524, .7396, .7273,
         .7154, .7039, .6928, .6820, .6741, .6829, .6920, .7012,
         .7108, .7257, .7465, .7684, .8069, .8857, .9817, 1.101,
         1.242, 1.374};

int a,b,c,d,e,f,g,dum4,dum5;
float q,r,s,t,u,v,dum1,dum2,dum3,dum6,dum7,dum8,dum9,dum10,dum11,dum12;

int k=1,z=1,n,counter;
char scint_id[50];
char name_ofOutfile1[100];
int f1,f2,f3,f4,blah;
char *HB = NULL;


/***** declare the hbook common ****/

float pawc_[MEMH];
float hcfitd_[25];
int quest_[100];


/* Function Definitions    */
/***************************/

float hv_new_l(float G_peak, float V_old, float centroid, char *ID, FILE *fp3out, char *fname, int count);
float hv_new_r(float G_peak, float V_old, float centroid, char *ID, FILE *fp3out, char *fname, int count);
void PrintUsage(char *processName);
void book_histos(char *fname);
void hini(char *out, char *hb);


/************************************* BEGIN MAIN *****************************************/

main(int argc, char *argv[])

{

 int icycle;
 int  i; 
 char *outfile = NULL;

 char *argptr;
 char *name_ofInfile1 = NULL; 
 char *name_ofInfile2 = NULL, *name_ofInfile4 = NULL;
 char *name_ofInfile5 = NULL, *name_ofInfile7 = NULL; 
 char name_ofOutfile1[100];
 char alarm[100] = "alarm.dat"; 
 char *mode_ptr = NULL; 

   FILE *fp1in; 
   FILE *fp1out;
   FILE *fp3out;

   FILE *fp2in; 
   FILE *fp4in; 
   FILE *fp5in; 
   FILE *fp7in; 

   FILE *scf1;
   FILE *scf2;
   FILE *scf3;
   FILE *scf4;
   FILE *scf5;
   FILE *scf6;

/*-------------- Command Line Routine--------------------- */
  
   for (i=1; i<argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {
       case 'f':
	name_ofInfile1 = ++argptr;
        break;       
       case 'g':
	name_ofInfile2 = ++argptr;
	break;
       case 'j':
	name_ofInfile4 = ++argptr;
	break;     
       case 'k':
	name_ofInfile5 = ++argptr;        
	break;      
       case 'm':
	name_ofInfile7 = ++argptr;        
	break;
       case 'M':
	mode_ptr = ++argptr;
	break;
       case 'h':
        PrintUsage(argv[0]);
        break;
       default:
	printf("\033[01;106mYou Gotta Read the Directions First!!!\033[m\n\n");
	PrintUsage(argv[0]);
	break;
      }
    }
  }
  

   /*Reads HV file from CLON machines */
   fp1in = fopen(name_ofInfile1, "r");     
  
   /**** ALARM FILE *********/
   fp3out = fopen(alarm,"a");


/*==================== FORWARD CARRIAGE ===================================*/


    f1 = strcmp(name_ofInfile1,"scf.dat");
    f2 = strcmp(name_ofInfile1,"sc_space.dat");
    f3 = strcmp(name_ofInfile1,"sc_north.dat");
    f4 = strcmp(name_ofInfile1,"sc_south.dat");

    if ( f1 == 0){

   /************ A's *****************/
   /*Reads in min_parm from peak fits at nominal voltage */
   fp2in = fopen(name_ofInfile2, "r");  
    /*Reads in min_parm from ln(adcl/adcr) fits */
   fp4in = fopen(name_ofInfile4, "r"); 

   

    /********* PAW STUFF *****************/
    HB = "N";   
    hini(outfile,HB); /* crappy PAW function */ 
    printf("\033[01;93m\n\nProcessing: %s\033[m\n",name_ofInfile1);
    strcpy(name_ofOutfile1,"scf_new.dat");
    book_histos(name_ofOutfile1); /* booking the histograms */   

   /*Outputs New High Voltage File */   
   fp1out = fopen(name_ofOutfile1, "w");  
   scf1   = fopen("sc1.dat", "w");
   scf2   = fopen("sc2.dat", "w");
   scf3   = fopen("sc3.dat", "w");
   scf4   = fopen("sc4.dat", "w");
   scf5   = fopen("sc5.dat", "w");
   scf6   = fopen("sc6.dat", "w");


   /*==========  Create Data Arrays ==========*/
  
   for(k=1;k<289;k++){
   fscanf(fp2in,"%d%f%f%f%d\n",&dum1,&G_peak_l[k],&dum2,&dum3,&dum4);
   fscanf(fp4in,"%d%f%f%f%f%f%f%f%f\n",&dum5,&centroid_l[k],&dum6,&dum7,&dum8,&dum9,&dum10,&dum11,&dum12);
   }

   
   
  /*===== Loop over High Voltage Files and Create updated file with new voltages =====*/

   for(z=1;z<277;z++){
   fscanf(fp1in,"%s%d%d%d%d%d%d%d%f%f%f%f%f%f%f\n",scint_id,&a,&b,&c,&d,&e,&f,&g,&V_old_l,&q,&r,&s,&t,&u,&v);
   
   
   if (z<24){
    V_new_l = hv_new_l(G_peak_l[z],  V_old_l, centroid_l[z],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v); 
 fprintf(scf1,  "%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);      
    }
   if (z>23 && z<47){
    V_new_r = hv_new_r(G_peak_l[z-23],  V_old_l, centroid_l[z-23],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v); 
 fprintf(scf1,  "%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v);  
     }
   
   if (z>46 && z<70){
    V_new_l = hv_new_l(G_peak_l[z+2],  V_old_l, centroid_l[z+2],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v); 
 fprintf(scf2,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);     
    }
   if (z>69 && z<93){
    V_new_r = hv_new_r(G_peak_l[z-21],  V_old_l, centroid_l[z-21],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v); 
 fprintf(scf2,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v);  
     }
   
   if (z>92 && z<116){
    V_new_l = hv_new_l(G_peak_l[z+4],  V_old_l, centroid_l[z+4],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v); 
 fprintf(scf3,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);     
    }
   if (z>115 && z<139){
    V_new_r = hv_new_r(G_peak_l[z-19],  V_old_l, centroid_l[z-19],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v); 
 fprintf(scf3,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v);
      }

   if (z>138 && z<162){
    V_new_l = hv_new_l(G_peak_l[z+6],  V_old_l, centroid_l[z+6],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);
 fprintf(scf4,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);      
    }
   if (z>161 && z<185){
    V_new_r = hv_new_r(G_peak_l[z-17],  V_old_l, centroid_l[z-17],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v);
 fprintf(scf4,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v);  
     }

   if (z>184 && z<208){
    V_new_l = hv_new_l(G_peak_l[z+8],  V_old_l, centroid_l[z+8],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);  
 fprintf(scf5,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);     
    }
   if (z>207 && z<231){
    V_new_r = hv_new_r(G_peak_l[z-15],  V_old_l, centroid_l[z-15],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v); 
 fprintf(scf5,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v); 
    }

   if (z>230 && z<254){
    V_new_l = hv_new_l(G_peak_l[z+10],  V_old_l, centroid_l[z+10],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v); 
 fprintf(scf6,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);  
    }
   if (z>253 && z<277){
    V_new_r = hv_new_r(G_peak_l[z-13],  V_old_l, centroid_l[z-13],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v);
 fprintf(scf6,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v); 
    }  

  }
}
 

/*======================= SPACE FRAME ====================================================================*/
   
    
   if ( f2 == 0){ 

   /************ A's *****************/
   /*Reads in min_parm from peak fits at nominal voltage */
   fp2in = fopen(name_ofInfile2, "r"); 
    /*Reads in min_parm from ln(adcl/adcr) fits */
   fp4in = fopen(name_ofInfile4, "r");

   /**************** B's ***********/

  /*Reads in min_parm from peak fits at nominal voltage */
   fp5in = fopen(name_ofInfile5, "r");  
   /*Reads in min_parm from ln(adcl/adcr) fits */
   fp7in = fopen(name_ofInfile7, "r");

   blah = strcmp(mode_ptr,"group");  
   if (blah == 0){
   HB = "U";
   }
   else{
   HB = "N";
   }
   
   printf("\033[01;93m\n\nProcessing: %s\033[m\n",name_ofInfile1);
   strcpy(name_ofOutfile1,"sc_space_new.dat");
   
   /********* PAW STUFF *****************/        
   hini(outfile,HB); /* crappy PAW function */
   book_histos(name_ofOutfile1); /* booking the histograms */


   /*Outputs New High Voltage File */   
   fp1out = fopen(name_ofOutfile1, "w");  
 
   /*==========  Create Data Arrays ==========*/
  
   for(k=1;k<289;k++){
   fscanf(fp2in,"%d%f%f%f%d\n",&dum1,&G_peak_l[k],&dum2,&dum3,&dum4);
   fscanf(fp4in,"%d%f%f%f%f%f%f%f%f\n",&dum5,&centroid_l[k],&dum6,&dum7,&dum8,&dum9,&dum10,&dum11,&dum12);
   
   fscanf(fp5in,"%d%f%f%f%d\n",&dum1,&G_peak_l2[k],&dum2,&dum3,&dum4);
   fscanf(fp7in,"%d%f%f%f%f%f%f%f%f\n",&dum5,&centroid_l2[k],&dum6,&dum7,&dum8,&dum9,&dum10,&dum11,&dum12);
   }

   /*===== Loop over High Voltage Files and Create updated file with new voltages =====*/

   for(z=1;z<145; z++){
   fscanf(fp1in,"%s%d%d%d%d%d%d%d%f%f%f%f%f%f%f",scint_id,&a,&b,&c,&d,&e,&f,&g,&V_old_l,&q,&r,&s,&t,&u,&v);
   
   n = ((z+1)/2);
   
   /*********** a ***************/
    if (z<13 && z!=2*n){
 V_new_l = hv_new_l(G_peak_l[n+42],  V_old_l, centroid_l[n+42],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);    
    }
   /************  b *************/ 
    if (z<13 && z==2*n){
 V_new_l = hv_new_l(G_peak_l2[n+42],  V_old_l, centroid_l2[n+42],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);    
    }
   
    /******  a ***********/
    if (z>12 && z<25 && z!=2*n){
 V_new_r = hv_new_r(G_peak_l[n+36],  V_old_l, centroid_l[n+36],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v); 
    }
    /***** b ***********/
    if (z>12 && z<25 && z==2*n){
 V_new_r = hv_new_r(G_peak_l2[n+36],  V_old_l, centroid_l2[n+36],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v); 
    }
   
    /***** a ********/
    if (z>24 && z<37 && z!=2*n){
 V_new_l = hv_new_l(G_peak_l[n+78],  V_old_l, centroid_l[n+78],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);    
    }
    /**** b *******/
    if (z>24 && z<37 && z==2*n){
 V_new_l = hv_new_l(G_peak_l2[n+78],  V_old_l, centroid_l2[n+78],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);    
    }

    /**** a *****/
    if (z>36 && z<49 && z!=2*n){
 V_new_r = hv_new_r(G_peak_l[n+72],  V_old_l, centroid_l[n+72],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v);
    }
    /**** b ******/
    if (z>36 && z<49 && z==2*n){
 V_new_r = hv_new_r(G_peak_l2[n+72],  V_old_l, centroid_l2[n+72],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v);
    }
   
    /**** a ******/
    if (z>48 && z<61 && z!=2*n){
 V_new_l = hv_new_l(G_peak_l[n+114],  V_old_l, centroid_l[n+114],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);    
    }
    /***** b ******/
    if (z>48 && z<61 && z==2*n){
 V_new_l = hv_new_l(G_peak_l2[n+114],  V_old_l, centroid_l2[n+114],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);    
    }

    /**** a ******/
    if (z>60 && z<73 && z!=2*n){
 V_new_r = hv_new_r(G_peak_l[n+108],  V_old_l, centroid_l[n+108],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v);
    }
    /**** b ****/
    if (z>60 && z<73 && z==2*n){
 V_new_r = hv_new_r(G_peak_l2[n+108],  V_old_l, centroid_l2[n+108],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v);
    }

    /**** a ****/
    if (z>72 && z<85 && z!=2*n){
 V_new_l = hv_new_l(G_peak_l[n+150],  V_old_l, centroid_l[n+150],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);    
    }
    /***** b *****/
    if (z>72 && z<85 && z==2*n){
 V_new_l = hv_new_l(G_peak_l2[n+150],  V_old_l, centroid_l2[n+150],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);    
    }

    /***** a *******/
    if (z>84 && z<97 && z!=2*n){
 V_new_r = hv_new_r(G_peak_l[n+144],  V_old_l, centroid_l[n+144],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v);
    }
    /**** b *******/
    if (z>84 && z<97 && z==2*n){
 V_new_r = hv_new_r(G_peak_l2[n+144],  V_old_l, centroid_l2[n+144],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v);
    }

    /**** a ********/
    if (z>96 && z<109 && z!=2*n){
 V_new_l = hv_new_l(G_peak_l[n+186],  V_old_l, centroid_l[n+186],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);    
    }
    /***** b *****/
    if (z>96 && z<109 && z==2*n){
 V_new_l = hv_new_l(G_peak_l2[n+186],  V_old_l, centroid_l2[n+186],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);    
    }
   
    /****** a *******/
    if (z>108 && z<121 && z!=2*n){
 V_new_r = hv_new_r(G_peak_l[n+180],  V_old_l, centroid_l[n+180],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v);
    }
    /***** b *******/
    if (z>108 && z<121 && z==2*n){
 V_new_r = hv_new_r(G_peak_l2[n+180],  V_old_l, centroid_l2[n+180],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v);
    }

    /**** a *******/
    if (z>120 && z<133 && z!=2*n){
 V_new_l = hv_new_l(G_peak_l[n+222],  V_old_l, centroid_l[n+222],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);    
    } 
    /***** b *******/
    if (z>120 && z<133 && z==2*n){
 V_new_l = hv_new_l(G_peak_l2[n+222],  V_old_l, centroid_l2[n+222],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);    
    }
   
    /**** a *******/
    if (z>132 && z<145 && z!=2*n){
 V_new_r = hv_new_r(G_peak_l[n+216],  V_old_l, centroid_l[n+216],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v);
    }
    /****** b *****/
    if (z>132 && z<145 && z==2*n){
 V_new_r = hv_new_r(G_peak_l2[n+216],  V_old_l, centroid_l2[n+216],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v);
    }     
  

 }

}


/*======================= NORTH CARRIAGE  ====================================================================*/


   if ( f3 == 0){   
   /************ A's *****************/
   /*Reads in min_parm from peak fits at nominal voltage */
   fp2in = fopen(name_ofInfile2, "r"); 
    /*Reads in min_parm from ln(adcl/adcr) fits */
   fp4in = fopen(name_ofInfile4, "r");

   /**************** B's ***********/

  /*Reads in min_parm from peak fits at nominal voltage */
   fp5in = fopen(name_ofInfile5, "r");  
   /*Reads in min_parm from ln(adcl/adcr) fits */
   fp7in = fopen(name_ofInfile7, "r");


   blah = strcmp(mode_ptr,"group");  
   if (blah == 0){
   HB = "U";
   }
   else{
   HB = "N";
   }

   strcpy(name_ofOutfile1,"sc_north_new.dat");
   
   /********* PAW STUFF *****************/        
   hini(outfile,HB); /* crappy PAW function */
   book_histos(name_ofOutfile1); /* booking the histograms */


  printf("\033[01;93m\n\nProcessing: %s\033[m\n",name_ofInfile1); 
  /*Outputs New High Voltage File */   
  fp1out = fopen(name_ofOutfile1, "w");  
 
   /*==========  Create Data Arrays ==========*/
  
   for(k=1;k<289;k++){
   fscanf(fp2in,"%d%f%f%f%d\n",&dum1,&G_peak_l[k],&dum2,&dum3,&dum4);   
   fscanf(fp4in,"%d%f%f%f%f%f%f%f%f\n",&dum5,&centroid_l[k],&dum6,&dum7,&dum8,&dum9,&dum10,&dum11,&dum12);
    
   fscanf(fp5in,"%d%f%f%f%d\n",&dum1,&G_peak_l2[k],&dum2,&dum3,&dum4);
   fscanf(fp7in,"%d%f%f%f%f%f%f%f%f\n",&dum5,&centroid_l2[k],&dum6,&dum7,&dum8,&dum9,&dum10,&dum11,&dum12);
   }

   
   /*===== Loop over High Voltage Files and Create updated file with new voltages =====*/

   for(z=1;z<133; z++){
   fscanf(fp1in,"%s%d%d%d%d%d%d%d%f%f%f%f%f%f%f",scint_id,&a,&b,&c,&d,&e,&f,&g,&V_old_l,&q,&r,&s,&t,&u,&v);

   n = ((z+1)/2);

   /**************** SECTOR 5 **********************************************************/   
   if (z<17){
 V_new_l = hv_new_l(G_peak_l[z+215], V_old_l, centroid_l[z+215],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);     
   }
   if (z!=2*n && z>16 && z<23){
   /*********** a's ********/
 V_new_l = hv_new_l(G_peak_l[n+223],  V_old_l, centroid_l[n+223],scint_id,fp3out,name_ofOutfile1,z); 
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v); 
   }
   if (z==2*n && z>16 && z<23){
   /************ b's *******/
 V_new_l = hv_new_l(G_peak_l2[n+223],  V_old_l, centroid_l2[n+223],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v); 
   }
    
   if (z>22 && z<39){
 V_new_r = hv_new_r(G_peak_l[z+193],  V_old_l, centroid_l[z+193],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v);     
   }   
   if (z!=2*n && z>38 && z<45){
   /*********** a's ********/
 V_new_r = hv_new_r(G_peak_l[n+212],  V_old_l, centroid_l[n+212],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v); 
   }
   if (z==2*n && z>38 && z<45){
   /************ b's *******/
 V_new_r = hv_new_r(G_peak_l2[n+212],  V_old_l, centroid_l2[n+212],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v); 
   }

   /**************** SECTOR 4 ***********************************************************/
   if (z>44 && z<61){
 V_new_l = hv_new_l(G_peak_l[z+123],  V_old_l, centroid_l[z+123],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);     
   }
   if (z!=2*n && z>60 && z<67){
   /*********** a's ********/
 V_new_l = hv_new_l(G_peak_l[n+153],  V_old_l, centroid_l[n+153],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v); 
   }
   if (z==2*n && z>60 && z<67){
   /************ b's *******/
 V_new_l = hv_new_l(G_peak_l2[n+153],  V_old_l, centroid_l2[n+153],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v); 
   }

   if (z>66 && z<83){
 V_new_r = hv_new_r(G_peak_l[z+101],  V_old_l, centroid_l[z+101],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v);     
   }   
   if (z!=2*n && z>82 && z<89){
   /*********** a's ********/
 V_new_r = hv_new_r(G_peak_l[n+142], V_old_l, centroid_l[n+142],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v); 
   }
   if (z==2*n && z>82 && z<89){
   /************ b's *******/
 V_new_r = hv_new_r(G_peak_l2[n+142], V_old_l, centroid_l2[n+142],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v); 
   }

   
   /******************** SECTOR 3 ********************************************************/
   if (z>88 && z<105){
 V_new_l = hv_new_l(G_peak_l[z+31],  V_old_l, centroid_l[z+31],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);     
   }   
   if (z!=2*n && z>104 && z<111){
   /*********** a's ********/
 V_new_l = hv_new_l(G_peak_l[n+83],  V_old_l, centroid_l[n+83],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v); 
   }
   if (z==2*n && z>104 && z<111){
   /************ b's *******/
 V_new_l = hv_new_l(G_peak_l2[n+83],  V_old_l, centroid_l2[n+83],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v); 
   }
   
   if (z>110 && z<127){
 V_new_r = hv_new_r(G_peak_l[z+9],  V_old_l, centroid_l[z+9],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v);     
   }   
   if (z!=2*n && z>126 && z<133){
   /*********** a's ********/
 V_new_r = hv_new_r(G_peak_l[n+72],  V_old_l, centroid_l[n+72],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v); 
   }
   if (z==2*n && z>126 && z<133){
   /************ b's *******/
 V_new_r = hv_new_r(G_peak_l2[n+72],  V_old_l, centroid_l2[n+72],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v); 
   }

 }

}


/*======================= SOUTH CARRIAGE ====================================================================*/

  
  if ( f4 == 0){   
  /************ A's *****************/
   /*Reads in min_parm from peak fits at nominal voltage */
   fp2in = fopen(name_ofInfile2, "r"); 
    /*Reads in min_parm from ln(adcl/adcr) fits */
   fp4in = fopen(name_ofInfile4, "r");

   /**************** B's ***********/

  /*Reads in min_parm from peak fits at nominal voltage */
   fp5in = fopen(name_ofInfile5, "r"); 
   /*Reads in min_parm from ln(adcl/adcr) fits */
   fp7in = fopen(name_ofInfile7, "r");


  blah = strcmp(mode_ptr,"group");  
   if (blah == 0){
   HB = "U";
   }
   else{
   HB = "N";
   }

  strcpy(name_ofOutfile1,"sc_south_new.dat");  
    
  /********* PAW STUFF *****************/        
   hini(outfile,HB); /* crappy PAW function */
   book_histos(name_ofOutfile1); /* booking the histograms */

   
   printf("\033[01;93m\n\nProcessing: %s\033[m\n",name_ofInfile1);
  /*Outputs New High Voltage File */   
  fp1out = fopen(name_ofOutfile1, "w");  
 
   /*==========  Create Data Arrays ==========*/
  
   for(k=1;k<289;k++){
   fscanf(fp2in,"%d%f%f%f%d\n",&dum1,&G_peak_l[k],&dum2,&dum3,&dum4);  
   fscanf(fp4in,"%d%f%f%f%f%f%f%f%f\n",&dum5,&centroid_l[k],&dum6,&dum7,&dum8,&dum9,&dum10,&dum11,&dum12);
   
   fscanf(fp5in,"%d%f%f%f%d\n",&dum1,&G_peak_l2[k],&dum2,&dum3,&dum4);  
   fscanf(fp7in,"%d%f%f%f%f%f%f%f%f\n",&dum5,&centroid_l2[k],&dum6,&dum7,&dum8,&dum9,&dum10,&dum11,&dum12);
   }
  
/*===== Loop over High Voltage Files and Create updated file with new voltages =====*/

   for(z=1;z<133; z++){
   fscanf(fp1in,"%s%d%d%d%d%d%d%d%f%f%f%f%f%f%f",scint_id,&a,&b,&c,&d,&e,&f,&g,&V_old_l,&q,&r,&s,&t,&u,&v);

   n = ((z+1)/2);
   
   
/**************** SECTOR 6 **********************************************************/   
   if (z<17){
 V_new_l = hv_new_l(G_peak_l[z+263],  V_old_l, centroid_l[z+263],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);       
   }
   if (z!=2*n && z>16 && z<23){
   /*********** a's ********/
 V_new_l = hv_new_l(G_peak_l[n+271],  V_old_l, centroid_l[n+271],scint_id,fp3out,name_ofOutfile1,z);
 //printf("PEAK:%f\tN+271:%d\tn:%d\n",G_peak_l[n+271],n+271,n);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v); 
   }
   if (z==2*n && z>16 && z<23){
   /************ b's *******/
 V_new_l = hv_new_l(G_peak_l2[n+271],  V_old_l, centroid_l2[n+271],scint_id,fp3out,name_ofOutfile1,z);
 //printf("PEAK:%f\tN+271:%d\tn:%d\n",G_peak_l2[n+271],n+271,n);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v); 
   }
       
   if (z>22 && z<39){
 V_new_r = hv_new_r(G_peak_l[z+241],  V_old_l, centroid_l[z+241],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v);     
   }   
   if (z!=2*n && z>38 && z<45){
   /*********** a's ********/
 V_new_r = hv_new_r(G_peak_l[n+260],  V_old_l, centroid_l[n+260],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v); 
   }
   if (z==2*n && z>38 && z<45){
   /************ b's *******/
 V_new_r = hv_new_r(G_peak_l2[n+260],  V_old_l, centroid_l2[n+260],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v); 
   }

/**************** SECTOR 1 ***********************************************************/
   if (z>44 && z<61){
 V_new_l = hv_new_l(G_peak_l[z-21],  V_old_l, centroid_l[z-21],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);     
   }
   if (z!=2*n && z>60 && z<67){
   /*********** a's ********/
 V_new_l = hv_new_l(G_peak_l[n+9],  V_old_l, centroid_l[n+9],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v); 
   }
   if (z==2*n && z>60 && z<67){
   /************ b's *******/
 V_new_l = hv_new_l(G_peak_l2[n+9],  V_old_l, centroid_l2[n+9],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v); 
   }

   if (z>66 && z<83){
 V_new_r = hv_new_r(G_peak_l[z-43],  V_old_l, centroid_l[z-43],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v);     
   }   
   if (z!=2*n && z>82 && z<89){
   /*********** a's ********/
 V_new_r = hv_new_r(G_peak_l[n-2],  V_old_l, centroid_l[n-2],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v); 
   }
   if (z==2*n && z>82 && z<89){
   /************ b's *******/
 V_new_r = hv_new_r(G_peak_l2[n-2],  V_old_l, centroid_l2[n-2],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v); 
   }

   
/******************** SECTOR 2 ********************************************************/
   if (z>88 && z<105){
 V_new_l = hv_new_l(G_peak_l[z-17],  V_old_l, centroid_l[z-17],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v);     
   }   
   if (z!=2*n && z>104 && z<111){
   /*********** a's ********/
 V_new_l = hv_new_l(G_peak_l[n+35],  V_old_l, centroid_l[n+35],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v); 
   }
   if (z==2*n && z>104 && z<111){
   /************ b's *******/
 V_new_l = hv_new_l(G_peak_l2[n+35],  V_old_l, centroid_l2[n+35],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_l,q,r,s,t,u,v); 
   }
   
   if (z>110 && z<127){
 V_new_r = hv_new_r(G_peak_l[z-39],  V_old_l, centroid_l[z-39],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v);     
   }   
   if (z!=2*n && z>126 && z<133){
   /*********** a's ********/
 V_new_r = hv_new_r(G_peak_l[n+24],  V_old_l, centroid_l[n+24],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v); 
   }
   if (z==2*n && z>126 && z<133){
   /************ b's *******/
 V_new_r = hv_new_r(G_peak_l2[n+24],  V_old_l, centroid_l2[n+24],scint_id,fp3out,name_ofOutfile1,z);
 fprintf(fp1out,"%s\t\t%2d %d %2d %2d %2d %2d %2d  %7.1f %2.1f %2.1f %5.1f %5.1f %2.1f %2.1f\n",scint_id,a,b,c,d,e,f,g,V_new_r,q,r,s,t,u,v); 
   }

 }

}
        
/*==============================================================================================================*/


fclose(fp1in);

 if (f1==0){
fclose(scf1);
fclose(scf2);
fclose(scf3);
fclose(scf4);
fclose(scf5);
fclose(scf6);
 }



 if (f1==0 && f2==0 && f3==0 && f4==0)
{
fclose(fp2in);
fclose(fp4in);
}

 if (f2==0 && f3==0 && f4==0)
{
fclose(fp5in);
fclose(fp7in);
}

fclose(fp1out);
fclose(fp3out);


/***********  MORE PAW STUFF  **********************/                        
  
  hrout(0,icycle," ");
  hldir_(" ", " ", 1L, 1L); /* don't remove this line */
  hrend_("esr", 3L);
}

/************************************ END OF MAIN ***********************************/
 


/*=== Functions ==*/


/*========= Print out usage help ========== */

void PrintUsage(char *processName)
{
  fprintf(stderr,"  \033[01;34m \n");
  fprintf(stderr,"Usage: %s [-h] [-f] [-g] [-j] [-k] [-m] [-M]\n\n",processName);
  fprintf(stderr,"\tOptions:\n");
  fprintf(stderr,"\t\t-f<filename.dat> Old High Voltage File\t \n\n");
  fprintf(stderr,"\t\t-g<filename> Parm File from GMEAN fits                 A's on (or Forward Carriage)\t \n\n");
  fprintf(stderr,"\t\t-j<filename> Parm File from log(ADCL/ADCR) fits        A's on (or Forward Carriage)\t \n\n");
  fprintf(stderr,"\t\t-k<filename> Parm File from GMEAN fits                 B's on\t \n\n"); 
  fprintf(stderr,"\t\t-m<filename> Parm File from log(ADCL/ADCR) fits        B's on\t \n\n");  
  fprintf(stderr,"\t\t-M Runs hv.exe in \"all carriages\" mode (-Mgroup) or \"one at a time\" mode (-Msingle)\t \n\n");
  fprintf(stderr,"\t\t-h Get help\n\033[m\n\n");
  exit(0);
}


/*====== Left PMTS===============*/

float hv_new_l(float G_peak, float V_old, float centroid, char *ID, FILE *fp3out, char *fname, int count)
{

float V_new;
double G;
double alpha;
double delta;
double del_V;
double del_G;
int g1,g2,g3,g4;
FILE *parms;


    parms  = fopen("sc_pmt_parms.dat", "a"); 
   
    g1 = strcmp(fname,"scf_new.dat");
    g2 = strcmp(fname,"sc_space_new.dat");
    g3 = strcmp(fname,"sc_north_new.dat"); 
    g4 = strcmp(fname,"sc_south_new.dat");
    
    V_old = fabs(V_old);
  
    if (g1==0){
    G = G_peak/exp(centroid/300);    
    }

    if (g2==0){
    G = G_peak/exp(centroid/1.8615);          
    }
    if (g3==0 || g4==0){
    G = G_peak/exp(centroid/2.04678);   
    }
         
 
    alpha = 7.2000; /* (alpha = .6 x 12)*/ 

    del_G    = 600.00 - G; 
    del_V    = (V_old*del_G)/(G*alpha);    
    V_new    = ( V_old + del_V);

      
    if (G_peak==600.00 || G_peak>1000.00 || G_peak<200.0 ){
     //printf("\033[01;97mPMT %s Peak Is Fishy!!!\033[m\n",ID);
    fprintf(fp3out,"PMT %s Peak Is Fishy!!!\n",ID);
    }   

    if (V_new > 2500.00){
    //printf("\033[01;31mPMT %s requires a voltage higher than 2400V!!!\033[m\n",ID);
    fprintf(fp3out,"PMT %s requires a voltage higher than 2400V!!!\n",ID);
    }
    
    delta = V_new - V_old;
    if (delta> 150.0 || delta< -150.0){
    //printf("\033[01;34mPMT %s requested a Voltage change of %4.2f\033[m\n",ID,delta);
    fprintf(fp3out,"PMT %s requested a Voltage change of %4.2f\n",ID,delta);
    
    if (delta > 150.0){
    V_new = V_old +150;
    //printf("\033[01;34mPMT %s will only be changed by 150 Volts\033[m\n",ID);
    fprintf(fp3out,"PMT %s will only be changed by 150 Volts\n",ID);
    }
    
    if (delta < -150.0){
    V_new = V_old - 150.0;
    //printf("\033[01;34mPMT %s will only be changed by -150 Volts\033[m\n",ID);
    fprintf(fp3out,"PMT %s will only be changed by -150 Volts\n",ID);
    }
   }


    /*  Don't touch a good thing =) */ 
    if ( (G >525.0) && (G <650.0) ){  
    V_new = V_old;  
    }
   
    /* Last Check */
    if ( V_new > 2500.00){
    V_new = 2500.00;
    }

    /* Get the stragglers */
    // if ( (centroid > .95 || centroid < -.95)){
    //if(V_new == 2500.00){
	
    //}
    //}



     if ( g1  == 0 ){
    hf2(1,count,delta,1.);
    hf1(20,G_peak,1.);    
     }     
   
     if ( g2 == 0 ){
    hf2(2,count,delta,1.);   
    hf1(21,G_peak,1.);   
    }
    
     if ( g3  == 0 ){
    hf2(3,count,delta,1.);    
    hf1(22,G_peak,1.);   
    }
    
     if ( g4 == 0){
    hf2(4,count,delta,1.);     
    hf1(23,G_peak,1.);   
    }
    
    V_new = -fabs(V_new);
    V_old =  -fabs(V_old);
   
    
  fprintf(parms, "%s\tPeak:%4.2f\tG_l:%4.4f\tC:%4.4f\tV_old:%.2f\tV_new:%.2f\n",ID,G_peak,G,centroid,V_old,V_new);
  fclose(parms);
 
  
    return V_new;

}

/*================ Right PMT's ============================*/

float hv_new_r(float G_peak, float V_old, float centroid, char *ID, FILE *fp3out, char *fname, int count)
{

float V_new;
double G;
double alpha;
double delta;    
double del_G;
double del_V;
int g1,g2,g3,g4;
FILE *parms;

    parms = fopen("sc_pmt_parms.dat", "a"); 

    g1 = strcmp(fname,"scf_new.dat");
    g2 = strcmp(fname,"sc_space_new.dat");
    g3 = strcmp(fname,"sc_north_new.dat"); 
    g4 = strcmp(fname,"sc_south_new.dat");

    V_old = fabs(V_old);

    if (g1==0){
    G = exp(centroid/300)*G_peak;   
    }

    if (g2==0){
    G = exp(centroid/1.8615)*G_peak;    
    }
    
    if (g3==0 || g4==0){
    G = exp(centroid/2.04678)*G_peak;   
    }
        
   
    alpha = 7.20000;  /* (alpha = .6 x 12) */ 

    del_G   = 600.00 - G; 
    del_V   = (V_old*del_G)/(G*alpha);    
    V_new   = (V_old + del_V);
   
    if (G_peak==600.00 || G_peak>1000.00 || G_peak<200.0 ){
    //printf("\033[01;97mPMT %s Peak Is Fishy!!!\033[m\n",ID);
    fprintf(fp3out,"PMT %s Peak Is Fishy!!!\n",ID);
    }   
    
    if (V_new > 2500.00){
    //printf("\033[01;31mPMT %s requires a voltage higher than 2400V!!!\033[m\n",ID);
    fprintf(fp3out,"PMT %s requires a voltage higher than 2400V!!!\n",ID);
    }
 
    delta = V_new - V_old;
    if (delta> 150.0 || delta < -150.0){
    //printf("\033[01;34mPMT %s requested a Voltage change of %4.2f\033[m\n",ID,delta);
    fprintf(fp3out,"PMT %s requested a Voltage change of %4.2f\n",ID,delta);

    if (delta > 150.0){
    V_new = V_old +150;
    //printf("\033[01;34mPMT %s will only be changed by 150 Volts\033[m\n",ID);
    fprintf(fp3out,"PMT %s will only be changed by 150 Volts\n",ID);
    }
    
    if (delta < -150.0){
    V_new = V_old - 150.0;
    //printf("\033[01;34mPMT %s will only be changed by -150 Volts\033[m\n",ID);
    fprintf(fp3out,"PMT %s will only be changed by -150 Volts\n",ID);
    }
   }
     

     /* Don't touch a good thing =)  */
    if ( (G > 525.0) && (G < 650.0)){
    V_new = V_old;  
    }

    /* Last Check */
   if ( V_new > 2500.00){
    V_new = 2500.00;
    }

    /* Get the stragglers */
   //    if ( (centroid > .95 || centroid < -.95)){
   // if(V_new == 2500.00){
   
     
   // }
   //}


     if (g1  == 0 ){
    hf2(1,count,delta,1.);
    hf1(20,G_peak,1.);
    }     
   
     if ( g2 == 0 ){
    hf2(2,count,delta,1.);
    hf1(21,G_peak,1.);
    }
    
     if (g3  == 0 ){
    hf2(3,count,delta,1.);
    hf1(22,G_peak,1.);
    }

     if ( g4 == 0){
    hf2(4,count,delta,1.);
    hf1(23,G_peak,1.);
    }
       
    V_new = -fabs(V_new);
    V_old =  -fabs(V_old);

 fprintf(parms,"%s\tPeak:%4.2f\tG_r:%4.4f\tC:%4.4f\tV_old:%.2f\tV_new:%.2f\n",ID,G_peak,G,centroid,V_old,V_new);    
 fclose(parms);

    return V_new;

}


/************** Creation of the H-Book File(s) *************/

void book_histos(char *fname)
{
  char title[100];
  int i,j,k,l,m;
  int g1,g2,g3,g4;    

    
    g1 = strcmp(fname,"scf_new.dat");
    g2 = strcmp(fname,"sc_space_new.dat");
    g3 = strcmp(fname,"sc_north_new.dat");
    g4 = strcmp(fname,"sc_south_new.dat");
 

    if (g1  == 0 ){
    sprintf(title, "Forward Carriage");
    hbook2(1, title, 100, 0.0, 300.0, 100, -300.0, 300.0, 0); 
    sprintf(title, "Old Forward Carriage Peaks");
    hbook1(20, title, 100, 0.0, 1000,0.0);    
    }
   
    if (g2  == 0 ){
    sprintf(title, "Space Frame");
    hbook2(2, title, 100, 0.0, 170.0, 100, -300.0, 300.0, 0);
    sprintf(title, "Old Space Frame Peaks");
    hbook1(21, title, 100, 0.0, 1000,0.0);    
    }
        
    if (g3  == 0 ){
    sprintf(title, "North Carriage");
    hbook2(3, title, 100, 0.0, 170.0, 100, -300.0, 300.0, 0);
    sprintf(title, "Old North Carriage Peaks");
    hbook1(22, title, 100, 0.0, 1000,0.0);     
    }
        
    if (g4  == 0 ){
    sprintf(title, "South Carriage");
    hbook2(4, title, 100, 0.0, 170.0, 100, -300.0, 300.0, 0);   
    sprintf(title, "Old South Carriage Peaks");
    hbook1(23, title, 100, 0.0, 1000,0.0);    
    }
}

/****** MORE PAW STUFF ***********/

void hini(char *out, char *hb)
{
  int lun = LUN, lrec = LREC, memh = MEMH;
  int istat, icycle;
  char *def_out = "delta_v_pmts.hbook";
  

  if (out == NULL) out = &def_out[0];
  quest_[9] = 65000;
  hlimit_(&memh);
  hropen_(&lun, "esr", out , hb , &lrec, &istat, 3L, strlen(out), 1L);
  
  return;
}
