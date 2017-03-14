#include <ntypes.h>
#include <bostypes.h>
#include <make_TGEO.h>
#include <trk_run_control.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int InitTarget(int RunNo)
{
 static int CurrentRun = -1;
 static int Initialise = 0 ;

 clasTGEO_t *TGEO;

 if (RunNo != CurrentRun) 
 { 
  CurrentRun = RunNo;
  Initialise = 1;
  if(TGEO = getBank(&wcs_, "TGEO")) dropBank(&wcs_,"TGEO",0);
  make_TGEO(RunNo);
  TGEO = getBank(&wcs_, "TGEO");
  fprintf(stderr,"Target position initialized to: x=%f y=%f z=%f \n", TGEO->tgeo[0].x,TGEO->tgeo[0].y,TGEO->tgeo[0].z);
 }
 else 
 {
  if ( Initialise == 0) 
  {
   Initialise = 1;
   if(TGEO = getBank(&wcs_, "TGEO")) dropBank(&wcs_,"TGEO",0);
   make_TGEO(RunNo);
   TGEO = getBank(&wcs_, "TGEO");
   fprintf(stderr,"Target position initialized to: x=%f y=%f z=%f \n", TGEO->tgeo[0].x,TGEO->tgeo[0].y,TGEO->tgeo[0].z);   
  }
 }
}


clasTGEO_t *make_TGEO(int RunNo)
{
 char *dir,map[100];  
 int firsttime;
 float T[3];
 clasTGEO_t *TGEO;

 // Added Matt Bellis 01/29/07
 // previously trktcl was not intialized so the
 // TGEO bank never got set with proper values.
 //   However, this sets the spar_prlink_name back to 'junk'
 //   so it is commented out now. JGoetz 2/7/8
 //trk_set_def_();

 /* Get target position from geometry bank */
 dir = getenv("CLAS_PARMS");
 sprintf(map,"%s/Maps/GEOMETRY.map",dir);
 map_get_float(map,"target","position",3,&T,RunNo,&firsttime);
 TGEO = makeBank(&wcs_, "TGEO", 0, 6, 1);
 if (trktcl_.dpar_TargetPos[2]==-9999.) 
   {
     TGEO->tgeo[0].x = T[0];
     TGEO->tgeo[0].y = T[1];
     TGEO->tgeo[0].z = T[2];
   }
 else
   {
     /* if target position is defined from a1c commans line or recsis tcl variable then overwrite map values */
     TGEO->tgeo[0].x = trktcl_.dpar_TargetPos[0];
     TGEO->tgeo[0].y = trktcl_.dpar_TargetPos[1];
     TGEO->tgeo[0].z = trktcl_.dpar_TargetPos[2];
   }
 return(TGEO);
}



int inittarget_(int *RunNo){
  InitTarget(*RunNo);
}




