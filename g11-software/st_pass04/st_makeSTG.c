#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <math.h>
#include <bostypes.h>
#include <kinematics.h>
#include <map_manager.h>

#define PI 3.1415927

/* routine which defines geometry bank for old start counter */
int make_STG_banks(int *runno);
int make_stg_banks_(int *runno);

void StConfigGeom(int *runno);
void stconfiggeom_(int *runno);

int make_stg_banks_(int *runno){
  /*Fortran callable wrapper*/
  int ret;
  ret = make_STG_banks(runno);
  return (ret);
}



int make_STG_banks(int *runno){
  /*input: runno, CLAS_PARMS environtment variable, GEOMETRY.map
  output: STG bank in wcs
  */
  

  int ret = 0;
  int i;
  char *dir;
  char map[100];
  int firsttime;
  float legVolume[18], noseVolume[24];
  float dist2bl[6], offset[18];
  float thickness[6], aperture[6], noseAngle[6];

  clasSTG_t *STG = NULL;

  dir = getenv("CLAS_PARMS");
  sprintf(map, "%s/Maps/GEOMETRY.map", dir);

  fprintf(stderr,"Reading ST geometry information from map\n");

  map_get_float(map, "st", "legVolume", 18, legVolume, *runno, &firsttime);
  map_get_float(map, "st", "noseVolume", 24, noseVolume, *runno, &firsttime);
  map_get_float(map, "st", "dist2bl", 6, dist2bl, *runno, &firsttime);
  map_get_float(map, "st", "offset", 18, offset, *runno, &firsttime);
  map_get_float(map, "st", "thickness", 6, thickness, *runno, &firsttime);
  map_get_float(map, "st", "aperture", 6, aperture, *runno, &firsttime);
  map_get_float(map, "st", "noseAngle", 6, noseAngle, *runno, &firsttime);


  /*drop bank if it already exits*/
  if(STG=(clasSTG_t *)getBank(&wcs_, "STG ")){
    dropBank(&wcs_, "STG ", 1);
  }
     
  if(STG = (clasSTG_t *)makeBank(&wcs_, "STG ", 1, sizeof(stg_t)/4, 6)){
    fprintf(stderr,"Constructing STG bank\n");
    for(i = 0; i < 6; i++){
      /*6 rows corresponding to 6 sectors
	each sector is rotated by 60 degrees
	in Lab frame, (x,y)values are different
	play rotation here
       */
      STG->stg[i].id = i + 1;
      STG->stg[i].leg_x_max = dist2bl[i] - thickness[i]/2.0;
      STG->stg[i].leg_y_max = legVolume[3*i+1]/2.0 + aperture[i]/2.0;
      STG->stg[i].leg_y_min = -(legVolume[3*i+1]/2.0 + aperture[i]/2.0);
      STG->stg[i].leg_z_max =  legVolume[3*i+2]/2.0 + offset[3*i+2] ; /*legVolume[2] is the full length of the leg*/
      STG->stg[i].leg_z_min = -legVolume[3*i+2]/2.0 + offset[3*i+2] ;/*offset points to the center of the start counter leg*/
      
      STG->stg[i].nose_x_max = dist2bl[i] - thickness[i]/2.0;
      /*noseAngle is in DEGREE!!!*/
      STG->stg[i].nose_x_min = (dist2bl[i] - thickness[i]/2.0) - noseVolume[4*i+3]*sin(noseAngle[i]*PI/180.0);
      STG->stg[i].nose_z_max = offset[3*i+2] + legVolume[3*i+2]/2.0 + noseVolume[4*i+3]*cos(noseAngle[i]*PI/180.0);
      STG->stg[i].nose_z_min = legVolume[3*i+2]/2.0 + offset[3*i+2];

      STG->stg[i].noseAngle = noseAngle[i];
      STG->stg[i].lleg = legVolume[3*i+2];
      STG->stg[i].lnose = noseVolume[4*i+3];
       
      }
    ret = 1;
  }
  
  return (ret);
}

void stconfiggeom_(int *runno)
{
  StConfigGeom(runno);
}

void StConfigGeom(int *runno)
{
  static int current_run = -1;
  static int initialized = 0;

  if(*runno != current_run){
    fprintf(stderr, "Start Counter geometry inialization runtine called for run %d\n", *runno);
    current_run = *runno;
    initialized = 1;
    make_STG_banks(runno); 
  }else{
    if(!initialized){
      initialized = 1;
      make_STG_banks(runno); 
    }
  }
}
