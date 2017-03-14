#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <trk_run_control.h>

int set_currents(float torus, float minitorus){
  trktcl_.ipar_torus_current = torus;
  trktcl_.ipar_minitorus_current = minitorus;
}

typedef struct {
  float torus;
  float mini;
  char *prlink;
} prlink_t;

static prlink_t assoc[20] = { 
	0,0,"prlink_00_00.bos",
	0,6000,"prlink_00_75.bos",
	965,6000,"prlink_25_75.bos",
	1930,6000,"prlink_50_75.bos",
	-1930,-6000,"prlink_50_75_inv.bos",
	2895,6000,"prlink_75_75.bos",
       	3475,6000,"prlink_90_75.bos",
	965,0,"prlink_25_00.bos",
	1930,0,"prlink_50_00.bos",
	2895,0,"prlink_75_00.bos",
	3375,0,"prlink_88_00.bos",
	-1500,121,"lnk_n1500_pt121.bos",
	1500,121,"lnk_p1500_pt121.bos",
	1930,121,"lnk_p1930_pt121.bos",
	-1930,121,"lnk_n1930_pt121.bos",
	1930,-121,"lnk_p1930_ptn121.bos",
	2250,121,"lnk_p2250_pt121.bos",
	-2250,121,"lnk_n2250_pt121.bos",
	-2250,6000,"lnk_m2250_6000.bos",
	};

int trk_run_control_(){ 
  /* take care of the prlink file name and torus field sign*/
  float torus = (float)trktcl_.ipar_torus_current;
  float minitorus = (float)trktcl_.ipar_minitorus_current; 
  int i;
  char ctest[4];

  /* a cludge for photon running should really straighten out
     sda_minit.f and not use the curre nts as switches */
  if (minitorus < 0.0){ 
    minitorus = 0.0;
    trktcl_.ipar_minitorus_current=0;
  }

  if (trktcl_.ipar_torus_current > 0) {
    trktcl_.ipar_Bsign_torus = 1.0;
  } else {
    trktcl_.ipar_Bsign_torus = -1.0;
  }
  strncpy(ctest,trktcl_.spar_prlink_name,4);
  if( !strncmp(ctest,"junk",4) ){
    for(i=0; i < 128; i++){
      trktcl_.spar_prlink_name[i] = ' ';
    }

    strncpy(trktcl_.spar_prlink_name,prlinkFile(torus, minitorus), strlen(prlinkFile(torus, minitorus)));
  }
  if (fabs(torus) < 10.) {
    trktcl_.ipar_trk_magtyp = 5;
  }
}

float get_map_current(char name[], int runno){
  int i, firsttime = 0, olength;
  float current[1];
  char *dir;
  char def_map[100], link_file[256];
  /*  int read_et = run_control_.read_et; */

  dir = getenv("CLAS_PARMS");
  sprintf(def_map,"%s/Maps/RUN_CONTROL.map",dir);

  map_get_float(def_map,"currents", name, 1 , current, runno, &firsttime);
  /*  if (!read_et && firsttime != runno) {
    fprintf(stderr, "FATAL ERROR, no %s current available in map database", name);
    exit(0);
    } */
  if(firsttime != runno)
    fprintf(stderr, "WARNING, no %s current available in map database, take value=%f for run %d instead\n", name,current[0],firsttime);

  return(current[0]);
}

float get_map_position(char name[], int runno, float *position){
  int i, firsttime = 0, olength;
  float pos[3];
  char *dir;
  char def_map[100];
  /*  int read_et = run_control_.read_et; */

  dir = getenv("CLAS_PARMS");
  sprintf(def_map,"%s/Maps/GEOMETRY.map",dir);

  map_get_float(def_map,name,"position", 3 , pos, runno, &firsttime);

  for(i=0;i<3;++i){
   *(position+i)=pos[i];
  }
  if(firsttime != runno)
    fprintf(stderr, "WARNING, no %s position available in the database, take value=%f%f%f for run %d instead\n", name,pos[0],pos[1],pos[2],firsttime);

  return(0);
}

char *prlinkFile(float torusCurrent,float miniCurrent)
{

  int i;

  float maxTorus = 3860.0;
  float maxMini = 8000.0;
  float match;
  float matchmin;
  int imatch;
  int nmatch;
  nmatch=0;

  matchmin=maxTorus+maxMini;

  /* roadmaps for EG1 polarized target*/
  if (trktcl_.ipar_poltarget_current){
    if(torusCurrent < -1400 && torusCurrent > -1600 && (trktcl_.ipar_poltarget_current == 121 || trktcl_.ipar_poltarget_current >= 121000))
      return(assoc[11].prlink);
    if(torusCurrent > 1400 && torusCurrent < 1600 && (trktcl_.ipar_poltarget_current == 121 || trktcl_.ipar_poltarget_current >= 121000))
      return(assoc[12].prlink);
    if(torusCurrent > 1830 && torusCurrent < 2030 && (trktcl_.ipar_poltarget_current == 121 || trktcl_.ipar_poltarget_current >= 121000))
      return(assoc[13].prlink);
    if(torusCurrent < -1830 && torusCurrent > -2030 &&(trktcl_.ipar_poltarget_current == 121 || trktcl_.ipar_poltarget_current >= 121000))
      return(assoc[14].prlink);
    if(torusCurrent > 1830 &&torusCurrent < 2030 && (trktcl_.ipar_poltarget_current == -121 || trktcl_.ipar_poltarget_current <= -12000))
      return(assoc[15].prlink);
    if( torusCurrent > 2150 &&  torusCurrent < 2350 &&  (trktcl_.ipar_poltarget_current == 121 || trktcl_.ipar_poltarget_current >= 121000))
      return(assoc[16].prlink);
    if(torusCurrent < -2150 &&torusCurrent > -2350 && (trktcl_.ipar_poltarget_current == 121 || trktcl_.ipar_poltarget_current >= 121000))
      return(assoc[17].prlink);
    /* default roadmaps*/
    if(torusCurrent > 0)
      return(assoc[13].prlink);
    if(torusCurrent < 0)
      return(assoc[11].prlink);
    
  }
  else {
    if (torusCurrent < 0.0 &&  torusCurrent > -9999. ) {
      for (i=18;i<19;i++) {
	if ((fabs(torusCurrent-assoc[i].torus)<(0.125*maxTorus))&&
	    (fabs(miniCurrent-assoc[i].mini)<(0.125*maxMini))) {
	  nmatch++;
	  match=fabs(abs(torusCurrent)-assoc[i].torus)+fabs(miniCurrent-assoc[i].mini);
	  if (match<matchmin) {
	    matchmin=match;
	    imatch=i;
	  }
	}
      }
    }
    else{
      for (i=0;i<11;i++) {
	if ((fabs(torusCurrent-assoc[i].torus)<(0.125*maxTorus))&&
	    (fabs(miniCurrent-assoc[i].mini)<(0.125*maxMini))) {
	  nmatch++;
	  match=fabs(torusCurrent-assoc[i].torus)+fabs(miniCurrent-assoc[i].mini);
	  if (match<matchmin) {
	    matchmin=match;
	    imatch=i;
	  }
	}
      }
    }
  }
  if (nmatch==0) return("unknown");
  else return(assoc[imatch].prlink);
}



