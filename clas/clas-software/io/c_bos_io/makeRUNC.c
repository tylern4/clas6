#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <math.h>
#include <bostypes.h>
#include <kinematics.h>
#include <utility.h>
#include <map_manager.h>

float beam_energy = 0;
int beam_type = -1;

/* in MEV!!!!!*/
int set_beam_energy_(float *MeV){ 
  set_beam_energy(*MeV);
}

int set_beam_energy(float MeV){
  beam_energy = MeV;
}

int set_beam_type_(int *type) { 
  set_beam_type(*type); 
}

int set_beam_type(int type){
  beam_type = type;
  fprintf(stderr, "<<<<<<<<<<< set beam type: %d >>>>>>>>>>>>", beam_type);
}

int get_beam_type() 
{
  return(beam_type);
}


/* Q: why is this function useful */
/* A: if the RUNC bank reads the map for you, you can
   control your software better when you process MC data
   also the RUNC bank is really compicated to understand in
   fortran, better to talk to it through C */
int get_beam_type_(int *btype){
  clasRUNC_t *RUNC = getBank(&wcs_, "RUNC");
  if (RUNC) { 
    *btype = RUNC->runc.beam.type.val.i[0];
    return (1);
  }
  return (-1);
}
int put_beam_type_(int *btype){
  clasRUNC_t *RUNC = getBank(&wcs_, "RUNC");
  if (RUNC) { 
    RUNC->runc.beam.type.val.i[0] = *btype;
    return (1);
  }
  return (-1);
}





clasRUNC_t * make_RUNC_bank(int runno){
  clasRUNC_t *RUNC;
  char *dir, def_map[100];
  int firsttime;
  map_obj_t target_mass;
  
  dir = getenv("CLAS_PARMS");
  sprintf(def_map,"%s/Maps/RUN_CONTROL.map",dir);
  if(RUNC = makeBank(&wcs_, "RUNC", 0, sizeof(runc_t)/4, 1)){
    float beam;
    /* make the bank!*/

    RUNC->runc.runno=runno;
    if (beam_energy){
      RUNC->runc.beam.energy.val.f[0] = beam_energy;
    } else {
      RUNC->runc.beam.energy = get_map_obj(def_map, "beam","energy", runno);
    }
    RUNC->runc.beam.energy.val.f[0] = RUNC->runc.beam.energy.val.f[0] / 1000.0; /* convert to GeV */
    
    if (beam_type != -1){
      RUNC->runc.beam.type.val.i[0] = beam_type;
    } else {
      RUNC->runc.beam.type = get_map_obj(def_map, "beam","type", runno);
    }

    RUNC->runc.q_all = get_map_obj(def_map, "Faraday_cup", "Q_all", runno);
    RUNC->runc.q_live = get_map_obj(def_map, "Faraday_cup", "Q_live", runno);
    RUNC->runc.torus = get_map_obj(def_map, "currents", "torus", runno);
    RUNC->runc.minitorus = get_map_obj(def_map, "currents", "minitorus", runno);
    RUNC->runc.tagger = get_map_obj(def_map, "currents", "tagger", runno);
    RUNC->runc.target_type = get_map_obj(def_map, "target", "type", runno);

    /* useful 4-vectors*/
    target_mass = get_map_obj(def_map, "target", "mass", runno);
    RUNC->runc.target.t = target_mass.val.f[0];
    RUNC->runc.target.space.x = RUNC->runc.target.space.y = RUNC->runc.target.space.z = 0.0;
    
    RUNC->runc.beam.p.t = sqrt(0.000511*0.000511 + ((RUNC->runc.beam.energy.val.f[0]) * (RUNC->runc.beam.energy.val.f[0])));

    RUNC->runc.beam.p.space.z = RUNC->runc.beam.energy.val.f[0];
    RUNC->runc.beam.p.space.x = RUNC->runc.beam.p.space.y = 0.0;
    RUNC->runc.init = v4add( RUNC->runc.target, RUNC->runc.beam.p);
    return RUNC;
  }
  return NULL;
}

/* only works for objects of length one at the moment - some day will
   capable of handling variable length arrays and may be reborn as a C++ object */
map_obj_t get_map_obj(char *map, char *subsystem, char *item, int runno){
  map_obj_t map_obj;
  int olength;
 
  map_obj.first =map_obj.len = map_obj.type = 0;

  map_stat_item(map, subsystem, item, &(map_obj.len), &(map_obj.type));
  /* do a malloc here and set the length - future upgrade*/
  if (map_obj.len ==1){
    switch (map_obj.type){
    case 0:
      map_get_int(map, subsystem, item, map_obj.len, &(map_obj.val.i[0]), runno, &(map_obj.first));
      break;
    case 1:
      map_get_float(map, subsystem, item, map_obj.len, &(map_obj.val.f[0]), runno, &(map_obj.first));
      break;
    case 2:
      map_get_char(map, subsystem, item, map_obj.len, &(map_obj.val.c[0]), runno, &(map_obj.first), &olength);
      break;
    default:
      fprintf(stderr, "get_map_obj: unknown type, %d\n", map_obj.type);
      break;
    }
  } 
  return map_obj;
}

int make_runc_bank_(int *runno){
  make_RUNC_bank(*runno);
}
