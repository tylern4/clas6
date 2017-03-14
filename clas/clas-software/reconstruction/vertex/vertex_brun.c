/* vertex_brun_ */
/* Author: John McNabb */
/* Purpose: set up values important for vertexing (ie beamline info)*/
/* Inputs:  */
/* Outputs: */

#include <stdlib.h>
#include <ntypes.h>
#include <bostypes.h>
#include <stdio.h>
#include <vertex.h>
#include <math.h>
#include <string.h>

vector3_t beam_position={0.,0.,0.};

void vertex_brun_(int *runno){
  vertex_brun(*runno);
}

void vertex_brun(int runno){
char map[100];
char *dir;
int firsttime;

  dir = getenv("CLAS_PARMS"); 
  sprintf(map,"%s/Maps/GEOMETRY.map",dir); 
  map_get_float(map,"beam","position",3,&beam_position,runno,&firsttime);
  fprintf(stderr,"using beam position: %g %g %g from run: %d\n",beam_position.x,beam_position.y,beam_position.z,firsttime);
}



