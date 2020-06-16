// C++ part bank utilities - Joe Manak 9/22/98

extern "C" {
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <clas_cern.h>
#include <particleType.h>
#include <kinematics.h>
#include <printBOS.h>
#include <utility.h>
#include <pid.h>
}

//#include <iostream.h>
#include <math.h>
#include <kinematics_o.h>
#include <clasPART_o.h>

part_o nothing;

int clasPID_o::num(int particletype){
  if (particletype < NUM_PIDS){
    return(Num[particletype]);
  } else{
    return(0);
  }
}

int clasPID_o::nchar(){
  return(ncharged);
}

int clasPID_o::nneut(){
  return(nneutral);
}

part_o &clasPID_o::part(int particletype,int partno){
  if ( (particletype < NUM_PIDS) && (partno < Num[particletype])){
    return ( PartPtr[particletype][partno] );
  } else {
    return nothing;
  }
}

void clasPID_o::print(FILE *fp){
  for(int i=0; i < NUM_PIDS; i++){
    if (Num[i]){
      for(int j=0; j < Num[i]; j++) part(i,j).print(fp);
    }
  }
}

vector4_o clasPID_o::beam(){
  return event_beam;
}

vector4_o clasPID_o::target(){
  return event_target;
}

vector3_o clasPID_o::vert(){
  return event_vertex;
}

int clasPID_o::build_from_BOS(int pidno){
  int i, ret = 0;
  clasPART_t *PART = (clasPART_t *)getGroup(&bcs_, "PART", pidno);
  clasBID_t *TBID = (clasBID_t *)getGroup(&bcs_, "TBID", pidno);
  clasRUNC_t *RUNC = (clasRUNC_t *)getBank(&wcs_, "RUNC");
  clasTAGR_t *TAGR = (clasTAGR_t *)getBank(&bcs_, "TAGR");
  clasMVRT_t *MVRT = (clasMVRT_t *)getBank(&bcs_, "MVRT");

  if (RUNC){
    switch(RUNC->runc.beam.type.val.i[0]){
    case 0: // electron beam
      event_beam.init(RUNC->runc.beam.p);
    case 1: // photon beam
      if (TAGR) event_beam.init(get_photon_v4(TAGR, TBID));
    }
    event_target.init(RUNC->runc.target);
  }
  
  if (MVRT) event_vertex.init(MVRT->mvrt[0].vert);

  if (PART){
    for(i=0; i < PART->bank.nrow; i++){
      if (PART->part[i].pid < NUM_PIDS && PART->part[i].flags != BEAM_FLAG){
	Num[PART->part[i].pid]++;
      }
    }

    // allocate memory for array of part_o
    for(i = 0; i < NUM_PIDS; i++){
      if (Num[i]){
	PartPtr[i] = new part_o[Num[i]];
      }
    }
    // fill up our new array
    for(i=0; i < PART->bank.nrow; i++){
      if (PART->part[i].flags !=BEAM_FLAG){
	int counter[NUM_PIDS];
	(PART->part[i].q > 0.1 || PART->part[i].q < 0.1) ? ncharged++ : nneutral++;
	for (int j = 0; j < NUM_PIDS; j++) counter[j] = 0;
	if ((PART->part[i].pid < NUM_PIDS) && (counter[PART->part[i].pid] < Num[PART->part[i].pid])){
	  // Yikes! what have I done!
	  (PartPtr[PART->part[i].pid])[counter[PART->part[i].pid]].fill_from_BOS(PART->part[i], pidno);
	  counter[PART->part[i].pid]++;
	}
      } else {
	event_beam.init(PART->part[i].p);
      }
    }
    ret = 1;
  }
  return ret;
}

void clasPID_o::set_default(){
  ncharged = 0;
  nneutral = 0;
  for (int i=0; i < NUM_PIDS; i++) {
    Num[i] = 0;
    PartPtr[i] = 0;
  }
}

// default empty constuctor 
clasPID_o::clasPID_o(){
  set_default();
}

// build from existing bos banks
clasPID_o::clasPID_o(int pidno){
  set_default();
  build_from_BOS(pidno);
}


clasPID_o::~clasPID_o(){
  int i;
  for(i=0; i < 40; i++){
    if (Num[i]){
      delete[] PartPtr[i];
    }
  }
}

int clasPID_o::good_nums(int particletype, int number){
  int ret = 0;
  if ((particletype < NUM_PIDS) && (Num[particletype] > number)) ret = 1;
  return ret;
}

/*--------------- part object stuff ----------------*/

part_o::part_o(){
  // set everything to zero
  memset(&part, 0, sizeof(part_t));
  memset(&tbid, 0, sizeof(tbid_t));
  nchild = 0;
  child = 0;
}

part_t part_o::part_c(){
  return part;
}

int part_o::fill_from_BOS(part_t fill_part, int partno){
  clasTBID_t *TBID= (clasTBID_t *)getGroup(&bcs_, "TBID", partno);
  int ret=0;
  part = fill_part;
  vert.init(part.vert);
  p.init(part.p);
  if (TBID){
    tbid = TBID->tbid[part.trkid -1];
    ret = 1;
  }
  return ret;
}


void part_o::print(FILE *fp){
  pPART(fp, &part);
  pTBID(fp, &tbid);
}

