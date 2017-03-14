/*--------------------------sc_geom.c----------------------*/
/* Routines for obtaining geometrical quantities for the scintillators.
   ROUTINES:
     make_SCG_banks:  opens the SCG fpack file and applies run-dependent 
	              rotations and translations of the carriages.  One SCG
		      BOS bank per sector is stored in the WCS array.
     make_scg_banks_:   Fortran wrapper for the routine above. 
     make_SCP_bank(s):  Creates the scintillator plane bank for sector s
		      using SCG.
     get_SCP_norm(i,j,SCG):   finds the normal vector to the scintillator  
	              plane determined by scintillators i and j
     get_SCP_distance(norm,i,SCG):  returns the closest distance between
		      the CLAS origin and the plane whose normal is "norm"
     get_bar_normal(SCP,SCG,i): gets the normal vector for the plane containing
                      scintillator i
     get_bar_center(SCG,i):  returns the center of scintillator i in the CLAS
                      coordinate system.
     get_bar_length(SCG,i):  returns the length of scintillator i
     get_bar_direction(SCG,i):  returns a unit vector pointing along the length
                      of scintillator i
     get_left_edge(SCG,i):  Gets the coordinates of the left edge of 
                      scintillator i
     get_right_edge(SCG,i): Gets the coordinates of the right edge of 
                      scintillator i
     fill_ang_rot(*ptr, ang[3]):  returns the rotation matrix corresponding to
                      the Euler angles ang[0] through ang[2].  ptr is a pointer
		      to the first entry in the matrix.
     mat_mult(rot. matrix,vec):  rotates the vector "vec" to a new orientation
                      by applying the rotation matrix to the vector.
*/
/*---------------------------------------------------------*/
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <math.h>
#include <bostypes.h>
#include <kinematics.h>
#include <scExtern.h>

/* -=======================================================- *
$Id: sc_geom.c,v 1.13 2008/03/16 04:33:22 fklein Exp $
$Author: fklein $
$Revision: 1.13 $
$Date: 2008/03/16 04:33:22 $
* -=======================================================- */

int make_scg_banks_(int runno){
  /* Stub for calling make_SCG_banks from Fortran */

  int ret;
  ret = make_SCG_banks(runno);
  return(ret);
}


int make_SCP_bank(int sector){
  /*input:sector, SCG bank
    output:SCP bank*/
  clasSCG_t *SCG=NULL;
  clasSCP_t *SCP=NULL;
  vector3_t norm;

  /*currently the code makes the assumption that the normal vector
    points away from the target - due to the structure of the SCP bank*/
  if((SCG =  getGroup(&wcs_, "SCG ", sector)) &&
     (SCP = makeBank(&wcs_, "SCP ", sector, 16, 1))){
    /*first plane*/
    norm = get_SCP_norm(1, 23, SCG);
    SCP->scp[0].n1x = norm.x;
    SCP->scp[0].n1y = norm.y;
    SCP->scp[0].n1z = norm.z;
    SCP->scp[0].r1n = get_SCP_distance(norm, 1, SCG);   
    /*second plane*/
    norm = get_SCP_norm(24, 34, SCG);
    SCP->scp[0].n2x = norm.x;
    SCP->scp[0].n2y = norm.y;
    SCP->scp[0].n2z = norm.z;
    SCP->scp[0].r2n = get_SCP_distance(norm, 24, SCG);
    /*third plane*/
    if(SC_VERSION_FLAG == 1)
      norm = get_SCP_norm(35, 42, SCG);
    else 
      norm = get_SCP_norm(35, 45, SCG);
    SCP->scp[0].n3x = norm.x;
    SCP->scp[0].n3y = norm.y;
    SCP->scp[0].n3z = norm.z;
    SCP->scp[0].r3n = get_SCP_distance(norm, 35, SCG);
    /*fourth plane*/
    if(SC_VERSION_FLAG == 1) {
      norm = get_SCP_norm(43, 48, SCG);
      SCP->scp[0].r4n = get_SCP_distance(norm, 43, SCG);
    }
    else {
      norm = get_SCP_norm(46, 57, SCG);
      SCP->scp[0].r4n = get_SCP_distance(norm, 46, SCG);
    }
    SCP->scp[0].n4x = norm.x;
    SCP->scp[0].n4y = norm.y;
    SCP->scp[0].n4z = norm.z;
  }
}

vector3_t get_SCP_norm(int low_id, int high_id, clasSCG_t *SCG){
  vector3_t left_low, right_low, left_high, right_high, v1, v2;
 
  left_low = get_left_edge(SCG, low_id);
  right_low = get_right_edge(SCG, low_id);
  
  left_high = get_left_edge(SCG, high_id);
  right_high = get_right_edge(SCG, high_id);
  
  v1 = v3sub(left_low, right_low);
  v2 = v3sub(left_low, left_high);
  
  return(v3norm(v3cross(v2,v1)));
}

float get_SCP_distance(vector3_t norm, int id, clasSCG_t *SCG){
  vector3_t point;
  float depth;
  /*input: normal vector to plane, paddle on plane, pointer to SCG bank
    output: distance to plane from the origin*/
  
  /*get a point on the plane*/
  point = get_left_edge(SCG, id);
  
  return(v3dot(norm, point));

}
/* Find the normal to the scintillator plane*/
vector3_t get_bar_normal(clasSCP_t *SCP, clasSCG_t *SCG, int id){
  int plane=SCG->scg[id-1].panel;
  vector3_t normal;

  switch(plane){
  case 1:
    normal.x = SCP->scp[0].n1x;
    normal.y = SCP->scp[0].n1y;
    normal.z = SCP->scp[0].n1z;
    break;
  case 2:
    normal.x = SCP->scp[0].n2x;
    normal.y = SCP->scp[0].n2y;
    normal.z = SCP->scp[0].n2z;
    break;
  case 3:
    normal.x = SCP->scp[0].n3x;
    normal.y = SCP->scp[0].n3y;
    normal.z = SCP->scp[0].n3z;
    break;
  case 4:
    normal.x = SCP->scp[0].n4x;
    normal.y = SCP->scp[0].n4y;
    normal.z = SCP->scp[0].n4z;
    break;
  default:
    break;
  }
  return(normal);
  
}

/*returns the bar_center in the clas coordinate system*/
vector3_t get_bar_center(clasSCG_t *SCG, int id){
  vector3_t bar_edge_l,bar_edge_r, bar_center;

  /*clas coordinate system*/
  bar_edge_l = get_left_edge(SCG, id);
  bar_edge_r = get_right_edge(SCG, id);

  /*find the center of the bar in the clas coordinate system*/
  bar_center = v3add(bar_edge_l, bar_edge_r);
  bar_center = v3div(2.0, bar_center);

  return(bar_center);
}

/*returns the bar length in cm*/
float get_bar_length(clasSCG_t *SCG, int id){
  vector3_t bar_edge_l,bar_edge_r, bar_center;

  /*clas coordinate system*/
  bar_edge_l = get_left_edge(SCG, id);
  bar_edge_r = get_right_edge(SCG, id);

  /*find the length of the bar */
  return(v3mag(v3sub(bar_edge_l,bar_edge_r)));
}

/*find a unit vector pointing along the length of the bar*/
vector3_t get_bar_direction(clasSCG_t *SCG, int id){
  vector3_t bar_edge_l,bar_edge_r, bar_direction;

  /*clas coordinate system*/
  bar_edge_l = get_left_edge(SCG, id);
  bar_edge_r = get_right_edge(SCG, id);

  bar_direction = v3sub(bar_edge_r, bar_edge_l);
  return(v3norm(bar_direction));
}

/* Find the left coordinate of the paddle in the CLAS coordinate system */
vector3_t get_left_edge(clasSCG_t *SCG, int id){
  vector3_t left;
  /*clas coordinate system*/
  left.x = SCG->scg[id-1].xccw; /*counter clockwise is left*/
  left.y = SCG->scg[id-1].yccw;
  left.z = SCG->scg[id-1].zccw;
  return(left);
}

/* Find the right coordinate of the paddle in the CLAS coordinate system */
vector3_t get_right_edge(clasSCG_t *SCG, int id){
  vector3_t right;

  right.x = SCG->scg[id-1].xcw; /*clockwise is right*/
  right.y = SCG->scg[id-1].ycw;
  right.z = SCG->scg[id-1].zcw;
  return(right);
}

void fill_ang_rot(double *ptr_mat, float ang[3]) {
  /* Function to fill array ang_rot(3,3) from Euler angles */
  /* ang(0) = thet, ang(1) = phi, ang(2) = psi */
  double thet,phi,psi;
  thet = ang[0]*PI/180.0;
  phi = ang[1]*PI/180.0;
  psi = ang[2]*PI/180.0;
  *ptr_mat       = cos(psi)*cos(phi) - cos(thet)*sin(phi)*sin(psi);
  *(ptr_mat + 1) = - sin(psi)*cos(phi) - cos(thet)*sin(phi)*cos(psi);
  *(ptr_mat + 2) = sin(thet)*sin(phi);
  *(ptr_mat + 3) = cos(psi)*sin(phi) + cos(thet)*cos(phi)*sin(psi);
  *(ptr_mat + 4) = -sin(psi)*sin(phi) + cos(thet)*cos(phi)*cos(psi);
  *(ptr_mat + 5) = - sin(thet)*cos(phi);
  *(ptr_mat + 6) = sin(thet)*sin(psi);
  *(ptr_mat + 7) = sin(thet)*cos(psi);
  *(ptr_mat + 8) = cos(thet);
  return;
}

vector3_t mat_mult(double ang_rot[3][3], vector3_t vec) {
 /* multiply vector by ang_rot matrix  */
   vector3_t vec_out;
   vec_out.x = ang_rot[0][0]*vec.x + ang_rot[0][1]*vec.y + ang_rot[0][2]*vec.z;
   vec_out.y = ang_rot[1][0]*vec.x + ang_rot[1][1]*vec.y + ang_rot[1][2]*vec.z;
   vec_out.z = ang_rot[2][0]*vec.x + ang_rot[2][1]*vec.y + ang_rot[2][2]*vec.z;
   return vec_out;
   }

int make_SCG_banks(int runno){
  /*input: runno, CLAS_PARMS env var, map file and bos file
    output: SCG bank*/
  char mess[200];
  char *dir,bosfile[100],map[100];
  float *sf_ang_ptr[6];
  double *ptr_mat, ang_rot[3][3];
  float fwd_carr_ang[3], n_clam_ang[3], s_clam_ang[3];
  float space_f1_ang[3], space_f2_ang[3], space_f3_ang[3];
  float space_f4_ang[3], space_f5_ang[3], space_f6_ang[3];
  vector3_t *sf_pos_ptr[6];
  vector3_t fwd_carr_pos;
  vector3_t n_clam_pos;
  vector3_t s_clam_pos;
  vector3_t space_f1_pos, space_f2_pos, space_f3_pos;
  vector3_t space_f4_pos, space_f5_pos, space_f6_pos;
  int firsttime, lun = 42;
  clasSCG_t *SCG=NULL;
  int ret, sec;
  vector3_t mat_mult(double ang_rot[3][3], vector3_t vec);
  void fill_ang_rot(double *ptr_mat, float ang[3]);

  sf_ang_ptr[0] = &space_f1_ang[0];
  sf_ang_ptr[1] = &space_f2_ang[0];
  sf_ang_ptr[2] = &space_f3_ang[0];
  sf_ang_ptr[3] = &space_f4_ang[0];
  sf_ang_ptr[4] = &space_f5_ang[0];
  sf_ang_ptr[5] = &space_f6_ang[0];
  sf_pos_ptr[0] = &space_f1_pos   ;
  sf_pos_ptr[1] = &space_f2_pos   ;
  sf_pos_ptr[2] = &space_f3_pos   ;
  sf_pos_ptr[3] = &space_f4_pos   ;
  sf_pos_ptr[4] = &space_f5_pos   ;
  sf_pos_ptr[5] = &space_f6_pos   ;
  /*reslun_("make_SCG_bank", lun, ret, 13);*/
  dir = getenv("CLAS_PARMS");
  if(SC_VERSION_FLAG == 1)
    sprintf(bosfile,"%s/sc_geom.bfp",dir); 
  else 
    sprintf(bosfile,"%s/sc_v2_geom.bfp",dir); 
  sprintf(map, "%s/Maps/GEOMETRY.map", dir);
  map_get_float(map, "fwd_carriage", "position", 3, (float *)(&fwd_carr_pos.x), runno, &firsttime);
  map_get_float(map, "north_clam", "position", 3, (float *)(&n_clam_pos.x), runno, &firsttime);
  map_get_float(map, "south_clam", "position", 3, (float *)(&s_clam_pos.x), runno, &firsttime);
  map_get_float(map, "space_frame_1", "position", 3, (float *)(&space_f1_pos.x), runno, &firsttime);
  map_get_float(map, "space_frame_2", "position", 3, (float *)(&space_f2_pos.x), runno, &firsttime);
  map_get_float(map, "space_frame_3", "position", 3, (float *)(&space_f3_pos.x), runno, &firsttime);
  map_get_float(map, "space_frame_4", "position", 3, (float *)(&space_f4_pos.x), runno, &firsttime);
  map_get_float(map, "space_frame_5", "position", 3, (float *)(&space_f5_pos.x), runno, &firsttime);
  map_get_float(map, "space_frame_6", "position", 3, (float *)(&space_f6_pos.x), runno, &firsttime);
  map_get_float(map, "fwd_carriage", "rotation", 3, fwd_carr_ang, runno, &firsttime);
  map_get_float(map, "north_clam", "rotation", 3, n_clam_ang, runno, &firsttime);
  map_get_float(map, "south_clam", "rotation", 3, s_clam_ang, runno, &firsttime);
  map_get_float(map, "space_frame_1", "rotation", 3, space_f1_ang, runno, &firsttime);
  map_get_float(map, "space_frame_2", "rotation", 3, space_f2_ang, runno, &firsttime);
  map_get_float(map, "space_frame_3", "rotation", 3, space_f3_ang, runno, &firsttime);
  map_get_float(map, "space_frame_4", "rotation", 3, space_f4_ang, runno, &firsttime);
  map_get_float(map, "space_frame_5", "rotation", 3, space_f5_ang, runno, &firsttime);
  map_get_float(map, "space_frame_6", "rotation", 3, space_f6_ang, runno, &firsttime);

  sprintf(mess,"OPEN SCGFILE UNIT=%d FILE=\"%s\" READ", lun, bosfile);
  fparm_c(mess);
  getBOS(&wcs_,lun,"T");

  int locClamIndex, locMaxPaddle;
  if(SC_VERSION_FLAG == 1){
    locClamIndex = 42;
    locMaxPaddle = 48;
  }else {
    locClamIndex = 45;
    locMaxPaddle = 57;
  }

  for(sec = 1; sec <= 6; sec++){
    if(SCG = getGroup(&wcs_, "SCG ", sec)){
      int i;
      for (i = 0; i < 23 ; i++){
	vector3_t sleft, sright; /*left and right edge corrected for fwd carriage position*/
	vector3_t left, right; /*left and right edge corrected for fwd carriage rotation*/
	sleft = v3add(get_left_edge(SCG,SCG->scg[i].id), fwd_carr_pos); 
	sright = v3add(get_right_edge(SCG,SCG->scg[i].id), fwd_carr_pos);
	ptr_mat = &ang_rot[0][0];
	fill_ang_rot(ptr_mat, fwd_carr_ang); /* Fill rotation matrix ang_rot  */
	left = mat_mult(ang_rot, sleft);     /* Rotate  */
	right = mat_mult(ang_rot, sright);
	SCG->scg[i].xccw = left.x; /*counter clockwise is left*/
	SCG->scg[i].yccw = left.y; 
	SCG->scg[i].zccw = left.z; 
	SCG->scg[i].xcw = right.x; /*clockwise is right*/
	SCG->scg[i].ycw = right.y; 
	SCG->scg[i].zcw = right.z; 
      }
    }
  }

  for(sec = 3; sec < 6; sec++){
    if(SCG = getGroup(&wcs_, "SCG ", sec)){
      int i;
      for (i = 23; i < locClamIndex; i++){
	vector3_t sleft, sright; /*left and right edge corrected for north clamshell position*/
	vector3_t left, right; /*left and right edge corrected for north clamshell rotation*/
	sleft = v3add(get_left_edge(SCG,SCG->scg[i].id), n_clam_pos); 
	sright = v3add(get_right_edge(SCG,SCG->scg[i].id), n_clam_pos);
	ptr_mat = &ang_rot[0][0];
	fill_ang_rot(ptr_mat, n_clam_ang);
	left = mat_mult(ang_rot, sleft); 
	right = mat_mult(ang_rot, sright); 
	SCG->scg[i].xccw = left.x; /*counter clockwise is left*/
	SCG->scg[i].yccw = left.y; 
	SCG->scg[i].zccw = left.z; 
	SCG->scg[i].xcw = right.x; /*clockwise is right*/
	SCG->scg[i].ycw = right.y; 
	SCG->scg[i].zcw = right.z; 
      }
    }
  }

   for(sec = 1; sec < 3; sec++){
    if(SCG = getGroup(&wcs_, "SCG ", sec)){
      int i;
      for (i = 23; i < locClamIndex; i++){
	vector3_t sleft, sright; /*left and right edge corrected for south clamshell position*/
	vector3_t left, right; /*left and right edge corrected for south clamshell rotation*/
	sleft = v3add(get_left_edge(SCG,SCG->scg[i].id), s_clam_pos); 
	sright = v3add(get_right_edge(SCG,SCG->scg[i].id), s_clam_pos); 
	ptr_mat = &ang_rot[0][0];
	fill_ang_rot(ptr_mat, s_clam_ang);
	left = mat_mult(ang_rot, sleft); 
	right = mat_mult(ang_rot, sright);
	SCG->scg[i].xccw = left.x; /*counter clockwise is left*/
	SCG->scg[i].yccw = left.y; 
	SCG->scg[i].zccw = left.z; 
	SCG->scg[i].xcw = right.x; /*clockwise is right*/
	SCG->scg[i].ycw = right.y; 
	SCG->scg[i].zcw = right.z; 
      }
    }
  }
 
   if(SCG = getGroup(&wcs_, "SCG ", 6)){
      int i;
      for (i = 23; i < locClamIndex; i++){
	vector3_t sleft, sright; /*left and right edge corrected for south clamshell position*/
	vector3_t left, right; /*left and right edge corrected for south clamshell rotation*/
	sleft = v3add(get_left_edge(SCG,SCG->scg[i].id), s_clam_pos); 
	sright = v3add(get_right_edge(SCG,SCG->scg[i].id), s_clam_pos);
	ptr_mat = &ang_rot[0][0];
	fill_ang_rot(ptr_mat, s_clam_ang);
	left = mat_mult(ang_rot, sleft); 
	right = mat_mult(ang_rot, sright); 
	SCG->scg[i].xccw = left.x; /*counter clockwise is left*/
	SCG->scg[i].yccw = left.y; 
	SCG->scg[i].zccw = left.z; 
	SCG->scg[i].xcw = right.x; /*clockwise is right*/
	SCG->scg[i].ycw = right.y; 
	SCG->scg[i].zcw = right.z; 
      }
    }
    
   /*handle space frame (panel 4)*/   
   for(sec = 1; sec <=6; sec++){
      if(SCG = getGroup(&wcs_, "SCG ", sec)){
      int i,isec=sec-1;
      float phisec;
      for (i = locClamIndex; i < locMaxPaddle ; i++){
	/*left and right edge corrected for space frame position*/
	vector3_t left, right, vaxle, vtemp, sleft, sright;
	/* vaxle is the position of the axle that the given space frame sector rotates about*/ 
	phisec = isec*PI/3.0;
	vaxle.x = 302.7*cos(phisec);
	vaxle.y = 302.7*sin(phisec);
	vaxle.z = -272.5;
	vtemp = v3add(*sf_pos_ptr[isec], vaxle);
	sleft = v3sub(get_left_edge(SCG,SCG->scg[i].id), vaxle); 
	sright = v3sub(get_right_edge(SCG,SCG->scg[i].id), vaxle);
	/*left and right edge corrected for space frame rotation*/
	ptr_mat = &ang_rot[0][0];
	fill_ang_rot(ptr_mat, sf_ang_ptr[isec]);
	left = v3add(mat_mult(ang_rot, sleft), vtemp); 
	right = v3add(mat_mult(ang_rot, sright), vtemp); 
	SCG->scg[i].xccw = left.x; /*counter clockwise is left*/
	SCG->scg[i].yccw = left.y; 
	SCG->scg[i].zccw = left.z; 
	SCG->scg[i].xcw = right.x; /*clockwise is right*/
	SCG->scg[i].ycw = right.y; 
	SCG->scg[i].zcw = right.z; 
      }
    }
  }
  
  fparm_c("CLOSE SCGFILE");
  fprintf(stderr,"ToF Geometry initialized for run %d.\n",runno);
}















