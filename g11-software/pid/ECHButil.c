#include <stdio.h>
#include <ntypes.h>
#include <bostypes.h>
#include <kinematics.h>
#include <ec.h>
#include <sc.h>
#include <utility.h>

int echb2sector(echb_t *echb){
  return(echb->sect/100);
}

int echb2layer(echb_t *echb){
  return((echb->sect)%100);
}

float dist_echb_tdpl(echb_t *echb, tdpl_t *tdpl){
  int sector = echb2sector(echb);
  int layer = echb2layer(echb);
  /*stuff the forward plane position into a three vector*/
  vector3_t trk_pos = tdpl[ForwardECPlane].pos; 
  vector3_t dir=tdpl[ForwardECPlane].dir;
  vector3_t ec_pos = xyz2v3(&(echb->x_hit));
  vector3_t diff;

  if (layer==ECHB_WHOLE)
    trk_pos=v3add(trk_pos,inner2whole_displacement(dir,sector));
  /*rotate to lab*/
  trk_pos = sector2lab(trk_pos, sector);

  diff = v3sub(trk_pos, ec_pos);

  return(v3mag(diff));
  
}

/*----------------------------------------------------------------------- */
/* Function that returns the vector projecting a track (whose direction at the
   inner face of the calorimeter is "dir") to the position of the so-called 
   "whole" plane.  The ECP bank is required. */

vector3_t inner2whole_displacement(vector3_t dir,int sec){
  vector3_t zero3={0.0,0.0,0.0};
  vector3_t ecp_norm=zero3;
  float dir_dot_norm=0.0,distance_to_layer=0.0;
  clasECP_t *ECP=NULL;

  if (!(ECP=getGroup(&wcs_,"ECP ",sec))) return (zero3);
  if( v3mag(dir) < 0.00001 ) return (zero3);

  ecp_norm=lab2sector(xyz2v3(&(ECP->ecp[0].n1x)),sec);
  dir_dot_norm=v3dot(dir,ecp_norm);
  distance_to_layer=ECP->ecp[0].plw/dir_dot_norm;
  
  return( v3mult(distance_to_layer,dir) );

}


vector3_t sc_pos2whole_plane(scrc_t *scrc,int sec){
  clasECP_t *ECP=getGroup(&wcs_,"ECP ",sec);
  clasSCP_t *SCP=getGroup(&wcs_,"SCP ",sec);
  clasSCG_t *SCG=getGroup(&wcs_,"SCG ",sec);
  vector3_t sc_projection=scrc->pos;
  vector3_t sc_ec_diff={0,0,0};
  vector3_t sc_plane_pos={0,0,0};
  vector3_t ec_plane_pos={0,0,0};
  vector3_t ec_norm={0,0,0},ec_norm_lab={0,0,0};
  vector3_t sc_norm={0,0,0},sc_norm_lab={0,0,0};
  float diff_dot_sc_norm=0,mag_sc_ec_diff=0;
  vector3_t sc_ec_displacement={0,0,0};

  if (!SCP || !SCG || !ECP) return(sc_projection);

  sc_norm_lab=get_bar_normal(SCP,SCG,scrc->id);
  sc_norm=lab2sector(sc_norm_lab,sec);
  sc_plane_pos=v3mult(get_SCP_distance(sc_norm_lab,scrc->id,SCG),sc_norm_lab);
  sc_plane_pos=lab2sector(sc_plane_pos,sec);

  ec_norm_lab=xyz2v3(&(ECP->ecp[0].n1x));
  ec_plane_pos=v3mult(ECP->ecp[0].r1n+ECP->ecp[0].plw,ec_norm_lab);
  ec_plane_pos=lab2sector(ec_plane_pos,sec);
 
  sc_ec_diff=v3sub(ec_plane_pos,sc_plane_pos);
  diff_dot_sc_norm=v3dot(sc_ec_diff,sc_norm);
  mag_sc_ec_diff=v3mag(sc_ec_diff);
  sc_ec_displacement=v3mult(mag_sc_ec_diff*mag_sc_ec_diff/diff_dot_sc_norm,
			    sc_norm);
  sc_projection=v3add(sc_projection,sc_ec_displacement);
  
  return(sc_projection);

}


/*-----------------------------------------------------------------------------------------------*/
int ec_Whole2InOut(echb_t * Whole, echb_t ** Inner, echb_t ** Outer){

  /* Purpose: Given an echb whole hit, give pointers to the associated inner and outer hits. 
              Returns 1 if matching successful and 0 if not.
     Author: R A Thompson
     
     Notes: Whole->matchid1(2) is a counter that represents the inner(outer) hit associated with
     the whole hit.  It is not the index in the ECHB bank of the associated hit, but rather a 
     counter on inner(outer) within a sector.  For example Whole->matchid1 = 2 means that the whole     hit is accociated with the second inner hit in the sector that the Whole hit is in (not the 
     second row in echb).  This subroutine should not have to exist. 
     */
     
  int i;
  int InHits  = 0;
  int OutHits = 0;      /* counts inner and outer hits within sectors*/
  int Sector, Layer;
  int WholeSector = echb2sector(Whole);

  clasECHB_t *ec_rslt;

  *Inner = *Outer = NULL;

  /* make sure this is really a whole hit */
  if(echb2layer(Whole) != ECHB_WHOLE)
    return 0;

  if(ec_rslt = (clasECHB_t *)getBank(&bcs_,"ECHB")){
    for(i=0; i< ec_rslt->bank.nrow; i++){
      if( (Sector = echb2sector(&(ec_rslt->echb[i]))) != WholeSector )
	continue;
      Layer  = echb2layer(&(ec_rslt->echb[i]));
      if(Layer==ECHB_INNER && ++InHits==Whole->matchid1)
	*Inner = &(ec_rslt->echb[i]);
      else if(Layer==ECHB_OUTER && ++OutHits==Whole->matchid2)
	*Outer = &(ec_rslt->echb[i]);
    }   
  }
  return (*Inner || *Outer);
}

/*-----------------------------------------------------------------------------------------------*/







