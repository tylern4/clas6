
/* the program calculate dE/dx in TOF */

/* input values: 
   - Sector number.
   - SC counter ID within the sector.
   - Dirrect cosines of the track, matched to SC counter.
   - Energy deposited in the SC counter.
   output value:
   - dE/dx in the SC counter (dx = counter thickness, 5.08 cm). */
  
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ntypes.h>
#include <bostypes.h>
#include <map_manager.h>
#include <clas_cern.h> 
#include <kinematics.h>
#include <utility.h>
#include <sc.h>
#include <scExtern.h>
#include <pid.h>

int sc_dedx_(int *sec, int *id, float c[3], float *del_e, float *dedx){
  vector3_t dir_cos = xyz2v3((float *)&(c[0]));

  *dedx = sc_dedx(*sec, *id, dir_cos, *del_e);

}

float sc_dedx(int sec, int id, vector3_t dir_cos_sec, float del_e)
{
  int m;
  float cos_angle;
  float dedx = 0.0;
  vector3_t dir_cos_lab, normal;
  clasSCG_t *SCG = getGroup(&wcs_, "SCG ", sec);
  clasSCP_t *SCP = getGroup(&wcs_, "SCP ", sec);  

  if (SCP && SCG) {
    dir_cos_lab = sector2lab(dir_cos_sec, sec);
    if(id >=0 && id <=SC_NPADDLES_SEC){
      normal = get_bar_normal(SCP, SCG, id);
      cos_angle = v3dot(dir_cos_lab,normal);
      dedx = (del_e)*cos_angle; 
    
      return(dedx);
    }
  } 
  return(-1.0);
}


float tbid2dedx(bid_t *tbid){
  clasSCRC_t *SCRC = getGroup(&bcs_, "SCRC", tbid->sec);
  clasTBTR_t *TBTR = NULL;
  tdpl_t *tdpl = NULL;

  if (TBTR = getBank(&bcs_, "TBTR")){
    tdpl = tbtr2tdpl(&TBTR->tbtr[tbid->track-1]);
    if(tbid->sc.stat && SCRC && tdpl){
      scrc_t *scrc = &(SCRC->scrc[tbid->sc.id-1]);
      return (sc_dedx(tbid->sec, scrc->id, tdpl->dir, scrc->energy));
    }
  }
  return (-1);
}

float hbid2dedx(bid_t *hbid) {
  clasSCRC_t *SCRC = getGroup(&bcs_, "SCRC", hbid->sec);
  clasHBTR_t *HBTR = NULL;
  hdpl_t *hdpl = NULL;
  
  if (HBTR = getBank(&bcs_, "HBTR")){
    hdpl = hbtr2hdpl(&HBTR->hbtr[hbid->track-1]);
    if(hbid->sc.stat && SCRC && hdpl){
      scrc_t *scrc = &(SCRC->scrc[hbid->sc.id-1]);
      return (sc_dedx(hbid->sec, scrc->id, hdpl->dir, scrc->energy));
    }
  }
  return (-1);
}

/* EOF */










