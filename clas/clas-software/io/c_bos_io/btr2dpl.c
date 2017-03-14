#include <stdio.h>
#include <ntypes.h>
#include <bostypes.h>
#include <utility.h>

/* Alan Coleman, College of Wm & Mary, 12/3/97 */


hdpl_t * hbtr2hdpl(hbtr_t  *hb) 
{
  int trkno; /*number of track in sector */
  int sec;
  clasHDPL_t *HDPL = NULL;
  

  /* lets unpack itr_sec -- Cheez! Is this stupid or what! */

  trkno = hb->itr_sec %100;
  sec = hb->itr_sec/100;

  
  if(HDPL = getGroup(&bcs_,"HDPL",sec)){
    return(&(HDPL->hdpl[(trkno-1)*NoOfPlanesPerTrack]));
  }
  return(NULL);
}

tdpl_t * tbtr2tdpl(tbtr_t  *tb) 
{
  int trkno; /*number of track in sector */
  int sec;
  clasTDPL_t *TDPL = NULL;
  

  /* lets unpack itr_sec -- Cheez! Is this stupid or what! */

  trkno = tb->itr_sec %100;
  sec = tb->itr_sec/100;

  
  if(TDPL = getGroup(&bcs_,"TDPL",sec)){
    return(&(TDPL->tdpl[(trkno-1)*NoOfPlanesPerTrack]));
  }
  return(NULL);
}

hbla_t *hbtr2hbla(hbtr_t *hbtr){
  int trkno; /*number of track in sector */
  int sec;
  clasHBLA_t *HBLA = NULL;
  

  trkno = hbtr->itr_sec %100;
  sec = hbtr->itr_sec/100;

  
  if(HBLA = getGroup(&bcs_,"HBLA",sec)){
    return(&(HBLA->hbla[(trkno-1)*HBLAPlanesPerTrack]));
  }
  return(NULL);  


}

tbla_t *tbtr2tbla(tbtr_t *tbtr){
  int trkno; /*number of track in sector */
  int sec;
  clasTBLA_t *TBLA = NULL;
  

  trkno = tbtr->itr_sec %100;
  sec = tbtr->itr_sec/100;

  
  if(TBLA = getGroup(&bcs_,"TBLA",sec)){
    return(&(TBLA->tbla[(trkno-1)*HBLAPlanesPerTrack])); /*same number of planes as HBLA */
  }
  return(NULL);  


}

trks_t *hbtr2trks(hbtr_t *hbtr){
  int trkno; /*number of track in sector */
  int sec;
  clasTRKS_t *TRKS = NULL;
  

  trkno = hbtr->itr_sec %100;
  sec = hbtr->itr_sec/100;

  
  if(TRKS = getGroup(&bcs_,"TRKS",sec)){
    return(&(TRKS->trks[trkno-1]));
  }
  return(NULL);  


}
