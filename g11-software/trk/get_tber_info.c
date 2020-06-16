#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <math.h>
#include <bostypes.h>
#include <makebanks.h>
#include <kinematics.h>

/* -=======================================================- *
$Id: get_tber_info.c,v 1.5 2001/12/06 20:32:48 feuerbac Exp $
$Author: feuerbac $
$Revision: 1.5 $
$Date: 2001/12/06 20:32:48 $
* -=======================================================- */

#define SMALL 0.05

int get_tber_info(tber_t *tber,int slinfo[6], int *psec, int *ptrk_in_sect,int *ptrk_in_hbtr){         
  /*extracts info from layinfo1 and layinfo2 and puts the layer hit 
   info into an array of 6 characters (1 per super layer) and the sector and
   track number info into appropriate chars.
   note that the slinfo is stored as slinfo[superlayer-1] as per usual for 
   c code */

    int temp1,temp2,i;
    
    temp1 = tber->layinfo1;
    temp2 = tber->layinfo2;
    for(i=0;i<5;i++){
       slinfo[i] = temp1%(64);/*extract layinfo by superlayer*/
       temp1=temp1/64;
    }
    slinfo[5]=temp2%256;
    temp2 = temp2/256;
    *ptrk_in_sect = temp2%256;
    temp2 = temp2/256;
    *ptrk_in_hbtr = temp2%256;
    temp2 = temp2/256;
    *psec = temp2%256;
    return(1);
}

int put_tber_info_sector(tber_t *tber, vector3_t vrt, vector3_t p, float charge, float chi2, float c[5][5], int slinfo[6], int trk_in_sect, int trk_in_hbtr, int sector)
     /* fills a tber structure given the track vertex and direction
      * ( as 3-vectors in the Detector Coordinate system ).
      * The covariance matrix, layer status info array, chi2, 
      * track number in the sector and track number in the hbtr bank
      * are here for only for completeness.
      */
{
  static float pi=-1.;
  float cossec,sinsec;
  float phi;
  vector3_t vscs,dscs;
  
  int i,tmp;

  if (pi<0.)
    pi = acos(-1.);

  phi = atan2(p.y,p.x)+pi/6.;
  if (phi<0.) phi += 2*pi;
  phi -= ((sector-1)*pi/3. + pi/6.);
  
  cossec = cos((sector-1)*pi/3.);
  sinsec = sin((sector-1)*pi/3.);

  vscs.z = vrt.z;
  vscs.x = vrt.x*cossec+vrt.y*sinsec;
  vscs.y =-vrt.x*sinsec+vrt.y*cossec;

  dscs.z = p.z;
  dscs.x = p.x*cossec+p.y*sinsec;
  dscs.y =-p.x*sinsec+p.y*cossec;

  if (fabs(vscs.x) > SMALL) {  /* error -- the vertex is not on the vertex plane. */
#ifdef DEBUG
    fprintf(stderr,"put_tber_info: track does not start at vertex plane\n");
#endif
    return 0;
  }

  tber->z0 = vscs.y;
  tber->phi = atan2(dscs.x,dscs.z);
  tber->d0 = -vscs.z*sin(tber->phi);
  tber->lambda = atan2(dscs.y,sqrt(dscs.x*dscs.x+dscs.z*dscs.z));
  
  tber->q_over_p = charge/v3mag(dscs);

  tber->c11 = c[0][0];
  tber->c12 = c[0][1];
  tber->c13 = c[0][2];
  tber->c14 = c[0][3];
  tber->c15 = c[0][4];
  tber->c22 = c[1][1];
  tber->c23 = c[1][2];
  tber->c24 = c[1][3];
  tber->c25 = c[1][4];
  tber->c33 = c[2][2];
  tber->c34 = c[2][3];
  tber->c35 = c[2][4];
  tber->c44 = c[3][3];
  tber->c45 = c[3][4];
  tber->c55 = c[4][4];

  tber->chi2 = chi2;
  tmp = 0;
  for (i=4; i>=0; i--){
    tmp = tmp*64 + slinfo[i];
  }
  tber->layinfo1 = tmp;
  tmp = sector;
  tmp = tmp*256+trk_in_hbtr;
  tmp = tmp*256+trk_in_sect;
  tmp = tmp*256+slinfo[5];
  tber->layinfo2 = tmp;

  return 1;
}

    
/* USE put_tber_info_sector INSTEAD to get RELIABLE results
 * Sometimes the sector number isn't what it appears to be....
 */

int put_tber_info(tber_t *tber, vector3_t vrt, vector3_t p, float charge, float chi2, float c[5][5], int slinfo[6], int trk_in_sect, int trk_in_hbtr)
     /* fills a tber structure given the track vertex and direction
      * ( as 3-vectors in the Detector Coordinate system ).
      * The covariance matrix, layer status info array, chi2, 
      * track number in the sector and track number in the hbtr bank
      * are here for only for completeness.
      */
{
  static float pi=-1.;
  float phi;
  
  int sector;

  if (pi<0.)
    pi = acos(-1.);

  phi = atan2(p.y,p.x)+pi/6.;
  if (phi<0.)   phi += 2*pi;
  if (phi>=2*pi) phi -= 2*pi;
  sector = phi/(pi/3.) + 1;
  if (sector > 6) sector -= 6;

  return put_tber_info_sector(tber,vrt,p,charge,chi2,c,slinfo,trk_in_sect,trk_in_hbtr,sector);
}


