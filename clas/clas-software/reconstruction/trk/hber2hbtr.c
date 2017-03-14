#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <math.h>
#include <bostypes.h>
#include <makebanks.h>

/* -=======================================================- *
$Id: hber2hbtr.c,v 1.3 1998/06/10 14:57:38 mcnabb Exp $
$Author: mcnabb $
$Revision: 1.3 $
$Date: 1998/06/10 14:57:38 $
* -=======================================================- */
     
int hber2hbtr(hber_t *hber,hbtr_t *hbtr){
  /*note that hber and hbtr are pointers to the row of the respective banks*/
  float alpha,q,p,lambda,phi;
  int slinfo[6],sector,trk_in_sect,trk_in_hbtr;
  static int first =1;
  static float Piby3;
  float cl,sl,cp,sp,ca,sa;      /*sin and cos of lambda,phi, and alpha */  

  if(first){
    first=0;
    Piby3 = acos(-1) / 3;
  }

  get_hber_info(hber,slinfo,&sector,&trk_in_sect,&trk_in_hbtr);
  alpha = Piby3*(sector-1);
  lambda = hber->lambda;
  phi = hber->phi;
  sl = sin(lambda);
  cl = cos(lambda);
  sp = sin(phi);
  cp = cos(phi);
  sa = sin(alpha);
  ca = cos(alpha);
  p = fabs(1/((hber)->q_over_p));
  q = (hber)->q_over_p * p;
  
  (hbtr->vert).x = -hber->z0 * sa;
  (hbtr->vert).y = hber->z0 * ca;
  (hbtr->vert).z = -1 * hber->d0 / sp;
             /* -d0/sin(phi) */
  (hbtr->p).x = p*(cl*sp*ca-sl*sa);
             /* p*(cos(lambda)*sin(phi)*cos(alpha)-sin(lambda)*sin(alpha)*/
  (hbtr->p).y = p*(cl*sp*sa+sl*ca);
             /* p*(cos(lambda)*sin(phi)*sin(alpha)+sin(lambda)*cos(alpha)*/
  (hbtr->p).z = p*cl*cp;
             /* p*cos(lambda)*cos(phi) */
  hbtr->q = q;
  hbtr->chi2 = hber->chi2;
  hbtr->itr_sec = (int) trk_in_sect + 100 * sector;
  return(1);
}




