#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <math.h>
#include <bostypes.h>
#include <makebanks.h>

/* -=======================================================- *
$Id: tber2tbtr.c,v 1.4 1999/08/05 16:30:18 feuerbac Exp $
$Author: feuerbac $
$Revision: 1.4 $
$Date: 1999/08/05 16:30:18 $
* -=======================================================- */
     
int tber2tbtr(tber_t *tber,tbtr_t *tbtr,int tbt_trkno){
  /* note that tber and tbtr are pointers to the row of the respective bank*/
  /* that you want to convert and that tbt_trkno is the track number       */
  /*(ie the row) tbt_trkno is not relevent */
  float alpha,q,p,lambda,phi;
  int slinfo[6],sector,trk_in_sect,trk_in_hbtr;
  static int first =1;
  static float Piby3;
  float cl,sl,cp,sp,ca,sa;      /*sin and cos of lambda,phi, and alpha */  

  if(first){
    first=0;
    Piby3 = acos(-1)/3;
  }

  get_tber_info(tber,slinfo,&sector,&trk_in_sect,&trk_in_hbtr);
  alpha = Piby3*(sector-1);
  lambda = tber->lambda;
  phi = tber->phi;
  sl = sin(lambda);
  cl = cos(lambda);
  sp = sin(phi);
  cp = cos(phi);
  sa = sin(alpha);
  ca = cos(alpha);
  p = fabs(1/((tber)->q_over_p));
  q = (tber)->q_over_p * p;
  
  (tbtr->vert).x = -tber->z0 * sa;
  (tbtr->vert).y = tber->z0 * ca;
  (tbtr->vert).z = -1 * tber->d0 / sin(phi);
  (tbtr->p).x = p*(cl*sp*ca-sl*sa);
             /* p*(cos(lambda)*sin(phi)*cos(alpha)-sin(lambda)*sin(alpha)*/
  (tbtr->p).y = p*(cl*sp*sa+sl*ca);
             /* p*(cos(lambda)*sin(phi)*sin(alpha)+sin(lambda)*cos(alpha)*/
  (tbtr->p).z = p*cl*cp;
             /* p*cos(lambda)*cos(phi) */
  tbtr->q = q;
  tbtr->chi2 = tber->chi2;
  tbtr->itr_sec = trk_in_sect + 100 * sector;
  tbtr->itr_hbt = trk_in_hbtr;/* + 100 * tbt_trkno;*/
                              /*note: this has been removed to be consistant*/
                              /*with the ddl file, though it differs from */
                              /*the previous filling of tbtr*/ 
  return(1);
}



int tber2tbtrBank(int number) {
  /* rebuild the whole TBTR bank from the TBER bank, with
   * the bank number "number"
   */
  clasTBER_t *TBER = getBank(&bcs_,"TBER");
  clasTBTR_t *TBTR = NULL;
  int i, nrow;

  if (TBER) {
    nrow = TBER->bank.nrow;
    TBTR = makeBank(&bcs_,"TBTR",number, sizeof(tbtr_t)/sizeof(int), nrow);
    if (TBTR) {
      for( i=0; i<nrow; i++ )
	tber2tbtr(&(TBER->tber[i]),&(TBTR->tbtr[i]),i);
    }
  }
  
  return 1;
}
