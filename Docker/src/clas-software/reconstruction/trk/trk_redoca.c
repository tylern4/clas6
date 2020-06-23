/*      float trk_redoca_ */
/*
 *_begin_doc
 *  RCS ID string
 *  $Id: trk_redoca.c,v 1.4 1999/02/03 18:55:56 feuerbac Exp $
*/
/*
 *  Documentation for function TRK_REDOCA
 *
 *  Purpose:      Returns distance of closest approach (doca) of a straight
 *  --------      track from ptpos in direction ptdir to (sector,layer,wire)
 *                (*psector,*player,*pwire).
 *
 *  Calling Sequence:
 *  ----------------
 *
 *  Input Parameters:  (Name - Type - Meaning)
 *  ----------------   ptpos  pointer to vector3_t (or array of float [3])
 *                              initial position of track segment.
 *                     ptdir  pointer to vector3_t (or array of float [3])
 *                              direction of track segment at ptpos.
 *                     psector, player, pwire  pointers to int
 *                              sector, layer, wire to calculate doca to.
 *
 *  Output Parameters:  (Name - Type - Meaning)
 *  -----------------  returns float   doca to wire
 *                      or 999. if it cannot get the bosbank.
 *
 *  Called from:
 *  ------------
 *
 *  Other routines:
 *  ---------------
 *
 *  Notes:
 *  ------
 *
 *  Author:   David Ciarletta      Created:  Tue Apr 14 11:05:38 EDT 1998
 *  -------
 *
 *  Major revisions:
 *  ----------------
 *     
 *
 *_end_doc
 */
/*
 *_begin_inc
 *  include files :
/*_end_inc */
#include <stdlib.h>
#include <math.h>
#include "kinematics.h"
#include "bostypes.h"

#define MAX_WIRE 192

/*_begin_var
 *  input/output variables:
 *  -----------------------
 *
 *  Local pre-defined variables:
 *  ---------------------------  
 *  RCS information: 
*/
      static char cfile[] =  "$RCSfile: trk_redoca.c,v $";
      static char crevis[] = "$Revision: 1.4 $";
      static char cstate[] = "$State: Exp $";
      static char cdate[] =  "$Date: 1999/02/03 18:55:56 $";
      static char cautho[] = "$Author: feuerbac $";
/*   Module information: */
      static char crname[] = "TRK_REDOCA";
      static char crauth[] = "David Ciarletta";
#define USE(var) static void * use_##var = (void *) &var
static char crcsid[] = 
   "$Id: trk_redoca.c,v 1.4 1999/02/03 18:55:56 feuerbac Exp $";
USE(crcsid);   /* make sure it is not optimized away */

/*
 *  Local User defined variables:
 *  ----------------------------- 
 *
 *_end_var
 *  executable code for routine TRK_REDOCA:
 *  -------------------------------------
*/
float trk_redoca_(vector3_t *ptpos, vector3_t *ptdir, int *psector, int *player, int *pwire) {
  
  vector3_t tpos;
  vector3_t tdir;
  vector3_t wpos;
  vector3_t wdir;
  vector3_t twdis;
  vector3_t twperp;
  vector3_t utwperp;
  int wire;
  float doca;
  int sector = *psector;

  clasDCGW_t *DCGW = NULL; /* pointer structure to BOS BANK */

  /* Get geometry values from BOS BANK, if NULL exit and return 999 */
  if(DCGW = (clasDCGW_t *)getGroup(&wcs_,"DCGW",sector)) {

    /* store arguments in local variables */
    wire = (int)((*player-1)*MAX_WIRE+(*pwire-1));
    tpos = *ptpos;
    tdir = *ptdir;

    wpos.x = DCGW->dcgw[wire].x_mid;
    wpos.y = DCGW->dcgw[wire].y_mid;
    wpos.z = DCGW->dcgw[wire].z_mid;
    wdir.x = DCGW->dcgw[wire].x_dir;
    wdir.y = DCGW->dcgw[wire].y_dir;
    wdir.z = DCGW->dcgw[wire].z_dir;

    /* Calculate unit vector perpendicular to wire and track */
    twperp = v3cross(wdir, tdir);
    utwperp = v3div(v3mag(twperp), twperp);

    /* Calculate distance from track point to wire at midplane */
    twdis = v3sub(tpos, wpos);

    /* Take dot product of distance and perpendicular unit vector */
    doca = v3dot(twdis, utwperp);
  }
  else {doca = 999;}

  return doca;
}
/*
*-------------------------------------------------------------------------
*/



