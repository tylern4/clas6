/*      subroutine gem_set_default*/
/*
 *_begin_doc
 *  RCS ID string
 *  $Id: gem_set_default.c,v 1.1.1.1 2006/02/03 21:36:19 stepanya Exp $
*/
/*
 *  Documentation for subroutine gem_set_default
 *
 *  Purpose:
 *  --------
 *     set default rtpc analysis flags
 *
 *
 *  Calling Sequence:
 *  ----------------
 *
 *  Input Parameters:  (Name - Type - Meaning)
 *  ----------------
 *
 *  Output Parameters:  (Name - Type - Meaning) all in SCS coord system.
 *  -----------------
 
 *  Called from: gem_init_c.c 
 *  ------------
 *
 * Author: gabriel niculescu, 2005
 *  -------
 *
 * Major revisions:  created - 2005 GN.
 * ----------------

 *
 *_end_doc
 */
/*  RCS information: 
*/

#include "gem.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>


#define USE(var) static void * use_##var = (void *) &var

static char crcsid[] =
   "$Id: gem_set_default.c,v 1.1.1.1 2006/02/03 21:36:19 stepanya Exp $";
USE(crcsid);   /* make sure it is not optimized away */
/*
 *   Module information:
 */
  static char crname[] = "GEM_INIT_C";
  static char crauth[] = "Gabriel Niculescu";
/*
 *_begin_inc
 *  include files :
 *  ---------------------
 */
/* BOS common block  uncomment the next line for BOS include file */
/*#include <bos.inc> */
/* _end_inc */

int doDefault;
int tail_cancel;
int cm_filter;
int base_sub;
int tail_clip;
int graphics;
int doDCcomp;//nkb


void gem_set_default_(){
 
  base_sub = TRUE;
  tail_cancel = TRUE;
  tail_clip = TRUE;
  cm_filter = TRUE;
  doDCcomp = FALSE;
  quiet = TRUE;
  graphics = FALSE;
  return;
}
/*
*-------------------------------------------------------------------------
*/



