
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <math.h>
#include <kinematics.h>
#include <g10pcor.h>

/*_end_inc */
/*
 *  Local pre-defined variables:
 *  ---------------------------  
 *  RCS information: 
*/
#define USE(var) static void * use_##var = (void *) &var
/* keep this all on one line so CVS can recognize this */
static char crcsid[] = "$Id: Initg10pcor.c,v 1.3 2005/03/18 18:40:55 mibe Exp $";
USE(crcsid);  /* make sure it is not optimized away */

/*   Module information: */
      static char crname[] = "Initg10pcor";
      static char crauth[] = "Nathan Baltzell";
/*
 *  executable code for InitEloss:
 *  -------------------------------------
*/

static int initialyzed=0;
static int CurrentRun=-1;

void initg10pcor_(int torus, int icor){
  /* Fortran wrapper */
  Initg10pcor(torus, icor);
  return;
}


/*--------------------------------------------*/
void Initg10pcor(int torus, int icor){
  char *dir;

  dir = getenv("G10PCOR");

  /* Check input parameters */
  fprintf(stderr, "\nInitg10pcor: Checking input parameters....\n");
  if (torus>0) {
    fprintf(stderr, "Initg10pcor: torus current                 = %d A\n",torus);
  } else {
    fprintf(stderr, "Initg10pcor:ERROR: unknown torus current  = %d A\n",torus);
  }

  if (icor==1) {
    fprintf(stderr, "Initg10pcor: choice of correction function = MM\'s correction (gd-->pppi-)\n");
    if ( (torus!=2250)&&(torus!=3375) ) {
      fprintf(stderr, "Initg10pcor:WARNING: corrections will be scaled by the torus current (untested!!!)\n",torus);
    }
  } else if (icor==2) {
    fprintf(stderr, "Initg10pcor: choice of correction function = TM\'s correction (gd-->pppi-)\n");
    if ( (torus!=2250)&&(torus!=3375) ) {
      fprintf(stderr, "Initg10pcor:WARNING: corrections will be scaled by the torus current (untested!!!)\n",torus);
    }
  } else if (icor==3) {
    fprintf(stderr, "Initg10pcor: choice of correction function = NB\'s correction (K0-->pi+pi-)\n");
    if ( (torus!=2250) ) {
      fprintf(stderr, "Initg10pcor:WARNING: corrections will be scaled by the torus current (untested!!!)\n",torus);
    }
  } else if (icor==4) {
    fprintf(stderr, "Initg10pcor: choice of correction function = combined correction (gd-->pppi-, K0)\n");
    if ( (torus!=2250)&&(torus!=3375) ) {
      fprintf(stderr, "Initg10pcor:WARNING: corrections will be scaled by the torus current (untested!!!)\n",torus);
    }
  } else {
    fprintf(stderr, "Initg10pcor:ERROR: unknown correction function, icor = %d\n",icor);
  }

  return;
}
