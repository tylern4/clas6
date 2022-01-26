/* Rewrite of cernlibs ctimef function
 *
 * With singularity >~ 3.5 ctime caused a segfault
 * This should fix that segfault.
 *
 * Nick Tyler Jan 26th 2022 (lbl, NERSC)
 *
 */
#include <stdio.h>
#include <time.h>

/*>    ROUTINE CTIMEF (CLOCK, STIME)
  CERN PROGLIB# Z265    CTIMEF          .VERSION KERNFOR  4.36  930602
  ORIG. 14/03/91, RDM
  Fortran interface routine to ctime

     CLOCK  encoded time (returned by, e.g. STATF)
     STIME  decoded time string of length 24 (CHARACTER*24 STIME)
*/
void nicksctimef_(clock, stime)
int  *clock;
char *stime;
{
    printf("time = %s", ctime(&clock));
    strncpy(stime,ctime(&clock),24);
}
