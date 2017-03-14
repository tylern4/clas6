/* -=======================================================- *
$Id: svt.c,v 1.3 2007/08/22 01:46:33 holtrop Exp $
$Author: holtrop $
* -=======================================================- */

static char rcsVers[] = "$Revision: 1.3 $" ;
static char rcsDate[] = "$Date: 2007/08/22 01:46:33 $" ;

#include <stdio.h>
#ifdef Darwin
#include <sys/malloc.h>
#else
#include <malloc.h>
#endif

#include "svt.h"

SVT *svt;
extern int scat_debuglevel;
/* -========================================================- */
/*
 *  U hand me a buffer I can write in. I'll put the
 *  current rcs version and date in the buffer for you.
 */
/* -========================================================- */
int Scat_Initialize (b)
char * b;
{
  if (b == 0) return 0;

  sprintf (b, "scat_init: error: no memory for svt...") ;
  svt = calloc (1, sizeof (SVT));
  if (svt == 0)
    return 0;

  sprintf (b, "Scat ...\tversion[%s]\tCompiledOn[%s] ", rcsVers, rcsDate) ;
  svt->SourceVersion = rcsVers ;
  svt->DateCompiled = rcsDate ;
  scat_debuglevel = 0;
  return 1;
}
/* -********************************************************- */
/* -********************************************************- */
int scat_initialize_ (z, lz)
char *z;
int lz;
{
  return Scat_Initialize (z) ;
}
/* -========================================================- */
/* -========================================================- */
printSVT (b)
char *b;
{
  sprintf (b, "program number .......: %ld\n", svt->program);
  sprintf (b, "%sversion number .....: %ld\n", b, svt->vers);
  sprintf (b, "%sprogram name .......: %s\n", b, svt->pgm_name);
  sprintf (b, "%strans number  ......: %d\n", b, svt->_trans);
  sprintf (b, "%ssystem name ........: %s\n", b,svt->uts.sysname);
  sprintf (b, "%snode name :.........: %s\n", b, svt->uts.nodename);
  sprintf (b, "%srelease : ..........: %s\n", b, svt->uts.release);
  sprintf (b, "%sversion : ..........: %s\n", b, svt->uts.version);
  sprintf (b, "%smachine : ..........: %s\n", b, svt->uts.machine);
  sprintf (b, "%sSource Version .....: %s\n", b, svt->SourceVersion);
  sprintf (b, "%sDate Compiled ......: %s\n", b, svt->DateCompiled);
/*
  sprintf (b, "%sdomain name : ......: %s\n", b, svt->uts.domainname);
*/
}
/* -========================================================- */
/* -========================================================- */
long Scat_ProgramNo ()
{
  return svt->program;
}
/* -========================================================- */
/* -========================================================- */
int scat_programno_ ()
{
  return svt->program;
}
/* -========================================================- */
/* -========================================================- */
long Scat_VersionNo ()
{
  return svt->vers;
}
/* -========================================================- */
/* -========================================================- */
int scat_versionno_ ()
{
  return svt->vers;
}
/* -========================================================- */
/* -========================================================- */
void Scat_CallTrace (f, n, l)
char *f;
char *n;
int   l;
{
  char b[104];

  sprintf (b, "file[%s] fcn[%s] line[%d]", f, n, l);
  Scat_ErrorMsg (b);
}
/* -========================================================- */
/* -========================================================- */
void Scat_Debug (dl)
int dl;
{
  scat_debuglevel = dl;
}
