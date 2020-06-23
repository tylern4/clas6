/*
 *  JAM - The Simple Communications Attachment Library.
 *
 *  package physics events up for xmission.
 *  
 *
 *  Revision history:
 *  JAM - 01/1996 - initial version
 *  JAM - 05/1996 - add in this banner.
 */
#include <stdio.h>
#include <stdlib.h>/* getenv, exit */
#include <sys/types.h>
#include <signal.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/utsname.h>
#include <rpc/rpc.h>
#include <rpc/pmap_prot.h>
#include <rpc/pmap_clnt.h>
#include <netdb.h>

#define TCP 6
#define UDP 17

#include "j.h"
#include "lst.h"
#include "svt.h"

typedef struct Handles {
  unsigned long program;
  unsigned long vers;
  char          *host;
} Handles;

#ifdef HPUX
  static char *rsh_name = "remsh";
#else
  static char *rsh_name = "rsh" ;
#endif
CtlBlk        lst_mgr;
extern SVT    *svt;
/* -========================================================- */
/*
 *  Answer the number of currently packed banks.
 * 
 * -========================================================- */
int Scat_PackedCount ()
{
  smodtrace ("Scat_PackedCount");
  return lst_count (&lst_mgr);
}
/* -========================================================- */
/* 
 *  Pack a bank.  The user *MUST NOT* touch the bank data
 *  until the bank has been: a. sent, b. forgotten. Handle any
 *  special bank packing.
 *
 *  Inputs:
 *    char * bank name
 *    int    record number
 *    int    number of rows
 *    int    number of columns
 *    int    size
 *    int  * pi
 *
 *  Outputs:
 *    return 1 if this worked.
 *
 * -========================================================- */
int Scat_Pack (name, format, rn, r, c, size, pi)
char *name;
char *format;
int rn;
int r;
int c;
int size;
int *pi;
{
  PFrag * p;
  char b[2048];
  
  smodtrace ("Scat_Pack");
  sprintf (b, "name[%s] format[%s] rn[%d] rows[%d] cols[%d] size[%d] pi[%x]", 
	   name, format, rn, r, c, size, pi);
  Scat_ErrorMsg (b);
  if (((strcmp (name, "DC0 ")) == 0) ||
      ((strcmp (name, "EC0 ")) == 0)) {
    format = "B16";
    p = get_pfrag (name, format, rn, r, c, (2*r), 1, pi) ;
    return lst_add (&lst_mgr, p) ;
  }
  if ((strcmp (name, "SC  ")) == 0) {
    format = "B16";
    p = get_pfrag (name, format, rn, r, c, (5*r), 1, pi) ;
    return lst_add (&lst_mgr, p) ;
  }
  if ((strcmp (name, "CC  ")) == 0) {
    format = "B16";
    p = get_pfrag (name, format, rn, r, c, (3*r), 1, pi) ;
    return lst_add (&lst_mgr, p) ;
  }
  if ((strcmp (name, "EC  ")) == 0) {
    format = "B16";
    p = get_pfrag (name, format, rn, r, c, (3*r), 1, pi) ;
    return lst_add (&lst_mgr, p) ;
  }
  format = malloc (1024);
  sprintf (format, "%dI", c);
  p = get_pfrag (name, format, rn, r, c, size, 0, pi) ;
  free (format);
  return lst_add (&lst_mgr, p) ;
}
/* -******************************************************- */
/* 
 *  Fortran wrapper for bank packing
 * -******************************************************- */
int scat_pack_(name, format, rn, nrow, ncol, size, pi, iret, nl, fl)
char *name;
char *format; 
int *rn;
int *nrow;
int *ncol;
int *size;
int *pi;
int *iret;
int nl;
int fl;
{
  char *b, *f;

  smodtrace ("scat_pack_");
  b = malloc (nl+1);
  strncpy (b, name, nl);
  b[nl] = 0x00;

  f = malloc (fl+1);
  strncpy (f, format, fl);
  f[fl] = 0x00;

  sprintf (svt->scratchBuffer, "Fscat_pack-> %d -> %d\n%s %s\n", nl, fl, b, f);
  Scat_ErrorMsg(svt->scratchBuffer) ;

  *iret = Scat_Pack (b, f, *rn, *nrow, *ncol, *size, pi);
  free (b);
  free (f);
}
/* -========================================================- */
/* 
 *  Bank packing has been setup. This just formats an event 
 *  so the xdr routines can pack'em.
 *
 *  Inputs:
 *    address of a event structure.
 *
 *  Outputs:
 *    integer return code.
 *
 * -========================================================- */
int Scat_AsPacked (ee)
Event *ee;
{
  int _z1, _z3;
  aBank *_z2;
  bdata *_z4;

  PFrag *p = lst_nxt (&lst_mgr, 0) ;
  smodtrace ("Scat_AsPacked");
  _z1 = lst_count (&lst_mgr) ;
  ee->b.b_len = _z1;

  ee->b.b_val = malloc (_z1 * sizeof (aBank)) ;
  if (ee->b.b_val == 0) {
    Scat_ErrorMsg("Scat_AsPacked: no mem") ;
    return 0;
  }
  _z2 = ee->b.b_val;
  for (_z3=0; _z3 < _z1; _z3++) {
    _z2->name = p->name;
    _z2->format = p->format;
    _z2->nr = p->nr;
    _z2->nrows = p->nrows;
    _z2->ncols = p->ncols;
    _z2->d_size = p->d_size;
    _z2->bd.bd_len = 1;
    _z2->bd.bd_val = malloc (sizeof (bdata)) ;
    _z4 = _z2->bd.bd_val;
    if (p->type == 0) {
      _z4->type = p->type;
      _z4->bdata_u.iraw.iraw_len = p->d_size;
      _z4->bdata_u.iraw.iraw_val = p->in;
    }
    else {
      _z4->type = p->type;
      _z4->bdata_u.sraw.sraw_len = p->d_size;
      _z4->bdata_u.sraw.sraw_val = p->sn;
    }

    _z2++ ;
    p = lst_nxt (&lst_mgr, 1) ;
  }
  return 1;
}
/* -========================================================- */
/* -========================================================- */
int Scat_PackInit ()
{
  smodtrace ("Scat_PackInit");
  lst_init (&lst_mgr);
  return 1;
}
/* -========================================================- */
/* -========================================================- */
void scat_packinit_ (iret)
int *iret;
{
  smodtrace ("scat_packinit_");
  *iret = Scat_PackInit ();
}
/* -========================================================- */
/* -========================================================- */
int Scat_PackForget (ep)
Event *ep ;
{
  smodtrace ("Scat_PackForget");
  if (ep != 0) {
    free (ep->b.b_val);
  }
  lst_drop (&lst_mgr) ;
  return 1;
}
/* -********************************************************- */
/* -********************************************************- */
int scat_packforget_ ()
{
  Scat_PackForget (NULL) ;
}
