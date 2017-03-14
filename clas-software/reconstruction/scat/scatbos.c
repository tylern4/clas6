/*
 *  JAM - The Simple Communications Attachment Library.
 *
 *  Read back physics events in BOS format.
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
#
extern Event * Scat_SendMsg ();
extern void bos_ ();
extern int nbank_ ();

extern void evout_printDCbank ();
extern void evout_printDCHbank ();
extern void evout_printPIbank ();
extern void evout_printPObank ();
extern void evout_printCCbank ();
extern void evout_printCC1bank ();
extern void evout_printCCHbank ();
extern void evout_printECbank ();
extern void evout_printECHbank ();
extern void evout_printEC1bank ();
extern void evout_printSTHbank ();
extern void evout_printSCHbank ();
extern void evout_printSCbank ();
extern void evout_printHEADbank ();
extern void evout_printMCINbank ();
extern void evout_printMCVXbank ();
extern void evout_printMCTKbank ();
extern void evout_printDC0bank ();
extern void evout_printMCEVbank ();
extern void evout_printMCPAbank ();




static int _r=0;
static int *rtn = &_r;


#define MAX_FCNS         22


extern char   *bosBankNameCC   ;
extern char   *bosBankNameCC1  ;
extern char   *bosBankNameSC   ;

extern char   *bosBankNameEC   ;
extern char   *bosBankNameEC1  ;

extern char   *bosBankNamePI   ;
extern char   *bosBankNamePO   ;
extern char   *bosBankNameTG   ;
extern char   *bosBankNameTA   ;

extern char   *bosBankNameHEAD ;
extern char   *bosBankNameMCIN ;
extern char   *bosBankNameMCVX ;
extern char   *bosBankNameMCTK ;

extern char   *bosBankNameDC0  ;

       


typedef struct CMD_TAB {
  char            *pbname;        /* bank name */
  void            (*fp)();       /* the print code */
} CMD_TAB;
 

CMD_TAB cmd_tab[MAX_FCNS];

void not_imp (k, l) 
int *k;
int l;
{
int i=0;
  printf ("bank type not implemented.\n");
}

void myinit () {

  cmd_tab[0].pbname = "no  ";
  cmd_tab[0].fp = not_imp;

  cmd_tab[1].pbname = "no  ";
  cmd_tab[1].fp = not_imp;

  cmd_tab[2].pbname = bosBankNameCC;
  cmd_tab[2].fp =    evout_printCCbank ;

  cmd_tab[3].pbname = bosBankNameCC1;
  cmd_tab[3].fp =    evout_printCC1bank ;

  cmd_tab[4].pbname = bosBankNameSC;
  cmd_tab[4].fp =    evout_printSCbank ;

  cmd_tab[5].pbname = "no  ";
  cmd_tab[5].fp = not_imp;

  cmd_tab[6].pbname = "no  ";
  cmd_tab[6].fp = not_imp;


  cmd_tab[7].pbname = bosBankNameEC;
  cmd_tab[7].fp =    evout_printECbank ;

  cmd_tab[8].pbname = bosBankNameEC1;
  cmd_tab[8].fp =    evout_printEC1bank ;

  cmd_tab[9].pbname = bosBankNamePI;    
  cmd_tab[9].fp     = not_imp ;

  cmd_tab[10].pbname = bosBankNamePO;
  cmd_tab[10].fp     = not_imp ;


  cmd_tab[11].pbname = bosBankNameTG;
  cmd_tab[11].fp     = not_imp ;

  cmd_tab[12].pbname = bosBankNameTA;
  cmd_tab[12].fp     = not_imp ;

  cmd_tab[13].pbname = bosBankNameHEAD;
  cmd_tab[13].fp =    evout_printHEADbank ;

  cmd_tab[14].pbname = bosBankNameMCIN;
  cmd_tab[14].fp =    evout_printMCINbank ;

  cmd_tab[15].pbname = bosBankNameMCVX;
  cmd_tab[15].fp =    evout_printMCVXbank ;

  cmd_tab[16].pbname = bosBankNameMCTK;
  cmd_tab[16].fp =    evout_printMCTKbank ;

  cmd_tab[17].pbname = "no  ";
  cmd_tab[17].fp = not_imp;

  cmd_tab[18].pbname = "no  ";
  cmd_tab[18].fp = not_imp;


  cmd_tab[19].pbname = bosBankNameDC0;
  cmd_tab[19].fp =    evout_printDC0bank ;

  cmd_tab[20].pbname = "no  ";
  cmd_tab[20].fp = not_imp;
}

int Scat_Filter2Bos (indx, list, cmd, piw)
int  indx;
char *list;
char *cmd;
int  *piw;
{
  int i, ind, j, nami, in, nbs, f;
  Event *e;
  aBank *zb;
  int *ibd, pid ;
  short *sbd ;
  char msg[1024];

  e = Scat_BrowFilter (indx, cmd, list);
  if (e == 0) {
    printf ("Browser Filter failed:\n");
    return 0;
  }
  nbs = e->b.b_len;
  printf ("number of banks %d\n", nbs);
  zb = e->b.b_val ;
  piw[0] = 3;
  bgarb_ (piw);
  for (i=0; i<e->b.b_len; i++) {
    bkfmt_(zb->name, zb->format, strlen(zb->name), strlen(zb->format));
    
    ind = nbank_ (zb->name, &zb->nr, &zb->ncols, &zb->nrows, strlen(zb->name));
    if (ind == 0) {
      printf ("mbank call failed - great software!\n");
      return 0;
    }
    printf ("made bank: %s %s indx=%d rn=%d c=%d r=%d\n", 
	    zb->name, zb->format, ind, zb->nr, zb->ncols, zb->nrows);
    if (zb->bd.bd_val->type == 1) {
      sbd = &piw[ind];
      for (j=0; j<zb->bd.bd_val->bdata_u.sraw.sraw_len; j++)
	*sbd++ = zb->bd.bd_val->bdata_u.sraw.sraw_val[j] ;
    }
    else
      if (zb->bd.bd_val->type == 0) {
	for (j=0; j<zb->bd.bd_val->bdata_u.iraw.iraw_len; j++)
	  *(piw+ind+j) = zb->bd.bd_val->bdata_u.iraw.iraw_val[j] ;
      }
      else {
	printf ("corrupt event rcv'd\n");
	break;
      }
    /*
    myinit () ;
    
    in = nlink_(zb->name, &zb->nr, 4) ;
    printf ("nlink: name = \"%s\", rec no = %d, index = %d\n", zb->name, zb->nr, in);
    
    for (f=0; f<MAX_FCNS; f++) {
      if ((strcmp (zb->name, cmd_tab[f].pbname)) == 0)
	if ((strcmp (zb->name, "DC0 ")) == 0)
	  cmd_tab[f].fp (&piw[in], zb->nrows) ;
	else
	  cmd_tab[f].fp (&piw[in], zb->nrows) ;
    }
    */
    zb++;
  }
  return 1;
}
/* -=====================================================================- */
