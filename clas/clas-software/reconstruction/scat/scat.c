/*
 *  JAM - The Simple Communications Attachment Library.
 *
 *  Code to send data around. The BOS interface 
 *  is in scatbos.c.
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


extern Handles ** pHandle ;
int           scat_debuglevel ;
extern SVT    *svt;

/* -========================================================- */
/*
 *  Write an error msg to standout.
 *
 * -========================================================- */
void Scat_ErrorMsg (p)
char *p ;
{
  if (scat_debuglevel > 0)
    printf ("%s\n", p);
}
/* -========================================================- */
/* -========================================================- */
int Scat_SendMsgNr (indx, msg)
int indx;
char *msg;
{
  CLIENT *clnt;
  int    *res ;
  Event  msgnoreply_arg;

  smodtrace ("Scat_SendMsgNr");
  Scat_ErrorMsg (msg) ;

  msgnoreply_arg.s = msg;
  msgnoreply_arg.b.b_len = 0;

  if (indx < 0) return 0;
/*
  printf ("%s %d %d\n", pHandle[indx]->host, pHandle[indx]->program, pHandle[indx]->vers);
*/
  clnt = clnt_create(pHandle[indx]->host, 
		     pHandle[indx]->program, 
		     pHandle[indx]->vers, "tcp");
  if (clnt == NULL) {
    clnt_pcreateerror("SendMsgNr: client create failed");
    exit(1);
  }
  res = msgnoreply_1(&msgnoreply_arg, clnt);
  if (res == NULL) {
    clnt_perror(clnt, "SendMsgNr: call failed:");
    clnt_destroy( clnt );
    return 0;
  }
  clnt_destroy( clnt );
  return *res;
}
/* -********************************************************- */
/* -********************************************************- */
int scat_sendmsgnr_ (indx, msg, ml)
int *indx;
char *msg;
int ml;
{
  char *b ;
  
  smodtrace ("scat_sendmsgnr_");
  b = malloc (ml+1);
  strncpy (b, msg, ml);
  b[ml] = 0;
  Scat_SendMsgNr (*indx, b) ;
  free (b);
}
/* -========================================================- */
/* -========================================================- */
int Scat_SendEvtNr (indx, msg)
int indx;
char *msg ;
{
  CLIENT *clnt;
  int    i, *res ;
  Event  msgnoreply_arg;

  smodtrace ("Scat_SendEvtNr");
  if (indx < 0) return 0;

  msgnoreply_arg.s = msg;
  i = Scat_AsPacked (&msgnoreply_arg) ;

  clnt = clnt_create(pHandle[indx]->host, 
		     pHandle[indx]->program, 
		     pHandle[indx]->vers, "tcp");
  if (clnt == NULL) {
    clnt_pcreateerror("SendEvtNr: client create failed");
    return 0;
  }
  res = msgnoreply_1(&msgnoreply_arg, clnt);
  if (res == NULL) {
    clnt_perror(clnt, "SendEvtNr: call failed:");
    clnt_destroy( clnt );
    return 0;
  }
  clnt_destroy( clnt );
  return *res;
}
/* -********************************************************- */
/* -********************************************************- */
int scat_sendevtnr_ (indx, msg, iret, ml)
int *indx;
char *msg;
int *iret;
int ml;
{
  char *b;
  b = malloc (ml+1);
  strncpy (b, msg, ml);
  b[ml] = 0x00;

  smodtrace ("scat_sendevtnr_");
  *iret = Scat_SendEvtNr (*indx, b);
  free (b);
}
/* -========================================================- */
/* -========================================================- */
void Scat_SendEvt2Lst (indx, list)
int indx;
char *list;
{
  char b[256];

  smodtrace ("Scat_SendEvt2Lst");
  sprintf (b, " create %s ", list);
  (void)Scat_SendEvtNr (indx, b);
}
/* -********************************************************- */
/* -********************************************************- */
int scat_sendevt2lst_ (indx, list, iret, ll)
int *indx;
char *list;
int *iret;
int ll;
{
  char *msg, *b;
  b = malloc (ll+1);
  msg = malloc (1024);
  strncpy (b, list, ll);
  b[ll] = 0x00;

  smodtrace ("scat_sendevt2lst");
  sprintf (msg, "create %s ", b);
  *iret = Scat_SendEvtNr (*indx, msg);
  free (b);
  free (msg);
}
/* -========================================================- */
/* -========================================================- */
Event *  Scat_SendMsg (indx, msg)
int indx;
char *msg;
{
  CLIENT *clnt;
  Event  *res ;
  Event  _arg;

  smodtrace ("Scat_SendMsg");
  if (indx < 0) return 0;

  _arg.s = msg;
  _arg.b.b_len = 0;

  clnt = clnt_create(pHandle[indx]->host, 
		     pHandle[indx]->program, 
		     pHandle[indx]->vers, "tcp");
  if (clnt == NULL) {
    clnt_pcreateerror("SendMsg: client create failed");
    return 0;
  }
  res = msgwreply_1(&_arg, clnt);
  if (res == NULL) {
    clnt_perror(clnt, "SendMsg: call failed:");
    clnt_destroy( clnt );
    return 0;
  }
  clnt_destroy( clnt );
  return res;
}
/* -========================================================- */
/* -========================================================- */
int  Scat_BrowCreate (h, p, v)
char *h;
unsigned long p;
unsigned long v;
{
  CLIENT *clnt;
  pid_t  pid;
  int    indx, *res ;
  Event  _arg;
  char   msg[250] ;

  smodtrace ("Scat_BrowCreate");
  indx = Scat_InitHandle (h, p, v);

  pid = getpid ();

  clnt = clnt_create(pHandle[indx]->host, 
		     pHandle[indx]->program, 
		     pHandle[indx]->vers, "tcp");
  if (clnt == NULL) {
    clnt_pcreateerror("BrowCreate: client create failed");
    return 0;
  }

  sprintf (msg, "browcrt %d ", pid);

  _arg.s = msg;
  _arg.b.b_len = 0;

  res = browcreate_1(&_arg, clnt);
  if (res == NULL) {
    clnt_perror(clnt, "BrowCreate: call failed:");
    clnt_destroy( clnt );
    return 0;
  }
  clnt_destroy( clnt );
  return indx;
}
/* -========================================================- */
/* -========================================================- */
Event *  Scat_BrowFilter (hand, cmd, lst)
int hand;
char *cmd;
char *lst;
{
  CLIENT *clnt;
  int    id;
  Event  *res ;
  Event  _arg;

  smodtrace ("Scat_BrowFilter");
  if (hand < 0) return 0;

  _arg.s = malloc (1024) ;
  id = getpid ();
  sprintf (_arg.s, "browflt %d %s %s ", id, lst, cmd) ;
  _arg.b.b_len = 0;

  clnt = clnt_create(pHandle[hand]->host, 
		     pHandle[hand]->program, 
		     pHandle[hand]->vers, "tcp");
  if (clnt == NULL) {
    clnt_pcreateerror("BrowFilter: client create failed");
    return 0;
  }
  res = browfilter_1(&_arg, clnt);
  free (_arg.s);
  if (res == NULL) {
    clnt_perror(clnt, "BrowFilter: call failed:");
    clnt_destroy( clnt );
    return 0;
  }
  clnt_destroy( clnt );
  free (_arg.s);
  return res;
}
/* -========================================================- */
