/*
 *  JAM - The Simple Communications Attachment Library.
 *
 *  Answer information about scat topology.
 *  
 *
 *  Revision history:
 *  JAM - 01/1996 - initial version
 *  JAM - 05/1996 - add in this banner.
 *  JAM - 12/1996 - changed EventStore cmd line and reflect that here.
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

Handles ** pHandle = 0;

static int numHandles = 0;   /* MUST Start at 0 */

#ifdef HPUX
  static char *rsh_name = "remsh";
#else
  static char *rsh_name = "rsh" ;
#endif

extern SVT    *svt;

/* -========================================================- */
/*
 *  Reach into the string scb and copy 
 *  the user id into the string b, which
 *  the caller provides.
 *
 * -========================================================- */
void Scat_Conn_Uid (scb, b)
char *scb;
char *b;
{
  int sl=0;
  char *p = &scb[0];

  smodtrace ("Scat_Conn_Uid");
  Scat_ErrorMsg (scb);
  while (*p != ' ') {
    p++;
    sl += 1;           /* find length of uid string */
  }

  strncpy (b, p, sl);
  b[sl] = 0;
}

/* -========================================================- */
/*
 *  Reach into the string scb and copy 
 *  the program name into the string z, which
 *  the caller provides.
 *
 * -========================================================- */
void Scat_Conn_PgmName (scb, z)
char *scb;
char *z;
{
  int sl=0;
  char *p = &scb[0], *a;

  smodtrace ("Scat_Conn_PgmName");
  Scat_ErrorMsg (scb);
  while (*p != ' ') p++;  /* skip uid */
  while (*p == ' ') p++;
  a = p;
  while (*p != ' ') {
    p++;
    sl += 1;            /* find length of pgm name */
  }
  strncpy (z, a, sl);
  z[sl] = 0;
}

/* -========================================================- */
/*
 *  Reach into the string scb and copy 
 *  the node name into the string z, which
 *  the caller provides.
 *
 * -========================================================- */
char *Scat_Conn_NodeName (scb, z)
char *scb;
char *z;
{
  int sl=0;
  char *p = &scb[0], *a;

  smodtrace ("Scat_Conn_NodeName");
  Scat_ErrorMsg (scb);
  while (*p != ' ') p++; /* skip uid */
  while (*p == ' ') p++; /* skip white */
  while (*p != ' ') p++; /* skip pgm name */
  while (*p == ' ') p++;
  a = p;
  while (*p != ' ') {
    p++;
    sl += 1;              /* find length of node name */
  }
  strncpy (z, a, sl);
  z[sl] = 0;
}

/* -========================================================- */
/*
 *  Reach into the string scb and return 
 *  the program number ( an unsigned long)
 *
 * -========================================================- */
unsigned long Scat_Conn_PgmNo (scb)
char *scb;
{
  char *p = &scb[0];

  smodtrace ("Scat_Conn_PgmNo");
  Scat_ErrorMsg (scb);
  while (*p != ' ') p++; /* skip uid */
  while (*p == ' ') p++; /* skip white */
  while (*p != ' ') p++; /* skip pgm name */
  while (*p == ' ') p++;
  while (*p != ' ') p++; /* skip node name */
  while (*p == ' ') p++; /* skip white */

  return atol (p);
}

/* -========================================================- */
/*
 *  Reach into the string scb and return 
 *  the program number ( an unsigned long)
 *
 * -========================================================- */
unsigned long Scat_Conn_VerNo (scb)
char *scb;
{
  char *p = &scb[0];

  smodtrace ("Scat_Conn_VerNo");
  Scat_ErrorMsg (scb);
  while (*p != ' ') p++; /* skip uid */
  while (*p == ' ') p++; /* skip white */
  while (*p != ' ') p++; /* skip pgm name */
  while (*p == ' ') p++;
  while (*p != ' ') p++; /* skip node name */
  while (*p == ' ') p++; /* skip white */
  while (*p != ' ') p++; /* skip pgm no */
  while (*p == ' ') p++; /* skip white */

  return atol (p);
}

/* -========================================================- */
/*
 *  Reach into the string scb and copy 
 *  the list name into the string z, which
 *  the caller provides.
 *
 * -========================================================- */
void Scat_Conn_ListName (scb, z)
char *scb;
char *z;
{
  int sl=0;
  char *p = &scb[0], *a;

  smodtrace ("Scat_Conn_ListName");
  Scat_ErrorMsg (scb);
  while (*p != ' ') p++; /* skip uid */
  while (*p == ' ') p++; /* skip white */
  while (*p != ' ') p++; /* skip pgm name */
  while (*p == ' ') p++;
  while (*p != ' ') p++; /* skip node name */
  while (*p == ' ') p++; /* skip white */
  while (*p != ' ') p++; /* skip pgm no */
  while (*p == ' ') p++; /* skip white */
  while (*p != ' ') p++; /* skip vers no */
  while (*p == ' ') p++; /* skip white */
  a=p;
  while (*p != ' ') {
    p++;
    sl += 1;             /* find length of list name */
  }
  strncpy (z, a, sl);
  z[sl] = 0;
}

/* -========================================================- */
/*
 *  Reach into the string scb and return
 *  the number of lists 
 *
 * -========================================================- */
int Scat_Conn_NumNlist (scb)
char *scb;
{
  char *p = &scb[0];

  smodtrace ("Scat_Conn_NumNlist");
  Scat_ErrorMsg (scb);
  while (*p != ' ') p++; /* skip uid */
  while (*p == ' ') p++; 
  while (*p != ' ') p++; /* skip pgm name */
  while (*p == ' ') p++;
  while (*p != ' ') p++; /* skip node name */
  while (*p == ' ') p++; 
  while (*p != ' ') p++; /* skip pgm no */
  while (*p == ' ') p++; 
  while (*p != ' ') p++; /* skip vers no */
  while (*p == ' ') p++; 
  while (*p != ' ') p++; /* skip list name */
  while (*p == ' ') p++; 

  return atoi (p);
}
/* -========================================================- *
/*
 *  get_handle - internal to Scat.
 *
 *  Make a new client handle.  The handle will contain
 *  enough information to allow a client app
 *  to attach to a server appl.
 *
 *  Intput: 
 *    char *host
 *    u_long program number
 *    u_long version number
 *
 *  Output:
 *    integer index into the handle table.
 *
 * -========================================================- */
Handles * get_handle (host, pgm, v)
char *host ;
unsigned long pgm ; 
unsigned long v;
{
  char b[1024];
  Handles * p = malloc (sizeof (Handles)) ;
  smodtrace ("get_handle");
  sprintf (b, "get_handle: host[%s] pgm[%d] ver[%d]", host, pgm, v);
  Scat_ErrorMsg (b);
  if (p == 0) {
    Scat_ErrorMsg("get_handle: no mem");
    return 0;
  }
  p->host = strdup (host);
  p->program = pgm;
  p->vers = v;

  return p;
}
/* -========================================================- */
/* -========================================================- */
int find_handle (host, program, vers)
char *host;
unsigned long program;
unsigned long vers;
{
  int j;

  smodtrace ("find_handle");
  for (j=0; j<numHandles; j++) {
    if ((strcmp (pHandle[j]->host, host)) != 0) continue;
    if (pHandle[j]->program != program) continue;
    if (pHandle[j]->vers != vers) continue;
    return j;
  }
  return -1;   /* if I'm here - no handles to be found */
}
/* -========================================================- */
/* -========================================================- */
void dump_handles ()
{
  int j;

  for (j=0; j<numHandles; j++)
    printf ("dump: %s\t\t%d  %d\n", pHandle[j]->host, pHandle[j]->program,pHandle[j]->vers);
 
}
/* -========================================================- */
/* -========================================================- */
int Scat_InitHandle (host, program, vers)
char *host;
unsigned long program;
unsigned long vers;
{
  Handles * hand;
  int z;

  smodtrace ("Scat_InitHandle");
  if ((z = find_handle (host, program, vers)) == -1) {
    hand = get_handle (host, program, vers) ;
    pHandle = (Handles **)realloc (pHandle, ((numHandles+1) * sizeof (Handles))) ;
    pHandle[numHandles] = hand;
    numHandles += 1;
    return (numHandles - 1);
  }
  else
    return z;
}
/* -********************************************************- */
/* -********************************************************- */
int scat_inithandle_ (host, program, vers, hl)
char *host;
unsigned long *program;
unsigned long *vers;
int hl;
{
  char *b;
  int z;

  smodtrace ("scat_inithandle_");
  b = malloc (hl+1);
  strncpy (b, host, hl);
  b[hl] = 0x00;

  z = Scat_InitHandle (b, *program, *vers);
  free (b);
  return z;
}
/* -========================================================- */
/* -========================================================- */
char * Scat_HandHost (z)
int z ;
{
  return pHandle[z]->host ;
}
/* -========================================================- */
/* -========================================================- */
unsigned long Scat_HandProgram (z)
int z;
{
  return pHandle[z]->program ;
}
/* -========================================================- */
/* -========================================================- */
unsigned long Scat_HandVers (z)
int z;
{
  return pHandle[z]->vers ;
}
/* -========================================================- */
/* -========================================================- */
char * Scat_ServerProbe (h, p, v)
char *h;
unsigned long p;
unsigned long v;
{ 
  CLIENT *clnt;
  char   **res ;
  void   *_arg;

  smodtrace ("Scat_ServerProbe");
  if ( (p == svt->program) && (v == svt->vers) )
    return 0;

  clnt = clnt_create(h, p, v, "tcp");
  if (clnt == NULL) {
    clnt_pcreateerror("SvrProbe: client create failed");
    return 0;
  }
  res = svcname_1(_arg, clnt);
  if (res == NULL) {
    clnt_perror(clnt, "SvrProbe: call failed:");
    clnt_destroy (clnt);
    return 0;
  }
  clnt_destroy( clnt );
  return *res ;
}
/* -========================================================- */
/* -========================================================- */
void BustStringUp (wargs, s, nn)
char **wargs;
char *s;
int  *nn;
{
  char *p, *t;
  int size;
  int tn=*nn, slen, j;

  smodtrace ("BustStringUp");
  if (s == 0) return;
  slen = 8192; /* 8192 = the number of scat lists in EventStore? */
  for (t = s, j=0; j<slen; j++) {
    if ((p = index(t, '\n')) != 0) {
      size = p - t;
      if (wargs[tn])
	free (wargs[tn]);
      wargs[tn] = calloc (1, (size+1));
      strncpy (wargs[tn], t, size);

      p++; t = p ; *nn += 1; tn += 1;

      if (*p == 0) return ;
    }
    else {
      if (wargs[tn])
	free (wargs[tn]);
      size = strlen (t);
      wargs[tn] = calloc (1, (size+1));
      strncpy (wargs[tn], t, size);
      *nn += 1;
      return;
    }
  }
  printf ("BustStringUp: Warning: %d times thru the loop!\n", slen);
  sprintf (svt->scratchBuffer, 
	   "BustStringUp: Warning: %d times thru the loop!\n", slen);
  Scat_ErrorMsg (svt->scratchBuffer);
}
/* -========================================================- */
/* -========================================================- */
char ** Scat_Probe (h)
char *h;
{
  static char         *wargs[100];
  struct sockaddr_in  pin;
  struct hostent      *hp;
  struct pmaplist     *z_out;
  char                *s;
  int                 n, z;

  smodtrace ("Scat_Probe");
  if (h == 0) {
    sprintf (svt->scratchBuffer, "Scat_Probe: need a hostname\n");
    Scat_ErrorMsg(svt->scratchBuffer) ;
    return 0;
  }

  hp = gethostbyname (h);

  if (hp == 0) {
    sprintf (svt->scratchBuffer, "bad name : %s\n", h);
    perror (svt->scratchBuffer);
    return 0;
  }

  bzero (&pin, sizeof (pin));
  pin.sin_family = AF_INET;
  pin.sin_addr.s_addr = 
    ((struct in_addr *)(hp->h_addr))->s_addr;

  z_out = (struct pmaplist *)pmap_getmaps (&pin);
  if (z_out == 0) {
    sprintf (svt->scratchBuffer, 
	     "can\'t check port mapper for some reason ...\nshould exit here ...\n");
    return 0;
  }
  n = 0;
  while (z_out) {
    if ((z_out)->pml_map.pm_vers > 32) {
      if ((s = Scat_ServerProbe (h, 
				 (z_out)->pml_map.pm_prog,
				 (z_out)->pml_map.pm_vers)) != 0) {
	BustStringUp (&wargs, s, &n);
      }
    }
    if (((struct pmaplist *)z_out)->pml_next != 0) 
      z_out = ((struct pmaplist *)z_out)->pml_next;
    else
      break;
  }
  wargs[n] = 0;
  return wargs;
}
/* -========================================================- */
/*
 *  you give me: the host to look at, the program name to find.
 *
 *  I return boolean: 
 *   -2=did not find; 
 *    *=found and init'ed.
 *
 *  where * = in the range ( 0..2^31 )
 *
 * -========================================================- */
int Scat_LocateProgram (host, pgmname)
char *host;
char *pgmname;
{
  char **wargs;
  char theName[1024];
  int  n = 0;

  smodtrace ("Scat_LocateProgram");
  if (host == NULL) return -2;

  wargs = Scat_Probe (host);
  if (wargs[0] == 0) return -2;

  while (wargs[n] != 0) {
    Scat_Conn_PgmName (wargs[n], theName);
    sprintf (svt->scratchBuffer,
	     "locate name --> <%s> ---> <%s>\n", theName, pgmname);
    Scat_ErrorMsg(svt->scratchBuffer);
    if ((strcmp (theName, pgmname)) == 0) {
      return Scat_InitHandle (host, 
		       Scat_Conn_PgmNo (wargs[n]), 
		       Scat_Conn_VerNo (wargs[n])); 
    }
    n++;
  }  
  return -2;
}
/* -********************************************************- */
/* -********************************************************- */
int scat_locateprogram_ (h, pgmn, nseq, hl, pl)
char *h;
char *pgmn;
int  *nseq;
int   hl;
int   pl;
{
  char *host;
  char *pgmname;
  char *zap;
  int r;

  smodtrace ("scat_locateprogram_");
  host = malloc (hl+1);
  pgmname = malloc (pl+1);
  zap = malloc (pl+25);

  strncpy (host, h, hl);
  host[hl] = 0;
  strncpy (pgmname, pgmn, pl);
  pgmname[pl] = 0;

  sprintf (zap, "%s.%d", pgmname, *nseq) ; 

  Scat_ErrorMsg(host);
  Scat_ErrorMsg(zap);
  r = Scat_LocateProgram (host, zap);
  free (host);
  free (pgmname);
  free (zap);
  return r;
}
/* -========================================================- */
/* -========================================================- */
int Scat_StartProgram (h, p, un, id)
char *h;
char *p;
char *un;
int  id;
{
  smodtrace ("Scat_StartProgram");
  sprintf (svt->scratchBuffer, 
	   "%s %s %s -ename %s -enum %d -pgmhost %s -pgmno %d -pgmver %d & ", rsh_name, h, p, un, id, svt->uts.nodename, svt->program, svt->vers) ;
  printf ("JAM-> %s\n", svt->scratchBuffer);
  Scat_ErrorMsg(svt->scratchBuffer);
  system (svt->scratchBuffer);
  printf ("done with start pgm\n");
  return 1;
}
/* -********************************************************- */
/* -********************************************************- */
int scat_startprogram_ (h, p, un, id, iret, hl, pl, ul)
char *h;
char *p;
char *un;
int  *id;
int  *iret;
int  hl;
int  pl;
int  ul;
{
  char *host;
  char *pgmname;
  char *username;
  int r;

  smodtrace ("scat_startprogram_");
  host = malloc (hl+1);
  pgmname = malloc (pl+1);
  username = malloc (ul+1);

  strncpy (host, h, hl);
  host[hl] = 0;

  strncpy (pgmname, p, pl);
  pgmname[pl] = 0;

  strncpy (username, un, ul);
  username[ul] = 0;

  r = Scat_StartProgram (host, pgmname, username, *id);
  free (host);
  free (pgmname);
  free (username);

  printf ("done with fortran start pgm\n");
  return r;
}
/* -========================================================- */
/* -========================================================- */
char * Scat_ServerInfo (indx)
int indx;
{ 
  CLIENT *clnt;
  char   **res ;
  void   *_arg;

  smodtrace ("Scat_ServerInfo");
  clnt = clnt_create(pHandle[indx]->host, 
		     pHandle[indx]->program, 
		     pHandle[indx]->vers, "tcp");
  if (clnt == NULL) {
    clnt_pcreateerror("SvrInfo: client create failed");
    return 0;
  }
  res = svcname_1(_arg, clnt);
  if (res == NULL) {
    clnt_perror(clnt, "SvrInfo: call failed:");
    clnt_destroy( clnt );
    return 0;
  }
  clnt_destroy( clnt );
  return *res ;
}
