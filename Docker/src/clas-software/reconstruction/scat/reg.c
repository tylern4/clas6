/*
 *  JAM - The Simple Communications Attachment Library.
 *
 *  Register the the caller program to scat.
 *  
 *
 *  Revision history:
 *  JAM - 01/1996 - initial version
 *  JAM - 05/1996 - add in this banner.
 */
#define PORTMAP
#include <stdio.h>
#include <stdlib.h>/* getenv, exit */
#include <sys/types.h>
#include <signal.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/utsname.h>
#include <rpc/rpc.h>
#include "j.h"
#include <rpc/pmap_prot.h>
#include <rpc/pmap_clnt.h>

#include "svt.h"

#define TCP 6
#define UDP 17


extern void evtsvc_inp_1 ();

extern SVT *svt;

char NameNdeath[] = "./.tmpfile";
extern void user_exit ();
/* -==========================================================- */
void Scat_SvcRun ()
{  
  svc_run();
  fprintf(stderr, "svc_run returned");
  exit(1);
}
/* -==========================================================- */
void exit_routine ()
{
  pmap_unset (svt->program, svt->vers);
  user_exit ();
  /*  printf ("exiting this world...\n");   */
  exit (0);
}
/* -==========================================================- */
int Scat_Register (pgm_name)
char *pgm_name;
{
  struct sockaddr_in  pin;
  struct hostent      *hp;
  struct pmaplist     *z_out;
  char                buf[4096];
  register SVCXPRT    *transp;

  svt->program = 0;
  svt->vers = 0;
  svt->_trans = 0;
  svt->stat_result = &svt->_trans;
  svt->pgm_name = strdup (pgm_name);

  signal (SIGSEGV, exit_routine);
  signal (SIGKILL, exit_routine);
  signal (SIGQUIT, exit_routine);
  signal (SIGINT, exit_routine);
  signal (SIGTRAP, exit_routine);
  signal (SIGABRT, exit_routine);
  signal (SIGBUS, exit_routine);
  signal (SIGTERM, exit_routine);

  if ((uname (&svt->uts)) == -1) {
    printf ("ERROR: reg.c: uname failed\n");
    exit (1);
  }
/*
  printf ("system name ........: %s\n", svt->uts.sysname);
  printf ("node name :.........: %s\n", svt->uts.nodename);
  printf ("release : ..........: %s\n", svt->uts.release);
  printf ("version : ..........: %s\n", svt->uts.version);
  printf ("machine : ..........: %s\n", svt->uts.machine);
  printf ("domain name : ......: %s\n", svt->uts.domainname);
*/
  hp = gethostbyname ((char *)svt->uts.nodename);

  if (hp == 0) {
    sprintf (buf, "bad name : %s\n", svt->uts.nodename);
    perror (buf);
    exit (0);
  }

  bzero (&pin, sizeof (pin));
  pin.sin_family = AF_INET;
  pin.sin_addr.s_addr = 
    ((struct in_addr *)(hp->h_addr))->s_addr;

  z_out = (struct pmaplist *)pmap_getmaps (&pin);
  if (z_out == 0) {
    printf ("can\'t check port mapper for some reason ...\nexiting...");
    exit (0);
  }

  while (z_out) {
    svt->program = (z_out)->pml_map.pm_prog;
    if (((struct pmaplist *)z_out)->pml_next != 0) 
      z_out = ((struct pmaplist *)z_out)->pml_next;
    else
      break;
  }
  if (svt->program < 0x20000033)
    svt->program = 0x20000033;
  svt->program += 1;

  /*  printf ("pgm = %d\n", svt->program);   */

  svt->vers = IVER;
  (void) pmap_unset(svt->program, svt->vers);
  
  transp = svctcp_create(RPC_ANYSOCK, 0, 0);
  if (transp == NULL) {
    fprintf(stderr, "cannot create tcp service.");
    exit(1);
  }
  if (!svc_register(transp, svt->program, svt->vers, evtsvc_inp_1, IPPROTO_TCP)) {
    sprintf(buf, "unable to register (%x, %x) %s service\n", svt->program, svt->vers, "tcp");
    fprintf(stderr, buf);
    exit(1);
  } 
  return 1;
}
/* -==========================================================- */
/*  Fortran wrapper            */
void scat_register_ (name, iret, nl)
char *name;
int *iret ;
int nl;
{
  char *n;

  n = malloc (nl);
  strncpy (n, name, nl);
  n[nl] = NULL;
  iret = Scat_Register (n) ;
  free (n);
}
/* -==========================================================- */
void Scat_UnRegister ()
{
  (void) svc_unregister (svt->program, svt->vers);
}
/* -*********************************************************- */
int scat_unregister_ ()
{
  Scat_UnRegister ();
}
/* -==========================================================- */
/*
 * Example of use ...
 *
 * int main (int argc, char *argv[])
 * {
 *   SCATregister (argc, argv);
 *   printf ("Im active and waiting\n");
 *   while (1) {
 *     ScatActive ();
 *   }
 * }
 * 
 * int
 * main(int argc, char **argv)
 * {
 * 	register SVCXPRT *transp;
 * 
 * 	(void) pmap_unset(EVTSVC_INP, IVER);
 * 
 * 	transp = svcudp_create(RPC_ANYSOCK);
 * 	if (transp == NULL) {
 * 		fprintf(stderr, "cannot create udp service.");
 * 		exit(1);
 * 	}
 * 	if (!svc_register(transp, EVTSVC_INP, IVER, evtsvc_inp_1, IPPROTO_UDP)) {
 * 		fprintf(stderr, "unable to register (EVTSVC_INP, IVER, udp).");
 * 		exit(1);
 * 	}
 * 
 * 	transp = svctcp_create(RPC_ANYSOCK, 0, 0);
 * 	if (transp == NULL) {
 * 		fprintf(stderr, "cannot create tcp service.");
 * 		exit(1);
 * 	}
 * 	if (!svc_register(transp, EVTSVC_INP, IVER, evtsvc_inp_1, IPPROTO_TCP)) {
 * 		fprintf(stderr, "unable to register (EVTSVC_INP, IVER, tcp).");
 * 		exit(1);
 * 	}
 * 
 * 	svc_run();
 * 	fprintf(stderr, "svc_run returned");
 * 	exit(1);
 * 
 * }
 *
 *
 *
 *
 *  printf ("          program           version    proto          port\n");
 *
 *  while (z_out) {
 *    svt->program = (z_out)->pml_map.pm_prog;
 *
 *    printf ("\t%10d\t%10d", 
 *	    (struct pmaplist *)(z_out)->pml_map.pm_prog,
 *	    (struct pmaplist *)(z_out)->pml_map.pm_vers);
 *
 *    if ((z_out)->pml_map.pm_prot == TCP)
 *      printf ("\ttcp");
 *    else if ((z_out)->pml_map.pm_prot == UDP)
 *      printf ("\tudp");
 *    else
 *      printf ("\tunknown");
 *    
 *    printf ("\t%10d\n",
 *	    (struct pmaplist *)(z_out)->pml_map.pm_port);
 *
 *    if (((struct pmaplist *)z_out)->pml_next != 0) 
 *      z_out = ((struct pmaplist *)z_out)->pml_next;
 *    else
 *      break;
 *  }
 *
 * 
 */
