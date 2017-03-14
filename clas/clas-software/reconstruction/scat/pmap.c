

#include <stdio.h>
#include <stdlib.h>/* getenv, exit */
#include <string.h> /* strcmp */ 
#include <memory.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <signal.h>
#include <sys/utsname.h>
#include <rpc/rpc.h>
#include "j.h"
#include <rpc/pmap_prot.h>
#include <rpc/pmap_clnt.h>
#include <netdb.h>

#define TCP 6
#define UDP 17

typedef struct SVT {
  unsigned long       program, vers;
  int                 _trans;
  int                 *stat_result ;
  struct utsname      uts;
} SVT;

SVT svt;

/* -==========================================================- */
int main (argc, argv)
int argc;
char *argv[];
{
  struct sockaddr_in  pin;
  struct hostent      *hp;
  struct pmaplist     *z_out;
  char                buf[4096];
  register SVCXPRT    *transp;
  int                 done;

  if (argc < 2) {
    printf ("usage: pmap hostame\n");
    exit (0);
  }

  if ((uname (&svt.uts)) != 0)
    exit (0);

  printf ("system name : ...... %s\n", svt.uts.sysname);
  printf ("node name :......... %s\n", svt.uts.nodename);
  printf ("release : .......... %s\n", svt.uts.release);
  printf ("version : .......... %s\n", svt.uts.version);
  printf ("machine : .......... %s\n", svt.uts.machine);
/*
  printf ("domain name : ...... %s\n", svt.uts.domainname);
*/
  hp = gethostbyname (argv[1]);

  if (hp == 0) {
    sprintf (buf, "bad name : %s\n", argv[1]);
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
  printf ("          program           version    proto          port\n");
  while (z_out) {
    svt.program = (z_out)->pml_map.pm_prog;
    if ((z_out)->pml_map.pm_vers > 32) {
      printf ("\t%10d\t%10d", 
	      (struct pmaplist *)(z_out)->pml_map.pm_prog,
	      (struct pmaplist *)(z_out)->pml_map.pm_vers);
      
      if ((z_out)->pml_map.pm_prot == TCP)
	printf ("\ttcp");
      else if ((z_out)->pml_map.pm_prot == UDP)
	printf ("\tudp");
      else
	printf ("\tunknown");
      
      printf ("\t%10d\n",
	      (struct pmaplist *)(z_out)->pml_map.pm_port);
    }
    if (((struct pmaplist *)z_out)->pml_next != 0) 
      z_out = ((struct pmaplist *)z_out)->pml_next;
    else
      break;
  }
}
