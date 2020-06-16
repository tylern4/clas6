/*
 *  JAM - The Simple Communications Attachment Library.
 *
 *  Poll the sockets for activity
 *
 *  Revision history:
 *  JAM - 01/1996 - initial version
 *  JAM - 05/1996 - add in this banner.
 */
#include <stdio.h>
#include <math.h>
#include <rpc/rpc.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/resource.h>

static int it = 0;

#include <errno.h>


void Scat_Active ()
{
  fd_set         readfdset;
  extern int     errno;

#if defined(HPUX) || defined(IRIX) || defined(IRIX64)
  static size_t  tsize=0;
#endif
#ifdef Linux
  static size_t  tsize=0;
#endif

  struct rlimit  jsrl, *srl=&jsrl;

  int            counter=0;
  int            done=0;
  struct timeval timeout;

#ifdef Linux

  if (!tsize) 
    if ((getrlimit (RLIMIT_NOFILE, srl)) == 0)
      tsize = srl->rlim_cur;

#endif
#ifdef HPUX

  if (!tsize) 
    if ((getrlimit (RLIMIT_NOFILE, srl)) == 0)
      tsize = srl->rlim_cur;

#endif
#ifdef AIX_SOURCE
  static int tsize ;
  tsize = 60; /* aix does not offer this option. */

#endif
#ifdef SunOS
  static int tsize ;
  tsize = 60; /* aix does not offer this option. */

#endif

  while (done==0) {
    readfdset = svc_fdset;
    timeout.tv_usec = 0;
    timeout.tv_sec = 0;
    switch (select(tsize, &readfdset, (fd_set*)NULL, (fd_set*)NULL,
		   (struct timeval*)&timeout)) {
    case -1:
      if (errno ==  EBADF) {
	if (++counter > 10) 
	  exit (0);
	continue;
      }
    case 0:
      return;

    default:
      svc_getreqset(&readfdset);
    }
  }
}


void scat_active_ ()
{
  Scat_Active ();
}

