/*
     this neat little code will return the hostname from whence it is run !
     Note this is a fortran wrapper that calls a Unix fcn.

     Interface:
       inputs: fstr = a fortran string.
               ret  = a fortran integer.
	       l1   = passed by the compiler, length of the fortran string.

     JAM - 1/3/1996 - improve this.
                      return:
		         0 = fcn ok.
			 1 = input string too short
			 2 = fcn failed need perror info.
*/

#include <stdio.h>
#include <strings.h>
#include <unistd.h>
#include <errno.h>
#define  MAX_LEN 1024

extern int errno;

void gethostname_ (fstr, ret, l1)
  char *fstr;
  int *ret;
  int *l1;
{
  int jh, l2, i;
  char buff[MAX_LEN], *ep;
  
  ret = 0;
  jh = gethostname (buff, MAX_LEN);
  if (jh != 0) {
    ep = strerror (errno);
    *ret = 2 ;
    if (*l1 > (strlen(ep))) 
      strncpy (fstr, ep, *l1-1);
    return;
  }
  
  l2 = strlen (buff);
  if (*l1 <= l2) {
    *ret = 1;
    return;
  }
  for (i=0; i<*l1; i++)
    fstr[i] = ' ';

  for (i=0; i<l2; i++)
    fstr[i] = buff[i];

  return ;
}
