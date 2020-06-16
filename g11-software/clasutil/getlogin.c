/* 
 *  Purpose: Return the user login name.
 *
 *  inputs:
 *  fstr = a fortran string
 *  ret  = a fortran integer
 *  flen = passed by the compiler, the length of the fortran string.
 *apf flen = the maximum length of the user name
 *
 *  Returns: 
 *  0 = everything worked
 *  1 = the unix call failed.
 *  2 = the string u passed is too short.
 *
 */
#include <unistd.h>
#include <errno.h>

extern int errno;

void getlogin_ (fstr, ret, flen)
char *fstr;
int  *ret;
int  *flen;
{
  char *p;
  int l;

  *ret = 0;
  p = getlogin () ;
  if (p == 0)
    *ret = 1;
  l = strlen (p);
  if (*flen > l-1)
      strncpy (fstr, p, l);
  else
      *ret = 2;
      *flen = l;
}

