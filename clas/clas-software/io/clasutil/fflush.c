/* 
*  flushes all io streams
*
*  EJW, 2-May-96
*
*/

#include <stdio.h>

#define _POSIX_SOURCE 1
#define __EXTENSIONS__


void
fflush_(void)
{
  /*fflush();*/
  fflush(stdout);
  fflush(stderr);
}

void
fflushout_()
{
  fflush(stdout);
}

void
fflusherr_()
{
  fflush(stderr);
}

void
cprint_(char *text, int len)
{
  int i;

  for(i=0; i<len; i++) printf("%c",text[i]);
  printf("\n");
  fflush(stdout);  
}

/*-----------------------------------------------------------------------*/





