/*
 * mtime.c
*/

static const char sccsid[] = "@(#)"__FILE__"\t$Revision: 1.2 $\tCreated $Date: 2007/05/28 19:20:28 $ , \tcompiled "__DATE__;

#include <stdio.h>
#include <unistd.h>
#include <limits.h>
#include <time.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/types.h>

unsigned long mtime(void)  /* current time in milliseconds */
{
  struct timeval tv;
  gettimeofday(&tv,NULL);
  return tv.tv_sec*1000+tv.tv_usec/1000;
}

unsigned long cpumtime(void) /* user CPU time in milliseconds */
{
  static long clk_tck = 0;
  static struct tms  tms;

  if (clk_tck==0)
    {
      clk_tck = CLOCKS_PER_SEC;  /* CLOCKS_PER_SEC is a macro the resolves into a system call,
			     see 'limits.h' */
    }

  times(&tms);

  return (tms.tms_utime * 1000 / clk_tck);
}

double dtime(void)  /* current time in seconds expressed as a double */
{
  struct timeval tv;
  gettimeofday(&tv,NULL);
  return (tv.tv_sec + tv.tv_usec/1000000.0);
}

/* endfile */
