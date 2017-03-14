/*
 * nanCheck.c
 *
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>



static int haveCheckedNaN = 0;
static double NaN = 0;

int is_NaN(double v)
{
  if (!haveCheckedNaN)
    {
      haveCheckedNaN = 1;

      NaN = sqrt(-1.0);     /* an easy way to get a NaN */

      if (finite(NaN))       /* NaN == NaN should always fail
				see 'man math' for details
				*/
	{
	  fprintf(stderr,"is_NaN: The NaN check is broken. Aborting...\n");
	  abort();
	}      
    }

  return(!finite(v));
}
