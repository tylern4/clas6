#ifndef Linux
/*CMZ :  1.00/16 30/03/96  16.44.06  by  Wiesiek Dlugosz*/
/*CMZU:  1.00/14 08/11/95  14.37.25  by  Dave Sankey at RAL*/
#endif /* not Linux */
/*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko*/
/*-- Author :    Zbigniew Szkutnik   16/12/94*/
 
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#ifndef Linux
#include <sys/types.h>
#include <sys/socket.h>
#else /* Linux */

#endif /* Linux */
  /* Return the current host name in chost and its length in clen.
     (-1 if error) */
 
#define MAXLEN 64
 
/* void fpgthc_(char *chost, int *clen, short *len) */
void fpgthc_(chost, clen, len)
char  *chost;
int   *clen;
short *len;
{
 char myname[MAXLEN+1];
 int  count;
 
   *clen = -1;
   if( gethostname(myname, MAXLEN+1) )
     {fprintf(stderr,"fpgthc. Cannot get host name.\n");
      perror("gethostname");
      return;
     }
#ifndef Linux
   if( (count = strlen(myname)) > *len) strtok(myname, ".");
#endif /* not Linux */
   if( (count = strlen(myname)) > *len)
     {fprintf(stderr,"fpgthc. Local hostname length greater then %d\n", *len);
      fflush(stderr);
      return;
     }
   strncpy(chost, myname, *len);
   *clen = count;
}
 
