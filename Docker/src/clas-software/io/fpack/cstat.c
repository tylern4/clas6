/*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko*/
/*-- Author :    Pavel Binko   16/12/94*/
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
 
#ifndef MAXNAMELEN
#define MAXNAMELEN 257
#endif
 
   /* The routine returns the size of the file
      ======================================== */
 
void cstat_(name, namelen, filesize, ret)
char  *name;
int   *filesize, *ret;
short *namelen;
 
{  char   filename[MAXNAMELEN];
   struct stat filestat;
   int retstat;
 
   if( *namelen >= MAXNAMELEN )
        {fprintf(stderr,"cstat. Filename too long\n");
         *ret = -1;
         return;
        }
   strncpy(filename,name,*namelen);
   filename[*namelen] = '\0';
 
   retstat = stat( filename, &filestat);
   if( retstat != 0 )
     {fprintf(stderr, "cstat. stat return code = %d\n", retstat);
      *ret = -1;
      return;
     }
 
   *filesize = filestat.st_size;
 
   *ret = 0;
   return;
}
 
