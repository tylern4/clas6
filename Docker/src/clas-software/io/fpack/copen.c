/*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko*/
/*-- Author :    Zbigniew Szkutnik   16/12/94*/
#ifndef MAXNAMELEN
#define MAXNAMELEN 257
#endif
/*KEEP,IOINCL.*/
#include "ioincl.h"
/*KEEP,SHIFTIN.*/
#include "shiftin.h"
/*KEND.*/
#include <string.h>
 
   /* open file on request from FORTRAN
      ================================= */
 
/* this is declared "extern" in IOINCL
   and only here is it declared for real. */
FILE *filepointer[NUNITS];
 
void copen_(lunptr, name, mode, medium, blksize, filesize, bufptr, ret,
            namelen, modelen)
int   *lunptr, *medium, *blksize, *filesize, *ret;
long  *bufptr;
char  *name, *mode;
short *namelen, *modelen;
 
{  char  filename[MAXNAMELEN];
   char  options[MAXNAMELEN];
   char *sp, *bufio;
   long  buflen;
   FILE *file;
#if defined(IRIX)
   struct mtop top;
#endif
   if( *namelen >= MAXNAMELEN )
        {fprintf(stderr,"Filename too long\n");
         *ret = -1;
         return;
        }
   strncpy(filename,name,*namelen);
   filename[*namelen] = '\0';
 
   if( *modelen >= MAXNAMELEN )
        {fprintf(stderr,"Options too long\n");
         *ret = -1;
         return;
        }
   strncpy(options,mode,*modelen);
   options[*modelen] = '\0';
   *ret = 0;
   *bufptr = 0;
 
   file = fopen(filename, options);
   if(file == NULL)
      {*ret = errno;
       perror("open");
       return;
      }
 
   if(*medium == 1)
     {/* allocate the I/O buffer for tapes */
#if defined(IRIX)
      buflen = *blksize + 8;
      /* on SGI one needs a buffer longer by exactly 8 bytes
         then blksize !!! */
#else
      buflen = *blksize;
#endif
      if((bufio = (char *)malloc(buflen)) == (char *)NULL)
        {fprintf(stderr,"copen: Cannot allocate IO buffer \n.");
         fclose(file);
         *ret = -1;
         return;
        }
      *bufptr = (long)bufio;
#if defined(IRIX)
      top.mt_op =  MTSCSI_SETFIXED;
      top.mt_count = *blksize;
      ioctl(fileno(file), MTSPECOP, &top);
      /* On SGI the tape file must be closed and reopened after
         changing the blocksize. Otherwise, the first I/O fails!   */
      fclose(file);
      file = fopen(filename, options);
      if(file == NULL)
        { *ret = errno;
          return;
        }
#endif
      setvbuf(file,bufio,_IOFBF,buflen);
     }
 
/*
   Now we have a file pointer to store in the array filepointer. Look
   for an empty slot.
*/
   for( *lunptr = 0; filepointer[*lunptr] != (FILE *) NULL; (*lunptr)++)
     ;
   if( *lunptr >= NUNITS )
   {
     *lunptr = -99;
     *ret = -1;
     fprintf(stderr,"copen: no empty slot for filepointer found.\n");
     fflush(stderr);
     return;
   }
   filepointer[*lunptr] = file;
   return;
}
 
