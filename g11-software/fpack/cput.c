/*CMZU:  1.00/04 08/04/95  15.17.03  by  Wiesiek Dlugosz*/
/*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko*/
/*-- Author :    Zbigniew Szkutnik   16/12/94*/
/*KEEP,IOINCL.*/
#include "ioincl.h"
/*KEEP,SHIFTIN.*/
#include "shiftin.h"
/*KEND.*/
 
   /* write to a file on request from FORTRAN
      ======================================= */
 
void cput_(lunptr, ircn, ntot, buffer, ret)
int *lunptr, *ircn, *ntot, *buffer, *ret;
 
{  size_t nbytes;
#ifndef Linux
   int nflush;
#endif /* not Linux */
 
#ifndef Linux
/*   fprintf(stderr,"lunptr %d, file number %d, fileno %d\n",
         *lunptr,filepointer[*lunptr],fileno(filepointer[*lunptr]));
   fprintf(stderr,"ircn  %d, ntot %d\n",*ircn,*ntot); */
#endif /* not Linux */
   nbytes = *ntot * sizeof(int);
   if( *ircn > 0 )
      { long offset, current, request;
#ifndef Linux
        int noff;
#endif /* not Linux */
        int whence;
 
        request = (*ircn - 1) * nbytes;
#ifndef Linux
 
 
#endif /* not Linux */
        current = ftell(filepointer[*lunptr]);
 
        offset = request - current;
        whence = SEEK_CUR;
        if( current < 0 || offset < 0)
           {offset = request;
            whence = SEEK_SET;
           }
 
#ifdef Linux
/*
 *       if( current < 0 )
 *          { whence = SEEK_SET;
 *            offset = request;
 *          }
 *       else
 *          { whence = SEEK_CUR;
 *            offset = request - current;
 *          }
 */

#endif /* Linux */
        if( fseek(filepointer[*lunptr], offset, whence) != 0 )
           { perror("cput"); clearerr( filepointer[*lunptr] );
             fprintf(stderr,"cput. Cannot set record %d\n", *ircn);
             *ret = 1;
             return;
           }
      }
   *ret = 0;
   if(nbytes >  fwrite(buffer, 1, nbytes, filepointer[*lunptr]))
     { fprintf(stderr,"cput: Write error.\n");
       perror("cput");
       fprintf(stderr,"Write failue:\n\t# bytes\t%d\n\tUnit #\t%d\nfilepointer\t%x\n",nbytes,*lunptr,filepointer[*lunptr]);
       /* why calling this dummy routine from another package ? 
       BosWriteFail(buffer, 1, nbytes, filepointer[*lunptr]);
       */
       *ret = 1;
     }
#ifndef Linux
 
 
#endif /* not Linux */
   return;
}
 
