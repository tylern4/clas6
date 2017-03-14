/*CMZ :  1.00/08 04/09/95  11.38.05  by  Wiesiek Dlugosz*/
/*CMZ :  1.00/07 02/09/95  16.03.29  by  Wiesiek Dlugosz*/
/*CMZ :  1.00/03 15/02/95  10.16.53  by  Unknown*/
/*CMZ :  1.00/02 15/02/95  09.33.12  by  Unknown*/
/*CMZ :  1.00/01 16/01/95  16.03.03  by  Unknown*/
/*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko*/
/*-- Author :    Zbigniew Szkutnik   16/12/94*/
/*KEEP,IOINCL.*/
#include "ioincl.h"
/*KEEP,SHIFTIN.*/
#include "shiftin.h"
/*KEND.*/
 
   /* read from a file on request from FORTRAN
      ======================================== */
 
void cget_(lunptr, ircn, medium, ntot, buffer, ret)
int *lunptr, *ircn, *medium, *ntot, *buffer, *ret;
 
{ size_t nbytes;
 
  nbytes = *ntot * sizeof(int);
  if( *ircn > 0 )
     { long offset, current, request;
       int noff;
       int whence;
 
       request = (*ircn - 1) * nbytes;
       current = ftell( filepointer[*lunptr] );
       offset = request - current;
       whence = SEEK_CUR;
       if( current < 0 || offset < 0)
          { whence = SEEK_SET;
            offset = request;
          }
       if( fseek(filepointer[*lunptr], offset, whence) != 0 )
          { perror("cget"); clearerr(filepointer[*lunptr]);
            fprintf(stderr,"cget. Cannot set record %d\n",*ircn);
            *ret = 1;
            return;
          }
     }
  *ret = 0;
    if( nbytes == fread(buffer, 1, nbytes, filepointer[*lunptr]) )
      return;
    if( feof( filepointer[*lunptr] ) )
      {*ret = -1;
       return;
    }
    if( ferror( filepointer[*lunptr] ) )
      {*ret = 1;
       perror("cget");
       clearerr( filepointer[*lunptr] );
       return;
    }
}
