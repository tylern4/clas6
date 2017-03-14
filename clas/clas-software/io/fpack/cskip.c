/*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko*/
/*-- Author :    Zbigniew Szkutnik   16/12/94*/
/*KEEP,IOINCL.*/
#include "ioincl.h"
/*KEEP,SHIFTIN.*/
#include "shiftin.h"
/*KEND.*/
#include <unistd.h>
 
   /* skip words on request from FORTRAN
      ================================== */
void cskip_(lunptr, nwords, ret)
int *lunptr, *nwords, *ret;
 
{ *ret = 0;
  if( -1 == lseek( fileno(filepointer[*lunptr]),
                                   *nwords * sizeof(int), SEEK_CUR) )
      *ret = -1;
  fflush(filepointer[*lunptr]);
  return;
}
 
