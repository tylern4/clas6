#ifndef Linux
/*CMZ :  1.00/16 13/03/96  10.44.45  by  Unknown*/
#endif /* not Linux */
/*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko*/
/*-- Author :    Zbigniew Szkutnik   16/12/94*/
 
/*KEEP,IOINCL.*/
#include "ioincl.h"
/*KEND.*/
#include <unistd.h>
 
   /* release a lock on a file
      ======================== */
 
void xunlock_(lunptr, ret)
int *lunptr, *ret;
 
{
 FILE  *fp;
 fp = filepointer[*lunptr];
#ifndef Linux
 lseek(fileno(fp), 0L, 0);
#endif /* not Linux */
 if(lockf(fileno(fp), F_ULOCK, 0) == -1)
   { *ret = 1;
     perror("UNLOCK");
   }
 else
   *ret = 0;
}
 
