#ifndef Linux
/*CMZ :  1.00/16 10/03/96  21.00.20  by  Unknown*/
/*-- Author :    Sergey Esenov   10/03/96*/
 
/*KEEP,IOINCL.*/
#include "ioincl.h"
/*KEND.*/
#include <unistd.h>
 
   /* release a lock on a file
      ======================== */
 
void xfsync_(lunptr, ret)
int *lunptr, *ret;
 
#else /* Linux */
void dummy_xfsync()
#endif /* Linux */
{
#ifndef Linux
 FILE  *fp;
 fp = filepointer[*lunptr];
 if(fsync(fileno(fp)) == -1)
   { *ret = 1;
     perror("FSYNC");
#endif /* not Linux */
   }
#ifndef Linux
 else
   *ret = 0;
}
 
#endif /* not Linux */
