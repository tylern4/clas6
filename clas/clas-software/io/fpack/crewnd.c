#ifdef AIX
/*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko*/
/*-- Author :    Zbigniew Szkutnik   16/12/94*/
/*KEEP,IOINCL.*/
#include "ioincl.h"
/*KEEP,SHIFTIN.*/
#include "shiftin.h"
/*KEND.*/
#include <unistd.h>
 
   /* rewind a file on request from FORTRAN
      ===================================== */
 
void crewnd_(lunptr, medium)
int *lunptr, *medium;
 
{
 struct stop top;
 FILE  *fp;
 if(*medium == 1)
  {/* for tapes rewind=backspace */
   top.st_op = STRSR;
   top.st_count = 1;
   fp = filepointer[*lunptr];
   if(ioctl(fileno(fp),STIOCTOP,(void *)&top))
     {fprintf(stderr,"Cannot backspace tape \n");
      fflush(stderr);
     }
   fflush(filepointer[*lunptr]);  /* necessary to discard the unread but
                                 already buffered data!!! */
  }
 else
   rewind( filepointer[*lunptr] );
 return;
}
#endif
#ifdef HPUX
/*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko*/
/*-- Author :    Zbigniew Szkutnik   16/12/94*/
/*KEEP,IOINCL.*/
#include "ioincl.h"
/*KEEP,SHIFTIN.*/
#include "shiftin.h"
/*KEND.*/
#include <unistd.h>

   /* rewind a file on request from FORTRAN
      ===================================== */

void crewnd_(lunptr, medium)
int *lunptr, *medium;

{
 struct mtop top;
 FILE  *fp;
 if(*medium == 1)
  {/* for tapes rewind=backspace */
   top.mt_op = MTBSR;
   top.mt_count = 1;
   fp = filepointer[*lunptr];
   if(ioctl(fileno(fp),MTIOCTOP,(void *)&top))
     {fprintf(stderr,"Cannot backspace tape \n");
      fflush(stderr);
     }
   fflush(filepointer[*lunptr]);  /* necessary to discard the unread but
                                 already buffered data!!! */
  }
 else
   rewind( filepointer[*lunptr] );
 return;
}
#endif
#ifdef Linux
/*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko*/
/*-- Author :    Zbigniew Szkutnik   16/12/94*/
/*KEEP,IOINCL.*/
#include "ioincl.h"
/*KEEP,SHIFTIN.*/
#include "shiftin.h"
/*KEND.*/
#include <unistd.h>

   /* rewind a file on request from FORTRAN
      ===================================== */

void crewnd_(lunptr, medium)
int *lunptr, *medium;

{
 FILE  *fp;
 if(*medium == 1)
  {/* for tapes rewind=backspace */
   if( fseek( filepointer[*lunptr], 0L, SEEK_SET ) )
   { fprintf(stderr,"Cannot backspace tape \n");
     fflush(stderr);
   }
   fflush(filepointer[*lunptr]);  /* necessary to discard the unread but
                                 already buffered data!!! */
  }
 else
   rewind( filepointer[*lunptr] );
 return;
}
#endif

#if defined(SunOS) || defined(IRIX)
/*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko*/
/*-- Author :    Zbigniew Szkutnik   16/12/94*/
/*KEEP,IOINCL.*/
#include "ioincl.h"
/*KEEP,SHIFTIN.*/
#include "shiftin.h"
/*KEND.*/
#include <unistd.h>
 
   /* rewind a file on request from FORTRAN
      ===================================== */
 
void crewnd_(lunptr, medium)
int *lunptr, *medium;
 
{
 struct mtop top;
 FILE  *fp;
 if(*medium == 1)
  {/* for tapes rewind=backspace */
   top.mt_op = MTBSR;
   top.mt_count = 1;
   fp = filepointer[*lunptr];
   if(ioctl(fileno(fp),MTIOCTOP,(void *)&top))
     {fprintf(stderr,"Cannot backspace tape \n");
      fflush(stderr);
     }
   fflush(filepointer[*lunptr]);  /* necessary to discard the unread but
                                 already buffered data!!! */
  }
 else
   rewind( filepointer[*lunptr] );
 return;
}
#endif
