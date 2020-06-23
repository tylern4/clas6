/*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko*/
/*-- Author :    Zbigniew Szkutnik   16/12/94*/
/*KEEP,IOINCL.*/
#include "ioincl.h"
/*KEEP,SHIFTIN.*/
#include "shiftin.h"
/*KEND.*/
 
   /* close a file on request from FORTRAN
      ==================================== */
 
void cclose_(lunptr, bufptr)
int  *lunptr;
long *bufptr;
 
{  fclose(filepointer[*lunptr]);
   filepointer[*lunptr] = (FILE *) NULL;
   if(*bufptr) free((char *)*bufptr);
   return;
}
