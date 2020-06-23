/*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko*/
/*-- Author :    Zbigniew Szkutnik   16/12/94*/
 
#include <ctype.h>
/*KEEP,LOWCASE.*/
#include "lowcase.h"
/*KEND.*/
 
   /* convert a string to lower case
      ============================== */
int lcase(charptr)
char *charptr;
 
{   char *ptr;
 
    ptr = charptr;
    --ptr;
    while(*++ptr != '\0') *ptr = TOLOWER(*ptr);
    return(0);
}
 
