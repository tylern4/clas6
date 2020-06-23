/* 
*  duplicates fortran string, removes trailing blanks and inserts ending NULL 
*
*  EJW, 2-May-96
*
*/

#include <stdio.h>
#include <stdlib.h>

char *strdupfort(fortran_string,fortran_len)
     char *fortran_string;
     int fortran_len;
{
  char *s;
  int len_occ;

  /* find occupied length, not including trailing blanks */
  for(len_occ=fortran_len;(len_occ>0)&&(fortran_string[len_occ-1]==' ');len_occ--) ;

  s=(char *)malloc(len_occ+1);
  strncpy(s,fortran_string,(size_t)len_occ);
  s[len_occ]='\0';
  
  return(s);

}


/*-----------------------------------------------------------------------*/



