/* 
   map_conv_Fstring.c
   Created: 10/7/97 Joe Manak
   Fix buggy Fortran routnes*/

/* Fixed this buggy routine (no return statement, therefore function type has to be `void'.
   This routine is very system specific and does not work with modern commercial compilers and operating systems.
   Whether one calls the strings as defined in Fortran and other languages 'buggy', or not, may also depend on the experience of
   person. In fact buffer overflow bugs in C are often caused by the lack of the length information. In fact this routine does
   not check for buffer overflows. If the buffer C_string is smaller than F_strlen+1, then we get a buffer overflow. Cheers!
   2002-11-21, HGJ */

#include <map_manager.h>
#include <map_internal.h>
#include <string.h>

/*converts a Fortran-like string to a C like string*/
void map_conv_Fstring(char *C_string, char *F_string, int F_strlen){
  int position;

  strncpy(C_string, F_string, F_strlen);
  C_string[F_strlen] = '\0';
  for( position = F_strlen-1 ; C_string[position] == ' '; position--)
    C_string[position] = '\0'; 
  

}
