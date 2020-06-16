#include <stdio.h>

void uconvert_(int *num,int *unum)
{
  int  xx = *num;
  int  uxx = 0;
  int  sh;
  int j;
  /* Unneccesary complication */
  /*
  printf("number is %X\n",xx);
  for(j=0;j<32;j++){
	sh = 1;
	sh = sh << j;
	if(xx&sh!=0) uxx = uxx | sh; 
  }

  printf("XX = %X ,  UXX = %X\n",xx,uxx);
  */
  *unum = xx;
}
