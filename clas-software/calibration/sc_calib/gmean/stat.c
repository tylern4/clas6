//  A PROGRAM TO PUT OUT 2 FILES OF 288 ZEROS

#include <stdio.h>
#include <stdlib.h>

void  main()
{	

FILE *stat_left;
FILE *stat_right;
int k;    

stat_left  = fopen("left_counter.status",  "w");
stat_right = fopen("right_counter.status", "w");    

     for(k=1;k<343;k++){
     fprintf(stat_left, "0\n");
     fprintf(stat_right,"0\n");
     }     
fclose(stat_left);
fclose(stat_right);
     
     return;
}


