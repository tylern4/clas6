#include <stdio.h>
#include <stdlib.h>
#include "csql.h"


void print_group(char *grpname){
int  indx;
int  i;

indx = find_group_byname(grpname);

 if(indx == -1){
   printf("GROUP NOT FOUND [%s]\n",grpname);
   return;
 }

 printf("+-------------------------------------------------------------+\n");
 printf("| Group [%s] Columns | Column Type | TYPE |      Value      |\n",
	grpname);
 printf("+-------------------------------------------------------------+\n");
 for(i = 0; i < CSQL_Groups[indx].ncolumns; i++){
   printf("|%22s|%13s|%6d|%17s|\n",CSQL_Groups[indx].fColumns[i].name,
	  CSQL_Groups[indx].fColumns[i].type,CSQL_Groups[indx].fColumns[i].mode,
	  CSQL_Groups[indx].fColumns[i].value);
 } 
 printf("+-------------------------------------------------------------+\n");
 printf("| GROUP [%s] has %10d columns                         |\n",
	grpname,CSQL_Groups[indx].ncolumns); 
 printf("+-------------------------------------------------------------+\n");
}

void print_group_info(){
int  indx;


 printf("+-----------------------------------------+\n");
 printf("|   Group name  | Number of columns |     |\n");
 printf("+-----------------------------------------+\n");
 for(indx = 0; indx < MAX_GROUPS; indx++){
   if(CSQL_Groups[indx].isActive > 0){
     printf("|%14s |  %15d  | %3s |\n",CSQL_Groups[indx].name,CSQL_Groups[indx].ncolumns," ");
   }
 }
 printf("+-----------------------------------------+\n");

}

void print_all_groups()
{
  int indx;
  for(indx=0;indx<MAX_GROUPS;indx++){
    if(CSQL_Groups[indx].isActive>0){
      print_group(CSQL_Groups[indx].name);
    }
  }
}

void print_group_(char *grpname){
char nextgroup[5];
strncpy(nextgroup,grpname,4); nextgroup[4] = '\0';
print_group(nextgroup);
return;
}
