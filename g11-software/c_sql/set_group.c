#include <stdio.h>
#include <stdlib.h>
#include "csql.h"


void  set_group(char *grpname, char *buffer){
int    gindx;
int    cindx;
int   *iptr;
int    ivalue;
float *fptr;
float  fvalue;

gindx = find_group_byname(grpname);
if(gindx<0||gindx>=MAX_GROUPS) return;

 if(CSQL_Groups[gindx].ncolumns<1){
   printf("SET_GROUP: Group [%s] has no columns defined.\n",grpname);
   return;
 }

 iptr = (int   *) buffer;
 fptr = (float *) buffer;

 for(cindx = 0; cindx < CSQL_Groups[gindx].ncolumns; cindx++){
   if(CSQL_Groups[gindx].fColumns[cindx].itype == COL_TYPE_INT){
     ivalue = *iptr;
     set_column_int(grpname,CSQL_Groups[gindx].fColumns[cindx].name, ivalue);
   } else {
     fvalue = *fptr;
     set_column_float(grpname,CSQL_Groups[gindx].fColumns[cindx].name,fvalue);
   }
   fptr++;
   iptr++;
 }

}
/*
void set_group_(char *grpname, int *buffer){
char  *ch_buff;
char   nextgroup[5];
ch_buff = (char *) buffer;
strncpy(nextgroup,grpname,4); nextgroup[4] = '\0';
set_group(nextgroup,ch_buff);
}
*/
