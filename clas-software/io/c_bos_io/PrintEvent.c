#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <printBOS.h>

#define CHECK_PRINT(QUOTE, printNAMEbank)  check_and_print(QUOTE,(void (*) (FILE *, void*))printNAMEbank, ngroups, groups, nxgroups, xgroups)
int check_and_print(char *name, void (*printfunction)(FILE *, void *), int ngroups, char **groups, int nxgroups, char **xgroups);
int printbanks(char *name, void (*printfunction) (FILE *, void *));

int checkgroup(int ngroups,char **groups,char *grp)
{
  int i;
  int ret = 0;
  for (i = 0;i < ngroups; ++i) {
    if (strcmp(groups[i],grp) == 0) {
      ret = 1;
      break;
    }
  }
  return(ngroups ? ret : 1);
}

     

int PrintEvent(int ngroups,char **groups,int nxgroups,char **xgroups)
{
  clasHEAD_t *header = NULL;
  

 if((header = (clasHEAD_t*)getBank(&bcs_, "HEAD")) && checkgroup(ngroups,groups,"HEAD") && (!nxgroups || !checkgroup(nxgroups,xgroups,"HEAD"))){
    pBankHead(stdout, &header->bank); 
    pHEAD(stdout,header);
    fprintf(stdout,"\n");
  }


#include "check_print.h"


  return(1);
}


int check_and_print(char *name, void (*printfunction) (FILE *, void *), int ngroups, char **groups, int nxgroups, char **xgroups){ 
  if (checkgroup(ngroups,groups,name) && (!nxgroups || !checkgroup(nxgroups,xgroups,name))){
    printbanks(name, printfunction);
  }
  return 0;
}

int printbanks(char *name, void (*printfunction) (FILE *, void *)){
  void *bank = NULL;
  int ret = 0;

  if( (bank = getBank(&bcs_, name)) ){
    (*printfunction)(stdout, bank);
    while( (bank = getNextBank(&bcs_,(bankHeader_t *)bank)) )
      (*printfunction)(stdout, bank);
    ret = 1;
  } 
  return ret;
}
     
 

