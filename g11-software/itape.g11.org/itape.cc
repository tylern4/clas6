/*
 * itape.cc   
 * Purpose: convert bos banks to itape format
 */


extern "C" {
  
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <clas_cern.h>
#include <particleType.h>
#include <kinematics.h>
#include <pdgutil.h>
#include <pid.h>
#include <scalers.h>
#include <utility.h>
#include <printBOS.h>
#include <ec.h>
#include <PartUtil.h>
#include <dataIO.h>
void *data_addGroup(void *buffer,int bufsize,int type,int length);

	   }


#include <iostream.h>
#include <itape.h>

#define BASE 1000


int GroupSize(int base)
{
  int ret;
  base /= BASE;
  switch (base) {
  case 800:
    ret = sizeof(part_t)/sizeof(int);
    break;
  case 100:
    ret = sizeof(dc0_t)/sizeof(int);
    break;
  default:
    ret = 1;
  }
  return(ret);
}



int GroupNum(char *name,int sec)
{
  int ibase;
  if (strcmp(name,"HEAD") == 0) {
    ibase = 1;
  }
  else if (strcmp(name,"DC0 ") == 0) {
    ibase = 100;
  }
  else if (strcmp(name,"PART") == 0) {
    ibase = 800;
  }
  else
    ibase = 0;

  return(ibase * BASE + sec);
}



int fillHeader(itape_header_t *buffer)
{
  int ret;
 clasHEAD_t *HEAD = (clasHEAD_t *) getBank(&bcs_,"HEAD");
  if (HEAD) {
    buffer->runNo  = HEAD->head[0].nrun;
    buffer->spillNo = HEAD->head[0].evtclass;
    buffer->eventNo = HEAD->head[0].nevent;
    buffer->time = HEAD->head[0].time;
    buffer->trigger = HEAD->head[0].type;
    buffer->latch = HEAD->head[0].roc;
    ret = 1;
  }
  else 
    ret = 0;
  return(ret);
    
}
int addGroup(itape_header_t *buffer,int bufsize,char *bank)
{
  int type;
  int size;
  int ret = 0;
  void *ptr;
  bankHeader_t  *BANK = (bankHeader_t *)getBank(&bcs_,bank); 
  if (BANK) {
    type = GroupNum(bank,BANK->nr);
    size = BANK->nwords * sizeof(int) + sizeof(bankHeader_t);
    cerr << type << "\t" << size << "\t" << 
      BANK->nrow << "\t" << BANK->ncol << endl;
    ptr = data_addGroup((void *)buffer,bufsize,type,size);
    memcpy(ptr,BANK,size);
    ret = 1;
  }
  return(ret);

}
int addGroups(itape_header_t *buffer,int bufsize,char *bank)
{
  int type;
  int size;
  int ret = 0;
  void *ptr;
  bankHeader_t  *BANK = (bankHeader_t *)getBank(&bcs_,bank); 
  if (BANK) {

    do {
    type = GroupNum(bank,BANK->nr);
    size = BANK->nwords * sizeof(int) + sizeof(bankHeader_t);
    cerr << bank << " " << type << "\t" << size << "\t" << 
      BANK->nrow << "\t" << BANK->ncol << endl;
    ptr = (void *) data_addGroup((void *)buffer,bufsize,type,size);
    memcpy(ptr,BANK,size);
    }
    while ((BANK = (bankHeader_t *) getNextBank(&bcs_,BANK)));
    ret = 1;
  }
  return(ret);

}

char *GroupName(int group,int *sec)
{
  static char ret[5];
  int ibase = group/BASE;
  *sec = group % BASE;
  switch (ibase) {
  case 1:
    strcpy(ret,"HEAD");
    break;
  case 800:
    strcpy(ret,"PART");
    break;
  case 100:
    strcpy(ret,"DC0 ");
    break;
  default:
    strcpy(ret,"UNKN");
    break;
  }
  return(ret);
}
    
int itape2bos(itape_header_t *buffer,BOSbank *bcs,int verbose)
{
  clasHEAD_t *hptr;
  char Elist[1024];

  int ngroups,*groups,*size;
  char name[5];
  int sec;
  char *ptr,*ptrold;
  group_header_t *gheader;
  bankHeader_t bankH;
  data_listGroups(buffer,&ngroups,&groups,&size);
  Elist[0] = '\0';
  for(int i = 0; i < ngroups; ++i) {
    strcpy(name,GroupName(groups[i],&sec));
    if (verbose) 
      cerr << i << "\t" << groups[i] << "\t" << size[i] << "\t" << name << "\t" << sec << " ";
    gheader = (group_header_t *)data_getGroupHeader(buffer,groups[i]);
    ptrold = (char *)data_getGroup(buffer,groups[i]);
    memcpy(&bankH,ptrold,sizeof(bankHeader_t));
    if (verbose)
      cerr << gheader->length << "\t" << gheader->type << endl;
    ptr = (char *)makeBank(bcs_,name,sec,bankH.ncol,bankH.nrow);
    ptr += sizeof(bankHeader_t);
    if (verbose)
      cerr << bankH.name << "\t" << bankH.ncol << "\t" << bankH.nrow << "\t" << bankH.nr << "\t" << bankH.nwords << endl;
    ptrold += sizeof(bankHeader_t);
    memcpy(ptr,ptrold,gheader->length - sizeof(bankHeader_t));
    strcat(Elist,name);
  }
  free(groups);
  free(size);

  bankList(bcs_,"E=",Elist);
  if (verbose)
    cerr << "E= " << Elist << endl;

  return(1);
} 
  
