
#ifndef BOS_INCLUDED_
#define BOS_INCLUDED_

extern "C" 
{
#include <ntypes.h>
#include <bostypes.h>
#include <bosddl.h>
#include "map_manager.h"
/*--------------- function prototypes ----------------*/
void initbos();
int  getBOS(BOSbank *bcs, int lun, char *list);
void cleanBanks(BOSbank *bcs);
void dropAllBanks(BOSbank *bcs, char *list);
void *getBank(BOSbank *,const char *);
void open_fpack_unit(char *filename,char *dataname,int unitnum);
void close_fpack_unit(char *dataname);
extern BOSbank bcs_;
}

#endif


