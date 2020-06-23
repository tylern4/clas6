#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <math.h>
#include <bostypes.h>
#include <makebanks.h>

/* -=======================================================- *
$Id: get_hber_info.c,v 1.1 1998/04/17 21:09:16 mcnabb Exp $
$Author: mcnabb $
$Revision: 1.1 $
$Date: 1998/04/17 21:09:16 $
* -=======================================================- */

int get_hber_info(hber_t *hber,int slinfo[6], int *psec, int *ptrk_in_sect,int *ptrk_in_hbtr){         
  /*extracts info from layinfo1 and layinfo2 and puts the layer hit 
   info into an array of 6 characters (1 per super layer) and the sector and
   track number info into appropriate chars.
   note that the slinfo is stored as slinfo[superlayer-1] as per usual for 
   c code */

    int temp1,temp2,i;
    
    temp1 = hber->layinfo1;
    temp2 = hber->layinfo2;
    for(i=0;i<5;i++){
       slinfo[i] = temp1%(64);/*extract layinfo by superlayer*/
       temp1=temp1/64;
    }
    slinfo[5]=temp2%256;
    temp2 = temp2/256;
    *ptrk_in_sect = temp2%256;
    temp2 = temp2/256;
    *ptrk_in_hbtr = temp2%256;
    temp2 = temp2/256;
    *psec = temp2%256;
    return(1);
}
    










