#include <stdio.h>
#include <ntypes.h>
#include <bostypes.h>
#include <printBOS.h>

void user_evnt_c_();

void user_evnt_c_(){
  int i;
  int id = 1000, weight = 1;
  clasHEAD_t *header = NULL;
  clasDC0_t *DC0 = NULL;

  /*dummy C user routine for RECSIS*/
  /*The idea is to be able to use the c_bos_io library of routines
    to access BOS banks, an example of the use of the c_bos_io library
    is given below*/

  /*access the contents of the HEAD bank and all of the dc0 banks*/

  /*get the HEAD bank*/
  if(header = (clasHEAD_t *)getBank(&bcs_, "HEAD")){
    /*print the contents*/
    /*fprintf(stdout,"\nVersion:\t%d\nRun:\t\t%d\nEvent:\t\t%d\nType:\t\t%d\nROC:\t\t%d\nCLASS:\t\t%d\nWeight:\t\t%d\nTIME:\t\t%s\n",header->head[0].version,header->head[0].nrun,header->head[0].nevent,header->head[0].type,header->head[0].roc,header->head[0].class,header->head[0].weight,ctime((const long *) &(header->head[0].time)));
     */
    /*fill a booked histogram - hopefully a library of hbook call wrappers
     will be available shortly*/
    /*hf1_(&id, &(header->head[0].nrun), &weight); */
  }

  /*get the first DC0 bank*/
  if(DC0 = (clasDC0_t *)getBank(&bcs_,"DC0 ")){
    /*do something, ex: print the entire contents */
    /*for (i = 0; i < DC0->bank.nrow; ++i){
      fprintf(stdout,"id:\t%5hd\ttdc:\t%5hd\n", DC0->dc0[i].id, DC0->dc0[i].tdc); 
    }*/
    /*then get the next DC0 bank until there are no more left*/
    /*while(DC0 = (clasDC0_t *)getNextBank(&bcs_,(bankHeader_t *)DC0)){*/
      /*do something again, ex: print the contents*/
        /*for (i = 0; i < DC0->bank.nrow; ++i){*/
            /*fprintf(stdout,"id:\t%5hd\ttdc:\t%5hd\n", DC0->dc0[i].id, DC0->dc0[i].tdc); */
    /* }
    }*/
  }

}








