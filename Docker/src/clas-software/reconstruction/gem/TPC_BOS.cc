// TPC_BOS.cc: offline version .
// v1.0 Jixie Zhang, Bonus Exp
// This is bos-read subroutine for user_ana
// supposing the bos file is opened.
//////////////////////////////////////////////////////////////////////

/*------------------------ tpc -----------------
//typedef unsigned int uint16;
//this part has been declared in bosddl.h
typedef struct {
  uint16 id;    //  the address of the hit detector element 
  uint16 tdc;   //  tdc information (channels) 
  uint16 adc;   //  adc information (channels) 
} tpc_t;

---------------------- END tpc -----------------*/
#include "TPC_BOS.h"

//#define TPC_BOS_DEBUG 1
  //////////////////////////////////////////////////
  vector<tpc_t> raw_data;
  int           imTrigType; 
  //////////////////////////////////////////////////
  int  ReadTPCBank();
  int  ReadTGBIBank();
  int  ReadOneTPCEvent(); 
//////////////////////////////////////////////////////////////////////

/************************************************************************/
int ReadOneTPCEvent()
{//keep reading until find some TPC bank
  char *function="ReadOneTPCEvent";
  int status=0;
   
  //keep reading until find some TPC bank        
  while (status<=0)
  {
   status=ReadTPCBank();
#ifdef TPC_BOS_DEBUG     
   if(!status) printf("%s:NO TPC bank data in this event, try again...\n",function);
#endif
  }
  return status;
}
/************************************************************************/

/************************************************************************/
int ReadTPCBank()
{
  char* function="ReadTPCBank";
  char* bankname="TPC ";
  int i=0,index=0,status=0;
  int iRow=0;
  clasTPC_t* TPC = NULL; 
  //clear the vector for new event
  raw_data.clear();
  for(index=2;index>=0;index--)
  {
    TPC=NULL;
    TPC = (clasTPC_t*) getGroup(&bcs_, bankname, index);
    if ( TPC )
    { 
      iRow = TPC->bank.nrow;
      status+=iRow;
#ifdef TPC_BOS_DEBUG     
      printf("TPC bank %d contains data rows = %d\n",index,iRow);
#endif
    
      //Fill to value into Global Vector raw_data
      for (i=0;i<iRow;i++)
      {
	if(index==2) TPC->tpc[i].id+=1664;
	raw_data.push_back(TPC->tpc[i]);
#ifdef TPC_BOS_DEBUG
      if (i<10)	
      printf("row%2d chan=%5d tdc=%5d adc=%5d\n",i,TPC->tpc[i].id,TPC->tpc[i].tdc,TPC->tpc[i].adc);
#endif	 
      }

    }
  }
#ifdef TPC_BOS_DEBUG	 
  printf("%s return=%d\n",function,status);
#endif
  imTrigType=0;
  imTrigType=ReadTGBIBank();
  return status;
}

/**********************************************************************/

/**********************************************************************/
// read the trigger type, read the first word of TGBI bank and extract the
// the first 10 bit
// name int ReadTGBIBank()
//output: 0-9
//    0-5: which sector the electron trigger came from
//    6-7: regular trigger with different threshold (need to check 
//         the trigger config file)
//    9:   pulse trigger
//    negative value: someting wrong
int ReadTGBIBank()
{
    char * function="ReadTGBIBank";
    int i=0, status=0;
    int iRow=0;
    int key=0;
    char *bankname="TGBI";

    clasTGBI_t* TGBI = (clasTGBI_t*) getBank(&bcs_, bankname);    
    if (TGBI) 
    {	      
      iRow = TGBI->bank.nrow;

#ifdef TPC_BOS_DEBUG	  
      for (i=0;i<iRow;i++)
      {
	printf("ROW%5d: ",i); 
	printf("   latch1          = %d ", TGBI->tgbi[i].latch1 );
	printf("   interupt_time   = %d ", TGBI->tgbi[i].interrupt_time );
	printf("   helicity_scaler = %d ", TGBI->tgbi[i].helicity_scaler );
	printf("   latch2          = %d ", TGBI->tgbi[i].latch2 );
	printf("   level3          = %d ", TGBI->tgbi[i].level3 );
	printf("\n");
      }
#endif
      //extract the first 10 bit 
      key = TGBI->tgbi->latch1;
      status= key; /* return the whole bit pattern, not just the bit number */
/*    for(i=0;i<10;i++) */
/* 	{ */
/* 	  cmp=(int)(pow(2,i)); */
/* 	  bit[i]=(int)(key & cmp); */
/* 	  if(bit[i]!=0) {status=i;bit[i]=1;} */
/* 	  // printf("The %d bit of %d is %d\n",i,key,bit[i]); */
/* 	}	    */
      
    }       
#ifdef TPC_BOS_DEBUG	 
    printf("%s return=%d\n",function,status);
#endif
    return status;
}

/**********************************************************************/
