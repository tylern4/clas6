#ifndef _ISGOODPADDLE_H
#define _ISGOODPADDLE_H

#include<stdlib.h>

/* isGoodPaddle.h
  
   09/14/05  M. Bellis
  
   This snippet of code will take in the sector and TOF paddle for some
   track in CLAS for some given run and return a 1 if it is a good paddle and
   0 if it is a "bad" paddle that we would like to cut out of the analysis.
   if it fails the fiducial cut. 
  
   sector - sector in CLAS
   paddle - TOF paddle in CLAS
   run - integer to represent some run, or different cuts for a given run.
   0 - g1c
   1 - g11 - cutting out 23 in all sectors
   2 - g11 - cutting out 23 in only sector 5
  
*/

/*int isGoodPaddle(int sector, int paddle, int run);*/

extern "C"{

  int isGoodPaddle(int sector, int paddle, int run);

  int isgoodpaddle_(int *sector, int *paddle, int *run);

}

int isgoodpaddle_(int *sector, int *paddle, int *run){
  // fortran wrapper
  int iflag;
  iflag=isGoodPaddle(*sector,*paddle,*run);
  return(iflag);
}


int isGoodPaddle(int sector, int paddle, int run)
{

  /* Knock out any paddles from some unphysical region in CLAS */
  //if(sector<=0 || sector >6 || paddle<=0 || paddle > 48)
  if(sector<=0 || sector >6 || paddle==0 || paddle > 48)
  {
    return 0;
  }

  if(run == 0) /* g1c run */
  {
    if( (sector==1 && paddle==44) ||
        (sector==1 && paddle==29) ||
        (sector==3 && paddle==45) ||
        (sector==4 && paddle==21) ||
        (sector==4 && paddle==23) ||
        (sector==6 && paddle==32) )
    {
      return 0;
    }
    else 
      return 1;
  }
  else if(run == 1) /* g11 run */
  {
    if( (sector==1 && paddle==18) ||
        (sector==1 && paddle==26) ||
        (sector==1 && paddle==27) ||
        
        (sector==1 && paddle==33) ||

        (sector==3 && paddle==11) ||

        (sector==4 && paddle==26) ||

        (sector==5 && paddle==20) ||

        (sector==6 && paddle==25) ||
        (sector==6 && paddle==30) ||
        (sector==6 && paddle==34) ||

        (sector==1 && paddle==23) ||
        (sector==2 && paddle==23) ||
        (sector==3 && paddle==23) ||
        (sector==4 && paddle==23) ||
        (sector==5 && paddle==23) ||
        (sector==6 && paddle==23) )
    {
      return 0;
    }

    else 
      return 1;
  }

  else if(run == 2) /* g11 run */
  {
    if( (sector==1 && paddle==18) ||
        (sector==1 && paddle==26) ||
        (sector==1 && paddle==27) ||
        
        (sector==1 && paddle==33) ||

        (sector==3 && paddle==11) ||
        (sector==3 && paddle==24) ||
        (sector==3 && paddle==25) ||

        (sector==4 && paddle==26) ||

        (sector==5 && paddle==20) ||
        (sector==5 && paddle==23) ||

        (sector==6 && paddle==25) ||
        (sector==6 && paddle==30) ||
        (sector==6 && paddle==34))
    {
      return 0;
    }

    else 
      return 1;
  }
  else if(run == 3) /* g11 run */
    {
    if( (sector==1 && paddle==33) ||
        (sector==3 && paddle==11) ||
        (sector==5 && paddle==23) )
    {
      return 0;
    }

    else 
      return 1;
  }
  else
    return 1;
}

#endif
