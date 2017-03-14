/*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko*/
/*-- Author :    Zbigniew Szkutnik   16/12/94*/
 
#define DUM /* just because of a CMZ bug; if not, it calls the deck
               SUN */
void cpwait_(secptr)
int *secptr;
 
 { int time;
 
   time = *secptr;
   sleep(time);
 }
