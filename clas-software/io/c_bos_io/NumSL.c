#include <stdio.h>
#include <ntypes.h>       /* data types like uint16 */
#include <bostypes.h>

  /* this routine returns the number of superlayers for the given track (counting from 1- index to 
     clasHBTR) */
  

int NumSL(int trackno)
{
  int i,ret=0;
  int sl[] = {0,0,0,0,0,0};
  clasHBTR_t *HBTR= (clasHBTR_t *)getBank(&bcs_,"HBTR");
  clasHBTB_t *HBTB;
  
  if (HBTR) {
    if(HBTB = (clasHBTB_t *)getGroup(&bcs_,"HBTB",HBTR->hbtr[trackno-1].itr_sec/100)) {
      
      for (i = 0; i < HBTB->bank.nrow; ++i) {
	if ((HBTB->hbtb[i].trk == trackno) && (HBTB->hbtb[i].isg > 0)) {
	  sl[HBTB->hbtb[i].sly-1] = 1;
	}
      }
    }
  }
  for (i = 0; i < 6; ++i)
    ret += sl[i];
  return(ret);
}

int numsl_(int *trackno)
{
  /* fortran wrapper for above (NumSL) */
  return(NumSL(*trackno));
}

	  
 

      
