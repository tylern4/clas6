/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include "rtpc.h"
#include "TPC_BOS.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
extern "C" 
int read_BOS()
{
  int ret=0;
  int i=0, j=0,jj=0;
  int altro_channel=0,altro_time=0,altro_signal=0,tic=0,pad_num=0;
  
  hh_num_hits= 0; //global
  good_altro_data = FALSE;  //global   
  for (j=0; j<NAL_SAMP; j++)
  {/* clear the data array */
    for (i=0; i<NUM_PADS; i++)   pad_dat[j][i]= 0.0; //global
    for (i=0; i<NUM_XTRA; i++) 	 xtra_dat[j][i]= 0.0; //global     
  }
	
  ret=ReadTPCBank();
  if(ret==0) 
  {// No TPC bank in this event
    good_altro_data = FALSE;
    if(!quiet)printf("No '%s' Bank Info in this Event\n","TPC ");
    return 1; //but keep going
  } 
  else
  { 
    //fill up Nate's pad_dat[NAL_SAMP][NUM_PADS] and xtra_dat[NAL_SAMP][NUM_XTRA] array here
    good_altro_data = TRUE;
      
    for(jj=raw_data.size()-1;jj>=0;jj--)
    {
      pad_num= -999;
      //fill from the end to the start will make the data in the ascending order
     
      altro_channel = (int)raw_data[jj].id;
      altro_time    = (int)raw_data[jj].tdc;
      altro_signal  = (int)raw_data[jj].adc;
      tic =(int) altro_time/NS_PER_TIC;
      if(altro_channel >= NAL_CHAN || altro_channel < 0) 
      {
	printf("channel from BOS_BANK is out of range:");
	printf("raw_data[%4d][0, 1, 2]= %6d %6d %6d\n",jj,altro_channel,altro_time,altro_signal);
	good_altro_data = FALSE;
      }
      else if(altro_time%NS_PER_TIC)
      {
	printf("ALTRO time value is not allowed:      ");
	printf("raw_data[%4d][0, 1, 2]= %6d %6d %6d\n",jj,altro_channel,altro_time,altro_signal);
	good_altro_data = FALSE;
      }
      else if( (tic >= NAL_SAMP) || (tic < 0) )
      {
	printf("ALTRO time value is out of range:     ");
	printf("raw_data[%4d][0, 1, 2]= %6d %6d %6d\n",jj,altro_channel,altro_time,altro_signal);
	good_altro_data = FALSE;
      }
      else
      {
	if(altro_channel < 1600)   pad_num= altro_channel;
	else if( (altro_channel >= 1664) && (altro_channel < 1664+1600) )
	  pad_num= altro_channel-64;
	  
	if(pad_num >=0) 
	  {
	    /* By design the pad_num would now be correct. However, a wiring error
	       left some mistakes in this channel-to-pad correspondence. Fix them here. */
	    
	    /* This is an ad-hoc correction to pad numbers 304-319 and 368-383. */
	    if ( (pad_num>=(1600+304)) && (pad_num<=1600+319) )
	      {
		pad_num+=64;
	      }
	    else if ( (pad_num>=(1600+368)) && (pad_num<=1600+383) )
	      {
		pad_num-=64;
	      }
	    /************ end of altro --> pad_num corrections *******/

	    pad_dat[tic][pad_num] = (float)altro_signal;
	    if( altro_signal > THR_Q_LINK)
	      {
		if(hh_num_hits < HH_MAX_NUM_HITS)
		  {
		    hh_hitlist[hh_num_hits].pad= pad_num;
		    hh_hitlist[hh_num_hits].t= tic;
		    hh_hitlist[hh_num_hits].z= rawCYL[tic][pad_num].z;     /* note that these things have to be */
		    hh_hitlist[hh_num_hits].r= rawCYL[tic][pad_num].s;     /* initialized somewhere at*/
		    hh_hitlist[hh_num_hits].phi= rawCYL[tic][pad_num].phi; /* program startup */
		    if(hh_hitlist[hh_num_hits].phi > 2*PI || hh_hitlist[hh_num_hits].phi < -2*PI)
		      {
			//printf("bad phi coordinate in hh_hitlist[%d]! quitting.\n", hh_num_hits);
		      }
		    
		    hh_hitlist[hh_num_hits].q= (float)altro_signal;
		    hh_hitlist[hh_num_hits].status=HUNUSED;
		    if( (float)altro_signal < THR_Q_LINK) hh_hitlist[hh_num_hits].status=HSMALLQ; /* charge too small */
		    hh_num_hits++;
		  }
		else
		  {
		    printf("More hits than space allocated in HH_MAX_NUM_HITS. Extras ignored.\n");
		  }
	      }
	  }
	//pulse signal	
	if( (altro_channel >= 1600) && (altro_channel < 1664))
	  xtra_dat[tic][altro_channel-1600] = (float)altro_signal;
	if( (altro_channel >= 1664+1600) && (altro_channel < 1664+1664))
	  xtra_dat[tic][altro_channel-1600-1600] = (float)altro_signal;
      }
      
    } 
  }
  /*      if(good_altro_data) return 1; */
  /*      return 0; */
  return 1;
  
}
