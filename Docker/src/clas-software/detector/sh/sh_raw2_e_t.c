// Converts raw adc and tdc into nphe and time
// adc hit type:
// "1" - coincidence between spiral-straight fibers
// "2" - NO coincidence between spiral-straight fibers
// "3" - hit in the channel of staight fiber
// tdc hit type:
// "5" - hit is in 3 sigmas of tdc peak
// "-5" - hit is out of 3 sigmas of tdc peak
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "sh.h"
#include "sh_global.h" 
#include "sh_extern.h"
 

int sh_adc2_nphe(int adch){
  
  int ia=0;
  int adccount=0;
  for(ia=0;ia<adch;ia++){
    float phe=0;
    int hit_type=0;
    
    int db=get_db_ind(adcev[ia].id);
    int adcstat=sh_calib.adcstat[db];   
    if(adcstat==1){      
      
      phe=get_nphe(adcev[ia].val,sh_calib.pedestal[db],sh_calib.gain[db]);
      int ch2check_id=coin_check(adcev[ia].id);

      if(ch2check_id != adcev[ia].id){
     	int iia=0;
	for(iia=0;iia<adch;iia++){
	  if(adcev[iia].id==ch2check_id){hit_type=1;break;}
	  else{hit_type=2;}
	}
      }
      else{hit_type=3;}

      adc2ph[adccount].pix=adcev[ia].id;
      adc2ph[adccount].val=phe;
      adc2ph[adccount].type=hit_type;
      adccount++;
    }
  }
  return adccount;
}

int sh_tdc2_nsec(int tdch){
  int it=0;
  int cc=0;// # of chid with hits
  int Tmh[tdch][16];//for multi hit tdc information, up to 16 hits
  int mhC[tdch][2];//for #of hits for this chid [][0] and chid [][1]
  //sort tdc
  for(it=0;it<tdch;it++){
    int id = tdcev[it].id;
    int shind=-1;
    int iit=0;
    for(iit=0;iit<cc;iit++){
      if(id==mhC[iit][1]){	
	mhC[iit][0]++;shind=iit;
      }
    }
    if(shind!=-1){
      Tmh[shind][mhC[shind][0]]= tdcev[it].val;
    }
    else{
      mhC[cc][0]=0; mhC[cc][1]=id;
      Tmh[cc][mhC[cc][0]]= tdcev[it].val;
      cc++;
    }
  }
  
  //to find correct hit
  int tdccount=0;
  for(it=0;it<cc;it++){//1
    float nsec=0;
    int hit_type=0;
    int db=get_db_ind(mhC[it][1]);   
    int tdcstat=sh_calib.tdcstat[db];
    if(tdcstat==1){//2
      float tm=sh_calib.tcoi_m[db];//mean
      float ts=sh_calib.tcoi_s[db];//sigma
      float range=3*ts;
      int m=0,fin_ind=0;
      float min=100000000;
      for(m=0;m<mhC[it][0]+1;m++){//3
	float sel=abs(tm-Tmh[it][m]);
	if(sel<min){
	  min=sel;fin_ind=m;
	  if(sel< range){hit_type= 5;}
	  if(sel>=range){hit_type=-5;}
	}		
      }//3
      nsec=get_time(Tmh[it][fin_ind],sh_calib.t2channel[db],sh_calib.t0[db]);
      tdc2ns[tdccount].pix=mhC[it][1];
      tdc2ns[tdccount].val=nsec;
      tdc2ns[tdccount].type=hit_type;
      tdccount++;
    }//2
  }//1
  return tdccount;
}


 

