#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "sh.h"
#include "sh_global.h" 
#include "sh_extern.h"
 

void sh_hb_pb(int nadc,int ntdc){

  dropBank(&bcs_, "SHHB",0); dropBank(&bcs_, "SHPB",0);
  clasSHHB_t *SHHB=NULL;     clasSHPB_t *SHPB=NULL; 
  shhb_t arrhb[nadc];        shpb_t arrpb[nadc];

  //SHHB
  int ia=0; int it=0;
  int chb=0;int cpb=0;
  for(ia=0; ia<nadc; ia++){
    int db=get_db_ind(adc2ph[ia].pix);
    arrhb[chb].hid=adc2ph[ia].pix;
    arrhb[chb].hx=xCLAS[db];
    arrhb[chb].hy=yCLAS[db];
    arrhb[chb].hz=zCLAS[db];
    arrhb[chb].hnphe=adc2ph[ia].val;
    int tcheck=0;
    int fstat=0;
    int ftime=0;
    for(it=0;it<ntdc;it++){
      if(adc2ph[ia].pix==tdc2ns[it].pix){
	fstat=adc2ph[ia].type*100+tdc2ns[it].type;
	ftime=tdc2ns[it].val;
	tcheck++;
      }
    }
    if(tcheck>1){
      printf("Error. There are more than one TDChit for ch:%d",tdc2ns[it].pix);
      exit(1);}
    if(!tcheck){      
      arrhb[chb].htime=-100;
      arrhb[chb].hstatus=adc2ph[ia].type;
    }
    else{     
      arrhb[chb].htime=ftime;
      arrhb[chb].hstatus=fstat;
    }
    chb++;
  }

  if(chb){
    if( (SHHB=makeBank(&bcs_, "SHHB",0,sizeof(shhb_t)/4,chb)) ){
      int c;   
      for(c=0;c<chb;c++){ 
	SHHB->shhb[c]=arrhb[c];
      }      
    }
  }

  //SHPB
  //spiral-straight fibers coincidence + tdc hit
  for(ia=0; ia<chb && cpb<SH_PIXELS ;ia++){
    if(arrhb[ia].hstatus==105 || arrhb[ia].hstatus==95){
      int iia=0; int nphe5cut=0;
      int pixid2check=coin_check(arrhb[ia].hid);
      for(iia=0;iia<chb;iia++){
	if(arrhb[iia].hid==pixid2check){
	  if(arrhb[iia].hnphe>5.){nphe5cut=1;}
	}
      }
      if(arrhb[ia].hnphe>5. && nphe5cut==1){
	arrpb[cpb].id=arrhb[ia].hid;
	arrpb[cpb].x=arrhb[ia].hx;
	arrpb[cpb].y=arrhb[ia].hy;
	arrpb[cpb].z=arrhb[ia].hz;
	arrpb[cpb].nphe=arrhb[ia].hnphe;
	arrpb[cpb].time=arrhb[ia].htime;
	arrpb[cpb].status=arrhb[ia].hstatus;
	cpb++;
      }
    }
  }
  if(cpb){
    if( (SHPB=makeBank(&bcs_, "SHPB",0,sizeof(shpb_t)/4,cpb)) ){
      int k;   
      for(k=0;k<cpb;k++){ 
	SHPB->shpb[k]=arrpb[k];
      }      
    }
  }
}

 

