/*
  This file contain some sobroutines to crate MTRK, CLST and SGMP BOS banks.
  
  Author         : G. Gavalian
  Creation Date  : 22 Feb 1998

 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <trklink.h>

int make_mtrk_bank_(int lnk_segm[MAX_TRK][6],int lnk_clust[MAX_TRK][6],int itr_sect[MAX_TRK],int ntr_link[1]){
  clasMTRK_t *MTRK = NULL;
  int n_row,j,i;
  
  n_row=ntr_link[0]; 
  if(n_row>0){
  MTRK = (clasMTRK_t *)makeBank(&bcs_,"MTRK",0,7,n_row);
  if(MTRK!=NULL){
    for(i=0;i<n_row;i++){
      MTRK->mtrk[i].sect=itr_sect[i];
      for(j=0;j<6;j++){ 
	MTRK->mtrk[i].data[j].segm=lnk_segm[i][j];
	MTRK->mtrk[i].data[j].clust=lnk_clust[i][j];
      }
    }
  } 
  }
}


int make_sgmp_bank_(int segm[6][6][60][12],int nsegm[6][6]){
  int gb_row,gb_col,Csec,Slayer,nsc,j,i;
  int Spoint[7];
  clasSGMP_t *SGMP = NULL;
  gb_col=7; 
  for(Csec=0;Csec<6;Csec++){
    gb_row=0;
    for(Slayer=0;Slayer<6;Slayer++) gb_row+=nsegm[Csec][Slayer];
    if(gb_row>0){
      SGMP = (clasSGMP_t *) makeBank(&bcs_,"SGMP",Csec+1,gb_col,gb_row);
      if(SGMP!=NULL){
	j=0;
	for(Slayer=0;Slayer<6;Slayer++){
	  if(nsegm[Csec][Slayer]>0){
	    for(nsc=0;nsc<nsegm[Csec][Slayer];nsc++){
	      SGMP->sgmp[j].is=Slayer;
	      for(i=0;i<6;i++){
		SGMP->sgmp[j].data[i].wireid =segm[Csec][Slayer][nsc][i*2];
		SGMP->sgmp[j].data[i].time   =segm[Csec][Slayer][nsc][(i*2)+1];
	      }
	      j++;
	    }
	  }
	}
      }
    }
  }

}


int make_clst_bank_(int clust[6][6][10][20],int nsegmc[6][6][10],int nclust[6][6]){
  clasCLST_t *CLST = NULL;
  int cevt;
  int Csec, j, Slayer, ncl, nsg, n_row;
  
  for(Csec=0;Csec<6;Csec++){
    n_row=0;
    for(Slayer=0;Slayer<6;Slayer++){
      for(ncl=0;ncl<nclust[Csec][Slayer];ncl++){
	for(nsg=0;nsg<nsegmc[Csec][Slayer][ncl];nsg++) n_row++;
      }
    }
    j=0;
    if(n_row>0){
      CLST = (clasCLST_t *) makeBank(&bcs_,"CLST",Csec+1,1,n_row);
      if(CLST!=NULL){
	for(Slayer=0;Slayer<6;Slayer++){
	  for(ncl=0;ncl<nclust[Csec][Slayer];ncl++){
	    for(nsg=0;nsg<nsegmc[Csec][Slayer][ncl];nsg++){
	      CLST->clst[j].layer     =Slayer;
	      CLST->clst[j].clustern  =nclust[Csec][Slayer];
	      CLST->clst[j].csegment  =nsg;
	      CLST->clst[j].cclust    =ncl;
	      CLST->clst[j].clustp    =clust[Csec][Slayer][ncl][nsg];
	      j++;
	    }
	  }
	}
      }
    }
  }
}


int read_clst_bank_(int clust[6][6][10][20],int nsegmc[6][6][10],int nclust[6][6],int nread[1]){
  int Csec, j, dread;
  int Slayer,Cclust,Csegm,Nclust,CClustp;
  clasCLST_t *CLST = NULL;
  int *PGp;
  int cevt;

  dread=0;
  for(Csec=0;Csec<6;Csec++){
    CLST = (clasCLST_t *) getGroup(&bcs_,"CLST",Csec+1);
    if(CLST!=0){
      dread++;
      for(j=0;j<CLST->bank.nrow;j++){
	Slayer=CLST->clst[j].layer;
	Nclust=CLST->clst[j].clustern;
	Csegm=CLST->clst[j].csegment;
	Cclust=CLST->clst[j].cclust;
	CClustp=CLST->clst[j].clustp;
	nclust[Csec][Slayer]=Nclust;
	nsegmc[Csec][Slayer][Cclust]++;
	clust[Csec][Slayer][Cclust][Csegm]=CClustp;
      }
    }
  }
  nread[0]=dread;
}


int read_mtrk_bank_(int lnk_segm[MAX_TRK][6],int lnk_clust[MAX_TRK][6],int itr_sect[MAX_TRK],int ntr_link[1],int nread[1]){
  clasMTRK_t *MTRK = NULL;
  int Trkn, j, i, dread;
  link_t LINK;
  int n_row;

  Trkn=0;
  dread=0;
  ntr_link[0]=0;
  MTRK=(clasMTRK_t *) getGroup(&bcs_,"MTRK",0);
  if(MTRK != NULL){
    dread++;
    n_row=MTRK->bank.nrow;
    for(j=0;j<MTRK->bank.nrow;j++){
      itr_sect[j]=MTRK->mtrk[j].sect;
      for(i=0;i<6;i++){
	LINK=MTRK->mtrk[j].data[i];
	lnk_segm[j][i]=(int)LINK.segm;
	lnk_clust[j][i]=(int)LINK.clust;	
      }
      Trkn++;
    }
  } 
  ntr_link[0]=Trkn;
  nread[0]=dread;
}



int read_sgmp_bank_(int segm[6][6][60][12],int nsegm[6][6], int nread[1]){
  int j, i, Csec, Slayer, Rpoint, Old_sl, Rp, dread;

  int pr;

  clasSGMP_t *SGMP = NULL;
  dread=0;
  
  for(j=0;j<6;j++){
    for(i=0;i<6;i++) nsegm[j][i]=0;
  }
  
  for(Csec=0;Csec<6;Csec++){
    SGMP=(clasSGMP_t *) getGroup(&bcs_,"SGMP",Csec+1);
    if(SGMP!=NULL){
      dread++;
      for(j=0;j<SGMP->bank.nrow;j++){
	Slayer=SGMP->sgmp[j].is;
	Rp=nsegm[Csec][Slayer];
	nsegm[Csec][Slayer]++;
	for(i=0;i<6;i++){
	  segm[Csec][Slayer][Rp][i*2]      = SGMP->sgmp[j].data[i].wireid;
	  segm[Csec][Slayer][Rp][(i*2)+1]  = SGMP->sgmp[j].data[i].time;  
	}
      }
    } 
  }
  nread[0]=dread;
}





