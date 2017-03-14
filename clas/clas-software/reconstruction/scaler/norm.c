/* Routines for photon normaliztion using scaler info from 
   $CLAS_PARMS/Maps/TAG_SCALER.map 
   Burin and Simon */
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ntypes.h>
#include <bostypes.h>
#include <bosddl.h>
#include <map_manager.h>
#include <scalers.h>
#include <clas_cern.h>

int get_scalers_map(int runno,float PC_Eff[64],float PS_Eff[64],float Tag_Eff[64]){
  char *dir,map[100];
  int firsttime,i;

  dir = getenv("CLAS_PARMS");
  sprintf(map,"%s/Maps/TAG_SCALER.map",dir);
  map_get_float(map,"PCEff","value",64,&PC_Eff[0],runno,&firsttime);
  map_get_float(map,"PSEff","value",64,&PS_Eff[0],runno,&firsttime);
  map_get_float(map,"TagEff","value",64,&Tag_Eff[0],runno,&firsttime);

  return(1);
}

int put_DSDEff_map(int runno, float PC_Eff[64], float PS_Eff[64], float Tag_Eff[64]) {
  char *dir = getenv("CLAS_PARMS");
  char map[100];
  int first;
  
  sprintf(map, "%s/Maps/TAG_SCALER.map", dir);
  map_put_float(map, "PCEff", "value", 64, PC_Eff, runno);
  map_put_float(map, "PSEff", "value", 64, PS_Eff, runno);
  map_put_float(map, "TagEff", "value", 64, Tag_Eff, runno);
}

int calc_scalers_data(int runno) {
  char mess[100],*dir,map[100];
  int firsttime,gate,i;
  float PC_Eff[64];
  float PS_Eff[64];
  float Tag_Eff[64];
  float Nphoton[NUM_E_BINS];
  clasTRGS_t *TRGS = getBank(&wcs_, "TRGS");
  clasSCALER_t *TGS = getBank(&wcs_, "TGS ");

  dir = getenv("CLAS_PARMS");
  sprintf(map,"%s/Maps/TAG_SCALER.map",dir);
  map_get_int(map,"gate","value",1,&gate,runno,&firsttime);
  if((gate==PC_PS_PCdelay_PSdelay) || (gate==PSdelay_PS_PS_PSdelay)) {
    /* get_Ngamma_wcs(runno,Nphoton);  don't use */
    ngamma_part_prod(runno,Nphoton);
    for(i=1;i<=767;i++)	hf1(5000,i,Nphoton[i-1]);
  }
  else {
    DSD_efficiency(gate,TGS,PC_Eff,PS_Eff,Tag_Eff);
  }

  return(1);
}

int fill_ET_histo() {
  int i;
  clasTAGR_t *TAGR = NULL;
  
  TAGR = getBank(&bcs_,"TAGR");
  if(TAGR) {
    for(i=0;i<TAGR->bank.nrow;i++) {
      if(TAGR->tagr[i].stat==7 || TAGR->tagr[i].stat==15) {
	hf2(500,(float)TAGR->tagr[i].e_id,(float)TAGR->tagr[i].t_id,1.);
	hf1(510,(float)TAGR->tagr[i].e_id,1.);
	hf1(520,(float)TAGR->tagr[i].t_id,1.);
      }
    }
  }
  return(1);
}

float epsilon_ijk(int E_T_count, int Tcount){
  float epsilon = 0.0;
  
  if (Tcount > 0) {
    epsilon = ((float)E_T_count)/((float)Tcount);
  }
  return epsilon; 
}




int tag_eff_prod(clasSCALER_t *TGS,float PC_T[64],float PCdelay_T[64],
    float PS_T[64],float PSdelay_T[64]) {
  int i;
  uint32 pc[64], pcdelay[64];
  uint32 ps[64], psdelay[64];
  uint32 rawt[64];
  float temp[64];
  
  memset(PC_T,0.,sizeof(float)*64);
  memset(PCdelay_T,0.,sizeof(float)*64);
  memset(PS_T,0.,sizeof(float)*64);
  memset(PSdelay_T,0.,sizeof(float)*64);
  memset(PS_T,0.,sizeof(float)*64);
  memset(pc,0,sizeof(int)*64);
  memset(pcdelay,0,sizeof(int)*64);
  memset(ps,0,sizeof(int)*64);
  memset(psdelay,0,sizeof(int)*64);
  memset(rawt,0,sizeof(int)*64);
  memset(temp,1.,sizeof(float)*64);
  if(fill_rtsl(TGS,rawt) && fill_g1sl(TGS,pc) && fill_g2sl(TGS,ps) &&
     fill_g3sl(TGS,pcdelay) && fill_g4sl(TGS,psdelay)) {
    get_eff(pc,rawt,PC_T);
    get_eff(pcdelay,rawt,PCdelay_T); 
    get_eff(ps,rawt,PS_T);
    get_eff(psdelay,rawt,PSdelay_T);
    for(i=0;i<61;i++) {
	hf1(1000,(float)(i+1),PC_T[i]);
	hf1(1100,(float)(i+1),PCdelay_T[i]);
	hf1(1200,(float)(i+1),PS_T[i]);
	hf1(1300,(float)(i+1),PSdelay_T[i]);
    }
    smooth(1000,POLYNOMIAL,5,PC_T);
    smooth(1100,POLYNOMIAL,1,PCdelay_T);
    smooth(1200,GAUSS,3,PS_T);
    smooth(1300,POLYNOMIAL,1,PSdelay_T);
    for(i=0;i<61;i++) {
	hf1(2000,(float)(i+1),PC_T[i]);
	hf1(2100,(float)(i+1),PCdelay_T[i]);
	hf1(2200,(float)(i+1),PS_T[i]);
	hf1(2300,(float)(i+1),PSdelay_T[i]);
    }	
  }   
}    

int tag_eff_prod2(clasSCALER_t *TGS,float PS_T[64],float PSdelay_T[64]) {
  int i;
  uint32 ps[64], psdelay[64];
  uint32 rawt[64];
  uint32 ps1[64], ps2[64], psd1[64], psd2[64];
  
  memset(PS_T,0.,sizeof(float)*64);
  memset(PSdelay_T,0.,sizeof(float)*64);
  memset(ps,0,sizeof(int)*64);
  memset(psdelay,0,sizeof(int)*64);
  memset(rawt,0,sizeof(int)*64);
  memset(ps1,0,sizeof(uint32)*64);
  memset(ps2,0,sizeof(uint32)*64);
  memset(psd1,0,sizeof(uint32)*64);
  memset(psd1,0,sizeof(uint32)*64);

  if(fill_rtsl(TGS,rawt) && fill_g1sl(TGS,psd1) && fill_g2sl(TGS,ps1) &&
     fill_g3sl(TGS,ps2) && fill_g4sl(TGS,psd2)) {
    addarr(ps1,ps2,ps,64);
    addarr(psd1,psd2,psdelay,64);
    get_eff(ps,rawt,PS_T);
    get_eff(psdelay,rawt,PSdelay_T);
    for(i=0;i<61;i++) {
	hf1(1200,(float)(i+1),PS_T[i]);
	hf1(1300,(float)(i+1),PSdelay_T[i]);
    }
    for(i=0;i<61;i++) {
	hf1(2200,(float)(i+1),PS_T[i]);
	hf1(2300,(float)(i+1),PSdelay_T[i]);
    }	
  }   
}    


int calc_Ngamma(int E_T_matrix[NUM_E_BINS+1][NUM_T_BINS+1],float PS_Eff[64], 
		float Tag_Eff[64], 
    uint32 rawt[64], float PS_T[64], float PSdelay_T[64], float Nphoton[768]) {
  int i,j,k;
  int Tbin[NUM_T_BINS+1];
  int Tid[NUM_T_COUNTERS+1];
  
  E_T_matrix2Tbin(E_T_matrix, Tbin);
  Tbin2Tid_arr(Tbin,Tid);
  for(i=1;i<=61;i++) {
    if(PS_Eff[i-1]*Tag_Eff[i-1]) {
      for(j=E_binmin(i);j<=E_binmax(i);j++) {
        for(k=T2Tbinmin(i);k<=T2Tbinmax(i);k++) {
            Nphoton[j] = Nphoton[j]+(epsilon_ijk(E_T_matrix[j][k],Tid[i])) * (PS_T[i-1]-PSdelay_T[i-1]) * ((float)rawt[i-1])/PS_Eff[i-1]/Tag_Eff[i-1];
        }  
      }  
    }
  }
  return(1);
}  
   
int get_Ngamma_(int runno, float Nphoton[768]) {
  char *dir,map[100];
  int firsttime,gate;

  dir = getenv("CLAS_PARMS");
  sprintf(map,"%s/Maps/TAG_SCALER.map",dir);
  map_get_int(map,"gate","value",1,&gate,runno,&firsttime);
  if((gate==PC_PS_PCdelay_PSdelay)||(gate==PSdelay_PS_PS_PSdelay)) {
    get_Ngamma_map(runno,Nphoton);
  }
  else {
    printf("The gate is not PC_PS_PCdelay_PSdelay or PSdelay_PS_PS_PSdelay type.\n");
  }   
}             

int get_Ngamma_wcs(int runno, float Nphoton[768]) {
  clasSCALER_t *TGS = getBank(&wcs_, "TGS ");

  get_Ngamma(runno, TGS, Nphoton);

}   
 

int get_Ngamma(int runno, clasSCALER_t *TGS, float Nphoton[NUM_E_BINS+1]){
  float PC_Eff[64];
  float PS_Eff[64];
  float Tag_Eff[64];
  float PC_T[64];
  float PS_T[64];
  float PCdelay_T[64];
  float PSdelay_T[64];
  float PS_norm[64];
  float alpha=1.0;
  uint32 rawt[64];
  float ratio[64];
  int i;
  int E_T_matrix[NUM_E_BINS+1][NUM_T_BINS+1];
  int Tbin[NUM_T_BINS+1]; 

  char *dir,map[100];
  int firsttime;
  int gate;

  dir = getenv("CLAS_PARMS"); 

  sprintf(map,"%s/Maps/NORM.map",dir);
  map_get_float(map,"PS_scalers","width_corr",1,&alpha,runno,&firsttime);

  memset(PS_norm,0,sizeof(float)*64);
  sprintf(map,"%s/Maps/NORM.map",dir);
  map_get_float(map,"PS_scalers","norm",64,PS_norm,runno,&firsttime);

  sprintf(map,"%s/Maps/TAG_SCALER.map",dir);
  map_get_int(map,"gate","value",1,&gate,runno,&firsttime);

  memset(Nphoton,0.,sizeof(float)*768);
  memset(rawt,0,sizeof(uint32)*64);
  memset(ratio,0,64*sizeof(float));

  /* not used yet
  clasTRGS_t *TRGS = getBank(&wcs_, "TRGS");
  clasS1ST_t *S1ST = getBank(&bcs_, "S1ST");
  */

  /* make sure the E->T matching table is filled */
  fill_E_T_map();

  if(fill_rtsl(TGS,rawt)) {
    float numerator=0.0,denominator=0.0;

    switch (gate) {
    case PC_PS_PCdelay_PSdelay:  /* g1a,b */
      tag_eff_prod(TGS,PC_T,PCdelay_T,PS_T,PSdelay_T);
      break;
    case PSdelay_PS_PS_PSdelay:  /* g1c, g2a */
      tag_eff_prod2(TGS,PS_T,PSdelay_T);
      break;
    default:
      break;
    }  

    fprintf(stdout,"Production PS trues\n");
    for (i=0;i<64;i++) fprintf(stdout,"%f\n",PS_T[i]*rawt[i]);
    fprintf(stdout,"Production PS accidentals\n");
    for (i=0;i<64;i++) fprintf(stdout,"%f\n",PSdelay_T[i]*rawt[i]);


    for (i=0;i<61;i++){
      if (PS_norm[i])
	ratio[i]=(PS_T[i]-alpha*PSdelay_T[i])*rawt[i]/PS_norm[i];
    } 
 
    fprintf(stdout,"Ratio of scalers\n");
    for (i=0;i<64;i++) fprintf(stdout,"%f\n",ratio[i]);

    get_scalers_map(runno,PC_Eff,PS_Eff,Tag_Eff);
    get_E_T_matrix_map(runno, E_T_matrix);
    calc_Ngamma(E_T_matrix,PS_Eff,Tag_Eff,rawt,PS_T,PSdelay_T,Nphoton);
  }

}


int get_Ngamma_map(int runno, float Nphoton[768]) {
  get_Ngamma(runno, make_TGS_from_map(&wcs_, runno, 1), Nphoton);
  dropBank(&wcs_, "TGS", 1);
}








