#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ntypes.h>
#include <bostypes.h>
#include <bosddl.h>
#include <map_manager.h>
#include <scalers.h>
#include <clas_cern.h>

/* Fortran wrapper for ngamma_prod */
int ngamma_prod_(int *runno, float Nphoton_prod[NUM_E_BINS]){
  ngamma_prod(*runno,Nphoton_prod);
}

/* Routine that returns the number of photons for a part of a production 
   run.  Information for the associated normalization run is read from the
   map, but the production scalers are obtained from the wcs BOS array filled
   by some other routines on a scaler event-by-event basis. */

int ngamma_part_prod(int runno, float Nphoton_prod[NUM_E_BINS]){
  char *dir,map[100];
  int i,gate,firsttime;
  float alpha=1.0;
  float PS_ratio[64],PS_norm[64],PS_norm_map[64],prod_live_time=1.0;
  float PC_T[64],PS_T[64],PCdelay_T[64],PSdelay_T[64];
  int PS_trans[61];
  uint32 rawt[64];
  clasTRGS_t *TRGS = getBank(&wcs_, "TRGS");
  clasSCALER_t *TGS = getBank(&wcs_, "TGS "); 
  clasS1ST_t *S1ST = getBank(&wcs_, "S1ST");

  dir = getenv("CLAS_PARMS"); 
  sprintf(map,"%s/Maps/TAG_SCALER.map",dir);
  map_get_int(map,"gate","value",1,&gate,runno,&firsttime);
  
  if((gate==PC_PS_PCdelay_PSdelay) || (gate==PSdelay_PS_PS_PSdelay)
     && TGS && TRGS && S1ST){

    /* Calculate the live time for the productino run based on the scalers */
    unsigned int clock=TRGS->trgs[0].trig_or_ug; 
    unsigned int liveclock=S1ST->s1st[0].event_count;
    float prod_live_time=1.0; 

    if (clock&&liveclock) prod_live_time=(float)liveclock/(float)clock;

    /* Intialize some memory */
    memset((void *)PS_ratio,0,64*sizeof(float));   

    /* Get the normalization scalers and a fudge factor from the map */
    sprintf(map,"%s/Maps/NORM.map",dir);
    map_get_float(map,"PS_scalers","norm",64,PS_norm_map,runno,&firsttime);
    map_get_float(map,"PS_scalers","width_corr",1,&alpha,runno,&firsttime);
    map_get_int(map,"PS_scalers","translation",61,PS_trans,runno,&firsttime);
    /* Compensate for cable swaps... */
    for (i=0;i<61;i++){
      PS_norm[PS_trans[i]-1]=PS_norm_map[i];
    }

    /* Obtain the smoothed PS scaler information for the production data */
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

    /* Get the raw T information from the TGS bank */
    fill_rtsl(TGS,rawt);
    fprintf(stdout,"Production PS trues\n");
    for (i=0;i<64;i++) fprintf(stdout,"%f\n",PS_T[i]*rawt[i]);
    fprintf(stdout,"Production PS accidentals\n");
    for (i=0;i<64;i++) fprintf(stdout,"%f\n",PSdelay_T[i]*rawt[i]);

    /* Calculate the ratio of production to normalization scalers */
    for (i=0;i<61;i++){
      if (PS_norm[i])
	PS_ratio[i]=(PS_T[i]-alpha*PSdelay_T[i])*rawt[i]/PS_norm[i];
    } 
    fprintf(stdout,"Ratio of scalers\n");
    for (i=0;i<64;i++) fprintf(stdout,"%f\n",PS_ratio[i]);

    /* Calculate the number of gammas in the production run */
    calc_ngamma_prod(runno,Nphoton_prod,PS_ratio,prod_live_time);
  }
}


/* Routine that returns the number of photons in an entire production run.
   It reads all the information from the map. */
int ngamma_prod(int runno, float Nphoton_prod[NUM_E_BINS]){
  char *dir,map[100];
  int gate,firsttime;
  float PS_ratio[64],PS_norm[64],PS_norm_map[64],PS_prod[64],PS_delay[64];
  int PS_trans[61],i;
  float prod_live_time=1.0,alpha=1.0;

  dir = getenv("CLAS_PARMS"); 
  sprintf(map,"%s/Maps/TAG_SCALER.map",dir);
  map_get_int(map,"gate","value",1,&gate,runno,&firsttime);
  
  if((gate==PC_PS_PCdelay_PSdelay)||(gate==PSdelay_PS_PS_PSdelay)){
    sprintf(map,"%s/Maps/NORM.map",dir);

    map_get_float(map,"production","livetime",1,&prod_live_time,runno,
		  &firsttime);
    if (runno!=firsttime) 
      fprintf(stderr,"Warning: no livetime for production run %d\n",runno);

    map_get_float(map,"PS_scalers","prod",64,PS_prod,runno,&firsttime);
    if (runno!=firsttime) 
      fprintf(stderr,"Warning: no production scalers for run %d\n",runno);
 
    map_get_float(map,"PS_scalers","delay",64,PS_delay,runno,&firsttime);
    if (runno!=firsttime) 
      fprintf(stderr,"Warning: no delayed production scalers for run %d\n",runno);

    map_get_float(map,"PS_scalers","norm",64,PS_norm_map,runno,&firsttime);
    map_get_float(map,"PS_scalers","width_corr",1,&alpha,runno,&firsttime);
    map_get_int(map,"PS_scalers","translation",61,PS_trans,runno,&firsttime);

    /* Compensate for cable swaps... */
    for (i=0;i<61;i++){
      PS_norm[PS_trans[i]-1]=PS_norm_map[i];
    } 

    /* Intialize some memory and calculate the ratios*/
    memset((void *)PS_ratio,0,64*sizeof(float));    
    for (i=0;i<61;i++){
      if (PS_norm[i])
	PS_ratio[i]=(PS_prod[i]-alpha*PS_delay[i])/PS_norm[i];
    } 
    /* Calculate the number of gammas in the production run */
    calc_ngamma_prod(runno,Nphoton_prod,PS_ratio,prod_live_time);
  }
}

/* routine that calculates the number of gammas for a production run by 
   scaling the number of gammas from a normalization run by the ratio 
   of the scalers (prod/norm). */
int calc_ngamma_prod(int runno, float Nphoton_prod[NUM_E_BINS],
		     float PS_ratio[64],float prod_live_time){
  float Nphoton_norm[NUM_E_BINS];
   char *dir,map[100];
  float ratio=1.0,norm_live_time=1.0;
  int denom=1;
  float prescale_norm=0.0,prescale_prod=0.0;
  int firsttime,gate,i,j;

  dir = getenv("CLAS_PARMS"); 
  sprintf(map,"%s/Maps/NORM.map",dir);
  map_get_float(map,"normalization","livetime",1,&norm_live_time,runno,
		&firsttime);
  map_get_float(map,"normalization","prescale",1,&prescale_norm,runno,
		&firsttime);
  map_get_float(map,"production","prescale",1,&prescale_prod,runno,
		&firsttime);
    
  memset((void *)Nphoton_norm,0,(NUM_E_BINS)*sizeof(float));
  memset((void *)Nphoton_prod,0,(NUM_E_BINS)*sizeof(float));
  
  map_get_float(map,"normalization","ngamma",NUM_E_BINS,Nphoton_norm,runno,
		&firsttime);
  fill_E_T_map();

  for (j=1;j<NUM_E_BINS+1;j++){
    int t_min=0,t_max=0;
    ratio=0.0;
    if (j%2) { /* odd bin */
      t_min=tmin((j+1)/2)-1;
      t_max=tmax((j+1)/2)-1;
    }
    else{ /* even bin */
      t_min=tmin(j/2)-1;
      t_max=tmax((j+2)/2)-1;
    }
    denom=t_max-t_min+1;
    for (i=t_min;i<=t_max;i++){
      ratio+=PS_ratio[i];
       if (PS_ratio[i]==0.0 && denom>1) denom--;
    }
    
    ratio/=(float)(denom);
    
    Nphoton_prod[j-1]=(prescale_norm+1.0)/(prescale_prod+1.0)
      *ratio*Nphoton_norm[j-1]*prod_live_time/norm_live_time;
  }
}



