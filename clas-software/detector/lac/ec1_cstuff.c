#include <stdio.h>
#include <math.h>
#include <stdlib.h>
typedef struct  {
        double ec1_threshold;
        double ec1stenmin_in;
        double ec1stenmin_out;
        double ec1_timeresol;
        float ec1_caliba1;
        float ec1_calibt1;
        float ec1_caliba2;
        float ec1_calibt2;
        int ec1_reco_type;
        int ec1Maxclust;
        int ec1_calib;
        } Ec11common;

typedef struct  {
        int ec1_calupdt;
        float ec1_cala1l[2][4][40];
        float ec1_calt1l[2][4][40];
        float ec1_cala2l[2][4][40];
        float ec1_calt2l[2][4][40];
        float ec1_cala1r[2][4][40];
        float ec1_calt1r[2][4][40];
        float ec1_cala2r[2][4][40];
        float ec1_calt2r[2][4][40];
        float ec1_caltw_al[2][4][40];
        float ec1_caltw_ar[2][4][40];
        float ec1_caltw_bl[2][4][40];
        float ec1_caltw_br[2][4][40];
         } Ec12common;
typedef struct  {
        int ec1_pedupdt;
        int ec1_pedl[2][4][40];
        int ec1_pedr[2][4][40];
        } Ec13common;

/* declare a global variable */

Ec11common ec1_control_;
Ec12common ec1_calibr_;
Ec13common ec1_peds_;
#define EC1_CHANNELS 128

int ec1_getblock_(short *x, int *block, int *stack) 
   { 
unsigned i;
     i= *x;
     *block = (i >> 8); 
     *stack = i & ~(~0 << 8);
   } 

/* get stack (1-24,block1,3  1-40 for block 2,4 & from j (1-128) */
void ec1_j2sbl_ (int *j, int *stack, int *block)
{
int outer;
	 outer= (*j>64) ? 1 : 0;    /* ATTENTION numbering from 1-128 */
         *stack=*j-64*outer;
         
	 (*block)  = (*stack>24) ? 2 : 1;
         (*stack) -=(*block-1)*24;
         (*block) +=2*outer;
}

int ec1_maxi_(int *ncluster,int ind[],int indmax[])
{
int i, j,dum;
for(i=0;i<(*ncluster);i++) indmax[i]=i+1;
for(i=0;i<(*ncluster-1);i++)
  for(j=(*ncluster-1);j>i;j--)
          if(ind[indmax[j]-1] > ind[indmax[j-1]-1]) 
            {
              dum=indmax[j];
              indmax[j]= indmax[j-1];
              indmax[j-1]=dum ;
	    }
}


int ec1_findpair_(int *iupd,int *cluster,int *ncluster, int ind[],int indi[])
{
int i,j;
*iupd=0;
for(i=0;i<(*ncluster);i++)
   {
      if(abs(indi[i]-(*cluster))<=2)
       {
             *iupd +=1;
             ind[(*iupd)-1]=i+1;
       }
   }
}      

int ec1_sametdc_(int *ncluster, int ivtdc[], int indtdc[],int *ltdc1,int *ltdc2)
{
int i,jmin,jmax;
float dtdc,diff;
dtdc=9999.9;
        *ltdc1=0;
        *ltdc2=0;
for(i=0;i<((*ncluster)-1);i++)
   {
     jmin=indtdc[i]-1;
     jmax=indtdc[i+1]-1;
     diff=abs(ivtdc[jmax]-ivtdc[jmin]);
     if(dtdc>diff)
       {
        dtdc=diff;
        *ltdc1=i+1;
        *ltdc2=i+2;
      }
   }
     if(dtdc>ec1_control_.ec1_timeresol)
       {
        *ltdc1=0;
        *ltdc2=0;
      }
}

int clusindex_(int *pmtfired,int *ncluster,int *nlines,int *numblock
              ,float aa0[][4][40],int indi[],
              float depinl[],float depinr[],float stackml[],float stackmr[])
{
int i, j,icluster,iok,dum,ind[40];
float aind[40],adcsum,adcsum0,adcsuml0,adcsumr0,adcsum1,adcsuml1,adcsumr1,depcomm0,depcomm1;
/*
TODO: check that Cluster Center CC is not i+2,when i+1 is rejected
because of being adjacent (tail problem)
*/
 int icomm,delete;
 float ec1StackEnergyMin;
  ec1StackEnergyMin=
 (*numblock >1 ? ec1_control_.ec1stenmin_out :ec1_control_.ec1stenmin_in);
     *ncluster=0;

for(i=0;i<(*nlines);i++)
  {
     aind[(ind[i]=i+1)-1]=aa0[0][*numblock][i]*aa0[1][*numblock][i];
     if( aa0[0][*numblock][i]>=ec1_control_.ec1_threshold) *pmtfired +=1; 
     if( aa0[1][*numblock][i]>=ec1_control_.ec1_threshold) *pmtfired +=1; 
   }
for(i=0;i<(*nlines);i++)
 for(j=(*nlines-1);j>i;j--)
  {
          if(aind[ind[j]-1] > aind[ind[j-1]-1]) 
            {
              dum=ind[j];
              ind[j]= ind[j-1];
              ind[j-1]=dum ;
            }
        }


/* end of maximizing ----    starting the cluster search */

    i=0;
    do
   {
        icluster=ind[i];
        iok=1;
            for(j=0;j<(*ncluster);j++)
               if(abs(icluster-indi[j]) <=1) iok=0;
        if(iok)
/* there are no adjacent blocks */
	  {
        adcsum=aa0[0][*numblock][icluster-1]*aa0[1][*numblock][icluster-1];
        adcsum=sqrt(adcsum);
            if(adcsum > ec1StackEnergyMin)
                 {
                   indi[*ncluster]=icluster;
                   *ncluster +=1;
		 }
      }
/* end -there are no adjacent blocks */
       i=i+1;
      } while ((*ncluster)< ec1_control_.ec1Maxclust && adcsum>ec1StackEnergyMin && i<(*nlines));


/* calculate the total deposition depinl/r(i) in clusters  */
            for(i=0;i<(*ncluster);i++)
	      {
             delete=0;
             icluster=indi[i];
        adcsum0=aa0[0][*numblock][icluster-1];
        adcsum1=aa0[1][*numblock][icluster-1];
        if(icluster>1)      {
                            adcsuml0=aa0[0][*numblock][icluster-2];
                            adcsuml1=aa0[1][*numblock][icluster-2];
			  }
                      else  
                            {
                              adcsuml0=0;
                              adcsuml1=0;
                            }          
        if(icluster<(*nlines)) {
                            adcsumr0=aa0[0][*numblock][icluster];
                            adcsumr1=aa0[1][*numblock][icluster];
			  }
                      else
                            {
                              adcsumr0=0;
                              adcsumr1=0;
			    }
                  depinl[i]=adcsum0+adcsuml0+adcsumr0;
                  depinr[i]=adcsum1+adcsuml1+adcsumr1;
                  stackml[i]=(depinl[i]>0 ? (adcsumr0-adcsuml0)/depinl[i] : 0);
                  stackmr[i]=(depinr[i]>0 ? (adcsumr1-adcsuml1)/depinr[i] : 0);

/* check for dep energy sharing by adjacent clusters  */
             for(j=0;(j<i && !delete);j++)
          {
              if(abs(icluster-indi[j]) ==2 )
		{
                  icomm =(icluster+indi[j])/2;
     /* so far the sharing of adjacent clusters is 50x50 */
                  depcomm0=(aa0[0][*numblock][icomm-1]);
                  depcomm1=(aa0[1][*numblock][icomm-1]);
              if(depcomm0*depcomm1 > 
          (aa0[0][*numblock][icluster-1])*(aa0[1][*numblock][icluster-1])) delete=j+1;
              else{
                  depinl[i]= depinl[i]-0.5*depcomm0;
                  depinl[j]= depinl[j]-0.5*depcomm0;
                  depinr[i]= depinr[i]-0.5*depcomm1;
                  depinr[j]= depinr[j]-0.5*depcomm1;
                  }
	         }
	  }     /* end of check for dep energy sharing by adjacent clusters  */
/* remove deleted  cluster */
             if(delete) 
                   {
                      depinr[delete-1]= depinr[delete-1]+ depinr[i]-depcomm1;
                      depinl[delete-1]= depinl[delete-1]+ depinl[i]-depcomm0;
                      for(j=i;(j< (*ncluster -1 ));j++) 
                           {
                             indi[j]=indi[j+1];
                             depinl[j]=depinl[j+1];
                             depinr[j]=depinr[j+1];
                           }

                      *ncluster = *ncluster -1;
                       i--;
		    }
                       

	}    /* end of calculation of the total deposition depin(i) in clusters  */


}


int ec1calibcons()
{
int i,j,k;
    for(j=0;j<2;j++)
    for(i=0;i<4;i++)
    for(k=0;k<40;k++)
     {
       ec1_calibr_.ec1_cala1l[j][i][k]=ec1_control_.ec1_caliba1; 
       ec1_calibr_.ec1_cala2l[j][i][k]=ec1_control_.ec1_caliba2; 
       ec1_calibr_.ec1_calt1l[j][i][k]=ec1_control_.ec1_calibt1; 
       ec1_calibr_.ec1_calt2l[j][i][k]=ec1_control_.ec1_calibt2; 
       ec1_peds_.ec1_pedl[j][i][k]=0; 
       ec1_calibr_.ec1_cala1r[j][i][k]=ec1_control_.ec1_caliba1; 
       ec1_calibr_.ec1_cala2r[j][i][k]=ec1_control_.ec1_caliba2; 
       ec1_calibr_.ec1_calt1r[j][i][k]=ec1_control_.ec1_calibt1; 
       ec1_calibr_.ec1_calt2r[j][i][k]=ec1_control_.ec1_calibt2; 
       ec1_peds_.ec1_pedr[j][i][k]=0; 
      }
}
int  ec1calibmap_(int *runno)
{
int ec1_sector;
  char *dir,pedmap[100],calmap[100],tdcmap[100];

    dir = getenv("CLAS_PARMS");
  sprintf(pedmap,"%s/Maps/LAC_PEDESTALS.map",dir);
  sprintf(calmap,"%s/Maps/LAC_CALIB.map",dir);

  if(ec1_control_.ec1_reco_type==1) {
   sprintf(tdcmap,"%s/Maps/LAC_T0.map",dir);
  }else { 
   sprintf(tdcmap,"%s/Maps/LAC_TIMING.map",dir);
  }
  for(ec1_sector=1;ec1_sector<3;ec1_sector++){
   ec1_read_map(pedmap,calmap,tdcmap,*runno, ec1_sector);
  }
return 0;

}

int ec1_read_map(char *pmap, char *cmap, char *tmap, int runno, int ec1_sector ){
  float val_left[EC1_CHANNELS], val_right[EC1_CHANNELS];
  float val_c1l[EC1_CHANNELS], val_c1r[EC1_CHANNELS];
  float val_c2l[EC1_CHANNELS], val_c2r[EC1_CHANNELS];
  float val_t1l[EC1_CHANNELS], val_t1r[EC1_CHANNELS];
  float val_t2l[EC1_CHANNELS], val_t2r[EC1_CHANNELS];
  float waltw_al[EC1_CHANNELS],waltw_bl[EC1_CHANNELS];
  float waltw_ar[EC1_CHANNELS],waltw_br[EC1_CHANNELS];
  int error_code,firsttime=0;
  int  i,iblock,istack;
  char subsystem[]="sector0";

  switch(ec1_sector){
  case 1:
    sprintf(subsystem,"sector1");
    break;
  case 2:
    sprintf(subsystem,"sector2");
    break;
  }

      map_get_float(pmap,subsystem,"left", EC1_CHANNELS, val_left, runno, &firsttime);
      map_get_float(pmap,subsystem,"right", EC1_CHANNELS, val_right, runno, &firsttime);
      map_get_float(cmap,subsystem,"c1l", EC1_CHANNELS, val_c1l, runno, &firsttime);
      map_get_float(cmap,subsystem,"c1r", EC1_CHANNELS, val_c1r, runno, &firsttime);
      map_get_float(cmap,subsystem,"c2l", EC1_CHANNELS, val_c2l, runno, &firsttime);
      map_get_float(cmap,subsystem,"c2r", EC1_CHANNELS, val_c2r, runno, &firsttime);
    if(ec1_control_.ec1_reco_type==1){
      map_get_float(tmap,subsystem,"t1l", EC1_CHANNELS, val_t1l, runno, &firsttime);
      map_get_float(tmap,subsystem,"t1r", EC1_CHANNELS, val_t1r, runno, &firsttime);
      map_get_float(tmap,subsystem,"t2l", EC1_CHANNELS, val_t2l, runno, &firsttime);
      map_get_float(tmap,subsystem,"t2r", EC1_CHANNELS, val_t2r, runno, &firsttime);
    } else {
      map_get_float(tmap,subsystem,"c_l", EC1_CHANNELS, val_t1l, runno, &firsttime);
      map_get_float(tmap,subsystem,"c_r", EC1_CHANNELS, val_t1r, runno, &firsttime);
      map_get_float(tmap,subsystem,"d_l", EC1_CHANNELS, val_t2l, runno, &firsttime);
      map_get_float(tmap,subsystem,"d_r", EC1_CHANNELS, val_t2r, runno, &firsttime);
      map_get_float(tmap,subsystem,"tw_al", EC1_CHANNELS, waltw_al, runno, &firsttime);
      map_get_float(tmap,subsystem,"tw_ar", EC1_CHANNELS, waltw_ar, runno, &firsttime);
      map_get_float(tmap,subsystem,"tw_bl", EC1_CHANNELS, waltw_bl, runno, &firsttime);
      map_get_float(tmap,subsystem,"tw_br", EC1_CHANNELS, waltw_br, runno, &firsttime);
    }
  /* err -101 MAP_SYSTEM_ERROR_OPEN no file
          -4  MAP_USER_WARN_NOMATCH wrong file
    error_code=map_get_float(tmap,subsystem,"t1l", EC1_CHANNELS, val_t1l, runno, &firsttime);
  printf(" error in map subsystem,t1l %d %d \n",error_code,firsttime);
  */
  for(i=1;i<=EC1_CHANNELS; i++){
         ec1_j2sbl_(&i,&istack,&iblock);
    ec1_peds_.ec1_pedl[ec1_sector-1][iblock-1][istack-1] = (int)val_left[i-1];
    ec1_peds_.ec1_pedr[ec1_sector-1][iblock-1][istack-1] = (int)val_right[i-1];

    if(ec1_control_.ec1_reco_type==1){
    ec1_calibr_.ec1_caltw_al[ec1_sector-1][iblock-1][istack-1] =3.79;
    ec1_calibr_.ec1_caltw_ar[ec1_sector-1][iblock-1][istack-1] =3.79;
    ec1_calibr_.ec1_caltw_bl[ec1_sector-1][iblock-1][istack-1] =-560.0;
    ec1_calibr_.ec1_caltw_br[ec1_sector-1][iblock-1][istack-1] =-560.0;
    } else {
    ec1_calibr_.ec1_caltw_al[ec1_sector-1][iblock-1][istack-1] =waltw_al[i-1];
    ec1_calibr_.ec1_caltw_ar[ec1_sector-1][iblock-1][istack-1] =waltw_ar[i-1];
    ec1_calibr_.ec1_caltw_bl[ec1_sector-1][iblock-1][istack-1] =waltw_bl[i-1];
    ec1_calibr_.ec1_caltw_br[ec1_sector-1][iblock-1][istack-1] =waltw_br[i-1];
    }
    ec1_calibr_.ec1_cala1l[ec1_sector-1][iblock-1][istack-1] =val_c1l[i-1];
    ec1_calibr_.ec1_cala1r[ec1_sector-1][iblock-1][istack-1] =val_c1r[i-1];
    ec1_calibr_.ec1_cala2l[ec1_sector-1][iblock-1][istack-1] =val_c2l[i-1];
    ec1_calibr_.ec1_cala2r[ec1_sector-1][iblock-1][istack-1] =val_c2r[i-1];
    ec1_calibr_.ec1_calt1l[ec1_sector-1][iblock-1][istack-1] =val_t1l[i-1];
    ec1_calibr_.ec1_calt1r[ec1_sector-1][iblock-1][istack-1] =val_t1r[i-1];
    ec1_calibr_.ec1_calt2l[ec1_sector-1][iblock-1][istack-1] =val_t2l[i-1];
    ec1_calibr_.ec1_calt2r[ec1_sector-1][iblock-1][istack-1] =val_t2r[i-1];
  }
 
}

int ec1calibfile()
{
/*  for asci file reading part */
FILE *fpasci;
char pedfile[20];
int j,k,i,ped1,ped2;
float dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8;
/*  for asci file reading part 
        fpasci=fopen(pedfile, "r");   */
        fpasci=fopen("ec1calib.dat", "r");

              while (! feof(fpasci)) 
	  {
      fscanf(fpasci," %d  %d  %d " ,&j,&k,&i);
       if(k < 1 || k > 4 || i < 1 || i > 40)  fscanf(fpasci,"\n");
       else  
	 {
              fscanf(fpasci," %d %d %f %f %f %f %f %f %f %f  \n",
                 &ped1,&ped2,&dum1,&dum2,&dum3,&dum4,&dum5,&dum6,&dum7,&dum8);
       ec1_peds_.ec1_pedl[j-1][i-1][k-1]=ped1; 
       ec1_peds_.ec1_pedr[j-1][i-1][k-1]=ped2; 
       ec1_calibr_.ec1_cala1l[j-1][i-1][k-1]=dum1; 
       ec1_calibr_.ec1_cala1r[j-1][i-1][k-1]=dum2; 
       ec1_calibr_.ec1_cala2l[j-1][i-1][k-1]=dum3;
       ec1_calibr_.ec1_cala2r[j-1][i-1][k-1]=dum4;
       ec1_calibr_.ec1_calt1l[j-1][i-1][k-1]=dum5;
       ec1_calibr_.ec1_calt1r[j-1][i-1][k-1]=dum6;
       ec1_calibr_.ec1_calt2l[j-1][i-1][k-1]=dum7;
       ec1_calibr_.ec1_calt2r[j-1][i-1][k-1]=dum8;
	 }
       }
        fclose(fpasci);
}





