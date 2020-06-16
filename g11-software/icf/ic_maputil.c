#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ic.h>
int ic_read_map_float_(int *runno_ic ,int *longueur , float *result_float_ic , char *subsystem_ic , char *item_ic);
int ic_read_map_int_(int *runno_ic , int *longueur , int *result_float_ic , char *subsystem_ic , char *item_ic);

int ic_read_map_float_(int *runno_ic , int *longueur , float *result_float_ic , char *subsystem_ic , char *item_ic)
{

  int time,i;
  float *values_float_ic;
  char *dir_ic, def_map_ic[100];
    
   if( (values_float_ic = (float *) malloc(longueur[0]*sizeof(float)))==NULL)
    {
     printf("Error allocating memory in ic_read_map_float...");
     return(1);
    }
   
  if(dir_ic = getenv("CLAS_PARMS") ) {
    sprintf(def_map_ic,"%s/Maps/IC.map",dir_ic);

    time=-99;
    if(map_get_float(def_map_ic,subsystem_ic,item_ic,longueur[0],values_float_ic,*runno_ic,&time)!=0)
      {
       printf("Error in map_get_float...");
        printf("path Map=%s\n",def_map_ic);
       return(1);
      }

    for(i=0;i<length_ic;i++) 
        result_float_ic[i]=values_float_ic[i];
    free(values_float_ic);
    return(0);
  } 
  else{
    printf(" Error trying to read the Map : %s \n",dir_ic);
    return(1);
      }
}


int ic_read_map_int_(int *runno_ic , int *longueur , int *result_int_ic , char *subsystem_ic , char *item_ic)
{

  int time,i;
  int *values_int_ic;
  char *dir_ic, def_map_ic[100];
   
    if( (values_int_ic = (int *) malloc(longueur[0]*sizeof(int)))==NULL)
    {
     printf("Error allocating memory in ic_read_map_int...");
     return(1);
    }
   
  
  if(dir_ic = getenv("CLAS_PARMS") ) {
    sprintf(def_map_ic,"%s/Maps/IC.map",dir_ic);
    time=-99;

    if(map_get_int(def_map_ic,subsystem_ic,item_ic,longueur[0],values_int_ic,*runno_ic,&time)!=0)
    {
     printf("Error in map_get_int...");
     printf("path Map=%s\n",def_map_ic);
     return(1);
    }
    for(i=0;i<length_ic;i++) 
       result_int_ic[i]=values_int_ic[i];
      free(values_int_ic);
    return(0);
  } 
  else{
    printf(" Error trying to read the Map : %s \n",dir_ic);
    return(1);
      }
}
