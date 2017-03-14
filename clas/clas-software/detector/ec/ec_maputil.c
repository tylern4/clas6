#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ntypes.h>
#include <bostypes.h>
#include <ec.h>
#include <makebanks.h>
#ifdef vaxc
#include <file.h>                   /* file and descriptor definitions      */
#else
#include <fcntl.h>
#endif

char *map_file = "EC_CALIB";

/* initialize the ec to handle MC and data */
int ec_begin_run(int runno){
  int one = 1;
  clasHEAD_t *HEAD = getBank(&bcs_, "HEAD");

  if (HEAD){
    if (HEAD->head[0].type >= 0) {
      /* data */
      ec_brun_(&runno);
    } else {
      /*monte carlo - do something else */
      //ec_brun_(&runno);
      ec_brun_(&one);
    }
  }
}


/*the EcCal common*/
typedef struct EcCal {
float EcCal_Atten[NSEC][EC_LAYERS_MAX][EC_ORIENTATION][EC_STRIPS_MAX];
float EcCal_Ech[NSEC][EC_LAYERS_MAX][EC_ORIENTATION][EC_STRIPS_MAX];
float EcCal_Eo[NSEC][EC_LAYERS_MAX][EC_ORIENTATION][EC_STRIPS_MAX];
float EcCal_Tch[NSEC][EC_LAYERS_MAX-1][EC_ORIENTATION][EC_STRIPS_MAX];
float EcCal_To[NSEC][EC_LAYERS_MAX-1][EC_ORIENTATION][EC_STRIPS_MAX];
float EcCal_Tadc[NSEC][EC_LAYERS_MAX-1][EC_ORIENTATION][EC_STRIPS_MAX];
float EcCal_dT1[NSEC][EC_LAYERS_MAX-1][EC_ORIENTATION][EC_STRIPS_MAX];
float EcCal_dT2[NSEC][EC_LAYERS_MAX-1][EC_ORIENTATION][EC_STRIPS_MAX];
float EcCal_Trms[NSEC][EC_LAYERS_MAX-1][EC_ORIENTATION][EC_STRIPS_MAX];
float EcCal_TDCstat[NSEC][EC_LAYERS_MAX-1][EC_ORIENTATION][EC_STRIPS_MAX];
} EcCal_t;

/*let fortran allocate the memory*/
extern EcCal_t eccal_;

/* EcCal is set up like [sector][layer:whole,inner,outer][orientation:u,v,w]
   [channel] so the pedestal for sector 1, inner layer, w orientation,
   channel 14 is... eccal_.EcCal_Eo[0][0][2][13] */


/*remember sector zero in C is really sector 1*/
int ec_channel(int index){
  return(index%36);
}

/*remember region zero in C is really region 1*/
int ec_sector(int index){
  return(index/36);
}

int ec_index(int sector, int channel){
  return(sector*36 + channel);
}

int ec_strip(int id)
{
  /* strip is 1-36, we are returning a number from 0-35 ala C programming language */
  return((id & 255) - 1);
}
int ec_layer(int id)
{
  int ret;
  /* layer, ala C, is 0,1 for inner,outer */
  int k = id/256;
  switch (k) {
  case 1:
  case 2:
  case 3:
    ret = 1;
    break;
  case 4:
  case 5:
  case 6:
    ret = 2;
    break;
  default:
    ret = -1;
    break;
  }
  return(ret);
}

int ec_orientation(int id)
{
  int ret;
  /* orientation is, ala C, 0,1,2 for u,v,w */
  int k = id/256;
  switch (k) {
  case 1:
  case 4:
    ret = 0;
    break;
  case 2:
  case 5:
     ret = 1;
    break; 
  case 3:
  case 6:
     ret = 2;
    break;
  default:
    ret = -1;
    break;
  }
  return(ret);
}


/* Generic read routine for Bogdan with slightly different output format
   - does not fill the common directly*/
int ec_readmap_(int *runno, float ec_common[6][6][36], char *map_f, int string){
  int i, firsttime;
  char *dir, def_map[100], map_c[50];
  char  map_layer[16], map_orient[16];
  int layer, orientation;
  float values[EC_CHANNELS_PER_LAYER];

  strncpy(map_c,map_f, string);
  map_c[string] = '\0';  /*terminate string*/
  if (dir = getenv("CLAS_PARMS")){
    sprintf(def_map,"%s/Maps/%s.map",dir, map_c);

    /* no error checking at the moment*/
    for(layer=1; layer <3; layer++){
      layer_string(layer, map_layer);
      for(orientation=U_EC; orientation<EC_ORIENTATION; orientation++){
	orient_string(orientation, map_orient);
	map_get_float(def_map,map_layer,map_orient,EC_CHANNELS_PER_LAYER,values, *runno, &firsttime);
	for(i=0; i< EC_CHANNELS_PER_LAYER; i++){
	  ec_common[ec_sector(i)][(layer-1)*3 + orientation][ec_channel(i)] = values[i];
	}
      }
    }
    return(SUCCESS);
  } else {
    return(FAILURE);
  }

}



/* Generic read routine - does not fill the common directly*/
int ec_read_adc_map_(int *runno, float ec_common[NSEC][EC_LAYERS][EC_ORIENTATION][EC_STRIPS], char *subsystem_f, float *def_value, int string){

  char subsystem_c[50];

  strncpy(subsystem_c, subsystem_f, string);
  subsystem_c[string] = '\0'; /* terminate string */

  return ec_read_adc_map(*runno,ec_common,subsystem_c,*def_value);
  /*
  if(dir = getenv("CLAS_PARMS") ){
    sprintf(def_map,"%s/Maps/%s.map",dir,map_file);

    /* no map error checking at the moment relying on the map 
     itself to tell you if it failed*/
  /*    for(layer=WHOLE; layer<EC_LAYERS; layer++) {
      for(orientation=U_EC; orientation<EC_ORIENTATION; orientation++) {
	item_id=layer*10+orientation;
	ec_item_string(item_id, item);

	map_get_float(def_map,subsystem_c,item,EC_CHANNELS_PER_LAYER,values, *runno, &firsttime);
	for(i=0; i<EC_CHANNELS_PER_LAYER; i++) {
	  ec_common[ec_sector(i)][layer][orientation][ec_channel(i)] = values[i];
	}
      }
    }
    return(SUCCESS);
  } else {
    return(FAILURE);
  }
  */
}


/* Generic read routine - does not fill the common directly*/
int ec_read_map_(int *runno, float ec_common[NSEC][EC_LAYERS][EC_ORIENTATION][EC_STRIPS], char *subsystem_f, float *def_value, int string){

  char subsystem_c[50];

  strncpy(subsystem_c, subsystem_f, string);
  subsystem_c[string] = '\0'; /* terminate string */

  return ec_read_map(*runno,ec_common,subsystem_c,*def_value);

  /*
  if(dir = getenv("CLAS_PARMS") ){
    sprintf(def_map,"%s/Maps/%s.map",dir,map_file);


    /* no map error checking at the moment relying on the map 
     itself to tell you if it failed*/
  /*    for(layer=WHOLE; layer<EC_LAYERS; layer++) {
      for(orientation=U_EC; orientation<EC_ORIENTATION; orientation++) {

	item_id=layer*10+orientation;
	ec_item_string(item_id, item);

	map_get_float(def_map,subsystem_c,item,EC_CHANNELS_PER_LAYER,values, *runno, &firsttime);
	for(i=0; i<EC_CHANNELS_PER_LAYER; i++) {
	  ec_common[ec_sector(i)][layer][orientation][ec_channel(i)] = values[i];
	}
      }
    }
    return(SUCCESS);
  } else {
    return(FAILURE);
  }
  */
}


/* Generic read routine - does not fill the common directly*/
int ec_read_map(int runno, float ec_common[NSEC][EC_LAYERS][EC_ORIENTATION][EC_STRIPS], char *subsystem_c, float def_value){

  int i, firsttime;
  char *dir, def_map[100];
  char item[16], map_orient[16];
  int layer, orientation;
  float values[EC_CHANNELS_PER_LAYER], *fval;
  int item_id, ierr;

  fval=&ec_common[0][0][0][0];
  for( i=0; i<NSEC*EC_LAYERS*EC_ORIENTATION*EC_STRIPS; i++)
    fval[i]=def_value;

  //  memset(ec_common,def_value,sizeof(float)*NSEC*EC_LAYERS*EC_ORIENTATION*EC_STRIPS);

  if(dir = getenv("CLAS_PARMS") ){
    sprintf(def_map,"%s/Maps/%s.map",dir,map_file);

    /* no map error checking at the moment relying on the map 
     itself to tell you if it failed*/
    for(layer=WHOLE; layer<EC_LAYERS; layer++) {
      for(orientation=U_EC; orientation<EC_ORIENTATION; orientation++) {

	item_id=layer*10+orientation;
	ec_item_string(item_id, item);

	ierr=map_get_float(def_map,subsystem_c,item,EC_CHANNELS_PER_LAYER,values, runno, &firsttime);
	if( !ierr ){
	  for(i=0; i<EC_CHANNELS_PER_LAYER; i++) {
	    ec_common[ec_sector(i)][layer][orientation][ec_channel(i)] = values[i];
	  }
	}
      }
    }
    return(SUCCESS);
  } else {
    return(FAILURE);
  }

}


int ec_read_adc_map(int runno, float ec_common[NSEC][EC_LAYERS][EC_ORIENTATION][EC_STRIPS], char *subsystem_c, float def_value)
{
  int isec,istrip;
  int i, firsttime;
  char *dir, def_map[100];
  char  map_layer[16], map_orient[16];
  int layer, orientation;
  float values[EC_CHANNELS_PER_LAYER], *fval;
  int item_id, ierr;
  char item[16];

  fval=&ec_common[0][0][0][0];
  for(i=0; i<NSEC*EC_LAYERS*EC_ORIENTATION*EC_STRIPS; i++)
    fval[i]=def_value;

  //  memset(ec_common,def_value,sizeof(float)*NSEC*EC_LAYERS*EC_ORIENTATION*EC_STRIPS);

  if(dir = getenv("CLAS_PARMS") ){
    sprintf(def_map,"%s/Maps/%s.map",dir, map_file);

    /* no map error checking at the moment relying on the map 
       itself to tell you if it failed*/
    for(layer=WHOLE; layer<EC_LAYERS; layer++){
      for(orientation=U_EC; orientation<EC_ORIENTATION; orientation++){

	item_id=layer*10+orientation;
	ec_item_string(item_id, item);

	ierr=map_get_float(def_map,subsystem_c,item,EC_CHANNELS_PER_LAYER,values, runno, &firsttime);
	if( !ierr ){
	  for (i=0,isec=0; isec<NSEC; ++isec) {
	    for  (istrip=0; istrip<EC_STRIPS;++istrip) {
	      ec_common[isec][layer][orientation][istrip] = values[i++];
	    }
	  }
	}

      }
    }
    return(SUCCESS);
  } 
  else {
    return(FAILURE);
  }

}


int ec_read_tdc_map(int runno, float ec_common[NSEC][EC_LAYERS-1][EC_ORIENTATION][EC_STRIPS], char *subsystem_c, float def_value)
{
  int sector, strip, layer, orientation;
  int i, firsttime;
  char *dir, def_map[100];
  char map_sector[16], map_parameter[16], item[16];
  float values[EC_CHANNELS_PER_LAYER], *fval;
  int item_id, ierr;

  fval=&ec_common[0][0][0][0];
  for(i=0; i<NSEC*(EC_LAYERS-1)*EC_ORIENTATION*EC_STRIPS; i++)
    fval[i]=def_value;

  //  memset(ec_common,def_value,sizeof(float)*NSEC*(EC_LAYERS-1)*EC_ORIENTATION*EC_STRIPS);

  if(dir = getenv("CLAS_PARMS") ){
    sprintf(def_map,"%s/Maps/%s.map",dir, map_file);

    /* no map error checking at the moment relying on the map 
       itself to tell you if it failed*/
    for (layer=INNER; layer<EC_LAYERS; layer++) {

      for(orientation=U_EC; orientation<EC_ORIENTATION; orientation++) {

	item_id=layer*10+orientation;
	ec_item_string(item_id, item);

	ierr=map_get_float(def_map,subsystem_c,item,EC_CHANNELS_PER_LAYER,values, runno, &firsttime);
	if(!ierr) {
	  for(i=0; i<EC_CHANNELS_PER_LAYER; i++) {
	    ec_common[ec_sector(i)][layer-1][orientation][ec_channel(i)] = values[i];
	  }
	}
      }
    }
    return(SUCCESS);
  } else {
    return(FAILURE);
  }


}


int ec_read_tdc_map_(int *runno, float ec_common[NSEC][EC_LAYERS-1][EC_ORIENTATION][EC_STRIPS], char *subsystem_f, float *def_value, int string)
{
  char subsystem_c[50];

  strncpy(subsystem_c,subsystem_f, string);
  subsystem_c[string] = '\0';  /*terminate string*/

  return ec_read_tdc_map(*runno,ec_common,subsystem_c,*def_value);
  /*  if(dir = getenv("CLAS_PARMS") ){
    sprintf(def_map,"%s/Maps/%s.map",dir,map_file);

    /* no map error checking at the moment relying on the map 
       itself to tell you if it failed*/
  /*    for (layer=INNER; layer<EC_LAYERS; layer++) {
      for (orientation=U_EC; orientation<EC_ORIENTATION; orientation++) {

	item_id=layer*10+orientation;
	ec_item_string(item_id, item);

	map_get_float(def_map,subsystem_c,item,EC_CHANNELS_PER_LAYER,values, *runno, &firsttime);
	for (i=0; i<EC_CHANNELS_PER_LAYER; i++) {
	  ec_common[ec_sector(i)][layer-1][orientation][ec_channel(i)] = values[i];
	}
      }
    }
    return(SUCCESS);
  } else {
    return(FAILURE);
  }
  */
}

int ec_put_tdc_map_(int *runno, float ec_common[6][2][3][36], char *subsystem_f, int string)
{
  int sector, channel, layer, orientation;
  int i, item_id;
  char def_map[100], subsystem_c[50], item[16];
  float values[EC_CHANNELS_PER_LAYER];

  sprintf(def_map,"%s_%d.map", map_file, *runno);

  strncpy(subsystem_c,subsystem_f, string);
  subsystem_c[string] = '\0';  /*terminate string*/

  for (layer=INNER; layer<EC_LAYERS; layer++) {
     for (orientation=U_EC; orientation<EC_ORIENTATION; orientation++) {

      item_id=layer*10+orientation;
      ec_item_string(item_id, item);

      for (i=0, sector=0; sector<NSEC; sector++) {
	for (channel=0; channel<EC_STRIPS_MAX; channel++) {
	  values[i++]=ec_common[sector][layer-1][orientation][channel];
	}
      }
      map_put_float(def_map, subsystem_c, item, EC_CHANNELS_PER_LAYER, values, *runno);
    }

  }

}

int ec_create_tdc_map_(int *runno) {

  int sector, strip, layer, orientation;
  int i, item_id, subsystem_id;
  char def_map[100], subsystem[50], item[16];
  int arraytype = 1; /* float */
  int fd;

  sprintf(def_map,"%s_%d.map", map_file, *runno);

  fd = open(def_map, O_RDONLY);
  if ( fd != -1 ) {
    /* fprintf(stderr, "The Map (%s) already exits and will be overwriting.\n", def_map); */
      map_close_map(fd);
      ec_rem_tdc_map_array(*runno, def_map);
    }
  else {
    map_create(def_map);    /* create a map if it does not exists */

    for (subsystem_id=0; subsystem_id<6; subsystem_id++) {

      ec_tdc_subsystem_string(subsystem_id, subsystem);

      for (layer=INNER; layer<EC_LAYERS; layer++) {
	for (orientation=U_EC; orientation<EC_ORIENTATION; orientation++) {

	  item_id=layer*10+orientation;
	  ec_item_string(item_id, item);

	  map_add_item(def_map, subsystem, item, EC_CHANNELS_PER_LAYER, arraytype);

	}
      }
    }
  }

}

int ec_rem_tdc_map_array(int runno, char *map_c) {

  int subsystem_id;
  int item_id;
  int layer, orientation;
  char item[16], subsystem[50];

  for (subsystem_id=0; subsystem_id < 6; subsystem_id++) {
    ec_tdc_subsystem_string(subsystem_id, subsystem);
    for (layer=INNER; layer<EC_LAYERS; layer++) {
      for(orientation=U_EC; orientation<EC_ORIENTATION; orientation++) {

	item_id=layer*10+orientation;
	ec_item_string(item_id, item);

	map_rem_arr(map_c, subsystem, item, runno);
      }
    }
  }
}

int ec_tdc_subsystem_string(int id, char *subsystem) {

  switch(id) {
  case 0:
    sprintf(subsystem, "EC_Tch");
    break;
  case 1:
    sprintf(subsystem, "EC_To");
    break;
  case 2:
    sprintf(subsystem, "EC_Tadc");
    break;
  case 3:
    sprintf(subsystem, "EC_dT1");
    break;
  case 4:
    sprintf(subsystem, "EC_dT2");
    break;
  case 5:
    sprintf(subsystem, "EC_Trms");
    break;
  case 6:
    sprintf(subsystem, "TDCstat");
    break;
  }

}

int ec_item_string(int id, char *item) {

  switch(id) {
  case 0:
    sprintf(item, "WholeU");
    break;
  case 1:
    sprintf(item, "WholeV");
    break;
  case 2:
    sprintf(item, "WholeW");
    break;
  case 10:
    sprintf(item, "InnerU");
    break;
  case 11:
    sprintf(item, "InnerV");
    break;
  case 12:
    sprintf(item, "InnerW");
    break;
  case 20:
    sprintf(item, "OuterU");
    break;
  case 21:
    sprintf(item, "OuterV");
    break;
  case 22:
    sprintf(item, "OuterW");
    break;
  }

}

int layer_string(int layer, char *map_layer){

  switch(layer){
  case 0:
    sprintf(map_layer,"whole");
    break;
  case 1:
    sprintf(map_layer,"inner");
    break;
  case 2:
    sprintf(map_layer,"outer");
    break;
  }

}

int orient_string(int orient, char *map_orient){

  switch(orient){
  case 0:
    sprintf(map_orient,"u");
    break;
  case 1:
    sprintf(map_orient,"v");
    break;
  case 2:
    sprintf(map_orient,"w");
    break;
  }

}


int print_ec_map(FILE *fp,char *name,float ec_common[NSEC][EC_LAYERS][EC_ORIENTATION][EC_STRIPS])
{
  int ilayer,iorient,isec,istrip;
  char nlayer[100];
  char orient[100];
  fprintf(fp,"EC %s Calibration constants\n",name);

  for (isec = 0; isec < NSEC; ++isec) {
    for (ilayer = 1; ilayer<EC_LAYERS; ++ilayer) {
      for (iorient = 0; iorient<EC_ORIENTATION; ++iorient) {
	layer_string(ilayer,nlayer);
	orient_string(iorient,orient);
	fprintf(fp,"%1d %s%s ",isec+1,nlayer,orient);
	for (istrip=0; istrip<EC_STRIPS; ++istrip) {
	  if (!(istrip%12))
	    putc('\n',fp);
	  fprintf(fp,"%f ",ec_common[isec][ilayer][iorient][istrip]);
	}
	putc('\n',fp);
      }
    }
  }
}


