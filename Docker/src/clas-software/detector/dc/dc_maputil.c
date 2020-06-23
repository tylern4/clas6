/*#include<mapmanager.h>*/
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ntypes.h>
#include <bostypes.h>
#include <makebanks.h>

/*------------prototypes------------------*/

/*--utility functions--*/
int dc_sector(int index);
int dc_region(int index);
int dc_index(int sector, int layer);

/*--Create the DC_GEOM map(only invoked once per experiment!)--*/ 
int create_DC_map(char *map);

/*--retrieve information(from C)--*/
int get_DCalign_map_arrays(char *map, int runno, float xalign[], float yalign[], float zalign[]);
int get_toroid_map_arrays(char *map, int runno, float xt[], float st[]);
int get_DCrot_map_arrays( char *map, int runno,float xrot[], float yrot[], float zrot[]);

/*--FORTRAN callable routine used in RECSIS--*/
int get_dc_map_geom_(int *runno, float xpos[3][6][3], float spos[3][6][3], float *xt, float *st, int ok[3][6]);
int dc_read_map_int_(int *runno, char *map_f, char *item_f, int arr[6][36][192], int ok_sect[6], int maplen_f, int itemlen_f);
int dc_read_map_float_(int *runno, char *map_f, char *item_f, float arr[6][36][192], int ok_sect[6], int maplen_f, int itemlen_f);

/*--write to the map--*/
int put_DCalign_map_arrays(char *map, int runno, float xalign[], float yalign[], float zalign[]);
int put_toroid_map_arrays(char *map, int runno, float xt[], float st[]);
int put_DCrot_map_arrays( char *map, int runno,float xrot[], float yrot[], float zrot[]);


/*--remove geometry for a run--*/
int remove_toroid_geom(char *map, int runno);
int remove_dc_geom(char *map, int runno);

/*--read/write contents of map to/from ascii--*/
void write_dc_text(FILE *fop, int runno, float xt[], float st[], float xalign[], float yalign[], float zalign[], float xrot[], float yrot[], float zrot[]);
void read_dc_text(FILE *fop, int *runno, float xt[], float st[], float xalign[], float yalign[], float zalign[], float xrot[], float yrot[], float zrot[]);

/*--test routine, how to pass arrays from C to FORTRAN--*/
int testmap_(float xpos[3][6][3]);

/*-------source code-------------------------------------------*/

/*remember sector zero in C is really sector 1*/
int dc_sector(int index){
  return(index%6);
}

/*remember region zero in C is really region 1*/
int dc_region(int index){
  return(index/6);
}

int dc_index(int sector, int layer){
  return(layer*6 + sector);
}

int get_DCalign_map_arrays(char *map, int runno, float xalign[], float yalign[], float zalign[]){
  int firsttime;

  map_get_float(map,"dc", "xalign", 18, xalign, runno, &firsttime);
  if (firsttime < 0 ) return( 0);
  map_get_float(map,"dc", "yalign", 18, yalign, runno, &firsttime);
  if (firsttime < 0 ) return( 0);
  map_get_float(map,"dc", "zalign", 18, zalign, runno, &firsttime);
  if (firsttime < 0 ) return( 0);

  return(1);
}

void write_dc_text(FILE *fop, int runno, float xt[], float st[], float xalign[],
 float yalign[], float zalign[], float xrot[], float yrot[], float zrot[]){
  int i;

  fprintf(fop, "DC geometry for run: %d\n", runno);
  fprintf(fop, "Toriod align(xyz):  %3.4f  %3.4f  %3.4f\n", xt[0], xt[1], xt[2])
; 
  fprintf(fop, "Toriod rot(xyz):  %3.4f  %3.4f  %3.4f\n", st[0], st[1], st[2]);
  for(i=0; i<18; i++){
    fprintf(fop, "DC align sec:%1d reg:%1d (xyz):  %3.4f  %3.4f  %3.4f\n", (dc_sector(i)+1), (dc_region(i)+1), xalign[i], yalign[i], zalign[i]);
  }
  for(i=0; i<18; i++){
    fprintf(fop, "DC rot sec:%1d reg:%1d (xyz):  %3.4f  %3.4f  %3.4f\n", (dc_sector(i)+1), (dc_region(i)+1), xrot[i], yrot[i], zrot[i]);
  }

}

int get_toroid_map_arrays(char *map, int runno, float xt[],float st[]){
  int firsttime;

  map_get_float(map,"toroid", "align", 3, xt, runno, &firsttime);
  if (firsttime < 0 ) return( 0);
  map_get_float(map,"toroid", "rot", 3, st, runno, &firsttime);
  if (firsttime < 0 ) return( 0);
  return(1);
}

int get_DCrot_map_arrays( char *map, int runno,float xrot[], float yrot[], float zrot[]){
  int firsttime;

  map_get_float(map,"dc", "xrot", 18, xrot, runno, &firsttime);
  if (firsttime < 0 ) return( 0);
  map_get_float(map,"dc", "yrot", 18, yrot, runno, &firsttime);
  if (firsttime < 0 ) return( 0);
  map_get_float(map,"dc", "zrot", 18, zrot, runno, &firsttime);
  if (firsttime < 0 ) return( 0);

  return(1);
}

int get_dc_map_geom_(int *runno, float xpos[3][6][3], float spos[3][6][3], float *xt, float *st, int ok[3][6]){
  int sec, layer;
  float xalign[18], yalign[18], zalign[18];
  float xrot[18], yrot[18], zrot[18];
  char *dir, *map, def_map[100];

  dir = getenv("CLAS_PARMS");
  sprintf(def_map,"%s/Maps/DC_GEOM.map",dir);
  map = &def_map[0];


  if (get_DCalign_map_arrays(map, *runno, xalign, yalign, zalign) && 
      get_DCrot_map_arrays(map, *runno, xrot, yrot, zrot) ){
    /*set ok equal to one - map load successful*/
    for(sec=0; sec<6; sec++){
      for(layer=0; layer<3; layer++)
	ok[layer][sec] = 1;
    }
  } else {
    /*set ok equal to zero - map load failed*/
    for(sec=0; sec<6; sec++){
      for(layer=0; layer<3; layer++)
	ok[layer][sec] = 0;
    }
  }

  get_toroid_map_arrays(map, *runno, xt, st);
  
  for(layer=0; layer<3; layer++){
    for(sec=0; sec<6; sec++) {
      xpos[layer][sec][0] = xalign[dc_index(sec, layer)]; 
      xpos[layer][sec][1] = yalign[dc_index(sec, layer)]; 
      xpos[layer][sec][2] = zalign[dc_index(sec, layer)]; 
      
      spos[layer][sec][0] = xrot[dc_index(sec, layer)]; 
      spos[layer][sec][1] = yrot[dc_index(sec, layer)]; 
      spos[layer][sec][2] = zrot[dc_index(sec, layer)]; 
      
    }
  }
  
  return(ok[0][0]);
}


int testmap_(float xpos[3][6][3]){

  xpos[0][0][0] = 1;
  xpos[1][0][0] = 2;
  xpos[2][0][0] = 3;

}


int remove_toroid_geom(char *map, int runno){
  map_rem_arr(map, "toroid", "align",runno);
  map_rem_arr(map, "toroid", "rot",runno);
  return(1);
}

int remove_dc_geom(char *map, int runno){
  map_rem_arr(map, "dc", "xalign", runno);
  map_rem_arr(map, "dc", "yalign", runno);
  map_rem_arr(map, "dc", "zalign", runno);
  map_rem_arr(map, "dc", "xrot", runno);
  map_rem_arr(map, "dc", "yrot", runno);
  map_rem_arr(map, "dc", "zrot", runno);
  return(1);
}

void read_dc_text(FILE *fop, int *runno, float xt[], float st[], float xalign[], float yalign[], float zalign[], float xrot[], float yrot[], float zrot[]){
  int i;
  char string[100];

  fscanf(fop, "DC geometry for run: %d\n", runno);
  fscanf(fop, "Toriod align(xyz): %f %f %f\n", &xt[0], &xt[1], &xt[2]); 
  fscanf(fop, "Toriod rot(xyz): %f %f %f\n", &st[0], &st[1], &st[2]);
  
  for(i=0; i<18; i++){
    sprintf(string, "DC align sec:%1d reg:%1d (xyz): %%f %%f %%f\n",(dc_sector(i)+1), (dc_region(i)+1)); 
    fscanf(fop, string,  &xalign[i], &yalign[i], &zalign[i]);
  }
  for(i=0; i<18; i++){
    sprintf(string, "DC rot sec:%1d reg:%1d (xyz): %%f %%f %%f\n",(dc_sector(i)+1), (dc_region(i)+1)); 
    fscanf(fop, string, &xrot[i], &yrot[i], &zrot[i]);
  }
  
}

int create_DC_map(char *map){
  /* this is how I made the map*/
  /*sector and region coded by an index*/
  /* use function dc_sector(index), dc_region(index) and index(sector, region)
     to figure things out */

  map_create(map);
  map_add_item(map,"toroid", "align", 3, 1);
  map_add_item(map,"toroid", "rot", 3, 1);

  map_add_item(map,"dc", "xalign", 18, 1);
  map_add_item(map,"dc", "yalign", 18, 1);
  map_add_item(map,"dc", "zalign", 18, 1);
  map_add_item(map,"dc", "xrot", 18, 1);
  map_add_item(map,"dc", "yrot", 18, 1);
  map_add_item(map,"dc", "zrot", 18, 1);
  return(1);
}


int put_toroid_map_arrays(char *map, int runno, float xt[], float st[]){

  
  map_put_float(map,"toroid", "align", 3, xt, runno);
  map_put_float(map,"toroid", "rot", 3, st, runno);
  fprintf(stderr, "runno = %d\n", runno);
  return(1);
}


int put_DCalign_map_arrays(char *map, int runno, float xalign[], float yalign[], float zalign[]){

  map_put_float(map,"dc", "xalign", 18, xalign, runno);
  map_put_float(map,"dc", "yalign", 18, yalign, runno);
  map_put_float(map,"dc", "zalign", 18, zalign, runno);

  return(1);
}

int put_DCrot_map_arrays( char *map, int runno,float xrot[], float yrot[], float zrot[]){
  map_put_float(map,"dc", "xrot", 18, xrot, runno);
  map_put_float(map,"dc", "yrot", 18, yrot, runno);
  map_put_float(map,"dc", "zrot", 18, zrot, runno);

  return(1);
}


int dc_read_map_float_(int *runno, char *map_f, char *item_f, float arr[6][36][192], int ok_sect[6], int maplen_f, int itemlen_f){
  char subsys[50];
  char item_c[50], map_c[50];
  char *dir, def_map[100];
  float itemarr[DC_WIRES_SEC];
  int sec, layer, wire, firsttime, i;
  
  /*convert fortan to C character strings*/
  strncpy(map_c,map_f, maplen_f);
  map_c[maplen_f] = '\0';
  strncpy(item_c,item_f, itemlen_f);
  item_c[itemlen_f] = '\0';  
  /*get the map name/location*/
  dir = getenv("CLAS_PARMS");
  sprintf(def_map,"%s/Maps/%s.map",dir, map_c);

  for(sec=0; sec<6; sec++){
    sprintf(subsys,"sector%d",sec+1);
    if(map_get_float(def_map, subsys, item_c, DC_WIRES_SEC, itemarr, *runno, &firsttime) == 0) {
      ok_sect[sec]=1;
    } else {
      /* if map failure load with zeroes*/
      ok_sect[sec]=0;
      for(i=0; i< DC_WIRES_SEC; i++) itemarr[i] = 0; 
    }
    for(layer=0; layer<36; layer++){
      for(wire=0; wire<192; wire++){
	arr[sec][layer][wire] = itemarr[layer*192+wire];
      }
    }
  }

}

int dc_read_map_int_(int *runno, char *map_f, char *item_f, int arr[6][36][192], int ok_sect[6], int maplen_f, int itemlen_f){
  char subsys[50];
  char item_c[50], map_c[50];
  char *dir, def_map[100];
  int itemarr[DC_WIRES_SEC];
  int sec, layer, wire, firsttime, i;
  
  /*convert fortan to C character strings*/
  strncpy(map_c,map_f, maplen_f);
  map_c[maplen_f] = '\0';
  strncpy(item_c,item_f, itemlen_f);
  item_c[itemlen_f] = '\0';  
  /*get the map name/location*/
  dir = getenv("CLAS_PARMS");
  sprintf(def_map,"%s/Maps/%s.map",dir, map_c);

  for(sec=0; sec<6; sec++){
    sprintf(subsys,"sector%d",sec+1);
    if(map_get_int(def_map, subsys, item_c, DC_WIRES_SEC, itemarr, *runno, &firsttime) == 0){
      /*success in loading sector*/
      ok_sect[sec]=1;
    } else {
      /* if map failure load with zeroes*/
      ok_sect[sec]=0;
      for(i=0; i< DC_WIRES_SEC; i++) itemarr[i] = 0; 
    }
    for(layer=0; layer<36; layer++){
      for(wire=0; wire<192; wire++){
	arr[sec][layer][wire] = itemarr[layer*192+wire];
      }
    }
  }
  return(1);
}



