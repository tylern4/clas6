#include <string.h>
#include <stdio.h>
#include <stdlib.h>
int get_current_(int *runno, char *item_f, int *current, int itemlen);
int get_real_map_item_(int *runno, char *map_f, char *sub_f, char *item_f, float *array, int *arraylen, int itemlen, int maplen, int sublen);

int get_current_(int *runno, char *item_f, int *current, int itemlen){
  char item_c[50], def_map[100];
  char *dir;
  float real_current[1];
  int firsttime;

  strncpy(item_c, item_f, itemlen);
  item_c[itemlen] = '\0';
  dir = getenv("CLAS_PARMS");
  sprintf(def_map, "%s/Maps/RUN_CONTROL.map", dir);
  map_get_float(def_map, "currents", item_c, 1, real_current, *runno, &firsttime);
  /* what a sack of *!@! why is the MAGNET CURRENT an integer in RECSIS */
  *current = (int)real_current[0];

}


/*Super generic wrapper for map_get_float*/
int get_real_map_item_(int *runno, char *map_f, char *sub_f, char *item_f, float *array, int *arraylen, int maplen, int sublen, int itemlen){
  char item_c[50], map_c[100], sub_c[100], def_map[100];
  char *dir;
  int firsttime;

  /* convert FORTRAN character strings into C char strings*/
  strncpy(item_c, item_f, itemlen);
  item_c[itemlen] = '\0';
  strncpy(sub_c, sub_f, sublen);
  sub_c[sublen] = '\0';
  strncpy(map_c, map_f, maplen);
  map_c[maplen] = '\0';

  /*get the location of the map directory*/
  dir = getenv("CLAS_PARMS");
  sprintf(def_map, "%s/Maps/%s.map", dir, map_c);


  map_get_float(def_map, sub_c, item_c, *arraylen, array, *runno, &firsttime);


}
