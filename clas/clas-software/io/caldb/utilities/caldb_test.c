#include <stdio.h>

int main () {

  char systemname[64], subsystemname[64], itemname[64];
  int arraylength=36, run=10000, firsttime, status;
  float farray[36];
  
  printf("hello world\n");
  status = map_get_float("/group/clas/parms/Maps/DC_DOCA.map", "t_max", "Sector6", arraylength, farray,
			 run, &firsttime);
  printf("%f %f %f\n", farray[0], farray[1], farray[2]);
  farray[0] = farray[0] - 1.0;
  status = map_put_float("/group/clas/parms/Maps/DC_DOCA.map", "t_max", "Sector6", arraylength, farray,
			 run, firsttime);
}
