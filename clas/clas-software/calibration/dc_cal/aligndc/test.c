#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ntypes.h>
#include <bostypes.h>
#include "aligndc.h"

int main(int argc, char *argv[])
{
  int i;
  short int j;
  char k;
  unsigned int ui;
  unsigned short int uj;
  unsigned char uk;
  hit_t temphit;
  track_t tmptrack;
  float f;
  float angsize[8][6];

  printf("size of int : %d,  short int : %d,  char : %d,\n"
	 "  unsigned int : %d,  unsigned short int : %d,  unsigned char : %d\n",
	 sizeof i, sizeof j, sizeof k, sizeof ui, sizeof uj, sizeof uk);
  printf("sizeof float: %d\n",sizeof f);
  printf("sizeof ptr: %d\n",sizeof tmptrack.next);
  printf("sizeof float array[8][6]: %d\n",sizeof angsize);
  
  printf("size of hit_t : %d,   size of track_t : %d\n",
	 sizeof temphit, sizeof tmptrack);
  return;
}
