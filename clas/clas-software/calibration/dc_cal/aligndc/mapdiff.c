/*
 * mapdiff  -- reads in two DC_GEOM text files and writes out
 *             map1 - map2 to stdout
 */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>


/* stuff outside that I need -- read in map, etc */
int get_DCalign_map_arrays(const char *map, int runno,
				  float xalign[], float yalign[], float zalign[]);

int get_DCrot_map_arrays(const char *map, int runno,
				float xrot[], float yrot[], float zrot[]);

int get_toriod_map_arrays(const char *map, int runno,
				 float xt[], float st[]);

void write_dc_text(FILE *fop, int runno, float xt[], float st[],
		   float xalign[], float yalign[], float zalign[],
		   float xrot[], float yrot[], float zrot[]);

void read_dc_text(FILE *fop, int *runno, float xt[], float st[],
		  float xalign[], float yalign[], float zalign[],
		  float xrot[], float yrot[], float zrot[]);

/* code for this program */

void PrintUsage(char *command)
{
  fprintf(stderr,"Usage:  %s dcmap1 dcmap2\n\n",command);
  fprintf(stderr,"Reads in 2 text files dcmap1 and dcmap2 and writes to stdout\n"
	  "the difference, (dcmap1 - dcmap2).\n\n");

  exit(1);
}

int main(int argc, char *argv[])
{
  FILE *fp1, *fp2, *fpout = stdout;
  char *dcmap1;
  char *dcmap2;

  float xt[3], st[3];
  float xalign[18], yalign[18], zalign[18];
  float xrot[18], yrot[18], zrot[18];

  int runno1;
  float xt1[3], st1[3];
  float xalign1[18], yalign1[18], zalign1[18];
  float xrot1[18], yrot1[18], zrot1[18];

  int runno2;
  float xt2[3], st2[3];
  float xalign2[18], yalign2[18], zalign2[18];
  float xrot2[18], yrot2[18], zrot2[18];

  int i;

  /* make sure there are enough arguements */
  if (argc != 3) PrintUsage(argv[0]);

  dcmap1 = argv[1];
  dcmap2 = argv[2];

  if ( (fp1 = fopen(dcmap1,"r")) && (fp2 = fopen(dcmap2,"r") ) ) {
    read_dc_text(fp1,&runno1,xt1,st1,xalign1,yalign1,zalign1,xrot1,yrot1,zrot1);
    read_dc_text(fp2,&runno2,xt2,st2,xalign2,yalign2,zalign2,xrot2,yrot2,zrot2);
    fclose(fp1);
    fclose(fp2);
    for (i = 0; i < 3; i++) {
      xt[i] = xt1[i] - xt2[i];
      st[i] = st1[i] - st2[i];
    }

    for (i = 0; i < 18; i++) {
      xalign[i] = xalign1[i] - xalign2[i];
      yalign[i] = yalign1[i] - yalign2[i];
      zalign[i] = zalign1[i] - zalign2[i];
      xrot[i] = xrot1[i] - xrot2[i];
      yrot[i] = yrot1[i] - yrot2[i];
      zrot[i] = zrot1[i] - zrot2[i];
    }

    write_dc_text(fpout,runno1,xt,st,xalign,yalign,zalign,xrot,yrot,zrot);
  }
  else {
    fprintf(stderr,"Could open files %s and %s\n\n",dcmap1,dcmap2);
  }
  return 0;
}



