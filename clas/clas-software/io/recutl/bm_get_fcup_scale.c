#include <stdlib.h>
#include <stdio.h>

/* Function to return the count to cb scale for FCUP  */

float  bm_get_fcup_scale_( int *RunNumb );
float  bm_get_fcup_scale( int RunNumb );


float bm_get_fcup_scale_( int *RunNumb )
{
  return (float) bm_get_fcup_scale( *RunNumb );
}




float bm_get_fcup_scale( int RunNumb )
{
  char MapName[512];
  char* ParmsDir;
  int iFirst;
  float FCUP_Scale[1];


  ParmsDir = getenv( "CLAS_PARMS" );
  sprintf( MapName, "%s/Maps/beam.map", ParmsDir );

  map_get_float( MapName, "fcup", "scale", 1, (float*)FCUP_Scale, RunNumb, &iFirst );

  printf("FCUP scale factor for Run %d is %f \n", RunNumb, FCUP_Scale[0] ); 
  
  return (float) FCUP_Scale[0];
}


