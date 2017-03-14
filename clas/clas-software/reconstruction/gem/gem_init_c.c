/*      subroutine gem_init_c*/
/*
 *_begin_doc
 *  RCS ID string
 *  $Id: gem_init_c.c,v 1.3 2006/07/14 19:05:06 stepanya Exp $
*/
/*
 *  Documentation for subroutine gem_init_c
 *
 *  Purpose:
 *  --------
 *      initialize rtpc data structure (other than geometry)
 *
 *
 *  Calling Sequence:
 *  ----------------
 *
 *  Input Parameters:  (Name - Type - Meaning)
 *  ----------------
 *
 *  Output Parameters:  (Name - Type - Meaning) all in SCS coord system.
 *  -----------------
 
 *  Called from: gem_brun.F 
 *  ------------
 *
 
 *
 *  Notes:  The SCS coordinate system has the nominal target at the origin,
 *  ------  the z-axis along the beamline, the x-axis parallel in the midplane,
 *          y-axis ZxX = pointing up (for Sector 1) parallel to axial wires
 *          pointing towards the HV side (at least for R1).
 *
 *
 * Author: gabriel niculescu, 2005
 *  -------
 *
 * Major revisions:  created - 2005 GN.
 *                   updated for Production run 11-11-05 N. Baillie
 * ----------------

 *
 *_end_doc
 */
/*  RCS information: 
*/

#include "rtpc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include "map_manager.h"


#define USE(var) static void * use_##var = (void *) &var
static char crcsid[] =
   "$Id: gem_init_c.c,v 1.3 2006/07/14 19:05:06 stepanya Exp $";
USE(crcsid);   /* make sure it is not optimized away */
/*
 *   Module information:
 */
  static char crname[] = "GEM_INIT_C";
  static char crauth[] = "Gabriel Niculescu";
/*
 *_begin_inc
 *  include files :
 *  ---------------------
 */
/* BOS common block  uncomment the next line for BOS include file */
/*#include <bos.inc> */
/* _end_inc */
cylindrical rawCYL[NAL_SAMP][NUM_PADS];
void gem_init_c_(){
  /*
  FILE *voltHeFile;
  
  if((voltHeFile = fopen("voltHe.init","r"))==NULL)
    {
      printf("failed to open voltage/Helium initialization file\n");
      exit(1);
    }
  else 
    {
      //fgets(textline, LINLMAX, voltHeFile); //eat the first line
      printf("read voltage/Helium initialization file\n");
    }
  fscanf(voltHeFile,"%f %f %f", &Lvolt, &Rvolt, &he);
  printf("Left RTPC Voltage = %.0f, Right RTPC Voltage = %.0f, Percent Helium = %.0f\n",Lvolt,Rvolt,he);
  */
  //Lvolt = 1736;
  //Rvolt = 1703;

  /* we read value from database in routine  , should overwrite value below */
  sol_current = -999;  

  n_bb_pulser=0, n_tb_pulser=0, n_either_pulser=0, n_both_pulser=0;
  nal_samp = NAL_SAMP;
  nal_keep = NAL_KEEP;
  good_altro_data= FALSE;
  quiet=FALSE; 
  indicate_dead_channels=FALSE;
  fit_track_to_beamline = 0;
  qtot_early=0.0, qtot_intime= 0.0, qtot_late= 0.0;
  leftCand = 0;
  rightCand = 0;
  strt_bfore_bound = 0;
  strt_aftr_bound = 0;
  end_bfore_bound = 0;
  end_aftr_bound = 0;
  siei = 0;//start inside, end inside
  soei = 0;//start outside, end inside
  sieo = 0;//start inside, end outside
  soeo = 0;//start outside, end outside
  num_events = 0;
  
  return;
}
/*
*-------------------------------------------------------------------------
*/

void gem_read_database_(int* irun) {
  char map[255];
  static int firsttime;
  float caldb_value;
  float VKL;
  float VGL;
  float VKR;
  float VGR;

  /* try to read database value, which is given in mA */
  sprintf(map,"%s/Maps/RUN_CONTROL.map", getenv("CLAS_PARMS") );  
  map_get_float(map, "currents", "solenoid", 1, &caldb_value, *irun, &firsttime);
  if (caldb_value != 0) sol_current = caldb_value / -1000.;

  fprintf(stdout, "Solenoid Currend read from database for run %d: %12.3f A\n", *irun, sol_current); 
  
  if(!(sol_current == 450 || sol_current == 534))
    {
      fprintf(stdout, "MAJOR WARNING: RTPC is not calibrated for this solenoid current!\n");
    }

  readMagField();//reads in the solenoid field map.  sol_current must be set
                 //properly in the init_globals routine.

  /* try to read database VGL, which is given in V */
  sprintf(map,"%s/Maps/rtpc.map", getenv("CLAS_PARMS") );  
  map_get_float(map, "voltages", "gem_left_rbk", 1, &caldb_value, *irun, &firsttime);
  if (caldb_value != 0) VGL = caldb_value;
  else VGL = -999;

  fprintf(stdout, "Gem Voltage Left read from database for run %d: %12.0f V\n", *irun, VGL); 

  /* try to read database VGR, which is given in V */
  sprintf(map,"%s/Maps/rtpc.map", getenv("CLAS_PARMS") );  
  map_get_float(map, "voltages", "gem_right_rbk", 1, &caldb_value, *irun, &firsttime);
  if (caldb_value != 0) VGR = caldb_value;
  else VGR = -999;

  fprintf(stdout, "Gem Voltage Right read from database for run %d: %12.0f V\n", *irun, VGR);

  /* try to read database VKL, which is given in V */
  sprintf(map,"%s/Maps/rtpc.map", getenv("CLAS_PARMS") );  
  map_get_float(map, "voltages", "cathode_left_rbk", 1, &caldb_value, *irun, &firsttime);
  if (caldb_value != 0) VKL = caldb_value;
  else VKL = -999;

  fprintf(stdout, "Cathode Voltage Left read from database for run %d: %12.0f V\n", *irun, VKL);

  /* try to read database VKR, which is given in V */
  sprintf(map,"%s/Maps/rtpc.map", getenv("CLAS_PARMS") );  
  map_get_float(map, "voltages", "cathode_right_rbk", 1, &caldb_value, *irun, &firsttime);
  if (caldb_value != 0) VKR = caldb_value;
  else VKR = -999;

  fprintf(stdout, "Cathode Voltage Right read from database for run %d: %12.0f V\n", *irun, VKR);

  calcVolt(VGL, VGR, VKL, VKR);
  he=82;  
  init_paths();  //this routine initializes our time to space coordinate conversion

}
