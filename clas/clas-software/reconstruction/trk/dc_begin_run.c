#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <makebanks.h>
#include <dc.h>
#include <trk_run_control.h>


int dc_begin_run_(int *runno){
  dc_begin_run(*runno);
}

int dc_begin_run(int runno){
  clasHEAD_t *HEAD = getBank(&bcs_, "HEAD");
  int force = 1;
  float position[3];

  /*initialize geometry */
  /* always allow geometry to vary with run-number
     regardless if MC or not*/
  dc_make_dgeo_(&runno, &force);
  dc_geom_banks_();
  StConfigGeom(&runno);

  if (HEAD){
    if (HEAD->head[0].type >= 0) {
      /* set torus and minitorus currents from the map */
      if (trktcl_.ipar_torus_current==-9999)
	trktcl_.ipar_torus_current = get_map_current("torus", runno);
      if (trktcl_.ipar_minitorus_current==-9999)
	trktcl_.ipar_minitorus_current = get_map_current("minitorus", runno);
      if (runno>=14150 && runno<=15930 && trktcl_.ipar_poltarget_current==-9999)
	trktcl_.ipar_poltarget_current = get_map_current("ptm", runno);
      if (trktcl_.ipar_poltarget_current==-9999)
	trktcl_.ipar_poltarget_current = 0;

      /* data */
      configure_dc(runno);
    } else {
      /* torus and minitorus currents must come from tcl*/
      /*monte carlo use run-number 1 for defaults*/
      /* must get dc_xvst choice and currents from tcl common*/
      if (trktcl_.ipar_torus_current==-9999)
	trktcl_.ipar_torus_current = 0;
      if (trktcl_.ipar_minitorus_current==-9999)
	trktcl_.ipar_minitorus_current = 0;
      if (trktcl_.ipar_poltarget_current==-9999)
	trktcl_.ipar_poltarget_current = 0;
      configure_dc(1);
    }

	  get_map_position("target",runno,position);
      if (trktcl_.dpar_TargetPos[2]==-9999.)
		trktcl_.dpar_TargetPos[2]=position[2];
	  fprintf(stderr,"Target Z %f\n",trktcl_.dpar_TargetPos[2]);


	  if(abs(trktcl_.ipar_poltarget_current) > 0) {
	    fprintf(stderr,"dc_begin_run: poltarget_current=%d \n",trktcl_.ipar_poltarget_current);

          fprintf(stderr,"Target Magnet position from init: %f %f %f \n",trktcl_.dpar_TargetMagPos[0],trktcl_.dpar_TargetMagPos[1],trktcl_.dpar_TargetMagPos[2]);
         get_map_position("targmagnet",runno,position);
	  if (trktcl_.dpar_TargetMagPos[2]==-9999.)  {
           trktcl_.dpar_TargetMagPos[2]=position[2];
          }

          fprintf(stderr,"Target Magnet position from Map: %f %f %f \n",position[0],position[1],position[2]);
	  fprintf(stderr,"Target Magnet position: %f %f %f angles: %f %f %f\n",
           trktcl_.dpar_TargetMagPos[0],trktcl_.dpar_TargetMagPos[1],trktcl_.dpar_TargetMagPos[2],
           trktcl_.dpar_TargetMagAng[0],trktcl_.dpar_TargetMagAng[1],trktcl_.dpar_TargetMagAng[2]);
	  }
	  
	  get_map_position("beam",runno,position);
      if (trktcl_.dpar_BeamOff[0]==-9999.)
		trktcl_.dpar_BeamOff[0]=position[0];
      if (trktcl_.dpar_BeamOff[1]==-9999.)
		trktcl_.dpar_BeamOff[1]=position[1];
	  fprintf(stderr,"Beam offset (x,y) %f  %f\n",trktcl_.dpar_BeamOff[0],trktcl_.dpar_BeamOff[1]);
  }
}

int configure_dc(int runno){
  int sec;
  /* clasRUNC_t *RUNC = NULL;*/

  /*set up prlink filename fields*/
  trk_run_control_();

  /* read the Map and make the DDLY banks*/
  for(sec=1; sec<=6; sec++) make_DDLY_bank(runno,sec);

  /* converts the nice DDLY BOS bank to a common block
     hopefully will be obsolete some day */
  ddly2dc_cal_tdly_(); 

  /*initialize DC translation tables*/
  init_trk_trans(runno); 

  /* set up xvst relations for option 2*/
  dc_xvst_init_(&runno); 

  trk_geom_(); 

  /* load magnetic field table */
  sda_minit_();
  
  /* load in roads - prlink file */
  if(trktcl_.ipar_trk_make_prlink == 3)
  {
    /*prinit_();*/
  }
  else
  {
    sda_dinit_();
  }

  /* initialize tracking statistics*/
  trk_set_stat_(&runno);

}






