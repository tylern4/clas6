/*
 * $Id: clas_step.c,v 1.10 2011/04/04 17:37:14 hyonsuk Exp $
 *
 * Documentation for subroutine clas_step.c
 *
 * Purpose:
 * --------
 *  Handle the calls from GEANTS gustep.F ( called at the end of each tracking step)
 *  it decides:
 * 
 *  1) Store Secondaries.
 *  2) Call Detector components for each detector as they are stepped in to.
 *
 * Notice: The 'Level' indicates the depth in the object tree a detector component
 * is in. Most active components will occupy several levels.
 *
 * Re-write the original clas_step.F by M. Guckes.
 * 
 * Author: Maurik Holtrop     Created: Sat April 28 23:05:00 EST 
 *
 *_end_doc
 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "ntypes.h"
#include "bostypes.h"
#include "kinematics.h"
#include "sc.h"
#include "geant_commons.h"
#include "gsim_c_common.h"
#include "sc_gsim.h"

static char cfile[] =  "$RCSfile: clas_step.c,v $";
static char crevis[] = "$Revision: 1.10 $";
static char cstate[] = "$State: Exp $";
static char cdate[] =  "$Date: 2011/04/04 17:37:14 $";
static char cautho[] = "$Author: hyonsuk $";
/*   Module information: */
static char crname[] = "clas_step.c";
static char crauth[] = "Maurik Holtrop";

#ifndef SQ
#define SQ(x) ((x)*(x))
#endif

extern FFPAR_t ffpar_;             /* FFREAD controls */
extern GSIMCONTROL_t gsimcontrol_; /* GSIM flags etc. */
extern GCTRAK_t  gctrak_;  /* Tracking parameters stored here.    */
extern GCVOLU_t  gcvolu_;   /* Names and volume stuff stored here. */
extern GCKING_t  gcking_;   /* Common that controls generation of secondaries. */
extern GCFLAG_t  gcflag_;   /* GEANT event control. */
extern GCFLAX_t  gcflax_;   /* GEANT event control#2. */
extern GCNUM_t   gcnum_;    /* GEANT - current numbers for some items. */
extern GCKINE_t  gckine_;   /* GEANT kinematics. */
extern GCTMED_t  gctmed_;   /* GEANT tracking media. */
extern GCTPOL_t  gctpol_;   /* GEANT tracking parameters for radiation proc. */
extern FFPAR_IC_t ffpar_ic_; /* IC controls */

int clas_gsking(int ivol, int nosec);
int clas_limits(void);
void track_debugger(void);

void step_ec1_(void);
void step_ec_(void);
void step_sc_(void);
void step_st_(void);
void step_dc_(void);
void step_cc_(void);
void step_ic1_(void);
void step_sh_(void);
void gpcxyz_(void);
void gsxyz_(void);
void gdcxyz_(void);
void gsking_(int *);

#ifdef DEBUG              /* These make for easy access of the COMMONS in a debugger. */
  FFPAR_t *ffpar_ptr;             /* FFREAD controls */
  GSIMCONTROL_t *gsimcontrol_ptr; /* GSIM flags etc. */
  GCTRAK_t  *gctrak_ptr;  /* Tracking parameters stored here.    */
  GCVOLU_t  *gcvolu_ptr;   /* Names and volume stuff stored here. */
  GCKING_t  *gcking_ptr;   /* Common that controls generation of secondaries. */
  GCFLAG_t  *gcflag_ptr;   /* GEANT event control. */
  GCNUM_t   *gcnum_ptr;
  GCKINE_t  *gckine_ptr;
#endif

void clas_step_(void){

  int i;
  int zero=0;
  char *tmpstr;
  int cf_inuse;

#ifdef DEBUG

  ffpar_ptr  = (FFPAR_t * )(&ffpar_);
  gsimcontrol_ptr = (GSIMCONTROL_t *)(&gsimcontrol_);
  gcvolu_ptr = (GCVOLU_t *)(&gcvolu_);
  gctrak_ptr = (GCTRAK_t  *)(&gctrak_);
  gcking_ptr = (GCKING_t  *)(&gcking_);
  gcflag_ptr = (GCFLAG_t  *)(&gcflag_);
  gcnum_ptr  = (GCNUM_t   *)(&gcnum_);
#endif


  clas_limits(); /* Out of bounds or too long ? */


/* Here we decide which detector routines need storing. 
 * The order in which items appear is not entirely arbitrary. I try to put
 * volumes that get many steps first, mostly "EC" "FOIL" "MINI"
 *
 * Current TREE:
 *   Level 1 = CLAS
 *   Level 2 = LAS-  FOIL  SC    ST EC1 DC
 *   Level 3 = LSS-  ...   SCIN  .. ....
 *   level 4 = EC CC MG MGSR .....
 */

//      printf("volumes %4s",gcvolu_.names[1]);
    /************** EC **************/

  if( gcvolu_.nlevel >= 4 && 
      strncmp(gcvolu_.names[3],"EC  ",4)==0){
      if (gsimcontrol_.fast_code[EC]){
	/*track_debugger(); */
	cfstep_();
	cfking_();
        gsking_(&zero);
        step_ec_();
      }
      else{
	/*track_debugger();*/
        clas_gsking(EC,gsimcontrol_.nosec[EC]); 
        step_ec_();
      }

    /************** FOIL **************/

  }else if( gcvolu_.nlevel >= 2 && 
	    strncmp(gcvolu_.names[1],"FOIL",4)==0){
    clas_gsking(FOIL,gsimcontrol_.nosec[FOIL]);

    /************** IC **************/

  }else if( gcvolu_.nlevel >= 2 && 
	   (strncmp(gcvolu_.names[2],"IC1 ",4)==0 || 
            strncmp(gcvolu_.names[3],"IC1 ",4)==0)){
   /* Hyon-Suk Jo - April 4, 2011 - Changed condition after moving IC1 */
   /* one level deeper (see geom_ic1.F). Left former level for flexibility. */

       gsimcontrol_.nosec[IC]=0;

    clas_gsking(IC,gsimcontrol_.nosec[IC]); 
    if (ffpar_ic_.ic_mod >= 2) step_ic1_(); 

    /************** SHodoscope **************/

  }else if( gcvolu_.nlevel >= 2 && 

           strncmp(gcvolu_.names[1],"SH  ",4)==0){

              gsimcontrol_.nosec[SH]=0;

    clas_gsking(SH,gsimcontrol_.nosec[SH]); 
           step_sh_(); 

    
    /************** MINI **************/

  }else if( gcvolu_.nlevel >= 2 && 
	    strncmp(gcvolu_.names[1],"MINI",4)==0){
    clas_gsking(MINI,gsimcontrol_.nosec[MINI]);

    /************** DC **************/

  }else if( gcvolu_.nlevel >= 2 && 
	    strncmp(gcvolu_.names[1],"DC",2)==0){ /* Matches DC1A DC1B DC2 DC3 */

    clas_gsking(DC,gsimcontrol_.nosec[DC]); 
    step_dc_();

    /************** EC1 **************/

  }else if( gcvolu_.nlevel>=2 && 
      strncmp(gcvolu_.names[1],"EC1 ",4)==0){ /* We're steppin in EC1 = LAC */
    
    clas_gsking(EC1,gsimcontrol_.nosec[EC1]); 
    step_ec1_();

    /************** SC **************/

  }else if( (gcvolu_.nlevel==2 || gcvolu_.nlevel==3 ) 
	    && strncmp(gcvolu_.names[1],"SC  ",4)==0){ 

    clas_gsking(SC,gsimcontrol_.nosec[SC]); 
    step_sc_();

    /************** CC **************/

  }else if( gcvolu_.nlevel >= 4 && 
	    strncmp(gcvolu_.names[3],"CC  ",4)==0){

    clas_gsking(CC,gsimcontrol_.nosec[CC]); 
    step_cc_();

    /************** TORU **************/

  }else if( gcvolu_.nlevel >= 4 && 
	    strncmp(gcvolu_.names[3],"MG  ",4)==0){ /* The actual torus only, MGSR is "other" */
    clas_gsking(TORU,gsimcontrol_.nosec[TORU]);

    /************** ST **************/
    
  }else if( gcvolu_.nlevel>=2 && 
	    strncmp(gcvolu_.names[1],"ST  ",4)==0){ /* We're steppin in ST = Start Counter. */

    clas_gsking(ST,gsimcontrol_.nosec[ST]); 
    step_st_();

    /************** PTG **************/

  }else if( gcvolu_.nlevel >= 2 && 
	    strncmp(gcvolu_.names[1],"PTGM",4)==0){
    clas_gsking(PTG,gsimcontrol_.nosec[PTG]);

    /************** TG **************/

  }else if( gcvolu_.nlevel >= 2 && 
	    strncmp(gcvolu_.names[1],"TARG",4)==0){
    clas_gsking(TG,gsimcontrol_.nosec[TG]);

    /************** OTHER **************/

  }else{                                      
    clas_gsking(OTHER,gsimcontrol_.nosec[OTHER]); /* Other volumes: We can make finer crontrol here.*/
  }

  /**************************************/


/* Here we have the debugging and interactive control through "switch" commands. */

  if(gcflag_.iswit[0] == 1 || gcflag_.iswit[0] == 3)gpcxyz_();  /* Dump the step to the screen in ASCII. */
  if(gcflag_.iswit[0] == 2 || gcflag_.iswit[0] == 3){
    gsxyz_(); 
    gdcxyz_();    /* Dump the step to the graphics screen. */
  } else if(!gcflax_.batch) {
    gsxyz_();
  }
  if(gcflag_.iswit[2] > 0){
    track_debugger();
  }

  return;

}


int clas_gsking(int ivol, int nosec){
/*
 * The gsking routine stores secondaries created in this step. We would like
 * somewhat finer control however. 
 *
 * 1) We ALWAYS store a decay particle.
 * 2) We store all secondaries if NOSEC was not set.
 * 3) We NEVER store a secondary neutrino (we wouldn't see em anyway.)
 *
 * If you put in some effort you can do fancy things here, however, remember that
 * this routine is called a bizillion times, so you want to keep it brief.
 *
 * Some of the ideas in this routine are due to Cole Smith.
 */
  int zero=0;
  int five=5;
  int inum, is;
  int i,j,it,iadd;
  int pid,nsec;
  float buff[5];

  if(gcking_.ngkine <=0 )return(0);  /* Nothing to do. */

  nsec = 0;
  if( strncmp(gcking_.kcase,"DCAY",4)==0 ){  /* Particles are decay products. */
    /* Store secondaries on stack and in JKINE structure. */
    /* Here we filter out unwanted particles, by testing the partid. */
    /* We only filter the NEUTRINO's = 4 */
    for(i=0;i< gcking_.ngkine; i++){    
      pid = (int)gcking_.gkin[i][4];
      if( pid == 4) gcking_.iflgk[i]= -1; 
      else {
	gcking_.iflgk[i]= 1;   /* ffpar_.iflgk_ff; */
	nsec++;
      }
    }
    gsking_(&zero);    
  } else {                   /* Other processes making secondaries. */
    if (!nosec) {
    /* here could be some flag specific code, e.g. for hadronic processes */ 
      for(i=0;i< gcking_.ngkine; i++){    
	gcking_.iflgk[i]= 1;   /* ffpar_.iflgk_ff; */    
      }
      nsec = gcking_.ngkine;
      gsking_(&zero);    
    } else {
      for(i=0;i< gcking_.ngkine; i++){    
	gcking_.iflgk[i]= -1;  /* discard them */ 
      }
    }
  }
  /* Store some info for secondaries: volume, medium, generating process */
  if(nsec) {
    is=0;
    inum=0;
    if (ivol==EC || ivol==CC || ivol==TORU) {
      is=gcvolu_.number[2];        /* sector number from LSS- volume */
    } else if (ivol==DC) {
      is=gcvolu_.number[1];        /* sector number from DC? region volumes */
      if (gcvolu_.nlevel>2) {
	if(strncmp(gcvolu_.names[2],"DS6",3)==0) {
	  inum=60; 
	} else if(strncmp(gcvolu_.names[2],"DS5",3)==0) {
	  inum=50; 
	} else if(strncmp(gcvolu_.names[2],"DS4",3)==0) {
	  inum=40; 
	} else if(strncmp(gcvolu_.names[2],"DS3",3)==0) {
	  inum=30; 
	} else if(strncmp(gcvolu_.names[2],"DS2",3)==0) {
	  inum=20; 
	} else if(strncmp(gcvolu_.names[2],"DS1",3)==0) {
	  inum=10; 
	}
	if (gcvolu_.nlevel>3) inum+=gcvolu_.number[3];
      }
    } else if (ivol==ST) {
      is=gcvolu_.number[1] *2 -1;
      if (gcvolu_.nlevel>2) is +=gcvolu_.number[2] -1;
    } else if (ivol==SC) {
      is=gcvolu_.number[1];
      if (gcvolu_.nlevel>2) inum=gcvolu_.number[2];
    } else if (ivol==EC1) {
      is=gcvolu_.number[1];
    }
    ivol++;

    for(i=0; i< gcking_.ngkine; i++) {
      if( (it=gcking_.iflgk[i]) > 0 ) {
	buff[0]=(float)gckine_.itra;
	buff[1]=(float)gctmed_.numed;
	buff[2]=(float)(ivol*10+is);
	buff[3]=(float)inum;
	buff[4]=0.0;
        for(j=0; j<MAXMEC && strncmp(gcking_.kcase,gctrak_.namec[j],4); j++) ;
	if(j<MAXMEC) {
	  buff[4]=(float)(j+1);
	} else {
	  for(j=0; j<MAXME1 && strncmp(gcking_.kcase,gctpol_.namec1[j],4); j++) ;
	  if(j<MAXME1) buff[4]=(float)(MAXMEC+j+1);
	}
	iadd=0;
	gskinu_(&it,&five,buff,&iadd);
      }
    }
  }
  return(nsec);
}


int clas_limits(){

  /* This routine should be fine tuned to make the cut-off closer to the actual
   * CLAS limits. Possibility: detect stepping out of last volume. 
   */

  int stop_track;
  float radius;

  stop_track=0;
  
#define X gctrak_.vect[0]
#define Y gctrak_.vect[1]
#define Z gctrak_.vect[2]

  if( Z < ffpar_.zmin_ff ) stop_track =1;
  else if( Z  > ffpar_.zmax_ff) stop_track =1;
  else if( gctrak_.sleng > ffpar_.slengmax_ff ) stop_track=1;
  else if ( gctrak_.nstep > ffpar_.nstepmax_ff ) stop_track =1;
  else if((radius=sqrt(SQ(X)+SQ(Y)))> ffpar_.rmax_ff ) stop_track = 1;  
  else if( Z > 422. - 0.47368 *(radius - 450.) )stop_track =1;

/* The line of the outside of the EC is given by: 
 * Z = Z1 + (R-R1)*(Z2-Z1)/(R2-R1) =>
 * Z = 420. + (R -450.)*((600.-420.)/(70. - 450.))
 * This "should" move as the Forward Carriage does, but the EC code doesn't do that yet
 * so we don't here either. I add 2 cm so you see at least a little track outside EC. */

  if(stop_track>0 && gctrak_.istop == 0) gctrak_.istop = 1; 

/* Stop the track: NOTETIVE that you don't set istop to 0, since it may already have been
 * set by GEANT. */

  return(stop_track);
  
}
									
void track_debugger(){

  int i,ip;
  int zero=0,idummy=0;
  float fdummy;
  static int   step_total=0;
  static float enter_path_len=0;
  char tmpstr[21];

  if(step_total==0 ){
    printf("\n[Trk# partname   ]   Volumes                             Materials            (step# ) [ Pathlen, Delta  ] Kin_e         Processes.\n");
  }

  step_total++;

  if(gctrak_.inwvol == 1 ){
    enter_path_len = gctrak_.sleng;
  }

  if( ( gcflag_.iswit[2]==1 && (gctrak_.inwvol == 2 || gctrak_.nstep == 0 ) ) || 
      ( gcflag_.iswit[2]==2 && gctrak_.inwvol > 0  ) ||
      ( gcflag_.iswit[2]==3 && ( gctrak_.inwvol > 0 || gctrak_.istop >0 )) ||
      ( gcflag_.iswit[2]==4 )
      ){ 


    strncpy(tmpstr,gckine_.napart,12);
    tmpstr[12]=0;
    printf("[%2d %2d %8s]: ",gckine_.itra,gckine_.istak,tmpstr);
    for(i=0;i<7;i++){
      if(i<gcvolu_.nlevel){
	strncpy(tmpstr,gcvolu_.names[i],4);
	tmpstr[4]=0;
	printf("%4s ",tmpstr);
      }else{
	printf("     ");
      }
    }

    printf("%20s ",gctmed_.natmed);

    printf("(%6d) [%8.4f,%8.4f] E: %7.3f %3s ",gctrak_.nstep,gctrak_.sleng,gctrak_.sleng-enter_path_len,
	   (gctrak_.gekin < .001 ? gctrak_.gekin*1000000. : gctrak_.gekin*1000.),
	   (gctrak_.gekin < .001 ? "KeV" : "MeV"));
    for(i=0;i< gctrak_.nmec && i<MAXMEC;i++){
      strncpy(tmpstr,gctrak_.namec[gctrak_.lmec[i]-1],4);
      tmpstr[4]=0;
      printf("%4s ",tmpstr);
    }
    printf("\n");
  }
  
  if(gcflag_.iswit[2] >0 && (gctrak_.inwvol == 3 || gctrak_.istop>0)){ /* Leaving the system or decay... */
    printf("\nTrack %3d total # of steps: %4d\n",gckine_.itra,gctrak_.nstep);
    printf("Secondaries [%d] = ", gcking_.ngkine);
    for(i=0;i<gcking_.ngkine;i++){
      ip = (int)gcking_.gkin[i][4];
      tmpstr[20]=0;
      gfpart_(&ip,tmpstr,&idummy,&fdummy,&fdummy,&fdummy,&fdummy,&idummy,20); 
      fix_f_string(tmpstr);  /* this routine defined in gsim_bos.c for now. */
      printf("%s ",tmpstr);  
    }
    printf("\n");
    step_total=0;
  }
}

#include <unistd.h>
#include <ctype.h>

int fix_f_string(char *str){

  int i;

  i=0;
  while(i<strlen(str) && isalnum(str[i]))i++;
  str[i]='\0';
  return(i);
}


