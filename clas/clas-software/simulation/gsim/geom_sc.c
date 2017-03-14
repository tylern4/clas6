/*
 * $Id: geom_sc.c,v 1.8 2008/12/10 18:55:09 nerses Exp $
 *
 * Documentation for subroutine geom_sc.c
 *
 * Purpose:
 * --------
 *  Place all 48 scintilators is each of the 6 sectors.
 *
 * Re-write of the sc geometry in c, with the actual position for 
 * each scintillator.
 * 
 * Author: Maurik Holtrop     Created: Sat Mar 7 06:45:00 EST 
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
#include "scGlobal.h"
#include "gsim_c_common.h"  
#include "geant_commons.h"  /* Gets a the geant commons as structures. */
#include "sc_gsim.h"

static char cfile[] =  "$RCSfile: geom_sc.c,v $";
static char crevis[] = "$Revision: 1.8 $";
static char cstate[] = "$State: Exp $";
static char cdate[] =  "$Date: 2008/12/10 18:55:09 $";
static char cautho[] = "$Author: nerses $";
/*   Module information: */
static char crname[] = "geom_sc.c";
static char crauth[] = "Maurik Holtrop";

/* Routine templates. */

float check_panel_angles(SCpanel_t *P);
void  fill_scgeom_common(SC_geom_t *G);

/* Fortran COMMON blocks used in this file. */

extern SCTMED_t sctmed_;
extern GSIMCONTROL_t gsimcontrol_;
extern GCFLAG_t  gcflag_;

int FIRST_SPLIT_PADDLE; /* First paddle that has ganged strips: set in geom_sc.c */
int NUM_GEOM; /* number of paddles in SCG bank */
/* after 10/2007 initialized with 57 paddles per sector, 
  but only entries for 48 paddles are provided for previous geometry */


/* Stub for calling from FORTRAN. */

void geom_sc_(){
  geom_sc();
}

void geom_sc(){

  char tmpstr[80];
  SC_geom_t Geom;
  int i,j;
  int isec,ipanel,ipaddle;                                 /* Loop variables. */
  int insc=0,inpb=0,inst=0,infm=0;                         /* Counters to count volumes. */
  int tmed,ivol;                                           /* medium index and volume index. */
  float ParaSc[3],ParaIn[3],ParaPb[3],ParaSt[3],ParaFm[3]; /* Parameters of volumes SCIN, SCPB, SCST, SCFM */
  int izero=0,ithree=3;                                    /* To call FORTRAN, some constants. */
  int irot;                                                /* Index to rotation matrix. */
  float fzero=0;
  float delta_z;
  vector3_t rm_diff;

  sprintf(tmpstr," New SC Geometry for 6 independent sectors.\n"); 
  recmes_(crname,"I",tmpstr,strlen(crname),1,strlen(tmpstr)); 

  if( !gsimcontrol_.fast_code[SC] ){
    sprintf(tmpstr," Using the SLOW CODE !.\n"); 
    recmes_(crname,"I",tmpstr,strlen(crname),1,strlen(tmpstr)); 
  }

  set_sc_version_constants(gcflag_.idrun);
  FIRST_SPLIT_PADDLE = (get_sc_version_flag(gcflag_.idrun)==2)? 57 : 39;

  /* #define USE_NOMINAL_BANK */
#ifdef USE_NOMINAL_BANK

  {
    char mess[200];
    char *dir,bosfile[100];
    int lun=42;

    dir = getenv("CLAS_PARMS");
    sprintf(bosfile,"%s/sc_geom.bfp.old_nominal.3.5.98",dir); 
    sprintf(mess,"OPEN SCGFILE UNIT=%d FILE=\"%s\" READ", lun, bosfile);
    fparm_c(mess);
    getBOS(&wcs_,lun,"T");
  }
#else
  
  make_SCG_banks(gcflag_.idrun);  
  sprintf(tmpstr," Geometry initialized with runnumber %d\n",gcflag_.idrun); 
  recmes_(crname,"I",tmpstr,strlen(crname),1,strlen(tmpstr));   
#endif

  calib_sc_init_();

  /* Define the Mother volume and Scint volumes. */

  if( gsimcontrol_.helium_only){
    tmed = sctmed_.sc_med_helium;
  }else{
    tmed = sctmed_.sc_med_air;
  }
  gsvolu_("SC  ","PGON",&tmed,Geom.sc_parm,&izero,&ivol,4,4);
  gsvolu_("SCIN","BOX ",&sctmed_.sc_med_scint,ParaIn,&izero,&ivol,4,4);

  if( !gsimcontrol_.fast_code[SC]){  /* Extra volumes for detail. */
    gsvolu_("SCPB","BOX ",&sctmed_.sc_med_lead ,ParaPb,&izero,&ivol,4,4);
    gsvolu_("SCST","BOX ",&sctmed_.sc_med_steel,ParaSt,&izero,&ivol,4,4);
    gsvolu_("SCFM","BOX ",&sctmed_.sc_med_foam ,ParaFm,&izero,&ivol,4,4);
  }

  for(isec = 1; isec <=6; isec++){  /* Loop over all 6 sectors */
    geom_sc_init(isec,&Geom);

    if(isec==1)  fill_scgeom_common(&Geom); /* Fill common with SEC 1 */
    
    /* Place the mother volume, parameters were calculated in the geom_sc_init routine. */    

    gsposp_("SC  ",&isec,"CLAS",&fzero,&fzero,&fzero,&izero,"ONLY",Geom.sc_parm,&Geom.sc_parm_num,4,4,4);

    /* Each strip needs to be rotated according to which of the (4x6) 24 panels it is in. */
    /* Rotation matrix is calculated from the angles of the 3 ortho body vectors. */

    for(ipanel=0; ipanel<MAX_PANEL; ipanel++){

      irot = get_rotm_();  /* Sort of a GEANT creation operator. */
      gsrotm_(&irot,
	      &Geom.panel[ipanel].tht[0],&Geom.panel[ipanel].phi[0],
	      &Geom.panel[ipanel].tht[1],&Geom.panel[ipanel].phi[1],
	      &Geom.panel[ipanel].tht[2],&Geom.panel[ipanel].phi[2]);
      
      for(ipaddle= Geom.panel[ipanel].ifirst; ipaddle <= Geom.panel[ipanel].ilast; ipaddle++){
	ParaSc[0] = Geom.Dim[ipaddle].width/2.;
	ParaSc[1] = Geom.Dim[ipaddle].length/2.;
	ParaSc[2] = Geom.Dim[ipaddle].thick/2.;

	insc++;
	gsposp_("SCIN",&insc,"SC  ",&Geom.Dim[ipaddle].rm.x,&Geom.Dim[ipaddle].rm.y,&Geom.Dim[ipaddle].rm.z,
		&irot,"ONLY",ParaSc,&ithree,4,4,4);


	if( !gsimcontrol_.fast_code[SC]){
	  ParaPb[0] = ParaSc[0];
	  ParaPb[1] = ParaSc[1];
	  ParaPb[2] = 0.00635;    /* Thin Layer of lead. */
	  
	  delta_z = -ParaSc[2] - ParaPb[2] - 0.006; /* Move inward by 1/2 the SCIN and 1/2 the SCPB. */
	  
	  rm_diff = v3add(Geom.Dim[ipaddle].rm,v3mult(delta_z,Geom.panel[ipanel].zo));
	  
	  inpb++;
	  gsposp_("SCPB",&inpb,"SC  ",&rm_diff.x,&rm_diff.y,&rm_diff.z,&irot,"ONLY",ParaPb,&ithree,4,4,4);
	  
	  ParaSt[0] = 0.95*ParaSc[0];
	  ParaSt[1] = ParaSc[1];
	  ParaSt[2] = 0.076;    /* Layer of Steel. */
	  
	  delta_z = ParaSc[2] + ParaSt[2] + 0.01; /* Move outward by 1/2 the SCIN and 1/2 the SCST. */
	  
	  rm_diff = v3add(Geom.Dim[ipaddle].rm,v3mult(delta_z,Geom.panel[ipanel].zo));
	  
	  inst++;
	  gsposp_("SCST",&inst,"SC  ",&rm_diff.x,&rm_diff.y,&rm_diff.z,&irot,"ONLY",ParaSt,&ithree,4,4,4);
	  
	  ParaFm[0] = 0.95*ParaSc[0];
	  ParaFm[1] = ParaSc[1];
	  ParaFm[2] = 1.27;    /* Layer of Foam. */
	  
	  delta_z += ParaSt[2] + ParaFm[2] + 0.01; /* Move outward by 1/2 the SCST and 1/2 the SCFM. */
	  
	  rm_diff = v3add(Geom.Dim[ipaddle].rm,v3mult(delta_z,Geom.panel[ipanel].zo));
	  
	  infm++;
	  gsposp_("SCFM",&infm,"SC  ",&rm_diff.x,&rm_diff.y,&rm_diff.z,&irot,"ONLY",ParaFm,&ithree,4,4,4);
	  
	  
	  delta_z += ParaFm[2] + ParaSt[2] + 0.01; /* Move outward by 1/2 the SCFM and 1/2 the SCST. */
	  
	  rm_diff = v3add(Geom.Dim[ipaddle].rm,v3mult(delta_z,Geom.panel[ipanel].zo));
	  
	  inst++;
	  gsposp_("SCST",&inst,"SC  ",&rm_diff.x,&rm_diff.y,&rm_diff.z,&irot,"ONLY",ParaSt,&ithree,4,4,4);
	}       
      }
    }
  }
}

void    fill_scgeom_common(SC_geom_t *G){

  extern SCGEOM_t scgeom_; 
  int i,j;

  for(i=0;i<MAX_PANEL;i++){
    scgeom_.ifirst[i]= G->panel[i].ifirst;
    scgeom_.ilast[i]= G->panel[i].ilast;
/*    for(j=0;j<3;j++){
 *     scgeom_.thtort[j][i] = G->panel[i].tht[j];
 *     scgeom_.phiort[j][i] = G->panel[i].phi[j];
 *    }
 */
    scgeom_.phiort[i] = G->panel[i].phi[0];
   
  }
  for(i=0;i<NUM_SCIN;i++){
    scgeom_.alsc[i] = G->Dim[i].length;
    scgeom_.xm[i] = G->Dim[i].rm.x;
    scgeom_.ym[i] = G->Dim[i].rm.y;
    scgeom_.zm[i] = G->Dim[i].rm.z;
    scgeom_.wdth[i] = G->Dim[i].width;
    scgeom_.thck[i] = G->Dim[i].thick;
  }
}

float check_panel(SCpanel_t *P){
  int i;
  float dot;

  dot= v3dot(P->xo,P->yo);
  if(dot>1e-7){
    printf("\n==================\nDot product x y not zero= %g\n",dot);
  }
  dot= v3dot(P->xo,P->zo);
  if(dot>1e-7){
    printf("\n==================\nDot product x z not zero= %g\n",dot);
  }
  dot= v3dot(P->yo,P->zo);
  if(dot>1e-7){
    printf("\n==================\nDot product y z not zero= %g\n",dot);
  }

#define cosD(x) cos((x/180.)*M_PI)
#define sinD(x) sin((x/180.)*M_PI)

  dot = cosD(P->phi[0])*sinD(P->tht[0])*cosD(P->phi[1])*sinD(P->tht[1]) + 
    sinD(P->phi[0])*sinD(P->tht[0])*sinD(P->phi[1])*sinD(P->tht[1]) + 
    cosD(P->tht[0])*cosD(P->tht[1]);

  if(dot>1e-6){
    printf("\n==================\nANGLE Dot product x y not zero= %g\n",dot);
  }

  dot = cosD(P->phi[0])*sinD(P->tht[0])*cosD(P->phi[2])*sinD(P->tht[2]) + 
    sinD(P->phi[0])*sinD(P->tht[0])*sinD(P->phi[2])*sinD(P->tht[2]) + 
    cosD(P->tht[0])*cosD(P->tht[2]);

  if(dot>1e-6){
    printf("\n==================\nANGLE Dot product x z not zero= %g\n",dot);
  }

  dot = cosD(P->phi[1])*sinD(P->tht[1])*cosD(P->phi[2])*sinD(P->tht[2]) + 
    sinD(P->phi[1])*sinD(P->tht[1])*sinD(P->phi[2])*sinD(P->tht[2]) + 
    cosD(P->tht[1])*cosD(P->tht[2]);

  if(dot>1e-6){
    printf("\n==================\nANGLE Dot product y z not zero= %g\n",dot);
  }
}




void geom_sc_init(int isec,SC_geom_t *Geom){

  char tmpstr[80];
  int i,j,k;
  int iret;
  float Z_Plus_Thick; /* Extra thickness when foam and steel is in place. (!fast_code) */

  if( !gsimcontrol_.fast_code[SC]){
    Z_Plus_Thick = 4.0;
  }else{
    Z_Plus_Thick = 0.1;
  }
  
  if( (Geom->SCGG = (clasSCGG_t *)getGroup(&wcs_, "SCG ", isec))==NULL){
    sprintf(tmpstr," Can not get SCG bank for sector %d.\n",isec);
    recmes_(crname,"F",tmpstr,strlen(crname),1,strlen(tmpstr));
  }
  
  printf(" GEOM_SC_INIT Sector %d\n",isec);

  /* Fill a list of first/last sc list. */
  
  for(i=0;i<MAX_PANEL;i++){
    IFirst(i)=0;
    ILast(i)=0;
  }
  
  j= -1;
  for(i=0;i<Geom->SCGG->bank.nrow;i++){
    if( Geom->SCGG->scgg[i].panel-1 > j ){
      j++;
      IFirst(j)= i;
    }else{
      ILast(j)= i;
    }
    
 /*  Rm = (Xl+Xh)/2. ;  Len = |(Xl-Xh)| */    
    
    Geom->Dim[i].rm     = v3div(2.,v3add(GeomCCWvec(i),GeomCWvec(i)));
    Geom->Dim[i].length = v3mag(   v3sub(GeomCCWvec(i),GeomCWvec(i)));
    Geom->Dim[i].width  = GeomWidth(i);
    Geom->Dim[i].thick  = GeomThick(i);
  }
  
  /* Fill the 3 orthogonal vectors for each plane and calc the polar angles. */
  
  for(j=0;j<MAX_PANEL;j++){
    Geom->panel[j].xo = v3sub(Geom->Dim[ILast(j)].rm,Geom->Dim[IFirst(j)].rm);
    Geom->panel[j].xo = v3norm(Geom->panel[j].xo);
    Geom->panel[j].yo = v3div(2.,v3sub(
				       v3add(GeomCWvec(ILast(j)) ,GeomCWvec(IFirst(j))),
				       v3add(GeomCCWvec(ILast(j)),GeomCCWvec(IFirst(j)))));
    Geom->panel[j].yo = v3norm(Geom->panel[j].yo);
    Geom->panel[j].zo = v3cross(Geom->panel[j].xo,Geom->panel[j].yo);

    if(v3dot(Geom->panel[j].xo,Geom->panel[j].yo) > 1e-7){      
          if(v3dot(Geom->panel[j].xo,Geom->panel[j].yo) > 1e-3){
	    fprintf(stderr,"SC_GEOM_INIT: Angle correction Y large for panel %d \n",j);}

	  fprintf(stderr,"Fix up the Y axis for panel %d\n",j);
	  Geom->panel[j].yo = v3cross(Geom->panel[j].zo,Geom->panel[j].xo);
    }

    v3dir_deg(Geom->panel[j].xo,&Geom->panel[j].tht[0],&Geom->panel[j].phi[0]);
    v3dir_deg(Geom->panel[j].yo,&Geom->panel[j].tht[1],&Geom->panel[j].phi[1]);
    v3dir_deg(Geom->panel[j].zo,&Geom->panel[j].tht[2],&Geom->panel[j].phi[2]);

    check_panel(&Geom->panel[j]);
  }
  
  /* Separate the ganged scintillators into seperate strips. */
  
  if( FIRST_SPLIT_PADDLE < Geom->SCGG->bank.nrow) {
    for(i= FIRST_SPLIT_PADDLE; i<Geom->SCGG->bank.nrow; i++){
      float w_offset;
      int panel_idx = -1;
      int jj = 2*i-FIRST_SPLIT_PADDLE;
      int kk = jj+1;

      for(j=0;j<MAX_PANEL;j++) /* Scan the panels to see which one it's in. */
	if( i <= ILast(j) && i >= IFirst(j)) panel_idx = j;
       
      if(kk>= NUM_SCIN || panel_idx<0){
	fprintf(stderr,"ABORT, divided strips (%d,%d) are more than total strips or panel error (%d). \n",i,kk,panel_idx);
	exit(1);
      }
    
    /* Center points are shifted along the X1 vector by 1/4 a width of the 2 ganged scints. */
    
      w_offset = GeomWidth(i)/4/v3mag(Geom->panel[panel_idx].xo);

      Geom->Dim[jj].rm     = v3add(v3div(2.,v3add(GeomCCWvec(i),GeomCWvec(i))),v3mult( -w_offset,Geom->panel[panel_idx].xo));
      Geom->Dim[jj].length = v3mag( v3sub(GeomCCWvec(i),GeomCWvec(i))) - 2*GeomDelta(i);
      Geom->Dim[jj].width  = GeomWidth(i)/2.0;
      Geom->Dim[jj].thick  = GeomThick(i);
      
      Geom->Dim[kk].rm     = v3add(v3div(2.,v3add(GeomCCWvec(i),GeomCWvec(i))),v3mult( +w_offset,Geom->panel[panel_idx].xo));
      Geom->Dim[kk].length = Geom->Dim[jj].length + 4*Geom->SCGG->scgg[i].delta;
      Geom->Dim[kk].width  = GeomWidth(i)/2.0;
      Geom->Dim[kk].thick  = GeomThick(i);
      
    }

/* Fix up the panel's "ifirst" and "ilast" to account for the split paddles. */
    
    for(j=0;j< MAX_PANEL; j++){
      if( ILast(j) >= FIRST_SPLIT_PADDLE){ /* Panel contains split paddles */
	if( IFirst(j) >=FIRST_SPLIT_PADDLE){ /* Panel is all split, ifirst needs fixing. */
	  IFirst(j) = IFirst(j) + (IFirst(j)-FIRST_SPLIT_PADDLE);
	}
	
	ILast(j) = ILast(j)+ (ILast(j) - FIRST_SPLIT_PADDLE) + 1;
      }
    }
  }  /* if( FIRST_SPLIT_PADDLE < Geom->SCGG->bank.nrow )  */

  /* Calculate a volume around the paddles for SC 'Mother' volume */
  /* What this code does is find the last and first paddle of each panel, and use 
   * vector geometry to calculate the Z and R-min and R-max for the outside edge. */
 
  Geom->sc_parm_num=28;
  Geom->sc_parm[0]= -90. + (float)isec*60.; /* First sector from -30 to 30 */
  Geom->sc_parm[1]= 60.;
  Geom->sc_parm[2]= 1.;
  Geom->sc_parm[3]= (float)MAX_PANEL*2.;
  
  i=0;
  
  for(j=0; j< MAX_PANEL; j++){
    
    vector3_t rvec,zvec;
    float xo_dot_z,xo_dot_r;
    float zo_dot_z,zo_dot_r;
    float zt1,zt2;
    float radius;
    float width;
    float thick;
    
    int pi = MAX_PANEL-j-1;
    
    radius = v3radius(Geom->Dim[ILast(pi)].rm);
    width  = Geom->Dim[ILast(pi)].width;
    thick  = Geom->Dim[ILast(pi)].thick;
    
    rvec.x = Geom->Dim[ILast(pi)].rm.x/radius;
    rvec.y = Geom->Dim[ILast(pi)].rm.y/radius;
    rvec.z = 0;
    
    zvec.x = zvec.y =0;
    zvec.z = 1;
    
    xo_dot_z = v3dot(Geom->panel[pi].xo,zvec);
    xo_dot_r = v3dot(Geom->panel[pi].xo,rvec);
    zo_dot_z = v3dot(Geom->panel[pi].zo,zvec);
    zo_dot_r = v3dot(Geom->panel[pi].zo,rvec);
    
    zt1 = Geom->Dim[ILast(pi)].rm.z + 0.5*width* xo_dot_z - 0.5*thick*zo_dot_z;
    zt2 = Geom->Dim[ILast(pi)].rm.z + 0.5*width* xo_dot_z + (Z_Plus_Thick+0.5*thick)*zo_dot_z;
    
    if( zt1 < zt2 ){
      Geom->sc_parm[j*6+4] = zt1;
       Geom->sc_parm[j*6+5] = radius + 0.5*width* xo_dot_r - 0.5*thick* zo_dot_r;
       Geom->sc_parm[j*6+6] = radius + 0.5*width* xo_dot_r - 0.5*thick* zo_dot_r + (Z_Plus_Thick +thick)/zo_dot_r;
    }else{
      Geom->sc_parm[j*6+4] = zt2;
      Geom->sc_parm[j*6+5] = radius + 0.5*width* xo_dot_r+(Z_Plus_Thick+0.5*thick)* zo_dot_r-(Z_Plus_Thick +thick)/zo_dot_r;
      Geom->sc_parm[j*6+6] = radius + 0.5*width* xo_dot_r+(Z_Plus_Thick+0.5*thick)* zo_dot_r;
    }
    
    radius = v3radius(Geom->Dim[IFirst(pi)].rm);
    width  = Geom->Dim[IFirst(pi)].width;
    thick  = Geom->Dim[IFirst(pi)].thick;
    
    rvec.x = Geom->Dim[IFirst(pi)].rm.x/radius;
    rvec.y = Geom->Dim[IFirst(pi)].rm.y/radius;
    rvec.z = 0;
    
    zvec.x = zvec.y =0;
    zvec.z = 1;
    
    xo_dot_z = v3dot(Geom->panel[pi].xo,zvec);
    xo_dot_r = v3dot(Geom->panel[pi].xo,rvec);
    zo_dot_z = v3dot(Geom->panel[pi].zo,zvec);
    zo_dot_r = v3dot(Geom->panel[pi].zo,rvec);
    
    zt1 = Geom->Dim[IFirst(pi)].rm.z - 0.5*width* xo_dot_z - 0.5*thick*zo_dot_z;               
    zt2 = Geom->Dim[IFirst(pi)].rm.z - 0.5*width* xo_dot_z + (Z_Plus_Thick+0.5*thick)*zo_dot_z;
    
    if( zt1 > zt2 ){
      Geom->sc_parm[j*6+7] = zt1;
      Geom->sc_parm[j*6+8] = radius - 0.5*width* xo_dot_r - 0.5*thick* zo_dot_r;
      Geom->sc_parm[j*6+9] = radius - 0.5*width* xo_dot_r - 0.5*thick* zo_dot_r + (Z_Plus_Thick +thick)/zo_dot_r;
    }else{
      Geom->sc_parm[j*6+7] = zt2;
      Geom->sc_parm[j*6+8] = radius - 0.5*width* xo_dot_r + 0.5*thick* zo_dot_r - (Z_Plus_Thick +thick)/zo_dot_r;
      Geom->sc_parm[j*6+9] = radius - 0.5*width* xo_dot_r + 0.5*thick* zo_dot_r;
    }
  }  
}
