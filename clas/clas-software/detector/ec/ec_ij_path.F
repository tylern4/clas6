      SUBROUTINE Ec_ij_path(di,dj,pu,pv,pw,layer,sector)
*--------------------------------------------------------------------------
* purpose and method: find distance parallel to strops to edge from (di,dj)
* 
* inputs   : di      - I
*          : dj      - J
*          : layer   Ec layer innor or outer (Ec_in, Ec_outer)
*          : sector 
* outputs  : pu      - path to u
*          : pv      - path to v
*          : pw      - path to w
* created 24-Mar-1995   Zhujun Li
*
*----------------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ec_ij_path.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/05/01 19:48:17 $')
      PARAMETER (CAUTHO = '$Author: freyberg $')
      DATA CRCSID/ 
     1'$Id: ec_ij_path.F,v 1.2 1997/05/01 19:48:17 freyberg Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ec_ij_path')                                   
      PARAMETER (CRAUTH='Zhujun Li')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c
#include "Ec_general.PAR"
#include "ecgeom.inc"
      
      real di,dj,pu,pv,pw,Lu,Lv,Lw,H,H1,du,dv,dw
      integer layer,sector 
*
      Lu=Ec_edge_L(Ec_u,layer,sector)
      Lv=Ec_edge_L(Ec_v,layer,sector)
      Lw=Ec_edge_L(Ec_w,layer,sector)
      H=sqrt(Lu**2-Lv**2/4.)              !assume Lu=Lw      
      H1=EcBsc_H1(layer)
*
      du=(di/H +H1/H)*Lu                  !calculate u,v,w of (i,j)
      dv=Lv-H1/2./H*Lv-dj-Lv/2./H*di
      dw=Lw/Lv*dj-Lw/2./H*di-H1/2./H*Lw+Lw
*
      pu=du/Lu*Lv-(Lv-dv)                  ! path
      pv=dv/Lv*Lw-(Lw-dw)
      pw=dw/Lw*Lu-(Lu-du)      
* 
      RETURN
      END
