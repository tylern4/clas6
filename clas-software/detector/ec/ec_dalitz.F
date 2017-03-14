***************************************************************************
      LOGICAL FUNCTION Ec_dalitz(du,dv,dw,ddu,ddv,ddw,
     &                 di,dj,dk,ddi,ddj,RMS,layer,sector,err,iterr)
*----------------------------------------------------------------------
*-
*-   Purpose and Methods : Uses "Dalitz" sum to see if (du,dv,dw)
*-                         is a point in the UVW coord system; if so
*-                          return the (I,J) coordinate
*-
*-   Inputs  : du        - U projection
*-           : dv        - V
*-           : dw        - W
*-           : MAXerr    - largest error to allow for success (cm)
*-           : layer     - Ec layer ( inner or  outer)
*-           : sector    - CLAS sector (1-6)
*-   Outputs : di        - I projection of mean crossings
*-           : dj        - J
*-           : dk        - K
*-           : RMS       - root mean square of UV,VW,WU crossings
*-
*-   Created  25-MAR-1992   Kevin B. Beard
*-   major modificatin 23-MAY-1995  Zhujun Li
*-   
*----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ec_dalitz.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.4 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:14 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/ 
     1'$Id: ec_dalitz.F,v 1.4 2008/03/16 04:33:14 fklein Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ec_dalitz')                                   
      PARAMETER (CRAUTH='Zhujun Li')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c
*
#include "Ec_general.PAR"
#include "ecgeom.inc"
#include "ec_control.inc"
*
      real du,dv,dw,ddu,ddv,ddw
      real MAXerr,RMS,err
      real ddi,ddj,di,dj,dk
      integer layer,sector,iterr
      real dalitz,sum_edge
*----------------------------------------------------------------------
*- for a triangle should always sum to 2/3
      dalitz= du/Ec_edge_L(Ec_u,layer,sector)
     &      + dv/Ec_edge_L(Ec_v,layer,sector)
     &      + dw/Ec_edge_L(Ec_w,layer,sector)
      Maxerr= 2.*sqrt ( 
     &                (ddu/Ec_edge_L(Ec_u,layer,sector))**2 
     &              + (ddv/Ec_edge_L(Ec_v,layer,sector))**2 
     &              + (ddw/Ec_edge_L(Ec_w,layer,sector))**2 )
c      print *,'Dalitz 1 ', dalitz,Maxerr
      sum_edge=Ec_edge_L(Ec_U,layer,sector)
     &         +Ec_edge_L(Ec_V,layer,sector)
     &         +Ec_edge_L(Ec_W,layer,sector)
c      Maxerr = ( ddu + ddv +ddw ) * 1.*SQRT(12.) 
*
*- rule satisfied?
*
c      print *,'Dalitz 2 ', dalitz,Maxerr,sum_edge
c      Ec_dalitz= ABS(dalitz-2.)*sum_edge.LT.MAXerr
      err=ABS(dalitz-2.)/MAXerr
      if(MAXerr.gt.0.001) err=ABS(dalitz-2.)/MAXerr
      Ec_dalitz= ABS(dalitz-2.).LT.MAXerr 
*
*    find the hit position in the local IJK coordinat system
*
      IF(Ec_dalitz) THEN
*
*
         dI=ecbsc_H(layer)*
     &     (du/Ec_edge_L(Ec_u,layer,sector) -
     &      dv/Ec_edge_L(Ec_v,layer,sector) -
     &      dw/Ec_edge_L(Ec_w,layer,sector) )/2.
     &     +ecbsc_h2(layer)
         dJ=Ec_edge_L(Ec_v,layer,sector) *
     &     (dw/Ec_edge_L(Ec_w,layer,sector) -
     &      dv/Ec_edge_L(Ec_v,layer,sector))/2.
*
         dK=EcBsc_d(layer)       ! surface of the layer
*
*
         if(iterr.eq.1)return
*
         ddi=sqrt((ddu/Ec_edge_L(Ec_u,layer,sector))**2+
     &            (ddv/Ec_edge_L(Ec_v,layer,sector))**2+
     &            (ddw/Ec_edge_L(Ec_w,layer,sector))**2)
     &      *ecbsc_H(layer)/2.
*
         ddj=sqrt((ddw/Ec_edge_L(Ec_w,layer,sector))**2+
     &            (ddv/Ec_edge_L(Ec_v,layer,sector))**2)*
     &             Ec_edge_L(Ec_v,layer,sector)/2.
*
         RMS=SQRT(ddi**2+ddj**2)
*
      ENDIF
*
      RETURN
      END
