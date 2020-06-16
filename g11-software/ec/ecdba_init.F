      SUBROUTINE EcDba_init(sector)
*----------------------------------------------------------------------
*-
*-   Purpose and Methods : read in geometry bos bank and initialize
*-                         for all sectors, fill part of EcStrip.CMN
*-
*-  Inputs: sector       - CLAS sector (1-6)
*-
*-
*-   Created  24-April-1995   Z.Li
*----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ecdba_init.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:50:27 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/ 
     1'$Id: ecdba_init.F,v 1.1.1.1 1997/03/28 20:50:27 marki Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ecdba_init')                                   
      PARAMETER (CRAUTH='Zhujun Li')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
#include "Ec_general.PAR"
#include "EcStrip.CMN"
#include "EcDba.CMN"
*
      real depth,ratio,H1,pi
*
      integer sector,id,axis,layer,s
*
*----------------------------------------------------------------------
*
      s=sector
      pi=4.*atan(1.)
c* temp, will be replaced with geometry initialization when BOS bank ready

      Ec_edge_L(Ec_u,whole,s)=433.4262
      Ec_edge_L(Ec_v,whole,s)=395.0382
      Ec_edge_L(Ec_w,whole,s)=433.4262
      Ec_edge_L(Ec_u,inner,s)=418.7994
      Ec_edge_L(Ec_v,inner,s)=381.7069
      Ec_edge_L(Ec_w,inner,s)=418.7994
      Ec_edge_L(Ec_u,outer,s)=433.4262
      Ec_edge_L(Ec_v,outer,s)=395.0382
      Ec_edge_L(Ec_w,outer,s)=433.4262

c      Ec_edge_L(Ec_u,cover,s)=456.8289
c      Ec_edge_L(Ec_v,cover,s)=416.3684
c      Ec_edge_L(Ec_w,cover,s)=459.8289
*
c!! fill EcStrip width later, each strip's length.
*
* temp 
      EcBsc_r=510.54
      EcBsc_a=25     !degrees for sector one 
      EcBsc_v=50.098 ! closest vertical distance to beam
* temp, calculate the depth of Ec
      ratio=(Ec_edge_L(Ec_u,outer,s)/Ec_edge_L(Ec_u,inner,s)
     @      + Ec_edge_L(Ec_v,outer,s)/Ec_edge_L(Ec_v,inner,s))/2.
      depth=EcBsc_r*(ratio-1)
      EcBsc_d(inner)=depth
      EcBsc_d(outer)=8./5.*depth      
      EcBsc_d(whole)=13./5.*depth      
      H1=(EcBsc_r*sin(EcBsc_a*pi/180.)-EcBsc_v)/cos(EcBsc_a*pi/180.)
      EcBsc_H1(inner)=H1
      EcBsc_H1(outer)=ratio*H1       
      EcBsc_H1(whole)=ratio*H1       

* the following line may not necessary
      Do layer= whole, outer
        do axis=Ec_u,Ec_w
          do id= 1, MAXsectors
             Ec_strip(ATTEN,id,axis,layer,sector)=1. ! no attenuation
             Ec_strip(DIM,id,axis,layer,sector)= 1.              !100% bright
             Ec_strip(FEY,id,axis,layer,sector)= 0.              !no feyness now
             Ec_strip(energy,id,axis,layer,s)=0.
             Ec_strip(time,id,axis,layer,s)=0.
          enddo
        enddo
      Enddo

      return
      end
