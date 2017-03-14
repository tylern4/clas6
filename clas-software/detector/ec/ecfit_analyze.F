      SUBROUTINE EcFit_analyze(sector)
*----------------------------------------------------------------------
*- 
*-   Purpose and Methods : Ec fitting using selected algorithm 
*- 
*-   Input : technique      - requested method of reconstruction
*-         : sector      - CLAS sector (1-6)
*- 
*-   Created  7-Nov-1992   Kevin B. Beard
*-   Modified 31-Jan-1994  Kevin B. Beard
*-   Modified 17_Feb-1995  Z. Li
*----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ecfit_analyze.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.9 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2006/06/15 14:46:15 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/ 
     1'$Id: ecfit_analyze.F,v 1.9 2006/06/15 14:46:15 stepanya Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ecfit_analyze')                                   
      PARAMETER (CRAUTH='Zhujun Li')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
*
#include "Ec_general.PAR"
#include "ec_control.inc"
#include "EcFit_general.CMN"
#include "ec_strip.inc"

*
      integer layer,sector,axis,iterr,ierr,ihit,ifltmp(ec_u:ec_w)
*----------------------------------------------------------------------
*
      DO layer= whole,cover
         if(layer.eq.cover)then
            MaxWstrips=MaxPcalStrips
         else
            MaxWstrips=MaxEcStrips
         endif
         ierr=1
         iterr=1
         Do axis= Ec_U,Ec_W
            call ecfsp(axis,layer,sector,iterr,ierr) !strips=>peaks
         EndDo
         if(ierr.eq.0)call ecfph(layer,sector) !peaks=>hits
      ENDDO
*     
      RETURN
      END
