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
      PARAMETER (CREVIS = '$Revision: 1.6 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/01/27 15:41:01 $')
      PARAMETER (CAUTHO = '$Author: brooksw $')
      DATA CRCSID/ 
     1'$Id: ecfit_analyze.F,v 1.6 1998/01/27 15:41:01 brooksw Exp $'
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
*
      integer layer,sector,axis,iterr,ierr
*----------------------------------------------------------------------
*
cSerguei      IF ( anal_opt.eq.1 ) THEN
cSerguei        DO layer= WHOLE,OUTER
cSerguei           Do axis= Ec_U,Ec_W
cSerguei             call EcFit_strips_peaks(axis,layer,sector) !strips=>peaks
cSerguei           EndDo
cSerguei           call EcFit_peaks_hits(layer,sector)             !peaks=>hits
cSerguei        ENDDO
cSerguei        call EcFit_hits_showers(sector)            !hits=>showers
cSerguei      ELSEIF (anal_opt.eq.2 ) THEN
*


      DO layer= whole,outer
          ierr=1
          iterr=1
           Do axis= Ec_U,Ec_W
             call ecfsp(axis,layer,sector,iterr,ierr) !strips=>peaks
           EndDo
           if(ierr.eq.0)call ecfph(layer,sector)          !peaks=>hits
        ENDDO
* 
cSerguei      ENDIF
*
      RETURN
      END
