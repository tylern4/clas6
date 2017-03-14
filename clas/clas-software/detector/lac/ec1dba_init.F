      SUBROUTINE Ec1Dba_init
c
      IMPLICIT NONE
      SAVE
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ec1dba_init.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:57:59 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/ 
     1'$Id: ec1dba_init.F,v 1.1.1.1 1997/03/28 20:57:59 marki Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ec1dba_init')                                   
      PARAMETER (CRAUTH='avakian')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
#include "ec1_control.inc"
#include "ec1_calibr.inc"
#include "ec1_pedstl.inc"
c#include "Ec1Dba.CMN"
*
      WRITE(CRMESS,20)ec1_calib
20    FORMAT('| LAC calibration type=',i4,' Reading calibration/pedestal data')
      CALL RECMES(CRNAME,'N',CRMESS)
c
c    reset the calibration time
c
       ec1_calupdt=0
       ec1_pedupdt=0
c      CALL EC1_PED(CURR_TIME)
c      CALL EC1_CAL(CURR_TIME)
      return
      end
