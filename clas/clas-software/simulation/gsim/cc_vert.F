CDECK  ID>, CC_VERT.
      SUBROUTINE CC_VERT(IPART,P0)
C     ============================
C
C GET INITIAL PARTICLE CHARACTERISTICS FROM SHELL BANK
C IN FORM PX,PY,PZ
C
C-- SHELL DATA PARAMETERS --
C -  Now is a dummy routine, because
C -  it should be extract from BOS - bank or any other way!
c
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*(132)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: cc_vert.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:05 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: cc_vert.F,v 1.2 1997/04/24 15:38:05 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='cc_vert')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
      INTEGER IPART
      REAL P0(3)
C
      IPART = 0
      P0(1) = 0.
      P0(2) = 0.
      P0(3) = 0.
C        
      RETURN
      END
