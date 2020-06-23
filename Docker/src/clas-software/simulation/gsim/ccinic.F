      SUBROUTINE CCINIC
CA)========================================(begin of short description)
C
CB)-----------------------------------------(begin of long description)
C
C    Purpose and methods :
C
C    Called by:
C
C    Input arguments : None
C    Input/output arguments : None
C    Output arguments : None
C
C    Side effects (other variables/banks modified, etc.)
C
C    Created:
C    Modified:
C
CC)-------------------------------------------(end of long description)
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*(132)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ccinic.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/06/08 19:29:31 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/ 
     1'$Id: ccinic.F,v 1.3 1998/06/08 19:29:31 fklein Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='ccinic')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
CD)----------------------------------(declare common blocks, variables)
C
#include "cctest.inc"
#include "ccdig.inc"
C
CE)===============================================(end of declarations)
C
C Define a card for CC package (example).
      CALL FFKEY( 'CCTEST', ICCTST, 10, 'INTE' )
C
      n_hits_CC = 0
C
      END
