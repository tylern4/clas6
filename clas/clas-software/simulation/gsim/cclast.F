CDECK  ID>, CCLAST.
      SUBROUTINE CCLAST
C-
C-   Purpose and Methods : Output for hbook
C_                         Close file for output diagnostics
C-
C-   Inputs: NONE
C-   Outputs : NONE
C-   Controls:
C-   Addition:
C-   Created  31-Jan-1994   Alexander Vlassov
C
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*(132)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: cclast.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:09 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: cclast.F,v 1.2 1997/04/24 15:38:09 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='cclast')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
C----------------------------------------------------------------------
C
C  Statement functions:
C  Local data statements:      None
CE)===============================================(end of declarations)
C
****************************************
      WRITE(CRMESS,*) 'CCLAST reached'
      call recmes(CRNAME,'I',CRMESS)
C=======================================
C
      RETURN
      END
