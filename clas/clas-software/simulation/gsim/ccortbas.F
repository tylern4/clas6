CDECK  ID>, CCORTBAS.
      SUBROUTINE CCORTBAS(P,PM,P1,P2,IC)
c
c_begin_doc
c  RCS ID string
c  $Id: ccortbas.F,v 1.2 1997/04/24 15:38:09 holtrop Exp $
c
c  Documentation for subroutine CCORTBAS(P,PM,P1,P2,IC)
c
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ORTOGANAL BASYS FOR VECTOR 'P'
C-
C-   Inputs  :: P(1-3) - INPUT VECTOR, PM=|P|
C-   Outputs :: P1(1-3),P2(1-3) - ORTS(MOD(P1)=MOD(P2)=1) TO P-VECTOR
C-   Controls:: IC=0: O'K; IC=-1: ZERO INPUT VECTOR 'P'
C-
C-   Created    19-SEP-1991   Mikhail KOSSOV
C-   Modified   18-AUG-1995   Alexander V. Vlassov
C-
C----------------------------------------------------------------------
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      INTEGER IC
      REAL PM, P(3), P1(3), P2(3)
c
c  Local pre-defined variables:
c  ---------------------------
C----------------------------------------------------------------------
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*(132)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ccortbas.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:09 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccortbas.F,v 1.2 1997/04/24 15:38:09 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='ccortbas')                                   
      PARAMETER (CRAUTH='Mikhail Kossov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c  Local User defined variables:
c  -----------------------------
      REAL RNORM,SCALE
      real vmod,vsmall
      external vmod
      data vsmall /0.000001/
c_end_var
c
c  executable code for routine CCORTBAS
c------------------------------------------------------------------------------
C
      IC=0
      IF(vmod(P,3).lt.vsmall) THEN
        PRINT*,'*** ORTBAS: ZERO INPUT VECTOR P=',P(1),P(2),P(3)
        IC=-1
        RETURN
      ENDIF
      IF(P(1).LT.P(2).AND.P(1).LT.P(3)) THEN
        RNORM=SQRT(P(2)*P(2)+P(3)*P(3))
        P1(1)=0.
        P1(2)=P(3)/RNORM
        P1(3)=-P(2)/RNORM
        P2(1)=(P1(2)*P(3)-P1(3)*P(2))
        P2(2)=P1(3)*P(1)
        P2(3)=-P1(2)*P(1)
      ELSEIF(P(2).LT.P(3).AND.P(2).LT.P(1)) THEN
        RNORM=SQRT(P(1)*P(1)+P(3)*P(3))
        P1(2)=0.
        P1(1)=P(3)/RNORM
        P1(3)=-P(1)/RNORM
        P2(2)=(P1(1)*P(3)-P1(3)*P(1))
        P2(1)=P1(3)*P(2)
        P2(3)=-P1(1)*P(2)
      ELSE
        RNORM=SQRT(P(2)*P(2)+P(1)*P(1))
        P1(3)=0.
        P1(2)=P(1)/RNORM
        P1(1)=-P(2)/RNORM
        P2(3)=(P1(2)*P(1)-P1(1)*P(2))
        P2(2)=P1(1)*P(3)
        P2(1)=-P1(2)*P(3)
      ENDIF
      IF(PM.NE.1.) THEN
        SCALE=1./PM
        CALL VSCALE(P2,SCALE,P2,3)
      ENDIF
      RETURN
      END
