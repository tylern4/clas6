CDECK  ID>, CCDEFPAR.
      SUBROUTINE CCDEFPAR(NCH)
c_begin_doc
c  RCS ID string
c  $Id: ccdefpar.F,v 1.2 1997/04/24 15:38:06 holtrop Exp $
c
c  Documentation for subroutine CCDEFPAR
c
C-----------------------------------------------------------------------
C-   Purpose and Methods : Geometry definitions for one CC  (# NCH)
C- 
C-   Inputs  : nch - # of CC
C-   Outputs : parameters for nch in /CCPW/
C-   Controls:
C-
C-   Created    09-FEB-1994   Alexander V. Vlassov
C-   Modified   09-MAY-1994   Alexander V. Vlassov
C-   Modified   18-AUG-1995   Alexander V. Vlassov
C-----------------------------------------------------------------------
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
C
#include "ccp.inc"
#include "ccfrmp.inc"
C
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER NCH
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
      PARAMETER (CFILE = '$RCSfile: ccdefpar.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:06 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccdefpar.F,v 1.2 1997/04/24 15:38:06 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='ccdefpar')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c  Local User defined variables:
c  -----------------------------
c
      INTEGER I,J
c
c_end_var
c
c  executable code for routine CCDEFPAR :
C ---
      DO I=1,5
        PE(I)=PE0(I,NCH)
        PH(I)=PH0(I,NCH)
      END DO
C ---
      DO I=1,3
        P1(I)  = P10(I,NCH)
        P2(I)  = P20(I,NCH)
        P3(I)  = P30(I,NCH)
        P4(I)  = P40(I,NCH)
        P5(I)  = P50(I,NCH)
        P6(I)  = P60(I,NCH)
        P7(I)  = P70(I,NCH)
        P8(I)  = P80(I,NCH)
        P9(I)  = P90(I,NCH)
        PA(I)  = PA0(I,NCH)
        WCC(I) = WC0(I,NCH)
        SW(I)  = SW0(I,NCH)
        PW(I)  = PW0(I,NCH)
c
        PE_PL(I) = ELT_PL(I,NCH)
        PWC1 (I) = PWC10 (I,NCH)
        PWC2 (I) = PWC20 (I,NCH)
        SCRP1(I) = SCRNP1(I,NCH)
        SCRP2(I) = SCRNP2(I,NCH)
c
      END DO
c
      CSA = csa0(NCH)
      XE_PL = X_PL(NCH)
c
      CALL UCOPY(PLMS0(1,1,NCH),PLMS(1,1),12)
c
      do i = 1,4
        do j = 1,4
          pcms(1,j,i) = xcms(j,i,NCH)
          pcms(2,j,i) = ycms(j,i,NCH)
          pcms(3,j,i) = zcms(j,i,NCH)
        end do
      end do
c
      RETURN
      END
