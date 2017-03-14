      SUBROUTINE MFM_TORFIELD(R,S1,S2,BT)
c
c_begin_doc
c  RCS ID string
c  $Id: mfm_torfield.F,v 1.2 1999/04/05 01:05:43 kossov Exp $
c
c  Documentation for subroutine MFM_TORFIELD
c
c  Purpose: Calculates magnetic field of streight currents from S1(3) to S2(3) 
c  -------- in point R(3)
c
c  Calling Sequence: See drawing (comment space below)
c  ----------------
c
c  Input Parameters: R(3), S1(3), S2(3)
c  ----------------
c
c  Output Parameters: BT(3) - magnetic field (DOUBLE PRECISION)
c  -----------------
c
c  Called from: MFM_FIELD
c  ------------
c
c  Other routines: CERNLIB Vector Algebra
c  ---------------
c
c  Notes: Accelerated by factor 1.5
c  ------
c
c  Author:   Mikhail Kossov      Created:  Mon May 12 14:01:12 EST 1997
c  -------
c
c_end_doc
c
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      SAVE
C
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
c#include ""
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      REAL*8 BT(3)           ! To avoid loosing of small numbers - M.K.
      REAL S1(3),S2(3),R(3)
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: mfm_torfield.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.2 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1999/04/05 01:05:43 $')
      PARAMETER (CAUTHO= '$Author: kossov $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='mfm_torfield')
      PARAMETER (CRAUTH='Mikhail Kossov')
c
c  Local User defined variables:
c  -----------------------------
c
      REAL*8 V1(3),V2(3),C(3),U(3)
      REAL*8 V1SQ,V2SQ,CSQ,COS1,COS2,USQ,FACT2,COEF
C
C
C V1: Vector to one end of line
      V1(1) = S1(1)-R(1)
      V1(2) = S1(2)-R(2)
      V1(3) = S1(3)-R(3)
C
C V1: Vector to other end of line
      V2(1) = S2(1)-R(1)
      V2(2) = S2(2)-R(2)
      V2(3) = S2(3)-R(3)
C
C C: The line itself
      C(1) = S2(1)-S1(1)
      C(2) = S2(2)-S1(2)
      C(3) = S2(3)-S1(3)
C
      V1SQ = V1(1)*V1(1) + V1(2)*V1(2) + V1(3)*V1(3)
      V2SQ = V2(1)*V2(1) + V2(2)*V2(2) + V2(3)*V2(3)
      CSQ  = C(1)*C(1) + C(2)*C(2) + C(3)*C(3)
      IF (CSQ.LE.0. .OR. V1SQ.LE.0. .OR. V2SQ.LE.0.) RETURN
C
C U: Vector product of C x V1
C      CALL CROSS(C, V1, U)
      U(1)=C(2)*V1(3)-C(3)*V1(2)
      U(2)=C(3)*V1(1)-C(1)*V1(3)
      U(3)=C(1)*V1(2)-C(2)*V1(1)
C
      COS1  = (V1(1)*C(1)+V1(2)*C(2)+V1(3)*C(3)) / DSQRT(V1SQ*CSQ)
      COS2  = (V2(1)*C(1)+V2(2)*C(2)+V2(3)*C(3)) / DSQRT(V2SQ*CSQ)
      USQ   = U(1)*U(1) + U(2)*U(2) + U(3)*U(3)
      FACT2 = V1SQ*USQ*(1.-COS1*COS1)
      IF (FACT2.LE.0.) RETURN
      COEF  = (COS2-COS1) / DSQRT(FACT2)
C
C      print*,'B=',U(1)*COEF,U(2)*COEF,U(3)*COEF
      BT(1) = BT(1)+COEF*U(1)
      BT(2) = BT(2)+COEF*U(2)
      BT(3) = BT(3)+COEF*U(3)
C
      RETURN
      END
