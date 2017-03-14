      SUBROUTINE MFM_GET27(Y,F)
c
c_begin_doc
c  RCS ID string
c  $Id: mfm_get27.F,v 1.1 1999/02/16 16:50:42 kossov Exp $
c
c  Documentation for subroutine MFM_GET27
c
c  Purpose: 27 points approximation of the New Magnetic Field
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters:  NONE
c  -----------------
c
c  Called from: tracking
c  ------------
c
c  Other routines: NONE
c  ---------------
c
c  Notes: Accurate in all cube under approximation
c  ------
c
c  Author:   Mikhail Kossov      Created:  Mon May 12 14:01:12 EST 1997
c  -------
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "mfm_grid.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      REAL Y(3),F(3)
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: mfm_get27.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.1 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1999/02/16 16:50:42 $')
      PARAMETER (CAUTHO= '$Author: kossov $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='mfm_get27')
      PARAMETER (CRAUTH='Mikhail Kossov')
c
c  Local User defined variables:
c  -----------------------------
c
      REAL X(3),B(3)
      REAL RDX,RDY,RDZ, AXM,AYM,AZM, PP,QQ,RR, XR,YR,ZR ,BXFAC,BYFAC
      REAL X1, X2, X3, X4
      REAL F000,F00P,F00M,F0P0,F0M0,FP00,FM00,F0PP,FP0P,FPP0,FPPP,DF000
C __ 27 points
      REAL FMMM,FPMM,FMPM,FMMP,FMPP,FPMP,FPPM,F0PM,F0MP,F0MM,FP0M,FM0P,FM0M
      REAL FMP0,FPM0,FMM0,FMM,FM0,FMP,F0M,F00,F0P,FPM,FP0,FPP,FM,F0,FP
C
      REAL*8  S60, C60, T30, T60
      INTEGER I,NX3,NYNX3, IXM,IYM,IZM, BIND, IREF, IDOUB, ISU
      LOGICAL LFIRST
C
      DATA    LFIRST /.TRUE./
c
c_end_var
c
c  executable code for routine MFM_GET27:
c  -----------------------------------------------------------------------
C
C Each sector is symmetric in Y-coordinate (DST System) (& X coordinate M.K.)
C
      IF(LFIRST) THEN
        WRITE(CRMESS,*)'First call NX=',NX,', NXp=',NXp
        CALL RECMES(CRNAME,'I',CRMESS)
        LFIRST = .FALSE.
        RDX = 1./DX
        RDY = 1./DY
        RDZ = 1./DZ
        NX3   = NX*3
        NYNX3 = NY*NX*3
C
        T60 = DSQRT(3.D0)
        T30 = 1.D0/T60
        C60 = 0.5D0
        S60 = T60/2.D0
C
      ENDIF
C
      IF (Y(2) .LT. 0.) THEN
        Y(2) = -Y(2)
        BXFAC = -1.
      ELSE
        BXFAC =  1.
      ENDIF
C
      IF (Y(1) .LT. 0.) THEN
        Y(1) = -Y(1)
        BYFAC = -1.
      ELSE
        BYFAC =  1.
      ENDIF
C
      IREF = 0
      IF (Y(1) .LT. 1.E-7) THEN
        IF (Y(2) .GT. 1.E-7) THEN
C
          IREF  = 1
          IDOUB = 1
C
          X1 = Y(2)*S60
          X2 = T30
        ELSE
          X1 = 0.
          X2 = 0.
        ENDIF
      ELSE
        X1 = Y(1)
        X2 = Y(2)/X1
        IDOUB = 1
        IF (X2 .GT. T30) THEN
          IF (X2 .GT. T60) THEN
            IREF = 1
            X1 =  Y(1)*C60 + Y(2)*S60
            X2 =(-Y(1)*S60 + Y(2)*C60)/X1
          ELSE
            IREF = 2
            X1 = Y(1)*C60 + Y(2)*S60
            X2 =(Y(1)*S60 - Y(2)*C60)/X1
          ENDIF
        ENDIF
      ENDIF
C
      X(1) = X1
      X(2) = X2
      X(3) = Y(3)
C
      F(1) = 0.
      F(2) = 0.
      F(3) = 0.
      B(1) = 0.
      B(2) = 0.
      B(3) = 0.
C
      ISU = 0
C
C Find intercepts for Big + Mini Toruses
C
      DO WHILE (IDOUB .GT. 0) ! first IDOUB = 1 .OR. = 2
        IF (NX .GT. 0) THEN
          XR  = (X(1)-XMIN)*RDX
          AXM = AINT(XR)
          IXM = IFIX(AXM)
          IF (IXM .LT. 1 .OR. IXM+2 .GT. NX) GO TO 100
          PP  = XR-AXM
C
          YR  = (X(2)-YMIN)*RDY
          AYM = AINT(YR)
          IYM = IFIX(AYM)
          IF (IYM .LT. 1 .OR. IYM+2 .GT. NY) GO TO 100
          QQ  = YR-AYM
C
          ZR  = (X(3)-ZMIN)*RDZ
          AZM = AINT(ZR)
          IZM = IFIX(AZM)
          IF (IZM .LT. 1 .OR. IZM+2 .GT. NZ) GO TO 100
          RR  = ZR-AZM
C
          ISU = ISU + 1
C
C 3-dim 2nd order interpolation inside TORUS for each component of the field.
C We use 11 values of the field:
          DO I=1,3
            BIND = IZM*NYNX3 + IYM*NX3 + IXM*3 + I
            F000= Q(BIND)
            F00P= Q(BIND + NYNX3)
            F00M= Q(BIND - NYNX3)
            F0P0= Q(BIND +   NX3)
            F0M0= Q(BIND -   NX3)
            FP00= Q(BIND +     3)
            FM00= Q(BIND -     3)
            F0PP= Q(BIND + NYNX3 + NX3)
            FP0P= Q(BIND + NYNX3   + 3)
            FPP0= Q(BIND +   NX3   + 3)
            FPPP= Q(BIND + NYNX3 + NX3 + 3)
C
C ___ Extractions for 27 points in addition to 11 points & new algorithm
            F0MM= Q(BIND - NYNX3 - NX3)
            FM0M= Q(BIND - NYNX3   - 3)
            FMM0= Q(BIND -   NX3   - 3)
            F0MP= Q(BIND + NYNX3 - NX3)
            FM0P= Q(BIND + NYNX3   - 3)
            FMP0= Q(BIND +   NX3   - 3)
            F0PM= Q(BIND - NYNX3 + NX3)
            FP0M= Q(BIND - NYNX3   + 3)
            FPM0= Q(BIND -   NX3   + 3)
            FPPM= Q(BIND - NYNX3 + NX3 + 3)
            FPMP= Q(BIND + NYNX3 - NX3 + 3)
            FMPP= Q(BIND + NYNX3 + NX3 - 3)
            FMMP= Q(BIND + NYNX3 - NX3 - 3)
            FMPM= Q(BIND - NYNX3 + NX3 - 3)
            FPMM= Q(BIND - NYNX3 - NX3 + 3)
            FMMM= Q(BIND - NYNX3 - NX3 - 3)
C_ New algorithm : X coordinate
            DF000 = F0MM + F0MM
            FMM   = DF000 + PP * ( FPMM - FMMM + PP * ( FPMM - DF000 + FMMM ) )
            DF000 = F0M0 + F0M0
            FM0   = DF000 + PP * ( FPM0 - FMM0 + PP * ( FPM0 - DF000 + FMM0 ) )
            DF000 = F0MP + F0MP
            FMP   = DF000 + PP * ( FPMP - FMMP + PP * ( FPMP - DF000 + FMMP ) )
            DF000 = F00M + F00M
            F0M   = DF000 + PP * ( FP0M - FM0M + PP * ( FP0M - DF000 + FM0M ) )
            DF000 = F000 + F000
            F00   = DF000 + PP * ( FP00 - FM00 + PP * ( FP00 - DF000 + FM00 ) )
            DF000 = F00P + F00P
            F0P   = DF000 + PP * ( FP0P - FM0P + PP * ( FP0P - DF000 + FM0P ) )
            DF000 = F0PM + F0PM
            FPM   = DF000 + PP * ( FPPM - FMPM + PP * ( FPPM - DF000 + FMPM ) )
            DF000 = F0P0 + F0P0
            FP0   = DF000 + PP * ( FPP0 - FMP0 + PP * ( FPP0 - DF000 + FMP0 ) )
            DF000 = F0PP + F0PP
            FPP   = DF000 + PP * ( FPPP - FMPP + PP * ( FPPP - DF000 + FMPP ) )
C_ New algorithm : Z coordinate
            DF000 = FM0 + FM0
            FM    = DF000 + RR * ( FMP - FMM + RR * ( FMP - DF000 + FMM ) )
            DF000 = F00 + F00
            F0    = DF000 + RR * ( F0P - F0M + RR * ( F0P - DF000 + F0M ) )
            DF000 = FP0 + FP0
            FP    = DF000 + RR * ( FPP - FPM + RR * ( FPP - DF000 + FPM ) )
C_ New algorithm : Y coordinate
            DF000 = F0 + F0
            F(I)  = ( DF000 + QQ * ( FP - FM + QQ * ( FP - DF000 + FM ) ) ) / 8
          ENDDO
        ENDIF
C
 100  CONTINUE
C
        IF (IREF .GT. 0) THEN
          X1 = F(1)
          X2 = F(2)
          IF (IREF .EQ. 2 .OR. IREF .EQ. 4) THEN
            F(1) = -X1*C60 - X2*S60
            F(2) = -X1*S60 + X2*C60
            F(3) = -F(3)
            IF (IREF .EQ. 4) IREF = 0
          ELSEIF (IREF .EQ. 1 .OR. IREF .EQ. 5) THEN
            F(1) = X1*C60 - X2*S60
            F(2) = X1*S60 + X2*C60
            IF (IREF .EQ. 5) IREF = 3
          ELSE ! IREF = 3
            F(1) =  X1*C60 - X2*S60
            F(2) = -X1*S60 - X2*C60
            F(3) = -F(3)
          ENDIF
        ENDIF
C
        IDOUB = IDOUB - 1
        IF (IDOUB .GT. 0) THEN ! two steps calculation
          B(1) = F(1)
          B(2) = F(2)
          B(3) = F(3)
          X(1) = X3
          X(2) = X4
          X(3) = Y(3)
        ELSE
          IF (ISU .EQ. 2) THEN
            F(1) = (F(1)+B(1))/2
            F(2) = (F(2)+B(2))/2
            F(3) = (F(3)+B(3))/2
          ELSEIF (ISU .EQ. 1) THEN
            F(1) = F(1)+B(1)
            F(2) = F(2)+B(2)
            F(3) = F(3)+B(3)
          ENDIF
        ENDIF
      ENDDO
C
      IF (ISU .LE. 0 .OR. ISU .GT. 2) THEN ! Check that mag. field is extracted
        RETURN
      ENDIF
C
      IF (BXFAC .LT. 0.) THEN
        F(1) = -F(1)
        F(3) = -F(3)
      ENDIF
C
      IF (BYFAC .LT. 0.) THEN
        F(2) = -F(2)
        F(3) = -F(3)
      ENDIF
c
      RETURN
      END
