C======================================================================
      SUBROUTINE snake_dipole(X,F)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods: Performs 3-dim 2-nd order interpolation for each 
C-                        component of the magnetic field using Taylor series. 
C-
C-   Inputs  : X(3) -space point and Magnetic Field Table (see "sdabgrid.inc")
C-             Note: space point in CLAS coordinates (any sector!)  
C-   Outputs : F(3) -magnetic field at point X(3)
C-   Controls:
C-
C-   Calls: none
C-
C-   Author:     P.Vernin     (Aug. 1987)
C-   Modified:   M.Guckes     (Oct. 1987)
C-               B.Niczyporuk (Feb. 1991)
C- 
C- 
C-    Called by sda_helix
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
c      SAVE
C
#include "bgrid.inc"
C
      REAL X(3),F(3)


	


C
C Local variables
      INTEGER I,NX3,NYNX3, IXM,IYM,IZM, BXFAC, BYFAC, BZFAC
      REAL    RDX,RDY,RDZ, AXM,AYM,AZM, PP,QQ,RR, XR,YR,ZR
      REAL    CP,CQ,CR, DP,DQ,DR,E, Xmax,Ymax,Zmax
      REAL    F000,F00P,F00M,F0P0,F0M0,FP00,FM00,F0PP,FP0P,FPP0,FPPP
      LOGICAL LFIRST
      DATA    LFIRST /.TRUE./

      SAVE RDX,RDY,RDZ,NX3,NYNX3,LFIRST,Xmax,Ymax,Zmax


C
C
C grid only for -1.<X<100; -1.<Y<100; -1.<Z<100
C
      BXFAC=1.
      BYFAC=1.
      BZFAC=1.
      IF(X(1).LT.0.) THEN
         X(1) = -X(1)
         BXFAC = -1.
      ENDIF
      IF(X(2).LT.0.) THEN
        X(2) = -X(2)
        BYFAC = -1.
      ENDIF
      IF(X(3).LT.0.) THEN
        X(3) = -X(3)
        BYFAC = -1.
      ENDIF
C
      IF(LFIRST) THEN
        LFIRST = .FALSE.
        RDX = 1./DX_PTG
        RDY = 1./DY_PTG
        RDZ = 1./DZ_PTG
        NX3   = NX_PTG*3
        NYNX3 = NY_PTG*NX_PTG*3
        Xmax  = Xmin_ptg+NX_ptg*DX_ptg
        Ymax  = Ymin_ptg+NY_ptg*DY_ptg
        Zmax  = Zmin_ptg+NZ_ptg*DZ_ptg
      ENDIF
C
C Find intercepts.
C
      IF(X(1).GT.Xmax .OR. X(2).GT.Ymax .OR. X(3).GT.Zmax) GOTO 100
      XR  = (X(1)-XMIN_ptg)*RDX
      AXM = AINT(XR)
      IXM = IFIX(AXM)
      IF(IXM.LT.1 .OR. IXM.GT.NX_ptg-2) GO TO 100
      PP  = XR-AXM
C
      YR  = (X(2)-YMIN_ptg)*RDY
      AYM = AINT(YR)
      IYM = IFIX(AYM)
      IF(IYM.LT.1 .OR. IYM.GT.NY_ptg-2) GO TO 100
      QQ  = YR-AYM
C
      ZR  = (X(3)-ZMIN_ptg)*RDZ
      AZM = AINT(ZR)
      IZM = IFIX(AZM)
      IF(IZM.LT.1 .OR. IZM.GT.NZ_ptg-2) GO TO 100
      RR  = ZR-AZM
C
C 3-dim 2nd order interpolation inside TORUS for each component of the field.
C We use 11 values of the field:
      DO 22 I=1,3
        F000= Qptg((IZM  )*NYNX3 + (IYM  )*NX3 + (IXM  )*3 + I)
        F00P= Qptg((IZM+1)*NYNX3 + (IYM  )*NX3 + (IXM  )*3 + I)
        F00M= Qptg((IZM-1)*NYNX3 + (IYM  )*NX3 + (IXM  )*3 + I)
        F0P0= Qptg((IZM  )*NYNX3 + (IYM+1)*NX3 + (IXM  )*3 + I)
        F0M0= Qptg((IZM  )*NYNX3 + (IYM-1)*NX3 + (IXM  )*3 + I)
        FP00= Qptg((IZM  )*NYNX3 + (IYM  )*NX3 + (IXM+1)*3 + I)
        FM00= Qptg((IZM  )*NYNX3 + (IYM  )*NX3 + (IXM-1)*3 + I)
        F0PP= Qptg((IZM+1)*NYNX3 + (IYM+1)*NX3 + (IXM  )*3 + I)
        FP0P= Qptg((IZM+1)*NYNX3 + (IYM  )*NX3 + (IXM+1)*3 + I)
        FPP0= Qptg((IZM  )*NYNX3 + (IYM+1)*NX3 + (IXM+1)*3 + I)
        FPPP= Qptg((IZM+1)*NYNX3 + (IYM+1)*NX3 + (IXM+1)*3 + I)
        CP = FP00+FM00-2.*F000
        CQ = F0P0+F0M0-2.*F000
        CR = F00P+F00M-2.*F000
        DP = F000-FP00+FPP0-F0P0
        DQ = F000-F0P0+F0PP-F00P
        DR = F000-F00P+FP0P-FP00
        E  = -DP-F0PP+F00P-FP0P+FPPP
C Compute the Taylor's series:
        F(I)= F000+0.5*(PP*(FP00-FM00)+PP*PP*CP
     1                 +QQ*(F0P0-F0M0)+QQ*QQ*CQ
     2                 +RR*(F00P-F00M)+RR*RR*CR)
     3                 +PP*QQ*DP+QQ*RR*DQ+PP*RR*DR+PP*QQ*RR*E
 22   CONTINUE
      F(1) = BXFAC*BYFAC*F(1)
      F(3) = BZFAC*BYFAC*F(3)
      GO TO 999
C
C Point is outside of the grid
 100  CONTINUE
      F(1) = 0.
      F(2) = 0.
      F(3) = 0.
C
C restore input values for (x,y,z)
 999  CONTINUE
      X(1)=BXFAC*X(1)
      X(2)=BYFAC*X(2)
      X(3)=BZFAC*X(3)

      RETURN
      END
