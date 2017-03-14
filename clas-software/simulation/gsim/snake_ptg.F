
      SUBROUTINE snake_ptg(X,F)

c_begin_doc
C-
C-   Purpose and Methods: Performs 2-dim 2-nd order interpolation for each 
C-                        component of the magnetic field using Taylor series. 
C-
C-   Inputs  : X(3) -space point and Magnetic Field Table (see "sdabgrid.inc")
C-   Outputs : F(3) -magnetic field at point X(3)
C-   Controls:
C-
C-   Author:     P.Vernin     (Aug. 1987)
C-               F.Klein      (Sep. 1998)
C- 
c_end_doc

      IMPLICIT NONE
      SAVE
C
c_begin_inc
#include "bgrid.inc"
c_end_inc
C
C Subroutine variables
      REAL X(3), F(3)

c_begin_var
      CHARACTER*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      CHARACTER*132 crcsid
      PARAMETER (CFILE=  '$RCSfile: snake_ptg.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.2 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 2005/10/18 18:15:43 $')
      PARAMETER (CAUTHO= '$Author: avakian $')
      PARAMETER (CRNAME= 'SNAKE_PTG')
      PARAMETER (CRAUTH= 'F.Klein')
      data crcsid/'
     1$Id: snake_ptg.F,v 1.2 2005/10/18 18:15:43 avakian Exp $
     1 '/
C
C Local variables
      INTEGER I, IXM, IZM, MX3
      REAL    Xmax, Zmax, RDDXY, RDDZ, R, Bfac
      REAL    AXM,AZM, PP,RR, XR,ZR
      REAL    CP, CR, DR
      REAL    F000, F00P, F00M, FP00, FM00, FP0P
      LOGICAL LFIRST
      DATA    LFIRST /.TRUE./
c_end_var
C
C
C Each sector is symmetric in Y-coordinate (DST System)
C
      IF(LFIRST) THEN
        LFIRST = .FALSE.
        Xmax = xmin_ptg + nx_ptg*dx_ptg
        Zmax = zmin_ptg + nz_ptg*dz_ptg
        RDDXY = 1./dx_ptg
        RDDZ  = 1./dz_ptg
        MX3   = nx_ptg*3
      ENDIF
C
C The Polarized Target Field TABLE was created for Z > 0
C
      IF(X(3).LT.0.) THEN
        X(3) = -X(3)
        Bfac = -1.
      ELSE
        Bfac =  1.
      ENDIF
C
      R = SQRT(X(1)*X(1) + X(2)*X(2))
      IF(R.LE.xmin_ptg .OR. R.GE.Xmax) GO TO 100
      IF(X(3).LE.zmin_ptg  .OR. X(3).GE.Zmax) GO TO 100
      XR  = (R - xmin_ptg)*RDDXY
      AXM = AINT(XR)
      IXM = IFIX(AXM)
      IF(IXM.LT.1 .OR. IXM.GT.nx_ptg-2) GO TO 100
      PP  = XR-AXM
C
      ZR  = (X(3)-zmin_ptg)*RDDZ
      AZM = AINT(ZR)
      IZM = IFIX(AZM)
      IF(IZM.LT.1 .OR. IZM.GT.nz_ptg-2) GO TO 100
      RR  = ZR-AZM
C
C 2-dim 2nd order interpolation inside POLTARG for each component of the field.
C
      DO 11 i = 1,3
        IF(i.EQ.2) GO TO 11
        F000= Qptg((IZM  )*MX3 + (IXM  )*3 + I)
        F00P= Qptg((IZM+1)*MX3 + (IXM  )*3 + I)
        F00M= Qptg((IZM-1)*MX3 + (IXM  )*3 + I)
        FP00= Qptg((IZM  )*MX3 + (IXM+1)*3 + I)
        FM00= Qptg((IZM  )*MX3 + (IXM-1)*3 + I)
        FP0P= Qptg((IZM+1)*MX3 + (IXM+1)*3 + I)
        CP = FP00+FM00-2.*F000
        CR = F00P+F00M-2.*F000
        DR = F000-F00P+FP0P-FP00
        F(I)= F000+0.5*(PP*(FP00-FM00)+PP*PP*CP
     1                   +RR*(F00P-F00M)+RR*RR*CR)+PP*RR*DR
 11   CONTINUE
      X(3) = Bfac*X(3)
      F(1) = Bfac*F(1)
C
C Rotation around Z
      IF(R .GT. 0.0001) THEN
        F(2) = F(1)*X(2)/R
        F(1) = F(1)*X(1)/R
      ENDIF
C
      RETURN
C
C Point is outside of the grid
 100  CONTINUE
      X(3) = Bfac*X(3)
      F(1) = 0.
      F(2) = 0.
      F(3) = 0.
C
      RETURN
      END


