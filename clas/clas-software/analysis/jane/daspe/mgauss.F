      SUBROUTINE MGAUSS(A,B,EPS)
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for subroutine MGAUSS
c
c  Purpose:
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:  H. Aznauryan       Created:  Wed Dec 11 17:16:39 EST 1996
c  -------
c
c  Major revisions:
c  ----------------
c
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
c
#include "ima.inc"
#include "int_func.inc"
c
c_end_inc   
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      REAL A,B,EPS
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information:
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile$')
      PARAMETER (CREVIS = '$Revision$')
      PARAMETER (CSTATE = '$State$')
      PARAMETER (CDATE = '$Date$')
      PARAMETER (CAUTHO = '$Author$')
      DATA CRCSID/ 
     1'$Id$'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='MGAUSS')
      PARAMETER (CRAUTH='')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER I,J,K,irep
      REAL X,W,S8,S16,Z1,HF,CST
      REAL AA,BB,CONST
      REAL XMIN0,XMIN1
      REAL C1,C2,U
      DOUBLE PRECISION NAME
      PARAMETER (Z1 = 1, HF = Z1/2, CST = 5*Z1/1000)
c_end_var
c
c  executable code for routine DASPE_RUN:
c----6----------------------------------------------------------------72

      DIMENSION W(12),X(12),S8(6,3),S16(6,3)



      DATA X
     1        /0.96028 985E0,
     2         0.79666 647E0,
     3         0.52553 240E0,
     4         0.18343 464E0,
     5         0.98940 093E0,
     6         0.94457 502E0,
     7         0.86563 120E0,
     8         0.75540 440E0,
     9         0.61787 624E0,
     A         0.45801 677E0,
     B         0.28160 355E0,
     C         0.95012 509E-1/
c
      DATA W
     1        /0.10122 853E0,
     2         0.22238 103E0,
     3         0.31370 664E0,
     4         0.36268 378E0,
     5         0.27152 459E-1,
     6         0.62253 523E-1,
     7         0.95158 511E-1,
     8         0.12462 897E0,
     9         0.14959 598E0,
     A         0.16915 651E0,
     B         0.18260 341E0,
     C         0.18945 061E0/
c

      DO j=1,jm
        DO k=1,km
          H(j,k)=0.E0
        ENDDO
      ENDDO
      IF(B .EQ. A) GO TO 99
      CONST=CST/ABS(B-A)
      BB=A
    1 AA=BB
      BB=B
    2 C1=HF*(BB+AA)
      C2=HF*(BB-AA)
      DO j=1,jm
        DO k=1,km
          S8(j,k)=0.E0
          S16(j,k)=0.E0
        ENDDO
      ENDDO
      DO I = 1,4
        U=C2*X(I)
        CALL integr_f(C1+U)
        DO j=1,jm
          DO k=1,km
            S8(j,k)=S8(j,k)+W(I)*ba(j,k)
          ENDDO
        ENDDO
        CALL integr_f(C1-U)
        DO j=1,jm
          DO k=1,km
            S8(j,k)=S8(j,k)+W(I)*ba(j,k)
          ENDDO
        ENDDO
c                 S8(j,k)=S8(j,k)+W(I)*(F(C1+U)+F(C1-U))
      ENDDO
      DO j=1,jm
        DO k=1,km
          S8(j,k)=S8(j,k)*C2
        ENDDO
      ENDDO
c 		     S16(j,k)=0.E0
      DO I = 5,12
        U=C2*X(I)
        CALL integr_f(C1+U)
        DO j=1,jm
          DO k=1,km
            S16(j,k)=S16(j,k)+W(I)*ba(j,k)
          ENDDO
        ENDDO
        CALL integr_f(C1-U)
        DO j=1,jm
          DO k=1,km
            S16(j,k)=S16(j,k)+W(I)*ba(j,k)
          ENDDO
        ENDDO
c           S16(j,k)=S16(j,k)+W(I)*(F(C1+U)+F(C1-U))
      ENDDO
      DO j=1,jm
        DO k=1,km
          S16(j,k)=C2*S16(j,k)
        ENDDO
      ENDDO

      irep = 1
      DO j=1,jm
        DO k=1,km
          xmin0=ABS(S16(j,k)-S8(j,k))
          xmin1=EPS*(1+ABS(S16(j,k))/100.)
          IF(xmin0 .GT. xmin1) irep=0
        ENDDO
      ENDDO
      IF(irep .EQ. 1) THEN
        DO j=1,jm
          DO k=1,km
            H(j,k)=H(j,k)+S16(j,k)
          ENDDO
        ENDDO
        IF(BB .NE. B) GO TO 1
      ELSE
        BB=C1
        IF(1+CONST*ABS(C2) .NE. 1) GO TO 2
        DO j=1,jm
          DO k=1,km
            H(j,k)=0.E0
          ENDDO
        ENDDO
        CALL MTLPRT(NAME,'D103.1','TOO HIGH ACCURACY REQUIRED')
      END IF
   99 RETURN
      END
