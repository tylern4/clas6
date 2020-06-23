      SUBROUTINE MCAUCH(A,B,S,EPS)
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for subroutine MCAUCH
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
      REAL A,B,EPS,S
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
      PARAMETER (CRNAME='MCAUCH')
      PARAMETER (CRAUTH='')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER I,J,K,irep
      REAL X,W,S1,S2,S8,S16,Z1,HF,CST
      REAL AA,BB,CONST
      REAL B0,C
      REAL XMIN0,XMIN1
      DOUBLE PRECISION NAME
      REAL C1,C2,C3,C4,U
      CHARACTER*80 ERRTXT
c_end_var
c
c  executable code for routine DASPE_RUN:
c----6----------------------------------------------------------------72


      DIMENSION X(12),W(12),S8(6,3),S16(6,3),S1(6,3),S2(6,3)

      PARAMETER (Z1 = 1, HF = Z1/2, CST = 5*Z1/1000)

      DATA X( 1) /9.6028985E-1/, W( 1) /1.0122853E-1/
      DATA X( 2) /7.9666647E-1/, W( 2) /2.2238103E-1/
      DATA X( 3) /5.2553240E-1/, W( 3) /3.1370664E-1/
      DATA X( 4) /1.8343464E-1/, W( 4) /3.6268378E-1/
      DATA X( 5) /9.8940093E-1/, W( 5) /2.7152459E-2/
      DATA X( 6) /9.4457502E-1/, W( 6) /6.2253523E-2/
      DATA X( 7) /8.6563120E-1/, W( 7) /9.5158511E-2/
      DATA X( 8) /7.5540440E-1/, W( 8) /1.2462897E-1/
      DATA X( 9) /6.1787624E-1/, W( 9) /1.4959598E-1/
      DATA X(10) /4.5801677E-1/, W(10) /1.6915651E-1/
      DATA X(11) /2.8160355E-1/, W(11) /1.8260341E-1/
      DATA X(12) /9.5012509E-2/, W(12) /1.8945061E-1/
      

      IF(S .EQ. A .OR. S .EQ. B) THEN
        DO j=1,jm
          DO k=1,km
            H(j,k)=0.E0
          ENDDO
        ENDDO
        WRITE(ERRTXT,101) S
        CALL MTLPRT(NAME,'D104.1',ERRTXT)
      ELSEIF(S .LT. MIN(A,B) .OR. S .GT. MAX(A,B)) THEN
        CALL MGAUSS(A,B,EPS)
      ELSE
       IF(2*S .LE. A+B) THEN
         CALL MGAUSS(2*S-A,B,EPS)
         B0=S-A
       ELSE
         CALL MGAUSS(A,2*S-B,EPS)
         B0=B-S
       ENDIF
       C=CST/B0
       BB=0.E0

    1  AA=BB
       BB=B0
    2  C1=HF*(BB+AA)
       C2=HF*(BB-AA)
       C3=S+C1
       C4=S-C1
        DO j=1,jm
          DO k=1,km
            S8(j,k)=0.E0
            S16(j,k)=0.E0
          ENDDO
        ENDDO
       DO I = 1,4
         U=C2*X(I)
         CALL integr_f(C3+U)
         DO j=1,jm
           DO k=1,km
             S1(j,k)=W(I)*ba(j,k)
           ENDDO
         ENDDO
         CALL integr_f(C4-U)
         DO j=1,jm
           DO k=1,km
             S1(j,k)=S1(j,k)+W(I)*ba(j,k)
           ENDDO
         ENDDO

         CALL integr_f(C3-U)
         DO j=1,jm
           DO k=1,km
             S2(j,k)=W(I)*ba(j,k)
           ENDDO
         ENDDO
         CALL integr_f(C4+U)
         DO j=1,jm
           DO k=1,km
             S2(j,k)=S2(j,k)+W(I)*ba(j,k)
             S8(j,k)=S8(j,k)+S1(j,k)+S2(j,k)
           ENDDO
         ENDDO


c          S8(j,k)=S8(j,k)+W(I)*((F(C3+U)+F(C4-U))+(F(C3-U)+F(C4+U)))
       ENDDO
       DO j=1,jm
         DO k=1,km
           S8(j,k)=C2*S8(j,k)
         ENDDO
       ENDDO
       DO I = 5,12
         U=C2*X(I)
         CALL integr_f(C3+U)
         DO j=1,jm
           DO k=1,km
             S1(j,k)=W(I)*ba(j,k)
           ENDDO
         ENDDO
         CALL integr_f(C4-U)
         DO j=1,jm
           DO k=1,km
             S1(j,k)=S1(j,k)+W(I)*ba(j,k)
           ENDDO
         ENDDO
             
         CALL integr_f(C3-U)
         DO j=1,jm
           DO k=1,km
             S2(j,k)=W(I)*ba(j,k)
           ENDDO
         ENDDO
         CALL integr_f(C4+U)
         DO j=1,jm
           DO k=1,km
             S2(j,k)=S2(j,k)+W(I)*ba(j,k)
             S16(j,k)=S16(j,k)+S1(j,k)+S2(j,k)
           ENDDO
         ENDDO
             
c          S16(j,k)=S16(j,k)+W(I)*((F(C3+U)+F(C4-U))+(F(C3-U)+F(C4+U)))
       ENDDO

       DO j=1,jm
         DO k=1,km
           S16(j,k)=C2*S16(j,k)
         ENDDO
       ENDDO

       irep=1
       DO j=1,jm
         DO k=1,km
           xmin0=ABS(S16(j,k)-S8(j,k))
           xmin1=EPS*(1+ABS(S16(j,k)))
           IF(xmin0 .GT. xmin1) irep=0
         ENDDO
       ENDDO
       IF(irep .EQ. 1) GO TO 5

       BB=C1
       IF(1+ABS(C*C2) .NE. 1) GO TO 2
       DO j=1,jm
         DO k=1,km
           H(j,k)=0.E0
         ENDDO
       ENDDO
       CALL MTLPRT(NAME,'D104.2','TOO HIGH ACCURACY REQUIRED')
       GO TO 9
    5  DO j=1,jm
        DO k=1,km
           H(j,k)=H(j,k)+S16(j,k)
         ENDDO
       ENDDO
       IF(BB .NE. B0) GO TO 1
      END IF
    9 RETURN
  101 FORMAT('SINGULARITY  S = ',D15.8,' AT END-POINT OF INTERVAL')
      END
