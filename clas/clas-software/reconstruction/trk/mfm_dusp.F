      SUBROUTINE MFM_DUSP(NLOOP,R,Z,RJ,RI,ZAU,ZBU,RA,ZAO,ZBO,H0,S5,S6,D5)
c
c_begin_doc
c  RCS ID string
c  $Id: mfm_dusp.F,v 1.1 1999/02/16 16:50:40 kossov Exp $
c
c  Documentation for subroutine MFM_DUSP
c
c  Purpose: Magnetic field calculation. Integration over D(I) and all coils.
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  R,Z - point, NLOOP - a#of coils, RJ - current,..., H0-max
c  ----------------
c
c  Output Parameters: S5 - radial comp., S6 - longitudinal comp., D5 - differ.
c  -----------------
c
c  Called from: MTM_PTGFIELD
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Volker Burkert      Created:  Mon May 12 14:01:12 EST 1994
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
c#include ""
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      INTEGER NLOOP
      REAL RI(NLOOP),ZAU(NLOOP),ZBU(NLOOP),RA(NLOOP),ZAO(NLOOP),ZBO(NLOOP)
      REAL RJ(NLOOP), R, Z, H0, S5, S6, D5
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: mfm_dusp.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.1 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1999/02/16 16:50:40 $')
      PARAMETER (CAUTHO= '$Author: kossov $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='mfm_dusp')
      PARAMETER (CRAUTH='Mikhail Kossov')
c
c  Local User defined variables:
c  -----------------------------
c
      INTEGER K, IFIRST, I1, N1, N2
      REAL RRN1, T1, S3, S4, S1, S2, R2
c
      DATA IFIRST/0/
c
c_end_var
c
c  executable code for routine MFM_DUSP:
c  -----------------------------------------------------------------------
c
      IF (IFIRST .EQ. 0) THEN
        WRITE(CRMESS,*)'First call r=',R,', z=',Z
        CALL RECMES(CRNAME,'I',CRMESS)
        IFIRST = 1
      ENDIF
C
      R2 = R*R
      S5 = 0.
      S6 = 0.
C
      DO K=1,NLOOP
        IF (R.GE.RI(K) .AND. R.LE.RA(K) .AND. ABS(Z).LE.ABS(ZBO(K))) THEN
           N1 = 100
           RRN1= 0.01
        ELSE
           N1 = 16
           RRN1= 1./16.
        END IF
        N2 = N1
        T1 = (RA(K)-RI(K))*RRN1
        S3 = 0.
        S4 = 0.
C
        DO I1=0,N1,N1
C -----  Integration over variable length
          CALL MFM_INULI(R,R2,Z,RI(K),FLOAT(I1),RRN1,N2,T1,ZAU(K),ZBU(K),
     &         ZAO(K),ZBO(K),S1,S2)
          S3 = S3+S1
          S4 = S4+S2
        ENDDO
C
        DO I1=1,(N1-1),2
          CALL MFM_INULI(R,R2,Z,RI(K),FLOAT(I1),RRN1,N2,T1,ZAU(K),ZBU(K),
     &         ZAO(K),ZBO(K),S1,S2)
          S3 = S3+4.*S1
          S4 = S4+4.*S2
        ENDDO
C
        DO I1=2,(N1-2),2
          CALL MFM_INULI(R,R2,Z,RI(K),FLOAT(I1),RRN1,N2,T1,ZAU(K),ZBU(K),
     &         ZAO(K),ZBO(K),S1,S2)
          S3 = S3+2.*S1
          S4 = S4+2.*S2
        ENDDO
C
        S3 = S3*(T1*0.33333333)
        S4 = S4*(T1*0.33333333)
        S5 = S5+S3*RJ(K)*0.0001
        S6 = S6+S4*RJ(K)*0.0001
c
      ENDDO
C
      D5 = (S6-H0)/H0
C
      RETURN
      END
