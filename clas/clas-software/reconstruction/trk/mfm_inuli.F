      SUBROUTINE MFM_INULI(R,R2,Z,RIK,RI1,RRN1,N2,T1,ZAUK,ZBUK,ZAOK,ZBOK,S1,S2)
c
c_begin_doc
c  RCS ID string
c  $Id: mfm_inuli.F,v 1.1 1999/02/16 16:50:43 kossov Exp $
c
c  Documentation for subroutine MFM_INULI
c
c  Purpose: Integration over radial layers of coils
c  --------
c
c  Calling Sequence: Calls MFM_BRUBZ in LOOP
c  ----------------
c
c  Input Parameters: R,R2,Z - point, RIK,RI1,RRN1,N2 - or/step (R), Z...- bound
c  ----------------
c
c  Output Parameters: S1 - radial coordinate, S2 - longitudinal coordinate
c  -----------------
c
c  Called from: MFM_DUSP
c  ------------
c
c  Other routines: NONE
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
      INTEGER N2
      REAL R,R2,Z,RIK,RI1,RRN1,T1,ZAUK,ZBUK,ZAOK,ZBOK,S1,S2
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: mfm_inuli.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.1 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1999/02/16 16:50:43 $')
      PARAMETER (CAUTHO= '$Author: kossov $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='mfm_inuli')
      PARAMETER (CRAUTH='Mikhail Kossov')
c
c  Local User defined variables:
c  -----------------------------
c
      INTEGER IFIRST, I2
      REAL T2,T3,A,A2,GIA,GIB,B1,B2
c
      DATA IFIRST/0/
c
c_end_var
c
c  executable code for routine MFM_INULI:
c  -----------------------------------------------------------------------
c
      IF (IFIRST .EQ. 0) THEN
        WRITE(CRMESS,*)'First call r=',R,', z=',Z,', RIK,T1,RI1=',RIK,T1,RI1
        CALL RECMES(CRNAME,'I',CRMESS)
        IFIRST = 1
      ENDIF
c
      A = RIK+T1*RI1
      A2 = A*A
      GIA = ZAUK+(ZAOK-ZAUK)*RRN1*RI1
      GIB = ZBUK+(ZBOK-ZBUK)*RRN1*RI1
      T2 = (GIB-GIA)/FLOAT(N2)
      S1 = 0.
      S2 = 0.
C
      DO I2=0,N2,N2
C--  Calculation of BR and BZ
        CALL MFM_BRUBZ(A,A2,R,R2,Z,FLOAT(I2),GIA,T2,B1,B2)
        S1 = S1+B1
        S2 = S2+B2
      ENDDO
C
      DO I2=1,(N2-1),2
        CALL MFM_BRUBZ(A,A2,R,R2,Z,FLOAT(I2),GIA,T2,B1,B2)
        S1 = S1+4.*B1
        S2 = S2+4.*B2
      ENDDO
C
      DO I2=2,(N2-2),2
        CALL MFM_BRUBZ(A,A2,R,R2,Z,FLOAT(I2),GIA,T2,B1,B2)
        S1 = S1+2.*B1
        S2 = S2+2.*B2
      ENDDO
C
      T3 = ABS(T2)/3.
      S1 = S1*T3
      S2 = S2*T3
C
      RETURN
      END
