      SUBROUTINE MFM_BRUBZ(A,A2,R,R2,Z,RI2,GIA,T2,B1,B2)
c
c_begin_doc
c  RCS ID string
c  $Id: mfm_brubz.F,v 1.1 1999/02/16 16:50:39 kossov Exp $
c
c  Documentation for subroutine MFM_BRUBZ
c
c  Purpose: After MFM_INULI subdivided the coil in radial direction this
c  -------- subroutine subdivide it in Z direction and calculate the field
c           for the thin conductor calling elliptic integral routine MFM_ELLIPT
c
c  Calling Sequence: MFM_ELLIPT in Z-loop
c  ----------------
c
c  Input Parameters: A,A2 - ring radius, R,R2,Z - point, RI2,GIA,T2 - ori/steps
c  ----------------
c
c  Output Parameters: B1 - radial coordinate, B2 - longitudinal coordinate
c  -----------------
c
c  Called from: MFM_INULI
c  ------------
c
c  Other routines: NONE
c  ---------------
c
c  Notes: Accuracy cuts are hadrwired and can be not optimal (M.Kossov)
c  ------ Commemts wit @ shows the R=0 simple test.
c         51 LOOP shoul be changed to DO WHILE ...
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
      REAL A,A2,R,R2,Z,RI2,GIA,T2,B1,B2
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: mfm_brubz.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.1 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1999/02/16 16:50:39 $')
      PARAMETER (CAUTHO= '$Author: kossov $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='mfm_brubz')
      PARAMETER (CRAUTH='Mikhail Kossov')
c
c  Local User defined variables:
c  -----------------------------
c
      INTEGER IFIRST, IR
      REAL AK2,U0,U1,U2,Z1,Z2,E1,E2
C
      DATA IFIRST/0/
c
c_end_var
c
c  executable code for routine MFM_BRUBZ:
c  -----------------------------------------------------------------------
c
      IF (IFIRST .EQ. 0) THEN
        WRITE(CRMESS,*)'First call a=',A,',r=',R,',z=',Z,',G,T2,R2=',GIA,T2,RI2
        CALL RECMES(CRNAME,'I',CRMESS)
        IFIRST = 1
      ENDIF
C
      Z1 = Z-(GIA+T2*RI2)
      Z2 = Z1*Z1
      IR=0
C
 51   CONTINUE
        U1 = (A+R)*(A+R)+Z2        ! @ R=0 : U1 = A2 + Z2
        U0 = SQRT(U1)              ! @ R=0 : U0 = SQRT (A2+Z2)
        U2 = (A-R)*(A-R)+Z2        ! @ R=0 : U2 = A2 + Z2
        IF (U2.LE.0.000064) THEN
           A = A+0.01
           A2 = A*A
           IR = IR + 1
c           IF (IR .GT. 1) THEN
c             WRITE(CRMESS,*)'IR=',IR,', U2=',U2,', A=',A,', R=',R,', Z2=',Z2
c             CALL RECMES(CRNAME,'I',CRMESS)
c           ENDIF
           GO TO 51
        ENDIF
C
      AK2 = 4.*A*R/U1              ! @ R=0 : ak2=0
      IF (AK2.GT.0.9999999) AK2=0.9999999
C
C--  Calculation of the elliptic integrals
C
      CALL MFM_ELLIPT(AK2, E1, E2) ! @ R=0 : ak2=0 : E1=E2=2pi
C
      IF (R.GE.0.000001 .AND. ABS(Z1).GE.0.000001) THEN
         B1 = (E2*(A2+R2+Z2)/U2-E1)*Z1/(R*U0)
         B1 = B1 + B1
      ELSE
         B1 = 0.                   ! @ R=0 : BR=0
      ENDIF
      B2 = (E2*(A2-R2-Z2)/U2+E1)/U0
      B2 = B2 + B2                 ! @ R=0 : BZ=8pi*a2/(a2+z2)/SQRT(a2+z2)
C                                    = O'K = M.Kossov = test =
      RETURN
      END
