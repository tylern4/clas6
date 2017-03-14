      SUBROUTINE MFM_ELLIPT(C8,E1,E2)
c
c_begin_doc
c  RCS ID string
c  $Id: mfm_ellipt.F,v 1.1 1999/02/16 16:50:41 kossov Exp $
c
c  Documentation for subroutine MFM_ELLIPT
c
c  Purpose: Calculation of elliptic integrals
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  C8 (changed) 
c  ----------------
c
c  Output Parameters: E1,E2
c  -----------------
c
c  Called from: MFM_BRUBZ
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes: Accuracy cuts are handwired (?!), @-comments is a R=0 test
c  ------ 60 LOOP should be replaced by DO WHILE
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
      REAL C8,E1,E2
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: mfm_ellipt.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.1 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1999/02/16 16:50:41 $')
      PARAMETER (CAUTHO= '$Author: kossov $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='mfm_ellipt')
      PARAMETER (CRAUTH='Mikhail Kossov')
c
c  Local User defined variables:
c  -----------------------------
c
      INTEGER IFIRST,I9
      REAL PIBY2,A8,B8,A9,B9,C9
      PARAMETER (PIBY2=6.2831853) ! I'm not sure that we need "*pi" here M.K.
c
      DATA IFIRST/0/
c
c_end_var
c
c  executable code for routine MFM_ELLIPT:
c  -----------------------------------------------------------------------
C
      IF (IFIRST .EQ. 0) THEN
        WRITE(CRMESS,*)'First call C8=',C8
        CALL RECMES(CRNAME,'I',CRMESS)
        IFIRST = 1
      ENDIF
c
      A8 = 1.
      I9 = 2
      B8 = SQRT(1.-C8)   ! @ C8=0 : B8 = 1.
C
 60   CONTINUE
        A9 = (A8+B8)*0.5 ! @ C8=0 : A9 = 1.
        B9 = SQRT(A8*B8) ! @ C8=0 : B9 = 1.
        C9 = (A8-B8)*0.5 ! @ C8=0 : C9 = 0.
        C8 = C8+I9*C9*C9 ! @ C8=0 : unchanged
        I9 = I9+I9       ! @ C8=0 : I9 = 4
        A8 = A9          ! @ C8=0 : A8 = 1.
        B8 = B9          ! @ C8=0 : B9 = 1.
c
      IF (ABS(C9).GT.0.000001) GO TO 60
C
      E1 = PIBY2/A8      ! @ C8=0 : 2pi
      E2 = (1.-C8*.5)*E1 ! @ C8=0 : 2pi
C
      RETURN
      END
