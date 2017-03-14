CDECK  ID>, NXTFMT.
      SUBROUTINE NXTFMT(IFMT,NFMT,N,K)
c
c_begin_doc
c  RCS ID string
c  $Id: nxtfmt.F,v 1.1.1.1 1997/03/28 20:55:48 marki Exp $
c
c  Purpose:
c  --------
c     return next format code (or reset for NFMT = 0)
c
c
c  Author:
c  -------
c     Volker Blobel          xxx Apr 15 15:00:00 EDT 1994
c
c  Major revisions:
c  ----------------
c     Serguei Boiarinov      Thu Oct 12 11:52:54 EDT 1995
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER IFMT(*),NFMT,N,K
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information:
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$rcsRCSfile$')
      PARAMETER (CREVIS = '$rcsRevision$')
      PARAMETER (CSTATE = '$rcsState$')
      PARAMETER (CDATE = '$rcsDate$')
      PARAMETER (CAUTHO = '$rcsAuthor$')
      DATA CRCSID/
     1'$Id: nxtfmt.F,v 1.1.1.1 1997/03/28 20:55:48 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='NXTFMT.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER I,ITERM,LEV,LV(3,10)
C      INTEGER LL
c_end_var
c----6----------------------------------------------------------------72
C      WRITE(6,101) (IFMT(LL),LL=1,NFMT)
C 101  FORMAT(' NXTFMT ',10I6)
      IF(NFMT.NE.0) GOTO 10
*     initialize format use
      I    =0
      ITERM=0
      LEV  =0
      N    =0
      K    =0
      GOTO 100
*     get next format code
   10 I=I+1
      IF (NFMT.EQ.1) THEN
         N=999999999
         K=MOD(IFMT(1),10)
         GOTO 100
      ENDIF
      IF(I.GT.NFMT) THEN
         I=ITERM
         GOTO 10
      END IF
*     right parenthesis
      IF(IFMT(I).EQ.0) THEN
         LV(3,LEV)=LV(3,LEV)+1
         IF(LV(3,LEV).GE.LV(2,LEV)) THEN
            ITERM=LV(1,LEV)-1
            LEV=LEV-1
         ELSE
            I=LV(1,LEV)
         END IF
      ELSE
         N=IFMT(I)/10
         K=IFMT(I)-10*N
         IF(K.EQ.0) THEN
*           left parenthesis
            LEV=LEV+1
            LV(1,LEV)=I
            LV(2,LEV)=N
            LV(3,LEV)=0
         ELSE
*           format F or I or A or
            IF(I.NE.NFMT-1)      GOTO 100
            IF(LEV.EQ.0)         GOTO 100
            IF(I.NE.LV(1,LEV)+1) GOTO 100
            N=999999999
            GOTO 100
         END IF
      END IF
      GOTO 10
  100 RETURN
      END
