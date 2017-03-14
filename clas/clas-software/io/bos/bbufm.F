CDECK  ID>, BBUFM.
      SUBROUTINE BBUFM(ARR,I1,I2,LUNP)
c
c_begin_doc
c  RCS ID string
c  $Id: bbufm.F,v 1.1.1.1 1997/03/28 20:49:28 marki Exp $
c
c  Purpose:
c  --------
c           print array ARR(I1)...ARR(I2) on unit LUNP
c           Automatic recognition of integers/floating point
c           will not work on VAX
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
      REAL ARR(*)
      INTEGER I1,I2,LUNP
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
     1'$Id: bbufm.F,v 1.1.1.1 1997/03/28 20:49:28 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BBUFM.F')
c
c  Local User defined variables:
c  -----------------------------
      REAL ABSARR
      INTEGER ILG(0:255),I,J,J1,J2,IZERO,JJ,K,L,JS,JT
      CHARACTER*131 LINE, CHLEG*48, STR*12, CHAINT*4
      LOGICAL START
      DATA CHLEG/'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ .,:''()+*-/='/
      DATA START/.TRUE./
c_end_var
c----6----------------------------------------------------------------72
      IF(START) THEN
*       determine legal character codes
        START=.FALSE.
        DO I=0,255
          ILG(I)=0
        ENDDO
        DO J=1,48
          I=ICHAR(CHLEG(J:J))
          ILG(I)=1
        ENDDO
      ENDIF
C     ...
      J1=MAX0(1,I1)
      IF(LUNP.LE.0.OR.J1.GT.I2) GOTO 100
      J2=((J1-1)/5)*5+5
      IZERO=0
      LINE=' '
   20 J2=MIN0(I2,J2)
      IF(J1.NE.I1.AND.J2.NE.I2) THEN
*       test all zeros in present line
        DO J=J1,J2
          IF(ARR(J).NE.0.0) GOTO 40
        ENDDO
        IF(IZERO.EQ.0) IZERO=J1
        GOTO 90
      ENDIF
   40 IF(IZERO.NE.0) THEN
        WRITE(LUNP,'(1X,I5,A)')
     +  IZERO,' - .. and all following words are zero'
        IZERO=0
      ENDIF
      DO 80 J=J1,J2
        JJ=12*(J-J1)+23
*       test all 8-bit bytes in word for character
        STR=CHAINT(ARR(J))
        DO K=1,4
          L=ICHAR(STR(K:K))
          IF(ILG(L).EQ.0) GOTO 70
        ENDDO
        JS=4
        GOTO 75
*       test real or integer
   70   ABSARR=ABS(ARR(J))
C if defined(SGI)
C*       check for NaN (not very nice but working ...)
C        WRITE(TEMP,'(F20.5)') ARR(J)
C        IF(INDEX(TEMP,'NaN').NE.0.OR.INDEX(TEMP,'nan')) THEN
C          CALL PNVI(ARR(J),STR,JS)
C        ELSEIF(ABSARR.LT.1.0E-20.OR.ABSARR.GT.1.E+20) THEN
C else
        IF(ABSARR.LT.1.0E-20.OR.ABSARR.GT.1.E+20) THEN
C endif
          CALL PNVI(ARR(J),STR,JS)
        ELSE
          CALL PNVF(ARR(J),STR,JS)
        ENDIF
        IF(JS.LE.0.OR.JS.GT.12) THEN
C          WRITE(6,'(1X,''WRONG '',I12)') JS
          JS=12
        END IF
   75   LINE(JJ+1-JS:JJ)=STR(1:JS)
   80 CONTINUE
*     print one line
      CALL PNVI(J1,STR,JS)
      JT=MIN0(5,JS)
      LINE(7-JT:6)=STR(JS-JT+1:JS)
      LINE(7:7)='<'
      CALL PNVI(J2,STR,JS)
      JT=MIN0(3,JS)
      LINE(11-JT:11)=STR(JS-JT+1:JS)
      WRITE(LUNP,'(A)') LINE
      LINE=' '
C
   90 IF(J2.EQ.I2) GOTO 100
      J1=J2+1
      J2=J2+5
      GOTO 20
  100 continue
      RETURN
      END
