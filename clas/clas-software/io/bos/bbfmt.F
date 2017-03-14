CDECK  ID>, BBFMT.
      SUBROUTINE BBFMT(NAMJ,IFMT,N)
c
c_begin_doc
c  RCS ID string
c  $Id: bbfmt.F,v 1.1.1.1 1997/03/28 20:49:27 marki Exp $
c
c  Purpose:
c  --------
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
#include "bcs.inc"
#include "bossys.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER NAMJ,N,IFMT(N)
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
     1'$Id: bbfmt.F,v 1.1.1.1 1997/03/28 20:49:27 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BBFMT.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER ID,INTCHA
c_end_var
c----6----------------------------------------------------------------72
      ID=IDFMT+NAMJ-NSYST
C  allow replacing bank format definition: comment following 3 lines
C     IF(IW(ID).NE.0) THEN
C        IF(IFMT(2).LE.IW(IW(ID)+2)) GOTO 100
C     END IF
      CALL WBANC(IW,IW(ID),N,1,*90)
      IW(IW(ID)-3)=INTCHA('+FMT')
      CALL UCOPY(IFMT,IW(IW(ID)+1),N)
C
C     FORMAT FLAG IEFMT  = -1   ERROR IN BKFMT CALL
C                        >  0   NUMBER OF DEFINED FORMATS
C
      IF(IEFMT.GE.0) IEFMT=IEFMT+1
      GOTO 100
   90 CALL BBSPC(IW,'BBFMT')
  100 RETURN
      END
