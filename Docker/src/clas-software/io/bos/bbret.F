CDECK  ID>, BBRET.
      SUBROUTINE BBRET(JW,ICODE,SUBPGM,NAME)
c
c_begin_doc
c  RCS ID string
c  $Id: bbret.F,v 1.1.1.1 1997/03/28 20:49:28 marki Exp $
c
c  Purpose:
c  --------
c       Print error 'INSUFFICIENT SPACE FOR NEW BANK'
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
#include "param.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER JW(*),ICODE
      CHARACTER *(*) SUBPGM
      CHARACTER*4    NAME
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
     1'$Id: bbret.F,v 1.1.1.1 1997/03/28 20:49:28 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BBRET.F')
c
c  Local User defined variables:
c  -----------------------------
      CHARACTER*40  TEXT(6)
      DATA TEXT/'INSUFF.SPACE FOR NEW BANK         BANK= ',
     2          'INSUFF.SPACE TO INCR. LENGTH      BANK= ',
     3          '                                        ',
     4          'READ ERROR                              ',
     5          'READ ERROR (FORMAL ERROR)               ',
     6          'READ ERROR (INSUFFICIENT SPACE)         '/
c_end_var
c----6----------------------------------------------------------------72
      IF(JW(LUP).EQ.0.OR.JW(LUW).EQ.0) GOTO 100
      JW(LUW)=JW(LUW)-1
      WRITE(JW(LUP),1001) SUBPGM,ICODE,TEXT(ICODE),NAME
  100 RETURN
 1001 FORMAT('0BOS--',A5,'-  RETURN CODE',I2,4X,A40,1X,A4)
      END
